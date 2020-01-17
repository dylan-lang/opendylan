Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Expanded actions command
define class <jam-action-command> (<object>)
  constant slot action-command-string :: false-or(<string>),
    required-init-keyword: string:;
  constant slot action-command-message :: false-or(<string>),
    required-init-keyword: message:;
  constant slot action-command-targets :: <sequence>,
    required-init-keyword: targets:;
  constant slot action-command-ignore? :: <boolean>,
    required-init-keyword: ignore?:;
  constant slot action-command-successors :: <object-set>
    = make(<object-set>);
  slot action-command-predecessor-count :: <integer>,
    init-value: 0;
  slot action-command-output-stream :: <sequence-stream>;
end class;

define sealed domain make(singleton(<jam-action-command>));

define method action-command-add-successor
    (predecessor :: <jam-action-command>, successor :: <jam-action-command>)
 => ();
  unless (member?(successor, predecessor.action-command-successors))
    add!(predecessor.action-command-successors, successor);
    successor.action-command-predecessor-count
      := successor.action-command-predecessor-count + 1;
  end unless;
end method;

// jam-target-build
//
define method jam-target-build
    (jam :: <jam-state>, target-names :: <sequence>,
     #key force?,
          jobs :: <integer> = 1,
          progress-callback :: <function> = ignore)
 => (build-successful? :: <boolean>);
  let thread-pool = make(<thread-pool>, size: jobs);
  thread-pool-start(thread-pool);

  let target-final-command = make(<object-table>);
  let output-queue = make(<blocking-deque>);
  let targets-lock = make(<lock>);

  let phases :: <integer> = 0;
  let completed-phases :: <integer> = 0;
  let ok? = #f;

  local
    method bind(name :: <byte-string>) => (target :: <jam-target>);
      bind-aux(#f, jam-target(jam, name))
    end method,
    
    method bind-aux
        (parent-target :: false-or(<jam-target>), target :: <jam-target>)
     => (target :: <jam-target>);
      if (target.target-build-status = $build-status-init)
        target.target-build-status := $build-status-making;
      
        if (target.target-file?)
          jam-target-bind-aux(jam, target.target-name, target);
          if (target.target-modification-date)
            target-header-scan(jam, target);
          end if;
        end if;
        
        let time-target
          = if (target.target-internal? & parent-target)
              parent-target
            elseif (target.target-temporary?
                      & ~target.target-modification-date
                      & parent-target
                      & parent-target.target-modification-date)
              parent-target
            else
              target
            end;
        
        // recursively bind dependencies
        do(curry(bind-aux, time-target), target.target-depends);
        let seen = make(<object-set>);
        for(depend in target.target-depends)
          bind-aux(time-target, depend);
        end for;
        add!(seen, target);

        for (invocation :: <jam-action-invocation> in target.target-action-invocations)
          for (invocation-target :: <jam-target> in invocation.action-invocation-targets)
            unless (member?(invocation-target, seen))
              for (depend in invocation-target.target-depends)
                bind-aux(time-target, depend)
              end for;
              add!(seen, invocation-target);
            end unless;
          end for;
        end for;
        
        // bind includes target
        if (target.target-includes-target)
          bind-aux(parent-target, target.target-includes-target);
        end if;
        
        // depend on dependencies' includes
        let includes
          = choose(identity, map(target-includes-target,
                                 target.target-depends));
        concatenate!(target.target-depends, includes);

        // find worst-case dependent modification date and build state
        let last :: <date> = $zero-date;
        let leaf :: <date> = $zero-date;
        let status = $build-status-stable;
        
        for (depend :: <jam-target> in target.target-depends)
          leaf := max(leaf, depend.target-leaf-date);
          if (target.target-leaf-only?)
            last := leaf;
          else
            if (depend.target-check-timestamps?
                  & depend.target-modification-date)
              last := max(last, depend.target-modification-date);
            end if;
            status := max(status, depend.target-build-status)
          end if;
        end for;

        // headers
        let header-last
          = if (target.target-includes-target)
              target.target-includes-target.target-modification-date
            else
              $zero-date;
            end if;

        // NoUpdate targets
        if (~target.target-check-timestamps?)
          last := $zero-date;
          status := $build-status-stable;
        end if;

        status :=
          case
            status >= $build-status-missing =>
              $build-status-nomake;

            status >= $build-status-temp =>
              $build-status-update;

            target.target-file? & ~target.target-modification-date =>
              $build-status-missing;

            target.target-file? & last > target.target-modification-date =>
              $build-status-outdated;

            time-target.target-modification-date
              & (last > time-target.target-modification-date
                   | header-last > time-target.target-modification-date) =>
              $build-status-needtmp;

            target.target-always-build?
              | (force? & target.target-check-timestamps?) =>
              $build-status-touched;

            target.target-check-timestamps?
              & target.target-modification-date
              & parent-target
              & parent-target.target-check-timestamps?
              & parent-target.target-modification-date
              & target.target-modification-date >
                  parent-target.target-modification-date =>
              $build-status-newer;

            otherwise =>
              $build-status-stable;
          end case;

        if (status = $build-status-missing
              & target.target-depends.empty?
              & target.target-action-invocations.empty?)
          if (target.target-allow-nonexistent?)
            status := $build-status-stable;
          else
            cerror("build remaining targets",
                   "don't know how to build %s", target.target-name);
            status := $build-status-nofind;
          end if;
        end if;

        if (target.target-modification-date)
          target.target-modification-date
            := max(target.target-modification-date, last);
        end if;
        target.target-leaf-date
          := if (leaf ~= $zero-date)
               leaf
             elseif (target.target-modification-date)
               target.target-modification-date
             else
               $zero-date
             end;
        target.target-build-status := status;
      elseif (target.target-build-status = $build-status-making
                & ~target.target-internal?)
        signal("target %s depends on itself", target.target-name);
      end if;
      target
    end method,

    method expand
        (target :: <jam-target>, successor :: <jam-action-command>)
     => ();
      if (target.target-build-progress == #"init")
        target.target-build-progress := #"onstack";

        unless (target.target-build-status > $build-status-init)
          error("target %s was never bound", target.target-name);
        end;

        let current-successor :: <jam-action-command> = successor;
        if (target.target-build-status >= $build-status-touched)
          with-jam-target(jam, target)
            for (invocation :: <jam-action-invocation>
                   in target.target-action-invocations
                   using backward-iteration-protocol)
              unless (invocation.action-invocation-subsumed?)
                let action = invocation.action-invocation-action;
                let targets = invocation.action-invocation-targets;
                let sources = invocation.action-invocation-sources;

                // handle "actions together"
                if (action.action-together?)
                  for (other-invocation :: <jam-action-invocation>
                         in target.target-action-invocations)
                    if (other-invocation ~== invocation
                          & ~other-invocation.action-invocation-subsumed?
                          & action == other-invocation.action-invocation-action
                          & targets = other-invocation.action-invocation-targets)
                      sources := concatenate(sources,
                                             other-invocation.action-invocation-sources);
                      other-invocation.action-invocation-subsumed? := #t;
                    end if;
                  end for;
                end if;

                // create bound versions of $(<) and $(>)
                let bound-targets
                  = bind-targets(jam, targets);
                let bound-sources
                  = bind-targets(jam, sources,
                                 existing?: action.action-existing?,
                                 updated?: action.action-updated?);

                let variables
                  = concatenate(#["<", ">", "1", "2"], action.action-bindlist);
                let outer-values
                  = map(method (variable :: <string>)
                          jam-variable(jam, variable, default: #f)
                        end,
                        variables);
                jam-variable(jam, "<")
                  := jam-variable(jam, "1") := bound-targets;
                jam-variable(jam, ">")
                  := jam-variable(jam, "2") := bound-sources;

                // bind variables specified in the action's bind list
                for (variable in action.action-bindlist)
                  let value-as-targets
                    = map(curry(jam-target, jam), jam-variable(jam, variable));
                  jam-variable(jam, variable)
                    := bind-targets(jam, value-as-targets);
                end for;

                // Build the progress message that will be emitted
                // when action execution begins
                let message :: false-or(<string>)= #f;
                unless (action.action-quietly?)
                  message := action.action-name;
                  for (bound-target in bound-targets)
                    message := concatenate(message, " ", bound-target);
                  end for;
                  unless (bound-sources.empty?)
                    message := concatenate(message, " :");
                    for (bound-source in bound-sources)
                      message := concatenate(message, " ", bound-source);
                    end for;
                  end unless;
                end unless;

                // Construct the command
                let command-string
                  = substitute-command(jam, action.action-commands);
                let command
                  = make(<jam-action-command>,
                         string: command-string,
                         message: message,
                         targets: targets,
                         ignore?: action.action-ignore?);
                phases := phases + 1;

                // restore values
                for(variable in variables, outer-value in outer-values)
                  jam-variable(jam, variable) := outer-value;
                end for;

                // Note the final command for each target
                for (invocation-target in targets)
                  unless (element(target-final-command, invocation-target,
                                  default: #f))
                    target-final-command[invocation-target] := command;
                  end unless;
                end for;

                // Make the successor command depend on this one
                action-command-add-successor(command, current-successor);
                current-successor := command;
              end unless;
            end for;
          end with-jam-target;
        end if;

        // If there were no commands that needed executing, create a
        // dependency placeholder command
        if (current-successor == successor)
          let placeholder-command
            = make(<jam-action-command>,
                   string: #f, message: #f,
                   targets: vector(target),
                   ignore?: #f);
          target-final-command[target] := placeholder-command;

          // Make the successor command depend on this one
          action-command-add-successor(placeholder-command, current-successor);
          current-successor := placeholder-command;
        end if;

        // Expand direct dependencies
        for (depend in target.target-depends)
          expand(depend, current-successor);
        end for;

        // Expand invocation dependencies
        for (invocation :: <jam-action-invocation> in target.target-action-invocations)
          for (invocation-target :: <jam-target> in invocation.action-invocation-targets)
            for (depend :: <jam-target> in invocation-target.target-depends)
              expand(depend, current-successor);
            end for;

            // Dependencies are now accounted for, so advance the
            // target to active state
            invocation-target.target-build-progress := #"active";
          end for;
        end for;

        // If the first command has no dependencies on the execution
        // of another command, we can start executing it immediately
        if (zero?(current-successor.action-command-predecessor-count))
          thread-pool-add(thread-pool,
                          curry(execute-command, current-successor));
        end if;
      else
        // We've already expanded this target; add a dependency
        // relationship
        if (target.target-build-status >= $build-status-touched)
          let final = element(target-final-command, target, default: #f);
          if (final)
            action-command-add-successor(final, successor);
          end if;
        end if;
      end if;
    end method,

    method execute-command
        (command :: <jam-action-command>) => ();
      if (command.action-command-string)
        // Emit the message
        let message = command.action-command-message;
        if (message)
          let phase = with-lock (targets-lock) completed-phases end;
          push-last(output-queue,
                    curry(progress-callback, message, phase: phase));
        end if;

        // Run the action
        let status
          = run-application(command.action-command-string,
                            under-shell?: #t,
                            inherit-console?: #f,
                            outputter: curry(command-outputter, command),
                            hide?: #t);

        // If there was any output from this command send to the
        // progress callback now.
        if (slot-initialized?(command, action-command-output-stream))
          let message = stream-contents(command.action-command-output-stream);
          if (status = 0)
            push-last(output-queue, curry(progress-callback, message));
          else
            push-last(output-queue, curry(progress-callback, message, error?: #t));
          end if;
        end if;

        with-lock (targets-lock)
          complete-command(command, status);
        end with-lock;
      else
        // This was an internal dependency marker for a target that
        // didn't require any actual command executions
        with-lock (targets-lock)
          complete-command(command, 0);
        end with-lock;
      end if;
    end method,

    method command-outputter (command :: <jam-action-command>,
                              msg :: <byte-string>, #key end: _end)
      if (slot-initialized?(command, action-command-output-stream))
        write(command.action-command-output-stream, msg, end: _end);
      else
        command.action-command-output-stream
          := make(<byte-string-stream>,
                  direction: #"output",
                  contents: copy-sequence(msg, end: _end));
      end if;
    end method,

    method complete-command (command :: <jam-action-command>,
                             status :: <integer>)
      //---*** need to verify targets were really built
      //       and (possibly) reset the modification date
      for (target in command.action-command-targets)
        target.target-build-progress := #"done";
      end for;

      if (command.action-command-string)
        completed-phases := completed-phases + 1;
      end;

      if (status ~= 0 & ~command.action-command-ignore?)
        let message
          = concatenate("Command failed: ", command.action-command-string);
        push-last(output-queue, curry(progress-callback, message, error?: #t));

        // Stop the build
        push-last(output-queue, #f);
      else
        // If any of the successors now have all of their
        // dependencies satisfied, then add their execution to the
        // thread pool.
        for (successor :: <jam-action-command> in command.action-command-successors)
          successor.action-command-predecessor-count
            := successor.action-command-predecessor-count - 1;
          if (zero?(successor.action-command-predecessor-count))
            if (successor.action-command-string)
              thread-pool-add(thread-pool, curry(execute-command, successor));
            else
              // This is a placeholder, so complete it immediately
              complete-command(successor, status);
            end if;
          end if;
        end for;
      end if;

      // Stop the build if this was the sentinel
      if (empty?(command.action-command-targets))
        push-last(output-queue, #f);
        ok? := #t;
      end if;
    end method;

  // first pass
  let targets = map(bind, target-names);

  // second pass
  let sentinel-command
    = make(<jam-action-command>,
           string: #f, message: #f, targets: #[], ignore?: #f);
  with-lock (targets-lock)
    let section-header
      = with-output-to-string (s)
          write(s, "Building targets:");
          for (target in targets)
            write-element(s, ' ');
            write(s, target.target-name);

            // Recursively expand commands and their dependencies,
            // construct the action-command successor relationship,
            // and count the total number of phases
            expand(target, sentinel-command)
          end for;
          write(s, " within ");
          write(s, as(<string>, working-directory()));
        end;
    progress-callback(section-header, phases: phases);
  end with-lock;

  // Process thunks posted to the output queue
  unless (zero?(sentinel-command.action-command-predecessor-count))
    block (done)
      while (#t)
        let thunk = blocking-pop(output-queue);
        if (thunk)
          thunk();
        else
          done()
        end if;
      end while;
    end block;
  end unless;
  thread-pool-stop(thread-pool);

  // clean up temporary files
  jam-clean-temporary-files(jam);

  ok?
end method;

define method bind-targets
    (jam :: <jam-state>, targets :: <sequence>, #key existing?, updated?)
 => (result :: <sequence>);
  collecting ()
    for (target :: <jam-target> in targets)
      let locator = jam-target-bind-aux(jam, target.target-name, target);
      if ((~existing? | target.target-modification-date)
            & (~updated? | target.target-build-status > $build-status-stable))
        collect(as(<string>, locator));
      end if;
    end for;
  end collecting;
end method;



// Expand a command string using Jam's special rules for actions.
//
define method substitute-command
    (jam :: <jam-state>, command :: <string>)
 => (result :: <byte-string>);
  let command-size = command.size;
  let result :: <byte-string>
    = make(<byte-string>, size: truncate/(command-size * 3, 2));
  let result-size :: <integer> = 0;

  local
    method accumulate
        (str :: <string>, start :: <integer>, _end :: <integer>)
     => ();
      let str-size :: <integer> = _end - start;
      if (result-size + str-size > result.size)
        let new-size = max(truncate/(result.size * 3, 2),
                           result.size + str-size);
        let new-result = make(<byte-string>, size: new-size);
        copy-bytes(new-result, 0, result, 0, result.size);
        result := new-result;
      end if;
      copy-bytes(result, result-size, str, start, str-size);
      result-size := result-size + str-size;
    end method,

    method whitespace?(c :: <character>) => (result :: <boolean>);
      c == ' ' | c == '\t' | c == '\r' | c == '\n'
    end method;

  // trim leading and trailing whitespace
  let command-start
    = for(i from 0 below command-size, while: whitespace?(command[i]))
      finally i;
      end for;

  let command-end
    = for(i from command-size above command-start by -1,
          while: whitespace?(command[i - 1]))
      finally i;
      end for;

  iterate loop(i :: <integer> = command-start,
               start :: <integer> = command-start,
               wordstart :: false-or(<integer>) = command-start)
    if (i == command-end)
      accumulate(command, start, i);
    elseif (whitespace?(command[i]))
      loop(i + 1, start, #f);
    elseif (command[i] == '$')
      let wordstart = wordstart | i;
      let wordend
        = for(j from i + 1 below command-end, until: whitespace?(command[j]))
          finally j;
          end for;
      accumulate(command, start, wordstart);

      let expansion = jam-expand-arg(jam, command,
                                     start: wordstart, end: wordend);
      for (word in expansion, space? = #f then #t)
        if (space?) accumulate(" ", 0, 1) end;
        accumulate(word, 0, word.size);
      end for;
      loop(wordend, wordend, #f);
    else
      loop(i + 1, start, wordstart | i);
    end if;
      
  end iterate;

  copy-sequence(result, end: result-size)
end method;
