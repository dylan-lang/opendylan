Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// jam-target-build
//
define method jam-target-build
    (jam :: <jam-state>, target-names :: <sequence>,
     #key force?,
          progress-callback :: <function> = ignore)
 => (build-successful? :: <boolean>);
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

        for (invocation in target.target-action-invocations)
          for (target in invocation.action-invocation-targets)
            unless (member?(target, seen))
              for(depend in target.target-depends)
                bind-aux(time-target, depend)
              end for;
              add!(seen, target);
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
        
        for (depend in target.target-depends)
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
              $build-status-old;

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

    // This is somewhat simplistic compared to mmk or the original Jam.
    // We can do better... 
    //
    method build(target :: <jam-target>) => (success? :: <boolean>);
      block (return)
        if (target.target-build-progress == #"done")
          return(target.target-build-execution-result == #"ok");
        elseif (target.target-build-progress ~== #"init")
          return(#t);
        end if;

        target.target-build-progress := #"onstack";

        let failed? = #f;
        let seen = make(<object-set>);

        for(depend in target.target-depends)
          unless (build(depend))
            failed? := #t;
          end unless;
        end for;
        add!(seen, target);
        if (failed?) return(#f) end;
        
        for (invocation in target.target-action-invocations)
          for (target in invocation.action-invocation-targets)
            unless (member?(target, seen))
              for(depend in target.target-depends)
                unless (build(depend))
                  failed? := #t;
                end unless;
              end for;
              add!(seen, target);
            end unless;
          end for;
        end for;
        if (failed?) return(#f) end;
        
        target.target-build-progress := #"active";

        unless (target.target-build-status > $build-status-init)
          error("target %s was never bound", target.target-name);
        end;
        
        if (target.target-build-status > $build-status-stable)
          with-jam-target(jam, target)
            for (invocation in target.target-action-invocations)
              unless (invocation.action-invocation-subsumed?)
                let action = invocation.action-invocation-action;
                let targets = invocation.action-invocation-targets;
                let sources = invocation.action-invocation-sources;

                // handle "actions together"
                if (action.action-together?)
                  for (other in target.target-action-invocations)
                    if (other ~== invocation
                          & ~other.action-invocation-subsumed?
                          & action == other.action-invocation-action
                          & targets = other.action-invocation-targets)
                      sources := concatenate(sources,
                                             other.action-invocation-sources);
                      other.action-invocation-subsumed? := #t;
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
                
                let command = substitute-command(jam, action.action-commands);

                // restore values
                for(variable in variables, outer-value in outer-values)
                  jam-variable(jam, variable) := outer-value;
                end for;

                // progress message callback
                unless (action.action-quietly?)
                  let message = action.action-name;
                  for (bound-target in bound-targets)
                    message := concatenate(message, " ", bound-target);
                  end for;
                  unless (bound-sources.empty?)
                    message := concatenate(message, " :");
                    for (bound-source in bound-sources)
                      message := concatenate(message, " ", bound-source);
                    end for;
                  end unless;
                  progress-callback(message);
                end unless;

                // run the action
                let status
                  = run-application(command,
                                    under-shell?: #t,
                                    inherit-console?: #f,
                                    outputter: outputter,
                                    hide?: #t);

                // clean up temporary files
                jam-clean-temporary-files(jam);

                //---*** need to verify targets were really built
                //       and (possibly) reset the modification date
                for (target in targets)
                  target.target-build-progress := #"done";
                  if (status ~= 0 & ~action.action-ignore?)
                    target.target-build-execution-result := #"fail";
                  end if;
                end for;

                if (status ~= 0 & ~action.action-ignore?)
                  cerror("build remaining targets",
                         "Building target %s: \"%s\" failed "
                           "with return status %d",
                         target.target-name, command, status);
                  return(#f);
                end if;
              end unless;
            end for;
          end with-jam-target;
        end if;

        target.target-build-progress := #"done";

        #t;
      end block;
    end method,

    method outputter (msg :: <byte-string>, #key end: _end)
      progress-callback(copy-sequence(msg, end: _end));
    end method;

  // first pass
  let targets = map(bind, target-names);

  // second pass
  let ok? = #t;
  for (target in targets)
    if(~build(target))
      ok? := #f;
    end if;
  end for;
  ok?
end method;

define method bind-targets
    (jam :: <jam-state>, targets :: <sequence>, #key existing?, updated?)
 => (result :: <sequence>);
  collecting ()
    for (target in targets)
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
