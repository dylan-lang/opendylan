module:     devel-dbg-ui
synopsis:   Unpicking command-line options for the batch debugger.
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// *CURRENT-SCRIPT-PATH*
//    Whenever the debugger executes a sequence of commands from a
//    script, the path from which the script came is pushed onto
//    this stack. This is so that any references to other scripts
//    that don't contain explicit paths can be resolved in a natural
//    way, by first looking in the directory of the current script.

// define variable *current-script-path* = make(<stretchy-vector>);


define constant $dylan-lib = "dxdylan";

///// <THREAD-REPORT-DETAIL>

define constant <thread-report-detail> = 
   one-of(#"ignore", #"summary", #"backtrace", #"verbose");


///// <DEBUGGER-OPTION-SET>
//    An object encapsulating all options used to control the debugger.

define class <debugger-option-set> (<object>)

  slot signalling-thread-detail :: <thread-report-detail>,
    init-value: #"verbose",
    init-keyword: signalling-thread-detail:;

  slot other-threads-detail :: <thread-report-detail>,
    init-value: #"ignore",
    init-keyword: other-threads-details:;

  slot spawn-new-console? :: <boolean>,
    init-value: #f,
    init-keyword: spawn-new-console?:;

  slot pre-command-sequence-index = 0;

  constant slot pre-command-sequence :: <sequence> = make(<stretchy-vector>),
    init-keyword: pre-command-sequence:;

  constant slot post-command-sequence :: <sequence> = make(<stretchy-vector>),
    init-keyword: post-command-sequence:;

  slot target-process :: false-or(<string>),
    init-value: #f,
    init-keyword: target-process:;

  slot arguments-for-target-process :: <string>,
    init-value: "",
    init-keyword: arguments-for-target-process:;

  slot jit-debug? :: <boolean>,
    init-value: #f,
    init-keyword: jit-debug?:;

  slot stack-trace-limit :: <integer>,
    init-value: 1000,
    init-keyword: stack-trace-limit:;

  slot large-object-threshold :: <integer>,
    init-value: 35,
    init-keyword: large-object-threshold:;

  slot show-debug-output? :: <boolean>,
    init-value: #t,
    init-keyword: show-debug-output?:;

  slot show-application-messages? :: <boolean>,
    init-value: #t,
    init-keyword: show-application-messages?:;

  slot show-dylan-debug-messages? :: <boolean>,
    init-value: #t,
    init-keyword: show-dylan-debug-messages?:;

  slot stop-at-library-initializations? :: <boolean>,
    init-value: #f,
    init-keyword: stop-at-library-initializations?:;

  slot stop-at-system-initialization? :: <boolean>,
    init-value: #t,
    init-keyword: stop-at-system-initialization?:;

  slot attempt-recovery? :: <boolean>,
    init-value: #f,
    init-keyword: attempt-recovery?:;

  slot component-name :: false-or(<string>),
    init-value: #f,
    init-keyword: component-name:;

  slot stop-for-interaction? :: <boolean>,
    init-value: #f,
    init-keyword: stop-for-interaction?:;

  slot name-of-dylan-component :: <string>,
    init-value: $dylan-lib,
    init-keyword: name-of-dylan-component:;

end class;


///// PROCESS-COMMAND-LINE-OPTIONS
//    Modifies *current-debugger-options* by parsing and processing the
//    command line that was passed to the debugger. It is vital that
//    only those options _preceeding_ the name of the target program are
//    taken into account.

define method process-command-line-options () => ()

  local method user-default-options () => (opt :: <vector>)
          block ()
            let sv = make(<stretchy-vector>);
            with-open-file (stream = $debugger-initialization-script-name,
                            direction: #"input")
              let opt = read-line(stream, on-end-of-stream: #f);
              while (opt)
                add!(sv, opt);
                opt := read-line(stream, on-end-of-stream: #f);
              end while;
            end;
            as(<simple-object-vector>, sv);
          exception (<file-does-not-exist-error>)
            #[]
          end block
        end method;

  let argument-vector = concatenate(user-default-options(),
                                    application-arguments());
  let i = 0;
  let numargs = argument-vector.size;
  let more-options = #t;

  local method sanity-check-detail (detail :: <string>) => (well? :: <boolean>)
          (detail = "ignore") |
          (detail = "summary") |
          (detail = "backtrace") |
          (detail = "verbose")
	end method;

  local method sanity-check-message-arg (detail :: <string>)
                 => (well? :: <boolean>)
          (detail = "none") |
          (detail = "application") |
          (detail = "debug") |
          (detail = "all")
        end method;

  local method alter-message-options-according-to (detail :: <symbol>) => ()
          select (detail)
            #"none" =>
              *current-debugger-options*.show-application-messages? := #f;
              *current-debugger-options*.show-dylan-debug-messages? := #f;
            #"application" =>
              *current-debugger-options*.show-application-messages? := #t;
              *current-debugger-options*.show-dylan-debug-messages? := #f;
            #"debug" =>
              *current-debugger-options*.show-application-messages? := #f;
              *current-debugger-options*.show-dylan-debug-messages? := #t;
            #"all" =>
              *current-debugger-options*.show-application-messages? := #t;
              *current-debugger-options*.show-dylan-debug-messages? := #t;
          end select
        end method;

  local method option-is (str :: <string>, opt :: <string>)
	 => (well? :: <boolean>)
          let str-size = str.size;
          let opt-size = opt.size;
          if (str-size == (1 + opt-size))
            let result = #t;
            block (exit)
              for (j from 0 below opt-size)
                if (opt[j] ~== str[j + 1])
                  result := #f;
                  exit();
		end if
	      end for;
	    end block;
            result;
          else
            #f
          end if
	end method;

  while (more-options & (i < numargs))
    let option-string = element(argument-vector, i, default: "-end");
    if ((option-string[0] == '/') | (option-string[0] == '-'))
      if (option-is(option-string, "signalling-thread"))
        i := i + 1;
        let detail = element(argument-vector, i, default: #f);
        if (detail & sanity-check-detail(detail))
          *current-debugger-options*.signalling-thread-detail := 
             as(<symbol>, detail)
	else
          debugger-message("WARNING: Bad or missing argument for the"
                           " -signalling-thread option. Ignoring.");
	end if;
        i := i + 1;
      elseif (option-is(option-string, "other-threads"))
        i := i + 1;
        let detail = element(argument-vector, i, default: #f);
        if (detail & sanity-check-detail(detail))
          *current-debugger-options*.other-threads-detail := 
             as(<symbol>, detail)
	else
          debugger-message("WARNING: Bad or missing argument for the"
                           " -other-threads option. Ignoring.");
	end if;
        i := i + 1;
      elseif (option-is(option-string, "messages"))
        i := i + 1;
        let detail = element(argument-vector, i, default: #f);
        if (detail & sanity-check-message-arg(detail))
          alter-message-options-according-to(as(<symbol>, detail));
        else
          debugger-message("WARNING: Bad or missing argument for the"
                           " -messages option. Ignoring.");
        end if;
        i := i + 1;
      elseif (option-is(option-string, "crossref-threshold"))
        i := i + 1;
        let int-string = element(argument-vector, i, default: #f);
        if (int-string)
          *current-debugger-options*.large-object-threshold :=
              as(<integer>, int-string);
	else
          debugger-message("WARNING: Missing argument for the"
                           " -crossref-threshold option. Ignoring.");
	end if;
        i := i + 1;
      elseif (option-is(option-string, "limit-stack"))
        i := i + 1;
        let int-string = element(argument-vector, i, default: #f);
        if (int-string)
          *current-debugger-options*.stack-trace-limit :=
              as(<integer>, int-string);
	else
          debugger-message("WARNING: Missing argument for the"
                           " -limit-stack option. Ignoring.");
	end if;
        i := i + 1;
      elseif (option-is(option-string, "component"))
        i := i + 1;
        *current-debugger-options*.component-name :=
           element(argument-vector, i, default: #f);
        i := i + 1;
      elseif (option-is(option-string, "dylan"))
        i := i + 1;
        *current-debugger-options*.name-of-dylan-component :=
           element(argument-vector, i, default: $dylan-lib);
        i := i + 1;
      elseif (option-is(option-string, "interact"))
        *current-debugger-options*.stop-for-interaction? := #t;
        *current-debugger-options*.stop-at-system-initialization? := #f;
        i := i + 1;
      elseif (option-is(option-string, "go"))
        *current-debugger-options*.stop-at-system-initialization? := #f;
        i := i + 1;
      elseif (option-is(option-string, "profile"))
        add!(*current-debugger-options*.pre-command-sequence,
             "profile");
        i := i + 1;
      elseif (option-is(option-string, "spawn-console"))
        *current-debugger-options*.spawn-new-console? := #t;
        i := i + 1;
      elseif (option-is(option-string, "share-console"))
        *current-debugger-options*.spawn-new-console? := #f;
        i := i + 1;
      elseif (option-is(option-string, "stop-button"))
        *stop-button* := #t;
        i := i + 1;
      elseif (option-is(option-string, "pre-command"))
        i := i + 1;
        let command = element(argument-vector, i, default: #f);
        if (command)
          add!(*current-debugger-options*.pre-command-sequence, command);
	end if;
        i := i + 1;
      elseif (option-is(option-string, "command"))
        i := i + 1;
        let command = element(argument-vector, i, default: #f);
        if (command)
          add!(*current-debugger-options*.post-command-sequence, command);
	end if;
        i := i + 1;
      elseif (option-is(option-string, "noisy"))
        *current-debugger-options*.show-debug-output? := #t;
        i := i + 1;
      elseif (option-is(option-string, "quiet"))
        *current-debugger-options*.show-debug-output? := #f;
        i := i + 1;
      elseif (option-is(option-string, "pli"))
        *current-debugger-options*.stop-at-library-initializations? := #t;
        i := i + 1;
      elseif (option-is(option-string, "recover"))
        *current-debugger-options*.attempt-recovery? := #t;
        i := i + 1;
      elseif (option-is(option-string, "end"))
        more-options := #f;
      elseif (option-is(option-string, "p"))
        i := i + 1;
        let proc-id = element(argument-vector, i, default: #f);
        if (proc-id)
          *current-debugger-options*.jit-debug? := #t;
          *current-debugger-options*.target-process := proc-id;
          i := i + 1;
        else
          debugger-message
            ("WARNING: %s is not an unsigned decimal process ID.", proc-id | "?");
        end if
      elseif (option-is(option-string, "e"))
        i := i + 1;
        let evt-id = element(argument-vector, i, default: #f);
        if (evt-id)
          *current-debugger-options*.arguments-for-target-process := evt-id;
          i := i + 1;
        else
          debugger-message
            ("WARNING: %s is not an unsigned decimal handle.", evt-id | "?");
        end if
      else
        debugger-message("WARNING: Ignoring unrecognized debugger option %s.",
                         option-string);
        i := i + 1;
      end if;
    else
      more-options := #f;
    end if;
  end while;

  if (i < numargs)
    *current-debugger-options*.target-process := argument-vector[i];
    i := i + 1;
  end if;

  // Anything else left on the command line just gets passed along to the
  // target program.

  if (i < numargs)
    *current-debugger-options*.arguments-for-target-process
       := argument-vector[i];
    i := i + 1;
    while (i < numargs)
      *current-debugger-options*.arguments-for-target-process :=
        concatenate
          (*current-debugger-options*.arguments-for-target-process,
           " ",
           argument-vector[i]);
      i := i + 1;
    end while;
  end if;
end method;


///// OPEN-DEBUGGER-SCRIPT

///// CLOSE-DEBUGGER-SCRIPT

///// READ-DEBUGGER-SCRIPT

///// FIND-DEBUGGER-SCRIPT
