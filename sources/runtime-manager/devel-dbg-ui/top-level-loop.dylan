module:         devel-dbg-ui
synopsis:       The main loop of execution for the console debugger
author:         Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// *GLOBAL-QUIT*
//    A flag that remains #f until the user issues the "quit" command
//    at some point.

define variable *global-quit* = #f;


///// *PROGRAM-FAILED?*
//    If the application signals any kind of error or exception, this
//    flag is set to #t. When the debugger exits, it will exit with a
//    negative return code if this variable flags #t.

define variable *program-failed?* = #f;


///// *CURRENT-LISTENER-MODE*
//    This flags what the default mode of the listener should be. This can
//    be to listen for debugger commands, or to listen for interactive
//    dylan code.

define constant $listen-for-commands = 0;
define constant $listen-for-dylan    = 1;

define variable *current-listener-mode* = $listen-for-commands;


///// *CURRENT-DYLAN-STRING*
//    When the listener mode is $listen-for-dylan, this string collects
//    the user's input. If the DFMC-aware version of the console debugger
//    is being run, then the command loop will detect when this string becomes
//    a complete dylan form, and send it to the compiler to be
//    evaluated.

define variable *current-dylan-string* = "";


///// SETUP-DEBUGGER
//    Performs any initializations needed by the debugger UI.

define method setup-debugger () => ()
  initialize-command-reader();
  *program-failed?* := #f;
  process-command-line-options();
  initialize-stop-button();
end method;


///// GET-NEXT-DEBUGGER-COMMAND
//    Taking into account the current listener mode, and the current
//    command list.
//    Returns #f if no well-formed command is obtained from anywhere!

define method get-next-debugger-command () 
       => (command :: false-or(<debugger-command>),
           empty? :: <boolean>,
           string-fragment :: false-or(<string>))   
  if (*current-debugger-options*.pre-command-sequence-index <
               *current-debugger-options*.pre-command-sequence.size)
    let command-string =
      *current-debugger-options*.pre-command-sequence
        [*current-debugger-options*.pre-command-sequence-index];
    set-parse-sequence(tokenize(command-string));
    let command = build-debugger-command();
    *current-debugger-options*.pre-command-sequence-index :=
       *current-debugger-options*.pre-command-sequence-index + 1;
    values(command, #f, #f)
  elseif (*command-queueing-enabled?*)
    values(fetch-debugger-command(), #f, #f);
  else
    if (*current-listener-mode* == $listen-for-commands)
      format-out("Command> ");
      let string-fragment = read-command();
      let seq = tokenize(string-fragment);
      if (seq.size < 2)
        values(#f, #t, #f)
      elseif (check-lexical-correctness(seq))
        set-parse-sequence(seq);
        values(build-debugger-command(), #f, #f);
      else
        values(#f, #f, #f)
      end if;
    else
      format-out("| ");
      let string-fragment = read-command();
      if (string-fragment.size == 0)
        values(#f, #t, "")
      elseif (string-fragment[0] == '!')
        let seq = tokenize(string-fragment);
        if (seq.size < 2)
          values(#f, #t, string-fragment)
        elseif (check-lexical-correctness(seq))
          set-parse-sequence(seq);
          values(build-debugger-command(), #f, #f)
        else
          values(#f, #f, #f)
        end if
      else
        *current-dylan-string* := 
            concatenate(*current-dylan-string*, "\n", string-fragment);
        if (string-complete?(*current-dylan-string*))
          let ex-command = 
            make(<execute-source-command>,
                 source-string: shallow-copy(*current-dylan-string*));
          *current-dylan-string* := "";
          values(ex-command, #f, #f)
        else
          values(#f, #t, string-fragment)
        end if
      end if;
    end if;
  end if;
end method;


///// PROCESS-GLOBAL-MODE
//    "Global mode" is like "stop mode" in that it is an indefinite loop
//    that reads and executes debugger commands. The difference is that
//    when executing a command in global mode, there is known to be no
//    application running. This means that "open", "quit" and "help" are
//    the only commands that will have any effect. When global mode
//    comes to an end, we are definitely quitting the debugger.
//    "open" is the only command that works _only_ in global mode.

define method process-global-mode () => ()
  *global-quit* := #f;
  let did-something = #t;
  if (*current-debugger-options*.target-process)
    let auto-command = 
      if (*current-debugger-options*.jit-debug?)
        *current-debugger-options*.stop-at-system-initialization? := #f;
        debugger-message(" *** JIT Auto-start.");
        debugger-message("     Process ID: %s",
                         *current-debugger-options*.target-process);
        debugger-message("     System Data: %s",
                         *current-debugger-options*.arguments-for-target-process);
        make(<attach-application-command>,
             id: *current-debugger-options*.target-process,
             JIT-bit: *current-debugger-options*.arguments-for-target-process);
      else
        make (<open-application-command>,
              filename:
                *current-debugger-options*.target-process,
              arguments: 
                *current-debugger-options*.arguments-for-target-process);
      end if;
     block ()
       execute-debugger-command (auto-command);
     exception (illegal-access :: <remote-access-violation-error>)
       debugger-message("*** ERROR: Tried to read illegal runtime memory.");
       debugger-message("           Aborted the execution of this command.");
     exception (bp-error :: <debug-point-error>)
       debugger-message("*** ERROR: Failed to set or clear a breakpoint.");
       debugger-message("           Aborted the execution of this command.");
     end block;
  end if;

  while (~*global-quit*)
    *history-table* := make (<stretchy-vector>, size: 0);
    if (did-something)
      debugger-message("No application open");
    else
      did-something := #t;
    end if;
    let (command, empty?, str) = get-next-debugger-command();
    if (command)
      block()
        execute-debugger-command (command);
      exception (illegal-access :: <remote-access-violation-error>)
        debugger-message
          ("*** ERROR: Tried to read illegal runtime memory.");
        debugger-message
          ("           Aborted the execution of this command.");
      exception (bp-error :: <debug-point-error>)
        debugger-message
          ("*** ERROR: Failed to set or clear a breakpoint.");
        debugger-message
          ("           Aborted the execution of this command.");
      exception(condition :: <keyboard-interrupt>)
	debugger-message("%s", condition);
        debugger-message
          ("           Aborted the execution of this command.");
      end block;
      format-out ("\n");
    elseif (empty?)
      did-something := #f;
    else
     debugger-message("Illegally formed command.\n");
    end if;
  end while
end method;


///// DO-THE-DEVEL-DEBUGGER
//    Intializes, and goes into global mode.

define method do-the-devel-debugger () => ()
  setup-debugger();
  welcome-banner();
  process-global-mode();
  // terminate-stop-button(); // Since candidate 9, this seems to crash.
  // exit-application(0);
end method;


///// Get on with it, then, ya bastid!

do-the-devel-debugger ();
