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
  *program-failed?* := #f;
  process-command-line-options();
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

  if (*program-failed?*)
    debugger-message("Debugger is returning exit-code -1.");
    exit-application(-1)
  end if; 

end method;


///// DO-THE-DEVEL-DEBUGGER
//    Intializes, and goes into global mode.
//    (Also ignores a few bindings that currently aren't relevant to batch
//    mode).

define method do-the-devel-debugger () => ()
  ignore(create-and-process-stop-button);
  ignore(fetch-debugger-command);
  ignore(*current-dylan-string*);
  ignore(initialize-command-reader);
  ignore(string-complete?);
  ignore(welcome-banner);
  ignore(c-signal-stop-button);
  setup-debugger();
  process-global-mode();
end method;


///// Get on with it, then, ya bastid!

do-the-devel-debugger ();


//eof
