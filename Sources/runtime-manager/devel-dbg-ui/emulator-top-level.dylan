module:         devel-dbg-ui
synopsis:       The main loop of execution for the devel debugger
author:         Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*

DEVELDBG - A note on threads.

There's only one. Just _try_ running this with a multi-threaded
access-path or debugger-manager, and everything will turn to
a huge pile of doggy-do.

Threads - just say no.

*/


///// *GLOBAL-QUIT*
//    A flag that remains #f until the user issues the "quit" command
//    at some point.

define variable *global-quit* = #f;


///// SETUP-DEBUGGER
//    Performs any initializations needed by the debugger UI.

define method setup-debugger () => ()
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
  while (~*global-quit*)
    *history-table* := make (<stretchy-vector>, size: 0);
    format-out("No Application. Open one, or type help > ");
    let seq = tokenize(read-command());
    if (check-lexical-correctness(seq))
      set-parse-sequence(seq);
      let command = build-debugger-command();
      execute-debugger-command (command);
      format-out ("\n");
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
end method;

//eof
