Module:    command-shell
Synopsis:  Generic commands
Author:    Roman Budzianowski, Andy Armstrong, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define command-argument command;

define command-argument restart-option;


define dylan-shell-command
  help (command)
  description "displays help for commands"
  documentation
  "Usage: HELP\n"
  "       HELP command\n"
  "\n"
  "If specified with no arguments, HELP shows a list of all commands\n"
  "with a one line description. If specified with a command name, HELP\n"
  "will show documentation for that command.\n"
  hidden? #f
  if (command)
    display-command-help(context, as(<symbol>, command))
  else
    display-help(context)
  end
end;

define dylan-shell-command
  help-all ()
  description "shows help for internal and external commands"
  documentation
  "Display Help Information for all commands.\n"
  hidden? #t
  display-help(context, test: identity)
end;

define dylan-shell-command 
  exit()
  description "exits the application"
  documentation
  "Usage: EXIT\n"
  "\n"
  "Exits the application.\n"
  hidden? #f
  exit-command-loop(context)
end;

define shell-command-alias 
  quit => exit
end;


// Recovery Protocol

define dylan-shell-command 
  continue (restart-option)
  description "selects a continuation restart"
  documentation
  "When the application has crashed, this operation will allow you to\n"
  "choose between one of the available restarts by specifying the\n"
  "restarts number. Typically you will just want to abort the operation\n"
  "for which the command ABORT is provided (use HELP for more details).\n"
  hidden? #f
  invoke-restart(restart-option);
end;

define dylan-shell-command 
  debug ()
  description "debugs a crashed compilation"
  documentation
  "When the application has crashed, this command will allow you to enter\n"
  "a debugger so that you can determine what is happening. Generally\n"
  "speaking, you should use the batch-debug application as your\n"
  "debugger, as this will produce a bug report that you can send\n"
  "to Functional Objects. See the documentation for more details\n"
  hidden? #f
  invoke-restart(0);
end;

define dylan-shell-command 
  abort ()
  description "aborts a crashed compilation"
  documentation
  "When the application has crashed, this command will allow you to abort\n"
  "that operation so that you can continue without having to restart\n"
  "the application from scratch. If you'd like to report the problem,\n"
  "you should use the DEBUG command while you are running the application\n"
  "inside a debugger (see the batch-debug application that is provided\n"
  "to help you to generate bug reports).\n"
  hidden? #f
  signal(make(<abort>));
end;

define dylan-shell-command 
  restarts ()
  description "displays available restarts"
  documentation
  "When the application has crashed, this command will display all of\n"
  "the available restarts. You can pick one of them using the CONTINUE\n"
  "command, or you can use ABORT or DEBUG to abort the operation or\n"
  "debug it respectively.\n"
  hidden? #f
  print-restart-options();
end;

define shell-command-alias 
  c => continue
end;

define shell-command-alias 
  d => debug
end;

define shell-command-alias 
  a => abort
end;
