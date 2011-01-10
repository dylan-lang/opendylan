Module:    command-lines
Synopsis:  The commands provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Operating system properties

define class <working-directory-property> (<command-property>)
end class <working-directory-property>;

define command-property directory => <working-directory-property>
  (summary:       "Working directory",
   documentation: "The current working directory.",
   type: <directory-locator>)
end command-property directory;

define method show-property
    (context :: <server-context>, property :: <working-directory-property>)
 => ()
  message(context, "Directory: %s", as(<string>, working-directory()))
end method show-property;

define method set-property
    (context :: <server-context>, property :: <working-directory-property>,
     locator :: <directory-locator>,
     #key save?)
 => ()
  working-directory() := locator
end method set-property;


/// Change directory command

define class <change-directory-command> (<basic-command>)
  constant slot %directory :: false-or(<directory-locator>) = #f,
    init-keyword: directory:;
end class <change-directory-command>;

define command-line cd => <change-directory-command>
    (summary:       "changes the current working directory",
     documentation: "Changes the current working directory.")
  optional directory :: <directory-locator> = "the chosen directory";
end command-line cd;

define method do-execute-command
    (context :: <server-context>, command :: <change-directory-command>)
 => ()
  let directory = command.%directory;
  if (directory)
    context-named-property(context, #"directory") := directory
  else
    show-named-property(context, #"directory")
  end
end method do-execute-command;


/// OS command

define class <operating-system-command> (<basic-command>)
  constant slot %command :: <string>,
    required-init-keyword: command:;
end class <operating-system-command>;

define command-line os => <operating-system-command>
    (summary:       "execute an operating system command",
     documentation: "Execute an operating system command.")
  argument command :: <string> = "the OS command to execute";
end command-line os;

define method do-execute-command
    (context :: <server-context>, command :: <operating-system-command>)
 => ()
  let os-command = command.%command;
  let result
    = run-application
        (os-command,
	 under-shell?:     #t,
	 inherit-console?: #t,
	 activate?:        #f,
	 outputter: method (text :: <string>, #key end: stop)
		      message(context, "%s",
			      copy-sequence(text, end: stop))
		    end);
  unless (result == 0)
    command-error("Operation failed with status code %d", result)
  end
end method do-execute-command;


/// Operating system commands

define command-group system
    (summary: "operating system commands",
     documentation: "Operating system level commands.")
  property directory;
  command  cd;
  command  os;
end command-group system;
