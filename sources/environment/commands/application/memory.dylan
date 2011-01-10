Module:    environment-application-commands
Synopsis:  The application commands provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Debugging properties

// Registers property

define class <registers-property> (<application-property>)
end class <registers-property>;

define command-property registers => <registers-property>
  (summary:       "Application registers",
   documentation: "The current contents of the application's registers.")
end command-property registers;

define method show-property
    (context :: <environment-context>, property :: <registers-property>)
 => ()
  let project = context.context-project;
  let application-context = context.context-application-context;
  let thread = application-context.context-thread;
  let frame = context.ensure-context-stack-frame;
  let module = context.context-project-context.context-module;
  let registers = project.application-registers;
  let stream = context.context-server.server-output-stream;
  print-table(stream, registers,
	      label-key: method (register :: <register-object>)
			   print-environment-object-to-string
			     (project, register, namespace: module)
			 end,
	      value-key: method (register :: <register-object>)
			   let value
			     = register-contents
			         (project, register, thread,
				  stack-frame-context: frame);
			   print-environment-object-to-string
			     (project, value, namespace: module)
			 end,
	      separator: " - ")
end method show-property;


/// Display memory command

define class <display-memory-command> (<application-command>)
  constant slot %object :: <environment-object>,
    required-init-keyword: object:;
end class <display-memory-command>;

define command-line display => <display-memory-command>
    (summary:       "displays application memory",
     documentation: "Displays application memory.")
  argument object :: <application-object> = "the object to examine";
end command-line display;

define method do-execute-command
    (context :: <environment-context>, command :: <display-memory-command>)
 => ()
  let project = context.context-project;
  let module = context.context-project-context.context-module;
  let group-size :: <integer> = 4;
  let memory-size :: <integer> = 20 * group-size;
  let object = command.%object;
  let address-object = application-object-address(project, object);
  if (address-object)
    let stream = context.context-server.server-output-stream;
    let address-string
      = environment-object-primitive-name(project, address-object);
    let address = string-to-machine-word(address-string);
    for (index :: <integer> from 0 below memory-size by group-size)
      let strings
	= address-read-memory-contents
	    (project, address-object,
	     size:       #"word",
	     format:     #"hexadecimal",
	     from-index: index,
	     to-index:   index + group-size);
      format(stream, "%s  ", mw/+(address, as(<machine-word>, index)));
      for (string :: <string> in strings)
	format(stream, "%s ", string)
      end;
      let ascii-characters
	= address-read-memory-contents
	    (project, address-object,
	     size:       #"byte",
	     format:     #"byte-character",
	     from-index: index,
	     to-index:   index + group-size);
      format(stream, "    ");
      for (string :: <string> in strings)
	format(stream, "%s", string)
      end;
      new-line(stream)
    end
  else
    message(context, "No address available for '%s'",
	    environment-object-display-name(project, object, module))
  end
end method do-execute-command;


/// Nearto application command

define class <near-to-object-command> (<application-command>)
  constant slot %object :: <environment-object>,
    required-init-keyword: object:;
end class <near-to-object-command>;

define command-line nearto => <near-to-object-command>
    (summary:       "shows the nearest symbol to an object",
     documentation: "Shows the nearest symbol to an object.")
  argument object :: <environment-object> = "the object to find a symbol for";
end command-line nearto;

define method do-execute-command
    (context :: <environment-context>, command :: <near-to-object-command>)
 => ()
  error("Not yet implemented!")
end method do-execute-command;


///---*** To do

/*
  show-multiple-values

  print
  describe
  explode

  walk
  evaluate
  heap-statistics
*/


/// Debugging commands

define command-group memory into environment
    (summary: "memory viewing commands",
     documentation: "Commands to view the memory of an application.")
  property registers;
  command  display;
  command  nearto;
end command-group memory;
