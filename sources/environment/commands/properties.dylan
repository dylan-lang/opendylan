Module:    command-lines
Synopsis:  The commands provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Properties

define open abstract class <command-property> (<basic-command-info>)
  sealed constant slot command-info-type :: false-or(<type>) = #f,
    init-keyword: type:;
  sealed constant slot command-info-persistent? :: <boolean> = #f,
    init-keyword: persistent?:;
end class <command-property>;

define method parameter-type-name
    (type == <command-property>) => (name :: <string>)
  "property"
end method parameter-type-name;

define open generic ensure-property-available
    (context :: <server-context>, property :: <command-property>)
 => ();

define open generic show-property
    (context :: <server-context>, property :: <command-property>)
 => ();

define open generic set-property
    (context :: <server-context>, property :: <command-property>, 
     value :: <object>,
     #key save?)
 => ();

define macro command-property-definer
  { define command-property ?name:name => ?class:name (?options:*)
    end }
    => { define constant "$" ## ?name ## "-command-property"
           = make(?class,
		  ?options,
		  name:          ?#"name",
		  title:         as-uppercase(?"name")) }
end macro command-property-definer;

define method ensure-property-available
    (context :: <server-context>, property :: <command-property>)
 => ()
  #f
end method ensure-property-available;

define function find-named-property
    (context :: <server-context>, name :: <symbol>)
 => (property :: <command-property>)
  let group = context.context-command-group;
  let property = find-command-info(group, name, type: <command-property>);
  property
    | command-error("No such property '%s'", name)
end function find-named-property;

define function show-named-property
    (context :: <server-context>, name :: <symbol>)
 => ()
  let property = find-named-property(context, name);
  ensure-property-available(context, property);
  show-property(context, property)
end function show-named-property;

define function set-named-property
    (context :: <server-context>, name :: <symbol>, value :: <object>,
     #key save?)
 => ()
  let property = find-named-property(context, name);
  ensure-property-available(context, property);
  set-property(context, property, value, save?: save?)
end function set-named-property;


/// Properties

define class <properties-property> (<command-property>)
end class <properties-property>;

define command-property properties => <properties-property>
  (summary:       "Command-line properties",
   documentation: "The set of command-line properties.")
end command-property properties;

define method show-property
    (context :: <server-context>, property :: <properties-property>)
 => ()
  let group = context.context-command-group;
  let stream = context.context-server.server-output-stream;
  let properties = collect-command-info(group, <command-property>);
  print-command-info(stream, properties)
end method show-property;


/// Copyright property

define class <copyright-property> (<command-property>)
end class <copyright-property>;

define command-property copyright => <copyright-property>
  (summary:       "Copyright information",
   documentation: "Copyright information.")
end command-property copyright;

define method show-property
    (context :: <server-context>, property :: <copyright-property>)
 => ()
  let stream = context.context-server.server-output-stream;
  format(stream, release-copyright());
end method;


/// Commands

define class <commands-property> (<command-property>)
end class <commands-property>;

define command-property commands => <commands-property>
  (summary:       "Command-line commands",
   documentation: "The set of commands.")
end command-property commands;

define method show-property
    (context :: <server-context>, property :: <commands-property>)
 => ()
  let group = context.context-command-group;
  let stream = context.context-server.server-output-stream;
  let commands = collect-command-info(group, <basic-command-line>);
  print-command-info(stream, commands)
end method show-property;


/// Errors

define class <set-property-error> (<format-string-condition>, <error>)
end class <set-property-error>;

define method set-error
    (format-string :: <string>, #rest format-arguments)
  error(make(<set-property-error>, 
	     format-string: format-string,
	     format-arguments: format-arguments))
end method set-error;


/// Property commands

define class <show-property-command> (<basic-command>)
  constant slot %property :: <command-property>,
    required-init-keyword: property:;
end class <show-property-command>;

define command-line show => <show-property-command>
    (summary:       "shows the specified property",
     documentation: "Shows the specified property.")
  argument property :: <command-property> = "the property to show";
end command-line show;

define class <set-property-command> (<basic-command>)
  constant slot %property :: <command-property>,
    required-init-keyword: property:;
  constant slot %value :: <string>,
    required-init-keyword: value:;
  constant slot %save? :: <boolean> = #f,
    init-keyword: save?:;
end class <set-property-command>;

define command-line set => <set-property-command>
    (summary:       "sets the specified property",
     documentation: "Sets the specified property to a given value.")
  argument property :: <command-property> = "the property to set";
  argument value :: <string> = "the value to set it to";
  flag save = "save the new setting persistently [off by default]";
end command-line set;

define sealed method do-execute-command
    (context :: <server-context>, command :: <show-property-command>)
 => ()
  let property = command.%property;
  ensure-property-available(context, property);
  show-property(context, property)
end method do-execute-command;

define method context-property-setter
    (value :: <object>, context :: <server-context>, 
     property :: <command-property>,
     #key save?)
 => (value :: <object>)
  ensure-property-available(context, property);
  set-property(context, property, value, save?: save?);
  show-property(context, property);
  value
end method context-property-setter;

define method context-named-property-setter
    (value :: <object>, context :: <server-context>, name :: <symbol>,
     #key save?)
 => (value :: <object>)
  let property = find-named-property(context, name);
  context-property(context, property, save?: save?) := value
end method context-named-property-setter;

define sealed method do-execute-command
    (context :: <server-context>, command :: <set-property-command>)
 => ()
  let property = command.%property;
  ensure-property-available(context, property);
  let type = property.command-info-type;
  if (type)
    let value-string = command.%value;
    let save? = command.%save?;
    block (return)
      let value
	= block ()
	    parse-next-argument(context, type, value-string);
	  exception (error :: <command-line-server-error>)
	    display-command-line-server-error(context, error);
	    return();
	  end;
      block ()
	if (save? & ~property.command-info-persistent?)
	  message(context, "Setting value, but property cannot be saved")
	end;
	context-property(context, property, save?: save?) := value
      exception (error :: <set-property-error>)
	message(context, "%s", error)
      end
    end
  else
    command-error("Property '%s' cannot be changed", property.command-info-title)
  end
end method do-execute-command;


/// Property commands

define command-group property 
    (summary: "property handling commands",
     documentation: "Commands to manipulate properties.")
  property properties;
  property copyright;
  property commands;
  command  set;
  command  show;
end command-group property;
