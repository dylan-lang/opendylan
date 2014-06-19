Module:    command-lines
Synopsis:  The commands provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Exit command

define class <exit-command> (<basic-command>)
end class <exit-command>;

define command-line exit => <exit-command>
    (summary:       "exits the command loop",
     documentation: "Exits the command loop.")
end command-line exit;


/// Version command
define class <version-command> (<basic-command>)
  constant slot %short :: false-or(<string>) = #f,
    init-keyword: short:;
end class <version-command>;

define command-line version => <version-command>
    (summary:
       "displays the version",
     documentation:
       "VERSION shows the current version of the compiler.\n")
  optional short :: <string> = "print short version";
end command-line version;

define sealed method do-execute-command
    (context :: <server-context>, command :: <version-command>)
 => ()
  let ver = if (command.%short)
              release-short-version()
            else
              release-version()
            end;
  message(context, ver)
end method do-execute-command;


/// Help command

define class <help-command> (<basic-command>)
  constant slot %title :: false-or(<string>) = #f,
    init-keyword: title:;
  constant slot %group :: false-or(<command-group>) = #f,
    init-keyword: group:;
  constant slot %command :: false-or(<command-line>) = #f,
    init-keyword: command:;
  constant slot %property :: false-or(<command-property>) = #f,
    init-keyword: property:;
end class <help-command>;

define command-line help => <help-command>
    (summary:
       "displays help for commands",
     documentation:
       "If specified with no arguments, HELP shows a list of all commands\n"
       "with a one line description. Help can display command options by\n"
       "specifying the name of the command. Additionally, it can display\n"
       "group or property help by specifying the GROUP or PROPERTY option.\n")
  optional command :: <command-line> = "the command to describe";
  keyword group :: <command-group> = "the command group to describe";
  keyword property :: <command-property> = "the property to describe";
end command-line help;

define sealed method do-execute-command
    (context :: <server-context>, command :: <help-command>)
 => ()
  let command-line = command.%command;
  let property = command.%property;
  let group = command.%group | context.context-command-group;
  if (property & command-line)
    message(context,
	    "A property and a command cannot be specified together.")
  else
    let stream = context.context-server.server-output-stream;
    let object = command-line | property | group;
    display-help(stream, context, object, group: group, title: command.%title)
  end
end method do-execute-command;

define method display-help
    (stream :: <stream>, context :: <server-context>, group :: <command-group>,
     #key group: root-group :: <command-group> = group,
          title :: false-or(<string>) = #f)
 => ()
  let groups = collect-command-info(root-group, <command-group>);
  if (groups.size > 1)
    format(stream, "\nThe following groups of commands are provided:\n\n");
    print-command-info(stream, remove(groups, root-group));
    new-line(stream);
  else
    format(stream, "\n%s:\n\n%s\n\n", 
	   title | group.command-info-title,
	   group.command-info-documentation);
    let command-lines = collect-command-info(group, <basic-command-line>);
    unless (empty?(command-lines))
      format(stream, "Commands:\n");
      print-command-info(stream, command-lines, prefix: "  ");
      new-line(stream);
    end;
    let properties = collect-command-info(group, <command-property>);
    unless (empty?(properties))
      format(stream, "Properties:\n");
      print-command-info(stream, properties);
      new-line(stream)
    end;
  end;
  format(stream, 
	 "For documentation on a group, use:    HELP %sGROUP group.\n"
	 "For documentation on a command, use:  HELP command\n"
	 "For a complete list of commands, use: SHOW COMMANDS\n"
	 "\n"
	 "For full documentation, see %sdocumentation/\n"
	 "For a guide to the command line tools, see %sdocumentation/getting-started-cli/\n",
	 $option-prefix,
	 release-web-address(),
	 release-web-address())
end method display-help;

define method display-help
    (stream :: <stream>, context :: <server-context>,
     info :: <command-property>,
     #key group :: <command-group> = context.context-command-group,
          title :: false-or(<string>) = #f)
 => ()
  ignore(group);
  format(stream, "\nProperty: %s\n\n%s\n",
	 title | info.command-info-title,
	 info.command-info-documentation)
end method display-help;

define method display-help
    (stream :: <stream>, context :: <server-context>, 
     command-line :: <command-line>,
     #key group :: <command-group> = context.context-command-group,
          title :: false-or(<string>) = #f,
          alias :: false-or(<command-line-alias>) = #f)
 => ()
  let command-class = command-line.command-info-command-class;
  let (arguments, optionals, keywords) = command-parameters(command-class);
  let aliases = command-line-aliases(group, command-line);
  display-usage(stream, context, command-line, group: group, title: title);
  format(stream, "\n%s\n", command-line.command-info-documentation);
  unless (empty?(aliases))
    write(stream, "Aliases: ");
    for (command-alias :: <command-line-alias> in aliases,
	 separator = "" then ", ")
      if (alias & separator == "")
	write(stream, command-line.command-info-title);
        separator := ", ";
      end;
      when (command-alias ~= alias)
        write(stream, separator);
        write(stream, command-alias.command-info-title)
      end
    end;
    new-line(stream)
  end;
  unless (empty?(arguments) & empty?(optionals))
    format(stream, "\nArguments:\n\n");
    print-table(stream, concatenate(arguments, optionals),
		label-key: parameter-name,
		value-key: parameter-summary,
		separator: " - ")
  end;
  unless (empty?(keywords))
    format(stream, "\nOptions:\n\n");
    print-table(stream, keywords,
		label-key: parameter-name-and-type,
		value-key: parameter-summary,
		separator: " - ")
  end
end method display-help;

define method display-help
    (stream :: <stream>, context :: <server-context>, 
     alias :: <command-line-alias>,
     #key group :: <command-group> = context.context-command-group,
          title :: false-or(<string>) = #f)
 => ()
  let command-line = alias.command-info-alias;
  display-help(stream, context, command-line, 
	       group: group, 
	       title: alias.command-info-title, 
	       alias: alias)
end method display-help;

define method display-usage
    (stream :: <stream>, context :: <server-context>, 
     command-line :: <command-line>,
     #key group :: <command-group> = context.context-command-group,
          title :: false-or(<string>) = #f)
 => ()
  let command-class = command-line.command-info-command-class;
  let (arguments, optionals, keywords) = command-parameters(command-class);
  format(stream, "Usage: ");
  if (title)
    format(stream, "%s", title)
  else
    format(stream, "%s",
	   command-line.command-info-title)
  end;
  unless (empty?(keywords))
    format(stream, " [options*]")
  end;
  for (argument :: <required-parameter> in arguments)
    format(stream, " %s", 
	   as-lowercase(argument.parameter-name))
  end;
  unless (empty?(optionals))
    format(stream, " [");
    for (argument :: <optional-parameter> in optionals,
	 separator = "" then " ")
      format(stream, "%s%s", separator,
	     as-lowercase(argument.parameter-name))
    end;
    format(stream, "]")
  end;
  format(stream, "\n");
end method display-usage;

define function print-command-info
    (stream :: <stream>, info-group :: <sequence>,
     #key prefix :: <string> = "  ")
 => ()
  print-table(stream, info-group,
	      label-key: command-info-title,
	      value-key: command-info-summary,
	      prefix:    prefix,
	      sort?:     #t)
end function print-command-info;

define function print-table
    (stream :: <stream>, items :: <sequence>,
     #key label-key :: <function>,
          value-key :: <function>,
          prefix :: <string> = "  ",
          separator = "  ",
          sort? :: <boolean> = #f)
 => ()
  let tab-column :: <integer>
    = reduce(method (max-size :: <integer>, item)
	       max(max-size, size(item.label-key))
	     end,
	     0, items);
  local method item-label<
	    (item1, item2) => (true? :: <boolean>)
	  item1.label-key < item2.label-key
	end method item-label<;
  let spaces = make(<byte-string>, size: tab-column, fill: ' ');
  let items 
    = case
	sort      => sort(items, test: item-label<);
	otherwise => items;
      end;
  for (item in items)
    let label = item.label-key;
    let value = item.value-key;
    write(stream, prefix);
    write(stream, label);
    write(stream, spaces, end: tab-column - label.size);
    write(stream, separator);
    write(stream, value);
    new-line(stream)
  end
end function print-table;


/// State describing

define constant $command-states :: <stretchy-object-vector> = make(<stretchy-object-vector>);

define function register-state-type
    (state :: <symbol>) => ()
  add!($command-states, state)
end function register-state-type;

define open generic find-state-value
    (context :: <server-context>, type :: <symbol>, name :: <string>)
 => (value :: <object>);

define open generic describe-state
    (context :: <server-context>, state :: <object>, #key prefix :: <string>, full? :: <boolean>)
 => ();

define class <describe-state-command> (<basic-command>)
  constant slot %type :: <symbol>,
    required-init-keyword: type:;
  constant slot %name :: <string>,
    required-init-keyword: name:;
end class <describe-state-command>;

define command-line describe => <describe-state-command>
    (summary:       "describes the specified state",
     documentation: "Describes the specified state.")
  argument type :: <symbol> = "the type of state to show";
  argument name :: <string> = "the name of the state to show";
end command-line describe;

define sealed method do-execute-command
    (context :: <server-context>, command :: <describe-state-command>)
 => ()
  let type = command.%type;
  let name = command.%name;
  unless (member?(type, $command-states))
    command-error("No such type %s available for DESCRIBE", type)
  end;
  let value = find-state-value(context, type, name);
  describe-state(context, value)
end method do-execute-command;


/// Basic commands

define command-group basic 
    (summary: "basic commands",
     documentation: "The basic commands.")
  command help;
  command version;
  command describe;
  command exit;
  alias quit = exit;
end command-group basic;
