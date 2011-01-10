Module:    command-args
Synopsis:  Generic command processing utility
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $option-prefixes = select ($os-name)
				     #"win32"  => #['/', '-'];
				     otherwise => #['-'];
				   end;

define constant $option-parameter-separator = select ($os-name)
						#"win32"  => ":";
						otherwise => "=";
					      end;

define abstract class <command> (<object>)
  constant slot command-id :: <symbol>, required-init-keyword: id:;
end class <command>;

define method print-object
    (o :: <command>, stream :: <stream>) => ()
  format(stream, "{command: %s args: %s}", o.command-id, 
	 o.command-arguments.key-sequence)
end;

define class <basic-command> (<command>)
  constant slot command-arguments :: <table> = make(<table>);
  slot command-function :: <function>, init-keyword: function:;
  constant slot command-hidden? :: <boolean>, required-init-keyword: hidden?:;
  constant slot command-description :: <string>, required-init-keyword: description:;
  constant slot command-documentation :: <string>, required-init-keyword: documentation:;
end;

define method make
    (class == <command>, 
     #rest all-keys, #key, #all-keys)
 => (command :: <basic-command>)
  apply(make, <basic-command>, all-keys)
end method make;

define method command-name
    (command :: <command>) => (name :: <string>)
  as-uppercase(as(<string>, command-id(command)))
end method command-name;

define class <command-alias> (<command>)
  constant slot command-alias :: <command>, required-init-keyword: alias:;
end class <command-alias>;

define method command-function
    (alias :: <command-alias>) => (function :: <function>)
  let command = command-alias(alias);
  command-function(command)
end method command-function;

define method command-arguments
    (alias :: <command-alias>) => (arguments :: <table>)
  let command = command-alias(alias);
  command-arguments(command)
end method command-arguments;

define method command-hidden?
    (alias :: <command-alias>) => (hidden? :: <boolean>)
  let command = command-alias(alias);
  command-hidden?(command)
end method command-hidden?;

define constant <command-argument-flags> = <list>;
  // = limited(<list>, of: <string>);

define class <command-argument> (<object>)
  constant slot command-argument-flags :: false-or(<command-argument-flags>),
    required-init-keyword: flags:;
  constant slot command-argument-keyword :: <symbol>, required-init-keyword: keyword:;
  constant slot command-argument-usage :: false-or(<string>) = #f, init-keyword: usage:;
  constant slot command-argument-value? :: <boolean> = #f, init-keyword: value?:;
end class;

define method print-object
    (o :: <command-argument>, stream :: <stream>) => ()
  format(stream, "command-arg flags: %s key: %s", o.command-argument-flags,
	 o.command-argument-keyword)
end;

define variable *all-arguments* = make(<table>);

define class <illegal-option> (<error>)
  constant slot illegal-option-command :: <command>, required-init-keyword: command:;
  constant slot illegal-option :: <string>, required-init-keyword: option:;
end;

define class <illegal-argument> (<error>)
  constant slot illegal-argument :: <string>, required-init-keyword: argument:;
end;

define method print-object
    (o :: <illegal-option>, stream :: <stream>) => ()
  format(stream, "Illegal option %s to command: %s", o.illegal-option, 
	 o.illegal-option-command)
end;

define method print-object 
    (o :: <illegal-argument>, stream :: <stream>) => ()
  format(stream, "Illegal argument: %s", o.illegal-argument)
end;

define class <missing-value> (<error>)
  constant slot missing-value-argument :: <command-argument>, required-init-keyword: argument:;
end;

define method print-object
    (o :: <missing-value>, stream :: <stream>) => ()
  format(stream, "Missing value for argument: %s", 
	 o.missing-value-argument.command-argument-flags)
end;

define class <illegal-syntax> (<error>)
  constant slot illegal-syntax-command :: <command>, required-init-keyword: command:;
end;

define method print-object(o :: <illegal-syntax>, stream :: <stream>) => ()
  format(stream, "Illegal syntax for command: %s", 
	 o.illegal-syntax-command)
end;

// arguments come to being independently of the commands
define method register-argument(argument :: <command-argument>)
 => (arg :: <command-argument>);
  if(argument.command-argument-flags)
    map(method(f) *all-arguments*[as(<symbol>, f)] := argument end,
	argument.command-argument-flags)
  end;
  *all-arguments*[argument.command-argument-keyword] := argument 
end;

define function find-argument(arg :: <symbol>) => (a :: <command-argument>);
  block()
    *all-arguments*[arg];
  exception(<error>)
    signal(make(<illegal-argument>, option: as(<string>, arg)));
  end;
end;


define macro command-argument-definer
  { define command-argument ?:name }
  =>
  { register-argument(make(<command-argument>, flags: #f, keyword: ?#"name")) }
end macro;

define macro command-flag-definer
  { define command-flag ?:name }
  =>
  { register-argument(make(<command-argument>, flags: #(?"name"), keyword: ?#"name")) }
end macro;


define macro command-argument-spec-definer
  { define command-argument-spec ?args:* end }
  =>
  { register-argument(make(<command-argument>, ?args)) }
end macro;

define function is-option?(flag :: <string>) => (yes :: <boolean>);
  member?(flag.first, $option-prefixes)
end;

define function parse-option(option :: <string>) => (option :: <string>, param :: <string>)
  let parameterized? = subsequence-position(option, $option-parameter-separator);
  if (parameterized?)
    values(copy-sequence(option, end: parameterized?),
	   copy-sequence(option, start: parameterized? + 1));
  else
    values(option, "");
  end if;
end function;

define function build-command-call(command :: <command>, #rest arguments)
 => (parameter-list :: <list>);
  let len = arguments.size;
  let parameter-list = #();
  // go through the flags first
  // bare-argument has to be last
  for(i from 0 below len,
      while: is-option?(arguments[i]))
    let arg-string = copy-sequence(arguments[i], start: 1);
    let (arg-string :: <string>, param :: <string>) = parse-option(arg-string);
    let arg-symbol = as(<symbol>, arg-string);
    let arg = element(*all-arguments*, arg-symbol, default: #f);
    let command-arg = arg & element(command.command-arguments, 
				    arg.command-argument-keyword, default: #f);
    unless(command-arg)
      //format-out("arg: %s all-args: %s\n", arg, key-sequence(*all-arguments*));
      error(make(<illegal-option>, command: command, option: arguments[i]))
    end;
    parameter-list := pair(as(<symbol>, arg.command-argument-keyword), 
			   parameter-list);
    if(arg.command-argument-value?)
      i := i + 1;
      if(i = len | is-option?(arguments[i]))
	error(make(<missing-value>, argument: arg));
      else
	parameter-list := pair(arguments[i], parameter-list);
      end;
    else
      parameter-list := pair(if (param.empty?) #t else param end,
			     parameter-list);
    end;
  finally
    for (j from i below len)
      let command-arg = arguments[j];
      let arg =
	element(command.command-arguments, j - i, default: #f);
      unless(arg)
	//format-out("finally command: %s arg: %s\n", command, command-arg);
	error(make(<illegal-option>, command: command, option: command-arg))
      end;
      parameter-list := pair(as(<symbol>, arg.command-argument-keyword), 
			     parameter-list);
      parameter-list := pair(command-arg, parameter-list);
    end for;
  end;
  reverse!(parameter-list)
end;

define method print-usage (c :: <command>)
  format(*standard-error*, "\nusage: %s [flag]* value\nLegal flags:",
         c.command-id);
  for (arg in c.command-arguments)
    format(*standard-error*, arg.command-argument-usage);
  end for;
end method;

