Module:    shell
Synopsis:  General purpose interactive shell functionality
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <command-loop> (<object>)
  constant slot command-table :: <table>, required-init-keyword: command-table:;
  constant slot level :: <integer>, required-init-keyword: level:;
  constant slot %prompt :: <string>, required-init-keyword: prompt:;
  slot %source, required-init-keyword: source:;
  slot %sink, required-init-keyword: sink:;
  constant slot %banner-function :: <function>, required-init-keyword: banner-function:;
  constant slot command-alias-table :: <table> = make(<table>);
end;

define constant *prompt-prefix* = "dylan";
define constant *top-prompt* = format-to-string("%s => ", *prompt-prefix*);

define function dylan-banner
    () => (banner :: <string>)
  format-to-string("\n%s\n%s\n%s\n",
		   release-name(),
		   release-version(),
		   release-copyright())
end function dylan-banner;

define variable *top-level-loop* = make(<command-loop>,
					command-table: make(<table>),
					prompt: *top-prompt*,
					source: *standard-input*,
					sink: *standard-output*,
					banner-function: dylan-banner,
					level: 0);

define method command-list
    (cl :: <command-loop>, #key test = identity, type = <command>)
 => (commands :: <sequence>)
  let commands = make(<stretchy-vector>);
  for (command in cl.command-table)
    when (instance?(command, type) & test(command))
      add!(commands, command)
    end
  end;
  commands
end;

define method user-message(cl :: <command-loop>, s :: <string>,
			   #rest args) => ();
  apply(format, cl.%sink, s, args);
end;

define class <end-of-loop> (<condition>) end;

define function register-command(cl :: <command-loop>,
				 c :: <command>) => ();
  cl.command-table[c.command-id] := c;
  for (alias in command-aliases(c.command-id))
    cl.command-table[alias] := make(<command-alias>, id: alias, alias: c);
  end for;
end;

define function find-command
    (cl :: <command-loop>, name :: <symbol>)
 => (command :: false-or(<command>))
  element(cl.command-table, name, default: #f);
end function find-command;

define function command-aliases(id :: <symbol>) => (aliases :: <sequence>)
  element(command-alias-table(*top-level-loop*), id, default: #());
end function;

define function command-aliases-setter(aliases :: <sequence>, id :: <symbol>) => (alias :: <sequence>)
  element(command-alias-table(*top-level-loop*), id) := aliases
end function;

define function register-command-alias
    (cl :: <command-loop>, alias :: <symbol>, command :: <symbol>) => ()
  let aliases = command-aliases(command);

  command-aliases(command) := add(aliases, alias);
end function register-command-alias;

define macro dylan-shell-command-definer
  { define dylan-shell-command ?:name (?args:*)
      description ?description:expression
      documentation ?documentation:expression
      hidden? ?hide:expression
      ?:body
    end }
    => { 
         define constant "$" ## ?name ## "-command" :: <command> =
           make-shell-command(?name, ?description, ?documentation, ?hide, #f);

         define shell-command-aux ?name 
           (?args) (?args)
           command "$" ## ?name ## "-command"
           ?body 
         end

       }
end macro;

define macro imported-shell-command-definer
  { define imported-shell-command ?:name
  }
    =>
  {
   register-command(*top-level-loop*, "$" ## ?name ## "-command") 
  }
end macro;

define macro shell-command-definer
  { define shell-command ?:name (?args:*)
      description ?description:expression
      documentation ?documentation:expression
      hidden? ?hide:expression
      ?:body
    end }
    => { define shell-command-aux ?name 
           (?args) (?args)
           command make-shell-command(?name, ?description, ?documentation, ?hide, #t)
           ?body 
         end
       }
end macro;

define macro shell-command-alias-definer
  { define shell-command-alias
      ?alias:name => ?command:name
    end }
    => { register-command-alias(*top-level-loop*, ?#"alias", ?#"command") }
end macro;

define macro shell-command-aux-definer
  { define shell-command-aux ?:name 
        (?args-as-names) (?args-as-symbols)
      command ?command:expression
      ?:body
    end }
    =>
    {
     begin

     let command = ?command;

     command.command-function :=
         method (?=context :: <command-loop>, #key ?args-as-names)
           ?body
         end;
     let arg-count = 0;
     map(method(a) 
	     let arg = find-argument(a);
	     if(arg.command-argument-flags = #f)
	       //format-out("bare arg for command %s\n", ?#"name");
	       command.command-arguments[arg-count] := arg;
	       arg-count := arg-count + 1;
	     end;
	     command.command-arguments[a] := arg
	 end,
	 #(?args-as-symbols))

     end
    }

    args-as-names:
    { } => { }
    { ?arg:name, ... } => { ?arg, ... }
    args-as-symbols:
    { } => { }
    { ?arg:name, ... } => { ?#"arg", ... } // coerce to a symbol
end macro;

define macro make-shell-command
  { make-shell-command
     (?:name,
      ?description:expression,
      ?documentation:expression,
      ?hide:expression,
      ?activate-command:expression)
  }
    =>
    {
      begin
        let c :: <command> =
          make(<command>,
	       id: ?#"name",
	       description: ?description,
	       documentation: ?documentation,
	       hidden?: ?hide);
	if (?activate-command) register-command(*top-level-loop*, c) end;
	c
      end
    }
end macro;

define generic run-command-loop(cl :: <command-loop>) => ();

define function display-banner (cl :: <command-loop>) => ()
  user-message(cl, cl.%banner-function())
end function display-banner;

define method run-command-loop(cl :: <command-loop>) => ();
  display-banner(cl);
  run-command-loop-internal(cl)
end;

define class <command-loop-continuation>(<condition>)
end class <command-loop-continuation>;


define open generic ask-user
    (context :: <command-loop>,
     #key prompt: prompt-string :: false-or(<string>),
          test :: false-or(<function>))
 => (answer :: false-or(<string>));

define function empty-line? (line :: <string>) => (empty? :: <boolean>)
  block(return)
    for (c in line)
      unless (c = ' ' | c = '\t')
	return(#f)
      end unless;
    end for;
    #t
  end block;
end function empty-line?;

define method ask-user
    (cl :: <command-loop>,
     #key prompt: prompt-string :: false-or(<string>),
          test :: false-or(<function>))
 => (answer :: false-or(<string>))
  block (return)
    let reply = #f;
    let prompt-string = prompt-string | cl.%prompt;
    let input = cl.%source;
    while (#t)
      user-message(cl, "\n%s", prompt-string);
      reply := as(<byte-string>, read-line(input));
      case
	reply.empty-line?   => return(#f);
	~test | test(reply) => return(reply);
	otherwise           => #f;
      end
    end;
  exception (error :: <error>)
    user-message(cl,
		 "\nA serious error was encountered reading from console... "
		 "Aborting\n");
    exit-command-loop(cl)
  end
end method ask-user;

define function read-command-line(cl :: <command-loop>) => (command-line :: <string>)
  let input = cl.%source;
  let output = cl.%sink;
  let command-line = "";
  block()
    while (command-line.empty-line?)
      format(output, "\n%s", cl.%prompt);
      command-line := as(<byte-string>, read-line(input));
    end;
  exception (error :: <error>)
    user-message(cl,
		 "\nA serious error was encountered reading from console... "
		 "Aborting\n");
    exit-command-loop(cl)
  end;
  command-line
end;

define function run-command-loop-internal(cl :: <command-loop>) => ();
  let input = cl.%source;
  let output = cl.%sink;
//  block(exit)
    let handler <warning> 
      = method (c, next-handler)
          user-message(cl, "%=\n", c);
        end;
      while(#t)
	let command-string = read-command-line(cl);
	let (command-name, #rest args) = os/tokenize-command-string(command-string);
	block()
	  apply(execute-command, cl, command-name, args);
	exception(c :: <command-loop-continuation>)
	exception(e :: <illegal-option>)
	    format(output, "\n%s\n", e)
	exception(e :: <missing-value>)
	  format(output, "\n%s\n", e)
	exception(e :: <illegal-syntax>)
	  format(output, "\n%s\n", e)
	end block;
      end while;
//  exception(e :: <error>)
//    format(output, "%s\n", e);
//    exit(#t);
//  exception(se :: <simple-error>)
//    apply(format, output, 
//	  condition-format-string(se), condition-format-arguments(se));
//    exit(#t);
//  end block;
end;

define function execute-command(cl :: <command-loop>, 
				command-name :: <string>,
				#rest args) => ();
  let command = find-command(cl, as(<symbol>, command-name));
  if(command)
    //user-message(cl, "raw args: %s\n", args);
    let the-args = apply(build-command-call, command, args);
    //user-message(cl, "command: %s, the args: %s\n", command.command-id, the-args);
    apply(command.command-function, cl, the-args);
  else
    user-message(cl, "Command %s not found", command-name);
  end;
end;

define function exit-command-loop (cl :: <command-loop>) => ()
  signal(make(<end-of-loop>))
end function exit-command-loop;

define function display-help
    (cl :: <command-loop>, #key test = complement(command-hidden?)) => ()
  let commands
    = command-list(cl, type: <basic-command>, test: test);
  let tab-column
    = 2 + reduce(method (max-size :: <integer>, command :: <command>)
		   max(max-size, size(command.command-name))
		 end,
		 0, commands);
  local method command-name<
	    (command1 :: <command>, command2 :: <command>)
	 => (true? :: <boolean>)
	  command1.command-name < command2.command-name
	end;
  user-message(cl, "\nAvailable commands:\n\n");
  for (command in sort(commands, test: command-name<))
    let name = command-name(command);
    user-message(cl, "  %s", name);
    for (i from 0 below (tab-column - size(name)))
      user-message(cl, " ")
    end;
    user-message(cl, "%s\n", command.command-description)
  end;
  user-message(cl, 
	       "\nFor full documentation on a command, use: HELP <command>.\n")
end function display-help;

define method display-command-help
    (cl :: <command-loop>, command-name :: <symbol>) => ()
  let command = find-command(cl, command-name);
  if (command)
    display-command-help(cl, command)
  else
    user-message(cl, "\nNo such command '%s'\n", 
		 as-uppercase(as(<string>, command-name)))
  end
end method display-command-help;

define method display-command-help
    (cl :: <command-loop>, alias :: <command-alias>) => ()
  display-command-help(cl, command-alias(alias))
end method display-command-help;

define method display-command-help
    (cl :: <command-loop>, command :: <command>) => ()
  user-message(cl, "\n  %s - %s\n\n",
	       command-name(command),
	       command.command-description);
  let aliases = command-aliases(command.command-id);
  unless (empty?(aliases))
    user-message(cl, "Aliases: ");
    let separator = #f;
    for (alias in aliases)
      user-message(cl, "%s%s", as-uppercase(as(<string>, alias)), separator | "");
      unless (separator) separator := ", " end
    end;
    user-message(cl, "\n\n");
  end;
  user-message(cl, "%s\n", command.command-documentation)
end method display-command-help;
