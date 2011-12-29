Module:    command-lines
Synopsis:  The commands provided by the environment
Author:    Andy Armstrong
Copyright: Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
           All rights reserved.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

/// Useful constants

define constant $whitespace                 = #[' ', '\t', '\n'];
define constant $quote-characters           = #['\'', '"'];

define constant $option-prefix              = command-line-option-prefix();
define constant $standard-option-prefix     = '-';
define constant $option-separator           = ',';
define constant $command-prompt-string      = "> ";
define constant $option-argument-separators = #[':', '='];

define inline function is-whitespace?
    (character :: <character>) => (whitespace? :: <boolean>)
  member?(character, $whitespace)
end function is-whitespace?;


/// Command line server

define class <command-line-server> (<object>)
  constant slot server-context :: <server-context>,
    required-init-keyword: context:;
  constant slot server-input-stream :: <stream>,
    required-init-keyword: input-stream:;
  constant slot server-output-stream :: <stream>,
    required-init-keyword: output-stream:;
  slot server-incomplete-command-line :: false-or(<string>) = #f;
  slot server-last-command :: false-or(<command>) = #f;
  slot server-debugger? :: <boolean> = #f,
    init-keyword: debugger?:;
  slot server-echo-input? :: <boolean> = #f,
    init-keyword: echo-input?:;
  slot server-profile-commands? :: <boolean> = #f,
    init-keyword: profile-commands?:;
end class <command-line-server>;

define method initialize
    (server :: <command-line-server>, #key) => ()
  next-method();
  let context = server.server-context;
  context.context-server := server
end method initialize;

define open abstract class <server-context> (<object>)
  slot context-server :: <command-line-server>;
  constant slot context-banner :: false-or(<string>) = #f,
    init-keyword: banner:;
end class <server-context>;

define dynamic generic command-raw-parameters
    (class :: subclass(<command>))
 => (parameters :: <sequence>);

define open generic ensure-command-available
    (context :: <server-context>, command :: <command>)
 => ();

define method ensure-command-available
    (context :: <server-context>, command :: <command>)
 => ()
  #f
end method ensure-command-available;

define open generic command-complete?
    (context :: <server-context>, command :: <command>)
 => (complete? :: <boolean>);

define method command-complete?
    (context :: <server-context>, command :: <command>)
 => (complete? :: <boolean>)
  #t
end method command-complete?;


/// Command info

define abstract class <command-info> (<object>)
  constant slot command-info-name :: <symbol>,
    required-init-keyword: name:;
  constant slot command-info-title :: <string>,
    required-init-keyword: title:;
end class <command-info>;

define abstract class <basic-command-info> (<command-info>)
  constant slot command-info-summary :: <string>,
    required-init-keyword: summary:;
  constant slot command-info-documentation :: <string>,
    required-init-keyword: documentation:;
end class <basic-command-info>;

define function command-info-class-title
    (class :: subclass(<command-info>)) => (title :: <string>)
  select (class)
    <command-group>    => "command group";
    <command-property> => "property";
    <command-line>     => "command";
  end
end function command-info-class-title;


/// Command groups

define class <command-group> (<basic-command-info>)
  constant slot command-info-contents :: <stretchy-object-vector>,
    required-init-keyword: contents:;
end class <command-group>;

define method add-command-group
    (group :: <command-group>, new-group :: <command-group>) => ()
  add!(group.command-info-contents, new-group)
end method add-command-group;

define macro command-group-definer
  { define command-group ?name:name (?options:*)
      ?contents:*
    end }
    => { define constant "$" ## ?name ## "-command-group"
           = make(<command-group>,
                  ?options,
                  name:     ?#"name",
                  title:    as-uppercase(?"name"),
                  contents: as(<stretchy-object-vector>, vector(?contents))) }

  { define command-group ?name:name into ?group:name (?options:*)
      ?contents:*
    end }
    => { define constant "$" ## ?name ## "-command-group"
           = make(<command-group>,
                  ?options,
                  name:     ?#"name",
                  title:    as-uppercase(?"name"),
                  contents: as(<stretchy-object-vector>, vector(?contents)));
         add-command-group("$" ## ?group ## "-command-group",
                           "$" ## ?name ## "-command-group") }

 contents:
   { } => { }
   { ?content:*; ... } => { ?content, ... }

 content:
   { property ?property:name }
     => { "$" ## ?property ## "-command-property" }
   { command ?name:name }
     => { "$" ## ?name ## "-command-line" }
   { alias ?name:name = ?command-line:name }
     => { make(<command-line-alias>,
               name:  ?#"name",
               title: as-uppercase(?"name"),
               alias: "$" ## ?command-line ## "-command-line") }
   { group ?group:name }
     => { "$" ## ?group ## "-command-group" }
end macro command-group-definer;

define method do-command-group-objects
    (function :: <function>, group :: <command-group>,
     #key type :: <type> = <command-info>)
 => ()
  instance?(group, type) & function(group);
  let contents = group.command-info-contents;
  for (info :: <command-info> in contents)
    select (info by instance?)
      <command-group> =>
        do-command-group-objects(function, info, type: type);
      otherwise =>
        instance?(info, type) & function(info);
    end
  end
end method do-command-group-objects;

define function collect-command-info
    (group :: <command-group>, type :: <type>, #key sort? :: <boolean> = #f)
 => (info :: <stretchy-object-vector>)
  let objects :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  do-command-group-objects
    (method (info :: <command-info>)
       instance?(info, type) & add!(objects, info)
     end,
     group);
  if (sort?)
    sort!(objects,
          test: method (info1 :: <command-info>, info2 :: <command-info>)
                  info1.command-info-title < info2.command-info-title
                end)
  else
    objects
  end
end function collect-command-info;

define function find-command-info
    (group :: <command-group>, name :: <symbol>,
     #key type :: <type> = <command-info>)
 => (info :: false-or(<command-info>))
  block (return)
    do-command-group-objects
      (method (command-info :: <command-info>)
         if (name == command-info.command-info-name)
           return(command-info)
         end
       end,
       group, type: type);
    #f
  end
end function find-command-info;

define function command-line-aliases
    (group :: <command-group>, command-line :: <command-line>)
 => (aliases :: <sequence>)
  let command-class = command-line.command-info-command-class;
  let aliases :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  do-command-group-command-lines
    (method (line :: <command-line>)
       if (line.command-info-command-class == command-class & line ~== command-line)
         add!(aliases, line)
       end
     end,
     group);
  aliases
end function command-line-aliases;

define inline function do-command-group-command-lines
    (function :: <function>, group :: <command-group>) => ()
  do-command-group-objects(function, group, type: <command-line>)
end function do-command-group-command-lines;


/// Context protocols

define open generic context-command-group
    (context :: <server-context>) => (group :: <command-group>);

define open generic context-command-prefix
    (context :: <server-context>) => (prefix :: false-or(<character>));

define open generic class-for-command-line
    (context :: <server-context>, command-line :: <string>,
     #key start, end: stop)
 => (class :: subclass(<command>), next-index :: <integer>);

define open generic parse-next-argument
    (context :: <server-context>, type :: <type>, text :: <string>,
     #key start, end: stop)
 => (value :: <object>, next-index :: <integer>);


/// Utilities

define class <command-line-server-error> (<format-string-condition>, <error>)
end class <command-line-server-error>;

define class <command-error> (<command-line-server-error>)
end class <command-error>;

define class <parse-error> (<command-line-server-error>)
end class <parse-error>;

define method command-error
    (format-string :: <string>, #rest format-arguments)
  error(make(<command-error>,
             format-string: format-string,
             format-arguments: format-arguments))
end method command-error;

define method parse-error
    (format-string :: <string>, #rest format-arguments)
  error(make(<parse-error>,
             format-string: format-string,
             format-arguments: format-arguments))
end method parse-error;

define open generic display-command-prompt
    (stream :: <stream>, context :: <server-context>) => ();

define method display-command-prompt
    (stream :: <stream>, context :: <server-context>) => ()
  format(stream, $command-prompt-string)
end method display-command-prompt;

define method display-command-line-server-error
    (context :: <server-context>, error :: <command-line-server-error>) => ()
  message(context, "%s", error)
end method display-command-line-server-error;

define method display-command-line-server-error
    (context :: <server-context>, error :: <parse-error>) => ()
  message(context, "%s", error)
end method display-command-line-server-error;

define inline method message
    (context :: <server-context>, format-string :: <string>,
     #rest format-arguments)
 => ()
  let server = context.context-server;
  let stream = server.server-output-stream;
  apply(format, stream, format-string, format-arguments);
  new-line(stream);
  force-output(stream);
end method message;

define function display-condition
    (context :: <server-context>, condition :: <serious-condition>,
     #key prefix = "Internal error: ")
 => ()
  let error-message :: <string>
    = block ()
        condition-to-string(condition)
      exception (error :: <error>)
        block ()
          format-to-string("*** Crashed printing condition of class %=: %s",
                           condition.object-class, error)
        exception (<error>)
          "*** Crashed printing error, and then printing crash condition"
        end
      end;
  message(context, "");
  message(context, "%s%s", prefix, error-message)
end function display-condition;

define method tokenize-string
    (string :: <string>, separator :: <character>,
     #key start :: <integer> = 0,
          end: stop :: false-or(<integer>) = #f)
 => (tokens :: <sequence>)
  let tokens = make(<stretchy-vector>);
  let stop :: <integer> = stop | string.size;
  let old-position :: <integer> = start;
  while (old-position < stop & is-whitespace?(string[old-position]))
    old-position := old-position + 1
  end;
  let position :: <integer> = old-position;
  while (position < stop)
    while (position < stop & string[position] ~= separator)
      position := position + 1
    end;
    if (position <= stop)
      let end-position = position;
      while (end-position > old-position & is-whitespace?(string[end-position - 1]))
        end-position := end-position - 1
      end;
      add!(tokens, copy-sequence(string, start: old-position, end: end-position));
      old-position := position + 1;
      while (old-position < stop & is-whitespace?(string[old-position]))
        old-position := old-position + 1
      end;
      position := old-position
    end;
  end;
  if (old-position < stop - 1)
    add!(tokens, copy-sequence(string, start: old-position))
  end;
  tokens
end method tokenize-string;


/// Command lines

define abstract class <command-line> (<command-info>)
end class <command-line>;

define generic command-info-command-class
    (command-line :: <command-line>)
 => (class :: subclass(<command>));

define method command-line-for-command
    (context :: <server-context>, command :: <command>)
 => (command-line :: false-or(<basic-command-line>))
  command-line-for-command-class(context, object-class(command))
end method command-line-for-command;

define method command-line-for-command-class
    (context :: <server-context>, command-class :: subclass(<command>))
 => (command-line :: false-or(<basic-command-line>))
  block (return)
    do-command-group-command-lines
      (method (command-line :: <command-line>)
         if (~instance?(command-line, <command-line-alias>)
               & command-class == command-line.command-info-command-class)
           return(command-line)
         end
       end,
       context.context-command-group);
    #f
  end
end method command-line-for-command-class;

define method command-parameters
    (class :: subclass(<command>))
 => (arguments :: <sequence>, optionals :: <sequence>, keywords :: <sequence>)
  let parameters = command-raw-parameters(class);
  let arguments :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  let optionals :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  let keywords  :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  for (parameter in parameters)
    select (parameter by instance?)
      <required-parameter> => add!(arguments, parameter);
      <optional-parameter> => add!(optionals, parameter);
      <keyword-parameter>  => add!(keywords,  parameter);
    end
  end;
  values(arguments, optionals, keywords)
end method command-parameters;

define abstract class <command-line-parameter> (<object>)
  constant slot parameter-name :: <string>,
    required-init-keyword: name:;
  constant slot parameter-keyword :: <symbol>,
    required-init-keyword: keyword:;
  constant slot parameter-type :: <type>,
    required-init-keyword: type:;
  constant slot parameter-summary :: <string>,
    required-init-keyword: summary:;
end class <command-line-parameter>;

define open generic parameter-type-name
    (type :: <type>) => (title :: false-or(<string>));

define method parameter-name-and-type
    (parameter :: <command-line-parameter>) => (name-and-type :: <string>)
  let name = parameter.parameter-name;
  let type = parameter.parameter-type;
  let type-name
    = select (type)
        <boolean> => #f;
        otherwise => type.parameter-type-name | as-lowercase(name);
      end;
  if (type-name)
    format-to-string("%c%s %s", $option-prefix, name, type-name)
  else
    format-to-string("%c%s", $option-prefix, name)
  end
end method parameter-name-and-type;

define class <required-parameter> (<command-line-parameter>)
end class <required-parameter>;

define class <optional-parameter> (<command-line-parameter>)
end class <optional-parameter>;

define class <keyword-parameter> (<command-line-parameter>)
end class <keyword-parameter>;

define class <basic-command-line> (<basic-command-info>, <command-line>)
  constant slot command-info-command-class :: subclass(<command>),
    required-init-keyword: command-class:;
end class <basic-command-line>;

define sealed inline method make
    (command == <command-line>, #rest args, #key)
 => (command :: <basic-command-line>)
  apply(make, <basic-command-line>, args)
end method make;

define method execute-command-line
    (server :: <command-line-server>, string :: <string>)
 => (exit? :: <boolean>)
  block (return)
    let context = server.server-context;
    if (server.server-echo-input?)
      message(context, "%s", string)
    end;
    let (command, complete?, string)
      = block ()
          parse-command-line(server, string)
        exception (error :: <command-line-server-error>)
          display-command-line-server-error(context, error);
          return(#f);
        end;
    case
      ~complete? =>
        server.server-incomplete-command-line := string;
        #f;
      ~command =>
        error("No command returned for '%s'", string);
        #f;
      instance?(command, <exit-command>) =>
        #t;
      otherwise =>
        block ()
          execute-server-command(server, command)
        exception (error :: <command-line-server-error>)
          display-command-line-server-error(context, error)
        end;
        #f;
    end
  end
end method execute-command-line;

define method execute-server-command
    (server :: <command-line-server>, command :: <command>)
 => (#rest values)
  let context = server.server-context;
  ensure-command-available(context, command);
  server.server-last-command := command;
  if (server.server-profile-commands?)
    profiling (cpu-time-seconds, cpu-time-microseconds, allocation)
      execute-command(command)
    results
      message(context, "Command took %d.%s seconds, and allocated %d bytes",
              cpu-time-seconds,
              integer-to-string(floor/(cpu-time-microseconds, 1000), size: 3),
              allocation)
    end
  else
    execute-command(command)
  end
end method execute-server-command;

define method parse-command-line
    (server :: <command-line-server>, text :: <string>,
     #key class :: false-or(subclass(<command>)) = #f)
 => (command :: false-or(<command>), complete? :: <boolean>, text :: <string>)
  let last-command = server.server-last-command;
  let incomplete-command-line = server.server-incomplete-command-line;
  if (incomplete-command-line)
    text := format-to-string("%s\n%s", incomplete-command-line, text);
    server.server-incomplete-command-line := #f
  end;
  let stop :: <integer> = text.size;
  local method skip-whitespace
            (start :: <integer>) => (next :: <integer>)
          while (start < stop & is-whitespace?(text[start]))
            start := start + 1
          end;
          start
        end method skip-whitespace;
  let context = server.server-context;
  block (return)
    let (class, next-index)
      = if (class)
          values(class, 0)
        else
          let start = skip-whitespace(0);
          if (start < text.size)
            class-for-command-line(context, text, start: start)
          else
            unless (last-command)
              parse-error("No previous command to execute")
            end;
            //---*** andrewa: how can we get display the last command?
            // message(context, "Repeating: %s", last-command);
            return(last-command, #t, text)
          end
        end;
    let (arguments, complete?)
      = parse-command-line-arguments(server, class, text, start: next-index);
    let command
      = if (complete?)
          apply(make, class,
                server: context,
                arguments)
        end;
    let complete? = complete? & command-complete?(context, command);
    values(command, complete?, text)
  end
end method parse-command-line;

define method class-for-command-line
    (context :: <server-context>, command-line :: <string>,
     #key start :: <integer> = 0,
          end: stop :: false-or(<integer>) = #f)
 => (class :: subclass(<command>), next-index :: <integer>)
  let (command-line, next-index)
    = parse-next-argument
        (context, <command-line>, command-line,
         start: start, end: stop);
  values(command-line.command-info-command-class, next-index)
end method class-for-command-line;

define inline method parse-next-word
    (text :: <string>,
     #key start :: <integer> = 0,
          end: stop :: false-or(<integer>) = #f,
          separators :: <sequence> = $whitespace)
 => (word :: false-or(<string>), next-index :: <integer>)
  let stop :: <integer> = stop | text.size;
  if (start < stop & member?(text[start], $quote-characters))
    parse-next-string(text, start: start, end: stop)
  else
    let next-index :: <integer> = start;
    while (next-index < stop & ~member?(text[next-index], separators))
      next-index := next-index + 1
    end;
    values(if (start < next-index)
             copy-sequence(text, start: start, end: next-index)
           end,
           next-index)
  end
end method parse-next-word;

define inline method parse-next-string
    (text :: <string>,
     #key start :: <integer> = 0,
          end: stop :: <integer> = text.size,
          separators :: <sequence> = $whitespace)
 => (word :: <string>, next-index :: <integer>)
  let next-index :: <integer> = start;
  let quote-character :: <character> = text[next-index];
  next-index := next-index + 1;
  while (next-index < stop & text[next-index] ~== quote-character)
    next-index := next-index + 1
  end;
  unless (next-index < stop)
    parse-error("Missing closing quote in '%s'",
                copy-sequence(text, start: start, end: stop))
  end;
  values(copy-sequence(text, start: start + 1, end: next-index),
         next-index + 1)
end method parse-next-string;

define method parse-command-line-arguments
    (server :: <command-line-server>, class :: subclass(<command>),
     text :: <string>,
     #key start :: <integer> = 0,
          end: stop :: false-or(<integer>) = #f)
 => (results :: <stretchy-object-vector>, complete? :: <boolean>)
  let stop :: <integer> = stop | text.size;
  let context = server.server-context;
  let results :: <stretchy-object-vector> = make(<stretchy-object-vector>);

  local method skip-whitespace
            (start :: <integer>) => (next :: <integer>)
          while (start < stop & is-whitespace?(text[start]))
            start := start + 1
          end;
          start
        end method skip-whitespace;

  let (arguments, optionals, keywords) = command-parameters(class);
  let arguments = as(<deque>, arguments);
  let optionals = as(<deque>, optionals);
  start := skip-whitespace(start);
  while (start < stop)
    let char :: <character> = text[start];
    let parameter :: false-or(<command-line-parameter>)
      = case
          ~empty?(arguments) => first(arguments);
          ~empty?(optionals) => first(optionals);
          otherwise          => #f;
        end;
    let (keyword :: <symbol>, value :: <object>, next-index :: <integer>)
      = case
          instance?(parameter, <optional-parameter>)
            & subtype?(parameter.parameter-type, <string>) =>
            let keyword = parameter.parameter-keyword;
            let type = parameter.parameter-type;
            let value
              = as(type, copy-sequence(text, start: start, end: stop));
            values(keyword, value, stop);
          char == $option-prefix | char == $standard-option-prefix =>
            parse-next-option
              (context, text, keywords, start: start + 1, end: stop);
          parameter =>
            case
              ~empty?(arguments) => pop(arguments);
              ~empty?(optionals) => pop(optionals);
            end;
            let keyword = parameter.parameter-keyword;
            let type = parameter.parameter-type;
            let (value, next-index)
              = parse-next-argument
                  (context, type, text, start: start, end: stop);
            values(keyword, value, next-index);
          otherwise =>
            parse-error("Unexpected command argument: %s",
                        copy-sequence(text, start: start));
        end;
    add!(results, keyword);
    add!(results, value);
    start := skip-whitespace(next-index)
  end;
  values(results, empty?(arguments))
end method parse-command-line-arguments;

define method parse-next-option
    (context :: <server-context>, text :: <string>, keywords :: <sequence>,
     #key start :: <integer> = 0, end: stop = #f)
 => (keyword :: <symbol>, value :: <object>, next-index :: <integer>)
  local method skip-whitespace
            (start :: <integer>) => (next :: <integer>)
          while (start < stop & is-whitespace?(text[start]))
            start := start + 1
          end;
          start
        end method skip-whitespace,

        method find-parameter-info
            (option-word :: <string>)
         => (keyword :: false-or(<symbol>), type :: false-or(<type>))
          block (return)
            for (parameter :: <keyword-parameter> in keywords)
              if (parameter.parameter-name = option-word)
                let keyword = parameter.parameter-keyword;
                let type = parameter.parameter-type;
                return(keyword, type)
              end
            end;
            values(#f, #f)
          end
        end method find-parameter-info;

  let (next-word, next-index) = parse-next-word(text, start: start, end: stop);
  let (option-word, option-next-index)
    = parse-next-word
        (text, start: start, end: next-index,
         separators: $option-argument-separators);
  let option-word = as-uppercase(option-word);
  let (keyword, type) = find-parameter-info(option-word);
  let option-separator? = option-next-index ~== next-index;
  case
    keyword =>
      case
        type == <boolean> & ~option-separator? =>
          values(keyword, #t, next-index);
        option-separator? =>
          let start = option-next-index + 1;
          if (start == stop)
            parse-error("Missing argument to option '%s'",
                        option-word)
          end;
          let (value, option-next-index)
            = parse-next-argument
                (context, type, text, start: start, end: next-index);
          assert(next-index == option-next-index,
                 "Unexpectedly didn't read all of option!");
          values(keyword, value, next-index);
        otherwise =>
          let start = skip-whitespace(next-index);
          if (start == stop)
            parse-error("Missing argument to option '%s'",
                        option-word)
          end;
          let (value, next-index)
            = parse-next-argument
                (context, type, text, start: start, end: stop);
          values(keyword, value, next-index);
      end;
    option-word.size > 2 & option-word[0] = 'N' & option-word[1] = 'O' =>
      let (keyword, type)
        = find-parameter-info(copy-sequence(option-word, start: 2));
      select (type)
        <boolean> => values(keyword, #f, next-index);
        otherwise => parse-error("Unrecognized option '%s'", option-word);
      end;
    otherwise =>
      parse-error("Unrecognized option '%s'", option-word);
  end
end method parse-next-option;

define method command-line-loop
    (server :: <command-line-server>,
     #key debugger? :: <boolean> = #f,
          echo-input? :: <boolean> = server.server-echo-input?,
          profile-commands? :: <boolean> = server.server-profile-commands?)
 => ()
  server.server-debugger? := debugger?;
  server.server-echo-input? := echo-input?;
  server.server-profile-commands? := profile-commands?;
  let context = server.server-context;
  let input-stream  = server.server-input-stream;
  let output-stream = server.server-output-stream;
  let banner = context.context-banner;
  if (banner)
    write(output-stream, banner);
    force-output(output-stream);
  end if;
  let exit? = #f;
  while (~ exit?)
    block ()
      unless (server.server-incomplete-command-line)
        new-line(output-stream);
        display-command-prompt(output-stream, context);
        force-output(output-stream);
      end;
      let command-line = read-line(input-stream, on-end-of-stream: #f);
      exit? := case
                 ~command-line =>
                   #t;
                 otherwise =>
                   execute-command-line(server, command-line);
               end
    exception (condition :: <serious-condition>)
      case
        server.server-debugger? =>
          signal(condition);
        instance?(condition, <keyboard-interrupt>) =>
          message(context, "Operation aborted");
        otherwise =>
          display-condition(context, condition);
      end
    end
  end
end method command-line-loop;

define macro command-line-definer
  { define command-line ?name:name (?options:*)
      ?parameters:*
    end }
    => { define command-line-class ?name (?options) ?parameters end;
         define command-line-constant ?name => "<" ## ?name ## "-command>" (?options)
           ?parameters
         end }

  { define command-line ?name:name => ?command:name (?options:*)
      ?parameters:*
    end }
    => { define command-line-constant ?name => ?command (?options)
           ?parameters
         end }
end macro command-line-definer;

define macro command-line-constant-definer
  { define command-line-constant ?name:name => ?command:name (?options:*)
      ?parameters:*
    end }
    => { define constant "$" ## ?name ## "-command-line"
           = make(<basic-command-line>,
                  ?options,
                  command-class: ?command,
                  name:          ?#"name",
                  title:         as-uppercase(?"name"));
         define sideways method command-raw-parameters
             (class == ?command)
          => (parameters :: <sequence>)
           vector(?parameters)
         end }

 parameters:
   { } => { }
   { ?parameter:*; ... } => { ?parameter, ... }

 parameter:
   { argument ?name:name :: ?type:expression = ?summary:expression }
     => { make(<required-parameter>,
               name: as-uppercase(?"name"),
               keyword: ?#"name",
               type: ?type,
               summary: ?summary) }
   { required ?name:name :: ?type:expression = ?summary:expression }
     => { make(<required-parameter>,
               name: as-uppercase(?"name"),
               keyword: ?#"name",
               type: ?type,
               summary: ?summary) }
   { optional ?name:name :: ?type:expression = ?summary:expression }
     => { make(<optional-parameter>,
               name: as-uppercase(?"name"),
               keyword: ?#"name",
               type: ?type,
               summary: ?summary) }
   { keyword ?name:name :: <boolean> = ?summary:expression }
     => { make(<keyword-parameter>,
               name: as-uppercase(?"name"),
               keyword: ?#"name" ## "?",
               type: <boolean>,
               summary: ?summary) }
   { keyword ?name:name :: ?type:expression = ?summary:expression }
     => { make(<keyword-parameter>,
               name: as-uppercase(?"name"),
               keyword: ?#"name",
               type: ?type,
               summary: ?summary) }
   { flag ?name:name = ?summary:expression }
     => { make(<keyword-parameter>,
               name: as-uppercase(?"name"),
               keyword: ?#"name" ## "?",
               type: <boolean>,
               summary: ?summary) }
end macro command-line-constant-definer;


/// Command line user interface

define method command-line-question
    (server :: <command-line-server>, prompt :: <string>)
 => (ok? :: <boolean>)
  let input-stream  = server.server-input-stream;
  let output-stream = server.server-output-stream;

  iterate loop ()
    new-line(output-stream);
    format(output-stream, "%s ", prompt);
    force-output(output-stream);

    let answer = read-line(input-stream, on-end-of-stream: #f);

    if (~answer)
      #f
    elseif (empty?(answer))
      loop()
    else
      select (as-lowercase(answer) by \=)
        "yes", "y" => #t;
        "no", "n" => #f;
        otherwise => loop();
      end
    end
  end
end method command-line-question;

define method command-line-choose-file
    (server :: <command-line-server>,
     #key prompt :: false-or(<string>),
          directory :: false-or(<directory-locator>) = #f,
          default :: false-or(<file-system-locator>) = directory,
          direction :: one-of(#"input", #"output") = #"input",
          filters :: false-or(<sequence>) = #f,
          filter :: false-or(<symbol>))
 => (filename :: false-or(<file-locator>),
     filter :: false-or(<symbol>))
  let input-stream  = server.server-input-stream;
  let output-stream = server.server-output-stream;

  iterate loop ()
    new-line(output-stream);
    format(output-stream, "%s ", prompt);
    force-output(output-stream);

    let filename = read-line(input-stream, on-end-of-stream: #f);

    case
      ~filename => values(#f, #f);

      empty?(filename) => values(#f, #f);

      file-exists?(filename) =>
        let locator = as(<file-locator>, filename);
        values(if (locator-relative?(locator))
                 merge-locators(locator, working-directory())
               else
                 locator
               end,
               #f);

      otherwise =>
        message(server.server-context, "File %s does not exist", filename);
        loop();
    end
  end
end method command-line-choose-file;


/// Aliases

define class <command-line-alias> (<command-line>)
  constant slot command-info-alias :: <basic-command-line>,
    required-init-keyword: alias:;
end class <command-line-alias>;

define method command-info-command-class
    (alias :: <command-line-alias>) => (command-class :: subclass(<command>))
  alias.command-info-alias.command-info-command-class
end method command-info-command-class;


/// Boolean argument parsing

define method parameter-type-name
    (type == <boolean>) => (name == #f)
  #f
end method parameter-type-name;

define method parse-next-argument
    (context :: <server-context>, type :: subclass(<boolean>),
     text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <boolean>, next-index :: <integer>)
  let (boolean, next-index)
    = parse-next-word(text, start: start, end: stop);
  if (boolean)
    let true?
      = select (as-lowercase(boolean) by \=)
          "yes", "on" => #t;
          "no", "off" => #f;
          otherwise =>
            parse-error("Unrecognized option '%s' for boolean argument",
                        boolean);
        end;
    values(true?, next-index)
  else
    parse-error("Missing boolean argument")
  end
end method parse-next-argument;


/// String argument parsing

define method parameter-type-name
    (type :: subclass(<string>)) => (name == #f)
  #f
end method parameter-type-name;

define method parse-next-argument
    (context :: <server-context>, type :: subclass(<string>), text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <string>, next-index :: <integer>)
  let stop :: <integer> = stop | text.size;
  if (stop > start)
    values(as(type, copy-sequence(text, start: start, end: stop)),
           stop)
  else
    parse-error("Missing argument")
  end
end method parse-next-argument;


/// Symbol argument parsing

define method parameter-type-name
    (type == <symbol>) => (name == #f)
  #f
end method parameter-type-name;

define method parse-next-argument
    (context :: <server-context>, type == <symbol>, text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <symbol>, next-index :: <integer>)
  let (name, next-index)
    = parse-next-word(text, start: start, end: stop);
  if (name)
    values(as(<symbol>, name), next-index)
  else
    parse-error("Missing keyword argument")
  end
end method parse-next-argument;


/// Number argument parsing

define method parameter-type-name
    (type == <integer>) => (name :: <string>)
  "number"
end method parameter-type-name;

define method parse-next-argument
    (context :: <server-context>, type == <integer>, text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <integer>, next-index :: <integer>)
  let (integer, next-index) = string-to-integer(text, start: start, end: stop);
  if (integer)
    values(integer, next-index)
  else
    parse-error("Missing number argument")
  end
end method parse-next-argument;


/// File and directory argument parsing

define method parameter-type-name
    (type :: subclass(<file-locator>)) => (name :: <string>)
  "file"
end method parameter-type-name;

define method parse-next-argument
    (context :: <server-context>, type :: subclass(<locator>),
     text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <locator>, next-index :: <integer>)
  let (filename, next-index)
    = parse-next-word(text, start: start, end: stop);
  if (filename)
    let locator = merge-locators(as(type, filename), working-directory());
    values(locator, next-index)
  else
    parse-error("Missing filename argument")
  end
end method parse-next-argument;

define method parameter-type-name
    (type :: subclass(<directory-locator>)) => (name :: <string>)
  "directory"
end method parameter-type-name;


/// Keyword list argument parsing

define constant $keyword-list-type = singleton(#"keyword-list");

define method parameter-type-name
    (type == $keyword-list-type) => (name == #f)
  #f
end method parameter-type-name;

define method parse-next-argument
    (context :: <server-context>, type == $keyword-list-type,
     text :: <string>,
     #key start :: <integer> = 0, end: stop = #f)
 => (value :: <sequence>, next-index :: <integer>)
  let (keyword, next-index)
    = parse-next-word(text, start: start, end: stop);
  if (keyword)
    let options
      = tokenize-string(text, $option-separator, start: start, end: stop);
    values(map(curry(as, <symbol>), options), next-index)
  else
    parse-error("Missing keyword argument")
  end
end method parse-next-argument;


/// Command info argument parsing

define method parameter-type-name
    (type :: subclass(<command-line>)) => (name :: <string>)
  "command"
end method parameter-type-name;

define method parameter-type-name
    (type == <command-group>) => (name :: <string>)
  "group"
end method parameter-type-name;

define method parse-next-argument
    (context :: <server-context>, type :: subclass(<command-info>),
     text :: <string>,
     #key start :: <integer> = 0,
          end: stop :: false-or(<integer>) = #f)
 => (command :: <command-info>, next-index :: <integer>)
  let (name-string, next-index)
    = parse-next-word(text, start: start, end: stop);
  if (name-string)
    let group = context.context-command-group;
    let name = as(<symbol>, name-string);
    let info = find-command-info(group, name, type: type);
    if (info)
      values(info, next-index)
    else
      parse-error("No %s named '%s'",
                  command-info-class-title(type),
                  name-string)
    end
  else
    parse-error("Missing %s argument", command-info-class-title(type))
  end
end method parse-next-argument;
