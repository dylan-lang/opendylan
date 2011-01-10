Module:    console-scepter
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $keyword-prefixes = #['/', '-'];

define inline-only method keyword-argument? (argument :: <string>)
 => (keyword? :: <boolean>)
  member?(argument[0], $keyword-prefixes);
end method;

define constant $ORB-keyword-prefix = "-ORB";

define inline-only method ORB-keyword-argument? (argument :: <string>)
 => (ORB-keyword? :: <boolean>)
  prefix?(argument, $ORB-keyword-prefix);
end method;

define inline-only method prefix? (big :: <sequence>, prefix :: <sequence>)
 => (prefix? :: <boolean>)
  (size(prefix) <= size(big)) & every?(\=, big, prefix)
end method;

define method process-argument (argument :: <string>)
 => (text :: <string>, keyword? :: <boolean>)
  if (keyword-argument?(argument))
    values(copy-sequence(argument, start: 1), #t);
  else
    values(argument, #f);
  end if;
end method;

define method separate-combined-option-and-value (argument :: <string>)
 => (arguments :: <sequence>)
  debug-assert(keyword-argument?(argument));
  let separator-pos = subsequence-position(argument, ":");
  if (separator-pos)
    list(copy-sequence(argument, end: separator-pos),
	 copy-sequence(argument, start: separator-pos + 1));
  else
    list(argument);
  end if;
end method;

define method canonicalize-arguments (arguments :: <sequence>)
 => (canonicalized-arguments :: <deque>)
  let canonicalized-arguments = make(<deque>);
  let skip? = #f;
  for (argument in arguments)
    if (skip? & keyword-argument?(argument))
      skip? := #f;
    end if;
    unless (skip?)
      case
	ORB-keyword-argument?(argument)
	  => skip? := #t;
	keyword-argument?(argument)
	  => do(curry(push-last, canonicalized-arguments),
		separate-combined-option-and-value(argument));
	otherwise
	  => push-last(canonicalized-arguments, argument)
      end case;
    end unless;
  end for;
  canonicalized-arguments;
end method;

define method parse-command-line-arguments (scepter :: <console-scepter>,
					    arguments :: <deque>)
 => ()
  until (arguments.empty?)
    let (argument, keyword?) = process-argument(pop(arguments));
    if (keyword?)
      let (option, rest) = scepter-find-option(scepter, argument);
      if (option)
	if (rest)
	  push(arguments, rest);
	end if;
        process-option(scepter, option, argument, arguments);
      else
	console-scepter-error(scepter, make(<illegal-option>, source: #f,
					    option-name: argument));
      end if;
    else
      scepter.scepter-sources := add!(scepter.scepter-sources, argument);
    end if;
  end until;
end method;

define method process-option (scepter :: <console-scepter>,
  option :: <scepter-option>, arg :: <string>, arguments :: <deque>)
 => ()
  if (scepter-option-value?(option))
    if (empty?(arguments))
      console-scepter-error(scepter, make(<missing-argument>, source: #f,
					  option-name: arg));
    else
      scepter-option-add-value(pop(arguments), option);
    end if;
  else
    scepter-option-supplied(option);
  end if;
end method;
