Module:    parser-generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Rule accessors.  The grammar compiler requires that a rule be a sequence
// whose first element is the name and second element is the production.
// We extend it with an action in third element.
define function make-rule (#key name, production, body)
  vector(name, production, canonicalize-body(production.size, body))
end;

define constant rule-name = first;
define constant rule-production = second;
define constant rule-action = third;

// Read rules until "end" token encountered.
define variable $grammar-end = "end";

define function read-rules (inp :: <stream>)
 => (rules, error-rules)
  iterate loop (rules = #(), error-rules = #(), name = #f)
    let tkn = read-token(inp);
    if (~tkn)
      values(reverse!(rules), reverse!(error-rules))
    elseif (tkn.last == ':')
      assert(~name | rules.head.rule-name == name, "No rules for %s!", name);
      let name = as(<symbol>, copy-sequence(tkn, end: tkn.size - 1));
      loop(rules, error-rules, name);
    else
      assert(name, "Malformed parse clause at token %s", tkn);
      let prod = iterate loop (tkns :: <list> = #(), tkn = tkn)
                   let tkn = as(<symbol>, tkn);
                   if (tkn == #"=>")
                     reverse!(tkns)
                   else
                     loop(pair(tkn, tkns), read-token(inp))
                   end;
                 end iterate;
      let body = read-until(inp, ";;");
      let rule = make-rule(name: name, production: prod,  body: body);
      if (~empty?(prod) & prod.head == #"error")
        assert(prod.tail == #(), "Non empty ERROR rule for %s", name);
	loop(rules, pair(rule, error-rules), name)
      else
	loop(pair(rule, rules), error-rules, name)
      end;
    end;
  end iterate;
end;

define inline function whitespace-char? 
    (char :: <byte-character>) => (whitespace? :: <boolean>)
  member?(char, " \t\n\r\f");
end;

define thread variable *parse-buffer* :: <stretchy-vector>
  = make(<stretchy-vector>);

define function parse-buffer () => (buf :: <stretchy-vector>)
  *parse-buffer*.size := 0;
  *parse-buffer*
end;

define function read-token (inp :: <stream>)
 => (t :: false-or(<byte-string>))
  let c :: <character> = read-element(inp);
  if (c == '/')
    read-line(inp);
    read-token(inp);
  elseif (whitespace-char?(c))
    read-token(inp);
  else
    let chars = parse-buffer();
    add!(chars, c);
    iterate loop ()
      let c = read-element(inp, on-end-of-stream: #f);
      if (~c | whitespace-char?(c))
	let string = as(<byte-string>, chars);
	if (string = $grammar-end)
	  read-line(inp);
	  #f
	else
	  string
	end;
      else
	add!(chars, c);
	loop();
      end;
    end iterate;
  end;
end function read-token;

define function read-until (inp :: <stream>, delim :: <byte-string>)
 => (s :: <byte-string>)
  let ndelim = delim.size;
  let last-delim = delim[ndelim - 1];
  let chars = parse-buffer();
  local method delim? (chars)
	  let start = chars.size - ndelim;
	  start >= 0 & for (i from 0 below ndelim,
			    while: delim[i] == chars[start + i])
		       finally i == ndelim
		       end;
	end method;
  iterate loop ()
    let c = read-element(inp);
    if (c == as(<character>, 10) & chars.last == as(<character>, 13))
      chars.last := '\n'; // crlf => newline
    else
      add!(chars, c);
    end;
    (c == last-delim & delim?(chars)) | loop()
  end;
  as(<byte-string>, copy-sequence(chars, end: chars.size - ndelim))
end;
