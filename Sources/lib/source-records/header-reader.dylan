module: file-source-records-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <badly-formed-file-header> (<source-record-error>) 
end;

define class <bad-header> (<simple-error>)
    constant slot file-header-error-message :: <string>,
    required-init-keyword: message:;
end;

define method read-file-header (file :: <locator>)
 => (keys :: <table>, lines :: <integer>, chars :: <integer>)
  block()
    with-open-file (stream = file)
      read-header-from-stream(stream)
    end;
  exception(bh :: <bad-header>)
    signal(make(<badly-formed-file-header>,
		format-string: "Badly formed file header in %s\n%s\n",
		format-arguments: vector(file, bh.file-header-error-message)))
  end;
end method;

define constant $unique-header-keywords =  #(#"module", #"language");

define method read-header-from-stream (s :: <stream>)
 => (keys :: <table>, lines :: <integer>, chars :: <integer>)
  let keys = make(<table>);
  local method loop (s, nlines)
	  let (key, strings, lines, eoh?) = read-file-header-component(s);
	  if (key)
	    let old-strings = element(keys, key, default: #());
	    if (~empty?(old-strings) & member?(key, $unique-header-keywords))
	      signal(make(<bad-header>, 
			  message: format-to-string("Duplicate keyword %s", key)))
	    end;
	    keys[key] := concatenate!(old-strings, strings);
	  end;
	  let nlines = nlines + lines;
	  if (eoh?)
	    values(keys, nlines, stream-position(s) + 1)
	  else
	    loop(s, nlines)
	  end;
	end;
  loop(s, 0);
end method;

define method read-file-header-component (s :: <stream>)
  let (key-line, nl?) = read-line(s, on-end-of-stream: "");
  let nlines = if (nl?) 1 else 0 end;
  if (header-end-marker-line?(key-line))
    values(#f, #f, nlines, #t)
  else
    local method loop (text-strings, nlines)
	    let char = read-element(s, on-end-of-stream: #f);
	    if (char == ' ' | char == '\t')
	      let (continuation-line, nl?) = read-line(s, on-end-of-stream: "");
	      let nlines = if (nl?) nlines + 1 else nlines end;
	      if (header-end-marker-line?(continuation-line))
		values(text-strings, nlines, #t);
	      else
		let text = parse-header-continuation-line(continuation-line);
		loop(pair(text, text-strings), nlines)
	      end;
	    else
	      if (char) unread-element(s, char) end;
	      values(text-strings, nlines, #f)
	    end;
	  end method;
    let (key, text) = parse-header-keyword-line(key-line);
    let (text-strings, nlines, past-eoh?) = loop(list(text), nlines);
    values(key, reverse!(text-strings), nlines, past-eoh?)
  end;
end method;

define method parse-header-keyword-line (line :: <string>)
  let colon = position(line, ':');
  if (~colon)
    signal(make(<bad-header>, 
		message: format-to-string("Syntax error on line: %=", line)))
  end;
  values(as(<symbol>, copy-sequence(line, end: colon)),
	 trim-whitespace(line, colon + 1))
end method;

define method parse-header-continuation-line (line :: <string>)
  trim-whitespace(line, 0)
end method;

define function header-end-marker-line? (line :: <string>)
  every?(method (c) c == ' ' | c == '\t' end, line)
end function;

define function trim-whitespace (line :: <string>, start)
  local method bwd (line, start, len)
	  let last = len - 1;
	  let c = line[last];
	  if (c == ' ' | c == '\t') bwd(line, start, last)
	  else copy-sequence(line, start: start, end: len) end;
	end method;
  local method fwd (line, start, len)
	  if (start == len) ""
	  else
	    let c = line[start];
	    if (c == ' ' | c == '\t') fwd(line, start + 1, len)
	    else bwd(line, start, len) end;
	  end;
	end method;
  fwd(line, start, size(line))
end function;










