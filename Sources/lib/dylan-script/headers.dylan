Module:    dylan-script-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Lifted from elsewhere

define method read-header-from-string (s :: <byte-string>) 
 => (keys :: <table>, lines :: <integer>, chars :: <integer>)
  read-header-from-stream(make(<sequence-stream>, contents: s));
end method;

define method read-header-from-stream (s :: <stream>)
 => (keys :: <table>, lines :: <integer>, chars :: <integer>)
  let keys = make(<table>);
  local method loop (s, nlines)
	  let (key, strings, lines, eoh?) = read-file-header-component(s);
	  if (key)
	    let old-strings = element(keys, key, default: #f);
	    keys[key] 
              := if (old-strings)
                   apply(concatenate!, old-strings, " ", strings);
                 else
                   apply(concatenate!, strings);
                 end;
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
  assert(colon, "malformed header");
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


// eof

