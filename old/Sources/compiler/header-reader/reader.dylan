Module: header-reader
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// File header reader.

define generic read-file-header (stream :: <file-stream);

define method read-file-header (s :: <file-stream>)
  let entries = #();
  block (return)
    while (#t)
      let (key, strings) = s.read-file-header-component;
      if (~key)
        return(reverse!(entries))
      else
        entries := pair(pair(key, strings), entries);
      end;
    end;
  end;
end method read-file-header;

define method read-file-header-component (s :: <stream>)
  block (return)
    let key-line = read-to(s, '\n');
    if (key-line.header-end-marker-line?)
      return(#f, #f)
    end if;
    let text-strings = make(<deque>);
    let (key, text) = key-line.parse-header-keyword-line;
    push-last(text-strings, text);
    for (char = s.peek then s.peek,
         until: ~whitespace?(char) | char == '\n')
      let continuation-line = read-to(s, '\n');
      if (continuation-line.header-end-marker-line?)
        return(key, as(<list>, text-strings))
      else
        push-last
          (text-strings, continuation-line.parse-header-continuation-line)
      end if;
    finally
      // s.undo-input;
    end for;
    return(key, as(<list>, text-strings))
  end block
end method read-file-header-component;

define method parse-header-keyword-line (line :: <string>)
  let colon = find-key(line, curry(\==, ':'), failure: #f);
  unless (colon)
    error("Badly formed file header line %=", line)
  end unless;
  values
    (as(<symbol>, copy-sequence(line, end: colon)),
     trim(copy-sequence(line, start: colon + 1), whitespace?))
end method parse-header-keyword-line;

define method parse-header-continuation-line (line :: <string>)
  trim(line, whitespace?)
end method parse-header-continuation-line;

define method header-end-marker-line? (line :: <string>)
  every?(whitespace?, line)
end method header-end-marker-line?;

// Utilities.

define method whitespace? (c :: <character>)
  c == ' ' | c == '\t' | c == '\n'
end method;

define method trim (seq, lose?)
  let win? = lose?.complement;
  let start = find-key(seq, win?);
  let \end = seq.size - find-key(seq.reverse, win?);
  // find-key ... from:
  copy-sequence(seq, start: start, end: \end)
end method trim;

// eof
