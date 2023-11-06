module: file-source-records-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <badly-formed-file-header> (<source-record-error>)
end;

define class <bad-header> (<simple-error>)
    constant slot file-header-error-message :: <string>,
    required-init-keyword: message:;
end;

define method read-file-header (file :: <locator>)
 => (keys :: <table>, nlines :: <integer>, nchars :: <integer>)
  block ()
    with-open-file (stream = file)
      read-header-from-stream(stream)
    end;
  exception (bh :: <bad-header>)
    signal(make(<badly-formed-file-header>,
                format-string: "Badly formed file header in %s\n%s\n",
                format-arguments: vector(file, bh.file-header-error-message)))
  end;
end method;

define constant $unique-header-keywords =  #(#"module", #"language");

// Read 'key: val' pairs up through the empty line that separates the header
// from the main source code. Returns
//   * a table mapping key (interned as a symbol) to values which are either a
//     single string or, if there were continuation lines, a sequence of strings.
//   * the number of lines read
//   * the number of characters read
define method read-header-from-stream (stream :: <stream>)
 => (keys :: <table>, lines :: <integer>, chars :: <integer>)
  let keys = make(<table>);
  iterate loop (nlines = 0)
    let (key, strings, lines, eoh?) = read-file-header-component(stream);
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
      values(keys, nlines, stream-position(stream))
    else
      loop(nlines)
    end
  end
end method;

// Read one 'key: val' pair, with val possibly having continuation lines.
define method read-file-header-component (stream :: <stream>)
 => (key, strings, nlines, end-of-header?)
  let (key-line, nl?) = read-line(stream, on-end-of-stream: "");
  let nlines = if (nl?) 1 else 0 end;
  if (header-end-marker-line?(key-line))
    values(#f, #f, nlines, #t)
  else
    let (key, text) = parse-header-keyword-line(key-line);
    iterate loop (text-strings = list(text), nlines = nlines)
      let char = read-element(stream, on-end-of-stream: #f);
      if (char & header-whitespace?(char))
        let (continuation-line, nl?) = read-line(stream, on-end-of-stream: "");
        let nlines = if (nl?) nlines + 1 else nlines end;
        if (header-end-marker-line?(continuation-line))
          values(key, reverse!(text-strings), nlines, #t)
        else
          loop(pair(strip(continuation-line, test: header-whitespace?),
                    text-strings),
               nlines)
        end
      else
        char & unread-element(stream, char);
        values(key, reverse!(text-strings), nlines, #f)
      end
    end iterate
  end
end method;

define method parse-header-keyword-line (line :: <string>)
  let colon = position(line, ':')
    | signal(make(<bad-header>,
                  message: format-to-string("Syntax error on line: %=", line)));
  values(as(<symbol>, copy-sequence(line, end: colon)),
         strip(line, start: colon + 1, test: header-whitespace?))
end method;

// Dylan Interchange Format explicitly defines whitespace as space and tab.
define inline function header-whitespace? (c :: <character>) => (white? :: <boolean>)
  c == ' ' | c == '\t'
end;

define function header-end-marker-line? (line :: <string>)
  every?(header-whitespace?, line)
end function;
