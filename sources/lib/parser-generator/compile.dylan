Module:    parser-generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define generic compile-grammar-file
    (input-name, output-name, report-grammar-conflict, #key);

define method compile-grammar-file
  (input-name, output-name, report-grammar-conflict :: <function>,  #rest keys, #key)
  with-open-file (inf = input-name)
    with-open-file (outf = output-name, direction: #"output")
      apply(compile-grammar-file, inf, outf, report-grammar-conflict, keys)
    end;
  end;
end;

define method compile-grammar-file
  (input-name, outf :: <stream>, report-grammar-conflict :: <function>, #rest keys, #key)
  with-open-file (inf = input-name)
    apply(compile-grammar-file, inf, outf, report-grammar-conflict, keys)
  end;
end;

define method compile-grammar-file
    (inf :: <stream>, outf :: <stream>, report-grammar-conflict :: <function>,
     #key terminal-string)
  // Writing directly to a file seems to output crlf's as newlines,
  // which I don't like.  Kludge around it.
  let out = make(<string-stream>, direction: #"output");
  let parser-name = copy-header(inf, out);
  let (rules, error-rules) = read-rules(inf);
  let grammar = begin
		  let handler <grammar-conflict>
		    = method (c, next-handler)
			ignore(next-handler);
			report-grammar-conflict(c)
		      end;
		  compile-grammar-rules(rules, error-rules: error-rules);
		end;
  output-dylan-parser(out, parser-name, grammar, terminal-string: terminal-string);
  copy-footer(inf, out);
  write(outf, stream-contents(out));
end;


define variable $grammar-begin = "define parser";

define function copy-header (inp :: <stream>, out :: <stream>)
 => (parser-name :: <string>)
  let n = $grammar-begin.size;
  iterate loop ()
    let line = read-line(inp);
    if (n <= line.size & every?(\==, $grammar-begin, line))
      read-token(make(<string-stream>, contents: line, start: n))
    else
      write-line(out, line);
      loop();
    end;
  end iterate;
end;

define function copy-footer (inp :: <stream>, out :: <stream>)
  let line = read-line(inp, on-end-of-stream: #f);
  if (line)
    write-line(out, line);
    copy-footer(inp, out);
  end;
end;

