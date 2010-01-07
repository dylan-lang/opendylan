Module:    file-source-records-implementation
Synopsis:  The stream opening abstraction
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Kludge to support the fact that right now, the compiler wants a
// lisp stream for source records!

define macro with-open-source-file
  { with-open-source-file (?:variable = ?:expression) ?:body end }
    => { do-with-open-source-file(method (?variable) ?body end, ?expression) }
end macro;

define function do-with-open-source-file (f :: <function>, path)
  with-open-file (stream = path, element-type: <byte>)
    f(stream);
  end;
end function;

define constant $newline-code = as(<integer>, '\n');

define function stream-skip-lines (stream, nlines)
  for (i from 0 below nlines) skip-through(stream, $newline-code) end;
end function;
