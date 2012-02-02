module:    dylan-user
copyright: Original Code is Copyright (c) 2008-2010 Dylan Hackers;
           All rights reversed.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define library lisp-reader
  use common-dylan;
  use io;

  export lisp-reader;
end library;

define module lisp-reader
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;
  use standard-io;

  export read-lisp, print-s-expression;
end module;
