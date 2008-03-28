module: dylan-user

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
