Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library file-reader
  use dylan;
  use locators;
  use streams;
  use header-reader;
  use lexer;
  use parser;
  use parser-run-time;

  export 
    file-reader;
end library;

define module file-reader
  use dylan;
  use locators;
  use streams;
  use header-reader;
  use lexer,
    export: { register-infix-macro-word };
  use parser;
  use parser-run-time;

  export
    read-da-big-form, read-source-file, read-source-file-header,
    open-source-file, close-source-file, read-form;
end module;

// eof
