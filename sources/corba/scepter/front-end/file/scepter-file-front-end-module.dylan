Module:    dylan-user
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module scepter-file-front-end
  use date;
  use generic-arithmetic-functional-dylan;
  use file-system;
  use format;
  use streams;
  use c-lexer,
    rename: { constant-value => lexer-value, $eoi-token => $lexer-eoi-token };
  use c-lexer-utilities;
  use cpp;
  use locators;
  use parser-run-time;
  use scepter-front-end;
  use scepter-error;
  use scepter-ast;

  export
    <file-source>,
    file-source-file,
    file-source-line;

  export
    make-token-stream,
    \with-token-stream,
    idl-parser-tag,
    idl-lexer-string;
end module;
