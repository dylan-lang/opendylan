Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library lexer
  use dylan;
  use streams;
  export
    lexer
end library;

define module lexer
  use dylan;
  use streams;
  export
    lex, <lexer-state>, 
    register-infix-macro-word;
end module;

// eof
