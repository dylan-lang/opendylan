Module: dylan-user
Author: Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library cpp-tester
  use functional-dylan;
  use system;
  use io;
  use c-lexer;
end library;

  
define module cpp-tester
  use functional-dylan;
  use standard-io;
  use c-lexer;
  use c-lexer-test-interface;
  use streams-internals /*, import: do-next-output-buffer*/ ;
  use format;
  use format-out;
  use cpp;
  use operating-system;
end module;
