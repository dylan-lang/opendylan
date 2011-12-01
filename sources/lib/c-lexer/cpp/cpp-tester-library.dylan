Module: dylan-user
Author: Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library cpp-tester
  use common-dylan;
  use system;
  use io;
  use c-lexer;
end library;

  
define module cpp-tester
  use common-dylan, exclude: { format-to-string };
  use standard-io;
  use c-lexer;
  use c-lexer-test-interface;
  use streams-internals /*, import: do-next-output-buffer*/ ;
  use format;
  use format-out;
  use cpp;
  use operating-system;
end module;
