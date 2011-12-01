Module: dylan-user
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library scepter-tests
  use coomon-dylan;
  use io;
  use system;
  use parser-run-time; 
  use c-lexer;
  use testworks;
  use scepter;
  export scepter-tests;
end library;

define module scepter-tests
  use dylan;
  use streams;
  use standard-io;
  use print;
  use format;
  use common-extensions, exclude: {format-to-string, <union>};
  use parser-run-time;
  use c-lexer,
    rename: { constant-value => lexer-value, $eoi-token => $lexer-eoi-token };
  use cpp; // from c-lexer
  use testworks;
  use scepter;
  use file-system;
  use locators;
  export
    all-idl,
    neo-idl,
    orbix-demo-idl,
    orbix-include-idl, 
    corba-core-idl,
    corba-services-idl,
    damaged-idl;
end module;
