Module:    dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module parser-generator
  use functional-dylan;
  use streams;
  use format;
  use standard-io;
  use operating-system;
  use file-system;
  use grammar-compiler, export: all;

  export
    compile-grammar-file,
    rule-name,
    rule-production;
end;
