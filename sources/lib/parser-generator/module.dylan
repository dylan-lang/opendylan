Module:    dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module parser-generator
  use dylan;
  use dylan-extensions;
  use common-extensions, exclude: { format-to-string };
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
