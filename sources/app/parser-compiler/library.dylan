module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library parser-compiler
  use functional-dylan;
  use io;
  use system;
  use parser-generator;
end;

define module parser-compiler
  use functional-dylan;
  use streams;
  use format;
  use standard-io;
  use operating-system;
  use file-system;
  use parser-generator;
end;
