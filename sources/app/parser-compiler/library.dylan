module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library parser-compiler
  use common-dylan;
  use io;
  use system;
  use parser-generator;
end;

define module parser-compiler
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format-out;
  use format;
  use standard-io;
  use operating-system;
  use file-system;
  use parser-generator;
end;
