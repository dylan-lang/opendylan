Module:       dylan-user
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library llvm-tablegen
  use common-dylan;
  use io;
  use system;
  use parser-run-time;
end library llvm-tablegen;

define module llvm-tablegen
  use common-dylan, exclude: { format-to-string };
  use streams;
  use print;
  use format;
  use format-out;
  use standard-io;
  use locators;
  use file-system;
  use operating-system;
  use parser-run-time;
end module llvm-tablegen;
