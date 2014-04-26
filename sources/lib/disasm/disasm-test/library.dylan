module:    dylan-user
Synopsis:  Test harness for the disassembler for 386 code
Author:    Jon Thackray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library disasm-test
  use common-dylan;
  use io;
  use disasm;

  export disasm-test;
end library;

define module disasm-test
  use common-dylan;
  use byte-vector;
  use format;
  use format-out;
  use disasm;

end module;
