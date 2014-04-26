module:    dylan-user
Synopsis:  mnemonic assembler output from harp for i486
Author:    Jon Thackray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library mnemonic-assembler
  use common-dylan;
  use collections;
  use io;
  use generic-arithmetic;
  use big-integers;
  use harp;
  use disasm;

  export mnemonic-assembler;
end library;

define module mnemonic-assembler
  use generic-arithmetic-dylan;
  use common-extensions;
  use byte-vector;
  use format;
  use format-out;
  use harp-for-extenders;
  use disasm;

end module;
