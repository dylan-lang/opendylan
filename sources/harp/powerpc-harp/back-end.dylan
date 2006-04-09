module:    powerpc-harp
Synopsis:  The PowerPC backend class.
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



// The PowerPC backend class.

define class <powerpc-back-end> (<harp-risc-back-end>)
end;

define class <powerpc-macos-back-end> (<powerpc-back-end>)
end;

register-back-end(<powerpc-macos-back-end>, #"harp", #"ppc", #"macos");

define class <powerpc-linux-back-end> (<powerpc-back-end>, <native-linux-back-end>)
end;

register-back-end(<powerpc-linux-back-end>, #"harp", #"ppc", #"linux");


define method initialize
   (obj :: <powerpc-back-end>, #key) => (new :: <powerpc-back-end>)
  next-method();
  obj.registers := powerpc-registers;
  obj;
end;
