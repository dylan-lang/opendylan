module:    pentium-harp
Synopsis:  The Pentium backend class.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



// The Pentium backend class. Nothing very interesting here.

define class <pentium-back-end> (<harp-cisc-back-end>)
end;

define class <windows-pentium-back-end> (<pentium-back-end>)
end;

define class <linux-pentium-back-end> (<pentium-back-end>)
end;


define method initialize
   (obj :: <pentium-back-end>, #key) => (new :: <pentium-back-end>)
  next-method();
  obj.registers := pentium-registers;
  obj;
end;
