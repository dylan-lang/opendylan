module:    pentium-harp
Synopsis:  The Pentium backend class.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The Pentium backend class. Nothing very interesting here.

define class <pentium-back-end> (<harp-cisc-back-end>)
end;

define method initialize
   (obj :: <pentium-back-end>, #key) => (new :: <pentium-back-end>)
  next-method();
  obj.registers := pentium-registers;
  obj;
end;

define class <pentium-windows-back-end> (<pentium-back-end>, 
                                         <native-windows-back-end>)
end;

register-back-end(<pentium-windows-back-end>, #"harp", #"x86", #"win32");

define class <pentium-unix-back-end> (<pentium-back-end>)
end;

define class <pentium-linux-back-end>
 (<pentium-unix-back-end>, <native-linux-back-end>)
end;

register-back-end(<pentium-linux-back-end>, #"harp", #"x86", #"linux");

define class <pentium-freebsd-back-end>
 (<pentium-unix-back-end>, <native-freebsd-back-end>)
end;

register-back-end(<pentium-freebsd-back-end>, #"harp", #"x86", #"freebsd");
