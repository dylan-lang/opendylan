module:    x86-harp
Synopsis:  The Pentium backend class.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The Pentium backend class. Nothing very interesting here.

define class <x86-back-end> (<harp-native-back-end>)
end;

define method initialize
   (obj :: <x86-back-end>, #key) => (new :: <x86-back-end>)
  next-method();
  obj.registers := pentium-registers;
  obj;
end;

define sideways method big-endian?
    (back-end :: <x86-back-end>) => (big-endian? :: <boolean>)
  #f
end;

define class <x86-windows-back-end> (<x86-back-end>,
                                     <native-windows-back-end>)
end;

register-back-end(<x86-windows-back-end>, #"harp", #"x86", #"win32");

define class <x86-unix-back-end> (<x86-back-end>)
end;

define class <x86-linux-back-end>
 (<x86-unix-back-end>, <native-linux-back-end>)
end;

register-back-end(<x86-linux-back-end>, #"harp", #"x86", #"linux");

define class <x86-freebsd-back-end>
 (<x86-unix-back-end>, <native-freebsd-back-end>)
end;

register-back-end(<x86-freebsd-back-end>, #"harp", #"x86", #"freebsd");

define class <x86-darwin-back-end>
 (<x86-unix-back-end>, <native-darwin-back-end>)
end;

register-back-end(<x86-darwin-back-end>, #"harp", #"x86", #"darwin");
