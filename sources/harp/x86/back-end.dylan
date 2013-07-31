module:    harp-x86
Synopsis:  The Pentium backend class.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The Pentium backend class. Nothing very interesting here.

define class <harp-x86-back-end> (<harp-native-back-end>)
end;

define method initialize
   (obj :: <harp-x86-back-end>, #key) => (new :: <harp-x86-back-end>)
  next-method();
  obj.registers := pentium-registers;
  obj;
end;

define sideways method big-endian?
    (back-end :: <harp-x86-back-end>) => (big-endian? :: <boolean>)
  #f
end;

define class <harp-x86-windows-back-end> (<harp-x86-back-end>,
                                     <harp-native-windows-back-end>)
end;

register-back-end(<harp-x86-windows-back-end>, #"harp", #"x86-win32");

define class <harp-x86-unix-back-end> (<harp-x86-back-end>)
end;

define class <harp-x86-linux-back-end>
 (<harp-x86-unix-back-end>, <harp-native-linux-back-end>)
end;

register-back-end(<harp-x86-linux-back-end>, #"harp", #"x86-linux");

define class <harp-x86-freebsd-back-end>
 (<harp-x86-unix-back-end>, <harp-native-freebsd-back-end>)
end;

register-back-end(<harp-x86-freebsd-back-end>, #"harp", #"x86-freebsd");

define class <harp-x86-darwin-back-end>
 (<harp-x86-unix-back-end>, <harp-native-darwin-back-end>)
end;

register-back-end(<harp-x86-darwin-back-end>, #"harp", #"x86-darwin");
