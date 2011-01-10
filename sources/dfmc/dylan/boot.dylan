Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This is a special hook recognised by the compiler that causes the 
// initial set of compile-time models of Dylan classes and objects
// to be booted and installed into the current compilation unit
// by the compiler.

boot-dylan-definitions();
boot-dylan-sources();

define open generic \+ (x, y) => (z);

define constant default-discriminator = method (#rest arguments) end;

define class <unsigned-machine-integer> (<integer>)
  slot data;
end class <unsigned-machine-integer>;

define constant <big-integer> = <unsigned-machine-integer>;
define constant <machine-integer> = <unsigned-machine-integer>;
