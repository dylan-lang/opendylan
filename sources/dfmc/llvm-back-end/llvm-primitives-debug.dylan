Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2013 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// %running-under-dylan-debugger? is a variable which indicates
// whether there is an interest in communicating with the debugger. By
// default it is set to 0. The Dylan debugger will set it to something
// else if it is involved in communication with the app.
//
define runtime-variable %running-under-dylan-debugger? :: <raw-integer>
  = make-raw-literal(0);

define side-effecting stateless dynamic-extent &runtime-primitive-descriptor primitive-inside-debugger? () => (debugging? :: <boolean>);
  let m = be.llvm-builder-module;
  let global
    = llvm-runtime-variable(be, m, %running-under-dylan-debugger?-descriptor);
  let inside?-raw = ins--load(be, global, volatile?: #t);
  let inside? = ins--icmp-ne(be, inside?-raw, 0);
  op--boolean(be, inside?)
end;

/*
define side-effecting stateful dynamic-extent &runtime-primitive-descriptor primitive-break () => ()
  //---*** Fill this in...
end;
*/

define side-effecting stateful dynamic-extent &runtime-primitive-descriptor primitive-invoke-debugger
    (format-string :: <byte-string>, arguments :: <simple-object-vector>)
 => ();
  //---*** Fill this in to do something with the format-string and arguments.
  ins--call-intrinsic(be, "llvm.debugtrap", vector())
end;

define side-effecting stateless dynamic-extent &unimplemented-primitive-descriptor primitive-debug-message // runtime
    (format-string :: <byte-string>, arguments :: <simple-object-vector>)
 => ()
  //---*** Fill this in...
end;
