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

define function op--inside-debugger? (be :: <llvm-back-end>) => (debugging-cmp :: <llvm-value>)
  let m = be.llvm-builder-module;
  let global
    = llvm-runtime-variable(be, m, %running-under-dylan-debugger?-descriptor);
  let inside?-raw = ins--load(be, global, volatile?: #t);
  ins--icmp-ne(be, inside?-raw, 0)
end function;

define side-effecting stateless dynamic-extent &runtime-primitive-descriptor primitive-inside-debugger? () => (debugging? :: <boolean>);
  let inside-cmp = op--inside-debugger?(be);
  op--boolean(be, inside-cmp)
end;

/*
define side-effecting stateful dynamic-extent &runtime-primitive-descriptor primitive-break () => ()
  //---*** Fill this in...
end;
*/

define side-effecting stateful dynamic-extent &runtime-primitive-descriptor primitive-invoke-debugger
    (format-string :: <byte-string>, arguments :: <simple-object-vector>)
 => ();
  ins--call-intrinsic(be, "llvm.debugtrap", vector())
end;

define side-effecting stateless dynamic-extent &runtime-primitive-descriptor primitive-debug-message
    (format-string :: <byte-string>, arguments :: <simple-object-vector>)
 => ()
  let inside-cmp = op--inside-debugger?(be);
  ins--if (be, inside-cmp)
    ins--call-intrinsic(be, "llvm.debugtrap", #[]);
  end ins--if
end;

// Called by the GC
define c-callable auxiliary &runtime-primitive-descriptor class-allocation-break
    (string :: <raw-byte-string>, class :: <class>,
     count :: <raw-c-signed-int>, size :: <raw-c-signed-int>)
 => (result :: <object>);
  let module = be.llvm-builder-module;
  ins--call-intrinsic(be, "llvm.debugtrap", #[]);
  emit-reference(be, module, &false)
end;
