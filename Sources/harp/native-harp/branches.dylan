module:    native-main-harp
Synopsis:  Native branching code
Author:    Tony Mann, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



// Force LOAD-NLX-ADDRESS to be with-stack.
// This instruction is used by bind-exit and unwind-protect. 
// For these constructs, leaf-case is inappropriate as stack-
// relative addressing will fail after building a frame on the stack.
// LOAD-NLX-ADDRESS also destroys all registers, because the 
// block introduced by the tag may be called from an arbitrary
// register context.

define macro with-load-nlx-address-ops
  { with-load-nlx-address-ops ?back-end:name end }
    => { with-ops-in ?back-end ## "-instructions" (load-nlx-address)
	   c-preserved-destroys-fn := ?=all-c-preserved-fn;  // not worth being smarter
	   destroys-fn := constant-fn(?back-end ## "-allocatable-registers"); 
	   flag := #t;
	 end with-ops-in }
end macro;

define macro load-nlx-address-template-definer
  { define ?back-end:name load-nlx-address-template }
    =>
  { define ?back-end ## "-template" load-nlx-address
      pattern (?=be, tag, dest)
        harp-out (?=be) lea(?=be, tag, dest, 0) end;
    end ?back-end ## "-template" }
end macro;



/// Source Code Location
///
/// This is almost like LEA - except that it's not actually an SDI. The
/// code size is always zero - but we emit a <code-locator-constant>


define macro strong-scl-template-definer
  { define ?back-end:name strong-scl-template }
    =>
  { define ?back-end ## "-template" strong-scl
      pattern (?=be, locator, named-registers)
        emit-scl(?=be, locator, named-registers);
    end ?back-end ## "-template" }
end macro;


define macro scl-template-definer
  { define ?back-end:name scl-template }
    =>
  { define ?back-end ## "-template" scl
      pattern (?=be, locator, requested-registers, really-live-registers)
        emit-scl(?=be, locator, really-live-registers);
    end ?back-end ## "-template" }
end macro;



define method emit-scl 
    (be :: <harp-native-back-end>, locator, named-registers :: <simple-object-vector>)
     => ()
  let with-stack = be.variables.with-stack;
  let label = make(<code-locator-constant>, 
                   data: locator, stack: with-stack, 
                   variables: named-registers, size: 0);
  emit-labelled-constant(be, label, 0);
end method;
