module: idvm-harp-test
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Now define a backend itself;


define variable idvm-backend = make(<idvm-back-end>);

define variable ib = idvm-backend;

// second generation tests .....

define method set-function-name
    (be :: <idvm-back-end>, name :: <byte-string>) 
  be.variables.function-name  := name;
end method;


define method run-test 
    (test :: <function>, #key outputter = make-interactive-print-outputter())
  let be :: <harp-back-end> = ib;
  invoke-harp(ib, test, "dummy", outputter: outputter);
  values();
end method;

define method test0 (be :: <idvm-back-end>)
  let regs = be.registers;
  set-function-name(be, "_Tony0");
  let rr1 = make-register(be);
  let rr2 = make-register(be);
  let rr3 = make-register(be);
  let rr4 = make-register(be);
  let rr5 = make-register(be);
  let rr6 = make-register(be);
  ins--move(be, regs.reg-result, 99);
  ins--move(be, rr1, 1);
  ins--move(be, rr2, 2);
  ins--move(be, rr3, 3);
  ins--add(be, rr4, rr1, rr2);
  ins--add(be, rr5, rr3, rr4);
  ins--add(be, rr6, rr5, 99);
  ins--add(be, regs.reg-result, regs.reg-result, 99);
  ins--add(be, regs.reg-result, regs.reg-result, rr6);
  ins--return-value(be, 0);
end;
     

define method test1 (be :: <idvm-back-end>)
  set-function-name(be, "_Tony1");
  let rr1 = make-register(be);
  let rr2 = make-register(be);
  let rr3 = make-register(be);
  let rr4 = make-register(be);
  let rr5 = make-register(be);
  let rr6 = make-register(be);
  let rr6 = make-register(be);
  let rr7 = make-register(be);
  let rr8 = make-register(be);
  let rr9 = make-register(be);
  ins--load-stack-arg-n(be, rr1, 0);
  ins--load-stack-arg-n(be, rr2, 1);
  ins--move(be, rr3, 99);
  ins--move(be, rr4, 99);
  ins--move(be, rr5, 999);
  ins--move(be, rr7, 9999);
  // set up for a call
  ins--store-vm-arg-n(be, rr1, 0);
  ins--store-vm-arg-n(be, rr2, 1);
  ins--store-vm-arg-n(be, rr3, 2);
  ins--store-vm-arg-n(be, rr4, 3);
  ins--store-vm-arg-n(be, rr5, 4);
  ins--store-vm-arg-n(be, 888, 5);
  ins--vm-call-n(be, ins--constant-ref(be, "Funny"), 6);
  ins--move(be, rr6, res);
  ins--store-vm-arg-n(be, rr6, 0);
  ins--store-vm-arg-n(be, rr7, 1);
  ins--store-vm-arg-n(be, rr3, 2);
  ins--store-vm-arg-n(be, rr4, 3);
  ins--store-vm-arg-n(be, rr1, 4);
  ins--vm-call-n(be, ins--constant-ref(be, "Funny2"), 5);
  ins--add(be, rr4, rr4, 99);
  ins--return-value(be, rr4);
end;
     
/* test2 is roughly:

define method test (rr1, rr2);
  let rr3 = 99;
  let rr4 = 99;
  let rr5 = 999;
  let rr6 = 333;
  let rr7 = 9999;
  block ()
    funny(rr1, rr2, rr3, rr4, rr5, 888);
  cleanup
    let (rr8, rr9, rr10) = funny2(rr6, rr7, rr3, rr4, rr1);
    funny3(rr8, rr9, rr10);
  end block;
end method;

*/

define method test2 (be :: <idvm-back-end>)
  set-function-name(be, "_Tony2");
  let cleanup-tag = make-tag(be);
  let up-finish-tag = make-tag(be);
  let mv-bind-tag = make-tag(be);
  let rr1 = make-register(be);
  let rr2 = make-register(be);
  let rr3 = make-register(be);
  let rr4 = make-register(be);
  let rr5 = make-register(be);
  let rr6 = make-register(be);
  let rr6 = make-register(be);
  let rr7 = make-register(be);
  let rr8 = make-register(be);
  let rr9 = make-register(be);
  let rr10 = make-register(be);
  ins--load-stack-arg-n(be, rr1, 0);
  ins--load-stack-arg-n(be, rr2, 1);
  ins--move(be, rr3, 99);
  ins--move(be, rr4, 99);
  ins--move(be, rr5, 999);
  ins--move(be, rr6, 333);
  ins--move(be, rr7, 9999);
  ins--vm-unwind-protect-returning(be, cleanup-tag, up-finish-tag);
  // set up for a call
  ins--store-vm-arg-n(be, rr1, 0);
  ins--store-vm-arg-n(be, rr2, 1);
  ins--store-vm-arg-n(be, rr3, 2);
  ins--store-vm-arg-n(be, rr4, 3);
  ins--store-vm-arg-n(be, rr5, 4);
  ins--store-vm-arg-n(be, 888, 5);
  ins--vm-call-n-returning(be, ins--constant-ref(be, "Funny"), 6);
  ins--tag(be, cleanup-tag);
  ins--vm-mv-bind(be, mv-bind-tag, 3);
  ins--store-vm-arg-n(be, rr6, 0);
  ins--store-vm-arg-n(be, rr7, 1);
  ins--store-vm-arg-n(be, rr3, 2);
  ins--store-vm-arg-n(be, rr4, 3);
  ins--store-vm-arg-n(be, rr1, 4);
  ins--vm-call-n-returning(be, ins--constant-ref(be, "Funny2"), 5);
  ins--tag(be, mv-bind-tag);
  ins--vm-mv-bind-finished(be, 3);
  ins--move(be, rr8, res);
  ins--load-vm-arg-n(be, rr9, 0);
  ins--load-vm-arg-n(be, rr10, 1);
  ins--store-vm-arg-n(be, rr8, 0);
  ins--store-vm-arg-n(be, rr9, 1);
  ins--store-vm-arg-n(be, rr10, 2);
  ins--vm-call-n-returning(be, ins--constant-ref(be, "Funny3"), 3);
  ins--tag(be, up-finish-tag);
  ins--return-any-values(be);
end;
     

define method test3 (be :: <idvm-back-end>)
  set-function-name(be, "_sdi_test");
  let argc = make-register(be);
  let tag = make-tag(be);
  ins--bne(be, tag, argc, 0);
  for (i from 0 below 40)
    ins--add(be, argc, argc, 123456);
    ins--bne(be, tag, argc, 0);
  end for;
  ins--tag(be, tag);
  ins--return-value(be, 0);
end;
     

define method test4 (be :: <idvm-back-end>)
  set-function-name(be, "_Tony4");
  let rr1 = make-register(be);
  let rr2 = make-register(be);
  let rr3 = make-register(be);
  let rr4 = make-register(be);
  let rr5 = make-register(be);
  let rr6 = make-register(be);
  let closure = make-register(be);
  ins--load-stack-arg-n(be, rr1, 0);
  ins--load-stack-arg-n(be, rr2, 1);
  ins--move(be, rr3, 99);
  ins--move(be, rr4, 99);
  ins--move(be, rr5, 999);
  ins--move(be, rr6, 333);
  ins--make-closure(be, closure, 
                        ins--constant-ref(be, "closure"),
                        rr1, rr2, rr3, rr4, rr5, rr6);
  ins--return-value(be, closure);
end;
     

     
