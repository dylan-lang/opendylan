Module:    IDVM
Language:  infix-dylan
Synopsis:  DOSS interface for IDVM (to provide compact encoding of IDVM code under DOSS)
Author:    Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant bytecodes-to-opcodes = vector(
        idvm-return,
        idvm-return-false,
        idvm-return-lit,
        idvm-return-loc,


        idvm-call-with-res,
        idvm-call-with-res-returning,
        idvm-call-0,
        idvm-call-0-returning,
        idvm-call-1,
        idvm-call-1-returning,
        idvm-call-2,
        idvm-call-2-returning,
        idvm-call-n,
        idvm-call-n-returning,
        idvm-call-res-0,
        idvm-call-res-0-returning,
        idvm-call-res-1,
        idvm-call-res-1-returning,
        idvm-call-res-2,
        idvm-call-res-2-returning,
        idvm-call-res-n,
        idvm-call-res-n-returning,


        idvm-jump-true,
        idvm-jump-false,
        idvm-jump,

        idvm-loc-br-lt,
        idvm-loc-br-gt,
        idvm-loc-br-le,
        idvm-loc-br-ge,
        idvm-loc-br-eq,
        idvm-loc-br-ne,
        idvm-loc-br-ideq,
        idvm-loc-br-idne,

        idvm-lit-br-lt,
        idvm-lit-br-gt,
        idvm-lit-br-le,
        idvm-lit-br-ge,
        idvm-lit-br-eq,
        idvm-lit-br-ne,
        idvm-lit-br-ideq,
        idvm-lit-br-idne,

        idvm-res-loc-br-lt,
        idvm-res-loc-br-gt,
        idvm-res-loc-br-le,
        idvm-res-loc-br-ge,
        idvm-res-loc-br-eq,
        idvm-res-loc-br-ne,
        idvm-res-loc-br-ideq,
        idvm-res-loc-br-idne,

        idvm-res-lit-br-lt,
        idvm-res-lit-br-gt,
        idvm-res-lit-br-le,
        idvm-res-lit-br-ge,
        idvm-res-lit-br-eq,
        idvm-res-lit-br-ne,
        idvm-res-lit-br-ideq,
        idvm-res-lit-br-idne,


        idvm-loc-lt,
        idvm-loc-gt,
        idvm-loc-le,
        idvm-loc-ge,
        idvm-loc-eq,
        idvm-loc-ne,
        idvm-loc-ideq,
        idvm-loc-idne,

        idvm-lit-lt,
        idvm-lit-gt,
        idvm-lit-le,
        idvm-lit-ge,
        idvm-lit-eq,
        idvm-lit-ne,
        idvm-lit-ideq,
        idvm-lit-idne,

        idvm-res-loc-lt,
        idvm-res-loc-gt,
        idvm-res-loc-le,
        idvm-res-loc-ge,
        idvm-res-loc-eq,
        idvm-res-loc-ne,
        idvm-res-loc-ideq,
        idvm-res-loc-idne,

        idvm-res-lit-lt,
        idvm-res-lit-gt,
        idvm-res-lit-le,
        idvm-res-lit-ge,
        idvm-res-lit-eq,
        idvm-res-lit-ne,
        idvm-res-lit-ideq,
        idvm-res-lit-idne,


        idvm-loc-add,
        idvm-loc-sub,

        idvm-lit-add,
        idvm-lit-sub,

        idvm-res-loc-add,
        idvm-res-loc-sub,

        idvm-res-lit-add,
        idvm-res-lit-sub,


        idvm-res-gets-lit,
        idvm-loc-gets-lit,
        idvm-res-gets-loc,
        idvm-loc-gets-res,
        idvm-loc-gets-loc,

        idvm-make-value-cell,
        idvm-new-value-cell-res,
        idvm-new-value-cell-lit,
        idvm-new-value-cell-loc,
        idvm-res-gets-ev,
        idvm-res-gets-evc,
        idvm-res-gets-vc,
        idvm-loc-gets-ev,
        idvm-loc-gets-evc,
        idvm-loc-gets-vc,
        idvm-evc-gets-res,
        idvm-vc-gets-res,
        idvm-evc-gets-loc,
        idvm-vc-gets-loc,
        idvm-evc-gets-lit,
        idvm-vc-gets-lit,

        idvm-make-closure,
        idvm-make-closure-copying,

        idvm-unwind-protect,
        idvm-unwind-protect-returning,

        idvm-handler-bind-lit,
        idvm-handler-bind-loc,
        idvm-handler-bind-lit-returning,
        idvm-handler-bind-loc-returning,

        idvm-bind-exit,
        idvm-bind-exit-returning,

        idvm-mv-bind,

        idvm-process-keys
);

define method encode-module-variable (object :: <function>, the-idvm-module == find-translator-module(#"idvm"), library, hint)
    let opcode = find-key(bytecodes-to-opcodes,curry(id?,object));

    if (opcode)
	values(opcode, the-idvm-module.module-name, library)
    else
	next-method()
    end
end method;

define method variable-value (opcode :: <integer>, the-idvm-module == find-translator-module(#"idvm"), library)
  let function = element(bytecodes-to-opcodes,opcode,default: #f);

  if (function)
    function
  else
    next-method()
  end
end method;
