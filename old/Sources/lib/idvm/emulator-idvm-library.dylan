Module:    dylan-user
Language:  infix-dylan
Synopsis:  Define the In-Dylan Virtual Machine (IDVM) library and modules
Author:    Eliot Miranda, Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library IDVM
  use dylan;
  use timers;
  use streams;
  use format;
  use doss;
end library;

define module IDVM
  use internal, import: {function-debug-name,debug-name,find-translator-module,module-name};

  use dylan;

  use syntax-case;

  use streams;

  use format;

  use timers;

  export
        <vm-method-info>,
            vm-code-vec,
            stack-size,
            parameter-type,
            arg-count,
            uses-next-method,
            uses-rest,
            takes-all-keys,
            key-value-pairs,
            outer-info,
            closed-var-indices,
            the-method-name,
            status,

        build-vm,

        debug,
        undebug,
        code-for,
        *single-step*,
        *trace*,

        make-tests,
        run-tests,

        idvm-return,
        idvm-return-false,
        idvm-return-lit,
        idvm-return-loc,


        idvm-call,
        idvm-call-returning,

        idvm-call-res,
        idvm-call-res-returning,
        idvm-call-lit,
        idvm-call-lit-returning,
        idvm-call-loc,
        idvm-call-loc-returning,

        idvm-call-lit-lit,
        idvm-call-lit-lit-returning,
        idvm-call-lit-loc,
        idvm-call-lit-loc-returning,
        idvm-call-loc-lit,
        idvm-call-loc-lit-returning,
        idvm-call-loc-loc,
        idvm-call-loc-loc-returning,

        idvm-call-n,
        idvm-call-n-returning,

        idvm-rescall,
        idvm-rescall-returning,

        idvm-rescall-lit,
        idvm-rescall-lit-returning,
        idvm-rescall-loc,
        idvm-rescall-loc-returning,

        idvm-rescall-lit-lit,
        idvm-rescall-lit-lit-returning,
        idvm-rescall-lit-loc,
        idvm-rescall-lit-loc-returning,
        idvm-rescall-loc-lit,
        idvm-rescall-loc-lit-returning,
        idvm-rescall-loc-loc,
        idvm-rescall-loc-loc-returning,

        idvm-rescall-n,
        idvm-rescall-n-returning,


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

        idvm-mv-bind, idvm-mv-bind-rest,

        idvm-process-keys;

end module IDVM;

define module IDVM-loader
  use dylan;
  use format;
  use streams;
  use variable-search;
  use doss;
  use equal-table;
  use IDVM;

  export
      install-constant,
      install-variable,
      install-variable-reader,
      load-idvm-code;

end module IDVM-loader;
