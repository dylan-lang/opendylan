module: idvm-names
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


library-namer("idvm");
module-namer("idvm");

constant-namer("lookup-constant-definer", lookup-constant-definer);
constant-namer("lookup-variable-reader", lookup-variable-reader);
constant-namer("lookup-variable-writer", lookup-variable-writer);
constant-namer("lookup-variable-definer", lookup-variable-definer);
constant-namer("lookup-mangled", lookup-mangled);
constant-namer("build-vm-method", build-vm-method);
constant-namer("build-vm-closure", build-vm-closure);
constant-namer("build-vm-generic-function", build-vm-generic-function);
constant-namer("build-vm-class", build-vm-class);
constant-namer("build-vm-singleton", build-vm-singleton);
constant-namer("<simple-object-vector>", <simple-object-vector>);
constant-namer("<extended-float>", <single-float>);


constant-namer("idvm-return", idvm-return);

constant-namer("idvm-return-false", idvm-return-false);

constant-namer("idvm-return-lit", idvm-return-lit);

constant-namer("idvm-return-loc", idvm-return-loc);

constant-namer("idvm-call", idvm-call);

constant-namer("idvm-call-returning", idvm-call-returning);

constant-namer("idvm-call-res", idvm-call-res);

constant-namer("idvm-call-res-returning", idvm-call-res-returning);

constant-namer("idvm-call-lit", idvm-call-lit);

constant-namer("idvm-call-lit-returning", idvm-call-lit-returning);

constant-namer("idvm-call-loc", idvm-call-loc);

constant-namer("idvm-call-loc-returning", idvm-call-loc-returning);

constant-namer("idvm-call-lit-lit", idvm-call-lit-lit);

constant-namer("idvm-call-lit-lit-returning", idvm-call-lit-lit-returning);

constant-namer("idvm-call-lit-loc", idvm-call-lit-loc);

constant-namer("idvm-call-lit-loc-returning", idvm-call-lit-loc-returning);

constant-namer("idvm-call-loc-lit", idvm-call-loc-lit);

constant-namer("idvm-call-loc-lit-returning", idvm-call-loc-lit-returning);

constant-namer("idvm-call-loc-loc", idvm-call-loc-loc);

constant-namer("idvm-call-loc-loc-returning", idvm-call-loc-loc-returning);

constant-namer("idvm-call-n", idvm-call-n);

constant-namer("idvm-call-n-returning", idvm-call-n-returning);

constant-namer("idvm-rescall", idvm-rescall);

constant-namer("idvm-rescall-returning", idvm-rescall-returning);

constant-namer("idvm-rescall-lit", idvm-rescall-lit);

constant-namer("idvm-rescall-lit-returning", idvm-rescall-lit-returning);

constant-namer("idvm-rescall-loc", idvm-rescall-loc);

constant-namer("idvm-rescall-loc-returning", idvm-rescall-loc-returning);

constant-namer("idvm-rescall-lit-lit", idvm-rescall-lit-lit);

constant-namer("idvm-rescall-lit-lit-returning", idvm-rescall-lit-lit-returning);

constant-namer("idvm-rescall-lit-loc", idvm-rescall-lit-loc);

constant-namer("idvm-rescall-lit-loc-returning", idvm-rescall-lit-loc-returning);

constant-namer("idvm-rescall-loc-lit", idvm-rescall-loc-lit);

constant-namer("idvm-rescall-loc-lit-returning", idvm-rescall-loc-lit-returning);

constant-namer("idvm-rescall-loc-loc", idvm-rescall-loc-loc);

constant-namer("idvm-rescall-loc-loc-returning", idvm-rescall-loc-loc-returning);

constant-namer("idvm-rescall-n", idvm-rescall-n);

constant-namer("idvm-rescall-n-returning", idvm-rescall-n-returning);

constant-namer("idvm-jump-true", idvm-jump-true);

constant-namer("idvm-jump-false", idvm-jump-false);

constant-namer("idvm-jump", idvm-jump);

constant-namer("idvm-loc-br-lt", idvm-loc-br-lt);

constant-namer("idvm-loc-br-gt", idvm-loc-br-gt);

constant-namer("idvm-loc-br-le", idvm-loc-br-le);

constant-namer("idvm-loc-br-ge", idvm-loc-br-ge);

constant-namer("idvm-loc-br-eq", idvm-loc-br-eq);

constant-namer("idvm-loc-br-ne", idvm-loc-br-ne);

constant-namer("idvm-loc-br-ideq", idvm-loc-br-ideq);

constant-namer("idvm-loc-br-idne", idvm-loc-br-idne);

constant-namer("idvm-lit-br-lt", idvm-lit-br-lt);

constant-namer("idvm-lit-br-gt", idvm-lit-br-gt);

constant-namer("idvm-lit-br-le", idvm-lit-br-le);

constant-namer("idvm-lit-br-ge", idvm-lit-br-ge);

constant-namer("idvm-lit-br-eq", idvm-lit-br-eq);

constant-namer("idvm-lit-br-ne", idvm-lit-br-ne);

constant-namer("idvm-lit-br-ideq", idvm-lit-br-ideq);

constant-namer("idvm-lit-br-idne", idvm-lit-br-idne);

constant-namer("idvm-res-loc-br-lt", idvm-res-loc-br-lt);

constant-namer("idvm-res-loc-br-gt", idvm-res-loc-br-gt);

constant-namer("idvm-res-loc-br-le", idvm-res-loc-br-le);

constant-namer("idvm-res-loc-br-ge", idvm-res-loc-br-ge);

constant-namer("idvm-res-loc-br-eq", idvm-res-loc-br-eq);

constant-namer("idvm-res-loc-br-ne", idvm-res-loc-br-ne);

constant-namer("idvm-res-loc-br-ideq", idvm-res-loc-br-ideq);

constant-namer("idvm-res-loc-br-idne", idvm-res-loc-br-idne);

constant-namer("idvm-res-lit-br-lt", idvm-res-lit-br-lt);

constant-namer("idvm-res-lit-br-gt", idvm-res-lit-br-gt);

constant-namer("idvm-res-lit-br-le", idvm-res-lit-br-le);

constant-namer("idvm-res-lit-br-ge", idvm-res-lit-br-ge);

constant-namer("idvm-res-lit-br-eq", idvm-res-lit-br-eq);

constant-namer("idvm-res-lit-br-ne", idvm-res-lit-br-ne);

constant-namer("idvm-res-lit-br-ideq", idvm-res-lit-br-ideq);

constant-namer("idvm-res-lit-br-idne", idvm-res-lit-br-idne);

constant-namer("idvm-loc-lt", idvm-loc-lt);

constant-namer("idvm-loc-gt", idvm-loc-gt);

constant-namer("idvm-loc-le", idvm-loc-le);

constant-namer("idvm-loc-ge", idvm-loc-ge);

constant-namer("idvm-loc-eq", idvm-loc-eq);

constant-namer("idvm-loc-ne", idvm-loc-ne);

constant-namer("idvm-loc-ideq", idvm-loc-ideq);

constant-namer("idvm-loc-idne", idvm-loc-idne);

constant-namer("idvm-lit-lt", idvm-lit-lt);

constant-namer("idvm-lit-gt", idvm-lit-gt);

constant-namer("idvm-lit-le", idvm-lit-le);

constant-namer("idvm-lit-ge", idvm-lit-ge);

constant-namer("idvm-lit-eq", idvm-lit-eq);

constant-namer("idvm-lit-ne", idvm-lit-ne);

constant-namer("idvm-lit-ideq", idvm-lit-ideq);

constant-namer("idvm-lit-idne", idvm-lit-idne);

constant-namer("idvm-res-loc-lt", idvm-res-loc-lt);

constant-namer("idvm-res-loc-gt", idvm-res-loc-gt);

constant-namer("idvm-res-loc-le", idvm-res-loc-le);

constant-namer("idvm-res-loc-ge", idvm-res-loc-ge);

constant-namer("idvm-res-loc-eq", idvm-res-loc-eq);

constant-namer("idvm-res-loc-ne", idvm-res-loc-ne);

constant-namer("idvm-res-loc-ideq", idvm-res-loc-ideq);

constant-namer("idvm-res-loc-idne", idvm-res-loc-idne);

constant-namer("idvm-res-lit-lt", idvm-res-lit-lt);

constant-namer("idvm-res-lit-gt", idvm-res-lit-gt);

constant-namer("idvm-res-lit-le", idvm-res-lit-le);

constant-namer("idvm-res-lit-ge", idvm-res-lit-ge);

constant-namer("idvm-res-lit-eq", idvm-res-lit-eq);

constant-namer("idvm-res-lit-ne", idvm-res-lit-ne);

constant-namer("idvm-res-lit-ideq", idvm-res-lit-ideq);

constant-namer("idvm-res-lit-idne", idvm-res-lit-idne);

constant-namer("idvm-loc-add", idvm-loc-add);

constant-namer("idvm-loc-sub", idvm-loc-sub);

constant-namer("idvm-lit-add", idvm-lit-add);

constant-namer("idvm-lit-sub", idvm-lit-sub);

constant-namer("idvm-res-loc-add", idvm-res-loc-add);

constant-namer("idvm-res-loc-sub", idvm-res-loc-sub);

constant-namer("idvm-res-lit-add", idvm-res-lit-add);

constant-namer("idvm-res-lit-sub", idvm-res-lit-sub);

constant-namer("idvm-res-gets-lit", idvm-res-gets-lit);

constant-namer("idvm-loc-gets-lit", idvm-loc-gets-lit);

constant-namer("idvm-res-gets-loc", idvm-res-gets-loc);

constant-namer("idvm-loc-gets-res", idvm-loc-gets-res);

constant-namer("idvm-loc-gets-loc", idvm-loc-gets-loc);

constant-namer("idvm-make-value-cell", idvm-make-value-cell);

constant-namer("idvm-new-value-cell-res", idvm-new-value-cell-res);

constant-namer("idvm-new-value-cell-lit", idvm-new-value-cell-lit);

constant-namer("idvm-new-value-cell-loc", idvm-new-value-cell-loc);

constant-namer("idvm-res-gets-ev", idvm-res-gets-ev);

constant-namer("idvm-res-gets-evc", idvm-res-gets-evc);

constant-namer("idvm-res-gets-vc", idvm-res-gets-vc);

constant-namer("idvm-loc-gets-ev", idvm-loc-gets-ev);

constant-namer("idvm-loc-gets-evc", idvm-loc-gets-evc);

constant-namer("idvm-loc-gets-vc", idvm-loc-gets-vc);

constant-namer("idvm-evc-gets-res", idvm-evc-gets-res);

constant-namer("idvm-vc-gets-res", idvm-vc-gets-res);

constant-namer("idvm-evc-gets-lit", idvm-evc-gets-lit);

constant-namer("idvm-vc-gets-lit", idvm-vc-gets-lit);

constant-namer("idvm-make-closure", idvm-make-closure);

constant-namer("idvm-make-closure-copying", idvm-make-closure-copying);

constant-namer("idvm-make-closure-copying-with-specs", idvm-make-closure-copying-with-specs);

constant-namer("idvm-unwind-protect", idvm-unwind-protect);

constant-namer("idvm-unwind-protect-returning", idvm-unwind-protect-returning);

constant-namer("idvm-handler-bind-lit", idvm-handler-bind-lit);

constant-namer("idvm-handler-bind-loc", idvm-handler-bind-loc);

constant-namer("idvm-handler-bind-lit-returning", idvm-handler-bind-lit-returning);

constant-namer("idvm-handler-bind-loc-returning", idvm-handler-bind-loc-returning);

constant-namer("idvm-bind-exit", idvm-bind-exit);

constant-namer("idvm-bind-exit-returning", idvm-bind-exit-returning);

constant-namer("idvm-mv-bind", idvm-mv-bind);

constant-namer("idvm-mv-bind-rest", idvm-mv-bind-rest);

//constant-namer("idvm-process-keys", idvm-process-keys);
