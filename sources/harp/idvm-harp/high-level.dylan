module:    idvm-harp
Synopsis:  High level templates for the idvm backend
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// bind-exit


with-ops-in idvm-instructions (vm-bind-exit, vm-bind-exit-returning)
  clash-fn :=  idvm-method (td)
                 list(list(def(1), res));
               end idvm-method;
end with-ops-in;


define idvm-template (vm-bind-exit)
  pattern (be, t, d by env/spill-ref)
    emit-bind-exit(be, t, d);
end idvm-template;


define idvm-template (vm-bind-exit-returning)
  pattern (be, t, d by env/spill-ref)
    emit-bind-exit-returning(be, d);
end idvm-template;




/// unwind-protect


with-ops-in idvm-instructions (vm-unwind-protect)
  info := emit-unwind-protect;
end with-ops-in;

with-ops-in idvm-instructions (vm-unwind-protect-returning)
  info := emit-unwind-protect-returning;
end with-ops-in;


define idvm-template (vm-unwind-protect, vm-unwind-protect-returning)
  options (self);

  pattern (be, emitter by op-info, t, u)
    emitter(be, t, u);

end idvm-template;



/// multiple-value-bind

with-ops-in idvm-instructions (vm-mv-bind)
  info := emit-mv-bind;
end with-ops-in;

with-ops-in idvm-instructions (vm-mv-bind-rest)
  info := emit-mv-bind-rest;
end with-ops-in;


define idvm-template (vm-mv-bind, vm-mv-bind-rest)
  options (self);

  pattern (be, emitter by op-info, t, u)
    emitter(be, t, be.arg-area-start-index, u);
end idvm-template;


define method ensure-machine-register-clash (#rest all-regs)
  let regs = as(<list>, all-regs);
  list(add!(add!(regs, res), env));
end method;

with-ops-in idvm-instructions (vm-mv-bind-finished)
  clash-fn :=  idvm-method (d)
                 let all-dest = def(1);  // D* vector 
                 apply(ensure-machine-register-clash, all-dest);
               end idvm-method;
end with-ops-in;


define idvm-template (vm-mv-bind-finished)
  pattern (be, num)
    // This HARP instruction generates no code.
    #f;
end idvm-template;



/// Closures


// NOTIFY-INNER-CLOSURE-VARIABLES
// The harp-cg library should define a method on this GF, so that indices
// are registered correctly

define open generic notify-inner-closure-variables 
   (be :: <idvm-back-end>, ref :: <constant-reference>, indices :: <vector>)
   => ();

// Dummy method on NOTIFY-INNER-CLOSURE-VARIABLES
// The harp-cg layer is expected to shadow this one.

define method notify-inner-closure-variables 
   (be :: <idvm-back-end>, ref :: <constant-reference>, indices :: <vector>)
   => ();
end method;


define method notify-closure-registers 
   (be :: <idvm-back-end>, ref :: <constant-reference>, 
    registers :: <simple-object-vector>)
   => ()
  let indices = map(curry(local-index, be), registers);
  notify-inner-closure-variables(be, ref, indices);
end method;


with-ops-in idvm-instructions (make-closure)
  destroys-fn := res-fn;
  clash-fn :=  idvm-method (duu)
                 let closes :: <simple-object-vector> = uze(2);
                 let clashes = #();
                 for (u in closes)
                   clashes := pair(list(u, res), clashes);
                 end for;
                 clashes;
               end idvm-method;
end with-ops-in;


define idvm-template make-closure

  pattern (be, d by res-ref, master by address-constant-ref, u2)
    notify-closure-registers(be, master, u2);
    emit-make-closure-copying(be, master);

  pattern (be, d by env/spill-ref, master by address-constant-ref, u2)
    notify-closure-registers(be, master, u2);
    emit-make-closure-copying(be, master);
    harp-out (be) move(be, d, res) end;

end idvm-template;

with-ops-in idvm-instructions (make-closure-with-specs)
  destroys-fn := res-fn;
  clash-fn :=  idvm-method (duuu)
                 let closes :: <simple-object-vector> = uze(3);
                 let clashes = list(list(uze(2), res));
                 for (u in closes)
                   clashes := pair(list(u, res), clashes);
                 end for;
                 clashes;
               end idvm-method;
end with-ops-in;


define idvm-template make-closure-with-specs

  pattern (be, d by res-ref, master by address-constant-ref, u2 by env/spill-ref, u3)
    notify-closure-registers(be, master, u3);
    emit-make-closure-copying-with-specs(be, master, u2);

  pattern (be, d by env/spill-ref, master by address-constant-ref, u2 by env/spill-ref, u3)
    notify-closure-registers(be, master, u3);
    emit-make-closure-copying-with-specs(be, master, u2);
    harp-out (be) move(be, d, res) end;

end idvm-template;


with-ops-in idvm-instructions (move-env, ld-env, st-env)
  implicit-uses := idvm-method ()
                     env.real-register-mask;
                   end idvm-method;
end with-ops-in;


define idvm-template (move-env)

  pattern (be, d by res-ref, u by const-ref)
    emit-res-gets-ev(be, u);

  pattern (be, d by env/spill-ref, u by const-ref)
    emit-loc-gets-ev(be, d, u);

end idvm-template;


define idvm-template (ld-env)

  pattern (be, d by res-ref, u by const-ref)
    emit-res-gets-evc(be, u);

  pattern (be, d by env/spill-ref, u by const-ref)
    emit-loc-gets-evc(be, d, u);

end idvm-template;



with-ops-in idvm-instructions (ld-vc)
  clash-fn :=  idvm-method (du)
                 list(list(uze(1), res));
               end idvm-method;
end with-ops-in;


define idvm-template (ld-vc)

  pattern (be, d by res-ref, u by env/spill-ref)
    emit-res-gets-vc(be, u);

  pattern (be, d by env/spill-ref, u by env/spill-ref)
    emit-loc-gets-vc(be, d, u);

end idvm-template;



define idvm-template (st-env)

  pattern (be, s by res-ref, u by const-ref)
    emit-evc-gets-res(be, u);

  pattern (be, s by env/spill-ref, u by const-ref)
    emit-evc-gets-loc(be, s, u);

  pattern (be, s, u by const-ref)
    emit-evc-gets-lit(be, u, s);

end idvm-template;



with-ops-in idvm-instructions (st-vc)
  clash-fn :=  idvm-method (uu)
                 list(list(uze(2), res));
               end idvm-method;
end with-ops-in;


define idvm-template (st-vc)

  pattern (be, s by res-ref, u by env/spill-ref)
    emit-vc-gets-res(be, u);

  pattern (be, s by env/spill-ref, u by env/spill-ref)
    emit-vc-gets-loc(be, s, u);

  pattern (be, s, u by env/spill-ref)
    emit-vc-gets-lit(be, u, s);

end idvm-template;



with-ops-in idvm-instructions (make-value-cell)
  clash-fn :=  idvm-method (du)
                 list(list(def(1), res));
               end idvm-method;
end with-ops-in;


define idvm-template make-value-cell

  // Special case source and dest the same
  pattern (be, d by env/spill-ref, d)
    emit-make-value-cell(be, d);

  // Value in result
  pattern (be, d by env/spill-ref, u by res-ref)
    emit-new-value-cell-res(be, d);

  // Value in a local. 
  pattern (be, d by env/spill-ref, u by env/spill-ref)
    emit-new-value-cell-loc(be, d, u);
      
  // Literal value 
  pattern (be, d by env/spill-ref, u)
    emit-new-value-cell-lit(be, d, u);

end idvm-template;

