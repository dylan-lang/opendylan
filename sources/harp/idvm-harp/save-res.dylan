module:    idvm-harp
Synopsis:  Idvm save and restore instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// LOAD-STACK-ARG-N
/// By clashing the def with res and env, we ensure that the template never
/// has to do anything.

with-ops-in idvm-instructions (load-stack-arg-n)
  clash-fn :=  idvm-method (du)
                 list(list(def(1), res, env));
               end idvm-method;
end with-ops-in;

define idvm-template load-stack-arg-n 
  pattern (be, stack? :: any, d :: any, n)
    #f;
end idvm-template;



/// LOAD-VM-ARG-N takes N as a constant offset in words. The offset is the 
/// index into the calling-parameters area (as opposed to the received
/// parameters area). The implementation is a bit like MOVE.
///
/// The instruction cannot be eliminatable, because we rely on the usage
/// of this instruction after operations which are destructive on the locals
/// vector (vm-mv-bind, for example) - and HARP must see that side-effected
/// register gets used.

define idvm-template load-vm-arg-n 

  // first check for dest as a spill (may be eliminable case)
  pattern (be, d by env/spill-ref, n)
    let loc-for-n = n + be.arg-area-start-index;
    let loc-for-d = local-index(be, d);
    unless (loc-for-n == loc-for-d)
       emit-loc-gets-loc(be, d, loc-for-n);
    end unless;

  // Register destination and spill-ref source
  pattern (be, d by res-ref, n)
    let loc-for-n = n + be.arg-area-start-index;
    emit-res-gets-loc(be, loc-for-n);

  // "Eliminated" case
  pattern (be, d :: any, n)
    #f;
end idvm-template;



/// STORE-VM-ARG-N is conceptually the opposite of LOAD-VM-ARG-N.
/// It's implemented at instantiation time by converting N to a
/// virtual register - so we can template it in terms of MOVE.
///
/// The instruction is spread as a UU instruction - but output as a DUU.
/// This means we can clash against the def too.
/// By clashing the def with res and env, we ensure that the def will, 
/// indeed, be in the desired spill location. By clashing the use against env,
///  too, we avoid poor colouring choices.

with-ops-in idvm-instructions (store-vm-arg-n)
  clash-fn :=  idvm-method (duu)
                 list(list(def(1), res, env),
                      list(uze(1), env));
               end idvm-method;
end with-ops-in;

define idvm-template store-vm-arg-n 
  pattern (be, u, index)
    // First calculate the D part of the DUU - because we don't 
    // want to make it visible in the spread.
    let dest = vm-arg-register(be, index);
    let vm-index = index + be.arg-area-start-index;
    let d = dest.virtual-register-colour;
    if (d.spill-ref & (local-index(be, d) == vm-index))
      harp-out (be) move(be, d, u) end;
    else
      // A quick consistency check. The clash function should avoid this.
      harp-error("Incorrectly allocated argument register");
    end if;
end idvm-template;



/// return-value and vm-returning mean the same to the VM (i.e. return
/// from VM thread) They are different from HARP's control flow viewpoint.

define idvm-template (return-value, vm-returning)

  // value in result
  pattern (be, value by res-ref)
    emit-return(be);

  // value is a local
  pattern (be, value by env/spill-ref)
    emit-return-loc(be, value);

  // value is a literal
  pattern (be, value)
    emit-return-lit(be, value);

end idvm-template;




/// return-any-values purely exists for HARP's flow-control reasons.

define idvm-template return-any-values
  pattern (be)
    #f;
end idvm-template;

