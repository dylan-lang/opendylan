module:    idvm-harp
Synopsis:  The Idvm HARP instruction set
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Jumps and calls for the IDVM instruction set


/// There are 3 types of calls / jumps. Each effects flow control
/// in a different way for both the IDVM itself, and for HARP:
///
///  call 		[IDVM flow continues]	[HARP flow continues]
///  call-returning 	[IDVM flow returns]	[HARP flow continues]
///  jmp	 	[IDVM flow returns]	[HARP flow returns]
///
/// The HARP control flow is described in instructions.dylan
/// So, here, we only have to distinguish between the IDVM cases.




/// Call with 0 args.
///
/// We can use res as a temporary - so it doesn't matter where
/// the function comes from.


with-ops-in idvm-instructions (vm-call-0) 
  info := vector(emit-rescall, emit-call);
end;

with-ops-in idvm-instructions  (vm-call-0-returning, vm-jmp-0)
  info := vector(emit-rescall-returning, emit-call-returning);
end;


define idvm-template  (vm-call-0, vm-call-0-returning, vm-jmp-0)
  options (self);

  pattern (be, emitters by op-info, function by res-ref)
    let emitter = emitters[0];
    emitter(be);

  pattern (be, emitters by op-info, function by m/spill-ref)
    let emitter = emitters[0];
    harp-out (be) move(be, res, function) end;
    emitter(be);

  pattern (be, emitters by op-info, function)
    let emitter = emitters[1];
    emitter(be, function);

end idvm-template;




/// Call with 1 arg.
///
/// Try to only use res as a temporary register. This is more tricky than 
/// it sounds. If the function is a literal, then we can always move the 
/// argument into res. However, if the function is a local, then it must be 
/// in res. This means we must clash the argument from res if the function
/// is a non-literal.


define constant call-1-clash-fn = 
  idvm-method (uu)
    if (instance?(uze(1), <register>))
      list(list(res, uze(2)));
    else
      #();
    end if;
  end idvm-method;


with-ops-in idvm-instructions (vm-call-1) 
  clash-fn := call-1-clash-fn;
  info := vector(emit-rescall-loc,
                 emit-rescall-lit,
                 emit-call-res,
                 emit-call-loc,
                 emit-call-lit);
end;

with-ops-in idvm-instructions  (vm-call-1-returning, vm-jmp-1)
  clash-fn := call-1-clash-fn;
  info := vector(emit-rescall-loc-returning,
                 emit-rescall-lit-returning,
                 emit-call-res-returning,
                 emit-call-loc-returning,
                 emit-call-lit-returning);
end;


define idvm-template  (vm-call-1, vm-call-1-returning, vm-jmp-1)
  options (self);

  /// First, handle the cases where the function is non-literal

  // function in res - argument in a local
  pattern (be, i by op-info, function by res-ref, arg1 by env/spill-ref)
    let emitter = i[0];
    emitter(be, arg1);

  // function in res - argument as a literal
  pattern (be, i by op-info, function by res-ref, arg1)
    let emitter = i[1];
    emitter(be, arg1);

  // function in local - (argument clashes with res - so move function there)
  pattern (be, i, function by env/spill-ref, arg1)
    harp-out (be) move(be, res, function) end;
    harp-reapply(be, i, res, arg1)

  /// Now the literal function cases

  // argument in res
  pattern (be, i by op-info, function, arg1 by res-ref)
    let emitter = i[2];
    emitter(be, function);

  // argument in a local
  pattern (be, i by op-info, function, arg1 by env/spill-ref)
    let emitter = i[3];
    emitter(be, function, arg1);

  // argument as a literal
  pattern (be, i by op-info, function, arg1)
    let emitter = i[4];
    emitter(be, function, arg1);

end idvm-template;





/// Call with 2 arg.
///
/// Again, try to only use res as a temporary register. 
/// At IDVM level, the arguments are never permitted to be in res, 
/// and there is direct support for literal arguments - so the clash 
/// function is easy. Just clash arg1 and arg2 from res.


define constant call-2-clash-fn = 
  idvm-method (uuu)
    list(list(res, uze(2), uze(3)));
  end idvm-method;

with-ops-in idvm-instructions (vm-call-2) 
  clash-fn := call-2-clash-fn;
  info := vector(emit-rescall-loc-loc, 
                 emit-rescall-lit-loc, 
                 emit-rescall-loc-lit, 
                 emit-rescall-lit-lit,
                 emit-call-loc-loc, 
                 emit-call-lit-loc, 
                 emit-call-loc-lit, 
                 emit-call-lit-lit);
end;

with-ops-in idvm-instructions  (vm-call-2-returning, vm-jmp-2)
  clash-fn := call-2-clash-fn;
  info := vector(emit-rescall-loc-loc-returning, 
                 emit-rescall-lit-loc-returning, 
                 emit-rescall-loc-lit-returning, 
                 emit-rescall-lit-lit-returning,
                 emit-call-loc-loc-returning, 
                 emit-call-lit-loc-returning, 
                 emit-call-loc-lit-returning, 
                 emit-call-lit-lit-returning);
end;


define idvm-template (vm-call-2, vm-call-2-returning, vm-jmp-2)
  options (self);

  /// First, handle the cases where the function is non-literal

  // function in local - (arguments clash with res - so move function there)
  pattern (be, i, function by env/spill-ref, arg1, arg2)
    harp-out (be) move(be, res, function) end;
    harp-reapply(be, i, res, arg1, arg2)

  // function in res - arguments are loc loc
  pattern (be, i by op-info, function by res-ref, 
           arg1 by env/spill-ref, arg2 by env/spill-ref)
    let emitter = i[0];
    emitter(be, arg1, arg2);

  // function in res - arguments are lit loc
  pattern (be, i by op-info, function by res-ref, 
           arg1, arg2 by env/spill-ref)
    let emitter = i[1];
    emitter(be, arg1, arg2);

  // function in res - arguments are loc lit
  pattern (be, i by op-info, function by res-ref, 
           arg1 by env/spill-ref, arg2)
    let emitter = i[2];
    emitter(be, arg1, arg2);

  // function in res - arguments are lit lit
  pattern (be, i by op-info, function by res-ref, arg1, arg2)
    let emitter = i[3];
    emitter(be, arg1, arg2);


  /// Now the literal function cases

  // arguments are loc loc
  pattern (be, i by op-info, function, 
           arg1 by env/spill-ref, arg2 by env/spill-ref)
    let emitter = i[4];
    emitter(be, function, arg1, arg2);

  // arguments are lit loc
  pattern (be, i by op-info, function, arg1, arg2 by env/spill-ref)
    let emitter = i[5];
    emitter(be, function, arg1, arg2);

  // arguments are loc lit
  pattern (be, i by op-info, function, arg1 by env/spill-ref, arg2)
    let emitter = i[6];
    emitter(be, function, arg1, arg2);

  // arguments are lit lit
  pattern (be, i by op-info, function, arg1, arg2)
    let emitter = i[7];
    emitter(be, function, arg1, arg2);

end idvm-template;



///call-n will need this:  arg-area-start-index(be)


/// Call with N args.
///
/// Again, only use res as a temporary register. No clashes are needed
/// as the arguments are in the locals.


with-ops-in idvm-instructions (vm-call-n) 
  info := vector(emit-rescall-n,
                 emit-call-n);
end;

with-ops-in idvm-instructions (vm-call-n-returning, vm-jmp-n) 
  info := vector(emit-rescall-n-returning,
                 emit-call-n-returning);
end;


define idvm-template  (vm-call-n, vm-call-n-returning, vm-jmp-n)
  options (self);

  // function in local - move it to res
  pattern (be, i, function by env/spill-ref, argnum)
    harp-out (be) move(be, res, function) end;
    harp-reapply(be, i, res, argnum)

  // function in res
  pattern (be, i by op-info, function by res-ref, argnum)
    let emitter = i[0];
    let arg-start = be.arg-area-start-index;
    emitter(be, argnum, arg-start);

  // function as a literal
  pattern (be, i by op-info, function, argnum)
    let emitter = i[1];
    let arg-start = be.arg-area-start-index;
    emitter(be, function, argnum, arg-start);

end idvm-template;
