Module:    IDVM
Language:  infix-dylan
Synopsis:  Simple information holder for vm methods in the IDVM + VM builder
Author:    Eliot Miranda, Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




define class <value-cell> (<object>)
    slot value :: <object>, init-keyword: value:
end;

define method my-debug-name(vc :: <value-cell>)
    if (slot-initialized?(vc,value))
        concatenate("{",vc.value.my-debug-name,"}")
    else
        "{??}"
    end
end;


// Pair of methods for union types before their arrival in the language.  TUC = Trades Union Council
define method optional (clss :: <object>)
//  union(clss, #f.singleton)
    <object>
end;

define method tuc (classA :: <object>, classB :: <object>)
//   union(classA, classB)
    <object>
end;

define class <vm-method-info> (<object>) 
    slot vm-code-vec        :: <simple-object-vector>,                           required-init-keyword: vm-code-vec:;
    slot parameter-type     :: <object>,                 required-init-keyword: parameters:;
// NB could pack stack-size, the boolean flags and status in single integer
    slot stack-size         :: <integer>,                         required-init-keyword: stack-size:;
    slot uses-next-method   :: <boolean>,                  init-value: #f, init-keyword: uses-next-method:;
    slot uses-rest          :: <boolean>,                  init-value: #f, init-keyword: uses-rest:;
    slot takes-all-keys     :: <boolean>,                  init-value: #f, init-keyword: takes-all-keys:;
    slot key-value-pairs    :: <object>,          init-value: #f, init-keyword: key-value-pairs:;
    slot outer-info         :: <object>, init-value: #f, init-keyword: outer-info:;
// NB This can be better encoded as an arbitrary precision integer or union(<byte-array>,<integer>)
    slot closed-var-indices :: <object>,          init-value: #f, init-keyword: closed-var-indices:;
    slot the-method-name    :: <symbol>,                   init-value: #"unknown function", init-keyword: method-name:;
    slot status             :: <symbol>,                   init-value: #"normal";
    slot vm-break?          :: <boolean>,                  init-value: #f;
    slot vm-step?           :: <boolean>,                  init-value: #f;
    slot vm-trace?          :: <boolean>,                  init-value: #f;
    slot vm-trace-ins?      :: <boolean>,                  init-value: #f;
end;

// define method \= (infoa :: <vm-method-info>, infob :: <vm-method-info>) => <boolean>;
//      (infoa.parameter-type = infob.parameter-type)
//    & (infoa.method-name = infob.method-name)
// end;

define method idvm-build-environment(info :: <vm-method-info>,vars :: <simple-object-vector>)
    if (info.closed-var-indices)
        map(method (i) vars[i] end, info.closed-var-indices)
    end
end;

define method is-a-method  (info :: <vm-method-info>) => <boolean>; ~ info.outer-info end;
define method is-a-closure (info :: <vm-method-info>) => <boolean>;   info.outer-info end;

define method arg-count (info :: <vm-method-info>) info.parameter-type.size end;

define method uses-key-or-rest (info :: <vm-method-info>)
    info.uses-rest | info.key-value-pairs
end;

define method method-name (info :: <vm-method-info>)
  let outer = info.outer-info;

  if (outer)
    method-name(outer)
  else
    info.the-method-name
  end
end;


define method add-prolog-and-build-static-chain (info :: <vm-method-info>)
  let vm-code :: <simple-object-vector> = info.vm-code-vec;

  for (i from 0 below vm-code.size)
    if (  vm-code[i] == idvm-make-closure
        | vm-code[i] == idvm-make-closure-copying)
      let inner-info = vm-code[i + 1];

      inner-info.outer-info := info;
      add-prolog-and-build-static-chain(inner-info);
    end;
  end;

  vm-code[0] := info;

  /* The compiler now generates the vm-code-vec in all its glory
  info.vm-code-vec := concatenate(if (info.key-value-pairs)
                                    vector(info,idvm-process-keys)
                                  else
                                    vector(info)
                                  end,
                                  vm-code);
  */
end;


define method build-vm(para-list :: <simple-object-vector>, code-vec :: <simple-object-vector>, stack-size :: <integer>, #rest keys)
    let info = apply(make, <vm-method-info>,
                           parameters: para-list,
                           vm-code-vec: code-vec,
                           stack-size: stack-size,
//                           uses-next-method: #t, // This is _only_ necessary to work around the translator
                                                 // which to relys on CLOS to our detriment.
                                                 // Under the translator to get method (closure creation)
                                                 // to build a method that can be added to a generic
                                                 // function the method must have a #next parameter.
                                                 // Under the translator closures _must not_ have a #next
                                                 // parameter, otherwise closure invocation fails.
                                                 // So we manage to use a single builder for both generic
                                                 // function methods and closures by _always_ creating
                                                 // generic function methods with a #next parameter.
                           keys);
    add-prolog-and-build-static-chain(info);
    let the-method = build-IDVM-method(info);

    unless(info.the-method-name = top-level-form:)
      debug-method(*idvm-byte*, the-method);
    end unless;

    the-method;
end;


// core method builder for IDVM code.
//
// Problems: No handling of arg lists with more than N typed parameters (currently N = 2)
//           Doesn't obey parameter list congruency rules for #key parameters (implementation
//           depends on passing key value pairs via #rest)
//
// Calling Convention for first idvm instruction:
//
//   next-method/#f passed in via result reg
//   environment in local 0 (#f in methods and closures with no outer environment)
//   first arg always in local 1
//   #rest follows
//   key-values follows



define method build-IDVM-method(info :: <vm-method-info>, #key env = #f)
  let stack-size  = info.stack-size;
  let vm-code     = info.vm-code-vec;
  let argc        = info.arg-count;
  let p-types     = info.parameter-type;
  let with-key    = info.key-value-pairs;
  let with-rest   = info.uses-key-or-rest;
  let with-next   = info.uses-next-method;

  if (with-next)
    if (with-key) 
      // rest _always_ passed as local 0, next-method is passed in result
      let fixed-locals = argc + 2; // all the args, plus env plus rest
      if (stack-size <= fixed-locals + 4)
        select-idvm-closure (argc, p-types, #next nm, #rest rv)
          idvm-invoke-code((vm-code, nm, env), 
                           rv, #f, #f, #f, #f, process-keys: #t)
        end select-idvm-closure
      else
        select-idvm-closure (argc, p-types, #next nm, #rest rv)
          idvm-invoke-code((vm-code, nm, env), 
                           rv, stack-size: stack-size, process-keys: #t)
        end select-idvm-closure
      end if // stack-size
    elseif (with-rest) 
      // rest _always_ passed as local 0, next-method is passed in result
      let fixed-locals = argc + 2; // all the args, plus env plus rest
      if (stack-size <= fixed-locals + 4)
        select-idvm-closure (argc, p-types, #next nm, #rest rv)
          idvm-invoke-code((vm-code, nm, env), rv, #f, #f, #f, #f)
        end select-idvm-closure
      else
        select-idvm-closure (argc, p-types, #next nm, #rest rv)
          idvm-invoke-code((vm-code, nm, env), rv, stack-size: stack-size)
        end select-idvm-closure
      end if // stack-size
    else // with-rest
      let fixed-locals = argc + 1; // all the args, plus env
      if (stack-size <= fixed-locals + 4)
        select-idvm-closure (argc, p-types, #next nm)
          idvm-invoke-code((vm-code, nm, env), #f, #f, #f, #f)
        end select-idvm-closure
      else
        select-idvm-closure (argc, p-types, #next nm)
          idvm-invoke-code((vm-code, nm, env), stack-size: stack-size)
        end select-idvm-closure
      end if // stack-size

    end if // with-rest

  else // with-next
    if (with-key)
      let fixed-locals = argc + 2; // all the args, plus env plus rest
      if (stack-size <= fixed-locals + 4)
        select-idvm-closure (argc, p-types, #rest rv)
          idvm-invoke-code((vm-code, #f, env), 
                           rv, #f, #f, #f, #f, process-keys: #t)
        end select-idvm-closure
      else
        select-idvm-closure (argc, p-types, #rest rv)
          idvm-invoke-code((vm-code, #f, env), 
                           rv, stack-size: stack-size, process-keys: #t)
        end select-idvm-closure
      end if // stack-size
    elseif (with-rest)
      let fixed-locals = argc + 2; // all the args, plus env plus rest
      if (stack-size <= fixed-locals + 4)
        select-idvm-closure (argc, p-types, #rest rv)
          idvm-invoke-code((vm-code, #f, env), rv, #f, #f, #f, #f)
        end select-idvm-closure
      else
        select-idvm-closure (argc, p-types, #rest rv)
          idvm-invoke-code((vm-code, #f, env),  rv, stack-size: stack-size)
        end select-idvm-closure
      end if // stack-size
    else // with-rest
      let fixed-locals = argc + 1; // all the args, plus env
      if (stack-size <= fixed-locals + 4)
        select-idvm-closure (argc, p-types) 
          idvm-invoke-code((vm-code, #f, env), #f, #f, #f, #f)
        end select-idvm-closure
      else
        select-idvm-closure (argc, p-types)
          idvm-invoke-code((vm-code, #f, env), stack-size: stack-size)
        end select-idvm-closure
      end if // stack-size
    end if // with-rest
  end if // with-next
end method;



/* 
// Eliot's Old version


define method build-IDVM-method(info :: <vm-method-info>, #key env = #f)
  let stack-size  = info.stack-size;
  let vm-code     = info.vm-code-vec;

  if (info.uses-next-method)
    if (info.uses-key-or-rest) // rest _always_ passed as local 0, next-method is passed in result
      if (stack-size <= 8)
        select (info.arg-count)
         0 => method (#next nm, #rest rv)
                  invoke-code(vm-code, nm, locals: env, rv, #f, #f, #f, #f, #f)
              end method;

         1 => method (a0 :: info.parameter-type[0],
                      #next nm, #rest rv)
                  invoke-code(vm-code, nm, locals: env, a0, rv, #f, #f, #f, #f)
              end method;

         2 => method (a0 :: info.parameter-type[0], a1 :: info.parameter-type[1],
                      #next nm, #rest rv)
                  invoke-code(vm-code, nm, locals: env, a0, a1, rv, #f, #f, #f)
              end method;
        end select
      else
//        error("stack sizes above 8 not supported. Now bog off."); (rather a _bad_ attitude, methinks).
        select (info.arg-count)
         0 => method (#next nm, #rest rv)
                  let locals-vector = make(<simple-object-vector>, size: stack-size);
                  locals-vector[0] := env;
                  locals-vector[1] := rv;
                  invoke-code-with-vectored-locals(vm-code, nm, locals-vector: locals-vector)
              end method;

         1 => method (a0 :: info.parameter-type[0],
                      #next nm, #rest rv)
                  let locals-vector = make(<simple-object-vector>, size: stack-size);
                  locals-vector[0] := env;
                  locals-vector[1] := a0;
                  locals-vector[2] := rv;
                  invoke-code-with-vectored-locals(vm-code, nm, locals-vector: locals-vector)
              end method;

         2 => method (a0 :: info.parameter-type[0], a1 :: info.parameter-type[1],
                      #next nm, #rest rv)
                  let locals-vector = make(<simple-object-vector>, size: stack-size);
                  locals-vector[0] := env;
                  locals-vector[1] := a0;
                  locals-vector[2] := a1;
                  locals-vector[3] := rv;
                  invoke-code-with-vectored-locals(vm-code, nm, locals-vector: locals-vector)
              end method;
        end select
      end if // stack-size
    else // info.uses-key-or-rest
      if (stack-size <= 4)
        select (info.arg-count)
         0 => method (#next nm)
                  invoke-code(vm-code, nm, locals: env, #f, #f, #f)
              end method;

         1 => method (a0 :: info.parameter-type[0], #next nm)
                  invoke-code(vm-code, nm, locals: env, a0, #f, #f)
              end method;

         2 => method (a0 :: info.parameter-type[0], a1 :: info.parameter-type[1], #next nm)
                  invoke-code(vm-code, nm, locals: env, a0, a1, #f)
              end method;
        end select
      elseif (stack-size <= 8)
        select (info.arg-count)
         0 => method (#next nm)
                  invoke-code(vm-code, nm, locals: env, #f, #f, #f, #f, #f, #f, #f)
              end method;

         1 => method (a0 :: info.parameter-type[0], #next nm)
                  invoke-code(vm-code, nm, locals: env, a0, #f, #f, #f, #f, #f, #f)
              end method;

         2 => method (a0 :: info.parameter-type[0], a1 :: info.parameter-type[1], #next nm)
                  invoke-code(vm-code, nm, locals: env, a0, a1, #f, #f, #f, #f, #f)
              end method;
        end select
      else
        select (info.arg-count)
         0 => method (#next nm)
                  let locals-vector = make(<simple-object-vector>, size: stack-size);
                  locals-vector[0] := env;
                  invoke-code-with-vectored-locals(vm-code, nm, locals-vector: locals-vector)
              end method;

         1 => method (a0 :: info.parameter-type[0], #next nm)
                  let locals-vector = make(<simple-object-vector>, size: stack-size);
                  locals-vector[0] := env;
                  locals-vector[1] := a0;
                  invoke-code-with-vectored-locals(vm-code, nm, locals-vector: locals-vector)
              end method;

         2 => method (a0 :: info.parameter-type[0], a1 :: info.parameter-type[1], #next nm)
                  let locals-vector = make(<simple-object-vector>, size: stack-size);
                  locals-vector[0] := env;
                  locals-vector[1] := a0;
                  locals-vector[2] := a1;
                  invoke-code-with-vectored-locals(vm-code, nm, locals-vector: locals-vector)
              end method;
        end select
      end if // stack-size
    end if // info.uses-key-or-rest
  else // info.uses-next-method
    if (info.uses-key-or-rest)
      if (stack-size <= 4)
        select (info.arg-count)
         0 => method (#rest rv)
                  invoke-code(vm-code, #f, /*result*/
                              locals: env, rv, #f, #f)
              end method;

         1 => method (a0 :: info.parameter-type[0],
                      #rest rv)
                  invoke-code(vm-code, #f, /*result*/
                              locals: env, a0, rv, #f)
              end method;

         2 => method (a0 :: info.parameter-type[0], a1 :: info.parameter-type[1],
                      #rest rv)
                  invoke-code(vm-code, #f, /*result*/
                              locals: env, a0, a1, rv)
              end method;
        end select
      elseif (stack-size <= 8)
        select (info.arg-count)
         0 => method (#rest rv)
                  invoke-code(vm-code, #f, /*result*/
                              locals: env, rv, #f, #f, #f, #f, #f, #f)
              end method;

         1 => method (a0 :: info.parameter-type[0],
                      #rest rv)
                  invoke-code(vm-code, #f, /*result*/
                              locals: env, a0, rv, #f, #f, #f, #f, #f)
              end method;

         2 => method (a0 :: info.parameter-type[0], a1 :: info.parameter-type[1],
                      #rest rv)
                  invoke-code(vm-code, #f, /*result*/
                              locals: env, a0, a1, rv, #f, #f, #f, #f)
              end method;
        end select
      else
        select (info.arg-count)
         0 => method (#rest rv)
                  let locals-vector = make(<simple-object-vector>, size: stack-size);
                  locals-vector[0] := env;
                  locals-vector[1] := rv;
                  invoke-code-with-vectored-locals(vm-code, #f, /*result*/ locals-vector: locals-vector)
              end method;

         1 => method (a0 :: info.parameter-type[0],
                      #rest rv)
                  let locals-vector = make(<simple-object-vector>, size: stack-size);
                  locals-vector[0] := env;
                  locals-vector[1] := a0;
                  locals-vector[2] := rv;
                  invoke-code-with-vectored-locals(vm-code, #f, /*result*/ locals-vector: locals-vector)
              end method;

         2 => method (a0 :: info.parameter-type[0], a1 :: info.parameter-type[1],
                      #rest rv)
                  let locals-vector = make(<simple-object-vector>, size: stack-size);
                  locals-vector[0] := env;
                  locals-vector[1] := a0;
                  locals-vector[2] := a1;
                  locals-vector[3] := rv;
                  invoke-code-with-vectored-locals(vm-code, #f, /*result*/ locals-vector: locals-vector)
              end method;
        end select
      end if // stack-size
    else // info.uses-key-or-rest
      if (stack-size <= 4)
        select (info.arg-count)
         0 => method ()
                  invoke-code(vm-code, #f, /*result*/ locals: env, #f, #f, #f, #f)
              end method;

         1 => method (a0 :: info.parameter-type[0])
                  invoke-code(vm-code, #f, /*result*/ locals: env, a0, #f, #f, #f)
              end method;

         2 => method (a0 :: info.parameter-type[0], a1 :: info.parameter-type[1])
                  invoke-code(vm-code, #f, /*result*/ locals: env, a0, a1, #f, #f)
              end method;
        end select;
      elseif (stack-size <= 8)
        select (info.arg-count)
         0 => method ()
                  invoke-code(vm-code, #f, /*result*/ locals: env, #f, #f, #f, #f, #f, #f, #f, #f)
              end method;

         1 => method (a0 :: info.parameter-type[0])
                  invoke-code(vm-code, #f, /*result*/ locals: env, a0, #f, #f, #f, #f, #f, #f, #f)
              end method;

         2 => method (a0 :: info.parameter-type[0], a1 :: info.parameter-type[1])
                  invoke-code(vm-code, #f, /*result*/ locals: env, a0, a1, #f, #f, #f, #f, #f, #f)
              end method;
        end select
      else
        select (info.arg-count)
         0 => method ()
                  let locals-vector = make(<simple-object-vector>, size: stack-size);
                  locals-vector[0] := env;
                  invoke-code-with-vectored-locals(vm-code, #f, /*result*/ locals-vector: locals-vector)
              end method;

         1 => method (a0 :: info.parameter-type[0])
                  let locals-vector = make(<simple-object-vector>, size: stack-size);
                  locals-vector[0] := env;
                  locals-vector[1] := a0;
                  invoke-code-with-vectored-locals(vm-code, #f, /*result*/ locals-vector: locals-vector)
              end method;

         2 => method (a0 :: info.parameter-type[0], a1 :: info.parameter-type[1])
                  let locals-vector = make(<simple-object-vector>, size: stack-size);
                  locals-vector[0] := env;
                  locals-vector[1] := a0;
                  locals-vector[2] := a1;
                  invoke-code-with-vectored-locals(vm-code, #f, /*result*/ locals-vector: locals-vector)
              end method;
        end select
      end if // stack-size
    end if // info.uses-key-or-rest
  end if // info.uses-next-method
end method;


*/


define method idvm-invalid-method-error (arg-count :: <integer>)
  error("IDVM cannot build a method with %= required parameters.\n", arg-count);
end method;

// eof
