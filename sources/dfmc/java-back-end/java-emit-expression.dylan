Module: dfmc-java-back-end
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




// Called at top level by the expression-tree stuff -
// duty is to generate code to evaluate the "expression" (ie all the
// computations in the "nodes" set), and store the result in the root
// "node"'s temporary (unless its #f, in which case must be side-effects)
define function emit-expression-tree-and-store (jbb :: <java-basic-block>, node, nodes) => ()
  let  temp = node.temporary;
  if (temp == #f | has-spurious-temp? (node) | temp.users.empty?)
    emit-expression-tree (jbb, node, nodes, #f)
  else
    let  value? =
      if (instance? (temp, <multiple-value-temporary>)) #t else 1 end;

    emit-expression-tree (jbb, node, nodes, value?);

    let  (env, slot) = maybe-find-in-env (temp);
    if (env)
      // if the temp is really a closed-over variable, we must deal differently
      emit-env-write (jbb, env, slot)
    else
      // standard idiom for popping into a temporary's Java local variable
      // SHOULD USE j-code-for and know the java-type!
      emit-pop-local (jbb, temp.number-local-var, j-ref-code)
    end
  end
end;


// some computations have a temporary, even though they don't return anything, DUH!
define sealed generic has-spurious-temp? (c :: <object>) => (spurious? :: <boolean>);

define method has-spurious-temp? (c :: <object>)    => (spurious? :: <boolean>)      #f end;
define method has-spurious-temp? (c :: <loop-call>) => (spurious? :: <boolean>)      #t end;
define method has-spurious-temp? (c :: <unwind-protect>) => (spurious? :: <boolean>) #t end;



// the value? argument is #f for value not used, 1 for single value used and #t otherwise

define sealed generic emit-expression-tree (jbb :: <java-basic-block>,
                                     node :: <object>,  // <computation>
                                     nodes,
                                     value?) =>
                                    ();

define sealed generic emit-expression (jbb :: <java-basic-block>,
                                node :: <value-reference>,
                                nodes,
                                value?) =>
                               ();

define sealed generic emit-expression-leaf (jbb :: <java-basic-block>, node) => ();


/* badly typed?!
define method emit-expression (jbb :: <java-basic-block>, node :: <binding-reference>, nodes, value?) => ()
break ();  // shouldn't be this generic
  emit-expression-leaf (jbb, node.referenced-binding)
end;
*/

define method emit-expression (jbb :: <java-basic-block>, node :: <defined-constant-reference>, nodes, value?) => ()
  format-out ("<defined-constant-reference>:\n");
  emit-expression-leaf (jbb, node.referenced-binding)
end;


// why are we getting <unbound> - is this really a compiler error?
//define method emit-expression-tree (jbb :: <java-basic-block>, node :: <unbound>, nodes, value?) => ()
//  if (value?)
//    emit-java-null (jbb)
//  end
//end;


define method emit-expression-tree (jbb :: <java-basic-block>, node :: <end-exit-block>, nodes, value?) => ()
  if (value?)
    format-out ("  WHOOPs, end-exit-block in value context?\n");
    break ();
  end;
//  java-branch-op (jbb, j-goto, node.entry-state.me-block.next-computation); // was completion-computation
end;


define method emit-expression (jbb :: <java-basic-block>, node :: <temporary>, nodes, value?) => ()
  let  gen = node.generator;
  // is this right - there might be more than one use, we
  // should force value? if so?  Perhaps nature of temporary
  // should control value?ness, and can loose it from these calls?
  let  (env, slot) = maybe-find-in-env (node);
  if (env)
    if (member? (gen, nodes))
      emit-expression-tree (jbb, gen, nodes, if (value?) 1 else #f end);
//      if (value?)
//        emit-dup (jbb);
//        emit-env-write (jbb, env, slot)
//      else
//        emit-env-write (jbb, env, slot)
//      end
    else
      emit-env-read (jbb, env, slot)
    end
  else
    if (member? (gen, nodes))
      emit-expression-tree (jbb, gen, nodes, value?)
    else
      if (value?)
        emit-expression-leaf (jbb, node)
      end
    end
  end
end;


define method emit-expression-leaf (jbb :: <java-basic-block>, node :: <computation>) => ()
  format-out ("WHOOPS!, emit-expression-leaf called with %s of type %s\n", node, node.object-class);
  break();
  emit-expression-leaf (jbb, #t)
end;



define function emit-java-null (jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-aconst-null)
end;


// Oh ARSE, the hierarchy is sealed - have to deal with this as a separate class
define sealed class <merging-temporary> (<temporary>)
  sealed slot actual-temporary :: <temporary>, required-init-keyword: actual-temporary:;
end;

define method print-object (t :: <merging-temporary>, stream :: <stream>) => ()
  format (stream, "M%s", t.actual-temporary)
end;


define sealed generic number-local-var (tt :: <temporary>) => (num :: <integer>);

define method number-local-var (tt :: <merging-temporary>) => (num :: <integer>)
  number-local-var (tt.actual-temporary)
end;

// we hash on the frame-offset so that we can share marged vars
define method number-local-var (tt :: <temporary>) => (num :: <integer>)
  let  key = tt.frame-offset;
  let  local-num = element (*temp-map*, key, default: #f);
  if (~local-num)
    local-num := get-temp-local-var ();
    *temp-map*[key] := local-num;
  end;
  local-num
end;

define function get-temp-local-var () => (num :: <integer>)
  let  local-num :: <integer> = *temp-seq*;
  *temp-seq* := local-num + 1;
  local-num
end;

define function next-local-var-num () => (num :: <integer>)
  *temp-seq*
end;


define method emit-expression-leaf (jbb :: <java-basic-block>, node :: <temporary>) => ()
  emit-push-local (jbb, node.number-local-var, j-ref-code);   // must get type &use J-CODE-FOR
end;







define method emit-raw-expression-leaf (jbb :: <java-basic-block>, int :: <integer>) => ()
  emit-java-int (jbb, int)
end;


define method emit-raw-expression-leaf (jbb :: <java-basic-block>, str :: <byte-string>) => ()
  emit-java-string (jbb, str)
end;

define method emit-expression-leaf (jbb :: <java-basic-block>, node :: <integer>) => ()
  emit-raw-expression-leaf (jbb, node);
  gen-raw-to-small-integer (jbb);
end;

define method emit-raw-expression-leaf (jbb :: <java-basic-block>, node :: <character>) => ()
  emit-raw-expression-leaf (jbb, as (<integer>, node))
end;
define method emit-expression-leaf (jbb :: <java-basic-block>, node :: <character>) => ()
  emit-expression-leaf (jbb, as (<integer>, node))
end;

define constant $dylan-true-slot$  = slot-spec ($dylan-class-<object>$, "Ptrue", $dylan-class-<boolean>$, #t);
define constant $dylan-false-slot$ = slot-spec ($dylan-class-<object>$, "Pfalse", $dylan-class-<boolean>$, #t);
define constant $dylan-empty-list-slot$ =
  slot-spec ($dylan-class-<object>$, "Pempty_list", $dylan-class-<empty-list>$, #t);
define constant $dylan-empty-vec-slot$  =
  slot-spec ($dylan-class-<object>$, "Psv_empty", $dylan-class-<simple-object-vector>$, #t);

define method emit-expression-leaf (jbb :: <java-basic-block>, node == #t) => ()
  java-read (jbb, $dylan-true-slot$)
end;

define method emit-expression-leaf (jbb :: <java-basic-block>, node == #f) => ()
  java-read (jbb, $dylan-false-slot$)
end;

//define method emit-expression-leaf (jbb :: <java-basic-block>, node :: <unbound>) => ()
//  format-out ("### seen an <unbound> value in java back-end\n");
//  emit-java-null (jbb)
//end;

define method emit-raw-expression-leaf (jbb :: <java-basic-block>, node == #t) => ()
  emit-raw-expression-leaf (jbb, 1)
end;
define method emit-raw-expression-leaf (jbb :: <java-basic-block>, node == #f) => ()
  emit-raw-expression-leaf (jbb, 0)
end;





define function get-symbol-slot-in-class (jc :: <java-concrete-class>, canonical :: <string>)
  let  slot-name = concatenate ("symJ", canonical.java-name-mangle);
  let  slot = slot-spec (jc, slot-name, $dylan-class-<symbol>$, #t);
  if (slot-not-already-present (jc, slot-name))
    let  concrete = jc.concrete-implementation;
    if (concrete)
      concrete.symbol-slots-list := pair (pair (canonical, slot), concrete.symbol-slots-list);
      java-field (slot)
    else
      error ("should have been a concrete class!")
    end
  end;
  slot
end;


define method emit-expression-leaf (jbb :: <java-basic-block>, node :: <symbol>) => ()
  // should really do this from the library
  //  let  spec = get-symbol-slot-in-class (jbb.meth.java-class, as (<string>, node));
  let  spec = get-symbol-slot-in-class (*current-module-java-class*, as (<string>, node).as-lowercase);
  java-read (jbb, spec)
end;


define constant $slot-empty-list$ =
  slot-spec ($dylan-class-<empty-list>$, "the_empty_list", $dylan-class-<empty-list>$, #t);

// the emulator has #() as a <symbol>, so beware
define method emit-expression-leaf (jbb :: <java-basic-block>, node == #()) => ()
  java-read (jbb, $slot-empty-list$)
end;

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <object>, nodes, value?) => ()
  format-out ("WHOOPS, emit-expression-tree called with %s of type %s\n", node, node.object-class);
  break();
  if (value?)
    emit-expression-leaf (jbb, #t)
  end
end;

define class <finally-handler> (<java-handler>)
  sealed slot  finally-handler-start :: <integer> = 10000;
  sealed slot  finally-handler-retpc :: <integer> = 255;
  sealed slot  finally-handler-ranges :: <list> = #();
  sealed slot  finally-handler-been-appended? :: <boolean> = #f;
end;

/* not used?
define function make-finally-handler (jmeth :: <java-method>, es :: <entry-state>)
break ();
  let  h = make (<finally-handler>);  // gen-finally-handler
  jmeth.finally-handlers [es] := h;
  h
end;
*/

// duplicate // define thread variable *unwind-handlers* = #f;

// this actually occurs at the start of the cleanup block
define method emit-expression-tree (jbb :: <java-basic-block>, node :: <unwind-protect>, nodes, value?) => ()
//  let  jmeth = jbb.meth;
  let  handlur = *unwind-handlers* [node];
  *jmc*.method-context-used-handlers := pair (handlur, *jmc*.method-context-used-handlers);
  handlur.handler-code-gen := gen-finally-handler;
  handlur.finally-handler-start := jbb.pc;
  handlur.finally-handler-retpc := get-temp-local-var ();
  if (jbb.initial-stack-depth &
      (jbb.initial-stack-depth ~= 1))
    format-out ("cleanup handler has wrong initial-stack-depth %s\n", jbb.initial-stack-depth);
    error ("cleanup handler has wrong initial-stack-depth")
  end;
  emit-pop-local (jbb, handlur.finally-handler-retpc, j-ref-code)
end;



define function emit-non-local-exit-unwind (jbb :: <java-basic-block>, uenv :: <list>, levels :: <integer>) => ()
  for (i :: <integer> from 0 below levels,
       unwind in uenv)
    let  handlur = *unwind-handlers* [unwind];
    let  jbbpc = jbb.pc;  // capture this instructions address
    java-branch-op (jbb, j-jsr, method () handlur.finally-handler-start - jbbpc end);
    if (*check-stack-types*)
      maintain-stack-types (jbb, $dummy-java-frag$, $one-retaddr$, #())
    else
      maintain-stack-depth (jbb, -1, j-jsr)  // adjust since ret-address popped by callee
    end
  end
end;

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <end-protected-block>, nodes, value?) => ()
  if (value?)
    format-out ("  WHOOPs, end-protected-block in value context?\n");
    break ()
  end;
  let  (depth, uenv) = get-uenv-level (node);
  emit-non-local-exit-unwind (jbb, uenv, 1)
end;


define method emit-expression-tree (jbb :: <java-basic-block>, node :: <end-cleanup-block>, nodes, value?) => ()
  if (value?)
    format-out ("  WHOOPs, end-cleanup-block in value context?\n");
    break ()
  end;
  let  unwind  = node.entry-state.me-block;
  emit-ret (jbb, *unwind-handlers* [unwind] . finally-handler-retpc)
end;

define function gen-finally-handler (jbb :: <java-basic-block>, handlur :: <finally-handler>) => ()
  let  tmp = get-temp-local-var ();
//  let  jmeth = jbb.meth;
  emit-pop-local (jbb, tmp, j-ref-code);  // j-code-for ($java/lang/Throwable$)
  java-branch-op (jbb, j-jsr, branch-relative (handlur.finally-handler-start));
  if (*check-stack-types*)
    maintain-stack-types (jbb, $dummy-java-frag$, $one-retaddr$, #())
  else
    maintain-stack-depth (jbb, -1, j-jsr)  // callee will have popped stack upon return
  end;
  emit-push-local (jbb, tmp, j-ref-code);  // j-code-for ($java/lang/Throwable$)
  java-simple-op (jbb, j-athrow)
end;




// most general method for emiting an expression:
//define method emit-expression-leaf (jbb :: <java-basic-block>, obj) => ()
//  emit-java-constant-load (jbb, java-rep (obj))
//end;
// now superseded by static modeling version

// most general for a model
define method emit-expression-leaf (jbb :: <java-basic-block>, model :: <&object>) => ()
  emit-model-reference (jbb, model)
end;



define function emit-model-reference (jbb :: <java-basic-block>, model :: <object>) => ()
  let  cached = find-cached-model (model);
  if (instance? (cached, <java-slot-spec>))
    java-read (jbb, cached)
  elseif (instance? (cached, <java-method-spec>))
    java-call (jbb, cached)
  else
    error ("unknown model cache spec %s %s", cached.object-class, cached)
  end
end;


// two special cases of raw things
define method emit-expression-leaf (jbb :: <java-basic-block>, obj :: <&raw-machine-word>) => ()
  emit-java-int (jbb, obj.^raw-object-value)
end;
define method emit-expression-leaf (jbb :: <java-basic-block>, obj :: <&raw-boolean>) => ()
  emit-java-int (jbb, if (obj.^raw-object-value) 1 else 0 end)
end;


define function emit-java-float (jbb :: <java-basic-block>, val :: <single-float>) => ()
  if (val = 0.0)
    java-simple-op (jbb, j-fconst-0)
  elseif (val = 1.0)
    java-simple-op (jbb, j-fconst-1)
  elseif (val = 2.0)
    java-simple-op (jbb, j-fconst-2)
  else
    let rep = make (<java-float-constant>, value: val);
    let ind = pool-index (rep, jbb.meth.java-class);
    emit-java-ldc (jbb, ind)
  end
end;

define method emit-expression-leaf (jbb :: <java-basic-block>, obj :: <&raw-single-float>) => ()
  let  val :: <single-float> = obj.^raw-object-value;
  emit-java-float (jbb, val)
end;

define method emit-expression-leaf (jbb :: <java-basic-block>, obj :: <&single-float>) => ()
  emit-expression-leaf (jbb, obj.^%single-float-data);
  emit-java-new-init-1 (jbb, $dylan-class-<single-float>$, $java-float-type$)
end;

define function emit-java-double (jbb :: <java-basic-block>, val :: <double-float>) => ()
  if (val = 0.0)
    java-simple-op (jbb, j-dconst-0)
  elseif (val = 1.0)
    java-simple-op (jbb, j-dconst-1)
  else
    let rep = make (<java-double-constant>, value: val);
    let ind = pool-index (rep, jbb.meth.java-class);
    emit-java-ldc (jbb, ind)
  end
end;

define method emit-expression-leaf (jbb :: <java-basic-block>, obj :: <&raw-double-float>) => ()
  let  val :: <double-float> = obj.^raw-object-value;
  emit-java-double (jbb, val)
end;

define method emit-expression-leaf (jbb :: <java-basic-block>, obj :: <&double-float>) => ()
  emit-expression-leaf (jbb, obj.^%double-float-data);
  emit-java-new-init-doub (jbb, $dylan-class-<double-float>$, $java-double-type$)
end;



define constant $convert-to-dylan-byte-string$ =
  meth-spec ($dylan-class-<byte-string>$, "intern_string",
             meth-type ($dylan-class-<byte-string>$, $java/lang/String$),
             j-invokestatic);

define method emit-expression-leaf (jbb :: <java-basic-block>, obj :: <byte-string>) => ()
  emit-raw-expression-leaf (jbb, obj);
  java-call (jbb, $convert-to-dylan-byte-string$)
end;


define method emit-expression-leaf (jbb :: <java-basic-block>, vec :: <simple-object-vector>, #next next-method) => ()
  emit-model-reference (jbb, vec)
/*
  // this should probably be done statically, in the class init.
  let  len = vec.size;
  // make a vector
  emit-raw-expression-leaf (jbb, len);
  java-op2 (jbb, j-anewarray, $dylan-class-<object>$);
  for (el in vec, n :: <integer> from 0)
    // push the array
    emit-dup (jbb);
    // push the index
    emit-raw-expression-leaf (jbb, n);
    // push the element - is this general enough?
    emit-expression-leaf (jbb, el);
    java-simple-op (jbb, j-aastore)
  end;
  emit-java-new-init-1 (jbb, $dylan-class-<simple-object-vector>$, $dylan-class-<object>-array$)
*/
end;

// not sure this copy needed - lists?
define method emit-expression-leaf (jbb :: <java-basic-block>, obj) => ()
  emit-model-reference (jbb, obj)
end;

define thread variable *current-method* = #f;
// duplicate // define thread variable *closure-env-lookup* = #f;


define method emit-expression-tree (jbb :: <java-basic-block>, node :: <make-closure>, nodes, value?) => ()
  let  mef = node.computation-closure-method;
  maybe-queue-method (mef);
  let  model = element (*closure-env-lookup*, mef.environment, default: #f);
  if (~model)
    error ("can't find environment model for %s", mef.environment)
  else
    let  mef-class = java-class-for-thing (mef);
    let  current-model = *closure-env-lookup*[*current-method*.environment];
    let  all-the-envs = reverse (current-model.prev-envs);
    let  next-all-the-envs = reverse (model.prev-envs);
    let  maker = meth-spec (mef-class,
                            "Jmake_closure_method",
                            apply (meth-type,
                                   java-class (current-model), // was $java-void-type$,
                                   map (java-class, next-all-the-envs)
                                   ),
                            j-invokevirtual);
    emit-java-new-init-0 (jbb, mef-class);
    emit-dup (jbb);
    let  method-class = *current-method*.java-class-for-thing;
    for (env in all-the-envs)
      emit-push-this (jbb);
      java-read (jbb, get-env-slot-spec (env, method-class))  // was jbb.meth.java-class
    end;
    if (*the-env-temp*)
      emit-push-local (jbb, *the-env-temp*, j-ref-code) // should use J-CODE-FOR
    end;
    java-call (jbb, maker);
    let  initter = node.computation-init-closure;
    if (~initter /* & at least one slot */)
      initialize-closure-slots (model, jbb, #t)
    end
  end
end;

define function initialize-closure-slots (model :: <java-env-model>, jbb :: <java-basic-block>, keep-on-stack? :: <boolean>)
  // for each closed-over variable involved
  //   if (not the last one)
  //     emit-dup (jbb)
  //   end;
  //   emit-expression-leaf (the initial value)
  // end;
  if (model.var-slots)
    for (slot in model.var-slots)
      emit-dup (jbb);
      emit-expression-leaf (jbb, #f);  // don't know where to set it from!
      java-write (jbb, slot.get-slot-spec)
    end
  end;
  unless (keep-on-stack?)
    emit-pop (jbb)
  end
end;

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <initialize-closure>, nodes, value?) => ()
  // must initialize the closure in the register, but make-closure does this?
  let  mef = node.computation-closure-method;
  let  meth = node.computation-closure;
  maybe-queue-method (mef);
  let  model = element (*closure-env-lookup*, mef.environment, default: #f);
  if (~model)
    error ("can't find closure model for environment %s", mef.environment)
  else
//    let  mef-class = java-class-for-thing (mef);
//  let  current-model = *closure-env-lookup*[*current-method*.environment];
    // for each closed over variable that needs initialization...
    // push the closure instance
    // push the value
    // set its relevant java-slot
    emit-expression-leaf (jbb, meth);
    initialize-closure-slots (model, jbb, #f)
  end
end;

define method emit-expression-leaf (jbb :: <java-basic-block>, mef :: <&method>) => ()
  maybe-queue-method (mef);
//  if (mef.environment.outer.find-lambda-env == ?current-env?)
/*
  let  model = element (*closure-env-lookup*, mef.environment, default: #f);
  if (model)
    let  mef-class = java-class-for-thing (mef);
    let  current-model = *closure-env-lookup*[*current-method*.environment];
      format-out ("PANG! current-model is %s (%s), env %s\n", current-model, current-model.object-class, *current-method*.environment);
    if (current-model == #t)
      next-method()
    else
      let  slot =
        block (return)
          for (slot in current-model.var-slots)
            if (mef-class == slot.type)
              return (slot)
            end
          end;
          break ()
        end;
      let  cls = slot.slot-type;
      let  nam = slot.slot-name;
      emit-push-this (jbb);
      java-call (jbb, meth-spec (jbb.meth.java-class, nam, meth-type (cls), j-invokevirtual));
      //    emit-java-constant-load (jbb, java-rep (model.java-class))  // should use slot!
      //    next-method()
    end
  else
*/    next-method()
//  end
end;




// new and <init>, for 0, 1, 2 pre-pushed arguments:
define function emit-java-new-init-0 (jbb :: <java-basic-block>, jc :: <java-class>) => ()
  java-op2 (jbb, j-new, jc);
  emit-dup (jbb);
  emit-java-init (jbb, jc)
end;

define function emit-java-new-init-1 (jbb :: <java-basic-block>, jc :: <java-class>, type1 :: <java-type>) => ()
  java-op2 (jbb, j-new, jc);
  java-simple-op (jbb, j-dup-x1);
  emit-swap (jbb);
  emit-java-init (jbb, jc, type1)
end;

define function emit-java-new-init-doub (jbb :: <java-basic-block>, jc :: <java-class>, type1 :: <java-type>) => ()
  java-op2 (jbb, j-new, jc);
  java-simple-op (jbb, j-dup-x2);
  java-simple-op (jbb, j-dup-x2);
  emit-pop (jbb);
  emit-java-init (jbb, jc, type1)
end;


// not actually used
define function emit-java-new-init-2 (jbb :: <java-basic-block>, jc :: <java-class>, type1 :: <java-type>, type2 :: <java-type>) => ()
  java-op2 (jbb, j-new, jc);
  java-simple-op (jbb, j-dup-x2);
  java-simple-op (jbb, j-dup-x2);
  emit-pop (jbb);
  emit-java-init (jbb, jc, type1, type2)
end;

define function emit-java-init (jbb :: <java-basic-block>, jc :: <java-class>, #rest types :: <java-type>) => ()
  java-call (jbb, meth-spec (jc,
                             $java-init-methname$,
                             apply (meth-type, $java-void-type$, types),
                             j-invokespecial))
end;





define constant $dylan-self-slot-name$ = "Jself";

// methods and other stuff represented by a java class of a single instance
// can use the Jself slot (a static slot) to get the canonical instance
// (this lazily forces instantiation too, via <clinit>)
// Note this method is _not_ used for dylan classes, because they
// want the (dylan) class, not an instance
define sideways method emit-java-constant-load (jbb :: <java-basic-block>, const :: <java-class>) => ()
  java-read (jbb, slot-spec (const, $dylan-self-slot-name$, const, #t))
end;



define method emit-expression-leaf (jbb :: <java-basic-block>, obj :: <object-reference>) => ()
  emit-expression-leaf (jbb, obj.reference-value)
end;

define method emit-expression-leaf (jbb :: <java-basic-block>, node :: <method-reference>) => ()
  // add method to set to be code-genned
  emit-expression-leaf (jbb, node.reference-value)
end;

define method emit-expression-leaf (jbb :: <java-basic-block>, val :: <mapped-unbound>) => ()
  emit-java-null (jbb)
end;


define method emit-expression (jbb :: <java-basic-block>, node :: <object-reference>, nodes, value?) => ()
  if (value?)
    emit-expression-leaf (jbb, node)
  end
end;

define method emit-expression (jbb :: <java-basic-block>, node :: <method-reference>, nodes, value?) => ()
  if (value?)
    emit-expression-leaf (jbb, node)
  end
end;



define method emit-expression-tree (jbb :: <java-basic-block>, node :: <variable-reference>, nodes, value?) => ()
  if (value?)
    let bnd = node.referenced-binding;
    // this needs to deal with real variables (instance variables, closed-over variables)
    let spec = java-rep (bnd);
    unless (spec.static?)
      error ("bad read of <variable-reference> as non-static field")
    end;
    java-read (jbb, spec)
  end
end;

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <set!>, nodes, value?) => ()
  emit-expression (jbb, node.computation-value, nodes, 1);
  let bnd = node.assigned-binding;
  // this needs to deal with real variables (instance variables, closed-over variables)
  if (value?)
    emit-dup (jbb)
  end;
  let  (env, slot) = maybe-find-in-env (bnd);
  if (env)
    emit-env-write (jbb, env, slot)
  else
    java-write (jbb, bnd.java-rep)
  end
end;


// is this right - are multiple-value bindings allowed here?
define method emit-expression-tree (jbb :: <java-basic-block>, node :: <definition>, nodes, value?) => ()
  emit-expression (jbb, node.computation-value, nodes, 1);
  let bnd = node.assigned-binding;
  // this needs to deal with real variables (instance variables, closed-over variables)
  if (value?)
    emit-dup (jbb)
  end;
  java-write (jbb, bnd.java-rep)
end;


define constant $find-dylanthread-meth$ =
  meth-spec ($java/lang/Thread$, "currentThread", meth-type ($java/lang/Thread$), j-invokestatic);

/*
// is this used now?
define function  push-the-thread (jbb) => ()
  java-call (jbb, $find-dylanthread-meth$);
  // this shouldn't sproing, it's purpose is to persuade the verifier
  // that dylan runs on a dylanthread
  java-op2 (jbb, j-checkcast, $dylan/dylanthread$)
end;
*/

define constant $dylanthread-class-constant$ =
  make (<java-class-constant>, java-class: $dylan/dylanthread$);

define constant $restore-mv-method$ =
  meth-spec ($dylan/dylanthread$, "restore_mv",
             meth-type ($dylan-class-<object>$, $dylan-class-<object>-array$),
             j-invokestatic);

define constant $save-mv-method$ =
  meth-spec ($dylan/dylanthread$, "save_mv",
             meth-type ($dylan-class-<object>$, $dylan-class-<object>-array$),
             j-invokestatic);


define constant $values-methods$ =
  vector (meth-spec ($dylan/dylanthread$, "mv_count_0", meth-type ($java-void-type$), j-invokestatic),
          meth-spec ($dylan/dylanthread$, "mv_count_1", meth-type ($java-void-type$), j-invokestatic),
          meth-spec ($dylan/dylanthread$, "values", meth-type ($dylan-class-<object>$,
                                                                 $dylan-class-<object>$,
                                                               $dylan-class-<object>$), j-invokestatic),
          meth-spec ($dylan/dylanthread$, "values", meth-type ($dylan-class-<object>$,
                                                               $dylan-class-<object>$,
                                                               $dylan-class-<object>$,
                                                               $dylan-class-<object>$), j-invokestatic),
          meth-spec ($dylan/dylanthread$, "values", meth-type ($dylan-class-<object>$,
                                                               $dylan-class-<object>$,
                                                               $dylan-class-<object>$,
                                                               $dylan-class-<object>$,
                                                               $dylan-class-<object>$), j-invokestatic));


define constant $next-methods-slot$ =
  slot-spec ($dylan/dylanthread$,
             "next_methods",
             $dylan-class-<list>$,
             #f);
define constant $next-methods-getter$ =
  meth-spec ($dylan/dylanthread$,
             "next_methods",
             meth-type ($dylan-class-<list>$),
             j-invokestatic);
define constant $next-methods-setter$ =
  meth-spec ($dylan/dylanthread$,
             "next_methods_setter",
             meth-type ($java-void-type$, $dylan-class-<list>$),
             j-invokestatic);
define constant $next-methods-popper$ =
  meth-spec ($dylan/dylanthread$,
             "pop_next_method",
             meth-type ($dylan-class-<function>$),
             j-invokestatic);


define method emit-expression-tree (jbb :: <java-basic-block>, node :: <values>, nodes, value?) => ()
  let  fixed = node.fixed-values;
  let  rest  = node.rest-value;
  let  num-fixed = fixed.size;

  if (value? | node.temporary.frame-offset)
    if (value? == 1)
      if (zero? (num-fixed))
        emit-expression-leaf (jbb, #f)
      else
        emit-expression (jbb, fixed[0], nodes, 1)
      end;
      for (n :: <integer> from 1 below num-fixed)
        emit-expression (jbb, fixed[n], nodes, #f)
      end
      // don't bother to set if context knows single-valued
    else
      let  use-array = num-fixed > 4;
      if (use-array)
        emit-raw-expression-leaf (jbb, num-fixed);
        java-op2 (jbb, j-anewarray, $dylan-class-<object>$)
      end;
      for (n :: <integer> from 0 below num-fixed)
        if (use-array)
          emit-dup (jbb);
          emit-raw-expression-leaf (jbb, n);
          emit-expression (jbb, fixed[n], nodes, 1);
          java-simple-op (jbb, j-aastore)
        else
          emit-expression (jbb, fixed[n], nodes, 1);
        end
      end;
      // now got values, process them
      if (use-array)
        // take array, set multiple values, return value 0
        java-call (jbb, $restore-mv-method$)
      else
        // small fixed cases
        java-call (jbb, $values-methods$[num-fixed])
      end;
      // finally fake a val0 if none at all
      if (num-fixed == 0)
        emit-expression-leaf (jbb, #f)
      end
    end;
    // add any rest to the end
    if (rest)
      emit-expression (jbb, rest, nodes, 1);
      java-call (jbb, meth-spec ($dylan/dylanthread$,
                                 "add_rest_values",
                                 meth-type ($dylan-class-<object>$,
                                            $dylan-class-<object>$,
                                            array-type ($dylan-class-<object>$)),
                                 j-invokestatic))
    end;
    if ((~value?) & node.temporary.frame-offset)
      emit-pop-local (jbb, node.temporary.number-local-var, j-ref-code) // J-CODE-FOR ?
    end

  else
    // only for side effects....
    for (tt in fixed)
      emit-expression (jbb, tt, nodes, #f)
    end;
    if (rest)
      emit-expression (jbb, rest, nodes, #f)
    end
  end
end;


define constant $value-extract-method$ =
  meth-spec ($dylan/dylanthread$,
             "value",
             meth-type ($dylan-class-<object>$, $java-int-type$),
             j-invokestatic);


define method emit-expression-tree (jbb :: <java-basic-block>, node :: <extract-single-value>, nodes, value?) => ()
  if (value?)
    emit-expression (jbb, node.computation-value, nodes, #t);
    let  n = node.index;
    if (n > 0)
      let  jc = jbb.meth.java-class;
      emit-pop (jbb);
      emit-raw-expression-leaf (jbb, n);
      java-call (jbb, $value-extract-method$)
    end
  end
end;

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <extract-rest-value>, nodes, value?) => ()
  if (value?)
    emit-expression (jbb, node.computation-value, nodes, #t);
    // ??
    format-out ("  magic-extract-rest-value %d\n", node.index);
    break ()
  end
end;

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <temporary-transfer>, nodes, value?) => ()
  emit-expression (jbb, node.computation-value, nodes, value?);
  // effectively a nop on the stack, unless at top level in expression
  if (~value?)
    emit-pop-local (jbb, node.temporary.number-local-var, j-ref-code) // J-CODE-FOR
  end
end;

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <faked-transfer>, nodes, value?) => ()
  emit-expression (jbb, node.computation-value, nodes, value?);
  // effectively a nop on the stack, unless at top level in expression
  if (~value?)
    emit-pop-local (jbb, node.temporary.number-local-var, j-ref-code) // J-CODE-FOR
  end
end;

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <keyword-default>, nodes, value?) => ()
  emit-expression (jbb, node.computation-value, nodes, value?);
  // effectively a nop on the stack, unless at top level in expression
  if (~value?)
    emit-pop-local (jbb, node.temporary.number-local-var, j-ref-code) // J-CODE-FOR
  end
end;


define method emit-expression-tree (jbb :: <java-basic-block>, node :: <multiple-value-spill>, nodes, value?) => ()
  emit-expression (jbb, node.computation-value, nodes, #t);
  java-call (jbb, $save-mv-method$);
  if (~value?)
    emit-pop-local (jbb, node.temporary.number-local-var, j-ref-code) // J-CODE-FOR
  end
end;


define method emit-expression-tree (jbb :: <java-basic-block>, node :: <multiple-value-unspill>, nodes, value?) => ()
  emit-expression (jbb, node.computation-value, nodes, 1);  // I think
  java-call (jbb, $restore-mv-method$);
  if (~value?)
    format-out ("funny: unspilt MV's immediately assigned\n");
    emit-pop-local (jbb, node.temporary.number-local-var, j-ref-code) // J-CODE-FOR
  end
end;




define function check-first-value-type (jbb :: <java-basic-block>, oref) => ()
  let tipe = oref;
  if (instance? (tipe, <object-reference>))
    tipe := tipe.reference-value
  end;
  if (instance? (tipe, <&class>))
//    let  excep-class = java-class-for-thing (tipe);
    let  handlur = find-simple-java-handler (*jmc*, $java/lang/ClassCastException$);
    let  scope-id = enter-handler-scope (handlur, jbb);
//    java-op2 (jbb, j-checkcast, excep-class);
    check-a-type (jbb, 0, tipe);
    exit-handler-scope (handlur, jbb, scope-id)
  else
    format-out ("  omitted check-type for non-class %s\n", tipe)
  end
end;

define function check-values-type (jbb :: <java-basic-block>, vec, rest-type) => ()
  let  jc  = jbb.meth.java-class;
  let  count = vec.size;
  let  handlur = find-simple-java-handler (*jmc*, $java/lang/ClassCastException$);
  let  scope-id = enter-handler-scope (handlur, jbb);
  for (tmp in vec, n :: <integer> from 0)
    let tipe = tmp;
    if (instance? (tipe, <object-reference>))
      tipe := tipe.reference-value
    end;
    check-a-type (jbb, n, if (n < count) tipe else rest-type end);
  end;
  exit-handler-scope (handlur, jbb, scope-id)
end;


define sealed generic check-a-type (jbb :: <java-basic-block>, mv :: <integer>, tipe) => ();

define method check-a-type (jbb :: <java-basic-block>, mv :: <integer>, tipe) => ()
  format-out ("WHOOPS  omitted a check-type %d for non-class %s (%s)\n",
              mv, tipe, tipe.object-class)
end;

// naughty but nice to use == on an <integer>
define method check-a-type (jbb :: <java-basic-block>, mv == 0, tipe :: <&class>) => ()
  java-op2 (jbb, j-checkcast, java-class-for-thing (tipe))
end;

define method check-a-type (jbb :: <java-basic-block>, mv :: <integer>, tipe :: <&class>) => ()
  emit-raw-expression-leaf (jbb, mv);
  java-call (jbb, $value-extract-method$);
  java-op2 (jbb, j-checkcast, java-class-for-thing (tipe));
  emit-pop (jbb)
end;

// for some perverse reason #f means <object>
define method check-a-type (jbb :: <java-basic-block>, mv :: <integer>, tipe == #f) => ()
  format-out ("#$#$#$ #f meaning <object> spotted in check-a-type\n");
end;


// note sure if should check type if value not used?
define method emit-expression-tree (jbb :: <java-basic-block>, node :: <multiple-value-check-type>, nodes, value?) => ()
  emit-expression (jbb, node.computation-value, nodes, value?);
  if (value? == #t)
    check-values-type (jbb, node.types, #f)
  else
    check-first-value-type (jbb, node.types[0])
  end;
  if (~value?)
    emit-pop-local (jbb, node.temporary.number-local-var, j-ref-code) // J-CODE-FOR
  end
end;


// very unsure here
define method emit-expression-tree (jbb :: <java-basic-block>, node :: <multiple-value-check-type-rest>, nodes, value?) => ()
  emit-expression (jbb, node.computation-value, nodes, value?);
  if (value? == #t)
    check-values-type (jbb, node.types, node.rest-type)
  else
    check-first-value-type (jbb, node.types[0])
  end;
  if (~value?)
    emit-pop-local (jbb, node.temporary.number-local-var, j-ref-code) // J-CODE-FOR
  end
end;



define method emit-expression-tree (jbb :: <java-basic-block>, node :: <check-type>, nodes, value?) => ()
  emit-expression (jbb, node.computation-value, nodes, value?);
  // ??
  check-first-value-type (jbb, node.type);
  if (~value?)
    emit-pop-local (jbb, node.temporary.number-local-var, j-ref-code) // J-CODE-FOR
  end
end;

// constrain type merely marks an addition type constraint for
// a branch of a conditional.
// For Java this means we should add explicit cast if source
// and dest temp have different types.
define method emit-expression-tree (jbb :: <java-basic-block>, node :: <constrain-type>, nodes, value?) => ()
  emit-expression (jbb, node.computation-value, nodes, value?);
  if (type-estimate (node.computation-value) ~== type-estimate (node.temporary))
    format-out ("$$$ real constraining of types from %s to %s for %s\n",
                type-estimate (node.computation-value),
                type-estimate (node.temporary),
                node);
    check-first-value-type (jbb, node.type)
  end;
  if (~value?)
    emit-pop-local (jbb, node.temporary.number-local-var, j-ref-code) // J-CODE-FOR
  end
end;


define method emit-expression-tree (jbb :: <java-basic-block>, node :: <if>, nodes, value?) => ()
  if (value?)
    format-out ("funny: <if> evaluated in value position\n")
  end;
  // context of test is single-valued, but arms should inherit from our value?
  let  dbb = *unwind-handlers* [node];  // really gets dylan bb
  let  successor-bbs = dbb.succs;
  if (successor-bbs.size ~= 2)
    error ("<if> basic block with wrong number of successors")
  end;
  emit-obj-if-expression (jbb, node.test, nodes, successor-bbs.second, successor-bbs.first, next-bb (dbb))
end;


define method emit-expression-tree (jbb :: <java-basic-block>, node :: <stack-vector>, nodes, value?) => ()
  let  temps = make (<stretchy-vector>);
  do-used-value-references
    (method (tt)
       temps := add! (temps, tt)
     end,
     node);
//  let  temps = node.used-temporaries;
  let  args  = temps.size;
  // make array
  emit-raw-expression-leaf (jbb, args);
  java-op2 (jbb, j-anewarray, $dylan-class-<object>$);
  for (n :: <integer> from 0 below args)
    emit-dup (jbb);
    emit-raw-expression-leaf (jbb, n);
    emit-expression (jbb, temps[n], nodes, #t);
    java-simple-op (jbb, j-aastore)
  end;
// keep as Java array, I think this is private to the calling convention.

// but for now, dont!
  emit-java-new-init-1 (jbb, $dylan-class-<simple-object-vector>$, $dylan-class-<object>-array$)
end;


define constant $max-java-args$ :: <integer> = 4;


// this is BLAH - I need to sort out what the function thing can
// really be (modelled iep/method/gf , or something less
//            definite like binding or temporary?)
define sealed generic instance-for-calling (thing) => (obj);
define method instance-for-calling (thing) => (obj)
  format-out ("UNSURE if can call %s: %s\n", thing.object-class, thing);
  thing
end;
define method instance-for-calling (ping :: <object-reference>) => (obj)
  ping
end;
define method instance-for-calling (ping == #f) => (obj)
  format-out ("UNSURE if can call #f!\n");
  ping
end;
define method instance-for-calling (gf :: <&generic-function>) => (obj)
  gf
end;
define method instance-for-calling (tmp :: <temporary>) => (obj)
  tmp
end;
define method instance-for-calling (meth :: <&method>) => (obj)
  meth
end;
define method instance-for-calling (iep :: <&iep>) => (obj)
  iep.function
end;
define method instance-for-calling (mep :: <&mep>) => (obj)
error ("instance-for-calling on <&mep> - what?");
  mep.function
end;
define method instance-for-calling (xep :: <&xep>) => (obj)
error ("instance-for-calling on <&xep> - what?");
  xep.function
end;

define function emit-normal-call-prolog (jbb :: <java-basic-block>, args, funct, nodes) => ()
  emit-expression (jbb, funct.instance-for-calling, nodes, 1);
  if (args >= $max-java-args$)
format-out ("## extra long call\n");
    // generate the arg count if 4 or more
    emit-raw-expression-leaf (jbb, args)
  end
end;

define function pop-into-array (jbb :: <java-basic-block>, size :: <integer>) => ()
  emit-raw-expression-leaf (jbb, size);
  java-op2 (jbb, j-anewarray, $dylan-class-<object>$);
  for (n :: <integer> from 1 to size)
    java-simple-op (jbb, j-dup-x1);
    emit-swap (jbb);
    emit-raw-expression-leaf (jbb, size - n);
    emit-swap (jbb);
    java-simple-op (jbb, j-aastore)
  end
end;


define function emit-normal-call-itself (jbb :: <java-basic-block>, args, funct, call) => ()
  java-call (jbb, dylan-invoke-method (call, funct, args))
end;

define function emit-normal-call (jbb :: <java-basic-block>, args, funct, call) => ()
  if (args >= $max-java-args$)
    if (args == $max-java-args$)
      emit-java-null (jbb)
    else
      pop-into-array (jbb, args - $max-java-args$)
    end
  end;
  emit-normal-call-itself (jbb, args, funct, call)
end;


define sealed generic emit-call (jbb :: <java-basic-block>, node :: <function-call>, function, arguments, nodes, value?) => ();


define method emit-expression-tree (jbb :: <java-basic-block>, node :: <function-call>, nodes, value?) => ()
  let effective-function = call-effective-function (node);
  emit-call (jbb, node, effective-function, node.arguments, nodes, value?)
end;
// temporary copy until stable
define method emit-expression-tree (jbb :: <java-basic-block>, node :: <simple-call>, nodes, value?) => ()
  let effective-function = call-effective-function (node);
  emit-call (jbb, node, effective-function, node.arguments, nodes, value?)
end;
define method emit-expression-tree (jbb :: <java-basic-block>, node :: <method-call>, nodes, value?) => ()
  let effective-function = call-effective-function (node);
  emit-call (jbb, node, effective-function, node.arguments, nodes, value?)
end;

define method emit-call (jbb :: <java-basic-block>, node :: <simple-call>, funct, arguments, nodes, value?) => ()
  let  arg-count = arguments.size;
  emit-normal-call-prolog (jbb, arg-count, node.function, nodes);

  for (n :: <integer> from 0 below arg-count)
    if (n >= $max-java-args$)
      if (n = $max-java-args$)
        emit-raw-expression-leaf (jbb, arg-count - $max-java-args$);
        java-op2 (jbb, j-anewarray, $dylan-class-<object>$)
      end;
      emit-dup (jbb);
      emit-raw-expression-leaf (jbb, n - $max-java-args$)
    end;

    emit-expression (jbb, arguments[n], nodes, 1);
    if (n >= $max-java-args$)
      java-simple-op (jbb, j-aastore)
    end
  end;
  if (arg-count == $max-java-args$)
    emit-java-null (jbb)
  end;

  emit-normal-call-itself (jbb, arg-count, funct, node);

  if (~value?)
    emit-pop (jbb)
  end
  // maybe clean up the mv area?
end;


define method emit-call (jbb :: <java-basic-block>, node :: <method-call>, function, arguments, nodes, value?, #next simple-call-method) => ()
  if (node.next-methods)
    emit-expression (jbb, node.next-methods, nodes, 1);
    java-call (jbb, $next-methods-setter$)
  end;
  simple-call-method ()
end;


define constant $dylan/applymethod$ =
  make (<java-stub-class>, class-name: "applymethod", package: java-package ("dylan"), super: $dylan-class-<method>$);

define constant $the-apply-method$ =
  slot-spec ($dylan/applymethod$, $dylan-self-slot-name$, $dylan/applymethod$, #t);

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <apply>, nodes, value?) => ()
  let  effective-function = call-effective-function (node);
  emit-call (jbb, node, effective-function, node.arguments, nodes, value?)
end;

define method emit-call (jbb :: <java-basic-block>, node :: <apply>, function, arguments, nodes, value?) => ()
  let  temps = make (<stretchy-vector>);
  do-used-value-references
    (method (tt)
       temps := add! (temps, tt)
     end,
     node);
  let  funct-tmp = temps[0];
  let  funct = funct-tmp.generator;
  let  args  = temps.size;
  java-read (jbb, $the-apply-method$);
  emit-normal-call-prolog (jbb, args, funct-tmp, nodes); // was args-1
  for (n :: <integer> from 1 below args)
    emit-expression (jbb, temps[n], nodes, 1)
  end;
  emit-normal-call (jbb, args, function, node);  // extra arg: function to be applied

  if (~value?)
    emit-pop (jbb)
  end
end;

/* not used
define function emit-assignment (jbb :: <java-basic-block>, var, val) => ()
  emit-push-local (jbb, val.number-local-var, j-ref-code);  // J-CODE-FOR
  emit-pop-local (jbb, var.number-local-var, j-ref-code)     // J-CODE-FOR
end;
*/

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <loop-call>, nodes, value?) => ()
  if (value?)
    format-out ("suspicious use of <loop-call> in a value context\n")
  end;
  let  loop = node.loop-call-loop;
  let  dest-depth    = get-uenv-level (loop);
  let  (depth, uenv) = get-uenv-level (node);
  if (depth > dest-depth)
    emit-non-local-exit-unwind (jbb, uenv, depth - dest-depth)
  end
end;

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <end-loop>, nodes, value?) => ()
  if (value?)
    format-out ("suspicious use of <end-loop> in a value context\n")
  end;
  let  loop = node.ending-loop;
  let  dest-depth    = get-uenv-level (loop);
  let  (depth, uenv) = get-uenv-level (node);
  if (depth > dest-depth)
    emit-non-local-exit-unwind (jbb, uenv, depth - dest-depth)
  end
end;



define constant $dylan-unbound-slot-meth$ =
  meth-spec ($dylan-class-<class>$, "unbound_slot_error", meth-type ($java-void-type$), j-invokestatic);


define method emit-expression-tree (jbb :: <java-basic-block>, node :: <slot-value>, nodes, value?) => ()
  let  inst-temp = node.computation-instance;
  let  slot-desc = node.computation-slot-descriptor;
  if (value?)
    emit-expression (jbb, inst-temp, nodes, 1);
    dylan-read-model-slot (jbb, slot-desc, #f);
    unless (node.computation-guaranteed-initialized?)
      emit-dup (jbb);
      java-branch-op (jbb, j-ifnonnull, 6);
      java-call (jbb, $dylan-unbound-slot-meth$)
    end
  end
end;

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <slot-value-setter>, nodes, value?) => ()
  let  inst-temp = node.computation-instance;
  let  slot-desc = node.computation-slot-descriptor;
  emit-expression (jbb, inst-temp, nodes, 1);
  emit-expression (jbb, node.computation-new-value, nodes, 1);
  if (value?)
    java-simple-op (jbb, j-dup-x1)
  end;
  dylan-write-model-slot (jbb, slot-desc, #f)
end;


define method emit-expression-tree (jbb :: <java-basic-block>, node :: <repeated-slot-value>, nodes, value?) => ()
  let  inst-temp = node.computation-instance;
  let  slot-desc = node.computation-slot-descriptor;
  if (value?)
    emit-expression (jbb, inst-temp, nodes, 1);
    dylan-read-model-slot (jbb, slot-desc, #t);
    emit-expression (jbb, node.computation-index, nodes, 1);
    let  jclass = java-class-for-thing (slot-desc.^slot-type);
    java-simple-op (jbb, j-acode-for (jclass, #"load"));
    unless (node.computation-guaranteed-initialized?)
      if (j-code-for (jclass) = j-ref-code)
        emit-dup (jbb);
        java-branch-op (jbb, j-ifnonnull, 6);
        java-call (jbb, $dylan-unbound-slot-meth$)
      end
    end
  end
end;


define method emit-expression-tree (jbb :: <java-basic-block>, node :: <repeated-slot-value-setter>, nodes, value?) => ()
  let  inst-temp = node.computation-instance;
  let  slot-desc = node.computation-slot-descriptor;
  emit-expression (jbb, inst-temp, nodes, 1);
  dylan-read-model-slot (jbb, slot-desc, #t);
  emit-expression (jbb, node.computation-index, nodes, 1);
  emit-expression (jbb, node.computation-new-value, nodes, 1);
  if (value?)
    java-simple-op (jbb, j-dup-x2);
  end;
  java-simple-op (jbb, j-acode-for (java-class-for-thing (slot-desc.^slot-type), #"store"))
end;

// if handling, branching code generation

define function emit-smart-if-branch (jbb :: <java-basic-block>, true-bytecode, false-bytecode, true-dest, false-dest, fall-through)
  if (true-dest == false-dest)
    format-out ("WHOOPS, both branches of a conditional identical!!\n");
    if (true-dest ~== fall-through)
      java-branch-op (jbb, j-goto, true-dest)
    end
  else
    if (fall-through == true-dest)
      java-branch-op (jbb, false-bytecode, false-dest)
    else
      java-branch-op (jbb, true-bytecode, true-dest);
      if (fall-through ~== false-dest)
        java-branch-op (jbb, j-goto, false-dest)
      end
    end
  end
end;


define function emit-raw-if-expression (jbb :: <java-basic-block>, node :: <temporary>, nodes, true-dest, false-dest, fall-through) => ()
  let  gen = node.generator;
  if (member? (gen, nodes))
//    format-out ("emit-raw-if-expression recursing through %s\n", node);
    emit-raw-if-expression-tree (jbb, gen, nodes, true-dest, false-dest, fall-through)
  else
    emit-expression-leaf (jbb, node);
    emit-smart-if-branch (jbb, j-ifne, j-ifeq, true-dest, false-dest, fall-through)
  end
end;


define method emit-raw-if-expression-tree (jbb :: <java-basic-block>, node, nodes, true-dest, false-dest, fall-through)
//format-out ("emit-raw-if-expression-tree failed to be smart on %s\n", node);
  // default method generates the code and tests the resulting raw boolean
  emit-expression-tree (jbb, node, nodes, 1);
  emit-smart-if-branch (jbb, j-ifne, j-ifeq, true-dest, false-dest, fall-through)
end;

define method emit-obj-if-expression-tree (jbb :: <java-basic-block>, node, nodes, true-dest, false-dest, fall-through) => ()
  // default method generates the code and tests the resulting <boolean> for eq with #f
//format-out ("emit-obj-if-expression-tree failed to be smart on %s\n", node);
  emit-expression-tree (jbb, node, nodes, 1);
  let  m = jbb.meth;
  emit-expression-leaf (jbb, #f);
  emit-smart-if-branch (jbb, j-if-acmpne, j-if-acmpeq, true-dest, false-dest, fall-through)
end;

define function emit-obj-if-expression (jbb :: <java-basic-block>, node :: <temporary>, nodes, true-dest, false-dest, fall-through) => ()
  let  gen = node.generator;
  if (member? (gen, nodes))
//    format-out ("emit-obj-if-expression recursing through %s\n", node);
    emit-obj-if-expression-tree (jbb, gen, nodes, true-dest, false-dest, fall-through)
  else
    unless (instance? (node, <lexical-variable>))
//      format-out ("emit-obj-if-expression failed to recurse through %s\n", node);
      for (el in nodes)
        format-out ("  nodes include:  %s\n", el)
      end
    end;
    emit-expression-leaf (jbb, node);
    let  m = jbb.meth;
    emit-expression-leaf (jbb, #f);
    emit-smart-if-branch (jbb, j-if-acmpne, j-if-acmpeq, true-dest, false-dest, fall-through)
  end
end;

define method emit-raw-if-expression-tree (jbb :: <java-basic-block>, node :: <primitive-call>, nodes, true-dest, false-dest, fall-through) => ()
  // enable further dispatch on primitives
  let  prim = node.primitive;
  let  temps = make (<stretchy-vector>);
  do-used-value-references
    (method (tt)
       temps := add! (temps, tt)
     end,
     node);
  let  prim-name = as (<symbol>, prim.binding-name);
  gen-raw-if-primitive (prim-name, node, temps, nodes, jbb, true-dest, false-dest, fall-through)
end;

define method emit-obj-if-expression-tree (jbb :: <java-basic-block>, node :: <primitive-call>, nodes, true-dest, false-dest, fall-through) => ()
  // enable further dispatch on primitives
//format-out ("emit-obj-if-expression-tree on <primitive-call>\n");
  let  prim = node.primitive;
  let  temps = make (<stretchy-vector>);
  do-used-value-references
    (method (tt)
       temps := add! (temps, tt)
     end,
     node);
  let  prim-name = as (<symbol>, prim.binding-name);
  gen-obj-if-primitive (prim-name, node, temps, nodes, jbb, true-dest, false-dest, fall-through)
end;

define method emit-obj-if-expression-tree (jbb :: <java-basic-block>, node :: <temporary-transfer>, nodes, true-dest, false-dest, fall-through) => ()
  // enable further dispatch on primitives
//format-out ("emit-obj-if-expression-tree recursing through %s\n", node);
  emit-obj-if-expression (jbb, node.computation-value, nodes, true-dest, false-dest, fall-through)
end;
define method emit-obj-if-expression-tree (jbb :: <java-basic-block>, node :: <guarantee-type>, nodes, true-dest, false-dest, fall-through) => ()
  // enable further dispatch on primitives
//format-out ("emit-obj-if-expression-tree recursing through %s\n", node);
  emit-obj-if-expression (jbb, node.computation-value, nodes, true-dest, false-dest, fall-through)
end;
define method emit-obj-if-expression-tree (jbb :: <java-basic-block>, node :: <merge-transfer>, nodes, true-dest, false-dest, fall-through) => ()
  // enable further dispatch on primitives
//format-out ("emit-obj-if-expression-tree recursing through %s\n", node);
  emit-obj-if-expression (jbb, node.computation-value, nodes, true-dest, false-dest, fall-through)
end;

define method emit-raw-if-expression-tree (jbb :: <java-basic-block>, node :: <temporary-transfer>, nodes, true-dest, false-dest, fall-through) => ()
  // enable further dispatch on primitives
//format-out ("emit-raw-if-expression-tree recursing through %s\n", node);
  emit-raw-if-expression (jbb, node.computation-value, nodes, true-dest, false-dest, fall-through)
end;
define method emit-raw-if-expression-tree (jbb :: <java-basic-block>, node :: <guarantee-type>, nodes, true-dest, false-dest, fall-through) => ()
  // enable further dispatch on primitives
//format-out ("emit-raw-if-expression-tree recursing through %s\n", node);
  emit-raw-if-expression (jbb, node.computation-value, nodes, true-dest, false-dest, fall-through)
end;
define method emit-raw-if-expression-tree (jbb :: <java-basic-block>, node :: <merge-transfer>, nodes, true-dest, false-dest, fall-through) => ()
  // enable further dispatch on primitives
//format-out ("emit-raw-if-expression-tree recursing through %s\n", node);
  emit-raw-if-expression (jbb, node.computation-value, nodes, true-dest, false-dest, fall-through)
end;

define thread variable *expression-tree-nodes* = #f;

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <primitive-call>, nodes, value?) => ()
  let  prim = node.primitive;
  let  temps = make (<stretchy-vector>);
  do-used-value-references
    (method (tt)
       temps := add! (temps, tt)
     end,
     node);
  let  args  = temps.size;
  for (tt in temps, n :: <integer> from 0)
    emit-expression (jbb, tt, nodes, 1)  // assume all args to primitives single-valued?
  end;
  let  desc = prim.primitive-descriptor-getter;
  let  prim-name = as (<symbol>, prim.binding-name);
  dynamic-bind (*expression-tree-nodes* = nodes)
    gen-primitive (prim-name, node, args, jbb);
    if (~value?)
      emit-pop (jbb)
    end
  end dynamic-bind
end;

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <return>, nodes, value?) => ()
  if (value?)
    format-out ("funny: return evaluated in value position\n")
  end;
  if (*emit-returns*)
    emit-expression (jbb, node.computation-value, nodes, #t);
    let  (depth, uenv) = get-uenv-level (node);
    if (depth > 0)
      emit-non-local-exit-unwind (jbb, uenv, depth)
    end;
    emit-return (jbb, j-ref-code)
  end
end;

// duplicate // define thread variable *uenv* = #();

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <exit>, nodes, value?) => ()
  format-out ("###### seen <exit> in emit-expression-tree, phew!\n");
  emit-expression (jbb, node.computation-value, nodes, #f); // this can't be right
  let  home-es = node.entry-state;
  let  home    = home-es.me-block;
  let  dest-lambda = home.environment.lambda;
  let  src-lambda  = node.environment.lambda;
  if (dest-lambda ~== src-lambda)
    error ("real non-local <exit>")
  else
    format-out ("point 1\n");
    let  dest-depth    = get-uenv-level (home);
    format-out ("point 2\n");
    let  (depth, uenv) = get-uenv-level (node);
    format-out ("point 3, source depth = %d, dest depth = %d\n", depth, dest-depth);
    emit-non-local-exit-unwind (jbb, uenv, depth - dest-depth);
    format-out ("point 4\n");
  end
end;

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <nop-computation>, nodes, value?) => ()
end;

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <nop>, nodes, value?) => ()
  if (value?)
    format-out ("funny: <nop> in value position\n")
  end
end;


define method emit-expression-tree (jbb :: <java-basic-block>, node :: <make-cell>, nodes, value?) => ()
  emit-expression (jbb, node.computation-value, nodes, 1);  // cells single-valued?
  let the-cell = node.temporary;
  let (env, slot) = maybe-find-in-env (the-cell);
  if (env)
    if (value?)
      emit-dup (jbb)
    end;
    emit-env-write (jbb, env, slot)
  else
    if (~value?)
      emit-pop-local (jbb, the-cell.number-local-var, j-ref-code)
    end
  end
end;

define function emit-env-access (jbb :: <java-basic-block>,
                                 env :: <java-env-model>,
                                 slot :: <java-bind-model>,
                                 this-env :: <java-env-model>,
                                 action :: <function>)
  format-out ("@@@ new style emit-env-access \n");
  if (env == this-env)
    emit-push-local (jbb, *the-env-temp*, j-ref-code)
  else
    emit-push-this (jbb);
//    java-read (jbb, get-env-slot-spec (env, jbb.meth.java-class))  // this is wrong, want actual method
    java-read (jbb, get-env-slot-spec (env, this-env.meth-class))
  end;
  action (jbb, slot.get-slot-spec)
end;

// duplicate // define thread variable *current-method* = #f;
define variable *spooky* = #f;

define function emit-env-read (jbb :: <java-basic-block>, env :: <java-env-model>, slot :: <java-bind-model>)
  let  this-env = element (*closure-env-lookup*, *current-method*.environment, default: #f);
  if (this-env)
    *spooky* := this-env;
    emit-env-access (jbb, env, slot, this-env, java-read)
  else
    error (" ######### old style emit-env-read");
    emit-expression-leaf (jbb, 42);
  end
end;

define function emit-env-write (jbb :: <java-basic-block>, env :: <java-env-model>, slot :: <java-bind-model>)
  let  this-env = element (*closure-env-lookup*, *current-method*.environment, default: #f);
  if (this-env)
    *spooky* := this-env;
    emit-env-access (jbb, env, slot, this-env, java-write)
  else
    error (" ######### old style emit-env-write");
    emit-expression-leaf (jbb, 42);
    emit-pop (jbb);
    emit-pop (jbb)
  end
end;

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <get-cell-value>, nodes, value?) => ()
  // if closed over, have to find the right environment, and getfield it
  let the-cell = node.computation-cell;
  let (env, slot) = maybe-find-in-env (the-cell);
  if (env)
    emit-env-read (jbb, env, slot)
  else
    emit-expression-leaf (jbb, the-cell)
  end;
  // just use as value
  if (~value?)
    emit-pop-local (jbb, node.temporary.number-local-var, j-ref-code)
  end
end;


define method maybe-find-in-env (bind :: <module-binding> /*<canonical-module-binding>*/ )
  #f
end;


define method maybe-find-in-env (tmp :: <temporary>)
  let  bind-model = element (*closure-env-lookup*, tmp, default: #f);
  if (bind-model)
//    format-out ("maybe-find-in-env, %s in %s\n", bind-model, bind-model.env);
    values (bind-model.env, bind-model)
  else
    #f
  end
end;


/*
define method maybe-find-in-env (tmp :: <temporary>)
  if (tmp.closed-over?)
    let  env = tmp.environment;
    let  env-number = env.number-environment;
    let  tmp-number = number-env-binding (env, tmp);
    values (env-number, tmp-number)
  else
    #f
  end
end;

define thread variable *environment-numbering-table* = #f; // bound around compilation


// skip stupid NEXT-METHOD lossage?  non-lambdas are not real environments!
define method number-environment (env :: <lexical-environment>)
  env.outer.number-environment
end;

define method number-environment (env :: <lambda-lexical-environment>)
  let  next = env.outer;
  if (~next)
    0
  else
    let  lookup = #f; // element (*environment-numbering-table*, env, default: #f);
    if (lookup)
      lookup
    else
      let result = 1 + next.number-environment;
//      *environment-numbering-table*[env] := result;
      result
    end
  end
end;

define thread variable *environment-bindings* = #f; // bound around compilation

define function vector-position (thing, svec :: <vector>) => (pos :: false-or (<integer>))
  let  s = svec.size;
  block (return)
    for (n :: <integer> from 0 below s)
      if (svec[n] == thing)
        return (n)
      end
    end;
    #f
  end
end;

define function number-env-binding (env, tmp)
  let  table = element (*environment-bindings*, env, default: #f);
  if (~table)
    table := make (<stretchy-vector>, size: 1, fill: tmp);
    *environment-bindings* [env] := table;
    0
  else
    let  num = vector-position (tmp, table);
    if (~num)
      num := table.size;
      add! (table, tmp)
    end;
    num
  end
end;
*/

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <set-cell-value!>, nodes, value?) => ()
  emit-expression (jbb, node.computation-value, nodes, 1);  // cells single valued?
  if (value? | node.temporary.frame-offset)
    emit-dup (jbb)
  end;
  emit-pop-local (jbb, node.computation-cell.number-local-var, j-ref-code);
  if ((~value?) & node.temporary.frame-offset)
    emit-pop-local (jbb, node.temporary.number-local-var, j-ref-code)
  end
end;


define constant $mv-count-setter-meth$ =
  meth-spec ($dylan/dylanthread$, "mv_count",
             meth-type ($java-void-type$, $java-int-type$), j-invokestatic);

define constant $mv-count-setter-meths$ =
  vector (meth-spec ($dylan/dylanthread$, "mv_count_0",
                     meth-type ($java-void-type$), j-invokestatic) ,
          meth-spec ($dylan/dylanthread$, "mv_count_1",
                     meth-type ($java-void-type$), j-invokestatic) ,
          meth-spec ($dylan/dylanthread$, "mv_count_2",
                     meth-type ($java-void-type$), j-invokestatic));

/*
define method emit-expression-tree (jbb :: <java-basic-block>, node :: <adjust-multiple-values>, nodes, value?) => ()
  let  n = node.number-of-required-values;
//  emit-expression (jbb, node.computation-value, nodes, value?);
  begin
    let  temps = #();
    do-used-value-references (method (tt) temps := pair (tt, temps) end, node);
    emit-expression (jbb, first(temps), nodes, #t)
  end;
  push-the-thread (jbb);
  if (n <= 1)  // easy case, no need to check mv-count
    emit-raw-expression-leaf (jbb, n);
    java-write (jbb, mv-count ())
  else
    java-read (jbb, mv-count ());
    emit-dup (jbb);
    emit-raw-expression-leaf (jbb, n);

    let  splat-from = #f;
    let  splat-from-2 = #f;
    let  splat-to = #f;
    let  exit-from = #f;
    let  exit-to = #f;
    let  splat   = method () splat-to - splat-from end;
    let  splat-2 = method () splat-to - splat-from-2 end;
    let  exit    = method () exit-to - exit-from end;

    splat-from := jbb.pc;
    java-branch-op (jbb, j-if-icmplt, splat);  // SPLAT-CASE
    // case of same or fewer, just set count
    push-the-thread (jbb);
    emit-raw-expression-leaf (jbb, n);
    java-write (jbb, mv-count ());

    exit-from := jbb.pc;
    java-branch-op (jbb, j-goto, exit);  // EXIT

    //SPLAT-CASE:
    // this should all be a Java method call!!!
    splat-to := jbb.pc;
    emit-dup (jbb);
    push-the-thread (jbb);
    java-read (jbb, mv-vec ());
    emit-swap (jbb);
    emit-expression-leaf (jbb, #f);
    java-simple-op (jbb, j-aastore);
    emit-raw-expression-leaf (jbb, 1);
    java-simple-op (jbb, j-iadd);
    emit-dup (jbb);
    emit-raw-expression-leaf (jbb, n);

    splat-from-2 := jbb.pc;
    java-branch-op (jbb, j-if-icmplt, splat-2); // SPLAT-CASE

    push-the-thread (jbb);
    emit-swap (jbb);
    java-write (jbb, mv-count ());

    // EXIT:
    exit-to := jbb.pc
  end
end;
*/


define method emit-expression-tree (jbb :: <java-basic-block>, node :: <adjust-multiple-values>, nodes, value?) => ()
  let  n = node.number-of-required-values;
  begin
    let  temps = #();
    do-used-value-references (method (tt) temps := pair (tt, temps) end, node);
    emit-expression (jbb, first(temps), nodes, #t)
  end;
  if (n < 3)
    java-call (jbb, $mv-count-setter-meths$ [n])
  else
    emit-raw-expression-leaf (jbb, n);
    java-call (jbb, $mv-count-setter-meth$)
  end
end;

define method emit-expression-tree (jbb :: <java-basic-block>, node :: <adjust-multiple-values-rest>, nodes, value?) => ()
  examine (node)  // unimplemented yet
end;


// we don't do anything for a guarantee-type, except pass the result along
define method emit-expression-tree (jbb :: <java-basic-block>, node :: <guarantee-type>, nodes, value?) => ()
  emit-expression (jbb, node.computation-value, nodes, value?)
end;

