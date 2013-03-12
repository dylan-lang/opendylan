Module: dfmc-java-back-end
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Closure stuff - need to compute a map of the environment as
// represented by Java classes at runtime - basically each level has
// its own class, which subsumes the lambda itself if only one lambda
// directly inside the environment.

// Currently assume the outer environment (outside the top-level method) is
// devoid of closed-over variables, since I don't know how to scope this
// (it may be trivial

// we map lambdas to one of these
define sealed class <java-env-model> (<object>)
  sealed slot java-class :: <java-class>, required-init-keyword: java-class: ;
  sealed slot meth-class :: <java-class>, required-init-keyword: meth-class: ;
  sealed slot prev-envs :: <list>, required-init-keyword: prev-envs: ;
  sealed slot var-slots :: <simple-object-vector>, required-init-keyword: var-slots: ;
  sealed slot depth :: <integer>, required-init-keyword: depth: ;
end;

define method print-object (model :: <java-env-model>, stream :: <stream>) => ()
  format (stream, "<java-env-model %s>", model.java-class)
end;

// map closed-over variables/temporaries and inner-lambdas (which have lost their bindings)
// to one of these
define sealed class <java-bind-model> (<object>)
  sealed slot env :: <java-env-model>, required-init-keyword: env: ;
  sealed slot indexx :: <integer>, required-init-keyword: indexx: ;
  sealed slot bind-name :: <string>, required-init-keyword: name: ;
  sealed slot bind-type :: <java-class>, required-init-keyword: type: ;
  sealed slot cached-slot-spec = #f ;
end;

define method print-object (model :: <java-bind-model>, stream :: <stream>) => ()
  format (stream, "<java-bind-model %s :: %s>", model.bind-name, model.bind-type)
end;

define function get-slot-spec (bmodel :: <java-bind-model>) => (slot-spec :: <java-slot-spec>)
  bmodel.cached-slot-spec |
  (bmodel.cached-slot-spec := slot-spec (bmodel.env.java-class, bmodel.bind-name, bmodel.bind-type, #f))
end;

define function get-env-slot-spec (model :: <java-env-model>, jclass :: <java-concrete-class>) => (slot-spec :: <java-slot-spec>)
  slot-spec (jclass, format-to-string ("envJ%d", model.depth), model.java-class, #f)
end;


define function generate-closure-constructor (env-model :: <java-env-model>, meth-class :: <java-concrete-class>)
  let  slots = env-model.var-slots ;
  if (slots)
    for (slot in slots)
      generate-closure-accessors (slot)
    end
  end;
  if (env-model.java-class)
    generate-closure-maker (env-model)
  end;
  generate-method-maker (env-model, meth-class)
end;

define function generate-method-maker (env-model :: <java-env-model>, meth-class :: <java-concrete-class>)
  let  all-the-envs = reverse (env-model.prev-envs) ;
  let  maker = java-method (meth-spec (meth-class, 
                          "Jmake_closure_method", 
                          apply (meth-type,
                                 $java-void-type$,
                                 map (java-class, all-the-envs)
                                 ),
                          j-invokevirtual)) ;
  let  jbb = make-jbb (maker) ;
  begin
    for (env in all-the-envs, n from 1)
      emit-push-this (jbb) ;
      emit-push-local (jbb, n, j-ref-code) ;
      java-write (jbb, get-env-slot-spec (env, meth-class)) ;
      jbb.max-locals := max (jbb.max-locals, n + 1)
    end;
    emit-return (jbb, j-void-code)
  end;
  finish-with-jbb (jbb, maker)
end;

define function generate-closure-maker (env-model)
  let  jclass = env-model.java-class ;
  let  all-prev-envs = reverse (env-model.prev-envs) ;

  // perhaps should simply use a constructor for this?
  let  the-maker =
    java-method (meth-spec (jclass, 
                            $java-init-methname$, 
                            apply (meth-type, 
                                   $java-void-type$, 
                                   map (java-class, all-prev-envs)), 
                            j-invokespecial)) ;
  let  jbb = make-jbb (the-maker) ;
  begin
    emit-push-this (jbb) ;
    java-call (jbb, meth-spec ($java/lang/Object$, $java-init-methname$, meth-type ($java-void-type$), j-invokespecial)) ;
    let  count = env-model.var-slots.size ;
    unless (count.zero?)
      emit-push-this (jbb) ;
      emit-expression-leaf (jbb, #f) ;
      for (slot in env-model.var-slots, n from 0)
        java-simple-op (jbb, j-dup2) ;
	java-write (jbb, slot.get-slot-spec)
      end ;
      emit-pop (jbb) ;
      emit-pop (jbb) ;
    end;
    emit-return (jbb, j-void-code)
  end;
  finish-with-jbb (jbb, the-maker)
end;

define variable *generate-closure-accessor-methods* = #f ;

define function generate-closure-accessors (bind-model :: <java-bind-model>)
  let  jclass = bind-model.env.java-class ;
  let  slot-name = bind-model.bind-name ;

//  format-out ("generate-closure-accessors:  bind-model name is %s\n", slot-name) ;

  let  slot-class = bind-model.bind-type ;
  let  slot-spec = bind-model.get-slot-spec ;

  if (*generate-closure-accessor-methods*)
    let  jmeth = java-method (meth-spec (jclass,
                                         slot-name,
                                         meth-type (slot-class),
                                         j-invokevirtual)) ;  // final?
    let  jbb = make-jbb (jmeth) ;
    begin
      emit-push-this (jbb) ;
      java-read (jbb, slot-spec) ;
      emit-return (jbb, j-ref-code)
    end;
    finish-with-jbb (jbb, jmeth) ;

    jmeth := java-method (meth-spec (jclass,
                                     slot-name,
                                     meth-type ($java-void-type$, slot-class),
                                     j-invokevirtual)) ;  //  final?
    jbb := make-jbb (jmeth) ;
    begin
      emit-push-this (jbb) ;
      emit-push-local (jbb, 1, j-ref-code) ;
      java-write (jbb, slot-spec) ;
      emit-return (jbb, j-void-code)
    end;
    finish-with-jbb (jbb, jmeth) 
  end
end;

// dummies - uncomment to bodge past native

//define class <top-level-environment> (<object>)
//end;

//define class <closure-lexical-environment> (<object>)
//end;



// ensures the whole nest of closures that this is a part of
// is modeled - goes up the tree until at the top,
// then calls java-model-environment.
// ARSE <top-level-environment> is not exported!!
define function java-model-env (meth :: <&method>)
  let  env = meth.environment ;
  if (element (*closure-env-lookup*, env, default: #f) == #f)
    let next-outer = find-lambda-env (env.outer) ;
    until (instance? (next-outer, <top-level-environment>))
      env := next-outer ;
      next-outer := find-lambda-env (env.outer)
    end;
//    if (env.inners.empty?)
//      *closure-env-lookup* [env] := #t
//    else
      java-model-environment (env, #f, #(), 0)
//    end
  end
end;

  
// ARSE, <closure-lexical-environment> not exported
define function java-model-environment
    (env :: <lambda-lexical-environment>, 
     outer-model,
     already-seen :: <list>,
     depth :: <integer>)
//  my-break (env) ;

  let  insides = env.inners ;

  let  closure-set = #() ;
  // search for closed over variables
  for (temp in env.temporaries)
    if (temp.closed-over? & ~ (member? (temp, closure-set)))
      closure-set := pair (temp, closure-set)
    end
  end;
  // don't forget closure variables!!
  if (instance? (env, <closure-lexical-environment>))
    for (temp in env.closure)
      if (temp.closed-over? & ~ (member? (temp, closure-set)))
        closure-set := pair (temp, closure-set)
      end
    end
  end;

  let  new-slots = #() ;
  let  prev-seen = already-seen ;
  // collect newly closed variables/temps
  for (closed in closure-set)
    unless (member? (closed, already-seen))
      new-slots := pair (closed, new-slots) ;
      already-seen := pair (closed, already-seen)
    end
  end;

  format-out ("@@@ %d new slots for %s at level %d\n", new-slots.size, env.lambda, depth) ;

  unless (already-seen.empty?)
    let  slot-count = new-slots.size ;

    let  prev-list = if (outer-model)
                       if (outer-model.java-class)
                         pair (outer-model, outer-model.prev-envs)
                       else
                         outer-model.prev-envs
                       end
                     else
                       #()
                     end;

    format-out ("@@@ level %d has previous levels %s\n", depth, prev-list) ;

    let meth-class = env.lambda.java-class-for-thing ;
    let env-class = #f ;
    let slot-vec = #f ;

    unless (zero? (slot-count))
      slot-vec  := make (<simple-object-vector>, size: slot-count) ;
      env-class := make-env-only-class (slot-vec)
    end;

    let new-env-model = make (<java-env-model>, 
                             java-class: env-class,
                             meth-class: meth-class,
                             var-slots:  slot-vec,
                             prev-envs:  prev-list,
                             depth:      depth) ;

    format-out ("@@@ made env model %s for %s\n", new-env-model, env.lambda) ;

    unless (zero? (slot-count))
      for (el in new-slots, n from 0)
        let  model = 
          if (instance? (el, <&method>))
            error ("Saw raw <&method> in env model!") ;
            make (<java-bind-model>, 
                  env: new-env-model, 
                  indexx: n,
                  name: new-invented-name ("", el.^debug-name),
                  type: java-class-for-thing (el))
          elseif (instance? (el.generator, <make-closure>))
            let  actual-method = el.generator.computation-closure-method ;
            let  type = actual-method.java-class-for-thing ;
            let  name = variable-name (el) ;
            format-out ("@@@ naming bind-model %d for inner lambda %s as %s :: %s\n", n, el, name, type) ;
            make (<java-bind-model>, 
                  env: new-env-model, 
                  indexx: n,
                  name: name, 
                  type: type)
          else
            let  dtype = new-find-specializer (el) ;
            let  type = if (dtype) 
                          dtype.java-class-for-thing
                        else
                          $dylan-class-<object>$ 
                        end ;
            let  name = variable-name (el) ;
            format-out ("@@@ naming bind-model %d for var %s as %s :: %s\n", n, el, name, type);
            make (<java-bind-model>,
                  env: new-env-model,
                  indexx: n,
                  name: name,
                  type: type)
          end;
        slot-vec [n] := model ;
        *closure-env-lookup* [el] := model
      end
    end;


    format-out ("@@@  env-model %s has var-slots %s\n", new-env-model, slot-vec);

    generate-closure-constructor (new-env-model, meth-class) ;
    if (env-class)
      finish-env-only-class (env-class, slot-vec) 
    end;

    finish-env-meth-class (meth-class, prev-list) ;

    *closure-env-lookup* [env] := new-env-model ;
    outer-model := new-env-model
  end;

  for (sub-env in insides)
    format-out ("@@@ env model recurse on %s\n", sub-env.lambda) ;
    java-model-environment (sub-env, outer-model, already-seen, depth + 1)
  end
end;


define sealed generic variable-name (thing) => (str :: <byte-string>) ;

define method variable-name (var :: <temporary>) => (str :: <byte-string>)
  format-to-string ("varJ%d", var.frame-offset)
end;

define method variable-name (var :: <lexical-variable>) => (str :: <byte-string>)
  java-name-mangle (as (<string>, var.name.fragment-name))
end;






define function  finish-env-meth-class (jclass :: <java-concrete-class>, env-list :: <list>)
  for (env :: <java-env-model> in env-list, n from 0)
    format-out ("@@@ adding field in %s for %s\n", jclass, env, get-env-slot-spec (env,jclass)) ;
    java-field (get-env-slot-spec (env, jclass))
  end
end;



define function make-env-only-class (slot-vec :: <simple-object-vector>) => (jclass :: <java-concrete-class>)
  let  jclass = new-named-java-class-of-category (#f, *current-be-library*, "envJ", "", $java/lang/Object$) ;
  java-emit-class (jclass) ;
  jclass
end;

define function finish-env-only-class (jclass :: <java-concrete-class>, slot-vec :: <simple-object-vector>) => ()
  for (slot :: <java-bind-model> in slot-vec)
    unless (slot.env.java-class == jclass)
      error ("fooy")
    end;
    format-out ("@@@ ading field in %s for %s\n", jclass, slot) ;
    java-field (slot.get-slot-spec)
  end;
end;
