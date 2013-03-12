Module: dfmc-java-back-end
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sealed generic emit-java-code-for (back-end :: <java-back-end>, o);

define method emit-java-code-for (back-end :: <java-back-end>, o) => (spec :: false-or (<java-method-spec>))
  format-out ("###### emit-java-code-for ignoring an %s %s\n", o.object-class, o);
  #f
end;



define thread variable *temp-map* = #f;
define thread variable *temp-seq* :: <integer> = 0;
// duplicate // define variable *print-bbs* = #f;
// duplicate // define thread variable *java-class-cache* = #f;
// duplicate // define thread variable *gensym* = #f;

define function new-invented-name (prefix, debug)
  // use #nnn for anonymous stuff, since can't clash with mangled names
  let  gen :: <integer> = *gensym*;
  *gensym* := gen + 1;
  if (debug)
    format-to-string ("%s%sJ%d", prefix, debug.java-name-mangle, gen)
  else
    format-to-string ("%sJ%d", prefix, gen)
  end
end;

// we need to link with separately compiled stuff, so guess the binding for
// the actual object (perhaps just need loose-mode?)
define function new-guessed-name (prefix, debug)
  // use #nnn for anonymous stuff, since can't clash with mangled names
  if (debug)
    concatenate (prefix, debug.java-name-mangle)
  else
    format-out ("problem linking to anonymous thing %s in other library", prefix);
    "linking_problem"
  end
end;


define function java-class-for-thing (thing) => (cls :: <java-class>)
  let  result = element (*java-class-cache*, thing, default: #f);
  if (result == #t)
    error ("Cyclic problem in java-class-for-thing")
  end;
  result |
    begin
      *java-class-cache* [thing] := #t;
      format-out ("finding java-class for %s\n", thing);
      let  class :: <java-class> = find-java-class-for-thing (thing);
    format-out ("@@ created new java class for %s\n", thing);
      *java-class-cache* [thing] := class;
      // changed so that now all concretely modeled classes are output
      if (instance? (class, <java-concrete-class>))
        java-emit-class (class)
      end;
      class
    end
end;



define method find-java-class-for-thing (thing :: <&generic-function>)
  new-java-class-of-category (thing, $dylan-class-<generic-function>$, "gfJ")
end;

define method find-java-class-for-thing (thing :: <&method>)
//  my-break (thing);
  let  defn = model-definition (thing);
  let  name-hint = #f;
  if (defn)
    let  name = form-variable-name (defn);
    let  binding = lookup-binding (name);
  format-out ("!! binding %s for method\n", binding);
    let  gf-defn = binding-definition (binding, default: #f);
    name-hint := gf-defn & instance? (gf-defn, <generic-definition>) & name;
  end;
  new-java-class-of-category (thing, $dylan-class-<method>$, "methJ", name-hint: name-hint)
end;


define method find-java-class-for-thing (thing)
  error ("find-java-class-for-thing on unhandled thing %s", thing);
  #f
end;


// HACK broken
define method find-java-class-for-thing (thing :: <&union>)
  $dylan-class-<type>$
end;


define method find-java-class-for-thing (thing :: <boolean>)
  $dylan-class-<boolean>$
end;


// a HACK
define method find-java-class-for-thing (thing :: <&raw-type>)
  new-java-class-of-category (thing, $java/lang/Object$, "")
end;

// a HACK
//define method find-java-class-for-thing (thing :: <&singleton>)
//end;

// a HACK
define method find-java-class-for-thing (thing :: <&limited-integer>)
  let  min = thing.^limited-integer-min;
  let  max = thing.^limited-integer-max;
/*
  if (min >= -#x80 & max < #x80)
    $java-byte-type$
  elseif  (min >= -#x8000 & max < #x8000)
    $java-short-type$
  elseif  (min >= 0 & max < #x10000)
    $java-char-type$
  else
    $java-int-type$
  end
*/
  $dylan-class-<integer>$
end;

// a HACK
//define method find-java-class-for-thing (thing :: <&subclass>)
//end;

define method find-java-class-for-thing (thing :: <&class>)
  if (thing == dylan-value (#"<object>"))
    $dylan-class-<object>$
  elseif (thing == dylan-value (#"<integer>"))
    $dylan-class-<integer>$
  elseif (thing == dylan-value (#"<boolean>"))
    $dylan-class-<boolean>$
  else
    let  direct-supers = thing.^direct-superclasses;
    if (direct-supers.empty?)
      format-out ("**** believed wrong method for matching <object> - %s\n", thing);
      $dylan-class-<object>$
    else
      new-java-class-of-category (thing, direct-supers.first.java-class-for-super, "")
    end
  end
end;

define method find-java-class-for-thing (thing == <&integer>)
  error ("**** believed wrong method for matching <integer> - %s\n", thing);
  $dylan-class-<integer>$
end;

define method find-java-class-for-thing (thing == <&boolean>)
  error ("**** believed wrong method for matching <boolean> - %s\n", thing);
  $dylan-class-<boolean>$
end;



// this is probably a hack.
define function java-class-for-super (thing :: <&class>)
  let  sup = java-class-for-thing (thing);
  if (sup == $dylan-class-<object>$)
    $dylan-class-object0$
  else
    sup
  end
end;


define method new-java-class-of-category (thing, category, prefix, #key name-hint = #f)
  let  name = thing.^debug-name;
  if (name-hint & ~name)
    if (instance? (name-hint, <name-fragment>))
      name := as (<string>, name-hint.fragment-name);
    else
      name := name-hint.^debug-name
    end
  end;
if (name-hint)
  format-out ("!!!! name hinted %s\n", name);
//  my-break (name-hint);
end;
  new-named-java-class-of-category
    (thing, thing.model-library.language-definition, prefix, name, category)
end;


define function new-named-java-class-of-category (thing, lib, prefix, name, category)
  let libclass  = java-class-for-thing (lib);
  let libname = libclass.java-class-name.the-string;
  let libpackname = copy-sequence (libname, end: (libname.size - "-library".size));
  let class = if (lib == *current-be-library*)
                let iname = new-invented-name (prefix, name);
                format-out ("*** making concrete class for %s\n", thing);
                  make (<java-concrete-class>,
                      class-name:   iname,
                      package-name: libpackname,
                      super:        category,
                      represents:   thing,
                      library:      lib);
              else
                let gname = new-guessed-name (prefix, name);
                format-out ("*** making stub class for %s\n", thing);
//                format-out ("*** accessing %s %s from another library %s\n", category, thing, libname);
                make (<java-stub-class>, class-name: gname, package-name: libpackname, super: category)
              end;
  class
end;


define method find-java-class-for-thing (thing :: <java-primitive-type>) => (java-class :: <java-class>)
  thing.java-prim-reflected-class
end;

define method find-java-class-for-thing (thing :: <&iep>)
  java-class-for-thing (thing.function)
end;
define method find-java-class-for-thing (thing :: <&mep>)
  java-class-for-thing (thing.function)
end;
define method find-java-class-for-thing (thing :: <&xep>)
  java-class-for-thing (thing.function)
end;




define constant $dylan-java-hierarchy-package-base$ = "";
define constant $dylan-java-hierarchy-package-base2$ = "dylan";


//define variable *foo* = #();

// the current library will already be in the table, all others will be stubs */
define method find-java-class-for-thing (thing :: <library>)
  let  dname = thing.debug-name;
  let  name =  java-name-mangle (concatenate (as (<string>, dname), "-library"));
  let  packagename = if (dname == #"dylan")
                       $dylan-java-hierarchy-package-base$
                     else
                       $dylan-java-hierarchy-package-base2$
                     end;
  if (thing == *current-be-library*)
    format-out ("*** MAKING concrete class for <library> %s\n", thing);
    make (<java-concrete-class>,
          class-name:   name,
          package-name: packagename,
          super:        $dylan-class-<library>$,
          represents:  thing,
          library:     thing)
  else
//    format-out ("*** reference from %s to other library %s\n", *current-be-library*, thing);
    format-out ("*** MAKING stub class for <library> %s\n", thing);
//    *foo* := pair (thing, *foo*);
//    my-break (*foo*);
    make (<java-stub-class>, class-name: name, package-name: packagename, super: $dylan-class-<library>$)
  end
end;

define method find-java-class-for-thing (thing :: <&library>)
  let  dname = thing.^namespace-name;
  unless (dname)
    dname := thing.^debug-name
  end;
  unless (dname)
    my-break (thing)
  end;
  let  name =  java-name-mangle (concatenate (as (<string>, dname), "-library"));
  let  packagename = if (dname == #"dylan")
                       $dylan-java-hierarchy-package-base$
                     else
                       $dylan-java-hierarchy-package-base2$
                     end;
  if (thing.model-creator == *current-be-library*.namespace-definition)
    format-out ("*** MAKING concrete class for <&library> %s\n", thing);
    make (<java-concrete-class>,
          class-name:   name,
          package-name: packagename,
          super:        $dylan-class-<library>$,
          represents:   thing,
          library:      thing)
  else
    format-out ("*** reference from %s to other library %s\n", *current-be-library*, thing);
    my-break (pair (thing, *current-be-library*));
    format-out ("*** MAKING stub class for <&library> %s\n", thing);
    make (<java-stub-class>, class-name: name, package-name: packagename, super: $dylan-class-<library>$)
  end
end;

define method find-java-class-for-thing (thing :: <&module>)
  let  dname = thing.^namespace-name;
  let  name =  java-name-mangle (concatenate (as (<string>, dname), "-module"));
  let  packagename = if (dname == #"dylan")
                       $dylan-java-hierarchy-package-base$
                     else
                       $dylan-java-hierarchy-package-base2$
                     end;
  if (thing.^home-library == *current-be-library*. /*private-*/ namespace-model)
    format-out ("*** Finding stub class for <&module> %s\n", thing);
    java-class-for-thing (*current-be-library*)
  else
    format-out ("*** reference from %s to other library %s\n", *current-be-library*, thing);
    format-out ("*** Making stub class for <&module> %s\n", thing);
    make (<java-stub-class>, class-name: name, package-name: packagename, super: $dylan-class-<module>$)
  end
end;


/*
define function string-from-javaname (name :: <list>)
break();
  let  str :: <string> = name.head;
  for (pack-part in name.tail)
    str := concatenate (pack-part, "/", str)
  end;
  str
end;
*/

define variable *add-a-main* = #t;


define constant $java-class-init-methname$ = "<clinit>".uniq;
define constant $java-init-methname$ = "<init>".uniq;



define constant *max-ep-args* :: <integer> = 4;
define variable *ep-arg-types* = make (<vector>, size: *max-ep-args* + 1);

define function ep-arg-type (n :: <integer>)
  if (n > *max-ep-args*)
    n := *max-ep-args*
  end;
  let  type = *ep-arg-types*[n];
  unless (type)
    let  obj = $dylan-class-<object>$;
    let  args = if (n == *max-ep-args*)
                  list ($java-int-type$, obj,obj,obj,obj, array-type (obj))
                else
                  make (<list>, size: n, fill: obj)
                end;
    *ep-arg-types*[n] := type := apply (meth-type, obj, args)
  end;
  type
end;

// not specializing result type yet
define function specialized-ep-arg-type-internal (args :: <sequence>, iep? :: <boolean>) => (list :: <list>)
  let  n = args.size;
  if (iep? | n < *max-ep-args*)
    map (compose (specializer-type, specializer), args)
  else
    let  types = map-as (<list>,
                         compose (specializer-type, specializer),
                         copy-sequence (args, end: n));
    pair ($java-int-type$,
          concatenate (types, list ($dylan-class-<object>-array$)))
  end
end;

define function specialized-ep-arg-type (args :: <sequence>, iep? :: <boolean>, this?) => (spec :: <java-function-type>)
  let  argtypes = specialized-ep-arg-type-internal (args, iep?);
  if (this?)
    apply (meth-type, $dylan-class-<object>$, this?, argtypes)
  else
    apply (meth-type, $dylan-class-<object>$, argtypes)
  end
end;


define sealed generic java-type-for-specializer (spec :: <&type>) => (java-type :: <java-type>);

define method java-type-for-specializer (spec :: <&singleton>) => (java-type :: <java-type>)
  java-type-for-specializer (spec.^singleton-object.^object-class)
end;

define method java-type-for-specializer (spec :: <&class>) => (java-type :: <java-type>)
  java-class-for-thing (spec)
end;

define method java-type-for-specializer (spec :: <&limited-integer>) => (java-type :: <java-type>)
  $dylan-class-<integer>$
end;


define method java-type-for-specializer (spec :: <&union>) => (java-type :: <java-type>)
  $dylan-class-<object>$
end;

define function specialized-ep-full-type (fun :: <&function>, iep? :: <boolean>, this?) => (spec :: <java-function-type>)
  let est-funtype  = type-estimate-function-from-signature (fun.^function-signature, fun.^object-class, #f, body: fun.body);
//  my-break (est-funtype);
  let  argtypes = est-funtype.type-estimate-requireds;
  let  restypes = est-funtype.type-estimate-values;
  let  fn = compose (java-type-for-specializer, curry (as, <&type>));
  let  atypes = map (fn, argtypes);
//  my-break (atypes);
  let  rtypes = map (fn, type-estimate-fixed-values (restypes));
//  my-break (rtypes);
  if (rtypes.empty?)
    rtypes := vector ($java-void-type$)
  end;
//  let argtypes = specialized-ep-arg-type-internal (fun.parameters, iep?);
  if (this?)
    if (this? == #t)
      error ("whhopps")
    end;
    apply (meth-type, rtypes.first, this?, atypes)
  else
    apply (meth-type, rtypes.first, atypes)
  end
end;


define sealed generic specializer-type (arg :: <&type>) => (jclass :: <java-class-or-interface>);

define method specializer-type (arg :: <&type>) => (jclass :: <java-class-or-interface>)
  format-out ("Couldn't handle specializer type %s\n", arg);
  $dylan-class-<object>$
end;

define method specializer-type (arg :: <&singleton>) => (jclass :: <java-class-or-interface>)
  format-out ("**** lax type for singleton %s\n", arg.^singleton-object);
  java-class-for-thing (arg.^singleton-object.^object-class)
end;

define method specializer-type (arg :: <&class>) => (jclass :: <java-class-or-interface>)
  if (arg == dylan-value (#"<object>"))
    $dylan-class-<object>$
  elseif (arg == <&object>)
    format-out ("**** believed wrong elision of <&class> <object>\n");
    $dylan-class-<object>$
  else
    java-class-for-thing (arg)
  end
end;



define generic find-lambda-env (environment :: <object>) => (res :: false-or (<lambda-lexical-environment>));

define method find-lambda-env (environment :: <lambda-lexical-environment>) => (res :: false-or (<lambda-lexical-environment>))
  environment
end;

/*
define method find-lambda-env (environment :: <top-level-environment>) => (res :: false-or (<lambda-lexical-environment>))
  environment
end;
*/

define method find-lambda-env (environment :: <local-lexical-environment>) => (res :: false-or (<lambda-lexical-environment>))
  find-lambda-env (environment.outer)
end;

define method find-lambda-env (environment == #f) => (res :: false-or (<lambda-lexical-environment>))
  #f
end;

define method find-lambda-env (environment :: <object>) => (res :: false-or (<lambda-lexical-environment>))
  if (environment.top-level-environment?)
    environment
  else
    format-out ("### find-lambda-env callled on a %s\n", environment.object-class);
    my-break (environment);
    error ("find-lambda-env callled on a bad thing!");
  end
end;



define sealed generic emit-iep-code-for-method (methodd :: <&method>,
                                         jc     :: <java-concrete-class>,
                                         lib-jc :: <java-concrete-class>,
                                         meth-spec :: <java-method-spec>,
                                         body, arguments) => (spec :: <java-method-spec>);

define method emit-iep-code-for-method (methodd :: <&getter-method>,
                                        jc     :: <java-concrete-class>,
                                        lib-jc :: <java-concrete-class>,
                                        meth-spec :: <java-method-spec>,
                                        body, arguments) => (spec :: <java-method-spec>)
  let  sd = methodd.^method-slot-descriptor;
  if (sd)
    create-getter-method-body (java-method (meth-spec), sd);
    emit-iep-random-stuff (jc, arguments, format-to-string ("%s", methodd.^method-slot-descriptor), #f, #f);
    emit-iep-normal-stuff (jc, #t);
  end;
  meth-spec
end;

define method emit-iep-code-for-method (methodd :: <&setter-method>,
                                        jc     :: <java-concrete-class>,
                                        lib-jc :: <java-concrete-class>,
                                        meth-spec :: <java-method-spec>,
                                        body, arguments) => (spec :: <java-method-spec>)
  let  sd = methodd.^method-slot-descriptor;
  if (sd)
    create-setter-method-body (java-method (meth-spec), sd);
    emit-iep-random-stuff (jc, arguments, format-to-string ("%s", methodd.^method-slot-descriptor), #f, #f);
    emit-iep-normal-stuff (jc, #t);
  end;
  meth-spec
end;

define function emit-iep-random-stuff (jc, arguments, name, protocol?, main?)
  if (*add-a-main* & main?)  // purely for bytecode verification debugging!
    add-a-main-method (jc, name)
  end;
  let  nargs = arguments.size;
//  let  arg-type = ep-arg-type (nargs);
//  add-an-xep-method (jc, nargs, arg-type);
  if (protocol?)
    add-function-protocol (jc, #(), #())  // arguments/get_arg_types/return_values
  end
end;

define function emit-iep-normal-stuff (jc :: <java-concrete-class>, needs-top-level-init :: <boolean>)
  add-dumb-init-method (jc, $dylan-class-<method>$, ~needs-top-level-init);
  add-class-protocol-init (jc, list ($dylan-class-<method>$), needs-top-level-init)
end;


// duplicate // define thread variable *unwind-handlers* = #f;


// given a value on the java stack, generate a type-check
// that either throws an Exception, or succeeds, the value is popped
define sealed generic emit-type-check (jbb, spec, handlur, pop? :: <boolean>) => ();

define method emit-type-check (jbb, spec, handlur, pop? :: <boolean>) => ()
  format-out ("cannot handle a specializer that is not a class or singleton yet %s\n", spec.object-class);
  if (pop?) emit-pop (jbb) end;
end;

define method emit-type-check (jbb, spec :: <&class>, handlur, pop? :: <boolean>) => ()
//  format-out ("**** generate argument type-check for <&class> %s\n", spec);
  let  tipe = java-class-for-thing (spec);
  let  scope-id = enter-handler-scope (handlur, jbb);
  java-op2 (jbb, j-checkcast, tipe);
  exit-handler-scope (handlur, jbb, scope-id);
  if (pop?) emit-pop (jbb) end
end;

define constant $fake-software-op$ =
  meth-spec ($java/lang/Object$, "software", meth-type ($java-void-type$), j-software);

define method emit-type-check (jbb, spec :: <&singleton>, handlur, pop? :: <boolean>) => ()
//  format-out ("**** generate argument type-check for <&singleton> %s\n", spec);
  unless (pop?) emit-dup (jbb) end;
  emit-expression-leaf (jbb, spec.^singleton-object);
  java-branch-op (jbb, j-if-acmpeq, 4);
  java-call (jbb, $fake-software-op$)  // hacky, should throw an exception
end;


define variable *print-dfm* = #f;


// iep probably doesn't need a trampoline?  to support GF dispatch??
define function emit-iep-trampoline (jc :: <java-concrete-class>,
                                     meth :: <java-method>,
                                     dest-meth :: <java-method-spec>,
                                     arguments) => ()
  let jbb = make-jbb (meth);
  begin
    if (*check-stack-types*)
      format-out ("@@@ modeling iep-trampoline args sets\n");
      model-set-meth-args (jbb, meth.slots-spec)
    end;
    push-trampolined-arguments (jbb, arguments, #f, #t, #t);  // merely cast types?
    java-call (jbb, dest-meth);
    emit-return (jbb, j-ref-code)
  end;
  finish-with-jbb (jbb, meth)
end;

define function emit-xep-trampoline (jc :: <java-concrete-class>,
                                     meth :: <java-method>,
                                     dest-meth :: <java-method-spec>,
                                     arguments) => ()
  let jmc = enter-java-method-context (meth);
  let jbb = make-jbb (meth);
  dynamic-bind (*jmc* = jmc)
    if (*check-stack-types*)
      format-out ("@@@ modeling xep-trampoline args sets\n");
      model-set-meth-args (jbb, meth.slots-spec)
    end;
    push-trampolined-arguments (jbb, arguments, #t, #f, #t);  // actually check types
    java-call (jbb, dest-meth);
    emit-return (jbb, j-ref-code);
  end dynamic-bind;
  finish-with-jbb (jbb, meth);
  exit-java-method-context (jmc);
  meth.max-locals := max (meth.max-locals, next-local-var-num ())
end;


define thread variable *the-env-temp* = #f;
define variable *use-debug-names* = #t;




define method emit-iep-code-for-method (methodd :: <&method>,
                                        jc ::     <java-concrete-class>,
                                        lib-jc :: <java-concrete-class>,
                                        entry-meth-spec :: <java-method-spec>,
                                        body, arguments) => (spec :: <java-method-spec>)
  if (*print-dfm*)
    pprint-dfms (body.next-computation, #f, 1)
  end;
  let  lib-concrete = lib-jc.concrete-implementation;
  let  implem-name = format-to-string ("iep%d", lib-concrete.ep-seqnum);
  lib-concrete.ep-seqnum := lib-concrete.ep-seqnum + 1;
  let  entry-function-type :: <java-function-type> = entry-meth-spec.slot-type;
  let  dest-spec = meth-spec (lib-jc,
                              implem-name,
                              specialized-ep-arg-type (arguments, #t, jc),
//                              apply (meth-type,
//                                     entry-function-type.java-function-result-type,
//                                     jc,    // insert the fake this
//                                     entry-function-type.java-function-arg-types),
                              j-invokestatic);
  let  meth = java-method (dest-spec);

  dynamic-bind (*temp-map* = make (<object-table>),
                *temp-seq* = 1)  // "this" is 0 in trampoline
    emit-iep-trampoline (jc, java-method (entry-meth-spec), dest-spec, arguments)
  end dynamic-bind;

  java-model-env (methodd);
  format-out (".. generating Java code for %s\n", methodd);
  dynamic-bind (*unwind-handlers* = make (<object-table>))
    let  (bbcoll, first-bb, last-bb, uenv-mapping) = identify-bbs-top-top-level (body);
    meth.bb-list := bbcoll;
    dynamic-bind (*current-method* = methodd,
                  *uenv-mapping*   = uenv-mapping,
                  *temp-map*       = make (<object-table>),
                  *temp-seq*       = 0,   // starting point for local vars (no "this" in static method)
                  *jmc*            = enter-java-method-context (meth),
                  *the-env-temp*   = #f)
      // do the specializer checking code (should really be in xep)
      let  jbb = make-jbb (meth);
      let  method-temp = get-temp-local-var ();  // method is first arg
      let  env-model = element (*closure-env-lookup*, methodd.environment, default: #f);

      if (env-model == #t) error ("whappen?") end;

      begin
//        let  the-object-class = dylan-value (#"<object>");
        let  handlur = #f;
        // NOTE must deal with arguments when greater than 4!!
        let  arg-count = arguments.size;
        let  arg-count-var =
          if (arg-count >= *max-ep-args*)
            get-temp-local-var()
          else #f
          end;
        let  rest-temp = #f;

        if (*use-debug-names*)
          emit-raw-expression-leaf (jbb, format-to-string ("%s", methodd));
          emit-pop (jbb);
        end;

        // need to set up argument models if checking java stack correctness
        if (*check-stack-types*)
          model-set-meth-args (jbb, dest-spec)
        end;

        for (arg in arguments, n from 0)
          let  spec = arg.specializer;
          if (n = *max-ep-args*)
            rest-temp := get-temp-local-var ()  // bind to the rest local
          end;
          let  varnum =
            if (n >= *max-ep-args*)
              emit-push-local (jbb, rest-temp, j-ref-code);
              emit-expression-leaf (jbb, n - *max-ep-args*);
              java-simple-op (jbb, j-aaload);
              let  temp = arg.number-local-var;
              emit-pop-local (jbb, temp, j-ref-code);
              temp
            else
              arg.number-local-var
            end;
/*
          if (spec == the-object-class)
            #f
          elseif (spec == <&object>)
            format-out ("**** believed wrong elision of <&class> <object>\n")
          else
            unless (handlur)
              handlur := find-simple-java-handler (*jmc*, $java/lang/ClassCastException$)
            end;
            emit-push-local (jbb, varnum, j-ref-code);
            emit-type-check (jbb, spec, handlur, #t)
          end
*/
        end;
        if (env-model & env-model.java-class)
          *the-env-temp* := get-temp-local-var ();
          emit-java-new-init-0 (jbb, env-model.java-class);
          emit-pop-local (jbb, *the-env-temp*, j-ref-code);
        end;

        handlur := #f;
        finish-with-jbb (jbb, meth)
      end;

      // have to do stack dataflow across these boundaries!
      dynamic-bind (*entry-stack-model* = jbb.local-var-types)
        process-bbs (meth, bbcoll);  // first pass, all but label fixup
      end dynamic-bind;

      exit-java-method-context (*jmc*);
      meth.max-locals := max (meth.max-locals, next-local-var-num())
    end dynamic-bind
  end dynamic-bind;

  let  env-top-level? =  instance? (methodd.environment.outer, <top-level-environment>);
  if (env-top-level? | ~methodd.users.empty?)
    emit-iep-random-stuff (jc, arguments, format-to-string ("%s", methodd), #t, #f)
  else
    // should make xep/mep conditional on users of them?
    format-out ("**** non-escaping method %s\n", methodd)
  end;
  emit-iep-normal-stuff (jc, env-top-level?);
  entry-meth-spec
end;


define sealed generic maybe-emit-ep-code (ep) => (spec :: false-or (<java-method-spec>));

define method maybe-emit-ep-code (iep :: <&iep>) => (spec :: false-or (<java-method-spec>))
  let  func = iep.function;
  maybe-emit-iep-code-internal (iep.function, iep.body, func.parameters)
end;
define method maybe-emit-ep-code (mep :: <&mep>) => (spec :: false-or (<java-method-spec>))
  let  func = mep.function;
//  maybe-emit-ep-code (func.^iep);
  maybe-emit-mep-code-internal (func, mep.body, if (instance? (func, <&lambda>)) func.parameters end)
end;
define method maybe-emit-ep-code (xep :: <&xep>) => (spec :: false-or (<java-method-spec>))
  let  func = xep.function;
//  maybe-emit-ep-code (func.^iep);
  maybe-emit-xep-code-internal (func, if (instance? (func, <&lambda>)) func.parameters end)
end;

define method maybe-emit-ep-code (function :: <&method>) => (spec :: false-or (<java-method-spec>))
  let spec = #f;
/*
  if (function.^iep)
    spec := maybe-emit-ep-code (function.^iep)
  end;
  if (function.^mep)
    spec := maybe-emit-ep-code (function.^mep)
  end;
*/
  if (function.^xep)
    spec := maybe-emit-ep-code (function.^xep)
  end;
  // attempt to return most general entry point
  // but probably should use nature of call instruction to determine that
  spec
end;

define method maybe-emit-ep-code (function :: <&lambda>) => (spec :: false-or (<java-method-spec>))
  let spec = #f;
  if (function.^mep)
    spec := maybe-emit-ep-code (function.^mep)
  end;
  let next = next-method ();
  if (next)
    spec := next
  end;
  spec
end;

define method maybe-emit-ep-code (function :: <&keyword-method>) => (spec :: false-or (<java-method-spec>))
  let spec = #f;
  if (function.^iep)
    spec := maybe-emit-ep-code (function.^iep)
  end;
  let next = next-method ();
  if (next)
    spec := next
  end;
  spec
end;



define function maybe-emit-iep-code-internal (methodd :: <&method>, body, arguments) => (spec :: false-or (<java-method-spec>))
  if (arguments)
    let  jc = java-class-for-thing (methodd);
    let  concrete = jc.concrete-implementation;
    if (concrete)
      concrete.iep-emitted? |
        begin
          concrete.iep-emitted? := #t; // mark that we are underway
          let  nargs = arguments.size;
          let  arg-type = specialized-ep-arg-type (arguments, #t, #f);
          let meth-entry-spec = meth-spec (jc, "iep", arg-type, j-invokevirtual);
          let emit-spec = emit-iep-code-for-method (methodd, jc,
                            *current-be-library*.java-class-for-thing,
                            meth-entry-spec, body, arguments);
          if (emit-spec == #f)
            format-out ("##### FAILED to output IEP for %s\n", methodd);
            my-break (methodd);
            emit-spec := #t
          end;
          concrete.iep-emitted? := emit-spec
        end
    else
      #f
    end
  end
end;

define function maybe-emit-mep-code-internal (methodd :: <&method>,
                                              body,
                                              arguments :: false-or (<sequence>)) =>
    (spec :: false-or (<java-method-spec>))
  if (arguments)
  let  jc = java-class-for-thing (methodd);
  let  concrete = jc.concrete-implementation;
  if (concrete)
    concrete.mep-emitted? |
      begin
        concrete.mep-emitted? := #t; // mark that we are underway
        let  nargs = arguments.size;
//        let  arg-type = ep-arg-type (arguments, $dylan-class-<method>$);
        let  arg-type = ep-arg-type (nargs);
format-out ("\narg type for mep is %s\n\n", arg-type);
        let  meth-entry-spec  = meth-spec (jc, "mep", arg-type, j-invokevirtual);
        let  emit-spec = emit-mep-code-for-method (methodd, jc,
                           *current-be-library*.java-class-for-thing,
                           meth-entry-spec, body, arguments);
        if (emit-spec == #f)
          format-out ("##### FAILED to output MEP for %s\n", methodd);
          my-break (methodd);
          emit-spec := #t
        end;
        concrete.mep-emitted? := emit-spec
      end
  else
    #f
  end
  end
end;

define function maybe-emit-xep-code-internal (methodd :: <&method>, arguments) => (spec :: false-or (<java-method-spec>))
  if (arguments)
  let  jc = java-class-for-thing (methodd);
  let  concrete = jc.concrete-implementation;
  if (concrete)
    concrete.xep-emitted? |
      begin
        concrete.xep-emitted? := #t; // mark that we are underway
        let  nargs = arguments.size;
        let  arg-type = ep-arg-type (nargs);
        let  meth-entry-spec  = meth-spec (jc, "xep", arg-type, j-invokevirtual);
        let  emit-spec = emit-xep-code-for-method (methodd, jc,
                           *current-be-library*.java-class-for-thing,
                           meth-entry-spec, arguments);
        if (emit-spec == #f)
          format-out ("##### FAILED to output XEP for %s\n", methodd);
          my-break (methodd);
          emit-spec := #t
        end;
        concrete.xep-emitted? := emit-spec
      end
  else
    #f
  end
  end
end;



define function check-argument-type (jbb :: <java-basic-block>, arguments, n :: <integer>, handlur) => (handlur)
  let  arg = arguments [n];
  let  spec = arg.specializer;
  let  the-object-class = dylan-value (#"<object>");
  if (spec == the-object-class)
    #f
  elseif (spec == <&object>)
    format-out ("**** believed wrong elision of <&class> <object>\n")
  else
    unless (handlur)
      handlur := find-simple-java-handler (*jmc*, $java/lang/ClassCastException$)
    end;
    emit-type-check (jbb, spec, handlur, #f)
  end;
  handlur
end;




define function push-trampolined-arguments (jbb, arguments, check? :: <boolean>, iep-in? :: <boolean>, iep-out? :: <boolean>) => (actuals :: <integer>)
//                                                             true                 false                 true
  let  nargs = arguments.size;
  let  large-case? :: <boolean> = nargs >= *max-ep-args*;
  emit-push-this (jbb); // push the method object itself
  let  handlur = #f;  // cache only one handlur here
  local method push-nth-in-arg (n :: <integer>) => ()
          if (iep-in?)
            emit-push-local (jbb, n + 1, j-ref-code)
          else
            emit-push-local (jbb,
                             if (n >= *max-ep-args*) *max-ep-args* else n end +
                             if (large-case?) 2 else 1 end,
                             j-ref-code);
            if (n >= *max-ep-args*)
              emit-raw-expression-leaf (jbb, n - *max-ep-args*);
              java-simple-op (jbb, j-aaload)
            end
          end;
          if (check?)
            handlur := check-argument-type (jbb, arguments, n, handlur)
          end
        end;

  if (iep-out? | ~ large-case?)
    for (n from 0 below nargs)
      push-nth-in-arg (n)
    end
  else
    if (iep-in?)
      emit-raw-expression-leaf (jbb, nargs)
    else
      emit-push-local (jbb, 1, j-int-code)
    end;

    for (n from 0 below min (nargs, *max-ep-args*))
      push-nth-in-arg (n)
    end;

    if (large-case? & ~iep-in? & ~iep-out?)
      emit-push-local (jbb, *max-ep-args* + 2, j-ref-code);
      if (check?)
        for (n from *max-ep-args* below nargs)
          emit-dup (jbb);
          emit-raw-expression-leaf (jbb, n - *max-ep-args*);
          java-simple-op (jbb, j-aaload);
          handlur := check-argument-type (jbb, arguments, n, handlur);
          emit-pop (jbb)
        end
      end
    elseif (nargs == *max-ep-args* & iep-in? & ~iep-out?)
      emit-java-null (jbb)
    elseif (large-case? & iep-in? & ~iep-out?)
      emit-raw-expression-leaf (jbb, nargs - *max-ep-args*);
      java-simple-op (jbb, j-anewarray);
      for (n from *max-ep-args* below nargs)
        emit-dup (jbb);
        push-nth-in-arg (n);
        emit-raw-expression-leaf (jbb, n - *max-ep-args*);
        java-simple-op (jbb, j-aastore)
      end
    end
  end;
  let nactuals = if (iep-in? & nargs >= *max-ep-args*)
                   *max-ep-args* + 2
                 else
                   nargs
                 end;
  jbb.max-locals := max (jbb.max-locals, nactuals + 1);
  nactuals
end;


define function emit-mep-code-for-method (methodd :: <&method>, jc, lib-jc, mep-spec, body, arguments) => (spec :: <java-method-spec>)

  format-out ("WHOOPS, generating MEP for %s, and DONT KNOW HOW!\n", methodd);

  let  nargs = arguments.size;
  let  arg-type = ep-arg-type (nargs);
  let  mep-meth = java-method (mep-spec);

  let  jbb = make-jbb (mep-meth);
  begin
    let  n-actuals = push-trampolined-arguments (jbb, arguments, #f, #f, #t);
    java-call (jbb,
               meth-spec (jc, "iep",
                          specialized-ep-arg-type (arguments, #t, #f),
                          j-invokevirtual));
    emit-return (jbb, j-ref-code)
  end;
  finish-with-jbb (jbb, mep-meth);
  mep-spec
end;

define function emit-xep-code-for-method (methodd :: <&method>, jc, lib-jc, xep-spec, arguments) => (spec :: <java-method-spec>)
  let  nargs = arguments.size;
  let  arg-type = ep-arg-type (nargs);

/*
  let  lib-concrete = lib-jc.concrete-implementation;
  let  implem-name = format-to-string ("xep%d", lib-concrete.ep-seqnum);
  lib-concrete.ep-seqnum := lib-concrete.ep-seqnum + 1;
*/
  let  iep-spec = maybe-emit-ep-code (methodd.^iep);
//  let  xep-meth = java-method (dest-spec);

  dynamic-bind (*temp-map* = make (<object-table>),
              *temp-seq* = 1)  // "this" is 0 in trampoline
    emit-xep-trampoline (jc, java-method (xep-spec), iep-spec, arguments)
  end dynamic-bind;
/*
  let  jmc = enter-java-method-context (xep-meth);
  let  jbb = make-jbb (xep-meth);
  dynamic-bind (*jmc* = jmc)
    let  n-actuals = push-trampolined-arguments (jbb, arguments, #t, #f, #t);
    java-call (jbb, meth-spec (jc, "iep", arg-type, j-invokevirtual));
    emit-return (jbb, j-ref-code)
  end dynamic-bind;
  finish-with-jbb (jbb, xep-meth);
  exit-java-method-context (jmc);
*/
  xep-spec
end;




define constant $java/io/OutputStream$ =
  java-io-class ("OutputStream", $java/lang/Object$);
define constant $java/io/FilterOutputStream$ =
  java-io-class ("FilterOutputStream", $java/io/OutputStream$);
define constant $java/io/PrintStream$ =
  java-io-class ("PrintStream", $java/io/OutputStream$);
define constant $java-out-stream$ =
  slot-spec ($java/lang/System$, "out", $java/io/PrintStream$, #t);



define function add-a-main-method (jc :: <java-concrete-class>, name :: <string>)
  let  jmeth = java-method (meth-spec (jc,
                                       "main",
                                       meth-type ($java-void-type$, array-type ($java/lang/String$)),
                                       j-invokestatic));

  let  printer = meth-spec ($java/io/PrintStream$,
                            "println",
                            meth-type ($java-void-type$, $java/lang/String$),
                            j-invokevirtual);
  let  jbb = make-jbb (jmeth);
  begin
    java-read (jbb, $java-out-stream$);
    emit-java-string (jbb, format-to-string ("Hi from main for %s", jc.java-class-name.the-string));
    java-call (jbb, printer);
    if (name)
      java-read (jbb, $java-out-stream$);
      emit-raw-expression-leaf (jbb, name);
      java-call (jbb, printer)
    end;
    emit-return (jbb, j-void-code)
  end;
  finish-with-jbb (jbb, jmeth)
end;



define constant $sov-init-meth$ =
  meth-spec ($dylan-class-<simple-object-vector>$,
             $java-init-methname$,
             meth-type ($java-void-type$, $java/lang/Object-array$),
             j-invokespecial);

define constant $dylan-type-array$ = array-type ($dylan-class-<type>$);



define function add-function-protocol (jc :: <java-concrete-class>, argtypes, restypes)
  add-function-protocol-return-values (jc, restypes);
  add-function-protocol-arguments (jc, argtypes.size);
  add-function-protocol-get-arg-types (jc, argtypes)
end;



define function add-dumb-init-method (jc, superclass, public?)
  let  spec  = meth-spec (jc,
                          $java-init-methname$,
                          meth-type ($java-void-type$, jc),
                          j-invokespecial);
  let  jmeth = java-method (spec, public?: public?);
  let  jbb = make-jbb (jmeth);
  begin
    if (*check-stack-types*)
      model-set-meth-args (jbb, spec)
    end;
    emit-push-local (jbb, 0, j-ref-code);  // this

//  java-call (jbb, meth-spec (jc, $slotname-dylan-class$, meth-type ($dylan-class-<class>$), j-invokestatic));
    java-read (jbb, slot-spec (jc, $slotname-dylan-class$, $dylan-class-<class>$, #t));
    emit-java-init (jbb, superclass, $dylan-class-<class>$);
    emit-return (jbb, j-void-code);
    jbb.max-locals := max (jbb.max-locals, 1)
  end;
  finish-with-jbb (jbb, jmeth)
end;


define function add-function-protocol-return-values (jc :: <java-concrete-class>, restypes)
  let  spec  = meth-spec (jc, "return_values",
                          meth-type ($dylan-class-<sequence>$),
                          j-invokevirtual);
  let  jmeth = java-method (spec);
  let  jbb = make-jbb (jmeth);
  begin
    if (*check-stack-types*)
      model-set-meth-args (jbb, spec)
    end;
    // set second result and rescount
    java-simple-op (jbb, j-aconst-null);
    emit-expression-leaf (jbb, #f);
    java-call (jbb, $values-methods$[2]);
    emit-pop (jbb);

    // create a simple-object-vector of restypes
    emit-java-int (jbb, restypes.size);
    java-op2 (jbb, j-anewarray, $java/lang/Object$);
    for (rtype in restypes, n from 0)
      emit-dup (jbb);
      emit-raw-expression-leaf (jbb, n);
      emit-expression-leaf (jbb, rtype); // hopefully this works
      java-simple-op (jbb, j-aastore)
    end;
    // now make object
    emit-java-new-init-1 (jbb, $dylan-class-<simple-object-vector>$, $java/lang/Object-array$);
    emit-return (jbb, j-ref-code)
  end;
  finish-with-jbb (jbb, jmeth)
end;


define function  add-function-protocol-arguments (jc :: <java-concrete-class>, argn)
  let  spec  = meth-spec (jc, "arguments", meth-type ($java-int-type$), j-invokevirtual);
  let  jmeth = java-method (spec);
  let  jbb = make-jbb (jmeth);
  begin
    if (*check-stack-types*)
      model-set-meth-args (jbb, spec)
    end;
    // set second/third results and rescount
    java-simple-op (jbb, j-aconst-null);
    emit-expression-leaf (jbb, #f);
    emit-dup (jbb);
    java-call (jbb, $values-methods$[3]);
    emit-pop (jbb);

    // create a simple-object-vector of restypes
    emit-java-int (jbb, argn);
    emit-return (jbb, j-int-code)
  end;
  finish-with-jbb (jbb, jmeth)
end;



define function add-function-protocol-get-arg-types (jc :: <java-concrete-class>, argtypes)
  let  spec  = meth-spec (jc, "get_arg_types", meth-type ($dylan-type-array$), j-invokevirtual);
  let  jmeth = java-method (spec);
  let  jbb = make-jbb (jmeth);
  begin
    if (*check-stack-types*)
      model-set-meth-args (jbb, spec)
    end;
    let  arg-types-slot = slot-spec (jc, "arg_types", $dylan-type-array$, #f);

    let  label-offset :: <integer> = 0;
    let  label-func = method () label-offset end;

    emit-push-local (jbb, 0, j-ref-code);
    java-read (jbb, arg-types-slot);
    emit-dup (jbb);
    emit-pop-local (jbb, 1, j-ref-code);
    label-offset := jbb.pc;
    java-branch-op (jbb, j-ifnonnull, label-func);

    // calculate the structure
    emit-java-int (jbb, argtypes.size);
    java-op2 (jbb, j-anewarray, $dylan-class-<type>$);
    for (atype in argtypes, n from 0)
      emit-dup (jbb);
      emit-raw-expression-leaf (jbb, n);
      emit-expression-leaf (jbb, atype); // hopefully this works
      java-simple-op (jbb, j-aastore)
    end;

    // cache it
    emit-dup (jbb);
    emit-pop-local (jbb, 1, j-ref-code);
    emit-push-local (jbb, 0, j-ref-code);
    emit-swap (jbb);
    java-write (jbb, arg-types-slot);

    // already cached:
    label-offset := jbb.pc - label-offset;

    emit-push-local (jbb, 1, j-ref-code);
    emit-return (jbb, j-ref-code);
    jbb.max-locals := max (jbb.max-locals, 2)
  end;
  finish-with-jbb (jbb, jmeth)
end;




// handling a class
define method emit-java-code-for (back-end :: <java-back-end>, o :: <&class>)
  java-emit-class-class (o.java-class-for-thing)
end;

define method emit-java-code-for (back-end :: <java-back-end>, o :: <&mep>)
  format-out("---mep %s ---\n", o.function);
  maybe-emit-ep-code (o)
end;

define method emit-java-code-for (back-end :: <java-back-end>, o :: <&iep>)
  format-out("---iep %s ---\n", o.function);
  maybe-emit-ep-code (o)
end;

define method emit-java-code-for (back-end :: <java-back-end>, o :: <&method>)
  format-out("---method %s ---\n", o);
  maybe-emit-ep-code (o)
end;

define method emit-java-code-for (back-end :: <java-back-end>, methodd :: <&getter-method>)
  // need to get the actual arg type
  maybe-emit-iep-code-internal (methodd, #f, list (#f))
end;

define method emit-java-code-for (back-end :: <java-back-end>, methodd :: <&setter-method>)
  // need to get the actual arg type(s)
  maybe-emit-iep-code-internal (methodd, #f, list (#f, #f))
end;

define method emit-java-code-for (back-end :: <java-back-end>, o :: <&generic-function>)
  for (meth in o.^generic-function-methods) // was %generic-function-methods
    emit-java-code-for (back-end, meth)
  end;
  create-generic-function-code (o)
end;


define constant  $gf-init-meth$ =
  meth-spec ($dylan-class-<generic-function>$,
             $java-init-methname$,
             meth-type ($java-void-type$,
                        array-type ($dylan-class-<method>$),
                        $dylan-class-<boolean>$,
                        $dylan-class-<sequence>$,
                        $java-int-type$,
                        array-type ($dylan-class-<type>$),
                        $dylan-class-<boolean>$,
                        $dylan-class-<sequence>$),
             j-invokespecial);

define function model-set-meth-args (jbb :: <java-basic-block>, spec :: <java-method-spec>) => ()
  let  n :: <integer> = 0;
  if (spec.invoke-op.pops-instance?)
    if (*debug-jvm-instrs* == #t)
      format-out ("@@@ modeling set this %s\n", jbb.meth.java-class)
    end;
    model-set-a-local (jbb, n, jbb.meth.java-class);
    n := 1
  end;
  for (arg in spec.slot-type.java-function-arg-types)
    if (*debug-jvm-instrs* == #t)
      format-out ("@@@ modeling set arg %d %s\n", n, arg)
    end;
    model-set-a-local (jbb, n, arg);
    n := n + 1
  end
end;

define function create-generic-function-code (o :: <&generic-function>)
  let  meth-classes = map (java-class-for-thing, o.^generic-function-methods);  // was %generic-function-methods
  let  jclass = o.java-class-for-thing;
  let  self-init-meth = meth-spec (jclass, $java-init-methname$, $gf-init-meth$.slot-type, j-invokespecial);
  let  self-slot = slot-spec (jclass, $dylan-self-slot-name$, jclass, #t);
  java-field (self-slot);
  let  jmeth = java-method (meth-spec (jclass, $java-class-init-methname$, meth-type ($java-void-type$), j-invokestatic));
  let  jbb = make-jbb (jmeth);
  begin
    java-op2 (jbb, j-new, jclass);
    emit-dup (jbb);

    emit-raw-expression-leaf (jbb, meth-classes.size);
    java-op2 (jbb, j-anewarray, $dylan-class-<method>$);
    for (meth in meth-classes, n from 0)
      emit-dup (jbb);
      emit-raw-expression-leaf (jbb, n);
      java-read (jbb, slot-spec (meth, $dylan-self-slot-name$, meth, #t));
      java-simple-op (jbb, j-aastore)
    end;
    emit-expression-leaf (jbb, #f); // should be rest-return-value
    emit-java-null (jbb);           // should be function return values
    emit-raw-expression-leaf (jbb, 0); // should be required-number

    emit-raw-expression-leaf (jbb, 0);
    java-op2 (jbb, j-anewarray, $dylan-class-<class>$);  // argument types should go here

    emit-expression-leaf (jbb, #f); // should be rest-boolean
    emit-java-null (jbb);           // keyword sequence

    java-call (jbb, self-init-meth);

    java-write (jbb, self-slot);

    emit-return (jbb, j-void-code)
  end;
  finish-with-jbb (jbb, jmeth);

  // the trampolining <init> method needed to satisfy verifier
  jmeth := java-method (self-init-meth, public?: #f);
  jbb   := make-jbb (jmeth);
  begin
    if (*check-stack-types*)
      model-set-meth-args (jbb, self-init-meth)
    end;
    emit-push-local (jbb, 0, j-ref-code);  // this
    for (arg-type in self-init-meth.slot-type.java-function-arg-types, n from 1)
      emit-push-local (jbb, n, j-code-for (arg-type))
    end;
    /*
    emit-push-local (jbb, 1, j-ref-code);
    emit-push-local (jbb, 2, j-ref-code);
    emit-push-local (jbb, 3, j-ref-code);
    emit-push-local (jbb, 4, j-int-code);
    emit-push-local (jbb, 5, j-ref-code);
    emit-push-local (jbb, 6, j-ref-code);
    emit-push-local (jbb, 7, j-ref-code);
    */
    java-call (jbb, $gf-init-meth$);
    emit-return (jbb, j-void-code);
    jbb.max-locals := max (jbb.max-locals, 8)
  end;
  finish-with-jbb (jbb, jmeth)
end;


define constant $dylan-bindings-setup-methname$ = "Jbind".uniq;
define constant $dylan-symbols-setup-methname$  = "Jintern".uniq;
define constant $dylan-symbols-setup-methname2$ = "JDintern".uniq;
define constant $dylan-init-methname$           = "Jinit".uniq;



define constant $symbol-fixer-method$ =
  meth-spec ($dylan-class-<symbol>$,
             $dylan-symbols-setup-methname$,
             meth-type ($dylan-class-<symbol>$, $java/lang/String$),
             j-invokestatic);

define constant $symbol-fixer-method2$ =
  meth-spec ($dylan-class-<symbol>$,
             $dylan-symbols-setup-methname2$,
             meth-type ($dylan-class-<symbol>$, $dylan-class-<byte-string>$),
             j-invokestatic);


// maybe this can all searching is handled by java-method itself?
define function find-static-void-method (jc :: <java-concrete-class>, nam :: <byte-string>)
  let  nam-index = java-name-pool-index (nam, jc);
  block (return)
    for (m in jc.methods)
      if (m.slot-name == nam-index)
        return (m)
      end
    end;
    java-method (meth-spec (jc, nam, meth-type ($java-void-type$), j-invokestatic))
  end
end;



define function emit-class-init (jc :: <java-concrete-class>, definitions, heap-root-init-code)
  let  jmeth = find-static-void-method (jc, $dylan-bindings-setup-methname$);
  let  jbb = make-jbb (jmeth);
  begin
    for (binding in definitions)
      let  slot = java-rep (binding);
      let  val = binding.binding-value-slot;
      if (instance? (val, <&generic-function>))
        let  cls = val.java-class-for-thing;
        java-read (jbb, slot-spec (cls, $dylan-self-slot-name$, cls, #t));
        java-write (jbb, slot)
      end;
      if (instance? (val, <&class>))
        let  cls = val.java-class-for-thing;
        java-read (jbb, slot-spec (cls, $slotname-dylan-class$, $dylan-class-<class>$, #t));
        java-write (jbb, slot)
      end
    end
  end;
  finish-with-jbb (jbb, jmeth);

  dynamic-bind (*temp-map* = make (<table>),
               *temp-seq* = 0)   // a static method this
    jmeth := find-static-void-method (jc, $dylan-init-methname$);
    let jmc = enter-java-method-context (jmeth);
//    jbb := make-jbb (jmeth);
    begin
      dynamic-bind (*jmc* = jmc)
        for (code in heap-root-init-code)
          java-model-env (code);
          emit-class-init-code (/*jbb*/ jmeth, code.^iep)
        end
      end dynamic-bind
    end;
//    finish-with-jbb (jbb, jmeth);
    exit-java-method-context (jmc);
    jmeth.max-locals := max (jmeth.max-locals, next-local-var-num ())
  end dynamic-bind;

  if (*add-a-main*)  // purely for bytecode verification debugging
    add-a-main-method (jc, "foo")
  end
end;



define function end-emit-class-init (jc :: <java-concrete-class>)
  let  symbol-setup-meth = meth-spec (jc, $dylan-symbols-setup-methname$,
                                      meth-type ($java-void-type$), j-invokestatic);
  let  concrete = jc.concrete-implementation;
  unless (concrete)
    error ("should be concrete java class")
  end;
  let  jmeth = java-method (symbol-setup-meth);
  let  jbb = make-jbb (jmeth);
  begin
    for (sym in concrete.symbol-slots-list) // sym is pair (string, slot)
      emit-raw-expression-leaf (jbb, sym.head);
      java-call (jbb, $symbol-fixer-method$);
      java-write (jbb, sym.tail)
    end;
    emit-return (jbb, j-void-code)
  end;
  finish-with-jbb (jbb, jmeth);

  // seal off bindings-setup method
  jmeth := find-static-void-method (jc, $dylan-bindings-setup-methname$);
  jbb   := make-jbb (jmeth);
  begin
    emit-return (jbb, j-void-code)
  end;
  finish-with-jbb (jbb, jmeth);

  // seal off the init code method
  jmeth := find-static-void-method (jc, $dylan-init-methname$);
  jbb   := make-jbb (jmeth);
  begin
    emit-return (jbb, j-void-code)
  end;
  finish-with-jbb (jbb, jmeth);

  // note that the rest of the backend can place stuff in <clinit> before this point
  jmeth := find-static-void-method (jc, $java-class-init-methname$);
  jbb   := make-jbb (jmeth);
  begin
    java-call (jbb, meth-spec (jc, $dylan-bindings-setup-methname$,
                               meth-type ($java-void-type$), j-invokestatic));
    java-call (jbb, symbol-setup-meth);
    java-call (jbb, meth-spec (jc, $dylan-init-methname$,
                               meth-type ($java-void-type$), j-invokestatic));
    emit-return (jbb, j-void-code)
  end;
  finish-with-jbb (jbb, jmeth)
end;


define variable *generate-init-code* = #t;

//define method emit-class-init-code (jbb :: <java-basic-block>, iep :: <&iep>)
define method emit-class-init-code (jmeth :: <java-method>, iep :: <&iep>)
//  let  jmeth = jbb.meth;
//  let  jc    = jmeth.java-class;
  format-out (".. generating Java code for %s\n", iep);
  let  (bbcoll, first-bb, last-bb, uenv-mapping) = identify-bbs-top-top-level (iep.function.body);
  jmeth.bb-list := bbcoll;
  if (*generate-init-code*)
    dynamic-bind (*uenv-mapping* = uenv-mapping)
      for (bb in bbcoll.bb-vec)
        gen-one-bb (jmeth, gen-from-dfmc-bb-inline, bb, #"entry");
        // this is broken, since any returns must be mapped to
        // to goto past the end---bind *emit-returns* to a thunk for label resolution?

    // au contraire, this broke label resolution totally
        // gen-from-dfmc-bb-inline (jbb, bb)
      end
    end dynamic-bind
  end
end;




define function  add-class-protocol-init (jc :: <java-concrete-class>, supers, self-slot)
  let  nsupers = supers.size;
  let  slot  = slot-spec (jc, $slotname-dylan-class$, $dylan-class-<class>$, #t);
  if (self-slot)
    self-slot := slot-spec (jc, $dylan-self-slot-name$, jc, #t)
  end;
  java-field (slot);

  // shouldn't this look it up?
  let  jmeth = java-method (meth-spec (jc, $java-class-init-methname$, meth-type ($java-void-type$), j-invokestatic));
  let  jbb = make-jbb (jmeth);
  begin
    if (jbb.pc > 0)
      format-out ("unexpected second call to add-class-protocol-init for same class? %s\n", jc);
      my-break (jc)
    end;

    // make the <class>
    java-op2 (jbb, j-new, $dylan-class-<class>$);
    emit-dup (jbb);

    // make vector of supers
    emit-raw-expression-leaf (jbb, nsupers);
    java-op2 (jbb, j-anewarray, $dylan-class-<class>$);
    for (super in supers, n from 0)
      emit-dup (jbb);
      emit-raw-expression-leaf (jbb, n);

//    java-read (jbb, slot-spec (super, $slotname-dylan-class$, $dylan-class-<class>$, #t));
      java-call (jbb, meth-spec (super /*jc*/, $slotname-dylan-class$, meth-type ($dylan-class-<class>$), j-invokestatic));

      java-simple-op (jbb, j-aastore)
    end;

//emit-expression-leaf (jbb, jc.class-name); // really want unmangled name!
    begin
      let  rep = jc.represents;
      format-out ("### represents %s\n", rep);
      let  name = rep.^debug-name;
      format-out ("### debug name %s\n", name);
      //my-break (rep);
      emit-raw-expression-leaf (jbb, name | "[Unnamed]")
    end;
    emit-java-null (jbb);
    emit-java-init (jbb, $dylan-class-<class>$,
                    $dylan-class-<class>-array$,
                    $java/lang/String$,
                    $dylan-class-<object>-array$);
    java-write (jbb, slot);

    if (self-slot)
      java-field (self-slot);
      emit-java-new-init-0 (jbb, jc);
      java-write (jbb, self-slot)
    end;
    emit-return (jbb, j-void-code)
  end;
  finish-with-jbb (jbb, jmeth)
end;



define function java-emit-class-class (jc :: <java-class-or-interface>) => ()
  let  concrete = jc.concrete-implementation;
  if (concrete)
    unless (concrete.been-inited?)
      concrete.been-inited? := #t;
      java-deal-with-inheritance (jc);  // do this just once
      java-emit-class (jc)
    end
  else
    error ("java-emit-class-class needs a concrete class")
  end
end;

define function clear-pending-java-classes ()
  *the-pending-java-classes* := make (<object-table>)
end;

define function flush-java-classes ()
  let  class-table = *the-pending-java-classes*;
  *the-pending-java-classes* := make (<object-table>);
  for (jc :: <java-concrete-class> in class-table.key-sequence)
    if (class-table[jc])
      java-actually-emit-class (jc)
    end
  end;
  unless (*the-pending-java-classes*.empty?)
    error ("more classes generated during flush of java classes");
    flush-java-classes ()
  end;
end;




// concrete classes need emitting, but only once
/*  - old straight output to a .class file - need to make this switch automatically
define function java-actually-emit-class (jc :: <java-concrete-class-or-interface>) => ()
  let  file-name  = concatenate (jc.java-class-name.the-string, ".class");
  jc.code-index := java-name-pool-index ($Code-attr-name$, jc);
  format-out ("// generating ++ %s ++\n", file-name);
  let  class-stream = #f;
  block ()
    class-stream := open-output-stream (*java-back-end*, file-name);
    java-emit (class-stream, jc)
  cleanup
    if (class-stream)
      close (class-stream)
    end
  end
end;
*/



define function java-actually-emit-class (jc :: <java-concrete-class-or-interface>) => ()
//  let  jar  = current-jar ();
  let  jar = *current-library-jar*;
  let  date = current-date();
  let  zip-date = as-zip-date (date);
  let  file-name = jc.java-class-name.the-string;
//  add-to-jar! (jar, make (<zip-dir-entry>, filename: concatenate (file-name, "/"), filetime: zip-date)); // test hack
  file-name := concatenate (file-name, ".class");
  format-out ("// packing ++ %s ++ into %s\n", file-name, jar.jar-name);
  // need to emit class to a bytevector or something (temp file?)
  // then pack into jar
  let  ent = make (<class-string-jar-entry>,
                          filename: file-name,
                          filetime: zip-date //,
//                          zstream:  temp-stream
                          );
  java-emit (ent.zstream, jc);
  add-to-jar! (jar, ent);
end;


// top level interface - take stem of jar file name, and a sequence of zip-entries
// and write the file, return total size in bytes
define function write-zip-file (file-name, entries :: <sequence>) => (size :: <integer>)
  let  local-offsets = make (<simple-object-vector>, size: entries.size);
  let  zip-stream = #f;
  block ()
    let out-stream = open-output-stream (*java-back-end*, concatenate (file-name, ".jar"));
    zip-stream := make (<zip-crc-stream>, stream: out-stream);

    for (entry in entries, n :: <integer> from 0)
      local-offsets [n] := zip-stream.get-offset;
      write-zip-loc (zip-stream, entry, entry.writer)
    end;
    let  cen-offset :: <integer> = zip-stream.get-offset;
    for (entry in entries, n :: <integer> from 0)
      write-zip-cen (zip-stream, entry, local-offsets [n])
    end;
    write-zip-end (zip-stream, entries.size, cen-offset, zip-stream.get-offset - cen-offset, "Zip file");
    zip-stream.get-offset
  cleanup
    if (zip-stream)  close (zip-stream)  end
  end
end;
