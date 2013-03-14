Module: dfmc-java-back-end
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// names and types resolution.

// underneath <method> comes the typed functions
// functional arguments are always hoisted to <function>, since signature strings
// may not recurse.
// underneath the typed function classes (which are "dylan/#aux.meth#789" etc)
// come the actual implementations.
// For closures, the implementations have environment slots, and constructors.
// For non-closures, a single instance will be made and put in the binding.
// The top level method of a closure tree has a single instance in a binding.



define variable xep-invokers = #f;
define variable mep-invokers = #f;
define variable iep-invokers = #f;

define sealed generic dylan-invoke-method (call :: <function-call>, funct, argc :: <integer>)
 => (spec :: <java-method-spec>);

define method dylan-invoke-method (call :: <function-call>, funct, argc :: <integer>)
 => (spec :: <java-method-spec>)
  dylan-xep-invoke-method (funct, argc)
end;

define method dylan-invoke-method (call :: <function-call>, funct :: <&iep>, argc :: <integer>)
 => (spec :: <java-method-spec>)
  dylan-iep-invoke-method (funct, argc)
end;

define method dylan-invoke-method (call :: <method-call>, funct, argc :: <integer>)
 => (spec :: <java-method-spec>)
  dylan-mep-invoke-method (funct, argc)
end;



define method dylan-xep-invoke-method (funct, argc :: <integer>) => (spec :: <java-method-spec>)
  if (argc > 4)
    argc := 4    // represents the n-case
  end;
  if (xep-invokers == #f)
    xep-invokers := make (<simple-object-vector>, size: 5, fill: #f)
  end;
    let invoker = xep-invokers[argc];
    if (invoker == #f)
      invoker := make-invoker ($dylan-class-<function>$, argc, "xep");
      xep-invokers[argc] := invoker
    end;
  invoker
end;


define method dylan-mep-invoke-method (funct, argc :: <integer>) => (spec :: <java-method-spec>)
//  let  args = funct.splang;
//  let  res  = funct.splang;
//  meth-spec ($dylan-class-<function>$, "mep", mep-arg-type (args, res), j-invokevirtual)
  let  func = funct.function;
  let  jc = java-class-for-thing (func);
  meth-spec ($dylan-class-<function>$, "mep", specialized-ep-full-type (func, #t, jc), j-invokevirtual)
end;


define method dylan-iep-invoke-method (funct, argc :: <integer>) => (spec :: <java-method-spec>)
  meth-spec ($dylan-class-<function>$, "iep", specialized-ep-full-type (funct.function, #t, #f), j-invokestatic)
/*
  if (argc > 4)
    argc := 4    // represents the n-case
  end;
  if (iep-invokers == #f)
    iep-invokers := make (<simple-object-vector>, size: 5, fill: #f)
  end;
    let invoker = iep-invokers[argc];
    if (invoker == #f)
      invoker := make-invoker ($dylan-class-<function>$, argc, "iep");
      iep-invokers[argc] := invoker
    end;
  invoker
*/
end;


define function make-invoker (java-class :: <java-class>, argc :: <integer>, name :: <string>) => (spec :: <java-method-spec>)
  let  type = ep-arg-type (argc);
  meth-spec (java-class, name, type, J-invokevirtual)
end;



//////// this stuff moved from java-emit-class to here, to be sorted out.

define sealed generic java-rep (thing) => (other-thing);




// should this be accessing something better?
define method java-rep (cls :: <java-class-or-interface>) => (thing)
cls.examine;
  <java-class-or-interface>
end;




define constant $dylan/dylanthread$ =
  make (<java-stub-class>, class-name: "dylanthread", package: java-package ("dylan"), super: $java/lang/Thread$);


define function predefined-multi-class (classname :: <byte-string>, packname :: <byte-string>, primary-super :: <java-stub-class>, #rest other-supers :: <java-stub-class>) => (cls :: <java-stub-class>)
  make (<java-stub-class>,
        class-name:   java-name-mangle (classname),
        package:      java-package (packname),
        super:        primary-super)
end;


define function predefined-dylan-class (classname :: <byte-string>, primary-super, #rest other-supers) => (cls :: <java-stub-class>)
  predefined-multi-class (classname, "dylan", primary-super)
end;

define function predefined-dylan-internal-class (classname :: <byte-string>, primary-super :: <java-stub-class>, #rest other-supers :: <java-stub-class>) => (cls :: <java-stub-class>)
  predefined-multi-class (classname, "dylan/internal", primary-super)
end;

// placeholder for runtime support static methods:
define constant $dylan-runtime-class$ =
  predefined-dylan-internal-class ("dylan_runtime_class", $java/lang/Object$);

define constant $dylan-class-<object>$ =
  predefined-dylan-class ("<object>", $java/lang/Object$);
define constant $dylan-class-object0$ =
  predefined-dylan-class ("object0", $dylan-class-<object>$);
define constant $dylan-class-<function>$ =
  predefined-dylan-class ("<function>", $dylan-class-object0$);
define constant $dylan-class-<method>$ =
  predefined-dylan-class ("<method>", $dylan-class-<function>$);
define constant $dylan-class-<generic-function>$ =
  predefined-dylan-class ("<generic-function>", $dylan-class-<function>$);
define constant $dylan-class-<type>$ =
  predefined-dylan-class ("<type>", $dylan-class-object0$);
define constant $dylan-class-<class>$ =
  predefined-dylan-class ("<class>", $dylan-class-<type>$);

define constant $dylan-class-<library>$ =
  predefined-dylan-internal-class ("<library>", $dylan-class-<object>$);
define constant $dylan-class-<module>$ =
  predefined-dylan-internal-class ("<module>",  $dylan-class-<object>$);
define constant $dylan-class-<closure>$ =
  predefined-dylan-internal-class ("<closure>", $java/lang/Object$);

define constant $dylan-class-<boolean>$ =
  predefined-dylan-class ("<boolean>", $dylan-class-<object>$);
define constant $dylan-class-<symbol>$ =
  predefined-dylan-class ("<symbol>", $dylan-class-<object>$);
define constant $dylan-class-<number>$ =
  predefined-dylan-class ("<number>", $dylan-class-<object>$);
define constant $dylan-class-<complex>$ =
  predefined-dylan-class ("<complex>", $dylan-class-<number>$);
define constant $dylan-class-<real>$ =
  predefined-dylan-class ("<real>", $dylan-class-<complex>$);
define constant $dylan-class-<rational>$ =
  predefined-dylan-class ("<rational>", $dylan-class-<real>$);
define constant $dylan-class-<abstract-integer>$ =
  predefined-dylan-class ("<abstract-integer>", $dylan-class-<rational>$);
define constant $dylan-class-<integer>$ =
  predefined-dylan-class ("<integer>", $dylan-class-<abstract-integer>$);
define constant $dylan-class-<float>$ =
  predefined-dylan-class ("<float>", $dylan-class-<real>$);
define constant $dylan-class-<single-float>$ =
  predefined-dylan-class ("<single-float>", $dylan-class-<float>$);
define constant $dylan-class-<double-float>$ =
  predefined-dylan-class ("<double-float>", $dylan-class-<float>$);
define constant $dylan-class-<complex-integer>$ =
  predefined-dylan-class ("<complex-integer>", $dylan-class-<complex>$);
define constant $dylan-class-<complex-single-float>$ =
  predefined-dylan-class ("<complex-single-float>", $dylan-class-<complex>$);
define constant $dylan-class-<complex-double-float>$ =
  predefined-dylan-class ("<complex-double-float>", $dylan-class-<complex>$);

define constant $dylan-class-<character>$ =
  predefined-dylan-class ("<character>", $dylan-class-<object>$);
define constant $dylan-class-<byte-character>$ =
  predefined-dylan-class ("<byte-character>", $dylan-class-<character>$);
define constant $dylan-class-<unicode-character>$ =
  predefined-dylan-class ("<unicode-character>", $dylan-class-<character>$);

define constant $dylan-class-<collection>$ =
  predefined-dylan-class ("<collection>", $dylan-class-<object>$);
define constant $dylan-class-<mutable-collection>$ =
  predefined-dylan-class ("<mutable-collection>", $dylan-class-<collection>$);
define constant $dylan-class-<sequence>$ =
  predefined-dylan-class ("<sequence>", $dylan-class-<collection>$);
define constant $dylan-class-<mutable-sequence>$ =
  predefined-dylan-class ("<mutable-sequence>", $dylan-class-<sequence>$, $dylan-class-<mutable-collection>$);
define constant $dylan-class-<string>$ =
  predefined-dylan-class ("<string>", $dylan-class-<mutable-sequence>$);
define constant $dylan-class-<array>$ =
  predefined-dylan-class ("<array>", $dylan-class-<mutable-sequence>$);
define constant $dylan-class-<vector>$ =
  predefined-dylan-class ("<vector>", $dylan-class-<array>$);
define constant $dylan-class-<byte-string>$ =
  predefined-dylan-class ("<byte-string>", $dylan-class-<string>$, $dylan-class-<vector>$);
define constant $dylan-class-<unicode-string>$ =
  predefined-dylan-class ("<unicode-string>", $dylan-class-<string>$, $dylan-class-<vector>$);
define constant $dylan-class-<simple-vector>$ =
  predefined-dylan-class ("<simple-vector>", $dylan-class-<vector>$);
define constant $dylan-class-<simple-object-vector>$ =
  predefined-dylan-class ("<simple-object-vector>", $dylan-class-<simple-vector>$);

define constant $dylan-class-<list>$ =
  predefined-dylan-class ("<list>", $dylan-class-<string>$, $dylan-class-<mutable-sequence>$);
define constant $dylan-class-<empty-list>$ =
  predefined-dylan-class ("<empty-list>", $dylan-class-<list>$);
define constant $dylan-class-<pair>$ =
  predefined-dylan-class ("<pair>", $dylan-class-<list>$);


define constant $dylan/dylancondition$ =
  make (<java-stub-class>, class-name: "dylancondition", package: java-package ("dylan"), super: $java/lang/RuntimeException$);


define constant $dylan-class-<object>-array$ =  $dylan-class-<object>$.array-type;
define constant $dylan-class-<class>-array$ =  $dylan-class-<class>$.array-type;


define constant $slotname-dylan-class$ = "dylan_class";


define method java-rep (model :: <&object>) => (thing)
  java-rep ("j random object")
end;

define method java-rep (cls :: <&class>) => (thing)
  let  java-class = java-class-for-thing (cls);
  let  supers = cls.^direct-superclasses;
  unless (supers.empty?)  // force superclass to be represented
    java-class-for-thing (supers [0])
  end;
  slot-spec (java-class, $slotname-dylan-class$, $dylan-class-<class>$, #t)
end;
define method java-rep (cls :: <&top-type>) => (thing)
  let  java-class = $dylan-class-<object>$;
  slot-spec (java-class, $slotname-dylan-class$, $dylan-class-<class>$, #t)
end;

// HACK
define method java-rep (union :: <&union>) => (thing)
  let java-class = java-class-for-thing (<&union>);
  slot-spec (java-class, $slotname-dylan-class$, $dylan-class-<class>$, #t)
end;

define method java-rep (lim :: <&limited-integer>) => (thing)
  format-out ("calling hacky method of JAVA-REP for limited integer\n");
  let  int-type = java-class-for-thing (lim);
  int-type
end;


// HACK - wrong!
define method java-rep (lim :: <&singleton>) => (thing)
  format-out ("calling hacky method of JAVA-REP for a singleton\n");
  let  int-type = java-class-for-thing (lim.^singleton-object);
  int-type
end;



// obsolete ?
/*
define constant  $False-name$ = "False".uniq;
define constant  $True-name$  = "True".uniq;
define constant  $dylan-true$ =
  make (<java-slot-constant>,
        nat: make (<java-nat-constant>,
                   name: $True-name$,
                   type: $dylan-class-<boolean>$),
        java-class: $dylan-object-class$);

define constant  $dylan-false$ =
  make (<java-slot-constant>,
        nat: make (<java-nat-constant>, name: $False-name$,
                   type: $dylan-class-<boolean>$),
        java-class: $dylan-object-class$);



define constant  $dylan-mv-count$ =
  make (<java-slot-constant>,
        nat:   make (<java-nat-constant>, name: "mv_count".uniq, type: $java-int-type$),
        java-class: $dylan-thread-class$);


define constant  $dylan-array-of-object$ =
    make (<java-class-constant>, java-class: $dylan-class-<simple-object-vector>$);

//define function mv-count () $dylan-mv-count$ end;

define function array-of-object () $dylan-array-of-object$ end;

define constant $max-dylan-mvs$ = 50;


define variable *mv-fields* = make (<vector>, size: $max-dylan-mvs$);

define function mv-field (n :: <integer>)
  if (n < 0 | n >= $max-dylan-mvs$)
    format-out ("WHOOPS! multiple-value-index out of bounds\n")
  end; /**/
  if (~ (*mv-fields*[n]))
    let  namey = format-to-string ("mv_field_%d", n);
    *mv-fields*[n] :=  make (<java-slot-constant>, nat:
                               make (<java-nat-constant>,
                                     name: namey.uniq,
                                     type: $dylan-class-<object>$),
                             java-class: $dylan-thread-class$);
  end;
  *mv-fields*[n]
end;
*/

define method java-rep (true == #t) => (thing)
  $dylan-true-slot$
end;
define method java-rep (false == #f) => (thing)
  $dylan-false-slot$
end;

define constant $dummy-slot-for-int$ =
  slot-spec ($java/lang/Object$, "pong", $java/lang/object$, #t);

// problem is sometimes have to generate code, not just a constants pool entry
define method java-rep (i :: <integer>) => (thing)
  format-out ("WHOOPS saw <integer> in code\n");
my-break (i);
  $dummy-slot-for-int$
//  i
end;

define method java-rep (i :: <&raw-machine-word>) => (thing)
  i.^raw-object-value
end;

define method java-rep (i :: <&raw-boolean>) => (thing)
  if (i.^raw-object-value) 1 else 0 end
end;

define constant $dummy-slot-for-float$ =
  slot-spec ($java/lang/Object$, "pung", $java/lang/object$, #t);

define method java-rep (f :: <float>) => (thing)
  format-out ("WHOOPS saw <float> in code\n");
my-break (f);
  $dummy-slot-for-float$
end;

define constant $dummy-slot-for-sfloat$ =
  slot-spec ($java/lang/Object$, "snom", $java/lang/object$, #t);

define method java-rep (sf :: <&single-float>) => (thing)
  format-out ("WHOOPS saw <&single-float> in code\n");
my-break (sf);

// should create a java/lang/Float for this?
//  sf.^%single-float-data.^raw-object-value
  $dummy-slot-for-sfloat$
end;

// should be dealt with higher up the call tree
define method java-rep (sf :: <&raw-single-float>) => (thing)
  format-out ("WHOOPS saw <&raw-single-float> in code\n");
my-break (sf);

// should create a java/lang/Float for this?
//  sf.^%single-float-data.^raw-object-value
  $dummy-slot-for-sfloat$
end;

define constant $dummy-slot-for-dfloat$ =
  slot-spec ($java/lang/Object$, "snam", $java/lang/object$, #t);

define method java-rep (df :: <&double-float>) => (thing)
  format-out ("WHOOPS saw <&double-float> in code\n");
my-break (df);
// should create a java/lang/Double for this?
//  df.^%double-float-data.^raw-object-value
  $dummy-slot-for-dfloat$
end;

// should be dealt with higher up the call tree
define method java-rep (df :: <&raw-double-float>) => (thing)
  format-out ("WHOOPS saw <&double-float> in code\n");
my-break (df);

// should create a java/lang/Double for this?
//  df.^%double-float-data.^raw-object-value
  $dummy-slot-for-dfloat$
end;

define constant $dummy-slot-for-symbol$ =
  slot-spec ($java/lang/Object$, "snim", $java/lang/object$, #t);

define method java-rep (sym :: <symbol>) => (thing)
  format-out ("WHOOPS saw <symbol> in code\n");
my-break (sym);

  $dummy-slot-for-symbol$
end;

//  ARSE private-debug-name isn't defined

define function java-rep-for-callable (thing, type-sig, extern-type-sig)
  let lib = thing.model-library.language-definition;
  if (lib == *current-be-library*)
    let  java-class = java-class-for-thing (thing);
    slot-spec (java-class, "Jself", java-class, #t)
  else
    let libclass = java-class-for-thing (lib);
    if (type-sig ~== extern-type-sig)
      record-generalized-external-reference (thing.^debug-name)
    else
      record-external-reference (thing.^debug-name)
    end;
    if (thing.^debug-name)
      let name = java-name-mangle (thing.^debug-name);
      slot-spec (libclass, name, extern-type-sig, #t)  // I think static for known callables?
    else
      if (thing./*private-*/debug-name)
        let  name = java-name-mangle (thing./*private-*/debug-name);
        slot-spec (libclass, name, extern-type-sig, #t)  // I think static for known callables?
      else
//        break();    // unnamed from other library - whoops
        format-out ("WHOOPS, java-rep-for-callable given a %s, %s\n", thing.object-class, thing);
        libclass
      end
    end
  end
end;

define method java-rep (meth :: <&method>) => (thing)
  java-rep-for-callable (meth,
                         $dylan-class-<method>$,
                         $dylan-class-<generic-function>$)
end;

define method java-rep (gf :: <&generic-function>) => (thing)
  java-rep-for-callable (gf,
                         $dylan-class-<generic-function>$,
                         $dylan-class-<generic-function>$)
end;

// no explicit XEP's, since call through the method object
define method java-rep (xep :: <&xep>) => (thing)
  java-rep (xep.function)
end;



// probably wrong - need a way to represent a tailored calling conv.
define method java-rep (iep :: <&iep>) => (thing)
  java-rep (iep.function)
end;


define method java-rep (mod :: <&module>) => (thing)
  java-class-for-thing (mod)
end;
define method java-rep (mod :: <&library>) => (thing)
  java-class-for-thing (mod)
end;


define method java-rep (s :: <byte-string>) => (thing)
  make (<java-string-constant>,
        utf: make (<java-utf-constant>, string: s.uniq))
end;



define function guess-binding-type (bnd :: <module-binding> /*<canonical-module-binding>*/ )
//  let  can-change = // bnd.exported? |           // BROKEN?
//                    ~ bnd.assignments.empty?;
//  if (can-change)
//    $dylan-class-<object>$
//  else
    guess-value-type (bnd.binding-value-slot)
//  end
end;

define method guess-value-type (o)
  $dylan-class-<object>$
end;

define method guess-value-type (o :: <&generic-function>)
  $dylan-class-<generic-function>$
end;

define method guess-value-type (o :: <&method>)
  $dylan-class-<method>$
end;

define method guess-value-type (o :: <&class>)
//  java-type-for (o)
  $dylan-class-<class>$
end;

define method java-type-for (o)
  $dylan-class-<object>$
end;

define method java-type-for (o :: <&class>)
  java-class-for-thing (o)
end;

define function java-rep-binding (bnd, slot-name)
  let  lib       = bnd.binding-home.home-library;
  let  lib-class = java-class-for-thing (lib);
  let  slot = slot-spec (lib-class, slot-name, guess-binding-type (bnd), #t);
  if ((lib == *current-be-library*) & slot-not-already-present (lib-class, slot-name))
    java-field (slot)
  end;
  slot
end;

//define method java-rep (bnd :: <module-binding> /*<canonical-module-binding>*/ ) => (thing)
//  java-rep-binding (bnd, java-name-mangle (bnd.name))
//end;

define method java-rep (bnd :: <module-binding>) => (thing)
//  java-rep-binding (bnd, java-name-mangle (bnd.^debug-name))    MARKT, DEC 97
  java-rep-binding (bnd, java-name-mangle (bnd.name))
end;


define constant $rev-pair-func$ =
  meth-spec ($dylan-class-<pair>$,
             "Jrev_pair",
             meth-type ($dylan-class-<pair>$,
                        $dylan-class-<list>$,
                        $dylan-class-<object>$),
             j-invokestatic);



define sealed generic java-name-generate (o) => (uniq :: <unique-string>);

define method java-name-generate (s :: <unique-string>) => (uniq :: <unique-string>)
  s
end;
define method java-name-generate (s :: <byte-string>) => (uniq :: <unique-string>)
//format-out ("JAVA_NAME-GENERATE on a byte string %s\n", s);
  s.uniq
end;

define method java-name-generate (mo :: <virtual-object>) => (uniq :: <unique-string>)
  let  dname = mo.^debug-name;
  let  gensym = *gensym*;
  *gensym* := gensym + 1;
  uniq (if (dname)
          format-to-string ("%sJH%d", dname.java-name-mangle, gensym)
        else
          format-to-string ("JH%d", gensym)
        end)
end;

define method java-name-generate (mo) => (uniq :: <unique-string>)
  let  gensym = *gensym*;
  *gensym* := gensym + 1;
  uniq (format-to-string ("HJ%d", gensym))
end;





define method java-rep (list :: <list>, #next next-method) => (thing)
  next-method ()
/*
  // need to create suitable binding in the library
  // and code to initialize it.

  // thus all the elements are java-rep'd into the library first,
  // and a variable and initialization code created
  let  lib       = *current-be-library*;
  let  lib-class = java-class-for-thing (lib);
  let  slot-name = java-name-generate (list);
  let  slot = slot-spec (lib-class, slot-name, $dylan-class-<list>$, #t);

  let  rev = reverse (list);

format-out ("## the types in the represented list are\n");
for (rep in rev)
  format-out ("  ## type %s\n", rep.object-class)
end;

  let  clinit-meth = find-static-void-method (lib-class, $java-class-init-methname$);

  let  jbb = make-jbb (clinit-meth);
  begin
    emit-expression-leaf (jbb, #());
    for (rep in rev)
      emit-expression-leaf (jbb, rep);
      java-call (jbb, $rev-pair-func$);
    end;
    java-write (jbb, slot);
  end;
  finish-with-jbb (jbb, clinit-meth);
  slot
*/
end;

define method java-rep (vec :: <simple-object-vector>, #next next-method) => (thing)
  next-method ()
/*
  // need to create suitable binding in the library
  // and code to initialize it.

  // thus all the elements are java-rep'd into the library first,
  // and a variable and initialization code created
  let  lib       = *current-be-library*;
  let  lib-class = java-class-for-thing (lib);
  let  slot-name = java-name-generate (vec);
  let  slot = slot-spec (lib-class, slot-name, $dylan-class-<simple-object-vector>$, #t);

  let  len = vec.size;

  let  clinit-meth = find-static-void-method (lib-class, $java-class-init-methname$);

  let  jbb = make-jbb (clinit-meth);
  begin
    emit-raw-expression-leaf (jbb, len);
    java-simple-op (jbb, j-anewarray);
    for (rep in vec, n from 0)
      emit-dup (jbb);
      emit-expression-leaf (jbb, rep);
      emit-raw-expression-leaf (jbb, n);
      java-simple-op (jbb, j-aastore)
    end;
    emit-java-new-init-1 (jbb, $dylan-class-<simple-object-vector>$, $java/lang/Object-array$);
    java-write (jbb, slot);
  end;
  finish-with-jbb (jbb, clinit-meth);
  slot
*/
end;


define constant $dummy-slot-for-mm-wrapper$ =
  slot-spec ($java/lang/Object$, "ping", $java/lang/object$, #t);

define method java-rep (vec :: <&mm-wrapper>) => (thing)
  // hmm!
  $dummy-slot-for-mm-wrapper$
end;
