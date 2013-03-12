Module: java-modeling
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


format-out ("initing java-types.dylan\n");

// represent types without having to build sig-strings

define abstract class <java-type> (<object>)
  slot  cached-sig :: false-or (<unique-string>) = #f;
end;

// most general non-function type (not really in Java the language,
// but the union of the types that have real meaning at runtime, ie not
// return addresses, nor method/function types.
define abstract class <java-any> (<java-type>) end;

// special one for return addresses in stack modeling
define class <java-return-address-type> (<java-type>) end;

define constant $java-return-address$ :: <java-return-address-type> = make (<java-return-address-type>);


define class <java-any-type> (<java-any>) end;


define method compute-signature-string (ft :: <java-any-type>) => (sig :: <byte-string>)
  "A"
end;

define abstract class <java-reference-type> (<java-any>)
end;

define class <java-null-type> (<java-reference-type>)
end;

define method compute-signature-string (ft :: <java-null-type>) => (sig :: <byte-string>)
  "N"
end;

define constant $java-any-type$ :: <java-any-type> = make (<java-any-type>);
define constant $java-null-type$ :: <java-null-type> = make (<java-null-type>);

define method java-type-words (type :: <java-type>) => (size :: <integer>)
  1  // default for classes, arrays
end;

define sealed generic compute-signature-string (jt :: <java-type>) => (sig :: <byte-string>);

define function signature-string (jt :: <java-type>) => (uniq :: <unique-string>)
  jt.cached-sig | (jt.cached-sig := uniq (jt.compute-signature-string))
end;

define function signature-string-string (jt :: <java-type>) => (str :: <byte-string>)
  jt.signature-string.the-string
end;

define class <java-function-type> (<java-type>)
  sealed slot java-function-result-type :: <java-type>, required-init-keyword: result-type:;
  sealed slot java-function-arg-types :: <list>,        required-init-keyword: arg-types:;
end;

define method print-object (ft :: <java-function-type>, s :: <stream>) => ()
  let  beginning? :: <boolean> = #t;
  for (a in ft.java-function-arg-types)
    format (s, "%s%s", if (beginning?) "(" else " " end, a);
    beginning? := #f;
  end;
  if (beginning?)
    format (s, "(")
  end;
  format (s, ") => %s", ft.java-function-result-type);
end;

define method compute-signature-string (ft :: <java-function-type>) => (sig :: <byte-string>)
  let  res = apply (concatenate, "(", map (signature-string-string, ft.java-function-arg-types));
  concatenate (res, ")", signature-string-string (ft.java-function-result-type))
end;

define class <java-array-type> (<java-reference-type>)
  sealed slot java-array-element-type :: <java-any>, required-init-keyword: element-type:;
end;

define method print-object (at :: <java-array-type>, s :: <stream>) => ()
  format (s, "%s[]", at.java-array-element-type)
end;

// most general array type (not really in Java the language)
define constant $java-array-type$ :: <java-array-type> = make (<java-array-type>, element-type: $java-any-type$);

define method compute-signature-string (at :: <java-array-type>) => (sig :: <byte-string>)
  concatenate ("[", at.java-array-element-type.signature-string-string)
end;




define constant  j-int-code    :: <integer> = 0;
define constant  j-long-code   :: <integer> = 1;
define constant  j-float-code  :: <integer> = 2;
define constant  j-double-code :: <integer> = 3;
define constant  j-ref-code    :: <integer> = 4;
define constant  j-byte-code   :: <integer> = 5;
define constant  j-char-code   :: <integer> = 6;
define constant  j-short-code  :: <integer> = 7;
define constant  j-void-code   :: <integer> = 5;  // hacky - shouldn't really be used in data moves

define sealed generic j-code-for (tipe :: <java-type>) => (j-code :: <integer>);

define class <java-primitive-type> (<java-any>)
  sealed constant slot java-prim-sig :: <string>, required-init-keyword: sig:;
  sealed constant slot java-type-words :: <integer> = 1, init-keyword: words:;
  sealed constant slot java-prim-reflected-class :: <java-stub-class>, required-init-keyword: reflected-class:;
  sealed constant slot j-code-for :: <integer>, required-init-keyword: j-code-for:;
end;


define method j-code-for (tipe :: <java-reference-type>) => (j-code :: <integer>)
  j-ref-code
end;
define method j-code-for (tipe :: <java-null-type>) => (j-code :: <integer>)
  j-ref-code
end;

define class <java-primitive-fragment-type> (<java-primitive-type>)
  sealed slot which-fragment :: <integer>, required-init-keyword: which-fragment:;
end;

define method compute-signature-string (jpt :: <java-primitive-type>) => (sig :: <byte-string>)
  jpt.java-prim-sig
end;

define method print-object (jpt :: <java-primitive-type>, str :: <stream>) => ()
  format (str, "{java-type %s}", jpt.signature-string-string)
end;


define open abstract class <java-concrete> (<object>)
  constant slot class-or-interface :: <java-class-or-interface>, required-init-keyword: class-or-interface:;
end;


// just the naming stuff, not even superclasses - ie the symbolic link info
define abstract open class <java-class-or-interface> (<java-reference-type>)
  constant slot class-name   :: <byte-string>, required-init-keyword: class-name:;
  constant slot class-package  :: <java-package>, required-init-keyword: package:;
  slot fullname :: false-or (<unique-string>) = #f;
  sealed slot represents = #f, init-keyword: represents:;

  slot concrete-implementation :: false-or (<java-concrete>) = #f;
  slot rcpl  :: false-or (<simple-object-vector>) = #f;
end;

format-out ("java-types.dylan point 1\n");

define open generic interfaces (thing) => (ifs :: <sequence>);
define open generic interfaces-setter (ifs :: <sequence>, thing) => (ifs :: <sequence>);

define abstract open class <java-class> (<java-class-or-interface>)
  slot super :: false-or(<java-class>), required-init-keyword: super:;
  slot interfaces :: <simple-object-vector> = #[], init-keyword: interfaces:;
//  slot rcpl  :: <simple-object-vector>;
end;

define method initialize (claz :: <java-class>, #key super) => ()
  let  parent-rcpl = if (super) super.rcpl else #[] end;
  claz.rcpl := concatenate (parent-rcpl, vector (claz))
end;

define abstract open class <java-interface> (<java-class-or-interface>)
  slot super-interfaces :: <sequence>, required-init-keyword: supers:;
end;

// FAKE for now for interfaces
define function calc-rcpl (claz :: <java-interface>, supers :: <sequence>) => (rcpl :: <simple-object-vector>)
  #[]
end;

define method initialize (claz :: <java-interface>, #key supers :: <sequence>) => ()
  claz.rcpl := calc-rcpl (claz, supers);
end;

define method print-object (cls :: <java-class>, stream :: <stream>) => ()
  format (stream, "{%s}", cls.class-name)
end;


define function java-fullname (pack :: <java-package>, name :: <byte-string>) => (full :: <byte-string>)
  package-concatenate-with-name (pack, name)
end;


define function java-class-name (jc :: <java-class-or-interface>) => (uniq :: <unique-string>)
  jc.fullname | (jc.fullname := uniq (java-fullname (jc.class-package, jc.class-name)))
end;


define method compute-signature-string (jc :: <java-class-or-interface>) => (sig :: <byte-string>)
  concatenate ("L", jc.java-class-name.the-string, ";")
end;


// things we reference but don't generate
define abstract class <java-stub-class-or-interface> (<java-class-or-interface>)
end;

define class <java-stub-class> (<java-class>, <java-stub-class-or-interface>)
end;

define class <java-stub-interface> (<java-interface>, <java-stub-class-or-interface>)
end;

format-out ("java-types.dylan point 1.2\n");

define constant $java-pack$ = java-package ("java", super: $java-default-package$);
define constant $java-lang-pack$ = java-package ("lang", super: $java-pack$);
define constant $java-io-pack$   = java-package ("io", super: $java-pack$);
define constant $java-util-pack$ = java-package ("util", super: $java-pack$);
format-out ("java-types.dylan point 1.24\n");

define function java-lang-class (classname :: <byte-string>,
                                 super :: false-or (<java-stub-class>),
                                 #key interfaces = #[]) =>
    (cls :: <java-stub-class>)
  make (<java-stub-class>,
        class-name:   classname,
        package:      $java-lang-pack$,
        super:        super,
        interfaces:   interfaces)
end;

define constant $java/lang/Object$ = java-lang-class ("Object", #f);

define constant $java/lang/Void$    = java-lang-class ("Void",    $java/lang/Object$);
define constant $java/lang/Integer$ = java-lang-class ("Integer", $java/lang/Object$);
define constant $java/lang/Character$ = java-lang-class ("Character", $java/lang/Object$);
define constant $java/lang/Byte$    = java-lang-class ("Byte",    $java/lang/Object$);
define constant $java/lang/Boolean$ = java-lang-class ("Boolean",    $java/lang/Object$);
define constant $java/lang/Short$    = java-lang-class ("Short",    $java/lang/Object$);
define constant $java/lang/Long$    = java-lang-class ("Long",    $java/lang/Object$);
define constant $java/lang/Float$   = java-lang-class ("Float",   $java/lang/Object$);
define constant $java/lang/Double$  = java-lang-class ("Double",  $java/lang/Object$);

// these are a problem, because we have to ensure new cached-sigs
// are created after the end of one library
define constant $java-void-type$ :: <java-primitive-type>  =
  make (<java-primitive-type>, sig: "V", reflected-class: $java/lang/Void$, words: 0, j-code-for: j-void-code);
define constant $java-int-type$  :: <java-primitive-type>  =
  make (<java-primitive-type>, sig: "I", reflected-class: $java/lang/Integer$,        j-code-for: j-int-code);
define constant $java-char-type$ :: <java-primitive-type>  =
  make (<java-primitive-type>, sig: "C", reflected-class: $java/lang/Character$,      j-code-for: j-int-code);
define constant $java-byte-type$ :: <java-primitive-type>  =
  make (<java-primitive-type>, sig: "B", reflected-class: $java/lang/Byte$,           j-code-for: j-int-code);
define constant $java-bool-type$ :: <java-primitive-type>  =
  make (<java-primitive-type>, sig: "Z", reflected-class: $java/lang/Boolean$,        j-code-for: j-int-code);
define constant $java-short-type$ :: <java-primitive-type>  =
  make (<java-primitive-type>, sig: "S", reflected-class: $java/lang/Short$,          j-code-for: j-int-code);
define constant $java-long-type$ :: <java-primitive-type>  =
  make (<java-primitive-type>, sig: "J", reflected-class: $java/lang/Long$, words: 2, j-code-for: j-long-code);
define constant $java-float-type$ :: <java-primitive-type> =
  make (<java-primitive-type>, sig: "F", reflected-class: $java/lang/Float$,          j-code-for: j-float-code);
define constant $java-double-type$ :: <java-primitive-type> =
  make (<java-primitive-type>, sig: "D", reflected-class: $java/lang/Double$, words: 2, j-code-for: j-double-code);

// these are used for stack modeling only
define constant $java-long-low-fragment$ :: <java-primitive-fragment-type> =
  make (<java-primitive-fragment-type>, sig: "J", reflected-class: $java/lang/Long$, j-code-for: j-long-code, which-fragment: 0);
define constant $java-long-high-fragment$ :: <java-primitive-fragment-type> =
  make (<java-primitive-fragment-type>, sig: "J", reflected-class: $java/lang/Long$, j-code-for: j-long-code, which-fragment: 1);
define constant $java-double-low-fragment$ :: <java-primitive-fragment-type> =
  make (<java-primitive-fragment-type>, sig: "D", reflected-class: $java/lang/Double$, j-code-for: j-double-code, which-fragment: 0);
define constant $java-double-high-fragment$ :: <java-primitive-fragment-type> =
  make (<java-primitive-fragment-type>, sig: "D", reflected-class: $java/lang/Double$, j-code-for: j-double-code, which-fragment: 1);


define generic double-type-halves (jtype :: <java-primitive-type>) =>
    (low :: <java-primitive-fragment-type>,
     high :: <java-primitive-fragment-type>);

define method double-type-halves (jtype == $java-long-type$) =>
    (low :: <java-primitive-fragment-type>,
     high :: <java-primitive-fragment-type>)
  values ($java-long-low-fragment$, $java-long-high-fragment$)
end;

define method double-type-halves (jtype == $java-double-type$) =>
    (low :: <java-primitive-fragment-type>,
     high :: <java-primitive-fragment-type>)
  values ($java-double-low-fragment$, $java-double-high-fragment$)
end;


format-out ("java-types.dylan point 2\n");

define function meth-type (result :: <java-type>, #rest args :: <java-type>)
  make (<java-function-type>, result-type: result, arg-types: as (<list>, args))
end;

define function array-type (element-type :: <java-type>)
  make (<java-array-type>, element-type: element-type)
end;



// equivalence of java types - basically have to deal with array constructed types,
// because they are not interned (a mistake?)
define function java-type-equivalent? (type1 :: <java-type>, type2 :: <java-type>) => (equivalent? :: <boolean>)
  if (type1 == type2)
    #t
  else
    java-type-equivalent-2? (type1, type2)
  end
end;

define sealed generic java-type-equivalent-2? (type1 :: <java-type>, type2 :: <java-type>) => (equivalent? :: <boolean>);

// default case - different
define method java-type-equivalent-2? (type1 :: <java-type>, type2 :: <java-type>) => (equivalent? :: <boolean>)
  #f
end;

define method java-type-equivalent-2? (type1 :: <java-any-type>, type2 :: <java-any-type>) => (equivalent? :: <boolean>)
  #t
end;
define method java-type-equivalent-2? (type1 :: <java-null-type>, type2 :: <java-null-type>) => (equivalent? :: <boolean>)
  #t
end;

define method java-type-equivalent-2? (type1 :: <java-array-type>, type2 :: <java-array-type>) => (equivalent? :: <boolean>)
  java-type-equivalent? (type1.java-array-element-type, type2.java-array-element-type)
end;


define method java-type-equivalent-2? (type1 :: <java-class-or-interface>, type2 :: <java-class-or-interface>) => (equivalent? :: <boolean>)
  format-out ("classes %s and %s deemed distinct!\n", type1, type2);
  #f
end;

define method java-type-equivalent-2? (type1 :: <java-function-type>, type2 :: <java-function-type>) => (equivalent? :: <boolean>)
  let  args1 = type1.java-function-arg-types;
  let  args2 = type2.java-function-arg-types;
  args1.size = args2.size &
    java-type-equivalent? (type1.java-function-result-type, type2.java-function-result-type) &
    (args1 == args2 | every? (java-type-equivalent?, args1, args2))
end;



// try to resolve two java types together, for the confluence of BBs -
// ie find best common superclass (superinterface)

// this wrapper bottoms us out
define function java-type-merge (type1 :: <java-type>, type2 :: <java-type>) => (type :: <java-type>)
  if (type1 == type2)
    type1
  else
    java-type-merge-2 (type1, type2)
  end
end;

define sealed generic java-type-merge-2 (type1 :: <java-type>, type2 :: <java-type>) => (type :: <java-type>);

define variable *debug-java-types* :: <boolean> = #t;

define method java-type-merge-2 (type1 :: <java-type>, type2 :: <java-type>) => (type :: <java-type>)
  if (*debug-java-types*)
    format-out ("@@@ failed to merge incompatible java-types, backing out to Object\n");
    $java/lang/Object$
  else
    error ("failed to merge java-types, internal Java backend problem?")
  end
end;

define method java-type-merge-2 (type1 :: <java-return-address-type>, type2 :: <java-return-address-type>) => (type :: <java-return-address-type>)
  type1
end;

define method java-type-merge-2 (type1 :: <java-primitive-type>, type2 :: <java-primitive-type>, #next next-method) => (type :: <java-primitive-type>)
  if     (type1 == $java-char-type$ & type2 == $java-int-type$)
    $java-int-type$
  elseif (type2 == $java-char-type$ & type1 == $java-int-type$)
    $java-int-type$
  elseif (type1 == $java-byte-type$ & type2 == $java-int-type$)
    $java-int-type$
  elseif (type2 == $java-byte-type$ & type1 == $java-int-type$)
    $java-int-type$
  elseif (type1 == $java-byte-type$ & type2 == $java-short-type$)
    $java-short-type$
  elseif (type2 == $java-byte-type$ & type1 == $java-short-type$)
    $java-short-type$
  elseif (type1 == $java-short-type$ & type2 == $java-int-type$)
    $java-int-type$
  elseif (type2 == $java-short-type$ & type1 == $java-int-type$)
    $java-int-type$
  else
    next-method ()
  end
end;

define method java-type-merge-2 (type1 :: <java-any-type>, type2 :: <java-any-type>) => (type :: <java-any-type>)
  type1
end;
define method java-type-merge-2 (type1 :: <java-null-type>, type2 :: <java-null-type>) => (type :: <java-null-type>)
  type1
end;


define method java-type-merge-2 (type1 :: <java-reference-type>, type2 :: <java-null-type>) => (type :: <java-reference-type>)
  type1
end;
define method java-type-merge-2 (type1 :: <java-null-type>, type2 :: <java-reference-type>) => (type :: <java-reference-type>)
  type2
end;

define method java-type-merge-2 (type1 :: <java-reference-type>, type2 :: <java-any-type>) => (type :: <java-reference-type>)
  type1
end;
define method java-type-merge-2 (type1 :: <java-any-type>, type2 :: <java-reference-type>) => (type :: <java-reference-type>)
  type2
end;

format-out ("java-types.dylan point 3\n");

define method java-type-merge-2 (type1 :: <java-array-type>, type2 :: <java-array-type>) => (type :: <java-array-type>)
  let  el1 = type1.java-array-element-type;
  let  el2 = type2.java-array-element-type;
  if (java-type-equivalent? (el1, el2))
    type1
  else
    make (<java-array-type>, element-type: java-type-merge (el1, el2))
  end
end;


define method java-type-merge-2 (type1 :: <java-function-type>, type2 :: <java-function-type>, #next next-method) => (type :: <java-function-type>)
  let  args1 = type1.java-function-arg-types;
  let  args2 = type2.java-function-arg-types;
  if (java-type-equivalent? (type1, type2))
    type1
  elseif (args1.size ~= args2.size)
    next-method ()
  else
    make (<java-function-type>,
          result-type: java-type-merge (type1.java-function-result-type, type2.java-function-result-type),
          arg-types:   map (java-type-intersect, args1, args2))
  end
end;

define sealed generic java-type-intersect (type1 :: <java-type>, type2 :: <java-type>) => (type :: <java-type>);

define method java-type-intersect (type1 :: <java-type>, type2 :: <java-type>) => (type :: <java-type>)
  error ("cannot intersect arb. java types")
end;

define method java-type-intersect (type1 :: <java-return-address-type>, type2 :: <java-return-address-type>) => (type :: <java-return-address-type>)
  type1
end;

define method java-type-intersect (type1 :: <java-primitive-type>, type2 :: <java-primitive-type>, #next next-method) => (type :: <java-primitive-type>)
  if (type1 == type2)
    type1
  else
    next-method ()
  end
end;

define method java-type-intersect (type1 :: <java-any-type>, type2 :: <java-any-type>) => (type :: <java-any-type>)
  type1
end;
define method java-type-intersect (type1 :: <java-null-type>, type2 :: <java-null-type>) => (type :: <java-null-type>)
  type1
end;

define method java-type-intersect (type1 :: <java-null-type>, type2 :: <java-reference-type>) => (type :: <java-null-type>)
  type1
end;
define method java-type-intersect (type1 :: <java-reference-type>, type2 :: <java-null-type>) => (type :: <java-null-type>)
  type2
end;
define method java-type-intersect (type1 :: <java-reference-type>, type2 :: <java-any-type>) => (type :: <java-reference-type>)
  type1
end;
define method java-type-intersect (type1 :: <java-any-type>, type2 :: <java-reference-type>) => (type :: <java-reference-type>)
  type2
end;

define method java-type-intersect (type1 :: <java-array-type>, type2 :: <java-array-type>) => (type :: <java-array-type>)
  let  el1 = type1.java-array-element-type;
  let  el2 = type2.java-array-element-type;
  if (java-type-equivalent? (el1, el2))
    type1
  else
    make (<java-array-type>, element-type: java-type-intersect (el1, el2))
  end
end;

// chicken out of finding LUB
define method java-type-intersect (type1 :: <java-class-or-interface>, type2 :: <java-class-or-interface>, #next next-method) => (type :: <java-class-or-interface>)
  if (type1 == type2)
    type2
  else
    next-method ()
  end
end;

define method java-type-intersect (type1 :: <java-function-type>, type2 :: <java-function-type>, #next next-method) => (type :: <java-function-type>)
  let  args1 = type1.java-function-arg-types;
  let  args2 = type2.java-function-arg-types;
  if (java-type-equivalent? (type1, type2))
    type1
  elseif (args1.size ~= args2.size)
    next-method ()
  else
    make (<java-function-type>,
          result-type: java-type-intersect (type1.java-function-result-type, type2.java-function-result-type),
          arg-types:   map (java-type-merge, args1, args2))
  end
end;




// use the RCPL thing to find best point in the hierarchy
define method java-type-merge-2 (type1 :: <java-class>, type2 :: <java-class>) => (type :: <java-class>)
  let  class1-rcpl = type1.rcpl;
  let  class2-rcpl = type2.rcpl;
  let  n = 1;
  let rcpl-min-size :: <integer> = min (class1-rcpl.size, class2-rcpl.size);
  while (n < rcpl-min-size & class1-rcpl [n] == class2-rcpl [n])
    n := n + 1
  end;
//  if (n < rcpl-min-size)
//    if (*debug-jvm-instrs* == #t)
//      format-out ("@@@ rcpl mismatch  %s, %s\n", class1-rcpl [n], class2-rcpl [n])
//    end
//  end;
  class2-rcpl [n - 1]
end;

define method java-type-merge-2 (type1 :: <java-class>, type2 :: <java-interface>) => (type :: <java-class-or-interface>)
  if (member? (type2, type1.interfaces, test: \==))  // don't we have to search further?
    type1
  else
    $java/lang/Object$
  end
end;

define method java-type-merge-2 (type1 :: <java-interface>, type2 :: <java-class>) => (type :: <java-class-or-interface>)
  java-type-merge-2 (type2, type1)
end;


define method java-type-merge-2 (type1 :: <java-interface>, type2 :: <java-interface>) => (type :: <java-class-or-interface>)
  if (member? (type1, type2.super-interfaces))
    type1
  elseif (member? (type2, type1.super-interfaces))
    type2
  else
    $java/lang/Object$
  end
end;

define method java-type-merge-2 (type1 :: <java-class-or-interface>, type2 :: <java-array-type>) => (type :: <java-class>)
  $java/lang/Object$
end;

define method java-type-merge-2 (type1 :: <java-array-type>, type2 :: <java-class-or-interface>) => (type :: <java-class>)
  $java/lang/Object$
end;



// easy to express assignment compatibility now - must ensure src is not more
// general than dest.

define function assignment-compatible? (src :: <java-type>, dest :: <java-type>) => (compatible? :: <boolean>)
  let  merge = java-type-merge (src, dest);
  let  result = java-type-equivalent? (dest, merge);
  unless (result)
    format-out ("@@@ failed assignment typecheck from %s to %s (merge = %s) - ", src, dest, merge);
  end;
  result
end;

format-out ("inited java-types.dylan\n");
