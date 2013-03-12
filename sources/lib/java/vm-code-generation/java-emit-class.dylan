Module: java-vm-code-generation
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// model a Java class as we output it (most of the outputing is defered till
// the constants pool is complete)


format-out ("initing java-emit-class.dylan\n");

define constant $java-access-public$    = #x0001;
define constant $java-access-private$   = #x0002;
define constant $java-access-protected$ = #x0004;   // not used
define constant $java-access-static$    = #x0008;
define constant $java-access-final$     = #x0010;   // not used
define constant $java-access-sync$      = #x0020;
define constant $java-access-sup$       = #x0020;
define constant $java-access-interface$ = #x0200;
define constant $java-access-abstract$  = #x0400;




define open generic java-class (obj) => (object);

define sealed class <java-slot-spec> (<object>)
  sealed constant slot java-class :: <java-class-or-interface>,   required-init-keyword: java-class:;
  sealed constant slot slot-name :: <unique-string>, required-init-keyword: name:;
  sealed constant slot slot-type :: <java-type>,     required-init-keyword: type:;
  sealed constant slot static? = #f, init-keyword: static?:;
  sealed constant slot public? = #t, init-keyword: public?:;
end;

define class <java-method-spec> (<java-slot-spec>)
  sealed constant slot invoke-op :: <java-call-bytecode>, required-init-keyword: invoke-op:;
end;

/* these need to take large-number of args cases into account? */
define function total-pushes (meth-spec :: <java-method-spec>) => (pushes :: <integer>)
  let  function-type :: <java-function-type> = meth-spec.slot-type;
  let  result :: <java-type> = function-type.java-function-result-type;
  result.java-type-words - total-args (meth-spec)
end;

define function total-args (meth-spec :: <java-method-spec>) => (args :: <integer>)
  let  args = if (meth-spec.invoke-op.pops-instance?) 1 else 0 end;
  let  function-type :: <java-function-type> = meth-spec.slot-type;
  for (arg-type :: <java-type> in function-type.java-function-arg-types)
    args := args + arg-type.java-type-words
  end;
  args
end;

define method pop-list (meth-spec :: <java-method-spec>) => (list :: <list>)
  let  arg-types = meth-spec.slot-type.java-function-arg-types;
  if (meth-spec.invoke-op.pops-instance?)
    arg-types := concatenate (vector (meth-spec.java-class), arg-types)
  end;
  reverse (map (method (tipe :: <java-type>) make (<push-pop-model-typed>, type-constraint: tipe) end,
                arg-types))
end;

define method push-list (meth-spec :: <java-method-spec>) => (list :: <list>)
  let  result-type = meth-spec.slot-type.java-function-result-type;
  if (result-type == $java-void-type$)
    #()
  else
    list (make (<push-pop-model-typed>, type-constraint: result-type))
  end;
end;



define function slot-spec (java-class :: <java-class-or-interface>,
                           name,
                           type :: <java-type>, static?)
                       => (spec  :: <java-slot-spec>)
  make (<java-slot-spec>,
        java-class: java-class,
        name: name.ensure-uniq /* .java-name-generate */,
        type: type,
        static?: static?)
end;

define function meth-spec (java-class :: <java-class-or-interface>,
                           name,
                           type :: <java-function-type>,
                           invoke-op)
                       => (spec :: <java-method-spec>)
//  if (~ instance?(type, <java-function-type>))
//    type.examine
//  end;
  let  static? = (invoke-op == j-invokestatic);
  make (<java-method-spec>,
        java-class: java-class,
        name: name.ensure-uniq /* .java-name-generate */,
        type: type,
        static?: static?,
        invoke-op: invoke-op)
end;



define function java-method (meth-spec :: <java-method-spec>,
                             #key native?       = #f,
                                  synchronized? = #f,
                                  public?       = #t)
 => (meth :: <java-method>)
  let  meth-name = meth-spec.slot-name;
  let  meth-sig  = signature-string (meth-spec.slot-type);
  let  jclass = meth-spec.java-class;
  let  concrete = jclass.concrete-implementation;
  if (concrete)
    let  meth-name-index = java-name-pool-index (meth-name, jclass);
    let  meth-sig-index  = java-name-pool-index (meth-sig, jclass);
    let  meth = any? (method (meth :: <java-method>)
                        if (meth.slot-name = meth-name-index & meth.slot-sig = meth-sig-index)
                          format-out ("###### found duplicate method %s:%s in %s, appending code regardless...\n",
                                      meth-name, meth-sig, jclass);
                          meth
                        end
                      end,
                      concrete.methods);
    unless (meth)
      meth := make (<java-method>,
                    max-locals: meth-spec.total-args,
                    java-class: jclass,
                    name:       meth-name-index,
                    sig:        meth-sig-index,
                    public?:    public?,
                    static?:    meth-spec.static?,
                    native?:    native?,
                    synchronized?: synchronized?,
                    slots-spec: meth-spec);
      concrete.methods := add! (concrete.methods, meth);
    end;
    meth
  else
    format-out ("Warning: java-method for a non-concrete class %s\n", jclass);
    make (<java-method>,
          max-locals: meth-spec.total-args,
          java-class: jclass,
          name:       meth-name,
          sig:        meth-sig,
          public?:    public?,
          static?:    meth-spec.static?,
          synchronized?: synchronized?,
          slots-spec: meth-spec)
   end
end;

define function java-interface-method (meth-spec :: <java-method-spec>,
                                       #key public? = #t)
          => (meth :: <java-abstract-method>)
  let  meth-name = meth-spec.slot-name;
  let  meth-sig  = signature-string (meth-spec.slot-type);
  let  jclass = meth-spec.java-class;
  let  concrete = jclass.concrete-implementation;
  if (concrete)
    let  meth-name-index = java-name-pool-index (meth-name, jclass);
    let  meth-sig-index  = java-name-pool-index (meth-sig, jclass);
    let  meth = any? (method (meth :: <java-method>)
                        if (meth.slot-name = meth-name-index & meth.slot-sig = meth-sig-index)
                          format-out ("###### found duplicate method %s:%s in %s, appending code regardless...\n",
                                      meth-name, meth-sig, jclass);
                          meth
                        end
                      end,
                      concrete.methods);
    unless (meth)
      meth := make (<java-abstract-method>,
                    java-class:  jclass,
                    name:   meth-name-index,
                    sig:    meth-sig-index,
                    public?: public?,
                    static?: meth-spec.static?,
                    slots-spec:   meth-spec);
      concrete.methods := add! (concrete.methods, meth);
    end;
    meth
  else
    make (<java-abstract-method>,
          java-class:  jclass,
          name:   meth-name,
          sig:    meth-sig,
          public?: public?,
          static?: meth-spec.static?,
          slots-spec: meth-spec);
  end
end;

define function my-break (x) => ()
  foo-break(x);
  break (x);
  foo-break(x);
end;

define function foo-break (x) => ()
  if (x == #"Sproing")
    format-out ("prevent optimization away\n")
  end
end;

define function java-field (slot-spec :: <java-slot-spec>, #key public? = #t) => (field :: <java-field>)
  let  name = slot-spec.slot-name;
  let  sig  = signature-string (slot-spec.slot-type);
  let  jclass :: <java-concrete-class> = slot-spec.java-class;
  let  concrete = jclass.concrete-implementation;
  if (concrete)
    let  slot-name-index = java-name-pool-index (name, jclass);
    let  slot-sig-index  = java-name-pool-index (sig, jclass);
    let  slot = any? (method (slot :: <java-field>)
                        if (slot.slot-name = slot-name-index & slot.slot-sig = slot-sig-index)
                          let  acc = slot.access-code;
                          local method
                                    check-flag-matches (bool, flags)
                                  let  flag-set = (logand (acc, flags) = flags);
                                  if ((~bool) == flag-set)
                                    format-out ("###### found duplicate incompatible slot %s:%s in %s\n",
                                                name, sig, jclass);
                                    my-break (slot)
                                  end
                                end;
                          check-flag-matches (slot-spec.static?, $java-access-static$);
                          check-flag-matches (public?,           $java-access-public$);
                          slot
                        end
                      end,
                      concrete.slots);
    unless (slot)
      slot := make (<java-field>,
                    java-class: jclass,
                    name:       slot-name-index,
                    sig:        slot-sig-index,
                    slots-spec: slot-spec);
      let  acc = slot.access-code;
      acc := logior (acc,
                     if (public?)
                       $java-access-public$
                     else
                       $java-access-private$
                     end);
      if (slot-spec.static?)
        acc := logior (acc, $java-access-static$)
      end;
      slot.access-code := acc;
      concrete.slots := add! (concrete.slots, slot);
    end;
    slot

  else
    let slot :: <java-field> = make (<java-field>,
                                     java-class: jclass,
                                     name:       slot-name,
                                     sig:        slot-sig,
                                     slots-spec: slot-spec);
    let  acc = slot.access-code;
    acc := logior (acc,
                   if (public?)
                     $java-access-public$
                   else
                     $java-access-private$
                   end);
    if (slot-spec.static?)
      acc := logior (acc, $java-access-static$)
    end;
    slot.access-code := acc;
    slot
  end
end;


// model those classes we actually generate
//define abstract class <java-concrete-class-or-interface> (<java-class-or-interface>)

define open abstract class <java-concrete-class-or-interface> (<java-concrete>)
  // accumulate constants here
  sealed slot constants = make (<stretchy-vector>, size: 1, fill: #f);
  sealed slot slots     = make (<stretchy-vector>); // of <java-field>
  sealed slot methods   = make (<stretchy-vector>); // of <java-method>
//  sealed slot interfaces = make (<stretchy-vector>); // see mark-as-implementing
  sealed slot attrs     = make (<stretchy-vector>);
  sealed slot access-code = logior ($java-access-public$, $java-access-sup$);
  sealed constant slot outstream, init-keyword: outstream:, init-value: *standard-output*;
  sealed slot code-index = #f;
  sealed slot constants-hash :: <object-table> = make (<object-table>);
  sealed slot been-emitted? :: <boolean> = #f;

/*
  sealed slot symbol-slots-list :: <list> = #();  // pairs of string/slot-spec
  sealed slot been-inited? :: <boolean> = #f;
  sealed constant slot library, required-init-keyword: library:;
  sealed slot ep-seqnum :: <integer> = 0;
  sealed slot iep-emitted? = #f;
  sealed slot mep-emitted? = #f;
  sealed slot xep-emitted? = #f;
*/
end;

define function mark-as-implementing (cls :: <java-class>, inter :: <java-interface>)
  add! (cls.interfaces, make (<java-class-constant>, java-class: inter))
end;

define class <java-concrete-class> (<java-class>) end;
define class <java-concrete-interface> (<java-interface>) end;

//define class <java-concrete-class> (<java-class>, <java-concrete-class-or-interface>)
//  sealed slot interfaces = make (<stretchy-vector>); // see mark-as-implementing
//end;
//
//define class <java-concrete-interface> (<java-interface>, <java-concrete-class-or-interface>)
//  inherited slot access-code = logior ($java-access-public$, $java-access-interface$);
//end;

define open class <java-concrete-class-info> (<java-concrete-class-or-interface>)
/*  sealed*/open slot interfaces :: <sequence> = make (<stretchy-vector>); // see mark-as-implementing
end;


define open class <java-concrete-interface-info> (<java-concrete-class-or-interface>)
  inherited slot access-code = logior ($java-access-public$, $java-access-interface$);
end;


define generic standard-concrete-class-for (jcls :: <java-class-or-interface>) => (cls :: <class>);

define method standard-concrete-class-for (jcls :: <java-class>) => (cls :: <class>)
  <java-concrete-class-info>
end;
define method standard-concrete-class-for (jcls :: <java-interface>) => (cls :: <class>)
  <java-concrete-interface-info>
end;


define function ensure-class-concrete (cls :: <java-class-or-interface>,
                                       #key class-for :: <function> = standard-concrete-class-for)
 => (cls :: <java-class-or-interface>)
  unless (cls.concrete-implementation)
    upgrade-class-to-concrete (cls, class-for)
  end;
  cls
end;

define generic upgrade-class-to-concrete (cls :: <java-class-or-interface>, class-for :: <function>) => ();

define method upgrade-class-to-concrete (cls :: <java-class-or-interface>, class-for :: <function>) => ()
  // all concrete classes should be queued for output
  java-emit-class (cls)
end;


define method upgrade-class-to-concrete (cls :: <java-class>, class-for :: <function>) => ()
  cls.concrete-implementation := make (cls.class-for, library: #f, class-or-interface: cls);
  next-method ()
end;

define method upgrade-class-to-concrete (cls :: <java-interface>, class-for :: <function>) => ()
  cls.concrete-implementation := make (cls.class-for, library: #f, class-or-interface: cls);
  next-method ()
end;


define sealed abstract class <java-constant> (<object>)
end;

define sealed generic type-byte (con :: <java-constant>) => (byte :: <integer>);
//// temp hack:
//define method type-byte-setter (foo :: <integer>, con :: <java-constant>) => (byte :: <integer>) foo end;

define sealed generic con-size  (con :: <java-constant>) => (size :: <integer>);

define method type-byte (con :: <java-constant>) => (byte :: <integer>) 0 end;
define method con-size (con :: <java-constant>) => (size :: <integer>) 1 end;


// used to find existing matching constant
define sealed generic same-java-constant (o1 :: <java-constant>, o2 :: <java-constant>) => (same? :: <boolean>);


// this matches same-java-constant
define sealed generic jcon-hash (jcon :: <java-constant>) => (hash :: <integer>);



define function pool-index (thing :: <java-constant>, concrete :: <java-concrete-class-or-interface>)
  block (return)
    let  cons-hash = concrete.constants-hash;
    let  hashval = jcon-hash (thing);

// new stuff = hash to a list of indices into the real stretchy-vector
    let  constants-vec = concrete.constants;
    let  cons :: <list> = element (cons-hash, hashval, default: #());

    for (ind :: <integer> in cons)
      let  cand = constants-vec [ind];
      if (same-java-constant (cand, thing))
        return (ind)
      end
    end;
    let  copy = pool-copy (thing, concrete);

    let  new-index = constants-vec.size;
    add! (constants-vec, copy);
    if (thing.con-size = 2)
      add! (constants-vec, #f);
    end;
    cons-hash [hashval] := pair (new-index, cons);
    new-index
  end;
end;


// default method - probably only called when of different types,
// so really returns #f

define method same-java-constant (o1 :: <java-constant>, o2 :: <java-constant>) => (same? :: <boolean>)
  o1 == o2
end;

define sealed generic constant-javatype (con :: <java-constant>) => (res :: <java-type>);


define class <java-int-constant> (<java-constant>)
  sealed constant slot value :: <integer>, init-keyword: value:;
end;


define method constant-javatype (con :: <java-int-constant>) => (res :: <java-type>)
  $java-int-type$
end;

define method type-byte (con :: <java-int-constant>) => (byte :: <integer>) 3 end;

//define method initialize (con :: <java-int-constant>, #key) => ()
//  next-method ();
////  con.type-byte := 3
//end;

define method print-object (int :: <java-int-constant>, stream :: <stream>) => ()
  format (stream, "INT(%s)", int.value)
end;

define method same-java-constant (o1 :: <java-int-constant>, o2 :: <java-int-constant>) => (same? :: <boolean>)
  o1.value = o2.value
end;

define method jcon-hash (jcon :: <java-int-constant>) => (hash :: <integer>)
  jcon.value
end;


define sealed generic print-jcon (o :: <java-constant>, cons :: <stretchy-vector>) => (res :: <byte-string>);

define method print-jcon (o :: <java-int-constant>, cons :: <stretchy-vector>) => (res :: <byte-string>)
  let val :: <integer> = o.value;
  format-to-string ("%d", val)
end;


define class <java-long-constant> (<java-constant>)
  sealed constant slot value :: <integer>, init-keyword: value:;
end;

define method constant-javatype (con :: <java-long-constant>) => (res :: <java-type>)
  $java-long-type$
end;

define method type-byte (con :: <java-long-constant>) => (byte :: <integer>) 5 end;
define method con-size  (con :: <java-long-constant>) => (size :: <integer>) 2 end;

//define method initialize (con :: <java-long-constant>, #key) => ()
//  next-method ();
////  con.type-byte := 5;
////  con.con-size := 2
//end;


define method print-object (long :: <java-long-constant>, stream :: <stream>) => ()
  format (stream, "LONG(%s)", long.value)
end;

define method same-java-constant (o1 :: <java-long-constant>, o2 :: <java-long-constant>) => (same? :: <boolean>)
  o1.value = o2.value
end;

define method jcon-hash (jcon :: <java-long-constant>) => (hash :: <integer>)
  jcon.value
end;

define method print-jcon (o :: <java-long-constant>, cons :: <stretchy-vector>) => (res :: <byte-string>)
  let val :: <integer> = o.value;
  format-to-string ("%d", val)
end;

define class <java-float-constant> (<java-constant>)
  sealed constant slot value :: <single-float>, init-keyword: value:;
end;

define method constant-javatype (con :: <java-float-constant>) => (res :: <java-type>)
  $java-float-type$
end;

define method type-byte (con :: <java-float-constant>) => (byte :: <integer>) 4 end;

//define method initialize (con :: <java-float-constant>, #key) => ()
//  next-method ();
////  con.type-byte := 4
//end;

define method same-java-constant (o1 :: <java-float-constant>, o2 :: <java-float-constant>) => (same? :: <boolean>)
  o1.value = o2.value
end;

//define function get-second-value (function :: <function>, value :: <object>) => (res)
//  let  (a, b) = function (value);
//  b
//end;

//define constant $dummy-hash-state$ = get-second-value (object-hash, 0);

define method jcon-hash (jcon :: <java-float-constant>) => (hash :: <integer>)
//  values (object-hash (jcon.value, $dummy-hash-state$))
  truncate/ (round (jcon.value), #xffffff)
end;

define method print-jcon (o :: <java-float-constant>, cons :: <stretchy-vector>) => (res :: <byte-string>)
  let val :: <single-float> = o.value;
  format-to-string ("%s", val)
end;


define class <java-double-constant> (<java-constant>)
  sealed constant slot value :: <double-float>, init-keyword: value:;
end;

define method constant-javatype (con :: <java-double-constant>) => (res :: <java-type>)
  $java-double-type$
end;

define method type-byte (con :: <java-double-constant>) => (byte :: <integer>) 6 end;
define method con-size  (con :: <java-double-constant>) => (size :: <integer>) 2 end;

//define method initialize (con :: <java-double-constant>, #key) => ()
//  next-method ();
////  con.type-byte := 6;
////  con.con-size := 2
//end;


define method same-java-constant (o1 :: <java-double-constant>, o2 :: <java-double-constant>) => (same? :: <boolean>)
  o1.value = o2.value
end;

define method jcon-hash (jcon :: <java-double-constant>) => (hash :: <integer>)
//  values (object-hash (jcon.value, $dummy-hash-state$))
  truncate/ (round (jcon.value), #xffffff)
end;

define method print-jcon (o :: <java-double-constant>, cons :: <stretchy-vector>) => (res :: <byte-string>)
  let val :: <double-float> = o.value;
  format-to-string ("%s", val)
end;





define class <java-utf-constant> (<java-constant>)
  sealed constant slot string :: <unique-string>, init-keyword: string:;
end;

define method type-byte (con :: <java-utf-constant>) => (byte :: <integer>) 1 end;

//define method initialize (con :: <java-utf-constant>, #key) => ()
//  next-method ();
////  con.type-byte := 1
//end;

define method print-object (utf :: <java-utf-constant>, stream :: <stream>) => ()
  format (stream, "UTF(%s)", utf.string)
end;

define method same-java-constant (o1 :: <java-utf-constant>, o2 :: <java-utf-constant>) => (same? :: <boolean>)
  let  s1 = o1.string;
  let  s2 = o2.string;
  if (s1 == s2)
    #t
  else
    let  tag1 = s1.unique-tag;
    let  tag2 = s2.unique-tag;
    if (tag1 = -1 | tag2 = -1)
      format-out ("revoked unique strings picked up by same-java-constant\n");
      my-break (o1)
    end;
    if (tag1 = tag2 & s1 ~= s2)
      format-out ("tags same for different strings %s %s\n", s1, s2);
      my-break (o1);
    end;
    tag1 = tag2
  end
end;



define sealed generic jcon-string-hash (str) => (hash :: <integer>);

define method jcon-string-hash (str :: <byte-string>) => (hash :: <integer>)
  format-out ("unexpected hashing on byte-string %s\n", str);
  jcon-string-hash (str.uniq)
end;

define method  jcon-string-hash (str :: <unique-string>) => (hash :: <integer>)
  str.unique-tag
end;

define method jcon-hash (jcon :: <java-utf-constant>) => (hash :: <integer>)
//  jcon.hash-cache |
//    (jcon.hash-cache := jcon-string-hash (jcon.string))
  jcon.string.unique-tag
end;


define method print-jcon (o :: <java-utf-constant>, cons :: <stretchy-vector>) => (res :: <byte-string>)
  o.string.the-string
end;


define sealed generic pool-copy (o :: <java-constant>,
                                 concrete :: <java-concrete-class-or-interface>) =>
    (c :: <java-constant>);

// default doesn't copy for unstructured constants (utf, int, etc)
define method pool-copy (o :: <java-constant>, concrete :: <java-concrete-class-or-interface>)
 => (con :: <java-constant>)
  o
end;




define class <java-string-constant> (<java-constant>)
  sealed constant slot utf :: <java-utf-constant>, required-init-keyword: utf:;
  sealed slot utf-index :: <integer> = -1, init-keyword: utf-index:;
//  slot hash-cache :: false-or (<integer>) = #f;
end;

define method constant-javatype (con :: <java-string-constant>) => (res :: <java-type>)
  $java/lang/String$
end;

define method type-byte (con :: <java-string-constant>) => (byte :: <integer>) 8 end;

//define method initialize (con :: <java-string-constant>, #key) => ()
//  next-method ();
////  con.type-byte := 8
//end;


define method print-object (str :: <java-string-constant>, stream :: <stream>) => ()
  format (stream, "STR(%s)", str.utf.string.the-string)
end;

define method same-java-constant (o1 :: <java-string-constant>, o2 :: <java-string-constant>) => (same? :: <boolean>)
  same-java-constant (o1.utf, o2.utf)
end;

define method jcon-hash (jcon :: <java-string-constant>) => (hash :: <integer>)
//  jcon.hash-cache |
//    (jcon.hash-cache := 357 + jcon-hash (jcon.utf))
  357 + jcon-hash (jcon.utf)
end;


define method print-jcon (o :: <java-string-constant>, cons :: <stretchy-vector>) => (res :: <byte-string>)
  print-jcon (cons[o.utf-index], cons)
end;

define method pool-copy (o :: <java-string-constant>, concrete :: <java-concrete-class-or-interface>)
 => (con :: <java-constant>)
  let  utf-index = pool-index (o.utf, concrete);
  make (<java-string-constant>, utf: o.utf, utf-index: utf-index)
end;



// can be an array class too, note
define class <java-class-constant> (<java-constant>)
  sealed constant slot java-class :: <java-type>, required-init-keyword: java-class:;
  sealed slot java-class-index :: <integer> = -1, init-keyword: java-class-index:;
//  slot hash-cache :: false-or (<integer>) = #f;
end;

define method constant-javatype (con :: <java-class-constant>) => (res :: <java-type>)
  con.java-class
end;

define method type-byte (con :: <java-class-constant>) => (byte :: <integer>) 7 end;

//define method initialize (con :: <java-class-constant>, #key) => ()
//  next-method ();
////  con.type-byte := 7
//end;

define method print-object (cls :: <java-class-constant>, stream :: <stream>) => ()
  format (stream, "CLASS(%s)", cls.java-class)
end;

define method same-java-constant (o1 :: <java-class-constant>, o2 :: <java-class-constant>) => (same? :: <boolean>)
  if (o1.java-class ~== o2.java-class &
      o1.java-class.java-class-name == o2.java-class.java-class-name)
    format-out ("@@@ two identically named classes! %s", o1);
    // my-break (pair (o1,o2));
    #t
  else
    o1.java-class == o2.java-class  // maybe should check names?
  end
end;

define method jcon-hash (jcon :: <java-class-constant>) => (hash :: <integer>)
  let  clz = jcon.java-class;
  439 + clz.java-class-name.unique-tag
end;

define method print-jcon (o :: <java-class-constant>, cons :: <stretchy-vector>) => (res :: <byte-string>)
  print-jcon (cons[o.java-class-index], cons)
end;

define method pool-copy (o :: <java-class-constant>,
                         concrete :: <java-concrete-class-or-interface>)
 => (con :: <java-constant>)
  let  utf-constant = make (<java-utf-constant>, string: o.java-class.java-class-name);
  make (<java-class-constant>,
        java-class: o.java-class,
        java-class-index: pool-index (utf-constant, concrete))
end;



define class <java-nat-constant> (<java-constant>)
  sealed constant slot  nat-name :: <unique-string>, required-init-keyword: name:;
  sealed constant slot  nat-type :: <java-type>,     required-init-keyword: type:;
  sealed slot  nat-name-index :: <integer> = -1, init-keyword: name-index:;
  sealed slot  nat-type-index :: <integer> = -1, init-keyword: type-index:;
  sealed slot  hash-cache :: false-or (<integer>) = #f;
end;

define method constant-javatype (con :: <java-nat-constant>) => (res :: <java-type>)
  con.nat-type
end;

define method type-byte (con :: <java-nat-constant>) => (byte :: <integer>) 12 end;



define method print-object (nat :: <java-nat-constant>, stream :: <stream>) => ()
  format (stream, "%s :: %s", nat.nat-name.the-string, nat.nat-type)
end;

define method same-java-constant (o1 :: <java-nat-constant>, o2 :: <java-nat-constant>) => (same? :: <boolean>)
  if (o1.nat-name ~== o2.nat-name &
      o1.nat-name.the-string = o2.nat-name.the-string)
    format-out ("@@ bad uniques for same-java-constant\n");
    my-break (pair (o1,o2))
  end;
  (o1.nat-name == o2.nat-name) &
  java-type-equivalent? (o1.nat-type, o2.nat-type)
end;

define method jcon-hash (jcon :: <java-nat-constant>) => (hash :: <integer>)
  jcon.hash-cache |
    (jcon.hash-cache := 49 + jcon.nat-name.unique-tag + jcon.nat-type.signature-string.unique-tag)
end;

define method print-jcon (o :: <java-nat-constant>, cons :: <stretchy-vector>) => (res :: <byte-string>)
    concatenate (concatenate (print-jcon (cons[o.nat-name-index], cons), ":"),
                     print-jcon (cons[o.nat-type-index], cons))
end;

define method pool-copy (o :: <java-nat-constant>,
                         concrete :: <java-concrete-class-or-interface>)
 => (con :: <java-constant>)
  unless (instance? (o.nat-name, <unique-string>))
    format-out ("WHOOPS! broken java nat(name=%s type=%s)\n", o.nat-name, o.nat-type);
    break (o)
  end;
  unless (instance? (o.nat-type, <java-type>))
    format-out ("WHOOPS! broken java nat(name=%s type=%s)\n", o.nat-name, o.nat-type)
  end;
  let  name-index = pool-index (make (<java-utf-constant>, string: o.nat-name), concrete);
  let  type-index = pool-index (make (<java-utf-constant>, string: o.nat-type.signature-string), concrete);
  make (<java-nat-constant>,
        name: o.nat-name, name-index: name-index,
        type: o.nat-type, type-index: type-index)
end;


define class <java-slot-constant> (<java-constant>)
  sealed constant slot  java-class :: <java-class-constant>, required-init-keyword: java-class:;
  sealed constant slot  nat :: <java-nat-constant>,   required-init-keyword: nat:;
  sealed slot  java-class-index :: <integer> = -1, init-keyword: java-class-index:;
  sealed slot  nat-index :: <integer> = -1,   init-keyword: nat-index:;
  sealed slot hash-cache :: false-or (<integer>) = #f;
end;

define method constant-javatype (con :: <java-slot-constant>) => (res :: <java-type>)
  constant-javatype (con.nat)
end;


define method type-byte (con :: <java-slot-constant>) => (byte :: <integer>) 9 end;

define method print-object (slot :: <java-slot-constant>, stream :: <stream>) => ()
  let  jc = slot.java-class.java-class;
  format (stream, "SLOT(%s.%s.%s)", jc.class-package, jc.class-name, slot.nat)
end;


define method same-java-constant (o1 :: <java-slot-constant>, o2 :: <java-slot-constant>) => (same? :: <boolean>)
//  if (o1.object-class == o2.object-class)
//    format-out ("@@ in same-java-constant for slot, %s, %s\n",
//                same-java-constant (o1.java-class, o2.java-class),
//                same-java-constant (o1.nat,   o2.nat))
//  end;
  (o1.object-class == o2.object-class) &  // methods and fields are different type bytes
  same-java-constant (o1.java-class, o2.java-class) &
  same-java-constant (o1.nat,   o2.nat)
end;

define method jcon-hash (jcon :: <java-slot-constant>) => (hash :: <integer>)
  jcon.hash-cache |
    (jcon.hash-cache  :=  49 + jcon-hash (jcon.java-class) + jcon-hash (jcon.nat))
end;

define method print-jcon (o :: <java-slot-constant>, cons :: <stretchy-vector>) => (res :: <byte-string>)
  concatenate (concatenate (print-jcon (cons[o.java-class-index], cons),
                            "."),
               print-jcon (cons[o.nat-index], cons));
end;

define method pool-copy (o :: <java-slot-constant>,
                         concrete :: <java-concrete-class-or-interface>)
 => (con :: <java-constant>)
  let  class-index = pool-index (o.java-class, concrete);
  let  nat-index   = pool-index (o.nat, concrete);
  make (o.object-class,
        java-class: o.java-class, java-class-index: class-index,
        nat:   o.nat,   nat-index:   nat-index)
end;



define class <java-meth-constant> (<java-slot-constant>)
end;


define method type-byte (con :: <java-meth-constant>) => (byte :: <integer>) 10 end;

//define method initialize (con :: <java-meth-constant>, #key) => ()
//  next-method ();
////  con.type-byte := 10
//end;

define method print-object (slot :: <java-meth-constant>, stream :: <stream>) => ()
  let  jc = slot.java-class.java-class;
  format (stream, "METH(%s.%s.%s)", jc.class-package, jc.class-name, slot.nat)
end;



define class <java-inter-meth-constant> (<java-meth-constant>)
end;

define method type-byte (con :: <java-inter-meth-constant>) => (byte :: <integer>) 11 end;

//define method initialize (con :: <java-inter-meth-constant>, #key) => ()
//  next-method ();
////  con.type-byte := 11
//end;


define method print-object (slot :: <java-inter-meth-constant>, stream :: <stream>) => ()
  format (stream, "IMETH(%s,%s)", slot.java-class, slot.nat)
end;





define function slot-not-already-present (jc :: <java-concrete-class>, name :: <string>)
  let  concrete = jc.concrete-implementation;
  let  name-index = java-name-pool-index (name.uniq, concrete);
  block (return)
    for (slot :: <java-field> in concrete.slots)
      if (slot.slot-name == name-index)
        return (#f)
      end
    end;
    #t
  end
end;





define function java-io-class (classname :: <byte-string>, super :: <java-stub-class>) => (cls :: <java-stub-class>)
  make (<java-stub-class>,
        class-name: classname,
        package:    $java-io-pack$,
        super:      super)
end;

// not used?
define constant $java-lang-reflect-pack$ = java-package ("reflect", super: $java-lang-pack$);

define function java-reflect-class (classname :: <byte-string>, super :: <java-stub-class>) => (cls :: <java-stub-class>)
  make (<java-stub-class>,
        class-name: classname,
        package:    $java-lang-reflect-pack$,
        super:      super)
end;

define constant $java/lang/Object-array$ =  $java/lang/Object$.array-type;

define constant $java/lang/String$ =    java-lang-class ("String", $java/lang/Object$);
define constant $java/lang/Math$   =    java-lang-class ("Math",   $java/lang/Object$);

// this should implement Runnable
define constant $java/lang/Thread$ =    java-lang-class ("Thread", $java/lang/Object$);

// already in java-opcodes:
// define constant $java/lang/Throwable$ = java-lang-class ("Throwable", $java/lang/Object$);

define constant $java/lang/Exception$ = java-lang-class ("Exception", $java/lang/Throwable$);
define constant $java/lang/RuntimeException$ = java-lang-class ("RuntimeException", $java/lang/Exception$);
define constant $java/lang/ClassCastException$ = java-lang-class ("ClassCastException", $java/lang/RuntimeException$);

define constant $java/lang/System$ = java-lang-class ("System", $java/lang/Object$);


// this for when don't want a Java String, but a Utf8 thing (typically
// a name of a method class or signature)
define function java-name-rep (s :: <unique-string>) => (thing)
  make (<java-utf-constant>, string: s)
end;

define function java-name-pool-index (o, jclass :: <java-class-or-interface>) => (index :: <integer>)
  let  concrete = jclass.concrete-implementation;
  pool-index (java-name-rep (o), concrete)
end;





define function write2 (s, i :: <integer>)
  write-element (s, as (<character>, logand (#xff, ash (i, -8))));
  write-element (s, as (<character>, logand (#xff, i)));
end;

define function write4 (s, i :: <integer>)
  write-element (s, as (<character>, logand (#xff, ash (i, -24))));
  write-element (s, as (<character>, logand (#xff, ash (i, -16))));
  write-element (s, as (<character>, logand (#xff, ash (i, -8))));
  write-element (s, as (<character>, logand (#xff, i)));
end;



define sealed generic java-emit (s :: <stream>, java-thing) => ();

define method java-emit (s :: <stream>, c :: <java-constant>) => ()
  write-element (s, as (<character>, c.type-byte));
end;

define method java-emit (s :: <stream>, c :: <java-int-constant>) => ()
  next-method ();
  write4 (s, c.value);
end;

define method java-emit (s :: <stream>, c :: <java-float-constant>) => ()
  next-method ();
  // broken
//  write4 (s, c.value);
  write4 (s, #x595959);   // Ought to fix this (or at least use the pattern for 42.0)!!
end;


define method java-emit (s :: <stream>, c :: <java-utf-constant>) => ()
  next-method ();
  let  str = c.string.the-string;
  write2 (s, str.size);
//  for (ch in str)
//    write-element (s, ch);
//  end
  write (s, str)
end;

define method java-emit (s :: <stream>, c :: <java-string-constant>) => ()
  next-method ();
  write2 (s, c.utf-index);
end;

define method java-emit (s :: <stream>, c :: <java-class-constant>) => ()
  next-method ();
  write2 (s, c.java-class-index);
end;

define method java-emit (s :: <stream>, c :: <java-slot-constant>) => ()
  next-method ();
  write2 (s, c.java-class-index);
  write2 (s, c.nat-index);
end;

define method java-emit (s :: <stream>, c :: <java-nat-constant>) => ()
  next-method ();
  write2 (s, c.nat-name-index);
  write2 (s, c.nat-type-index);
end;



define sealed abstract class <java-slot> (<object>)
  sealed constant slot java-class :: <java-concrete-class>, required-init-keyword: java-class:;
  sealed slot public? = #t, init-keyword: public?:;
  sealed slot static? = #f, init-keyword: static?:;
  sealed slot access-code :: <integer> = #x0000; // other access bits
  sealed constant slot slot-name :: <integer>, required-init-keyword: name:;   // this is constants-pool offset
  sealed constant slot slot-sig  :: <integer>, required-init-keyword: sig:;     // this is constants-pool offset
  sealed constant slot slots-spec :: <java-slot-spec>, required-init-keyword: slots-spec:;
end;

define method initialize (js :: <java-slot>, #key)
  next-method();
  let  acc = js.access-code;
  acc := logior (acc, if (js.public?) $java-access-public$ else $java-access-private$ end);
  if (js.static?) acc := logior (acc, $java-access-static$) end;
  js.access-code := acc
end;

define method print-object (slot :: <java-slot>, stream :: <stream>) => ()
  format (stream, "{java-slot %s}", slot.slots-spec.slot-name)
end;

define class <java-abstract-method> (<java-slot>)
end;

define method initialize (jam :: <java-abstract-method>, #key)
  next-method();
  jam.access-code := logior (jam.access-code, $java-access-abstract$)
end;


define abstract sealed class <java-code> (<object>)
  slot  pc         :: <integer> = 0, init-keyword: pc:;
  slot  max-stack  :: <integer> = 0, init-keyword: max-stack:;
  slot  max-locals :: <integer> = 0, init-keyword: max-locals:;
end;

define class <java-method> (<java-slot>, <java-code>)
  sealed slot excep-table = make (<stretchy-vector>);  // not used yet
  sealed slot basic-blocks = make (<stretchy-vector>); // of <java-basic-block>
  sealed slot bb-list :: <list> = #();          // the sequence of raw bbs, for label resolution
  sealed constant slot finally-handlers :: <object-table> = make (<object-table>);
  sealed slot synchronized? :: <boolean> = #f, init-keyword: synchronized?:;
  sealed slot native? :: <boolean> = #f, init-keyword: native?:;
  sealed constant slot label-table :: <object-table> = make (<object-table>);
end;

define method initialize (jm :: <java-method>, #key)
  next-method();
  if (jm.synchronized?)
    jm.access-code := logior (jm.access-code, $java-access-sync$)
  end
end;

define method print-object (slot :: <java-method>, stream :: <stream>) => ()
  format (stream, "{java-method %s}", slot.slots-spec.slot-name)
end;


define class <java-field> (<java-slot>)
end;

// not sure these should be constant?
define sealed class <java-exception-entry> (<object>)
  sealed constant slot start-pc :: <integer>, init-keyword: start-pc:;
  sealed constant slot end-pc   :: <integer>, init-keyword: end-pc:;
  sealed constant slot excep-type :: <integer>, init-keyword: excep-type:;
  sealed constant slot excep-pc :: <integer>, init-keyword: excep-pc:;
end;

define method java-emit (s :: <stream>, slot :: <java-slot>) => ()
  let  access =
    logior (slot.access-code,
            if (slot.public?) $java-access-public$ else $java-access-private$ end);
  if (slot.static?) access := logior (access, $java-access-static$) end;
  write2 (s, access);
  write2 (s, slot.slot-name);
  write2 (s, slot.slot-sig)
end;


define variable *max-max-stack* :: <integer> = 0;
define variable *max-max-locals* :: <integer> = 0;


define method java-emit (s :: <stream>, meth :: <java-method>) => ()
  if (meth.synchronized?) meth.access-code := logior (meth.access-code, $java-access-sync$) end;
  next-method();   // do slot stuff
  write2 (s, 1); // one attribute, "Code"
  write2 (s, meth.java-class.concrete-implementation.code-index);
  write4 (s, meth.pc + 12 + 8 * meth.excep-table.size);
  write2 (s, meth.max-stack);
  write2 (s, meth.max-locals);
//format-out (" +%d+%d+\n", meth.max-stack, meth.max-locals);
  *max-max-stack* := max (*max-max-stack*, meth.max-stack);
  *max-max-locals* := max (*max-max-locals*, meth.max-locals);
  write4 (s, meth.pc);
  // new byte-vec for byte codes stuff
  let  byte-vec = make (<byte-vector>, size: meth.pc);
//format-out ("byte vector created size %s\n", meth.pc);
  let  pc = 0;
  for (jbb :: <java-basic-block> in meth.basic-blocks)
//format-out ("  << bb %s\n", pc);
  pc := output-bytecodes (byte-vec, jbb, pc);
//format-out ("  >> bb %s\n", pc);
  end;
  // write the vector, single stream op
  write (s, byte-vec);

  write2 (s, meth.excep-table.size);
  for (excep :: <java-exception-entry> in meth.excep-table)
    write2 (s, excep.start-pc);
    write2 (s, excep.end-pc);
    write2 (s, excep.excep-pc);
    write2 (s, excep.excep-type)
  end;
  write2 (s, 0);   // no other attributes!
end;


define method java-emit (s :: <stream>, meth :: <java-abstract-method>) => ()
  next-method();   // do slot stuff
  write2 (s, 0); // no attributes
end;

define method java-emit (s :: <stream>, meth :: <java-field>) => ()
  next-method();   // do slot stuff
  write2 (s, 0); // no attributes
end;



//define method java-emit (s :: <stream>, cls :: <java-class-or-interface>) => ()
//  error ("attempt to generate a non-concrete Java class\n")
//end;

define constant $SourceFile-name$ = "SourceFile".uniq;
define constant $dummy-file-name$ = "dummy.java".uniq;

define function filename-for-class (cls :: <java-class-or-interface>) => (filename :: <unique-string>)
  concatenate (cls.java-class-name.the-string, ".java").uniq
end;

define method java-emit (s :: <stream>, cls :: <java-class-or-interface>) => ()
  let  concrete = cls.concrete-implementation;
  if (concrete)
    // various preparatory stuff
    unless (concrete.code-index)
      concrete.code-index := java-name-pool-index ($Code-attr-name$, cls)
    end;

    // note write4 is not emulator/native portable, so use write2 etc for now
    //  write4 (s, #xcafebabe);  // magic number for Java class files
    write2 (s, #xcafe);  // magic number for Java class files
    write2 (s, #xbabe);  //
    write2 (s, #x0003);      // minor version number = 3   (JDK 1.0, 1.1)
    write2 (s, #x002d);      // major version number = 45  (JDK 1.0, 1.1)

    let  self = make (<java-class-constant>, java-class: cls);
    let  this = pool-index (self, concrete);
    let  super = if (instance? (cls, <java-interface>))
                   0
                 elseif (cls.super)
                   pool-index (make (<java-class-constant>, java-class: cls.super),
                               concrete)
                 else
                   0
                 end;
    if (instance? (cls, <java-interface>))
      for (ifc :: <java-class-constant> in cls.super-interfaces)
        pool-index (ifc, concrete)
      end
    else
      for (ifc :: <java-class-constant> in cls.interfaces)  // these should be <java-class-constant>s
        pool-index (ifc, concrete);
      end
    end;

    let src-file-tag = java-name-pool-index  ($SourceFile-name$,  cls);
    let src-file-name = java-name-pool-index (filename-for-class (cls), cls);

    // no more constants can be created after here!

    write2 (s, concrete.constants.size);
    let  cons :: <stretchy-vector> = concrete.constants;

    for (con in cons, i :: <integer> from 0)
      unless (zero? (i))
        java-emit (s, con);
      end;
    end;
    write2 (s, concrete.access-code);
    write2 (s, this);
    write2 (s, super);

    if (instance? (cls, <java-interface>))
      write2 (s, cls.super-interfaces.size);
      for (ifc :: <java-class-constant> in cls.super-interfaces)
        write2 (s, pool-index (ifc, concrete))
      end
    else
      write2 (s, cls.interfaces.size);
      for (ifc :: <java-class-constant> in cls.interfaces)
        write2 (s, pool-index (ifc, concrete))
      end
    end;

    write2 (s, concrete.slots.size);
    for (slot :: <java-field> in concrete.slots)
      java-emit (s, slot)
    end;

    write2 (s, concrete.methods.size);

    *max-max-stack* := 0;
    *max-max-locals* := 0;
    for (meth :: <java-method> in concrete.methods)
      java-emit (s, meth)
    end;
    format-out ("..  %d constants, %d, %d max stack, locals\n", cons.size, *max-max-stack*, *max-max-locals*);

    // add a sourcefile attribute
    write2 (s, concrete.attrs.size + 1);
    for (attr in concrete.attrs)
      java-emit (s, attr)
    end;
    write2 (s, src-file-tag);
    write4 (s, 2);  write2 (s, src-file-name);
  end
end;




define constant $Code-attr-name$ = "Code".uniq;



// stuff to implement the jar entry - should
// put the zip stuff in a module of its own, see if we
// already have an API for it.

define class <class-file-jar-entry> (<zip-entry>)
  sealed constant slot java-class :: <java-class-or-interface>, required-init-keyword: class:;
  sealed slot cached-filesize = #f;
  sealed slot cached-crc = #f;
end;
define class <class-string-jar-entry> (<zip-entry>)
  sealed constant slot zstream :: <zip-crc-stream> = make (<zip-crc-stream>, stream: make (<chunk-spool-stream>));
end;

define method zip-details-upfront? (e :: <class-file-jar-entry>) => (upfront? :: <boolean>)
  #f
end;
define method zip-details-upfront? (e :: <class-string-jar-entry>) => (upfront? :: <boolean>)
  #t
end;

define method filesize (e :: <class-file-jar-entry>) => (size :: <integer>)
  e.cached-filesize |
    error ("cannot get filesize for <class-file-jar-entry> - perhaps never wrote it? %s", e)
end;
define method filesize (e :: <class-string-jar-entry>) => (size :: <integer>)
  e.zstream.get-offset
end;

define method filesize-setter (size :: <integer>, e :: <class-file-jar-entry>) => (size :: <integer>)
  e.cached-filesize := size
end;
define method filesize-setter (size :: <integer>, e :: <class-string-jar-entry>) => (size :: <integer>)
  size  // dummy method
end;

define method file-crc (e :: <class-file-jar-entry>) => (crc :: <machine-word>)
  e.cached-crc |
    error ("cannot get file-crc for <class-file-jar-entry>, perhaps not written? %s", e)
end;
define method file-crc (e :: <class-string-jar-entry>) => (crc :: <machine-word>)
  e.zstream.get-crc
end;

define method file-crc-setter (crc :: <machine-word>, e :: <class-file-jar-entry>) => (crc :: <machine-word>)
  e.cached-crc := crc
end;
define method file-crc-setter (crc :: <machine-word>, e :: <class-string-jar-entry>) => (crc :: <machine-word>)
  crc // dummy method
end;


define method writer (e :: <class-file-jar-entry>) => (writer :: <function>)
  method (s :: <stream>)
    java-emit (s, e.java-class)
  end
end;

define method writer (e :: <class-string-jar-entry>) => (writer :: <function>)
  let  raw-stream = e.zstream.the-stream;
  method (s :: <stream>)
    raw-stream.rewind;
    let  count :: <integer> = 0;
    while (available? (raw-stream))
      let  ch = read-element (raw-stream);
      if (ch)
//        format-out ("recopy %d\n", as (<integer>, ch));
        write-element (s, ch);
        count := count + 1
      end
    end;
    format-out ("recopied %d chars\n", count);
  end
end;

//////////////// end of zip file .jar file tailoring



// stuff to track extant java classes and allow the final write-everything-out
// phase.
define variable *the-pending-java-classes* = make (<object-table>);

define sealed generic java-emit-class (jc :: <java-class-or-interface>) => ();

//define method java-emit-class (jc :: <java-class-or-interface>) => ()
//end;

//define method java-emit-class (jc :: <java-concrete-class-or-interface>) => ()
//  unless (jc.been-emitted?)
//    jc.been-emitted? := #t;
//    *the-pending-java-classes* [jc] := #t
//  end
//end;
define method java-emit-class (jc :: <java-class-or-interface>) => ()
  let concrete = jc.concrete-implementation;
  if (concrete)
    unless (concrete.been-emitted?)
      concrete.been-emitted? := #t;
      *the-pending-java-classes* [jc] := #t
    end
  end
end;

//define function java-unemit-class (jc :: <java-concrete-class-or-interface>) => ()
//  *the-pending-java-classes* [jc] := #f;
//  jc.been-emitted? := #f;
//end;
define function java-unemit-class (jc :: <java-class-or-interface>) => ()
  let concrete = jc.concrete-implementation;
  if (concrete & concrete.been-emitted?)
    *the-pending-java-classes* [jc] := #f;
    concrete.been-emitted? := #f;
  end
end;

format-out ("inited java-emit-class.dylan\n");
