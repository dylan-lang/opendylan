Module:   dfmc-modeling
Synopsis: Models for types.
Author:   Keith Playford and Paul Haahr
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The model type protocol (not yet a MOP)

// <type> is defined elsewhere.

// These protocol functions represent compile-time versions of their
// language/run-time equivalents and do not exploit "extra" information
// that the compiler might be able to work out by looking further
// afield.

// Note that disjointness is relative to your viewpoint, and is always
// looked at from the point of view of the current library, since that's
// how definition lookup is arranged. This also goes for ^pseudosubtype?
// which involves a disjointness test.

// Each type must implement:
//
//   ^base-type?
//   ^instance?
//   ^subtype?
//   ^known-disjoint?
//   ^instantiable?

define generic ^instance? (object :: <model-value>, type :: <&type>)
 => (instance? :: <boolean>);

define generic ^subtype? (t1 :: <&type>, t2 :: <&type>)
 => (subtype? :: <boolean>);

// *permissibly-ambiguous-generics* := add(*permissibly-ambiguous-generics*, ^subtype?);


define generic ^base-type (type :: <&type>)
 => (base-type :: <&type>);

define generic ^type-equivalent? (t1 :: <&type>, t2 :: <&type>)
 => (equivalent? :: <boolean>);

define generic ^known-disjoint? (t1 :: <&type>, t2 :: <&type>)
 => (disjoint? :: <boolean>);

// *permissibly-ambiguous-generics* := add(*permissibly-ambiguous-generics*, ^known-disjoint?);

//// Top rules

define method ^subtype? (t1 :: <&top-type>, t2 :: <&top-type>)
 => (subtype? :: <boolean>)
  #t
end method;

define method ^subtype? (t1 :: <&type>, t2 :: <&top-type>)
 => (subtype? :: <boolean>)
  #t
end method;

define method ^subtype? (t1 :: <&top-type>, t2 :: <&type>)
 => (subtype? :: <boolean>)
  #f
end method;

//// Bottom rules

define method ^subtype? (t1 :: <&bottom-type>, t2 :: <&bottom-type>)
 => (subtype? :: <boolean>)
  #t
end method;

define method ^subtype? (t1 :: <&type>, t2 :: <&bottom-type>)
 => (subtype? :: <boolean>)
  #f
end method;

define method ^subtype? (t1 :: <&bottom-type>, t2 :: <&type>)
 => (subtype? :: <boolean>)
  #t
end;

//// Generic implementations.

define method ^instance? (object :: <model-value>, t :: <&type>)
    => (instance? :: <boolean>)
  ^subtype?(&object-class(object), t)
end method;

// Two types are equivalent when ``each ... is a subtype of the other.''
// (DRM 29 Sep 95, page 420).

define method ^type-equivalent?
    (t1 :: <&type>, t2 :: <&type>) => (equivalent? :: <boolean>)
  (t1 == t2) | (^subtype?(t1, t2) & ^subtype?(t2, t1))
end method;

//// Limited types.

define abstract &class <limited-type> (<type>) end &class;

define &class <limited-collection-type> (<limited-type>) 
  constant &slot limited-collection-class :: <class>,
    required-init-keyword: class:;
  constant &slot limited-collection-element-type :: <type>,
    required-init-keyword: element-type:;
  constant &slot limited-collection-concrete-class /* :: <class> */,
    init-value:   #f,
    init-keyword: concrete-class:;
  constant &slot limited-collection-size /* :: false-or(<integer>) */,
    init-value:   #f,
    init-keyword: size:;
  constant &slot limited-collection-dimensions /* :: false-or(<sequence>) */,
    init-value:   #f,
    init-keyword: dimensions:;
end &class;

define &class <limited-explicit-key-collection-type> (<limited-collection-type>) 
end &class;

define &class <limited-mutable-collection-type> (<limited-collection-type>) 
end &class;

define &class <limited-stretchy-collection-type> (<limited-collection-type>) 
end &class;

define &class <limited-mutable-explicit-key-collection-type> 
  (<limited-mutable-collection-type>, <limited-explicit-key-collection-type>) 
end &class;

define &class <limited-sequence-type> (<limited-collection-type>) 
end &class;

define &class <limited-mutable-sequence-type> 
  (<limited-mutable-collection-type>, <limited-sequence-type>) 
end &class;

define &class <limited-array-type> (<limited-mutable-sequence-type>)
end &class;

define &class <limited-vector-type> (<limited-array-type>)
end &class;

define &class <limited-string-type> (<limited-mutable-sequence-type>)
end &class;

define &class <limited-stretchy-vector-type> 
  (<limited-stretchy-collection-type>, <limited-vector-type>)
end &class;

define &class <limited-deque-type> 
  (<limited-stretchy-collection-type>, <limited-mutable-sequence-type>)
end &class;

define &class <limited-table-type> (<limited-mutable-explicit-key-collection-type>) 
end &class;

define &class <limited-set-type> (<limited-mutable-explicit-key-collection-type>) 
end &class;

define method ^instance?-function (t :: <&limited-collection-type>) => (res :: <symbol>)
  #"limited-collection-instance?"
end method;

define method ^subtype? 
    (t1 :: <&limited-collection-type>, t2 :: <&limited-collection-type>) => (result :: <boolean>)
  let s1 = ^limited-collection-size(t1);
  let s2 = ^limited-collection-size(t2);
  let d1 = ^limited-collection-dimensions(t1);
  let d2 = ^limited-collection-dimensions(t2);
  ^subtype?(^limited-collection-class(t1), ^limited-collection-class(t2))
    & ^type-equivalent?(^limited-collection-element-type(t1), ^limited-collection-element-type(t2))
    & (if (~d1 & ~d2)
	 (~s2 | s1 = s2)
       else 
	 (d1 & (~d2 | every?(\=, d1, d2)) & (~s2 | reduce1(\*, d1) = s2))
       end if)
end method;

define method ^subtype? 
    (c :: <&type>, t :: <&limited-collection-type>) => (result :: <boolean>)
  #f
end method;

define method ^subtype? 
    (s :: <&singleton>, t :: <&limited-collection-type>) => (result :: <boolean>)
  ^instance?(^singleton-object(s), t)
end method;

define method ^subtype?
    (u :: <&union>, t :: <&limited-collection-type>) => (well? :: <boolean>)
  ^subtype?(u.^union-type1, t) & ^subtype?(u.^union-type2, t)
end method;

define method ^subtype? 
    (t :: <&limited-collection-type>, c :: <&type>) => (result :: <boolean>)
  #f
end method;

define method ^subtype? 
    (t :: <&limited-collection-type>, c :: <&class>) => (result :: <boolean>)
  ^subtype?(^limited-collection-concrete-class(t) | ^limited-collection-class(t), c)
end method;

define method ^subtype?
    (t :: <&limited-collection-type>, u :: <&union>) => (well? :: <boolean>)
  ^subtype?(t, u.^union-type1) | ^subtype?(t, u.^union-type2)
end method;

define method ^instance? (object :: <model-value>, t :: <&limited-collection-type>)
    => (instance? :: <boolean>)
  let lc-size       = ^limited-collection-size(t);
  let lc-dimensions = ^limited-collection-dimensions(t);
  ^instance?(object, dylan-value(#"<limited-collection>"))
    & ^instance?(object, ^limited-collection-class(t))
    & ^instance?(^element-type(object), ^limited-collection-element-type(t))
    & (~lc-size | size(object) = lc-size)
    & (~lc-dimensions | every?(\=, dimensions(object), lc-dimensions))
end method;

//// Disjointness relationships.

define method ^known-disjoint? 
    (t1 :: <&limited-collection-type>, t2 :: <&limited-collection-type>)
 => (known-disjoint? :: <boolean>)
  ^known-disjoint?(^limited-collection-class(t1), ^limited-collection-class(t2))
    | ~^type-equivalent?(^limited-collection-element-type(t1), ^limited-collection-element-type(t2))
  // TODO: PUT IN SIZE AND DIMENSIONS HERE
end method ^known-disjoint?;

// "A singleton type is disjoint from another type if the singleton's object
// is not an instance of that other type"

define method ^known-disjoint? (t1 :: <&class>, t2 :: <&limited-collection-type>)
 => (known-disjoint? :: <boolean>)
  ^known-disjoint?(t1, ^limited-collection-class(t2))
end method ^known-disjoint?;

define method ^known-disjoint? (t1 :: <&limited-collection-type>, t2 :: <&class>)
 => (known-disjoint? :: <boolean>)
  ^known-disjoint?(t2, t1)
end method ^known-disjoint?;

define &class <limited-function-type> (<limited-type>)
  constant &slot limited-function-argument-types :: <simple-object-vector>,
    required-init-keyword: types:;
  constant &slot limited-function-return-values :: <simple-object-vector>,
    required-init-keyword: values:;
end;

define inline function ^subtype-arguments-and-values?
    (arg1 :: <simple-object-vector>, val1 :: <simple-object-vector>,
     arg2 :: <simple-object-vector>, val2 :: <simple-object-vector>)
 => (result :: <boolean>)
  if (arg1.size == arg2.size)
    block (return)
      for (x in arg1, y in arg2)
        unless(^subtype?(y, x)) //contravariance
          return(#f)
        end;
      end;
      for (x in val1, y in val2) //length! (append #f to val2 until size is equal to val1)
        unless(^subtype?(x, y)) //covariant
          return(#f);
        end;
      end;
      #t;
    end block;
  end;
end;

define method ^base-type (function :: <&limited-function-type>) => (result :: <&type>)
  dylan-value(#"<function>"); //or introduce <arrow-type>?
end;

define method ^instance? (o :: <model-value>, lft :: <&limited-function-type>) => (result :: <boolean>)
  #f
end;

define method ^instance? (f :: <&function>, lft :: <&limited-function-type>) => (result :: <boolean>)
  ^subtype?(f, lft);
end;

define method ^subtype? (f :: <&limited-function-type>, g :: <&limited-function-type>) => (result :: <boolean>)
  ^subtype-arguments-and-values?(f.^limited-function-argument-types,
                                 g.^limited-function-argument-types,
                                 f.^limited-function-return-values,
                                 g.^limited-function-return-values)
end;

define method ^subtype? (f :: <&function>, lft :: <&limited-function-type>) => (result :: <boolean>)
  let signature = ^function-signature(f);
  let arguments = ^signature-required(signature);
  let values = ^signature-values(signature);
  //take care about #rest!
  ^subtype-arguments-and-values?(arguments,
                                 lft.^limited-function-argument-types,
                                 values,
                                 lft.^limited-function-return-values)  
end;

define method ^subtype? (f :: <&type>, lft :: <&limited-function-type>) => (result :: <boolean>)
  #f
end;

//^known-disjoint? - ^instantiable?
define function ^limited-function (#rest all-keys,
                                   #key arguments :: <simple-object-vector> = #[],
                                   values :: <simple-object-vector> = #[])
 => (result :: <&limited-function-type>)
  immutable-model(make(<&limited-function-type>, arguments: arguments, values: values));
end;

define &override-function ^limited 
    (type :: <&type>, #rest keys) => (type :: <&type>)
  select (type)
    dylan-value(#"<integer>"), <&integer> // Convenient hack.
      => apply(^limited-integer, keys);
    dylan-value(#"<function>"), <&function>
      => apply(^limited-function, keys);
    otherwise
      => if (^subtype?(type, dylan-value(#"<collection>")))
           apply(^limited-collection, type, keys);
         else
           error("^limited applied to invalid type %=.", type);
         end;
  end;
end &override-function;

//// Compiler type properties.

define method type-checked-at-run-time? (type) 
  ~(raw-type?(type) | type == dylan-value(#"<object>"))
end method;
