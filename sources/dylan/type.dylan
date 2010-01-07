Module:    internal
Author:    Jonathan Bachrach, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Types

// BOOTED: define ... class <type> ... end;

/// HACK BEGIN: NEEDED FOR GROUNDING OUT PARTIAL DISPATCH

define constant <false-or-dependent-generic-function> = <object>;

/// HACK END:


define generic subtype? (type1 :: <type>, type2 :: <type>) 
  => (boolean :: <boolean>);

define generic subjunctive-subtype? (type1 :: <type>, type2 :: <type>, 
				     scu :: <subjunctive-class-universe>)
 => (boolean :: <boolean>);


define generic disjoint-types? (t1 :: <type>, t2 :: <type>, 
				scu :: <subjunctive-class-universe>,
				dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>);


define generic disjoint-types-1? (t1 :: <type>, t2 :: <type>, 
				  scu :: <subjunctive-class-universe>,
				  dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>);


define method disjoint-types? (t1 :: <type>, t2 :: <type>, 
			       scu :: <subjunctive-class-universe>,
			       dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  disjoint-types-1?(t1, t2, scu, dep)
end method;


// This is a default which is not correct for all types.
define method disjoint-types-1? (t1 :: <type>, t2 :: <type>, 
				 scu :: <subjunctive-class-universe>,
				 dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  // This gets hit a fair bit, most importantly load time method/domain definition.
  let dis?
    = if (scu == $empty-subjunctive-class-universe)
	~subtype?(t1, t2) & ~subtype?(t2, t1)
      else
	~subjunctive-subtype?(t1, t2, scu) & ~subjunctive-subtype?(t2, t1, scu)
      end if;
  when (dis? & dep)
    %register-disjoint-dependent-generic(t1, t2, dep);
  end when;
  dis?
end method;

define inline method %register-disjoint-dependent-generic 
    (t1 :: <type>, t2 :: <type>, dep :: <false-or-dependent-generic-function>)
  when (dep)
    map-congruency-classes(curry(%register-subclass-dependent-generic, dep), t1);
    map-congruency-classes(curry(%register-subclass-dependent-generic, dep), t2);
  end when;
end method;

define sealed generic recompute-type-complete! (x) => (well? :: <boolean>);


define method recompute-type-complete! (x) => (well? :: <boolean>)
  type-complete?(x)
end method;


define sealed generic type-complete? (x :: <object>) => (well? :: <boolean>);


define method type-complete? (t :: <type>) => (v == #t)
  #t
end method;

define function type-complete?-sov (x :: <simple-object-vector>, n :: <integer>)
 => (well? :: <boolean>)
  local method loop (i :: <integer>) => (well? :: <boolean>)
	  (i < 0) | (type-complete?(x[i]) & loop (i - 1))
	end method;
  loop(n - 1)
end function;
  

define sealed generic map-congruency-classes (f :: <function>, t :: <object>) => ();

define function map-congruency-classes-sov 
    (f :: <function>, x :: <simple-object-vector>, n :: <integer>) => ()
  local method loop (i :: <integer>) => (well? :: <boolean>)
	  if (i >= 0) map-congruency-classes(f, x[i]); loop(i - 1) end
	end method;
  loop(n - 1)
end function;

define method map-congruency-classes (f :: <function>, t :: <type>) => ()
end method;


    
define function incomplete-classes (x)
 => (ans :: <list>)
  reduce-incomplete-classes(method (class :: <class>, ans :: <list>)
			      add-new!(ans, class)
			    end method,
			    x, #())
end function;


define sealed generic reduce-incomplete-classes (f :: <function>, x, accumulating-answer)
 => (accumulating-answer);


define method reduce-incomplete-classes (f :: <function>, t :: <type>, accumulating-answer)
 => (accumulating-answer)
  accumulating-answer
end method;


define method reduce-incomplete-classes (f :: <function>, t == #f, accumulating-answer)
 => (accumulating-answer)
  accumulating-answer
end method;
  


define function reduce-incomplete-classes-sov
    (f :: <function>, v :: <simple-object-vector>, n :: <integer>, ans :: <collection>)
 => (ans)
  local method loop (i :: <integer>, ans) => (ans)
	  if (i >= 0)
	    loop(i - 1, reduce-incomplete-classes(f, v[i], ans))
	  else
	    ans
	  end if
	end method;
  loop(n - 1, ans)
end function;



// This is *not* extensible;  it is a g.f. simply to make the override-function work.
// Use instance?-function, q.v., to augment instance?.
// The one following method should be the only method defined, lest random calls to instance?
// not get turned into primitive-instance?.
define generic instance? (object :: <object>, type :: <type>) => (boolean :: <boolean>);

define inline method instance? (object :: <object>, type :: <type>) => (boolean :: <boolean>);
  primitive-instance?(object, type)
end method;


define generic instance?-function (type :: <type>) => (m :: <method>);


// The IEP of this goes in the instance?-iep slot of types which the compiler
// doesn't know how to set up at compile time.
define constant uninitialized-instance?-function = method (obj, type :: <type>) => (v :: <boolean>);
  let m :: <simple-method> = instance?-function(type);
  instance?-iep(type) := simple-method-iep(m);
  primitive-instance?(obj, type)
end method;

  
// Dispatch protocol

// If some of the instances of class are instances of type, return #t
// As the first result, otherwise #f.
// If all of the instances of class are instances of type, return #t
// as the second result, otherwise #f.

define generic has-instances?
    (class :: <class>, type :: <type>, scu :: <subjunctive-class-universe>)
  => (some? :: <boolean>, all? :: <boolean>);

// Called to determine whether the first type precedes the second with
// respect to a particular argument during generic dispatch. Its 
// default behaviour is to simply call subtype? on the types without 
// considering the argument.

// TODO: OBSOLETE?

/*
define generic preceding-specializer?
    (type1 :: <type>, type2 :: <type>, argument :: <object>)
  => (boolean);

define method preceding-specializer?
    (type1 :: <type>, type2 :: <type>, argument :: <object>)
 => (boolean)
  subtype?(type1, type2)
end method;
*/

//// Primitive types

// TODO: OBSOLETE?

/*
define abstract class <primitive-type> (<type>) end;
*/

//// Limited types

// BOOTED: define ... class <limited-type> ... end;

define generic limited (class :: <class>, #key, #all-keys) 
  => (type :: <type>);

define generic limits (type :: <limited-type>) 
  => (class :: <class>);

define method limited 
    (class :: subclass(<collection>), #key, #all-keys) => (type :: <type>)
  class
end method;

define method limited 
    (class == <string>, #key of, size, #all-keys) => (type :: <type>)
  limited-string(of, size)
end method;

define method limited 
    (class == <table>, 
     #key of :: <type> = <object>, size :: false-or(<integer>), #all-keys)
 => (type :: <type>)
  limited-table(of, size)
end method;

define method limited 
    (class == <object-table>, 
     #key of :: <type> = <object>, size :: false-or(<integer>), #all-keys)
 => (type :: <type>)
  limited-table(of, size)
end method;

define method limited 
    (class == <vector>, 
     #key of :: <type> = <object>, size :: false-or(<integer>), #all-keys) 
 => (type :: <type>)
  limited-vector(of, size)
end method;

define method limited 
    (class == <simple-vector>, 
     #key of :: <type> = <object>, size, size :: false-or(<integer>), #all-keys)
 => (type :: <type>)
  limited-vector(of, size)
end method;

define method limited 
    (class == <array>, 
     #key of :: <type> = <object>, 
          size: sz :: false-or(<integer>), 
          dimensions :: false-or(<sequence>), 
     #all-keys) 
 => (type :: <type>)
  if (sz)
    if (dimensions & (size(dimensions) ~= 1 | dimensions[0] ~= sz))
      error("Dimensions %= incompatible to size %= in call to limited(<array>)",
	    dimensions, sz);
    end if;
    limited-vector(of, sz)
  elseif (dimensions & size(dimensions) = 1)
    limited-vector(of, first(dimensions))
  else
    limited-array(of, dimensions)
  end if
end method;

/// UNINSTANTIATED LIMITED COLLECTION TYPES

define method limited
    (class == <collection>, 
     #key of :: <type> = <object>, size :: false-or(<integer>), #all-keys) 
 => (type :: <type>)
  make(<limited-collection-type>,
       class:          <collection>,
       element-type:   of,
       size:           size)
end method;

define method limited
    (class == <explicit-key-collection>, 
     #key of :: <type> = <object>, size :: false-or(<integer>), #all-keys) 
 => (type :: <type>)
  make(<limited-explicit-key-collection-type>,
       class:          <explicit-key-collection>,
       element-type:   of,
       size:           size)
end method;

define method limited
    (class == <mutable-collection>, 
     #key of :: <type> = <object>, size :: false-or(<integer>), #all-keys) 
 => (type :: <type>)
  make(<limited-mutable-collection-type>,
       class:          <mutable-collection>,
       element-type:   of,
       size:           size)
end method;

define method limited
    (class == <stretchy-collection>, #key of :: <type> = <object>, #all-keys) 
 => (type :: <type>)
  make(<limited-stretchy-collection-type>,
       class:          <stretchy-collection>,
       element-type:   of)
end method;

define method limited
    (class == <mutable-explicit-key-collection>, 
     #key of :: <type> = <object>, size :: false-or(<integer>), #all-keys) 
 => (type :: <type>)
  make(<limited-mutable-explicit-key-collection-type>,
       class:          <mutable-explicit-key-collection>,
       element-type:   of,
       size:           size)
end method;

define method limited
    (class == <sequence>, 
     #key of :: <type> = <object>, size :: false-or(<integer>), #all-keys) 
 => (type :: <type>)
  make(<limited-sequence-type>,
       class:          <sequence>,
       element-type:   of,
       size:           size)
end method;

define method limited
    (class == <mutable-sequence>, 
     #key of :: <type> = <object>, size :: false-or(<integer>), #all-keys) 
 => (type :: <type>)
  make(<limited-mutable-sequence-type>,
       class:          <mutable-sequence>,
       element-type:   of,
       size:           size)
end method;

/// define class <limited-collection-type>  ... end

define sealed inline method make 
    (t :: <limited-collection-type>, #rest all-keys, #key size = unsupplied(), #all-keys) 
 => (res :: <collection>)
  let concrete-class = limited-collection-concrete-class(t);
  if (concrete-class)
    let size :: <integer> = limited-collection-size(t) | (supplied?(size) & size) | 0;
    apply(make, concrete-class,
	  element-type: limited-collection-element-type(t),
	  size:         size,
	  all-keys)
  else
    error("Cannot instantiate an uninstantiable limited collection type - %=", t)
  end if
end method;

define sealed inline method make 
    (t :: <limited-stretchy-collection-type>, #rest all-keys, #key, #all-keys) 
 => (res :: <stretchy-collection>)
  apply(make, limited-collection-concrete-class(t),
	element-type: limited-collection-element-type(t),
	all-keys)
end method;

define sealed inline method make 
    (t :: <limited-array-type>, #rest all-keys, 
     #key size = unsupplied(), dimensions = unsupplied(), #all-keys) 
 => (res :: <array>)
  if (supplied?(size))
    if (limited-collection-dimensions(t))
      error("Incompatible size %= and limited array type %=.", size, t);
    else
      apply(make, concrete-limited-vector-class(t),
	    element-type: limited-collection-element-type(t),
	    size:         size,
	    all-keys)
    end if
  else // multidimensional limited array
    let lim-dims
      = limited-collection-dimensions(t);
    let dims
      = if (lim-dims)
	  if (supplied?(dimensions) & dimensions ~= lim-dims)
	    error("Dimensions %= incompatible to limited array dimensions %= in call to make(<array>)",
		  dimensions, lim-dims);
	  end if;
	  lim-dims
	else 
	  dimensions
	end if;
    apply(make, limited-collection-concrete-class(t),
	  element-type: limited-collection-element-type(t),
	  dimensions:   dims,
	  all-keys)
  end if
end method;

define sealed inline method make 
    (t :: <limited-vector-type>, #rest all-keys, 
     #key size = unsupplied(), #all-keys) 
 => (res :: <vector>)
  let concrete-class = limited-collection-concrete-class(t);
  let size :: <integer> = limited-collection-size(t) | (supplied?(size) & size) | 0;
  apply(make, concrete-class,
	element-type: limited-collection-element-type(t),
	size:         size,
	all-keys)
end method;

define function limited-collection-instance? 
    (x, t :: <limited-collection-type>) => (well? :: <boolean>)
  let lc-size       = limited-collection-size(t);
  let lc-dimensions = limited-collection-dimensions(t);
  instance?(x, <limited-collection>)
    & instance?(x, limited-collection-class(t))
    & type-equivalent?(element-type(x), limited-collection-element-type(t))
    & (~lc-size | size(x) = lc-size)
    & (~lc-dimensions | every?(\=, dimensions(x), lc-dimensions))
end function;

define sealed method initialize (t :: <limited-collection-type>, #key, #all-keys)
  next-method();
  t.instance?-iep := limited-collection-instance?.simple-method-iep;
end method;

define inline method limits (t :: <limited-collection-type>) => (result :: <class>)
  limited-collection-concrete-class(t)
end method;

//// Subtype? relationships

// With other limited integer types

define method subjunctive-type-equivalent? 
    (t1 :: <type>, t2 :: <type>, scu :: <subjunctive-class-universe>) => (well? :: <boolean>)
  (t1 == t2) | (subjunctive-subtype?(t1, t2, scu) & subjunctive-subtype?(t2, t1, scu));
end method;

define method subjunctive-type-equivalent? 
    (t1 :: <class>, t2 :: <class>, scu :: <subjunctive-class-universe>) => (well? :: <boolean>)
  (t1 == t2) | (scu-entry(t1, scu) == scu-entry(t2, scu))
end method;

define inline function type-equivalent? 
    (t1 :: <type>, t2 :: <type>) => (well? :: <boolean>)
  subjunctive-type-equivalent?(t1, t2, $empty-subjunctive-class-universe)
end function;

define method subtype? 
    (t1 :: <limited-collection-type>, t2 :: <limited-collection-type>) 
 => (result :: <boolean>)
  let c1 = limited-collection-class(t1);
  let c2 = limited-collection-class(t2);
  let e1 = limited-collection-element-type(t1);
  let e2 = limited-collection-element-type(t2);
  let s1 = limited-collection-size(t1);
  let s2 = limited-collection-size(t2);
  let d1 = limited-collection-dimensions(t1);
  let d2 = limited-collection-dimensions(t2);
  subtype?(c1, c2)
    & type-equivalent?(e1, e2)
    & (if (~d1 & ~d2)
	 ~s2 | s1 = s2
       else 
	 d1 & (~d2 | every?(\=, d1, d2)) & (~s2 | reduce1(\*, d1) = s2)
       end if)
end method;

define method subjunctive-subtype? 
    (t1 :: <limited-collection-type>, t2 :: <limited-collection-type>,
     scu :: <subjunctive-class-universe>) 
 => (result :: <boolean>)
  let c1 = limited-collection-class(t1);
  let c2 = limited-collection-class(t2);
  let e1 = limited-collection-element-type(t1);
  let e2 = limited-collection-element-type(t2);
  let s1 = limited-collection-size(t1);
  let s2 = limited-collection-size(t2);
  let d1 = limited-collection-dimensions(t1);
  let d2 = limited-collection-dimensions(t2);
  subjunctive-subtype?(c1, c2, scu) & subjunctive-type-equivalent?(e1, e2, scu)
    & (if (~d1 & ~d2)
	 ~s2 | s1 = s2
       else 
	 d1 & (~d2 | every?(\=, d1, d2)) & (~s2 | reduce1(\*, d1) = s2)
       end if)
end method;

// With other integer types - should consider different integer class 
// precisions.  

define method subtype? 
    (t1 :: <type>, t2 :: <limited-collection-type>) => (result == #f)
  #f
end method;

define method subjunctive-subtype? (t1 :: <type>, t2 :: <limited-collection-type>,
                                 scu :: <subjunctive-class-universe>)
 => (result == #f)
  #f
end method;

define method subtype? 
    (t1 :: <singleton>, t2 :: <limited-collection-type>) => (well? :: <boolean>)
  instance?(singleton-object(t1), t2)
end method;

define method subjunctive-subtype? 
    (t1 :: <singleton>, t2 :: <limited-collection-type>,
     scu :: <subjunctive-class-universe>) => (well? :: <boolean>)
  instance?(singleton-object(t1), t2)
end method;

define method subtype? 
    (t1 :: <limited-collection-type>, t2 :: <type>) => (result == #f)
  #f
end method;

define method subjunctive-subtype? (t1 :: <limited-collection-type>, t2 :: <type>,
                                 scu :: <subjunctive-class-universe>)
 => (result == #f)
  #f
end method;

// With union types

define method subtype? (u :: <union>, t :: <limited-collection-type>)
 => (result :: <boolean>)
  subtype?(u.union-type1, t) & subtype?(u.union-type2, t)
end method;

define method subtype? (t :: <limited-collection-type>, u :: <union>)
 => (result :: <boolean>)
  subtype?(t, u.union-type1) | subtype?(t, u.union-type2)
end method;

define method subjunctive-subtype? (u :: <union>, t :: <limited-collection-type>,
				    scu :: <subjunctive-class-universe>) 
 => (result :: <boolean>)
  subjunctive-subtype?(u.union-type1, t, scu) & subjunctive-subtype?(u.union-type2, t, scu)
end method;

define method subjunctive-subtype? (t :: <limited-collection-type>, u :: <union>,
				    scu :: <subjunctive-class-universe>)
 => (result :: <boolean>)
  subjunctive-subtype?(t, u.union-type1, scu) | subjunctive-subtype?(t, u.union-type2, scu)
end method;

define method subtype? 
    (t1 :: <limited-collection-type>, t2 :: <class>) => (result == #f)
  subtype?(limited-collection-class(t1), t2)
end method;

define method subjunctive-subtype? 
    (t1 :: <limited-collection-type>, t2 :: <class>,
     scu :: <subjunctive-class-universe>) => (well? :: <boolean>)
  subjunctive-subtype?(limited-collection-class(t1), t2, scu)
end method;

define method has-instances? 
    (class :: <class>, type :: <limited-collection-type>,
     scu :: <subjunctive-class-universe>)
 => (some? :: <boolean>, all? :: <boolean>);
  subclass?(class, <limited-collection>)
    & values(has-instances?(class, limited-collection-class(type), scu), #f)
  // & has-instances?(element-type(class), limited-collection-element-type(type))
end method has-instances?;

define inline function disjoint-grounded-has-instances? 
    (c :: <class>, t :: <type>, scu :: <subjunctive-class-universe>)
  if (scu == $empty-subjunctive-class-universe)
    if (instance?(t, <singleton>))
      let t :: <singleton> = t;
      has-instances?(c, t, $empty-subjunctive-class-universe);
    else
      grounded-has-instances?(c, t);
    end if
  else 
    has-instances?(c, t, scu)
  end if
end function;

define method disjoint-types-1? (t1 :: <limited-type>, t2 :: <class>, 
				 scu :: <subjunctive-class-universe>,
				 dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  let dis? = ~disjoint-grounded-has-instances?(t2, t1, scu);
  when (dis?)
    %register-disjoint-dependent-generic(t1, t2, dep);
  end when;
  dis?
end method;

define method disjoint-types-1? (t1 :: <class>, t2 :: <limited-type>, 
				 scu :: <subjunctive-class-universe>,
				 dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  let dis? = ~disjoint-grounded-has-instances?(t1, t2, scu);
  when (dis?)
    %register-disjoint-dependent-generic(t1, t2, dep);
  end when;
  dis?
end method;

define method reduce-incomplete-classes (f :: <function>, t :: <limited-type>, ans)
 => (ans)
  reduce-incomplete-classes(f, limits(t), ans)
end method;


define method type-complete? (t :: <limited-type>) => (well? :: <boolean>)
  type-complete?(limits(t))
end method;

define method map-congruency-classes (f :: <function>, t :: <limited-type>) => ()
  map-congruency-classes(f, limits(t))
end method;
