Module:    internal
Synopsis:  Subclass types
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The subclass type

// BOOTED define ... class <subclass> ... end;

define method initialize (x :: <subclass>, #key, #all-keys) => ()
  next-method();
  instance?-iep(x) := simple-method-iep(subclass-instance?);
end method;

define method subclass (class :: <class>)
  make(<subclass>, class: class)
end method;

define inline method limits (subc :: <subclass>) => (result == <class>)
  <class>
end method;

//// Instance? relationships

define method instance?-function (t :: <subclass>) => (m :: <method>)
  subclass-instance?
end method;


define constant subclass-instance? = method (x, sc :: <subclass>) => (v :: <boolean>);
  if (instance?(x, <class>))
    let x :: <class> = x;	// This should be automatic...
    subclass?(x, sc.subclass-class)
  else
    #f
  end if
end method;


//// Subtype? relationships

// With other subclass types

define method subtype? 
    (subc1 :: <subclass>, subc2 :: <subclass>) => (result :: <boolean>)
  subtype?(subc1.subclass-class, subc2.subclass-class)
end method;


define method subjunctive-subtype? (subc1 :: <subclass>, subc2 :: <subclass>,
				    scu :: <subjunctive-class-universe>)
 => (result :: <boolean>)
  subjunctive-subtype?(subc1.subclass-class, subc2.subclass-class, scu)
end method;


// With singletons

define method subtype? 
    (subc1 :: <subclass>, s :: <singleton>) => (result == #f)
  #f
end method;

define method subtype? 
    (s :: <singleton>, subc :: <subclass>) => (result :: <boolean>)
  let obj = s.singleton-object;
  if (instance?(obj, <class>))
    let obj :: <class> = obj;
    subtype?(obj, subc.subclass-class)
  else
    #f
  end if
end method;

define method subjunctive-subtype? (subc1 :: <subclass>, s :: <singleton>,
				    scu :: <subjunctive-class-universe>)
 => (result == #f)
  #f
end method;

define method subjunctive-subtype? (s :: <singleton>, subc :: <subclass>,
				    scu :: <subjunctive-class-universe>)
 => (result :: <boolean>)
  let obj = s.singleton-object;
  if (instance?(obj, <class>))
    let obj :: <class> = obj;
    subtype?(obj, subc.subclass-class)
  else
    #f
  end if
end method;


// With classes

define method subtype? 
    (subc :: <subclass>, c :: <class>) => (result :: <boolean>)
  // c == <class>
    subtype?(<class>, c)
end method;

define method subtype? 
    (c :: <class>, subc :: <subclass>) => (result :: <boolean>)
  c == <class> & subc.subclass-class == <class>
end method;


define method subjunctive-subtype? (subc :: <subclass>, c :: <class>,
				    scu :: <subjunctive-class-universe>)
 => (result :: <boolean>)
  // c == <class>
  subtype?(<class>, c)
end method;

define method subjunctive-subtype? (c :: <class>, subc :: <subclass>,
				    scu :: <subjunctive-class-universe>)
 => (result :: <boolean>)
  c == <class> & subc.subclass-class == <class>
end method;


//// disjoint-type relationships


define method disjoint-types-1? (t1 :: <subclass>, t2 :: <subclass>,
				 scu :: <subjunctive-class-universe>,
				 dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  disjoint-types-1?(t1.subclass-class, t2.subclass-class, scu, dep)
end method;


define method disjoint-types-1? (t1 :: <subclass>, t2 :: <class>,
				 scu :: <subjunctive-class-universe>,
				 dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  disjoint-types-1?(<class>, t2, scu, dep)
end method;


define method disjoint-types-1? (t1 :: <class>, t2 :: <subclass>,
				 scu :: <subjunctive-class-universe>,
				 dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  disjoint-types-1?(<class>, t1, scu, dep)
end method;



define method type-complete? (t :: <subclass>) => (well? :: <boolean>)
  type-complete?(subclass-class(t))
end method;


define method map-congruency-classes (f :: <function>, t :: <subclass>) => ()
  map-congruency-classes(f, subclass-class(t))
end method;


define method reduce-incomplete-classes (f :: <function>, t :: <subclass>, ans) 
 => (ans)
  reduce-incomplete-classes(f, subclass-class(t), ans)
end method;



//// Preceding-specializer? relationships

// TODO: OBSOLETE?

/*
define method preceding-specializer? 
    (subc1 :: <subclass>, subc2 :: <subclass>, arg :: <class>)
 => (result :: <boolean>)
  precedes?(subc1.subclass-class, subc2.subclass-class, all-superclasses(arg))
end method;

// We rule that all applicable subclass specializers precede applicable 
// metaclass specializers.

define method preceding-specializer? 
    (subc :: <subclass>, class :: <class>, arg :: <class>) => (result == #t)
  #t
end method;
*/

///// Potential instance relationships?

define method has-instances? (class :: <class>, subc :: <subclass>,
			      scu :: <subjunctive-class-universe>)
 => (some? :: <boolean>, all? :: <boolean>)
  let class? :: <boolean> = subjunctive-subtype?(class, <class>, scu);
  values(class?, class? & subclass-class(subc) == <object>)
end method;
 
// eof
 
