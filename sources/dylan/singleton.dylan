Module:    internal
Synopsis:  The definition of singleton types
Author:    Jonathan Bachrach, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The singleton type

// BOOTED: define ... class <singleton> ... end;

define method initialize (x :: <singleton>, #key, #all-keys) => ()
  next-method();
  initialize-singleton-instance?-function(x);
end method;

// TODO: figure out how to break the circularity that prevents
//       us from making this a non-generic function
define method singleton (object :: <object>) => (singleton :: <singleton>);
  make(<singleton>, object: object)
end;

define method \= (singleton-1 :: <singleton>, singleton-2 :: <singleton>)
 => (equal :: <boolean>)
  singleton-1.singleton-object = singleton-2.singleton-object;
end method \=;

define method limits (s :: <singleton>) => (class :: <class>)
  s.singleton-object.object-class
end method limits;


// @@@@@ This assumes we don't have hollow singletons!  (If we did, they'd
// @@@@@ be another type anyway.)
define method type-complete? (t :: <singleton>) => (well? :: <boolean>)
  #t
end method;


define method map-congruency-classes (f :: <function>, t :: <singleton>) => ()
  map-congruency-classes(f, object-class(singleton-object(t)))
end method;


// @@@@@ This assumes we don't have hollow singletons!
define method reduce-incomplete-classes (f :: <function>, t :: <singleton>, ans)
 => (ans)
  ans
end method;



//// Instance? relationships

define inline function initialize-singleton-instance?-function (s :: <singleton>) => ()
  instance?-iep(s)
    := simple-method-iep
         (if (indirect-object?(s.singleton-object))
            singleton-value-object-instance?
          else
            singleton-pointer-id?-instance?
          end if);
end function;


// This is the default instance?-function for singletons.  It's just a non-generic version
// of uninitialized-instance?-function.
define function singleton-instance? (x, s :: <singleton>) => (v :: <boolean>);
  initialize-singleton-instance?-function(x);
  primitive-instance?(x, s)
end function;


define function singleton-pointer-id?-instance? (x, s :: <singleton>) => (v :: <boolean>);
  pointer-id?(x, s.singleton-object)
end function;


define function singleton-value-object-instance? (x, s :: <singleton>) => (v :: <boolean>);
  let o = singleton-object(s);
  indirect-object?(x)
    & pointer-id?(indirect-object-class(x), indirect-object-class(o))
    & (x = o)
end function;


//// Subtype? relationships

define method subtype? (s1 :: <singleton>, s2 :: <singleton>)
 => (subtype? :: <boolean>)
  s1.singleton-object == s2.singleton-object
end method subtype?;

define method subtype? (s :: <singleton>, t :: <type>)
 => (subtype? :: <boolean>)
  instance?(s.singleton-object, t)
end method subtype?;

define method subtype? (t :: <type>, s :: <singleton>)
 => (subtype? :: <boolean>)
  #f
end method subtype?;


define method subjunctive-subtype? (s1 :: <singleton>, s2 :: <singleton>,
                                    scu :: <subjunctive-class-universe>)
 => (subtype? :: <boolean>)
  s1.singleton-object == s2.singleton-object
end method subjunctive-subtype?;

define method subjunctive-subtype? (s :: <singleton>, t :: <type>,
                                    scu :: <subjunctive-class-universe>)
 => (subtype? :: <boolean>);
  // It's a subjunctive subtype if the object is an instance of the type
  // and the class of the object is not being redefined.
  instance?(s.singleton-object, t)
    & ((scu == $empty-subjunctive-class-universe)
        |
        ~scu-entry?(s.singleton-object.object-class, scu))
end method subjunctive-subtype?;


define method subjunctive-subtype? (t :: <type>, s :: <singleton>,
                                    scu :: <subjunctive-class-universe>)
 => (subtype? :: <boolean>)
  #f
end method subjunctive-subtype?;


define method disjoint-types-1? (t1 :: <singleton>, t2 :: <singleton>,
                                 scu :: <subjunctive-class-universe>,
                                 dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  t1.singleton-object ~== t2.singleton-object
end method;

//define method disjoint-types-1? (t1 :: <singleton>, t2 :: <type>)
// => (well? :: <boolean>)
//  ~instance?(t1.singleton-object, t2)
//end method;

//define method disjoint-types-1? (t1 :: <type>, t2 :: <singleton>)
// => (well? :: <boolean>)
//  ~instance?(t2.singleton-object, t1)
//end method;


//// Potential instance relationships

define method has-instances? (class :: <class>, s :: <singleton>,
                              scu :: <subjunctive-class-universe>)
 => (some? :: <boolean>, all? :: <boolean>);
  // values(class == s.singleton-object.object-class, #f)
  // values(scu-entry(class, scu) == s.singleton-object.object-implementation-class, #f)
  let some? = has-instances?(s.singleton-object.object-class, class, scu);
  values(some?, #f)
end method has-instances?;
