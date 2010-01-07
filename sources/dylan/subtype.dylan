Module:    internal
Synopsis:  Subtype types
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The subtype type

define class <subtype> (<limited-type>)
  slot type :: <type>,
    required-init-keyword: type:;
end class;

// Perform some simple canonicalisation

define method make (class == <subtype>, #key type :: <type>) => (object)
  if (has-subtypes?(type))
    next-method()
  else
    type
  end
end method;

define constant subtype =
  method (type :: <type>)
    make(<subtype>, type: type)
  end;

define method limited 
    (type == <type>, #key subtype: sub :: <type> = <object>) 
 => (result :: <type>)
  subtype(sub)
end method;

define method limits (subt :: <subtype>) => (result == <type>)
  <type>
end method;

// Assume a type can have subtypes by default

define method has-subtypes? (t :: <type>) => (result == #t)
  #t
end method;

define method has-subtypes? (t :: <singleton>) => (result == #f)
  #f
end method;

//// Instance? relationships

define method instance? (o :: <object>, subt :: <subtype>) => (result == #f)
  #f
end method;

define method instance? (t :: <type>, subt :: <subtype>) => (result :: <boolean>)
  subtype?(t, subt.type)
end method;

//// Subtype? relationships

// With other subtype types

define method subtype? 
    (subt1 :: <subtype>, subt2 :: <subtype>) => (result :: <boolean>)
  subtype?(subt1.type, subt2.type)
end method;

// With singletons (assumes the subtype(singleton(X)) -> singleton(X)
// canonicalisation)

define method subtype? (subt1 :: <subtype>, s :: <singleton>) => (result == #f)
  #f
end method;

// With miscellaneous types

define method subtype? (subt :: <subtype>, t :: <type>) => (result :: <boolean>)
  subtype?(<type>, t)
end method;

define method subtype? (t :: <type>, subt :: <subtype>) => (result :: <boolean>)
  subtype?(t, <type>) & subtype?(<object>, subt.type)
end method;

//// Preceding-specializer? relationships

define method preceding-specializer? 
    (subt1 :: <subtype>, subt2 :: <subtype>, arg :: <class>)
 => (result :: <boolean>)
  if (instance?(subt1.type, <class>) & instance?(subt2.type, <class>))
    precedes?(subt1.type, subt2.type, all-superclasses(arg))
  else
    #f
  end
end method;

define method preceding-specializer? 
    (subt1 :: <subtype>, subt2 :: <subtype>, arg :: <limited-type>)
 => (result :: <boolean>)
  preceding-specializer?(subt1, subt2, limits(arg))
end method;

define method preceding-specializer?
    (subt1 :: <subtype>, subt2 :: <subtype>, arg :: <type>) => (result == #f)
  #f
end method;

// We rule that all applicable subtype specializers precede applicable 
// metatype specializers.

define method preceding-specializer? 
    (subt :: <subtype>, type :: <type>, arg :: <type>) => (result == #t)
  #t
end method;

define method preceding-specializer? 
    (subt :: <subtype>, type :: <singleton>, arg :: <type>) => (result == #f)
  #f
end method;

///// Potential instance relationships?

define method has-instances? 
    (class :: <class>, subt :: <subtype>, scu :: <subjunctive-class-universe>)
 => (some? :: <boolean>, all? == #f)
  values(subtype?(class, <type>), #f)
end method;

///// Hacks because of dodgy installation

define method preceding-specializer? 
    (subt1 :: <subtype>, subt2 :: <subtype>, arg :: <singleton>)
 => (result :: <boolean>)
  preceding-specializer?(subt1, subt2, object-class(arg.singleton-object))
end method;

define method preceding-specializer? 
    (subt1 :: <subtype>, subt2 :: <subtype>, arg :: <subtype>)
 => (result :: <boolean>)
  preceding-specializer?(subt1, subt2, object-class(arg.type))
end method;
