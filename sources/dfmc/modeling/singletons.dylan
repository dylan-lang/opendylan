Module:   dfmc-modeling
Synopsis: Singleton type models
Author:   Paul Haahr, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The singleton type.

define primary &class <singleton> (<limited-type>)
  runtime-constant &slot singleton-object,
    required-init-keyword: object:;
end &class <singleton>;

define &override-function ^singleton (object :: <model-value>)
  immutable-model(make(<&singleton>, object: object))
end &override-function;


//// Base type.

// "The base type of a singleton is the singleton itself"

define method ^base-type (type :: <&singleton>) => (type :: <&type>)
  type
end method ^base-type;

//// Instance? relationships.

define method ^instance? (test-object :: <model-value>, type :: <&singleton>)
 => (instance? :: <boolean>)
  // TODO: Is == the right test to use here?  Should there be an explicit
  // model-== test?  For now, I'm assuming that identity of model objects
  // is equivalent to identity of the objects they model.
  test-object == type.^singleton-object
end method ^instance?;

//// Subtype? relationships.

define method ^subtype? (t1 :: <&singleton>, t2 :: <&singleton>)
 => (subtype? :: <boolean>)
  t1.^singleton-object == t2.^singleton-object
end method ^subtype?;

define method ^subtype? (t1 :: <&singleton>, t2 :: <&type>)
 => (subtype? :: <boolean>)
  ^instance?(t1.^singleton-object, t2)
end method ^subtype?;

define method ^subtype? (t1 :: <&type>, t2 :: <&singleton>)
 => (subtype? :: <boolean>)
  #f
end method ^subtype?;

// Tiebreaker

define method ^subtype? (t1 :: <&limited-collection-type>, t2 :: <&singleton>)
 => (subtype? :: <boolean>)
  #f
end method ^subtype?;

//// Disjointness relationships.

define method ^known-disjoint? 
    (t1 :: <&singleton>, t2 :: <&singleton>)
 => (known-disjoint? :: <boolean>)
  t1.^singleton-object ~== t2.^singleton-object 
end method ^known-disjoint?;

// "A singleton type is disjoint from another type if the singleton's object
// is not an instance of that other type"

define method ^known-disjoint? (t1 :: <&singleton>, t2 :: <&type>)
 => (known-disjoint? :: <boolean>)
  ~^instance?(t1.^singleton-object, t2);
end method ^known-disjoint?;

define method ^known-disjoint? (t1 :: <&type>, t2 :: <&singleton>)
 => (known-disjoint? :: <boolean>)
  ~^instance?(t2.^singleton-object, t1);
end method ^known-disjoint?;

// Tiebreaker methods

define method ^known-disjoint? (t1 :: <&singleton>, t2 :: <&union>)
 => (known-disjoint? :: <boolean>)
  ^known-disjoint?(t1.^singleton-object, t2)
end method ^known-disjoint?;

define method ^known-disjoint? (t1 :: <&union>, t2 :: <&singleton>)
 => (known-disjoint? :: <boolean>)
  ^known-disjoint?(t1, t2.^singleton-object)
end method ^known-disjoint?;
