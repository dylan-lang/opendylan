Module:    dfmc-modeling
Synopsis:  Subclass types
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The subclass type

define primary &class <subclass> (<limited-type>)
  runtime-constant &slot subclass-class :: <class>,
    required-init-keyword: class:;
  inherited slot ^instance?-function, init-value: #"subclass-instance?";
end &class;

define &override-function ^subclass (class :: <&class>)
  make(<&subclass>, class: class)
end &override-function;

//// Base type.

define method ^base-type (subc :: <&subclass>) => (type :: <&type>)
  dylan-value(#"<class>");
end method;

//// Instance? relationships

define method ^instance?
    (o :: <model-value>, subc :: <&subclass>) => (result :: <boolean>)
  #f
end method;

define method ^instance?
    (c :: <&class>, subc :: <&subclass>) => (result :: <boolean>)
  ^subtype?(c, subc.^subclass-class)
end method;

//// Subtype? relationships

// From "Subclass specializers", Version 2

//  SUBTYPE-1. subtype?(subclass(X), subclass(Y))
//  This will be true if and only if X is a subclass of Y.

define method ^subtype?
    (subc1 :: <&subclass>, subc2 :: <&subclass>) => (result :: <boolean>)
  ^subtype?(subc1.^subclass-class, subc2.^subclass-class)
end method;

//  SUBTYPE-2. subtype?(singleton(X), subclass(Y))
//  This will be true if and only if X is a class and X is a subclass of
//  Y.

define method ^subtype?
    (s :: <&singleton>, subc :: <&subclass>) => (result :: <boolean>)
  ^instance?(s.^singleton-object, dylan-value(#"<class>"))
    & ^subtype?(s.^singleton-object, subc.^subclass-class)
end method;

//  SUBTYPE-3. subtype?(subclass(X), singleton(Y))
//  This is always false.

define method ^subtype?
    (subc1 :: <&subclass>, s :: <&singleton>) => (result :: <boolean>)
  #f
end method;

//  SUBTYPE-4. subtype?(subclass(X), Y), where Y is not a subclass type
//  This will be true if Y is <class> or any proper superclass of
//  <class> (including <object>, any implementation-defined supertypes,
//  and unions involving any of these). There may be other
//  implementation-defined combinations of types X and Y for which this
//  is also true.

define method ^subtype?
    (subc :: <&subclass>, t :: <&class>) => (result :: <boolean>)
  ^subtype?(dylan-value(#"<class>"), t)
end method;

//  SUBTYPE-5. subtype?(X, subclass(Y)), where X is not a subclass type
//  This will be true if Y is <object> or any proper supertype of
//  <object> and X is a subclass of <class>.

define method ^subtype?
    (t :: <&class>, subc :: <&subclass>) => (result :: <boolean>)
  ^subtype?(dylan-value(#"<object>"), subc.^subclass-class)
    & ^subtype?(t, dylan-value(#"<class>"))
end method;

//// Disjointness relationships.

//  DISJOINTNESS+1. A subclass type subclass(X) and a type Y are
//  disjoint if Y is disjoint from <class>.

define method ^known-disjoint?
    (subc :: <&subclass>, type :: <&type>) => (value :: <boolean>)
  ^known-disjoint?(type, dylan-value(#"<class>"));
end method;

define method ^known-disjoint?
    (type :: <&type>, subc :: <&subclass>) => (value :: <boolean>)
  ^known-disjoint?(subc, type);
end method;

//  DISJOINTNESS+2. Two subclass types subclass(X) and subclass(Y) are
//  disjoint if the classes X and Y are disjoint.

define method ^known-disjoint?
    (subc1 :: <&subclass>, subc2 :: <&subclass>) => (value :: <boolean>)
  ^known-disjoint?(subc1.^subclass-class, subc2.^subclass-class);
end method;

//  DISJOINTNESS+3. A subclass type subclass(X) and a singleton type
//  singleton(O) are disjoint unless O is a class and O is a subclass of
//  X.

define method ^known-disjoint?
    (subc :: <&subclass>, singleton :: <&singleton>) => (value :: <boolean>)
  ~^subtype?(singleton, subc)
end method;

define method ^known-disjoint?
    (singleton :: <&singleton>, subc :: <&subclass>) => (value :: <boolean>)
  ~^subtype?(singleton, subc)
end method;
