Module:   dfmc-modeling
Synopsis: Limited function type models
Author:   Bruce Mitchener, Jr.
Copyright:    Original Code is Copyright (c) 2014 Dylan Hackers.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The limited function type.

define primary &class <limited-function> (<limited-type>)
  constant &slot limited-function-signature,
    required-init-keyword: signature:;
end &class;

//// Base type.

// "The base type of limited(C, ...) is C"

define method ^base-type (lf :: <&limited-function>) => (type :: <&type>)
  dylan-value(#"<function>")
end method;

//// Instance? relationships.

define method ^instance? (o :: <model-value>, lf :: <&limited-function>)
 => (well? :: <boolean>)
  #f
end method;

define method ^instance? (i :: <function>, lf :: <&limited-function>)
 => (well? :: <boolean>)
  #f // XXX: Implement
end method;

//// Subtype? relationships.

// With other limited integer types

define method ^subtype?
    (lf1 :: <&limited-function>, lf2 :: <&limited-function>)
 => (well? :: <boolean>)
  #f // XXX: Implement
end method;

// With other integer types - should consider different integer class
// precisions.

define method ^subtype? (class :: <&class>, lf :: <&limited-function>)
 => (well? :: <boolean>)
  #f
end method;

define method ^subtype? (lf :: <&limited-function>, class :: <&class>)
 => (well? :: <boolean>)
  ^subtype?(^base-type(lf), class)
end method;

//// Disjointness relationships.

define method ^known-disjoint?
    (lf1 :: <&limited-function>, lf2 :: <&limited-function>)
 => (well? :: <boolean>)
  #f // XXX: Implement
end method;

// "A limited type is disjoint from a class if their base types are
// disjoint."

define method ^known-disjoint?
    (lf :: <&limited-function>, t :: <&type>) => (well? :: <boolean>)
  ^known-disjoint?(^base-type(lf), t)
end method;

define method ^known-disjoint?
    (t :: <&type>, lf :: <&limited-function>) => (well? :: <boolean>)
  ^known-disjoint?(lf, t)
end method;
