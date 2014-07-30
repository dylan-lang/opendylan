Module:   dfmc-modeling
Synopsis: Union type models
Author:   Paul Haahr, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The union type

// Pairwise union for simplicity - we'll need to generalize to deal
// reasonably with, say, the union of a large number of singletons
// for example.

define primary &class <union> (<type>)
  runtime-constant &slot union-type1 :: <type>,
    required-init-keyword: type1:;
  runtime-constant &slot union-type2 :: <type>,
    required-init-keyword: type2:;
  inherited slot ^instance?-function, init-value: #"union-instance?";
end &class;

define &override-function ^type-union
    (type :: <&type>, #rest more-types) => (type :: <&type>)
  local method binary-type-union
      (t1 :: <&type>, t2 :: <&type>) => (union :: <&type>)
    case
      t1 == t2
        => t1;
      ^subtype?(t1, t2)
        => t2;
      ^subtype?(t2, t1)
        => t1;
      // Have no idea how to code the "exhaustive partition" thing.
      // Currently exhaustive? Guaranteed exhaustive? Humbug!
      // (Exhaustive partition seems to have been dropped for the DRM.)
      otherwise
        => ^merge-types(t1, t2)
    end case;
  end binary-type-union;
  reduce(binary-type-union, type, more-types)
end &override-function;

// This protocol allows types with specific knowledge about how to merge
// to bring it into play.

define generic ^merge-types (type1 :: <&type>, type2 :: <&type>)
  => (type :: <&type>);

define method ^merge-types (t1 :: <&type>, t2 :: <&type>) => (type :: <&type>)
  immutable-model(make(<&union>, type1: t1, type2: t2))
end method;

//// one-of

define &override-function ^one-of
  (object, #rest objects)
  reduce(^type-union, ^singleton(object), map(^singleton, objects))
end &override-function;

//// Instance? relationships

define method ^instance?
    (o :: <model-value>, u :: <&union>) => (well? :: <boolean>)
  ^instance?(o, u.^union-type1) | ^instance?(o, u.^union-type2)
end method;

//// Subtype? relationships

// With non-union types

define method ^subtype? (u :: <&union>, t :: <&type>) => (well? :: <boolean>)
  ^subtype?(u.^union-type1, t) & ^subtype?(u.^union-type2, t)
end method;

define method ^subtype? (t :: <&type>, u :: <&union>) => (well? :: <boolean>)
  ^subtype?(t, u.^union-type1) | ^subtype?(t, u.^union-type2)
end method;

define method ^subtype? (u :: <&union>, t :: <&singleton>) => (well? :: <boolean>)
  ^subtype?(u.^union-type1, t) & ^subtype?(u.^union-type2, t)
end method;

define method ^subtype? (t :: <&singleton>, u :: <&union>) => (well? :: <boolean>)
  let s = t.^singleton-object;
  ^instance?(s, u.^union-type1) | ^instance?(s, u.^union-type2)
end method;

// With other union types (disambiguating method)

define method ^subtype?
    (u :: <&union>, t :: <&union>) => (well? :: <boolean>)
  ^subtype?(u.^union-type1, t) & ^subtype?(u.^union-type2, t)
end method;


//// Disjointness relationships.

// "A union type is disjoint from another type if both the union type's
// component types are disjoint from that other type"

define method ^known-disjoint? (u :: <&union>, t :: <&type>)
 => (disjoint? :: <boolean>)
  ^known-disjoint?(u.^union-type1, t)
    & ^known-disjoint?(u.^union-type2, t)
end method ^known-disjoint?;

define method ^known-disjoint? (t1 :: <&type>, t2 :: <&union>)
 => (disjoint? :: <boolean>)
  ^known-disjoint?(t2, t1);
end method ^known-disjoint?;

// Tie-breaker method..

define method ^known-disjoint? (u :: <&union>, t :: <&union>)
 => (disjoint? :: <boolean>)
  ^known-disjoint?(u.^union-type1, t)
    & ^known-disjoint?(u.^union-type2, t)
end method ^known-disjoint?;

//// False-or.

define &override-function ^false-or
    (type :: <&type>, #rest types) => (type :: <&type>)
  apply(^type-union, ^singleton(#f), type, types)
end &override-function;
