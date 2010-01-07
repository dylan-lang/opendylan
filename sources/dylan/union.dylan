Module:    internal
Synopsis:  Union types
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The union type

// Pairwise union for simplicity - we'll need to generalize to deal 
// reasonably with, say, the union of a large number of singletons
// for example.

// BOOTED: define ... class <union> ... end;

define method initialize (x :: <union>, #key, #all-keys) => ()
  next-method();
  instance?-iep(x) := simple-method-iep(union-instance?);
end method;

define function type-union (type :: <type>, #rest more-types) => (type :: <type>)
  local method binary-type-union 
      (t1 :: <type>, t2 :: <type>) => (union :: <type>)
    case
      t1 == t2
  	=> t1;
      /* Illegal for loose Mode
      subtype?(t1, t2)
        => t2;
      subtype?(t2, t1)
  	=> t1;
      */
      // Have no idea how to code the "exhaustive partition" thing.
      // Currently exhaustive? Guaranteed exhaustive? Humbug!
      // (Exhaustive partition seems to have been dropped for the DRM.)
      otherwise
  	=> merge-types(t1, t2)
    end case;
  end method binary-type-union;
  reduce(binary-type-union, type, more-types)
end;

// This protocol allows types with specific knowledge about how to merge
// to bring it into play.

define generic merge-types (type1 :: <type>, type2 :: <type>) 
  => (type :: <type>);

define method merge-types (t1 :: <type>, t2 :: <type>) => (result :: <type>)
  make(<union>, type1: t1, type2: t2)
end method;

//// Instance? relationships

define method instance?-function (t :: <union>) => (m :: <method>)
  union-instance?
end method;


define constant union-instance? = method (x, u :: <union>) => (v :: <boolean>);
  primitive-instance?(x, union-type1(u)) | primitive-instance?(x, union-type2(u))
end method;


//// Subtype? relationships

// With other union types

define method subtype? (u1 :: <union>, u2 :: <union>) => (result :: <boolean>)
  //(subtype?(u1.union-type1, u2.union-type1) | subtype?(u1.union-type1, u2.union-type2)) 
  //  & (subtype?(u1.union-type2, u2.union-type1) | subtype?(u1.union-type2, u2.union-type2)) 
  subtype?(u1.union-type1, u2) & subtype?(u1.union-type2, u2)
end method;

define method subjunctive-subtype? (u1 :: <union>, u2 :: <union>,
				    scu :: <subjunctive-class-universe>) 
 => (result :: <boolean>)
  //(subtype?(u1.union-type1, u2.union-type1) | subtype?(u1.union-type1, u2.union-type2)) 
  //  & (subtype?(u1.union-type2, u2.union-type1) | subtype?(u1.union-type2, u2.union-type2)) 
  subjunctive-subtype?(u1.union-type1, u2, scu) & subjunctive-subtype?(u1.union-type2, u2, scu)
end method;

// With non-union types

define method subtype? (u :: <union>, t :: <type>) => (result :: <boolean>)
  subtype?(u.union-type1, t) & subtype?(u.union-type2, t)
end method;

define method subtype? (t :: <type>, u :: <union>) => (result :: <boolean>)
  subtype?(t, u.union-type1) | subtype?(t, u.union-type2)
end method;


define method subjunctive-subtype? (u :: <union>, t :: <type>,
				    scu :: <subjunctive-class-universe>) 
 => (result :: <boolean>)
  subjunctive-subtype?(u.union-type1, t, scu) & subjunctive-subtype?(u.union-type2, t, scu)
end method;

define method subjunctive-subtype? (t :: <type>, u :: <union>,
				    scu :: <subjunctive-class-universe>)
 => (result :: <boolean>)
  subjunctive-subtype?(t, u.union-type1, scu) | subjunctive-subtype?(t, u.union-type2, scu)
end method;

//// disjointness relationships


define method disjoint-types? (t1 :: <union>, t2 :: <union>,
			       scu :: <subjunctive-class-universe>,
			       dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  disjoint-types?(t1.union-type1, t2.union-type1, scu, dep)
    & disjoint-types?(t1.union-type1, t2.union-type2, scu, dep)
    & disjoint-types?(t1.union-type2, t2.union-type1, scu, dep)
    & disjoint-types?(t1.union-type2, t2.union-type2, scu, dep)
end method;


define method disjoint-types? (t1 :: <union>, t2 :: <type>,
			       scu :: <subjunctive-class-universe>,
			       dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  disjoint-types?(t1.union-type1, t2, scu, dep)
    & disjoint-types?(t1.union-type2, t2, scu, dep)
end method;


define method disjoint-types? (t1 :: <type>, t2 :: <union>,
			       scu :: <subjunctive-class-universe>,
			       dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  disjoint-types?(t1, t2.union-type1, scu, dep)
    & disjoint-types?(t1, t2.union-type2, scu, dep)
end method;


define method type-complete? (t :: <union>) => (well? :: <boolean>)
  type-complete?(t.union-type1) & type-complete?(t.union-type2)
end method;


define method map-congruency-classes (f :: <function>, t :: <union>) => ()
  map-congruency-classes(f, t.union-type1);
  map-congruency-classes(f, t.union-type2);
end method;


define method reduce-incomplete-classes (f :: <function>, t :: <union>, ans)
 => (ans)
  reduce-incomplete-classes(f, t.union-type2, 
			    reduce-incomplete-classes(f, t.union-type1, ans))
end method;


///// Potential instance relationships?

define method has-instances? (class :: <class>, u :: <union>,
			      scu :: <subjunctive-class-universe>)
  => (some? :: <boolean>, all? :: <boolean>);
  let (some1?, all1?) = has-instances?(class, u.union-type1, scu);
  let (some2?, all2?) = has-instances?(class, u.union-type2, scu);
  values(some1? | some2?, all1? | all2?)
end method;

//// False-or

define inline function false-or (#rest types) => (type :: <type>)
  apply(type-union, singleton(#f), types)
end;

//// One-of

define inline function one-of (first, #rest rest)
  reduce(type-union, singleton(first), map(singleton, rest))
end;
