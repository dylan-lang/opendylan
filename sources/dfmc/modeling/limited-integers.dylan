Module:   dfmc-modeling
Synopsis: Limited integer type models
Author:   Paul Haahr, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The limited integer type.

define primary &class <limited-integer> (<limited-type>)
  constant &slot limited-integer-min,
    required-init-keyword: min:;
  constant &slot limited-integer-max,
    required-init-keyword: max:;
end &class;

define method ^limited-integer
    (#rest all-keys,
     #key min :: false-or(<integer>) = #f, 
          max :: false-or(<integer>) = #f)
  if (min | max)
    // Could check for min > max and return a dignified empty type but
    // I'm not sure that's worthwile.
    immutable-model(make(<&limited-integer>, min: min, max: max))
  else
    dylan-value(#"<integer>")
  end
end method;

//// Base type.

// "The base type of limited(C, ...) is C"

define method ^base-type (limint :: <&limited-integer>) => (type :: <&type>)
  dylan-value(#"<integer>")
end method;

//// Instance? relationships.

define method ^instance? (o :: <model-value>, limint :: <&limited-integer>)
 => (well? :: <boolean>)
  #f
end method;

define method ^instance? (i :: <integer>, limint :: <&limited-integer>)
 => (well? :: <boolean>)
  let min = limint.^limited-integer-min;
  let max = limint.^limited-integer-max;
  (if (min) min <= i else #t end) & (if (max) i <= max else #t end)
end method;

//// Subtype? relationships.

// With other limited integer types

define method ^subtype? 
    (limint1 :: <&limited-integer>, limint2 :: <&limited-integer>)
 => (well? :: <boolean>)
  local method satisifies-bound (value, bound, satisfied?)
    if (~bound)
      #t
    else
      if (value) satisfied?(value, bound) else #f end
    end
  end;
  satisifies-bound(limint1.^limited-integer-min, limint2.^limited-integer-min, \>=)
    & satisifies-bound(limint1.^limited-integer-max, limint2.^limited-integer-max, \<=)
end method;

// With other integer types - should consider different integer class 
// precisions.  

define method ^subtype? (class :: <&class>, limint :: <&limited-integer>)
 => (well? :: <boolean>)
  #f
end method;

define method ^subtype? (limint :: <&limited-integer>, class :: <&class>)
 => (well? :: <boolean>)
  ^subtype?(^base-type(limint), class)
end method;

//// Disjointness relationships.

// "Two limited integer types are disjoint if the minimum value of one
// is greater than the maximum value of the other".

define method ^known-disjoint? 
    (limint1 :: <&limited-integer>, limint2 :: <&limited-integer>)
 => (well? :: <boolean>)
  local method exceeds-bound (value, bound)
    if (~bound)
      #f
    else
      if (value) value > bound else #f end
    end
  end;
  exceeds-bound(limint1.^limited-integer-min, limint2.^limited-integer-max)
    | exceeds-bound(limint2.^limited-integer-min, limint1.^limited-integer-max)
end method;

// "A limited type is disjoint from a class if their base types are
// disjoint or the class is a subclass of <integer> whose range is
// disjoint from the limited integer's range type".

// TODO: Consider more than one class of integer.

define method ^known-disjoint? 
    (limint :: <&limited-integer>, t :: <&type>) => (well? :: <boolean>)
  ^known-disjoint?(^base-type(limint), t)
end method;

define method ^known-disjoint? 
    (t :: <&type>, limint :: <&limited-integer>) => (well? :: <boolean>)
  ^known-disjoint?(limint, t)
end method;
