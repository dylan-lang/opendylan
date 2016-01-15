Module:    internal
Synopsis:  Limited function types
Author:    Bruce Mitchener, Jr.
Copyright:    Original Code is Copyright (c) 2015 Dylan Hackers.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The limited function type

// BOOTED: define ... class <limited-function> ... end;

define method limited
    (class == <function>, #key signature :: <signature> = #f)
 => (result :: <type>)
  if (signature)
    let type :: <limited-function> = make(<limited-function>, signature: signature);
    unless (instance?-iep(type))
      instance?-iep(type) := simple-method-iep(limited-function-instance?-function);
    end unless;
    type
  else
    <function>
  end
end method;

define inline method limits (lf :: <limited-function>) => (result == <function>)
  <function>
end method;

//// Instance? relationships

define function limited-function-instance?-function
    (f, lf :: <limited-function>) => (result :: <boolean>)
  if (instance?(f, <function>))
    let f :: <function> = f;
    let signature = lf.limited-function-signature;
    congruent?(signature, function-signature(f))
  else
    #f
  end if
end function;

define method instance?-function (t :: <limited-function>) => (m :: <method>)
  limited-function-instance?-function
end method;

//// Subtype? relationships

// With other limited function types

define method subtype?
    (lf1 :: <limited-function>, lf2 :: <limited-function>)
 => (result :: <boolean>)
  #f
end method;

define method subjunctive-subtype? (lf1 :: <limited-function>, lf2 :: <limited-function>,
                                    scu :: <subjunctive-class-universe>)
 => (result :: <boolean>)
  subtype?(lf1, lf2)
end method;


// With other function types

define method subtype?
    (class :: <class>, lf :: <limited-function>) => (result == #f)
  #f
end method;

define method subtype?
    (lf :: <limited-function>, class :: <class>) => (result :: <boolean>)
  subclass?(limits(lf), class)
end method;

define method subjunctive-subtype? (class :: <class>, lf :: <limited-function>,
                                    scu :: <subjunctive-class-universe>)
 => (result == #f)
  #f
end method;

define method subjunctive-subtype? (lf :: <limited-function>, class :: <class>,
                                    scu :: <subjunctive-class-universe>)
 => (result :: <boolean>)
  subjunctive-subtype?(limits(lf), class, scu)
end method;

//// Disjointness

define method disjoint-types-1? (t1 :: <limited-function>, t2 :: <limited-function>,
                                 scu :: <subjunctive-class-universe>,
                                 dep :: <false-or-dependent-generic-function>)
 => (well? :: <boolean>)
  // For now, just assume any 2 limited functions are disjoint.
  #t
end method;


///// Potential instance relationships

define method has-instances? (class :: <class>, lf :: <limited-function>,
                              scu :: <subjunctive-class-universe>)
 => (some? :: <boolean>, all? == #f)
  values(subjunctive-subtype?(<function>, class, scu) | subjunctive-subtype?(class, <function>, scu), #f)
end method;
