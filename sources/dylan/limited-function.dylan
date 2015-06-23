Module:    internal
Synopsis:  Limited function types
Author:    Bruce Mitchener, Jr.
Copyright:    Original Code is Copyright (c) 2014 Dylan Hackers.
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

