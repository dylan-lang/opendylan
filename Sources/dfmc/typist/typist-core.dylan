Module:    DFMC-Typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This code should be identical in all/both typists.
// Long term, we only intend to have one typist, so not worth abstracting into 
// some sort of "abstract typist" library.


define generic constant-value? (ref)
  => (constant? :: <boolean>, value :: <object>);

define method constant-value? 
  (ref :: <object-reference>)
   => (constant-value? :: <boolean>, constant-value)
  // Extract the constant from an <object-reference>.
  values(#t, reference-value(ref))
end method;

define method constant-value? 
  (ref :: <defined-constant-reference>)
   => (constant-value? :: <boolean>, constant-value)
  // Extract the constant from an <defined-constant-reference>.
  // TODO: DOESN'T HANDLE FALSE
  let value = computation-value(ref);
  if (value)
    let (inlineable?, inline-value) = inlineable?(value);
    if (inlineable?)
      values(#t, inline-value)
    else
      values(#f, #f)
    end if
  else
    values(#f, #f)
  end if;
end method;

define method constant-value?
    (ref :: <temporary>) => (constant-value? :: <boolean>, constant-value)
  // If this temporary is estimated as a singleton, extract the constant.
  let type = type-estimate(ref);
  if (instance?(type, <type-estimate-limited-instance>))
    values(#t, type-estimate-singleton(type))
  else
    values(#f, #f)
  end
end method;

define method constant-value?
  (ref :: <value-reference>) => (constant-value? :: <boolean>, constant-value)
  // Other kinds of <value-reference>s are not constants.
  values(#f, #f)
end method;

define function constant-value (ref :: <value-reference>)
 => (constant-value)
  let (cv?, value) = constant-value?(ref);
  value
end;
