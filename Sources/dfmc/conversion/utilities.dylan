Module:   dfmc-conversion
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define inline method make-with-temporary*
    (env :: <environment>, class :: <class>, #rest initargs, #key, #all-keys)
  let (computation, temporary)
    = apply(make-with-temporary, env, class, initargs);
  values(computation, computation, temporary)
end method make-with-temporary*;

/// CONSTANT-VALUE? NOW IN TYPIST-INFERENCE

define function constant-value (ref :: <value-reference>)
 => (constant-value)
  let (cv?, value) = constant-value?(ref);
  value
end;

define generic function-value (c :: <call>) => (fv :: <object>);

define method function-value (c :: <function-call>) => (fv :: <object>)
  constant-value(c.function)
end method;

define method function-value (c :: <primitive-call>) => (fv :: <&primitive>)
  c.primitive
end method;

define function call-effective-function (c :: <function-call>)
  let funct = function-value(c);
  if (call-iep?(c))
    iep(function(funct))
  else
    funct
  end
end function;

define method extractable-constant-value? (ref :: <temporary>)
 => (constant-value? :: <boolean>, constant-value)
  values(#f, #f)
end method;


define method extractable-constant-value? (ref :: <value-reference>)
 => (constant-value? :: <boolean>, constant-value)
  let (constant-value?, constant-value) = constant-value?(ref);
  if (constant-value?)
    values(if (instance?(constant-value, <&lambda-or-code>))
	     let lambda = if (instance?(constant-value, <&code>))
			    function(constant-value)
			  else
			    constant-value
			  end;
	     lambda-top-level?(lambda) |
	       // this is the only user
	       size(users(iep(lambda))) +
	       size(users(lambda)) = 1
	   else
	     #t
	   end,
	   constant-value)
  end
end;

define function extract-constant (ref :: <value-reference>) => (constant-value)
  // assumes that above is true
  let constant-value = constant-value(ref);
  if (instance?(constant-value, <&lambda-or-code>))
    // splice lambda out of the outer lambda
    unless (lambda-top-level?(constant-value))
      extract-lambda(constant-value);
    end;
  end;
  constant-value
end;
