Module: dfmc-management
Author: Jonathan Bachrach, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Compilation unit reset.

// Utility for resetting the state of a compilation unit.

define method reset (unit :: <compilation-unit>)
  // *id* := -1;
  /* Hack!!! The following is too slow, and we don't have clear!
  for (object in unit.literals.key-sequence)
    remove-key!(unit.literals, object);
  end for;
  */
  unit.roots := make(<table>);
  unit.objects := make(<table>);
  unit.compile-stage-literals := make(<table>);
  unit.variables.size := 0;
  unit.top-level-lambda := #f;
  // register-object(unit, &eval(*global-environment*, #"<class>"));
  run-registration-thunks();
  for (object in vector(&true, &false))
    register-object(unit, object)
  end;
end method;

//// Root object registration.

define method find-literal (unit :: <compilation-unit>, object) => (object)
  element(unit.compile-stage-literals, object, default: #f) 
  | (unit.compile-stage-literals[object] := register-object(unit, object))
end method;

define method register-variable 
    (unit :: <compilation-unit>, variable :: <module-binding>) => ()
  add-new!(unit.variables, variable);
  if (~instance?(variable.value, <unknown>))
    register-object(unit, variable.value);
  end;
end method;

define method register-top-level-lambda 
    (unit :: <compilation-unit>, lambda :: <&top-level-lambda>) => ()
  unit.top-level-lambda := lambda
end method;

define method register-object 
    (unit :: <compilation-unit>, object :: <virtual-object>)
  object
end method;

define method register-object 
    (unit :: <compilation-unit>, object :: <direct-object>)
  object
end method;

define method register-object 
    (unit :: <compilation-unit>, object :: <indirect-object>)
  unless (element(unit.roots, object, default: #f))
    unit.roots[object] := object;
  end;
  object
end method;

define method register-object 
    (unit :: <compilation-unit>, object :: <object>)
  let &object = make-compile-time-literal(object);
  unit.roots[&object] := &object;
  &object
end method;

//// Object tracing.

define method internal? 
    (unit :: <compilation-unit>, object :: <emitted-object>)
  ~object.emit-state | (object.emit-state & object.emit-state == unit);
end method;

define method mark-internal 
    (unit :: <compilation-unit>, object :: <emitted-object>)
  object.emit-state := unit
end method;

define method unmark
    (unit :: <compilation-unit>, object :: <emitted-object>)
  object.emit-state := #f
end method;

// trace-objects:
//
//   Trace from the roots of a compilation unit to generate the sets of
//   internal and directly referenced external objects for use in object
//   dumping.

// trace-object:
//
//   Trace from this object, testing whether the object in hand is 
//   internal or external to this compilation unit and calling the
//   appropriate protocol function.

// trace-internal-object:
//
//   Trace from this object known to be internal to the given 
//   compilation unit. This involves adding the object to the
//   set of internal objects of the compilation unit and descending
//   its references.

// trace-external-object:
//
//   Add this object to the set of external objects referenced directly
//   by objects internal to this compilation unit.

// trace-object-references:
//
//   Invoke the tracer on the slot values of the given object.

define generic trace-objects 
    (unit :: <compilation-unit>) => ();

define generic trace-object 
    (unit :: <compilation-unit>, object :: <object>) => (object :: <object>);

define generic trace-internal-object
    (unit :: <compilation-unit>, object :: <object>) => (object :: <object>);
define generic trace-external-object
    (unit :: <compilation-unit>, object :: <object>) => (object :: <object>);

define generic trace-object-references
    (unit :: <compilation-unit>, object :: <object>) => ();

// Default methods.

define method trace-objects (unit :: <compilation-unit>) => ()
  // Reset any information from a previous trace.
  for (object in unit.objects)
    unmark(unit, object);
  end;
  unit.objects := make(<table>);
  unit.external-objects := make(<table>);
  // Do the new trace.
  for (variable in unit.variables)
    if (~instance?(variable.value, <unknown>))
      trace-object(unit, variable.value)
    end
  end;
  for (object in unit.roots)
    trace-object(unit, object)
  end;
end method;

define method trace-object 
    (unit :: <compilation-unit>, object :: <direct-object>) => (object)
  object
end method;

define method trace-object 
    (unit :: <compilation-unit>, object :: <emitted-object>) => (object)
  if (~internal?(unit, object))
    trace-external-object(unit, object)
  else
    trace-internal-object(unit, object)
  end
end method;

// This is called when tracing the references contained within a body
// of code.

define method trace-object 
    (unit :: <compilation-unit>, object :: <module-binding>) => (object)
  if (~member?(object, unit.variables, default: #f))
    next-method();
  end
end method;

define method trace-object 
    (unit :: <compilation-unit>, object :: <object>) => (object)
  let &object = make-compile-time-literal(object);
  unit.objects[&object] := &object;
  trace-object-references(unit, &object);
  &object
end method;

define method trace-internal-object
    (unit :: <compilation-unit>, object :: <emitted-object>)
 => (object, traced?)
  if (~element(unit.objects, object, default: #f))
    unit.objects[object] := object;
    mark-internal(unit, object);
    trace-object-references(unit, object);
    values(object, #t)
  else
    values(object, #f)
  end;
end method;

define method trace-external-object
    (unit :: <compilation-unit>, object :: <emitted-object>) => (object)
  if (~element(unit.external-objects, object, default: #f))
    unit.external-objects[object] := object;
    // The following is necessary since extern declarations of objects
    // are typed.
    if (instance?(object, <indirect-object>))
      trace-object(unit, object.&object-class); 
    end;
  end;
  object
end method;

// Hack!!! This should be done in a more uniform way:

define method trace-computation
    (unit :: <compilation-unit>, c :: <object-reference>) => (object)
  trace-object(unit, c.value);
end method;

define method trace-computation
    (unit :: <compilation-unit>, c :: <variable-reference>) => (object)
  trace-object(unit, c.binding);
end method;

define method trace-computation
    (unit :: <compilation-unit>, c) => (object)
end method;

// !@#$ seems like a wart

define method trace-computation
    (unit :: <compilation-unit>, c :: <primitive-call>) => (object)
  trace-object(unit, c.primitive);
end method;

// !@#$ seems like a wart

define method trace-computation
    (unit :: <compilation-unit>, c :: <c-variable-pointer-call>) => (object)
  trace-external-object(unit, c.c-variable);
end method;

define method trace-internal-object 
    (unit :: <compilation-unit>, l :: <&lambda>) => (object)
  let (reg-l, traced?) = next-method();
  if (traced?)
    for-computations (c in l)
      trace-computation(unit, c);
    end for-computations;
    for-lambda (sub-f in l)
      trace-object(unit, sub-f)
    end for-lambda;
  end unless;
  reg-l
end method;

/*
define method trace-object-references 
    (unit :: <compilation-unit>, object :: <indirect-object>) => ()
  trace-object(unit, &object-class(object));
  for-slot-value (value in object)
    trace-object(unit, value)
  end;
end method;
*/

define method trace-object-references 
    (unit :: <compilation-unit>, object :: <module-binding>) => ()
  trace-object(unit, object.value);
end method;

// eof
