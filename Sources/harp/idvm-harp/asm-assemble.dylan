module:    idvm-harp
Synopsis:  IDVM -> DOSS assembly code genmeration
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




/// The default outputter type is DOSS.


define method default-harp-output-type
    (backend :: <idvm-back-end>) => (type)
   #"doss";
end method;


// ensure interactive outputters don't try to print assembler -
// because we do that through the print-harp outputter anyway.
// Instead, we multiplex an interactive-print-harp and a doss

define method make-harp-outputter-by-type
    (backend :: <idvm-back-end>, filename, type == #"interactive")
    => (output-stream :: <harp-multiple-outputter>)
  let dossname = filename | "dylan-form";
  multiplex-outputters
    (make-harp-outputter-by-type(backend, filename, #"interactive-print-harp"),
     make-harp-outputter-by-type(backend, dossname, #"doss"));
end method;



/// Unusually, this back-end is not able to complete the code-building process.
/// The back-end can generate a code-vector on it's own - but it requires
/// effort by the client of the back-end to make a method object out of the 
/// code vector before dumping it.
///
/// Support for this mechanism is provided via INVOKE-HARP, which returns
/// the code vector and size of the locals vector as two values.


// The values are returned from INVOKE-HARP by specializing POST-CG-LAMBDA
//
define method post-cg-lambda
       (backend :: <idvm-back-end>, print-stream, asm-stream) 
    => (code-vector :: <simple-object-vector>, locals-size :: <integer>)
  next-method();
  values(backend.variables.code-vector.resolve-code-vector,
         backend.locals-vector-size);
end method;




// RESOLVE-CODE-VECTOR
//
// This is the function which builds the real IDVM code vector
//
define method resolve-code-vector 
    (code :: <stretchy-vector>) => (resolved-code :: <simple-object-vector>)
  let code-size = 1;  // allow for the info object at index 0
  let index     = 1;  // allow for the info object at index 0
  process-all-code(code, method (obj) code-size := code-size + 1 end);
  let resolved-code = make(<simple-object-vector>, size: code-size);
  let add-code-item =
    method (obj)
      resolved-code[index] := obj;
      index := index + 1;
    end method;
  process-all-code(code, add-code-item);
  resolved-code;
end method;


define method process-all-code 
    (code :: <stretchy-vector>, action :: <function>) => ()
  for (elt in code)
    process-code-element(elt, action);
  end for;
end method;

define method process-code-element 
    (operand :: <object>, action :: <function>) => ()
 action(operand);
end method;

define method process-code-element 
    (operand :: <idvm-hilo>, action :: <function>) => ()
 action(encode-hilo(operand));
end method;

define method process-code-element 
    (operand :: <new-sdi>, action :: <function>) => ()
  for (elt in operand.new-sdi-code-holder)
    process-code-element(elt, action);
  end for;
end method;


define method encode-hilo (operand :: <idvm-hilo>) => (res :: <integer>)
  let hi :: <integer> = operand.idvm-hi;
  let lo :: <integer> = operand.idvm-lo;
  lo + ash(hi, 16);
end method;
