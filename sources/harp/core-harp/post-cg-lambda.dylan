module:    main-harp
Synopsis:  Top level support for invoking HARP.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Do the register allocation, assembly etc. At this point we have the
/// basic blocks making up the code, the constants vector and the name.
/// We have to turn this into real code and then perform the magic to dump
/// this out to a file. BB is the top block, BLOCKS is the block-hash
/// table, LAMBDA is only passed through to generate-debug-info.


// pre-cg-lambda gives us a dummy first block - which ensures
// that the leafcase analysis will always be able to put the stack-build
// in before any real code, if necessary.

define method pre-cg-lambda (backend :: <harp-back-end>) => ()
  call-instruction(rem, backend, "This is a dummy first basic block.");
  make-fall-thru-bb(backend);
end method;


/// Y 13-jul-93 make not print anything when function name is nil 

/// Y 24Nov93. better check for defstruct stuff before printing. Maybe
/// we should block any subfunction ?

//   "Do the register allocation, assembly etc. At this point we have the
// basic blocks making up the code, the constants vector and the name.
// We have to turn this into real code and then perform the magic to dump
// this out to a file. BB is the top block
// LAMBDA is only passed through to generate-debug-info, NAME,
// TYPE, NAME-TO-DUMP-AS I don't yet know much about."


define open generic post-cg-lambda
   (backend :: <harp-back-end>, outputter, harp-debug?, public?, export?, 
    source-locator, section) 
   => (compiled-lambda :: <compiled-lambda>);



define method post-cg-lambda
    (backend :: <harp-back-end>, outputter, harp-debug?, public?, export?, 
     source-locator, section)
    => (compiled-lambda :: <compiled-lambda>)
  let vars = backend.variables;
  let bb :: <basic-block> = vars.top-block;
  let print-stream 
    = if (harp-debug?) 
        make(<string-stream>, direction: #"output", 
             contents: make(<byte-string>, size: 1000));
      else #f 
      end;
  code-gen-from-block(backend, bb, print-stream);
  resolve-sdis(backend);
  let print-info
    = if (harp-debug?) 
        stream-contents-as(<byte-string>, print-stream)
      else #f 
      end;
  let compiled-lambda 
    = assemble-compiled-lambda(backend, print-info, public?, export?, source-locator);
  if (outputter)
    output-compiled-lambda(backend, outputter, compiled-lambda, section: section);
  end if;
  compiled-lambda;
end method;

// define method emit-back-end-variables-data
//     (backend :: <harp-back-end>) => ()
//   // format-out("\n### HARP DATA \n");
//   format-out("\n### SV-INSTRUCTIONS %= (%=)\n",
// 	     backend.variables.fp-instructions,
// 	     backend.variables.sv-instructions.size);
//   let result =
//     as(<single-float>,
//        (backend.variables.fp-instructions / 
// 	  as(<single-float>, truncate/(estimate-harp-instructions-size(backend, backend.variables.function-name), 25))));
//   format-out("\n### SV-INSTRUCTIONS RATIO %=\n", result);
// 
//   slot-initialized?(backend.variables, code-vector)
//     & (begin
// 	format-out("\n### CODE-VECTOR %=\n", backend.variables.code-vector.size);
// 	let result =
// 	   (as(<single-float>, backend.variables.code-vector.size) /
// 	      as(<single-float>, backend.variables.fp-instructions));
// 	format-out("\n### CODE-VECTOR RATIO %=\n", result);
//        end);
//   /*
//   format-out("\n### ALL-THE-SDIS %=\n", backend.variables.all-the-sdis.size);
//   slot-initialized?(backend.variables, pgm-vect)
//     & (begin
// 	 format-out("\n### PGM-VECT %=\n", backend.variables.pgm-vect.size);
// 	 format-out("### BASIC BLOCKS: %s\n",
// 		    begin
// 		      let string :: <string> = "";
// 		      do(method(bb :: <basic-block>)
// 			     string :=
// 			     concatenate(string, ", ",
// 					 if (instance?(bb.bb-live-entry, <set-thingy>)
// 					       & instance?(bb.bb-live-entry.set-thingy-vect, <vector-32bit>))
// 					   integer-to-string(bb.bb-live-entry.set-thingy-vect.size)
// 					 else "" end,
// 					 ".",
// 					 if (instance?(bb.bb-defs, <set-thingy>)
// 					       & instance?(bb.bb-defs.set-thingy-vect, <vector-32bit>))
// 					   integer-to-string(bb.bb-defs.set-thingy-vect.size)
// 					 else "" end);
// 			 end,
// 			 backend.variables.pgm-vect);
// 		      string
// 		    end);
//        end);
//   format-out("\n### NAMED-VARIABLES-TABLES %= %= %=\n",
// 	     backend.variables.named-variables-tables[0]
// 	       & backend.variables.named-variables-tables[0].size,
// 	     backend.variables.named-variables-tables[1]
// 	       & backend.variables.named-variables-tables[1].size,
// 	     backend.variables.named-variables-tables[2]
// 	       & backend.variables.named-variables-tables[2].size);
//   slot-initialized?(backend.variables.vreg-state, vr-vect)
//     & (begin
// 	 format-out("\n### VR-VECT %=\n", backend.variables.vreg-state.vr-vect.size);
// 	 format-out("### VIRTUAL REGISTERS: %s\n",
// 		    begin
// 		      let string :: <string> = "";
// 		      do(method(reg :: <virtual-register>)
// 			     string :=
// 			     concatenate(string, ", ",
// 					 integer-to-string(reg.virtual-register-named-indirections.size),
// 					 ".",
// 					 integer-to-string(reg.virtual-register-colour-pref.size),
// 					 ".",
// 					 if (instance?(reg.virtual-register-clashes, <vector-32bit>))
// 					   integer-to-string(reg.virtual-register-clashes.size)
// 					 else "" end)
// 			 end,
// 			 backend.variables.vreg-state.vr-vect);
// 		      string
// 		    end);
//        end);
//   format-out("\n### GREEN-VR-VECT %=\n", backend.variables.vreg-state.green-vr-vect.size);
//   format-out("\n### NEXT-VREG-ID %=\n", backend.variables.vreg-state.next-vreg-id);
//   slot-initialized?(backend.variables.vreg-state, gregs-to-reuse)
//     & format-out("\n### GREGS-TO-REUSE %=\n", backend.variables.vreg-state.gregs-to-reuse.size);
//   slot-initialized?(backend.variables.vreg-state, nregs-to-reuse)
//     & format-out("\n### NREGS-TO-REUSE %=\n", backend.variables.vreg-state.nregs-to-reuse.size);
//   */
// end method;


// define method emit-pgm-data(pgm-vect :: <stretchy-basic-block-vector>) => ()
//   format-out("\n### PGM-VECT %=\n", pgm-vect.size);
//   format-out("### BASIC BLOCKS: %s\n",
// 	     begin
// 	       let string :: <string> = "";
// 	       do(method(bb :: <basic-block>)
// 		      string :=
// 		      concatenate(string, ", ",
// 				  reduce
// 				    (method (string :: <string>, word :: <machine-word>)
// 				       concatenate(string, ".",
// 						   machine-word-to-string(word));
// 				     end,
// 				     "",
// 				     bb.bb-live-entry.set-thingy-vect));
// 		  end,
// 		  pgm-vect);
// 	       string
// 	     end);
// end method;
