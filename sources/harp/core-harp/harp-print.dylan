module:    print-harp
Synopsis:  Printing support for HARP object types.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Printing support for HARP object types 


// Printing TAGs


define method print-object
   (object :: <tag>, stream :: <stream>) => ()
  let bck = object.tag-bb;
  let start = if (bck) bck.bb-start end;
  format(stream, "{tag %=:%s}", object.tag-no, start);
end;

// Printing OPs


define method print-object
   (object :: <op>, stream :: <stream>) => ()
  format(stream, "{op %s}", object.op-name);
end;


// Printing virtual registers


define method print-object
   (object :: <greg>, stream :: <stream>) => ()
  print-register(object, "G", stream);
end;

define method print-object
   (object :: <nreg>, stream :: <stream>) => ()
  print-register(object, "N", stream);
end;

define method print-object
   (object :: <sfreg>, stream :: <stream>) => ()
  print-register(object, "SF", stream);
end;

define method print-object
   (object :: <dfreg>, stream :: <stream>) => ()
  print-register(object, "DF", stream);
end;


define method print-register
   (reg :: <virtual-register>, prefix :: <byte-string>, stream)
  if (reg.virtual-register-name)
    format(stream, "%=/", reg.virtual-register-name);
  end if;
  unless (reg.virtual-register-named-indirections.empty?)
    format(stream, "%=/", reg.virtual-register-named-indirections);
  end unless;
  format(stream, "%s%s/%s", prefix, 
         reg.virtual-register-id, reg.virtual-register-colour);
end;


// Printing real registers

define method print-object
   (object :: <real-register>, stream :: <stream>) => ()
  format(stream, "%s", object.real-register-pname);
end;


// Printing Spills


define method print-object
   (object :: <gspill>, stream :: <stream>) => ()
  format(stream, "GSPILL-%s", object.spill-offset);
end;

define method print-object
   (object :: <nspill>, stream :: <stream>) => ()
  format(stream, "NSPILL-%s", object.spill-offset);
end;

define method print-object
   (object :: <sfspill>, stream :: <stream>) => ()
  format(stream, "SFSPILL-%s", object.spill-offset);
end;

define method print-object
   (object :: <dfspill>, stream :: <stream>) => ()
  format(stream, "DFSPILL-%s", object.spill-offset);
end;


// Printing SDIs


define method print-object
   (sdi :: <new-sdi>, stream :: <stream>) => ()
  format(stream, "{sdi:%=}", sdi.new-sdi-preceding-sdis);
end;



// Printing constant references

define method constant-reference-class-name 
    (ref :: <constant-reference>) => (name :: <byte-string>)
  "unknown type of constant";
end method;

define method constant-reference-class-name 
    (ref :: <i-address-constant-reference>) => (name :: <byte-string>)
  "constant";
end method;

define method constant-reference-class-name 
    (ref :: <i-indirect-constant-reference>) => (name :: <byte-string>)
  "indirect-constant";
end method;

define method constant-reference-class-name 
    (ref :: <sf-address-constant-reference>) => (name :: <byte-string>)
  "single float constant";
end method;

define method constant-reference-class-name 
    (ref :: <sf-indirect-constant-reference>) => (name :: <byte-string>)
  "single float indirect-constant";
end method;

define method constant-reference-class-name 
    (ref :: <df-address-constant-reference>) => (name :: <byte-string>)
  "double float constant";
end method;

define method constant-reference-class-name 
    (ref :: <df-indirect-constant-reference>) => (name :: <byte-string>)
  "double float indirect-constant";
end method;

define method constant-reference-full-name 
    (ref :: <imported-constant-reference>) => (name :: <byte-string>)
  concatenate("imported ", ref.constant-reference-class-name);
end method;

define method constant-reference-full-name 
    (ref :: <constant-reference>) => (name :: <byte-string>)
  ref.constant-reference-class-name;
end method;

define method constant-reference-full-name 
    (ref :: <interactor-constant-reference>) => (name :: <byte-string>)
  "interactor constant";
end method;

define method print-object
  (ref :: <constant-reference>, stream :: <stream>) => ()
  let offset = ref.cr-const-offset;
  let name = ref.cr-refers-to;
  let type-name = ref.constant-reference-full-name;
  format(stream,
	 "{%s %=%s}",
         type-name,
         name,
         if (offset == 0) 
           "" 
         else 
           if (offset > 0) format-to-string("+%=", offset) else offset end;
         end);
end method;

// Printing labelled constants

define method print-object
  (lab :: <explicit-labelled-constant>, stream :: <stream>) => ()
  let ref = lab.labelled-constant-reference;
  let offset = ref.cr-const-offset;
  let name = ref.cr-refers-to;
  format(stream,
	 "{%s %=%s}",
         print-name-for-label(lab),
         name,
         if (offset == 0) 
           "" 
         else 
           if (offset > 0) format-to-string("+%=", offset) else offset end;
         end);
end;


define method print-name-for-label 
    (lab :: <labelled-absolute-constant>) => (s :: <byte-string>)
  "label";
end method;

define method print-name-for-label 
    (lab :: <labelled-relative-constant>) => (s :: <byte-string>)
  "relative-label";
end method;


define method print-name-for-label 
    (lab :: <labelled-constant-with-opcode>) => (s :: <byte-string>)
  format-to-string("op-label %s", lab.opcode);
end method;


define method print-object
  (lab :: <relative-address-constant>, stream :: <stream>) => ()
  let offset = lab.relative-offset;
  format(stream, "{relative-address +%=}", offset);
end;


// Printing basic blocks


define method print-object
   (bb :: <basic-block>, stream :: <stream>) => ()
  format(stream, "{basic-block %= %= %=}", bb.bb-taags, bb.bb-start, bb.bb-end);
end;


/////


define method print-basic-blocks (backend :: <harp-back-end>)
  let stream = *standard-output*;
  bb-print-basic-blocks(backend, backend.variables.top-block, stream);
end;

define method bb-print-basic-blocks
    (backend :: <harp-back-end>, top-block :: <basic-block>, stream)
  let ones-done = #();
  let vec = backend.variables.sv-instructions;
  local method bb-print-dfs (bb :: <basic-block>)
          if (member?(bb, ones-done))
            format(stream, "  (BRA %s)\n", bb.bb-taags[0]);
          else
	    format(stream, "---------- %=\n", bb.bb-taags);
            print-instructions-in-range(stream, backend, 
                                        bb.bb-start, bb.bb-end);
	    // We have to find some more blocks to do
            push!(bb, ones-done);
            for (b in bb.bb-next-set)
              bb-print-dfs(b);
            end for;
          end if;
        end method;
  bb-print-dfs(top-block);
end method;


define method print-linearised-harp (backend :: <harp-back-end>, 
                                     stream :: <stream>, 
                                     blk-vector :: <simple-basic-block-vector>, 
                                     blk-num :: <integer>) => ()
  let vars = backend.variables;
  let name = vars.function-name;
  let name = if (instance?(name, <string>)) name
	     else model-object-as-string(name) end;
  format(stream, "\n+++ starting code for %s +++\n", name);
  for (x from 0 below blk-num,
       bb :: <basic-block> in blk-vector)
    format(stream, "\n%= [%=->%=] (level:%s)" ,
           bb.bb-taags, bb.bb-start, bb.bb-end, bb.bb-loop-depth - 1);
    if (vars.optimize-leaf-case)
      format(stream, " is %s with stack state %s", 
             bb.print-bb-colour, bb.print-bb-stack-state);
    end if;
    format(stream, "\n%=\n", bb.bb-next-set);
    print-instructions-in-range(stream, backend,
                                bb.bb-start, bb.bb-end);
  end for;
  format(stream, "\n+++ ending code for %s +++", name);
end;

define method try-to-linearise-and-print-harp (backend :: <harp-back-end>,
                                               stream :: <stream>) => (res)
  block (return)
    let (bv, bn) = linearise(backend, backend.variables.top-block);
    print-linearised-harp(backend, stream, bv, bn);
    return(#t);
  exception (<error>)
    format(stream, 
           "\n+++ Failed to linearise HARP. Using simple print instead +++\n");
    return(#f);
  end block;
end method;

define method print-instructions  (backend :: <harp-back-end>, 
                                   #key linearise? = #t, 
                                   stream = *standard-output*)
  unless (linearise? & try-to-linearise-and-print-harp(backend, stream))
    format(stream, "\n+++ Simple HARP print using order of generation: +++\n");
    print-instructions-in-range(stream, 
                                backend,
                                0,
                                backend.variables.fp-instructions);
  end unless;
end;

define method print-instructions-in-range
    (stream, backend :: <harp-back-end>, 
     start :: <integer>, finish :: <integer>)
  for (ins from start below finish by instruction-size)
    print-instruction(stream, backend, ins); 
    format(stream, "\n");
  end;
end;

define method print-instruction 
    (stream, backend :: <harp-back-end>, ins :: <integer>)
  local method print-operands (backend, op, #rest operands)
          let limit = operands.size - 1;
          format(stream, "  %s\t", op.op-name);
          unless (limit < 0)
            for (i from 0 below limit)
              format(stream, "%=, ", operands[i]);
            finally format(stream, "%=", operands[limit]);
            end for;
          end unless;
        end method;
  let sv = backend.variables.sv-instructions;
  let op :: <op> = ins-op(sv, ins);
  let spread = op.op-spread;
  spread(backend, op, print-operands, sv, ins);
end method;

