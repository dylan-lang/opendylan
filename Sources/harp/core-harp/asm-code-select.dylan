module:    main-harp
Synopsis:  HARP code selection entry point.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// A file in the asm package to hold the code selector.
///
/// Contrived 15/1/88 by AJW

//// new version of the DSM. we the actual block to record-resume points,
//// that in can calculate the living virtual registers over the call

/// Under the new instruction scheme code selection takes place after
/// linearization, hence we can gather address offset information here.
/// CIM 24/11/88


//// !! The migration code is relying on bb-seen to point to the fill-pointer
//// !! of *code-vector*

/// there is now a macro bb-code-vector which accesses the appropriate slot in
/// bbs pointing to the fill pointer of *code-vector* (cim 25/7/90)

/// (cim 25/7/90)



// default is to not do scheduling

define open generic do-scheduling 
    (backend :: <harp-back-end>) => (b :: <boolean>);

define method do-scheduling (backend :: <harp-back-end>) => (b :: <boolean>)
  #f
end;



define method find-defining 
    (sv :: <instructions-vector>, ins :: <integer>) => (b :: <boolean>)
  block (return)
    for-instruction-defs (d in sv at ins)
      if (d) return(#t) end;
    end for-instruction-defs;
    #f;
  end block;
end method;



define method code-select-program 
    (backend :: <harp-back-end>,
     blk-s-vector :: <simple-basic-block-vector>,
     blk-num :: <integer>)

  let vars = backend.variables;
  let sv-ins = vars.sv-instructions;
  let total-fixed-offset :: <integer> = 0;

  vars.prev-instruction-can-be-moved := #f;
  vars.current-instruction-can-be-moved := #f;

  for (a-block :: <basic-block> in blk-s-vector, i from 0 below blk-num)
    if (empty?(a-block.bb-prev-set.tail))
      a-block.bb-prev-set := #();
      // when it is nil guaranteed not to be jumped to Y 2/1/92
    end if;
  end for;

  for (blk :: <basic-block> in blk-s-vector, i :: <integer> from 0 below blk-num)
    let state :: <integer> = blk.bb-stack-state;
    vars.current-block := blk;
    vars.with-stack := (~ vars.optimize-leaf-case) | (state == $with-stack-state);
    vars.block-fixed-offset := 0;
    vars.in-back-end := #t;
	  
    blk.bb-fixed-offset := total-fixed-offset;
    blk.bb-preceding-sdis := vars.all-the-sdis.size;
    blk.bb-code-ptr := vars.code-vector.size;

    for-instructions-in-basic-block (ins in blk)
      let op :: <op> =  ins-op(sv-ins, ins);
      if ((~ op.op-eliminatable) | find-defining(sv-ins, ins))
         /// this actually selects for each instruction
         let spread = op.op-spread;
         spread(backend, op, op.op-code-gen-fn, sv-ins, ins);
      end if;

      vars.prev-instruction-can-be-moved := current-instruction-can-be-moved;
      vars.current-instruction-can-be-moved := #f;
    end for-instructions-in-basic-block;

    if (zero?(vars.block-fixed-offset) & ~ empty?(blk.bb-prev-set))
      vars.prev-instruction-can-be-moved := #f;
    end if;
    let maybe-tag = blk.bb-branch-inf;
    if (instance?(maybe-tag, <tag>))
      harp-out (backend) bra(backend, maybe-tag) end;
    end if;
    inc!(total-fixed-offset, vars.block-fixed-offset);
  end for;

  do-any-scheduling(backend, total-fixed-offset);
end method;


define method do-any-scheduling
    (backend :: <harp-back-end>, total-fixed-offset :: <integer>)
  // By default we don't do any scheduling.
  let vars = backend.variables;
  values(vars.code-vector, total-fixed-offset);
end method;



// machine independent machinery for generating code


define method emit-1
    (backend :: <harp-back-end>, word,
     #key increment :: <integer> = backend.code-item-increment)
  let vars = backend.variables;
  add!(vars.code-vector, word);
  inc!(vars.block-fixed-offset, increment);
end method;

// this next used to allow 32-bit parcels to never be bignums


define method emit-2 
    (backend :: <harp-back-end>, hi, lo)
  let vars = backend.variables;
  let vec = vars.code-vector;
  add!(vec, hi);
  add!(vec, lo);
  inc!(vars.block-fixed-offset, backend.code-item-increment);
end method;



define method emit-sdi (backend :: <harp-back-end>, sdi :: <new-sdi>)
  let vars = backend.variables;
  let vec = vars.code-vector;
  let all-sdis = vars.all-the-sdis;
  let curr = vars.current-block;

  // set up some fields in the sdi
  sdi.new-sdi-fixed-offset := vars.block-fixed-offset + curr.bb-fixed-offset;
  sdi.new-sdi-preceding-sdis := size(all-sdis);
  sdi.new-sdi-my-block := curr;

  let target-bb :: <basic-block> = find-bb(backend, sdi.new-sdi-dest-tag);
  let prev-set = target-bb.bb-prev-set;
  target-bb.bb-prev-set := pair(sdi, prev-set); // IS THIS SAFE????

  add!(all-sdis, sdi);
  
  // code vector stuff
  add!(vec, sdi);
  inc!(vars.block-fixed-offset, sdi.new-sdi-cached-size);
end method;



define method emit-constant-ref 
     (backend :: <harp-back-end>,
      constant-ref :: <constant-reference>,
      #key high?, low?)
  // Emit a reference to a constant. A constant-reference should be
  // supplied, and from that an absolute label-constant will be created.
  let ins-size = backend.labelled-constant-increment;
  let class =
    case
      high? => <labelled-absolute-constant-high>;
      low? => <labelled-absolute-constant-low>;
      otherwise => <labelled-absolute-constant>;
    end;
  let label 
    = make(class,
           size: ins-size,
           constant-reference: constant-ref);
  emit-labelled-constant(backend, label, ins-size);
end method;


define method emit-constant-ref-relative 
     (backend :: <harp-back-end>,
      constant-ref :: <constant-reference>)
  // Emit a reference to a constant. A constant-reference should be
  // supplied, and from that a relative label-constant will be created.
  let ins-size = backend.labelled-constant-increment;
  let label 
    = make(<labelled-relative-constant>, 
           size: ins-size,
           constant-reference: constant-ref);
  emit-labelled-constant(backend, label, ins-size);
end method;

define method emit-constant-ref-with-opcode
     (backend :: <harp-back-end>,
      opcode :: <symbol>,
      value :: <integer>,
      constant-ref :: <constant-reference>,
      ins-size :: <integer>)
  // Emit a reference to a n opcode with a constant. 
  // A symbolic opcode and a constant-reference should be
  // supplied
  let label 
    = make(<labelled-constant-with-opcode>, 
           size: ins-size,
           constant-reference: constant-ref,
           opcode: opcode,
           value: value);
  emit-labelled-constant(backend, label, ins-size);
end method;


define method emit-labelled-constant
    (backend :: <harp-back-end>, label :: <labelled-constant>, increment :: <integer>)
  // Emit a labelled constant - which may be either absolute or relative.
  let vars = backend.variables;
  let vec = vars.code-vector;
  add!(vec, label);
  inc!(vars.block-fixed-offset, increment);
end method;
