module:    main-harp
Synopsis:  HARP linerisation support.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// A file in the asm package to hold the lineariser.
///
/// Created AJW 15/1/88, from the late '87 version.
///         AJW  3/8/88 parameterise branch block size for other processors
///         cim 24/2/89 eliminate any conditional branches with same tag and
///         		fall-thru
///         cim 6/3/90  eliminate any branches over branches by flipping test

/// The new strategy is to linearise the code 'blind', then assemble the tag
/// references as a separate phase (the relaxer, followed by the assembler).

/// The sdi lineariser takes a collection of blocks (containing sdis), and
/// linearises the result, returning a vector of blocks in the linearised
/// order.

define constant block-vector-size = 40;

/// As a preliminary, something to make a small block for the fixup branches.

define method make-branch-block 
    (backend :: <harp-back-end>, dest-tag :: <tag>) => (b :: <basic-block>)
  let fixup-block :: <basic-block> = make-bb(backend);
  make-current-this-bb(backend, fixup-block);

  // note that here we cannot use ins::bra directly as we want a real m/c
  // branch instruction
  let bra = op-element(backend.instructions, bra);
  output-instruction(backend, bra, dest-tag);

  finish-bb(backend);

  // and finally return...
  fixup-block;
end;


/* Add temporary queues for basic-block lists to eliminate consing;

   This removes unnecessary copying and concatenating of original lists
   while iterating over the entire collection

   Nosa  Jan 25, 1999 */

define abstract class <abstract-queue>(<object>)
end class;

define class <empty-queue>(<abstract-queue>)
end class;

define constant $empty-queue = make(<empty-queue>);

define class <queue>(<abstract-queue>)
  slot top-of-queue :: <list> = #(), init-keyword: top:;
  slot rest-of-queue :: <abstract-queue> = $empty-queue;
end class;


define inline method empty-queue() => (queue :: <queue>)
  make(<queue>);
end method;

define inline method empty-queue?(queue :: <abstract-queue>)
 => (empty? :: <boolean>)
  queue == $empty-queue
end method;

define inline method head-queue(queue :: <queue>) => (elt)
  queue.top-of-queue.head
end method;

define inline method tail-queue(queue :: <queue>) => (queue :: <queue>)
  let first-tail :: <list> = queue.top-of-queue.tail;
  if (first-tail.empty?)
    let queue-tail :: <abstract-queue> = queue.rest-of-queue;
    if (queue-tail.empty-queue?)
      queue.top-of-queue := first-tail;
      queue
    else queue-tail
    end;
  else
    queue.top-of-queue := first-tail;
    queue
  end;
end method;

define inline method add-queue
    (l :: <list>, queue-tail :: <queue>) => (queue-tail :: <queue>)
  if (l.empty?) queue-tail
  else
    let first :: <list> = queue-tail.top-of-queue;
    if (first.empty?)
      queue-tail.top-of-queue := l;
      queue-tail
    else
      queue-tail.rest-of-queue := make(<queue>, top: l);
    end;
  end;
end method;

define method linearise
    (backend :: <harp-back-end>,  starting-blk :: <basic-block>)
 => (blk-vector :: <simple-basic-block-vector>, blk-num :: <integer>)
  let vars = backend.variables;
  let sv-ins = vars.sv-instructions;
  let instructions = backend.instructions;
  let ins-bra = op-element(instructions, bra);


  // first do some tidying up
  for (bb :: <basic-block> in backend.variables.pgm-vect)
    bb.bb-seen := #f;
  end for;

  let length :: <integer> = block-vector-size - 2;
  let blk-vector = 
    make(<simple-basic-block-vector>, size: block-vector-size, fill: $empty-basic-block);
  let index :: <integer> = 0;
  let next-cand = #f;
  let queue :: <queue> = empty-queue();
  let queue-tail :: <queue> = queue;

  for (nblk = starting-blk then next-cand | queue.head-queue, until: nblk == #())
    // step onto next-cand or queue.head as appropriate

    let blk :: <basic-block> = nblk;
    unless (next-cand)
      queue := queue.tail-queue;
    end;
    next-cand := #f;

    unless (blk.bb-seen)	// unless we've been here
      blk.bb-seen := index;	// say where

      let empty-block :: <boolean> = #f;
      block (non-empty)
  
        for-instructions-in-basic-block (ins in blk)
          let op :: <op> = ins-op(sv-ins, ins);
          unless (op.op-is-rem |
                  (op.op-is-move & 
                   with-du (sv-ins at ins) colour(du-def(1)) == colour(du-uze(1)) end))
            non-empty(#f);
          end unless;
        end for-instructions-in-basic-block;
		
        let alias = blk.bb-fall-thru;
        if (alias)
          let alias-tags :: <list> = alias.bb-taags;
          let real-alias :: <basic-block> = 
            if (alias-tags == #()) alias else alias-tags.head.tag-bb end;
          let seen = real-alias.bb-seen;
          for (tag :: <tag> in blk.bb-taags)
            tag.tag-bb := real-alias;
            pushnew!(tag, real-alias.bb-taags);
          end for;
          if (seen)
            eliminate-local-branches(sv-ins, blk-vector, seen, real-alias);
          end if;
        end if;
  		
        for (prev :: <basic-block> in blk.bb-prev-set)
          if (prev.bb-seen & blk == prev.bb-fall-thru)
            non-empty(#f);
          end if;
        end for;

        empty-block := #t;
      end block;
	      
      if (index >= length)
	length := length + length;
	let new = make(<simple-basic-block-vector>, size: length, fill: $empty-basic-block);
        replace-subsequence!(new, blk-vector, end: blk-vector.size);
	blk-vector := new;
        dec!(length, 2);	// leave room for the ones below
      end if;
      blk-vector[index] := blk;
      inc!(index);

      // Now time to determine where we go next

      let fall-thru = blk.bb-fall-thru;

      if (fall-thru & ~ empty-block)
        let last-ins :: <integer> = blk.bb-end - instruction-size;
	let last-tag = ins-tag(sv-ins, last-ins);
        if (fall-thru.bb-seen)
	  let thru-tag :: <tag> = fall-thru.bb-taags.head;
          let last-ins-op :: <op> = ins-op(sv-ins, last-ins);
          let reverse = last-ins-op.op-reverse-op;
          if (reverse)
	    // block ends with a conditional branch instruction of some sort
	    if (last-tag == thru-tag)
	      ins-tag(sv-ins, last-ins) := thru-tag;
	      ins-op(sv-ins, last-ins) := ins-bra;
            else
	      let last-bb :: <basic-block> = last-tag.tag-bb;
	      if (last-bb.bb-seen)
		// both tag AND fall thru seen therefore
		// can't flip branch
		let fixup-block :: <basic-block> =
		  preserving-instructions(sv-ins)
		    make-branch-block(backend, thru-tag);
	          end;
		blk-vector[index] := fixup-block;
		inc!(index);
              else
                let reverse-op :: <op> = reverse(instructions);
	        next-cand := last-bb;
                blk.bb-fall-thru := last-bb;
		ins-tag(sv-ins, last-ins) := thru-tag;
		ins-op(sv-ins, last-ins) := reverse-op;
              end if;
            end if;
          else
	    // block ends with non branch instruction
	    let fixup-block :: <basic-block> =
	      preserving-instructions(sv-ins)
	        make-branch-block(backend, thru-tag);
	      end;
            blk-vector[index] := fixup-block;
	    inc!(index);
          end if;
        else
          if (last-tag == fall-thru)
	    eliminate-instruction(backend, last-ins);
          end if;
	  // must do fall thru next as it's not been seen
	  next-cand := fall-thru;
        end if;
      end if;
	    
      queue-tail := add-queue(blk.bb-next-set,
			      add-queue(blk.bb-other-set, queue-tail));

    end unless;
  end for;
  values(blk-vector, index);
end;



/// some block has ben redirected to the block the-next-block.
/// check if the previous block ends with a conditional branch to this
/// block, and if it is get rid of the instruction. If this empties
/// the block than check the preceding one as well.

define method eliminate-local-branches
    (sv-ins :: <instructions-vector>,
     block-vector :: <simple-basic-block-vector>, inx :: <integer>,
     the-next-block :: <basic-block>)
  if (inx > 0)
    let index :: <integer> = inx - 1;
    let prev-block :: <basic-block> = block-vector[index];
    let start :: <integer> = prev-block.bb-start;
    let finish :: <integer> = prev-block.bb-end;
    while (op-is-rem(ins-op(sv-ins, start)))
      inc!(start, instruction-size);
    end while;
    if ((start >= finish & untagged-prev(prev-block)) |
        begin
          let last-ins = finish - instruction-size;
	  let last-tag = ins-tag(sv-ins, last-ins);
          instance?(last-tag, <tag>) & 
            last-tag.tag-bb == the-next-block &
            begin prev-block.bb-end := last-ins; start == last-ins end;
        end)

      let tags :: <list> = prev-block.bb-taags;
      for (tag :: <tag> in tags)
        tag.tag-bb := the-next-block;
      end for;
      the-next-block.bb-taags := concatenate(tags, the-next-block.bb-taags);
      eliminate-local-branches(sv-ins, block-vector, index, the-next-block);
    else
      index;
    end if;
  end if;
end;
    

define method untagged-prev (prev-block :: <basic-block>) => (b :: <boolean>)
  let prev-tags :: <list> = prev-block.bb-taags;
  prev-tags == #() | prev-tags.head.tag-bb == prev-block;
end;    

