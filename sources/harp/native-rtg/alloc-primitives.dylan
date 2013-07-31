module:    harp-native-rtg
Synopsis:  Allocation Primitives for the Dylan runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Inlined allocation support

define c-fun runtime-external alloc                 = "primitive_alloc";
define c-fun runtime-external alloc-s1              = "primitive_alloc_s1";
define c-fun runtime-external alloc-s2              = "primitive_alloc_s2";
define c-fun runtime-external alloc-s               = "primitive_alloc_s";
define c-fun runtime-external alloc-r               = "primitive_alloc_r";
define c-fun runtime-external alloc-rf              = "primitive_alloc_rf";
define c-fun runtime-external alloc-rt              = "primitive_alloc_rt";
define c-fun runtime-external alloc-s-r             = "primitive_alloc_s_r";
define c-fun runtime-external alloc-s-rb            = "primitive_alloc_s_rb";

define c-fun runtime-external alloc-leaf            = "primitive_alloc_leaf";
define c-fun runtime-external alloc-leaf-s-r        = "primitive_alloc_leaf_s_r";
define c-fun runtime-external alloc-leaf-s1         = "primitive_alloc_leaf_s1";
define c-fun runtime-external alloc-leaf-s2         = "primitive_alloc_leaf_s2";
define c-fun runtime-external alloc-leaf-s          = "primitive_alloc_leaf_s";
define c-fun runtime-external alloc-leaf-r          = "primitive_alloc_leaf_r";
define c-fun runtime-external alloc-leaf-s-rbf      = "primitive_alloc_leaf_s_rbf";
define c-fun runtime-external alloc-leaf-s-rbfz     = "primitive_alloc_leaf_s_rbfz";
define c-fun runtime-external alloc-leaf-rbfz       = "primitive_alloc_leaf_rbfz";
define c-fun runtime-external alloc-leaf-s-rb       = "primitive_alloc_leaf_s_rb";

define c-fun runtime-external alloc-exact-awl-s-r   = "primitive_alloc_exact_awl_s_r";
define c-fun runtime-external alloc-exact-awl-rf    = "primitive_alloc_exact_awl_rf";
define c-fun runtime-external alloc-weak-awl-s-r    = "primitive_alloc_weak_awl_s_r";
define c-fun runtime-external alloc-weak-awl-rf     = "primitive_alloc_weak_awl_rf";

define c-fun runtime-external alloc-wrapper-s-r     = "primitive_alloc_wrapper_s_r";

define c-fun runtime-external prim-copy             = "primitive_copy";
define c-fun runtime-external prim-copy-r           = "primitive_copy_r";



define method op--allocate-byte-repeated-unfilled
    (be :: <harp-back-end>, obj :: <register>,
     byte-size, wrapper, rep-size, rep-size-slot)
  with-harp (be)
    c-result c-result;
    op--call-c(be, alloc-leaf-r, byte-size, wrapper, rep-size, rep-size-slot);
    ins--move(be, obj, c-result);
  end with-harp;
end method;


define method op--allocate-untraced
    (be :: <harp-back-end>, obj :: <register>, byte-size, wrapper)
  with-harp (be)
    c-result c-result;
    op--call-c(be, alloc-leaf, byte-size, wrapper);
    ins--move(be, obj, c-result);
  end with-harp;
end method;


define method op--allocate-traced
    (be :: <harp-back-end>, obj :: <register>, byte-size, wrapper,
     #rest slots)
  with-harp (be)
    c-result c-result;
    select (slots.size)
      0 => op--call-c(be, alloc, byte-size, wrapper);
      1 => apply(op--call-c, be, alloc-s1, byte-size, wrapper, slots);
      2 => apply(op--call-c, be, alloc-s2, byte-size, wrapper, slots);
      otherwise => 
        // Suboptimal - but rare. (case never used currently)
        // Initialize all slots with the first slot value (known safe)
	// Fill in the proper values at the end.
	op--call-c(be, alloc-s, byte-size, wrapper, slots.size, slots[0]);
	for (index from 4 by 4,
	     slot in slots)
	  unless (index = 4) ins--st(be, slot, c-result, index) end;
	end for;
    end select;
    ins--move(be, obj, c-result);
  end with-harp;
end method;


define method op--allocate-from-template
     (be :: <harp-back-end>, new :: <register>, 
      template :: <register>, byte-size)
  with-harp (be)
    c-result c-result;

    op--call-c(be, prim-copy, byte-size, template);
    ins--move(be, new, c-result);
  end with-harp;
end method;



define method op--perform-allocation-filled-internal
    (be :: <harp-back-end>, word-size, wrapper, 
     no-to-fill, fill, rep-size, rep-size-slot,
     allocator)
  with-harp (be)
    result result;
    c-result c-result;
    nreg byte-size;

    ins--asl(be, byte-size, word-size, 2);
    op--call-c(be, allocator, byte-size, wrapper, 
	       no-to-fill, fill, rep-size, rep-size-slot);
    ins--move(be, result, c-result);
  end with-harp;
end method;


define method op--perform-allocation-filled
    (be :: <harp-back-end>, word-size, wrapper, 
     no-to-fill, fill, rep-size, rep-size-slot)
  op--perform-allocation-filled-internal
    (be, word-size, wrapper, 
     no-to-fill, fill, rep-size, rep-size-slot,
     alloc-s-r);
end method;


define method op--perform-allocation-filled-leaf
    (be :: <harp-back-end>, word-size, wrapper, 
     no-to-fill, fill, rep-size, rep-size-slot)
  op--perform-allocation-filled-internal
    (be, word-size, wrapper, 
     no-to-fill, fill, rep-size, rep-size-slot,
     alloc-leaf-s-r);
end method;


define method op--perform-allocation-filled-wrapper
    (be :: <harp-back-end>, word-size, wrapper, 
     no-to-fill, fill, rep-size, rep-size-slot)
  op--perform-allocation-filled-internal
    (be, word-size, wrapper, 
     no-to-fill, fill, rep-size, rep-size-slot,
     alloc-wrapper-s-r);
end method;


define method op--perform-allocation-filled-linked-internal
    (be :: <harp-back-end>, word-size, wrapper, 
     no-to-fill, fill, rep-size, rep-size-slot, assoc,
     allocator)
  with-harp (be)
    result result;
    c-result c-result;
    nreg byte-size;

    ins--asl(be, byte-size, word-size, 2);
    op--call-c(be, allocator, byte-size, wrapper, assoc,
	       no-to-fill, fill, rep-size, rep-size-slot);
    ins--move(be, result, c-result);
  end with-harp;
end method;


define method op--perform-allocation-filled-exact-awl
    (be :: <harp-back-end>, word-size, wrapper, 
     no-to-fill, fill, rep-size, rep-size-slot, assoc)
  op--perform-allocation-filled-linked-internal
    (be, word-size, wrapper, 
     no-to-fill, fill, rep-size, rep-size-slot, assoc,
     alloc-exact-awl-s-r);
end method;


define method op--perform-allocation-filled-weak-awl
    (be :: <harp-back-end>, word-size, wrapper, 
     no-to-fill, fill, rep-size, rep-size-slot, assoc)
  op--perform-allocation-filled-linked-internal
    (be, word-size, wrapper, 
     no-to-fill, fill, rep-size, rep-size-slot, assoc,
     alloc-weak-awl-s-r);
end method;


define method op--perform-repeated-byte-allocation
    (be :: <harp-back-end>, byte-size, wrapper, 
     no-to-fill, fill, rep-size, rep-size-slot,
     #key fill-bytes? = #t)

  with-harp (be)
    result result;
    c-result c-result;
    nreg byte-fill;

    if (fill-bytes?)
      op--untaggify(be, byte-fill, fill);
      op--call-c(be, alloc-leaf-s-rbf, byte-size, wrapper, 
		 no-to-fill, fill, rep-size, rep-size-slot, byte-fill);
    else
        op--call-c(be, alloc-leaf-s-rb, byte-size, wrapper, 
	         no-to-fill, fill, rep-size, rep-size-slot);
    end if;

    ins--move(be, result, c-result);
  end with-harp;
end method;


define method op--perform-repeated-byte-allocation-terminated
    (be :: <harp-back-end>, byte-size, wrapper, 
     no-to-fill, fill, rep-size, rep-size-slot,
     #key fill-words? = #t)

  with-harp (be)
    result result;
    c-result c-result;
    nreg byte-fill;

    op--untaggify(be, byte-fill, fill);
    if (fill-words?)
      op--call-c(be, alloc-leaf-s-rbfz, byte-size, wrapper, 
		 no-to-fill, fill, rep-size, rep-size-slot, byte-fill);
    else
      op--call-c(be, alloc-leaf-rbfz, byte-size, wrapper, 
		 rep-size, rep-size-slot, byte-fill);
    end if;
    ins--move(be, result, c-result);
  end with-harp;
end method;



/// Allocation primitives ...


define runtime-primitive allocate
  // This one should not be being used any more
  op--unimplemented-primitive(be);
end runtime-primitive;


define runtime-primitive allocate-filled
/*
  On entry:
    size  - the size in words of the allocation
    MM class wrapper
    number of slots to fill
    fill value
    repeated-size(raw, NOT tagged)
    repeated slot offset
  On exit:
    A pointer to freshly allocated memory
*/

  arg0 arg0;
  nreg wrapper, no-to-fill, fill, rep-size, rep-size-slot;

  op--load-arguments(be, arg0, wrapper, no-to-fill, fill, rep-size, rep-size-slot);
  op--perform-allocation-filled(be, arg0, wrapper, 
				no-to-fill, fill, rep-size, rep-size-slot);
  op--rts-dropping-n-args(be, 6);
end runtime-primitive;




define runtime-primitive allocate-filled-in-leaf-pool
/*
  On entry:
    size  - the size in words of the allocation
    MM class wrapper
    number of slots to fill
    fill value
    repeated-size(raw, NOT tagged)
    repeated slot offset
  On exit:
    A pointer to freshly allocated memory, allocated from the Leaf Object pool
*/

  arg0 arg0;
  nreg wrapper, no-to-fill, fill, rep-size, rep-size-slot;

  op--load-arguments(be, arg0, wrapper, no-to-fill, fill, rep-size, rep-size-slot);
  op--perform-allocation-filled-leaf(be, arg0, wrapper, 
				     no-to-fill, fill, rep-size, rep-size-slot);
  op--rts-dropping-n-args(be, 6);
end runtime-primitive;


define runtime-primitive allocate-weak-in-awl-pool
/*
  On entry:
    size  - the size in words of the allocation
    MM class wrapper
    number of slots to fill
    fill value
    repeated-size(raw, NOT tagged)
    repeated slot offset
    Associated link
  On exit:
    A pointer to freshly allocated memory, allocated weak from the AWL pool
*/

  arg0 arg0;
  nreg wrapper, no-to-fill, fill, rep-size, rep-size-slot, assoc;

  op--load-arguments(be, arg0, wrapper, no-to-fill, fill, rep-size, rep-size-slot, assoc);
  op--perform-allocation-filled-weak-awl(be, arg0, wrapper, 
					 no-to-fill, fill, 
					 rep-size, rep-size-slot, assoc);
  op--rts-dropping-n-args(be, 7);
end runtime-primitive;


define runtime-primitive allocate-in-awl-pool
/*
  On entry:
    size  - the size in words of the allocation
    MM class wrapper
    number of slots to fill
    fill value
    repeated-size(raw, NOT tagged)
    repeated slot offset
    Associated link
  On exit:
    A pointer to freshly allocated memory, allocated from the AWL pool
*/

  arg0 arg0;
  nreg wrapper, no-to-fill, fill, rep-size, rep-size-slot, assoc;

  op--load-arguments(be, arg0, wrapper, no-to-fill, fill, rep-size, rep-size-slot, assoc);
  op--perform-allocation-filled-exact-awl(be, arg0, wrapper, 
					  no-to-fill, fill, 
					  rep-size, rep-size-slot, assoc);
  op--rts-dropping-n-args(be, 7);
end runtime-primitive;


define runtime-primitive allocate-wrapper
/*
  On entry:
    size  - the size in words of the allocation
    MM class wrapper
    number of slots to fill
    fill value
    repeated-size(raw, NOT tagged)
    repeated slot offset
  On exit:
    A pointer to freshly allocated memory
*/
  arg0 arg0;
  nreg wrapper, no-to-fill, fill, rep-size, rep-size-slot;

  op--load-arguments(be, arg0, wrapper, no-to-fill, fill, rep-size, rep-size-slot);
  op--perform-allocation-filled-wrapper(be, arg0, wrapper, 
					no-to-fill, fill, rep-size, rep-size-slot);
  op--rts-dropping-n-args(be, 6);
end runtime-primitive;


define runtime-primitive untraced-allocate
  // On entry:
  //   size  - the size in bytes of the allocation
  // On exit:
  //   A pointer to freshly allocated memory in a raw area
  arg0 arg0;
  op--call-c(be, raw-malloc, arg0);
  ins--rts-and-drop(be, 0);
end runtime-primitive;


/****** Real version which doesn't null terminate

define runtime-primitive byte-allocate-filled
/*
     On entry:
        word-size  - the base size in words of the allocation
        byte-size  - the additional size in bytes of the allocation
        MM class wrapper
        number of slots to fill
        fill value
        repeated-size(raw, NOT tagged)
        repeated slot offset
     On exit:
        A pointer to an area of memory. 
*/
  arg0 arg0;
  arg0 word-size;
  arg0 total-size;
  result result;
  nreg byte-size, wrapper, no-to-fill, fill, rep-size, rep-size-slot;

  op--load-arguments(be, arg0, byte-size, wrapper, no-to-fill, 
                     fill, rep-size, rep-size-slot);
  ins--asl(be, total-size, word-size, 2);     // get the word size in bytes
  ins--add(be, total-size, total-size, byte-size); // add the byte part
  ins--add(be, total-size, total-size, 3);         // Word align
  ins--and(be, total-size, total-size, ash(-1, 2));
  
  // call MM primitive
  op--perform-repeated-byte-allocation(be, total-size, wrapper, 
				       no-to-fill, fill, rep-size, rep-size-slot);
  
  op--rts-dropping-n-args(be, 7);
end runtime-primitive;

******/


define runtime-primitive byte-allocate-filled
/*
     On entry:
        word-size  - the base size in words of the allocation
        byte-size  - the additional size in bytes of the allocation
        MM class wrapper
        number of slots to fill
        fill value
        repeated-size(raw, NOT tagged)
        repeated slot offset
     On exit:
        A pointer to an area of memory
*/
  arg0 arg0;
  arg0 word-size;
  arg0 total-size;
  result result;
  nreg byte-size, wrapper, no-to-fill, fill, rep-size, rep-size-slot;

  op--load-arguments(be, arg0, byte-size, wrapper, no-to-fill, 
                     fill, rep-size, rep-size-slot);
  ins--asl(be, total-size, word-size, 2);     // get the word size in bytes
  ins--add(be, total-size, total-size, byte-size); // add the byte part
  ins--add(be, total-size, total-size, 3);         // Word align
  ins--and(be, total-size, total-size, ash(-1, 2));
  
  // Allocate. Don't fill bytes because streams buffers don't need it
  op--perform-repeated-byte-allocation(be, total-size, wrapper, 
				       no-to-fill, fill, rep-size, rep-size-slot,
				       fill-bytes?: #f);
  
  op--rts-dropping-n-args(be, 7);
end runtime-primitive;



define runtime-primitive byte-allocate-filled-terminated
/*
     On entry:
        word-size  - the base size in words of the allocation
        byte-size  - the additional size in bytes of the allocation 
                     INCLUDING the null terminator
        MM class wrapper
        number of slots to fill
        fill value
        repeated-size(raw, NOT tagged)
        repeated slot offset
     On exit:
        A pointer to a null-terminated area of memory.
*/
  arg0 arg0;
  arg0 word-size;
  arg0 total-size;
  result result;
  nreg byte-size, wrapper, no-to-fill, fill, rep-size, rep-size-slot, null-offset;

  op--load-arguments(be, arg0, byte-size, wrapper, no-to-fill, 
                     fill, rep-size, rep-size-slot);
  ins--asl(be, total-size, word-size, 2);     // get the word size in bytes
  ins--add(be, total-size, total-size, byte-size); // add the byte part
  ins--sub(be, null-offset, total-size, 1);  // index for the null-terminator *****
  ins--add(be, total-size, total-size, 3);         // Word align
  ins--and(be, total-size, total-size, ash(-1, 2));
  
  // Allocate. Don't fill words because strings don't have fixed slots
  op--perform-repeated-byte-allocation-terminated
    (be, total-size, wrapper, 
     no-to-fill, fill, rep-size, rep-size-slot,
     fill-words?: #f);

  op--rts-dropping-n-args(be, 7);
end runtime-primitive;


define runtime-primitive allocate-vector
  // On entry:
  //   size  - the size in words of the vector
  // On exit:
  //   A pointer to freshly allocated memory
  arg0 arg0;
  result result;
  reg vec;
  nreg size;

  ins--move(be, size, arg0);
  op--allocate-vector(be, vec, size);
  ins--move(be, result, vec);
  ins--rts-and-drop(be, 0);
end runtime-primitive;


define method op--allocate-vector 
    (be :: <harp-back-end>, vec :: <register>, word-size, 
     #key fill = dylan-unbound)
  with-harp (be)
    nreg vec-size-in-bytes;
    c-result c-result;
    
    ins--asl(be, vec-size-in-bytes, word-size, 2);  // get the size in bytes
    ins--add(be, vec-size-in-bytes, vec-size-in-bytes, 8); // allow for header
    op--call-c(be, alloc-rf, vec-size-in-bytes, dylan-sov-class, word-size, 1, fill);
    ins--move(be, vec, c-result);
  end with-harp;
end method;


define runtime-primitive vector
  // On entry:
  //   size  - the size in words of the vector
  //   #rest data - the words to fill in
  // On exit:
  //   A pointer to freshly allocated memory

  arg0 arg0;
  result result;
  nreg size-in-bytes, size-in-words;
  nreg dummy, first-dst, first-datum, ret-addr;
  stack stack;

  let max-num-arg-regs = be.registers.arguments-passed-in-registers;

  ins--move(be, size-in-words, arg0);

  // The easiest strategy is to manipulate the reg-args in-place on the 
  // stack to make the vector
  op--vector-case-generator(be, size-in-words);

  ins--load-address-of-stack-arg-n(be, first-datum, 0);  // first arg to copy
  let vec = op--make-vector-from-data(be, first-datum, size-in-words);
  ins--asl(be, size-in-bytes, size-in-words, 2); // get the size in bytes
  ins--move(be, result, vec);

  ins--rts-and-drop(be, size-in-bytes);
end runtime-primitive;

// This has to be done in leaf-case, hence specifically use adjust-stack
// and store-stack-arg-n to position the part of the vector passed in
// registers on the stack, before the rest of vector and after return address
// if there is one

define method op--vector-case-generator
    (be :: <harp-back-end>, count) => ()
  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  let num-cases = max(max-num-arg-regs - 1, 0);
  let cases? = num-cases > 0;
  let tags :: <simple-object-vector> = make-tags(be, num-cases);
  let done :: <tag> = make-tag(be);

  for (tag :: <tag> in tags,
       i :: <integer> from num-cases - 1 by -1)
    ins--bgt(be, tag, count, i);
  end for;

  cases? & ins--bra(be, done);

  for (tag :: <tag> in tags,
       i :: <integer> from num-cases by -1)
    ins--tag(be, tag);
    ins--adjust-stack(be, 4 * i);
    for (j :: <integer> from 1 to i)
      ins--store-stack-arg-n(be, be.registers.reg-machine-arguments[j], j - 1);
    end;
    ins--bra(be, done);
  end for;

  cases? & ins--tag(be, done);

end method;


define method op--copy-vector-internal
     (be :: <harp-back-end>, stack-vec :: <register>) 
     => (heap-vec :: <register>)
  with-harp (be)
    arg-count argc;
    result result;
    nreg dummy, byte-size, nsize;
    greg vec, new;
    tag done, non-zero;

    ins--move(be, vec, stack-vec);   // should be able to preserve across C call
    op--vector-size-times-4(be, byte-size, vec);  // find the size of the vector
    ins--bne(be, non-zero, byte-size, 0);   // see if it's an empty vector
    ins--move(be, new, dylan-empty-vector); // canonicalize if so
    ins--bra(be, done);
    ins--tag(be, non-zero);
    ins--add(be, byte-size, byte-size, 8);     // allow for header / size in vector
    op--allocate-from-template(be, new, vec, byte-size);
    ins--tag(be, done);
    new;
  end with-harp;
end method;



define method op--make-vector-from-data
     (be :: <harp-back-end>, data-start :: <register>, size-in-words) 
     => (heap-vec :: <register>)
  with-harp (be)
    c-result c-result;
    greg new;
    nreg vec-size-in-bytes;
    tag done, non-zero;

    ins--bne(be, non-zero, size-in-words, 0);   // see if it's an empty vector
    ins--move(be, new, dylan-empty-vector);     // canonicalize if so
    ins--bra(be, done);
    ins--tag(be, non-zero);
    ins--asl(be, vec-size-in-bytes, size-in-words, 2);  // get the size in bytes
    ins--add(be, vec-size-in-bytes, vec-size-in-bytes, 8); // allow for header
    op--call-c(be, alloc-rt, vec-size-in-bytes, dylan-sov-class,
	       size-in-words, 1, data-start);
    ins--move(be, new, c-result);
    ins--tag(be, done);
    new;
  end with-harp;
end method;

define runtime-primitive copy-vector
  // On entry:  vector 
  // On exit:   new-vector

  arg0 arg0;
  result result;

  let new = op--copy-vector-internal(be, arg0);
  ins--move(be, result, new);
  ins--rts-and-drop(be, 0);
end runtime-primitive;


define runtime-primitive make-box
  // On entry:
  //    object  -- the object to put in the box;
  // On exit:
  //    A pointer to a freshly alloced box

  arg0 arg0;
  result result;
  reg object, box;

  ins--move(be, object, arg0);
  op--allocate-traced(be, box, 8, dylan-value-cell-class, object);
  ins--move(be, result, box);
  ins--rts-and-drop(be, 0);
end runtime-primitive;

define runtime-primitive make-raw-box
  // On entry:
  //    object  -- the object to put in the box;
  // On exit:
  //    A pointer to a freshly alloced box

  arg0 arg0;
  result result;
  nreg object;
  reg box;

  ins--move(be, object, arg0);
  op--allocate-untraced(be, box, 8, raw-value-cell-class);
  ins--st(be, object, box, 4);
  ins--move(be, result, box);
  ins--rts-and-drop(be, 0);
end runtime-primitive;

define runtime-primitive make-single-float-box
  // On entry:
  //    object  -- the object to put in the box;
  // On exit:
  //    A pointer to a freshly alloced box

  float-arg0 arg0;
  result result;
  sfreg object;
  reg box;

  ins--fmove(be, object, arg0);
  op--allocate-untraced(be, box, 8, raw-value-cell-class);
  ins--fst(be, object, box, 4);
  ins--move(be, result, box);
  ins--rts-and-drop(be, 0);
end runtime-primitive;

define runtime-primitive make-double-float-box
  // On entry:
  //    object  -- the object to put in the box;
  // On exit:
  //    A pointer to a freshly alloced box

  float-arg0 arg0;
  result result;
  dfreg object;
  reg box;

  ins--dmove(be, object, arg0);
  op--allocate-untraced(be, box, 12, raw-double-value-cell-class);
  ins--dst(be, object, box, 4);
  ins--move(be, result, box);
  ins--rts-and-drop(be, 0);
end runtime-primitive;


define runtime-primitive make-closure
  // On entry:
  //    (template, env-size)
  // On exit:
  //    the new closure

  op--prim-make-closure(be, #f);
end runtime-primitive;


define runtime-primitive make-keyword-closure
  // On entry:
  //    (template, env-size)
  // On exit:
  //    the new closure

  op--prim-make-closure(be, #t);
end runtime-primitive;


define method op--prim-make-closure
    (be :: <harp-back-end>, keyword? :: <boolean>)
  with-harp (be)
    result result;
    nreg env-size;
    reg  template, fn;
    
    op--load-arguments(be, template, env-size);
    op--make-closure(be, fn, template, env-size, keyword?);
    ins--move(be, result, fn);
    op--rts-dropping-n-args(be, 2);
  end with-harp;
end method;



define runtime-primitive make-closure-with-signature
  // On entry:
  //    (template, signature, env-size)
  // On exit:
  //    the new closure

  op--prim-make-closure-with-signature(be, #f);
end runtime-primitive;


define runtime-primitive make-keyword-closure-with-signature
  // On entry:
  //    (template, signature, env-size)
  // On exit:
  //    the new closure

  op--prim-make-closure-with-signature(be, #t);
end runtime-primitive;


define method op--prim-make-closure-with-signature
    (be :: <harp-back-end>, keyword? :: <boolean>)
  with-harp (be)
    result result;
    nreg env-size;
    reg  template, fn, signature;
    
    op--load-arguments(be, template, signature, env-size);
    op--make-closure(be, fn, template, env-size, keyword?);
    ins--move(be, result, fn);
    ins--st(be, signature, result, be.function-signature-offset);
    op--rts-dropping-n-args(be, 3);
  end with-harp;
end method;



define runtime-primitive initialize-closure
  // On entry:
  //    (closure, env-size, environment-data...)
  // On exit:
  
  op--prim-initialize-closure(be, #f);
end runtime-primitive;


define runtime-primitive initialize-keyword-closure
  // On entry:
  //    (closure, env-size, environment-data...)
  // On exit:
  
  op--prim-initialize-closure(be, #t);
end runtime-primitive;


define method op--prim-initialize-closure
    (be :: <harp-back-end>, keyword? :: <boolean>)
  with-harp (be)
    nreg env-size, bytes-to-drop;
    reg  closure;
  
    op--load-arguments(be, closure, env-size);
    op--initialize-closure-environment(be, bytes-to-drop,
				       closure, 
                                       env-size, 2,
                                       keyword?);
    ins--rts-and-drop(be, bytes-to-drop);
  end with-harp;
end method;




define runtime-primitive make-closure-with-environment
  // On entry:
  //    (template, env-size, environment-data...)
  // On exit:
  //    the new closure
  
  op--prim-make-closure-with-environment(be, #f);
end runtime-primitive;


define runtime-primitive make-keyword-closure-with-environment
  // On entry:
  //    (template, env-size, environment-data...)
  // On exit:
  //    the new closure
  
  op--prim-make-closure-with-environment(be, #t);
end runtime-primitive;


define method op--prim-make-closure-with-environment
    (be :: <harp-back-end>, keyword? :: <boolean>)
  with-harp (be)
    result result;
    nreg env-size, bytes-to-drop;
    reg  template, fn;
  
    op--load-arguments(be, template, env-size);
    op--make-closure-with-environment(be, fn, bytes-to-drop, template, 
                                      env-size, 2, keyword?);
    ins--move(be, result, fn);
    ins--rts-and-drop(be, bytes-to-drop);
  end with-harp;
end method;



define runtime-primitive make-closure-with-environment-signature
  // On entry:
  //    (template, signature, env-size, environment-data...)
  // On exit:
  //    the new closure

  op--prim-make-closure-with-environment-signature(be, #f);  
end runtime-primitive;


define runtime-primitive make-keyword-closure-with-environment-signature
  // On entry:
  //    (template, signature, env-size, environment-data...)
  // On exit:
  //    the new closure

  op--prim-make-closure-with-environment-signature(be, #t);  
end runtime-primitive;


define method op--prim-make-closure-with-environment-signature
    (be :: <harp-back-end>, keyword? :: <boolean>)
  with-harp (be)
    result result;
    nreg env-size, bytes-to-drop;
    reg  template, signature, fn;
  
    op--load-arguments(be, template, signature, env-size);
    op--make-closure-with-environment(be, fn, bytes-to-drop, template, 
                                      env-size, 3, keyword?);
    ins--move(be, result, fn);
    ins--st(be, signature, result, be.function-signature-offset);
    ins--rts-and-drop(be, bytes-to-drop);
  end with-harp;
end method;



define runtime-primitive make-method-with-signature
  // On entry:
  //    (template, signature)
  // On exit:
  //    the new method
  
  op--prim-make-method-with-signature(be, #f);
end runtime-primitive;


define runtime-primitive make-keyword-method-with-signature
  // On entry:
  //    (template, signature)
  // On exit:
  //    the new method
  
  op--prim-make-method-with-signature(be, #t);
end runtime-primitive;


define method op--prim-make-method-with-signature
    (be :: <harp-back-end>, keyword? :: <boolean>)
  with-harp (be)
    result result;
    reg  template, signature, fn;
    nreg dummy;
  
    // Allocate a new function object
    op--load-arguments(be, template, signature);
    op--allocate-method(be, fn, template, keyword?, #f);
    ins--move(be, result, fn);
    ins--st(be, signature, result, be.function-signature-offset);
    op--rts-dropping-n-args(be, 2);
  end with-harp;
end method;


define method op--make-closure
    (be :: <harp-back-end>, 
     fn :: <register>, 
     template :: <register>, 
     env-size :: <register>,
     keyword-closure? :: <boolean>)
  with-harp (be)
    op--allocate-method(be, fn, template, keyword-closure?, #t,
			env-size: env-size);
  end with-harp;
end method;


define method op--make-closure-with-environment
    (be :: <harp-back-end>, 
     fn :: <register>, bytes-to-drop :: <register>,
     template :: <register>, env-size :: <register>,
     num-args :: <integer>,
     keyword-closure? :: <boolean>)
  with-harp (be)
    nreg env-stack-args-start, first-dst, words;
    nreg copy-bytes-to-drop, env-size-bytes;
    tag done-env;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;

    let args-in-regs :: <integer> = arguments-in-registers(be, num-args);
    let args-on-stack :: <integer> = arguments-on-stack(be, num-args);

    with-preserved-argument-registers(args-in-regs)
      op--allocate-method(be, fn, template, keyword-closure?, #t,
			  env-size: env-size);
    end;

    // Sort out the environment, if applicable
    let env-offset = environment-offset(be, keyword-closure?);
    ins--move(be, bytes-to-drop, args-on-stack * 4);
    ins--beq(be, done-env, env-size, 0);
    ins--move(be, copy-bytes-to-drop, bytes-to-drop);
    ins--add(be, first-dst, fn, env-offset + 4); // first data cell in environment
    ins--move(be, words, env-size); // help the colourer find a reg

    op--copy-registers-with-update
      (be, first-dst, words, first-dst, words, args-in-regs);

    unless (args-on-stack > 0)
      // Don't do the copy from stack if the passed env fitted in registers
      ins--ble(be, done-env, env-size, max-num-arg-regs - args-in-regs);
    end;

    // Copy from stack and calculate copied data for popping
    ins--load-address-of-stack-arg-n(be, env-stack-args-start, args-on-stack);
    op--copy-words-with-update(be, #f, first-dst, env-stack-args-start, words);

    // Adjust env-size by env-data passed in registers
    ins--move(be, env-size-bytes, env-size);
    let adjust :: <integer> = max-num-arg-regs - args-in-regs;
    unless (adjust == 0)
      ins--sub(be, env-size-bytes, env-size-bytes, adjust);
    end;

    ins--asl(be, env-size-bytes, env-size-bytes, 2); // env-size in bytes
    ins--move(be, bytes-to-drop, copy-bytes-to-drop);
    ins--add(be, bytes-to-drop, bytes-to-drop, env-size-bytes);
    ins--tag(be, done-env);

  end with-harp;
end method;



define method op--initialize-closure-environment
    (be :: <harp-back-end>, 
     bytes-to-drop :: <register>,
     fn :: <register>,
     env-size :: <register>,
     num-args :: <integer>,
     keyword-closure? :: <boolean>)
  with-harp (be)
    nreg env-stack-args-start, first-dst, words;
    nreg copy-bytes-to-drop, env-size-bytes;
    greg env;
    tag done-env;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;

    let args-on-stack :: <integer> = arguments-on-stack(be, num-args);
    let args-in-regs :: <integer> = arguments-in-registers(be, num-args);

    // Sort out the environment, if applicable
    let env-offset = environment-offset(be, keyword-closure?);
    ins--move(be, bytes-to-drop, args-on-stack * 4);
    ins--beq(be, done-env, env-size, 0);
    ins--move(be, copy-bytes-to-drop, bytes-to-drop);
    ins--add(be, first-dst, fn, env-offset + 4); // first data cell in environment
    ins--move(be, words, env-size); // help the colourer find a reg

    op--copy-registers-with-update
      (be, first-dst, words, first-dst, words, args-in-regs);

    unless (args-on-stack > 0)
      // Don't do the copy from stack if the passed env fitted in registers
      ins--ble(be, done-env, env-size, max-num-arg-regs - args-in-regs);
    end;

    // Copy from stack and calculate copied data for popping
    ins--load-address-of-stack-arg-n(be, env-stack-args-start, args-on-stack);
    op--copy-words-with-update(be, #f, first-dst, env-stack-args-start, words);

    // Adjust env-size by env-data passed in registers
    ins--move(be, env-size-bytes, env-size);
    let adjust :: <integer> = max-num-arg-regs - args-in-regs;
    unless (adjust == 0)
      ins--sub(be, env-size-bytes, env-size-bytes, adjust);
    end;

    ins--asl(be, env-size-bytes, env-size-bytes, 2); // env-size in bytes
    ins--move(be, bytes-to-drop, copy-bytes-to-drop);
    ins--add(be, bytes-to-drop, bytes-to-drop, env-size-bytes);
    ins--tag(be, done-env);

  end with-harp;
end method;


define method op--allocate-method 
    (be :: <harp-back-end>, dest :: <register>, template :: <register>, 
     keyword? :: <boolean>, closure? :: <boolean>,
     #key env-size)
  with-harp (be)
    nreg closure-size, env-size-bytes;
    c-result c-result;

    let fn-size = function-size(be, keyword?, closure?);
    if (closure?)
      let env-offset-in-words = truncate/(environment-offset(be, keyword?), 4);
      ins--asl(be, env-size-bytes, env-size, 2); // env-size in bytes
      ins--add(be, closure-size, env-size-bytes, fn-size);
      op--call-c(be, prim-copy-r, closure-size, env-size, env-offset-in-words, template);
    else
      op--call-c(be, prim-copy, fn-size, template);
    end if;
    ins--move(be, dest, c-result);
  end with-harp;
end method;


define method environment-offset 
    (be :: <harp-back-end>, keyword-closure? :: <boolean>) => (o :: <integer>)
  if (keyword-closure?) 
    be.keyword-closure-environment-offset 
  else be.closure-environment-offset 
  end;
end method;



define method function-size 
    (be :: <harp-back-end>, keyword? :: <boolean>, closure? :: <boolean>)
    => (s :: <integer>)
  if (closure?)
    if (keyword?)
      be.keyword-closure-size;
    else be.simple-closure-size;
    end if;
  else
    if (keyword?)
      be.keyword-method-size;
    else be.method-size;
    end if;
  end if;
end method;
