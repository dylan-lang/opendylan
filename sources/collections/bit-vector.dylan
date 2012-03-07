Module:       collections-internals
Author:       Keith Dennison
Synopsis:     Define <bit-vector> and its operations
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



///////////////////////////////////////////////////////////////////////////
// INTERNAL CONSTANTS                                                    //
///////////////////////////////////////////////////////////////////////////

define constant $word-size :: <integer> = $machine-word-size;
define constant $log-word-size :: <integer> = integer-length($machine-word-size) - 1;

define constant $machine-word-zero       = as(<machine-word>, 0);
define constant $machine-word-one        = as(<machine-word>, 1);
define constant $machine-word-minus-one  = as(<machine-word>, -1);



///////////////////////////////////////////////////////////////////////////
// INTERNAL FUNCTIONS                                                    //
///////////////////////////////////////////////////////////////////////////

define inline-only function compute-word-offset
    (index :: <integer>) => (offset :: <integer>)
  ash(index, -$log-word-size);
end function;

define inline-only function compute-bit-offset
    (index :: <integer>) => (offset :: <integer>)
  logand(index, ($word-size - 1));
end function;

/*
define inline-only function compute-word-and-bit-offsets
    (index :: <integer>) => (word-offset :: <integer>, bit-offset :: <integer>)
  values(compute-word-offset(index), compute-bit-offset(index));
end function;
*/

/*
define inline-only function mask-for-bits-above
    (bit :: <integer>) => (mask :: <machine-word>)
  %shift-left($machine-word-minus-one, bit);
end function;

define inline-only function mask-for-bits-strictly-below
    (bit :: <integer>) => (mask :: <machine-word>)
  %lognot(%shift-left($machine-word-minus-one, bit));
end function;

define inline-only function mask-and-pad-word
    (word :: <machine-word>, bit :: <integer>, pad :: <bit>)
 => (result :: <machine-word>)
  if (bit = 0)
    word;
  else
    if (pad = 0)
      %logand(word, mask-for-bits-strictly-below(bit));
    else
      %logior(word, mask-for-bits-above(bit));
    end if;
  end if;
end function;
*/

define inline-only function raw-mask-for-bits-above
    (bit :: <integer>) // => (mask :: <raw-machine-word>)
  primitive-machine-word-shift-left-low
    (integer-as-raw(-1), integer-as-raw(bit));
end function;

define inline-only function raw-mask-for-bits-strictly-below
    (bit :: <integer>) // => (mask :: <raw-machine-word>)
  primitive-machine-word-lognot
    (primitive-machine-word-shift-left-low
      (integer-as-raw(-1), integer-as-raw(bit)));
end function;

define inline-only function raw-mask-and-pad-word
    (word :: <raw-machine-word>, bit :: <integer>, pad :: <bit>)
// => (result :: <raw-machine-word>)
  if (bit = 0)
    word;
  elseif (pad = 0)
    primitive-machine-word-logand(word, raw-mask-for-bits-strictly-below(bit));
  else
    primitive-machine-word-logior(word, raw-mask-for-bits-above(bit));
  end if;
end function;

//////////
// Copy a word from the source vector to the destination vector. The bits
// strictly below bit-limit are taken from the source word, the rest are
// determined by the fill value. If bit-limit is zero, the whole word is
// copied.
//
define inline-only function copy-bit-vector-word
    (source :: <bit-vector>, destination :: <bit-vector>,
     word :: <integer>, bit-limit :: <integer>, fill :: <bit>)
 => ()
  let bit-limit = compute-bit-offset(bit-limit);
  if (bit-limit = 0)
    bit-vector-word(destination, word) := bit-vector-word(source, word);
  else
    let src :: <raw-machine-word> = bit-vector-word(source, word);
    let dst :: <raw-machine-word>
      = if (fill = 0)
          primitive-machine-word-logand
            (src, raw-mask-for-bits-strictly-below(bit-limit));
        else
          primitive-machine-word-logior
            (src, raw-mask-for-bits-above(bit-limit));
        end if;
    bit-vector-word(destination, word) := dst;
  end if;
end function;
/*
define inline function set-bit-vector-word
    (vector :: <bit-vector>, i :: <integer>, word :: <machine-word>,
     bit-limit :: <integer>, fill :: <bit>)
 => ()
  bit-limit := compute-bit-offset(bit-limit);
  if (bit-limit ~= 0)
    word := if (fill = 0)
              %logand(word, mask-for-bits-strictly-below(bit-limit));
	    else
              %logior(word, mask-for-bits-above(bit-limit));
            end if;
  end if;
  bit-vector-word(vector, i) := primitive-unwrap-machine-word(word);
end function;
*/

//
// CHECK-START-COMPUTE-END
//
/*
define method check-start-compute-end
  (vector :: <bit-vector>, start :: <integer>, last) 
      => (real-last :: <integer>);
  let vector-size = vector.size;
  let last :: <integer> = if (unsupplied?(last)) vector-size else last end;

  if (start < 0) invalid-sequence-start-error(vector, start) end;

  case
    last > vector-size =>  invalid-sequence-end-error(vector, last);
    last < start =>     invalid-sequence-bounds-error(vector, start, last);
    otherwise => last;
  end
end method check-start-compute-end;
*/


define inline-only function bit-vector-and-internal!
    (result :: <bit-vector>, v1 :: <bit-vector>, v2 :: <bit-vector>,
     p1 :: <bit>, p2 :: <bit>)
 => ()

  // Can just directly combine the words below the smaller vectors last word

  let word-limit :: <integer> = v1.word-size - 1;
  for (i :: <integer> from 0 below word-limit)
    bit-vector-word(result, i) :=
      primitive-machine-word-logand
        (bit-vector-word(v1, i), bit-vector-word(v2, i));
  end for;

  let v1_size :: <integer> = v1.size;

  if (v1_size = result.size)

    // In the case where the result has the same size as the smaller argument
    // we can just combine the two words directly to get the last word too.

    bit-vector-word(result, word-limit) :=
      primitive-machine-word-logand
        (bit-vector-word(v1, word-limit), bit-vector-word(v2, word-limit));

  else

    // Otherwise, the result must be bigger than the smaller vector so we
    // need to take into account the pad value for the smaller vector.

    // Combine the last word of the smaller vector with its equivalent
    // in the larger argument, taking account of the smaller vector's pad.

    if (word-limit >= 0)
      bit-vector-word(result, word-limit) :=
        primitive-machine-word-logand(bit-vector-word(v2, word-limit),
          raw-mask-and-pad-word(bit-vector-word(v1, word-limit),
                                compute-bit-offset(v1_size), p1));
    end if;

    // If the result has more words than the smaller vector, create them
    // from the smaller vector's pad value and the larger vector's data.

    if (result.word-size > v1.word-size)
      for (i :: <integer> from v1.word-size below result.word-size)
        bit-vector-word(result, i) := 
          if (p1 = 0)
            primitive-unwrap-machine-word($machine-word-zero);
          else
            bit-vector-word(v2, i);
	  end if;
      end for;
    end if;
  end if;
end function;


define inline-only function bit-vector-andc2-internal!
    (result :: <bit-vector>, v1 :: <bit-vector>, v2 :: <bit-vector>,
     p1 :: <bit>, p2 :: <bit>)
 => ()

  // Can just merge the words before the smaller vectors last word

  let word-limit :: <integer> = v1.word-size - 1;
  for (i :: <integer> from 0 below word-limit)
    bit-vector-word(result, i) :=
      primitive-machine-word-logand
        (bit-vector-word(v1, i),
         primitive-machine-word-lognot(bit-vector-word(v2, i)));
  end for;

  let v1_size :: <integer> = v1.size;

  if (v1_size = result.size)

    // In the case where the result has the same size as the smaller argument
    // we can just combine the two words directly to get the last word too.

    bit-vector-word(result, word-limit) :=
      primitive-machine-word-logand
        (bit-vector-word(v1, word-limit),
         primitive-machine-word-lognot(bit-vector-word(v2, word-limit)));

  else

    // Otherwise, the result must be bigger than the smaller vector so we
    // need to take into account the pad value for the smaller vector.

    // Combine the last word of the smaller result with its equivalent
    // in the larger argument, taking account of the smaller vector's pad

    if (word-limit >= 0)
      bit-vector-word(result, word-limit) :=
        primitive-machine-word-logand
          (raw-mask-and-pad-word
            (bit-vector-word(v1, word-limit), compute-bit-offset(v1_size), p1),
           primitive-machine-word-lognot(bit-vector-word(v2, word-limit)));
    end if;

    // If the result has more words than the smaller vector, create them
    // from the vector's pad value and the larger vector.

    if (v1.word-size < result.word-size)
      if (p1 = 0)
        for (i :: <integer> from v1.word-size below result.word-size)
          bit-vector-word(result, i) := 
            primitive-unwrap-machine-word($machine-word-zero);
        end for;
      else
        for (i :: <integer> from v1.word-size below result.word-size)
          bit-vector-word(result, i) := 
            primitive-machine-word-lognot(bit-vector-word(v2, i));
        end for;
      end if;
    end if;
  end if;
end function;

define inline-only function bit-vector-andc2-internal-swapped!
    (result :: <bit-vector>, v1 :: <bit-vector>, v2 :: <bit-vector>,
     p1 :: <bit>, p2 :: <bit>)
 => ()

  // Can just merge the words before the smaller vectors last word

  let word-limit :: <integer> = v1.word-size - 1;
  for (i :: <integer> from 0 below word-limit)
    bit-vector-word(result, i) :=
      primitive-machine-word-logand
        (primitive-machine-word-lognot(bit-vector-word(v1, i)),
         bit-vector-word(v2, i));
  end for;

  let v1_size :: <integer> = v1.size;

  if (v1_size = result.size)

    // In the case where the result has the same size as the smaller argument
    // we can just combine the two words directly to get the last word too.

    bit-vector-word(result, word-limit) :=
      primitive-machine-word-logand
        (primitive-machine-word-lognot(bit-vector-word(v1, word-limit)),
         bit-vector-word(v2, word-limit));

  else

    // Otherwise, the result must be bigger than the smaller vector so we
    // need to take into account the pad value for the smaller vector.

    // Combine the last word of the smaller result with its equivalent
    // in the larger argument, taking account of the smaller vector's pad

    if (word-limit >= 0)
      bit-vector-word(result, word-limit) :=
        primitive-machine-word-logand
          (primitive-machine-word-lognot(raw-mask-and-pad-word
                                           (bit-vector-word(v1, word-limit),
                                            compute-bit-offset(v1_size), p1)),
           bit-vector-word(v2, word-limit));
    end if;

    // If the result has more words than the smaller vector, create them
    // from the vector's pad value and the larger vector.

    if (v1.word-size < result.word-size)
      if (p1 = 0)
        for (i :: <integer> from v1.word-size below result.word-size)
          bit-vector-word(result, i) := bit-vector-word(v2, i);
        end for;
      else
        for (i :: <integer> from v1.word-size below result.word-size)
          bit-vector-word(result, i) :=
            primitive-unwrap-machine-word($machine-word-zero);
	end for;
      end if;
    end if;
  end if;
end function;


define inline-only function bit-vector-or-internal!
    (result :: <bit-vector>, v1 :: <bit-vector>, v2 :: <bit-vector>,
     p1 :: <bit>, p2 :: <bit>)
 => ()

  // Can just merge the words before the smaller vectors last word

  let word-limit :: <integer> = v1.word-size - 1;
  for (i :: <integer> from 0 below word-limit)
    bit-vector-word(result, i) :=
      primitive-machine-word-logior
        (bit-vector-word(v1, i), bit-vector-word(v2, i));
  end for;

  let v1_size :: <integer> = v1.size;

  if (v1_size = result.size)

    // In the case where the result has the same size as the smaller argument
    // we can just combine the two words directly to get the last word too.

    bit-vector-word(result, word-limit) :=
      primitive-machine-word-logior
        (bit-vector-word(v1, word-limit), bit-vector-word(v2, word-limit));

  else

    // Otherwise, the result must be bigger than the smaller vector so we
    // need to take into account the pad value for the smaller vector.

    // Combine the last word of the smaller result with its equivalent
    // in the larger argument.

    if (word-limit >= 0)
      bit-vector-word(result, word-limit) :=
        primitive-machine-word-logior(bit-vector-word(v2, word-limit),
           raw-mask-and-pad-word(bit-vector-word(v1, word-limit),
                                  compute-bit-offset(v1_size), p1));
    end if;

    // If the result has more words than the smaller vector, create them
    // from the vector's pad value and the larger vector.

    if (v1.word-size < result.word-size)
      if (p1 = 0)
	for (i :: <integer> from v1.word-size below result.word-size)
	  bit-vector-word(result, i) := bit-vector-word(v2, i);
	end for;
      else
        for (i :: <integer> from v1.word-size below result.word-size)
          bit-vector-word(result, i)
            := primitive-unwrap-machine-word($machine-word-minus-one);
	end for;
      end if;
    end if;
  end if;
end function;


define inline-only function bit-vector-xor-internal!
    (result :: <bit-vector>, v1 :: <bit-vector>, v2 :: <bit-vector>,
     p1 :: <bit>, p2 :: <bit>)
 => ()

  // Can just merge the words before the smaller vectors last word

  let word-limit :: <integer> = v1.word-size - 1;
  for (i :: <integer> from 0 below word-limit)
    bit-vector-word(result, i) :=
      primitive-machine-word-logxor
        (bit-vector-word(v1, i), bit-vector-word(v2, i));
  end for;

  let v1_size :: <integer> = v1.size;

  if (v1_size = result.size)

    // In the case where the result has the same size as the smaller argument
    // we can just combine the two words directly to get the last word too.

    bit-vector-word(result, word-limit) :=
      primitive-machine-word-logxor
        (bit-vector-word(v1, word-limit), bit-vector-word(v2, word-limit));

  else

    // Otherwise, the result must be bigger than the smaller vector so we
    // need to take into account the pad value for the smaller vector.

    // Combine the last word of the smaller result with its equivalent
    // in the larger argument.

    if (word-limit >= 0)
      bit-vector-word(result, word-limit) :=
        primitive-machine-word-logxor(bit-vector-word(v2, word-limit),
          raw-mask-and-pad-word(bit-vector-word(v1, word-limit),
                                compute-bit-offset(v1_size), p1));
    end if;

    // If the result has more words than the smaller vector, create them
    // from the vector's pad value and the larger vector.

    if (v1.word-size < result.word-size)
      if (p1 = 0)
        for (i :: <integer> from v1.word-size below result.word-size)
	  bit-vector-word(result, i) := bit-vector-word(v2, i);
	end for;
      else
	for (i :: <integer> from v1.word-size below result.word-size)
          bit-vector-word(result, i) :=
            primitive-machine-word-lognot(bit-vector-word(v2, i));
	end for;
      end if;
    end if;
  end if;
end function;



///////////////////////////////////////////////////////////////////////////
// GENERIC FUNCTIONS                                                     //
///////////////////////////////////////////////////////////////////////////

define generic bit-vector-word
    (v :: <bit-vector>, i :: <integer>) => (r :: <raw-machine-word>);

define generic bit-vector-word-setter
    (n :: <raw-machine-word>, v :: <bit-vector>, i :: <integer>)
 => (r :: <raw-machine-word>);



///////////////////////////////////////////////////////////////////////////
// SEALED DOMAINS                                                        //
///////////////////////////////////////////////////////////////////////////

define sealed domain bit-vector-word (<bit-vector>, <integer>);
define sealed domain bit-vector-word-setter
  (<raw-machine-word>, <bit-vector>, <integer>);

define sealed domain size (<bit-vector>);
define sealed domain size-setter (<integer>, <bit-vector>);

define sealed domain make (singleton(<bit-vector-internal>));
define sealed domain initialize (<bit-vector-internal>);

///////////////////////////////////////////////////////////////////////////
// EXPORTED THINGS                                                       //
///////////////////////////////////////////////////////////////////////////


//
// <BIT-VECTOR>
//
define open abstract primary class <bit-vector> (<vector>)
  slot size :: <integer> = 0, init-keyword: #"size";
  repeated slot bit-vector-word :: <raw-machine-word>,
//    init-value: primitive-unwrap-abstract-integer(0),
//    init-keyword: word-fill,
    size-getter: word-size,
    size-init-keyword: word-size:,
    size-init-value: 0;
end class;

define sealed concrete primary class <bit-vector-internal> (<bit-vector>)
end class;

define sealed method type-for-copy (object :: <bit-vector>) => (class :: <class>)
  <bit-vector>
end method type-for-copy;


//
// MAKE
//

define constant $empty-bit-vector
  = make(<bit-vector-internal>, size: 0, word-size: 0);

define sealed method make
    (class == <bit-vector>, #key size: bit-size :: <integer> = 0,
                                 round-up-size? :: <boolean> = #f,
                                 fill :: <bit> = 0,
                                 copy-from :: <bit-vector> = $empty-bit-vector)
 => (vector :: <bit-vector>)
  if (bit-size = 0)
    $empty-bit-vector;
  else
    let wsize = ash(bit-size - 1, - $log-word-size) + 1;
    if (round-up-size?)
      bit-size := ash(wsize, $log-word-size);
    end if;
    let vector :: <bit-vector>
      = make(<bit-vector-internal>, size: bit-size, word-size: wsize);
    let word-fill :: <machine-word>
      = if (fill = 0) $machine-word-zero else $machine-word-minus-one end;
    for (i :: <integer> from 0 below wsize)
      bit-vector-word(vector, i) := primitive-unwrap-machine-word(word-fill);
    end for;
    let copy-from-size :: <integer> = copy-from.size;
    if (copy-from-size ~== 0)
      if (bit-size <= copy-from-size)
        for (i :: <integer> from 0 below wsize)
          bit-vector-word(vector, i) := bit-vector-word(copy-from, i);
        end for;
      else
        let last :: <integer> = copy-from.word-size - 1;
        for (i :: <integer> from 0 below last)
          bit-vector-word(vector, i) := bit-vector-word(copy-from, i);
        end for;
        copy-bit-vector-word(copy-from, vector, last, copy-from-size, fill);
      end if;
    end if;
    vector;
  end if;
end method;

/*
define constant $empty-bit-vector
  = begin
      let vector :: <bit-vector-internal>
        = system-allocate-repeated-instance(<bit-vector-internal>, 0, 0);
      vector.size := 0;
      vector;
    end;

define sealed method make
    (class == <bit-vector>, #key size :: <integer> = 0,
                                 round-up-size? :: <boolean> = #f,
                                 fill :: <bit> = 0,
                                 copy-from :: <bit-vector> = $empty-bit-vector)
 => (vector :: <bit-vector>)
  make(<bit-vector-internal>, size: size, round-up-size?: round-up-size?,
       fill: fill, copy-from: copy-from);
end method;

define sealed method make
    (class == <bit-vector-internal>,
       #key size: bit-size :: <integer> = 0,
            round-up-size? :: <boolean> = #f,
            fill :: <bit> = 0,
            copy-from :: <bit-vector> = $empty-bit-vector)
 => (vector :: <bit-vector-internal>)

  // TODO: Check for bit-size < 0 & raise error
  if (bit-size = 0)
    $empty-bit-vector;
  else
    let wsize :: <integer> = ash(bit-size - 1, - $log-word-size) + 1;
    let word-fill :: <machine-word>
      = if (fill = 0) $machine-word-zero else $machine-word-minus-one end;
    let vector :: <bit-vector-internal>
      = system-allocate-repeated-instance(<bit-vector-internal>, wsize, 0);
    for (i :: <integer> from 0 below wsize)
      bit-vector-word(vector, i) := primitive-unwrap-machine-word(word-fill);
    end for;
    if (round-up-size?)
      bit-size := ash(wsize, $log-word-size);
    end if;
    vector.size := bit-size;
    if (copy-from ~== $empty-bit-vector)
      if (bit-size <= copy-from.size)
        for (i :: <integer> from 0 below wsize)
          bit-vector-word(vector, i) := bit-vector-word(copy-from, i);
	end for;
      else
        let last = copy-from.word-size - 1;
        for (i :: <integer> from 0 below last)
          bit-vector-word(vector, i) := bit-vector-word(copy-from, i);
        end for;
        copy-bit-vector-word(copy-from, vector, last, copy-from.size, fill);
      end if;
    end if;
    vector;
  end if;
end method;
*/


//
// ELEMENT
//
define sealed method element (vector :: <bit-vector>, index :: <integer>,
                       #key default = unsupplied())
                   => (bit :: <bit>)
  let vector-size :: <integer> = size(vector);
  if (primitive-range-check(integer-as-raw(index),
			    integer-as-raw(0),
			    integer-as-raw(vector-size)))

    // TODO: Convert offsets to be raw-integers ???
    let word-offset :: <integer> = ash(index, -$log-word-size);
    let bit-offset :: <integer> = logand(index, ($word-size - 1));

    // TODO: replace with use of primitive-bit-element
    let bit
      = primitive-machine-word-logbit?
          (integer-as-raw(bit-offset),
	   bit-vector-word(vector, word-offset));
    if (bit) 1 else 0 end
/*
    raw-as-integer(primitive-bit-element(vector,
      integer-as-raw(word-offset),
      primitive-repeated-slot-offset(vector),
      integer-as-raw(bit-offset)));
*/
  else
    if (unsupplied?(default))
      element-range-error(vector, index)
    else
      default
    end if
  end if
end method;


//
// ELEMENT-NO-BOUNDS-CHECK
//
define inline sealed method element-no-bounds-check
    (vector :: <bit-vector>, index :: <integer>, #key default)
 => (bit :: <bit>)
  // TODO: Convert offsets to be raw-integers ???
  let word-offset :: <integer> = ash(index, -$log-word-size);
  let bit-offset :: <integer> = logand(index, ($word-size - 1));

  // TODO: replace with use of primitive-bit-element
  let bit
    = primitive-machine-word-logbit?
        (integer-as-raw(bit-offset),
	 bit-vector-word(vector, word-offset));
  if (bit) 1 else 0 end
/*
  raw-as-integer(primitive-bit-element(vector,
    integer-as-raw(word-offset),
    primitive-repeated-slot-offset(vector),
    integer-as-raw(bit-offset)));
*/
end method;


//
// ELEMENT-SETTER
//
define sealed method element-setter
    (bit :: <bit>, vector :: <bit-vector>, index :: <integer>)
 => (bit :: <bit>)
  let vector-size :: <integer> = size(vector);
  if (primitive-range-check(integer-as-raw(index),
			    integer-as-raw(0),
			    integer-as-raw(vector-size)))

    // TODO: convert offsets to be raw-integers ???
    let word-offset :: <integer> = ash(index, -$log-word-size);
    let bit-offset :: <integer> = logand(index, ($word-size - 1));

    // TODO: replace with use of primitive-bit-element-setter
    let word
      = primitive-wrap-machine-word(bit-vector-word(vector, word-offset));
    let new-word :: <machine-word>
      = if (bit = 0)
          %logand(word, %lognot(%shift-left($machine-word-one, bit-offset)));
	else
          %logior(word, %shift-left($machine-word-one, bit-offset));
	end if;
    bit-vector-word(vector, word-offset)
      := primitive-unwrap-machine-word(new-word);
/*
    primitive-bit-element-setter(integer-as-raw(bit), vector,
                                 integer-as-raw(word-offset),
                                 primitive-repeated-slot-offset(vector),
                                 integer-as-raw(bit-offset));
*/
    bit;
  else
    element-range-error(vector, index)
  end if
end method;


//
// ELEMENT-NO-BOUNDS-CHECK-SETTER
//
define inline sealed method element-no-bounds-check-setter
    (bit :: <bit>, vector :: <bit-vector>, index :: <integer>)
 => (bit :: <bit>)
  // TODO: convert offsets to be raw-integers ???
  let word-offset :: <integer> = ash(index, -$log-word-size);
  let bit-offset :: <integer> = logand(index, ($word-size - 1));

  // TODO: replace with use of primitive-bit-element-setter
  let word
    = primitive-wrap-machine-word(bit-vector-word(vector, word-offset));
  let new-word
    = if (bit = 0)
        %logand(word, %lognot(%shift-left($machine-word-one, bit-offset)));
      else
        %logior(word, %shift-left($machine-word-one, bit-offset));
      end if;
  bit-vector-word(vector, word-offset)
    := primitive-unwrap-machine-word(new-word);
/*
    primitive-bit-element-setter(integer-as-raw(bit), vector,
                                 integer-as-raw(word-offset),
                                 primitive-repeated-slot-offset(vector),
                                 integer-as-raw(bit-offset));
*/
  bit;
end method;


//
// FILL!
//
define sealed method fill!
    (vector :: <bit-vector>, value :: <bit>,
     #key start :: <integer> = 0, end: last = unsupplied())
 => (result :: <bit-vector>)

  if (vector.size > 0)
    // TODO: change this to a call to check-start-compute-end(start, last)
//    let last :: <integer> = check-start-compute-end(vector, start, last);
    let last :: <integer> = if (unsupplied?(last)) vector.size else last end;
    let start-word-offset :: <integer> = compute-word-offset(start);
    let last-word-offset :: <integer> = compute-word-offset(last);
    let start-bit-offset :: <integer> = compute-bit-offset(start);
    let last-bit-offset :: <integer> = compute-bit-offset(last);

    // Special case where start and last are in the same word

    if (last-word-offset = start-word-offset)
      let start-mask = integer-as-raw(ash(-1, start-bit-offset));
      let last-mask = integer-as-raw(ash(-1, last-bit-offset));
      if (value = 0)
        let mask =  primitive-machine-word-logior(last-mask,
          primitive-machine-word-lognot(start-mask));
        bit-vector-word(vector, start-word-offset)
          := primitive-machine-word-logand(mask,
               bit-vector-word(vector, start-word-offset));
      else
        let mask =  primitive-machine-word-logxor(last-mask, start-mask);
        bit-vector-word(vector, start-word-offset)
          := primitive-machine-word-logior(mask,
               bit-vector-word(vector, start-word-offset));
      end if;

    else

      // Fill the last word to be filled. If we're filling to the end of the
      // vector or the whole word has to be filled, don't do anything yet but
      // let the word be filled in later by primitive-fill!. Otherwise need
      // to do some bit manipulations.

      if (last = vector.size)
        let new-last-word-offset :: <integer> = vector.word-size;
        last-word-offset := new-last-word-offset;
      elseif (last-bit-offset ~= 0)
        let word :: <raw-machine-word> = bit-vector-word(vector, last-word-offset);
        if (value = 0)
          let mask = raw-mask-for-bits-above(last-bit-offset);
          word := primitive-machine-word-logand(word, mask);
	else
          let mask = raw-mask-for-bits-strictly-below(last-bit-offset);
          word := primitive-machine-word-logior(word, mask);
	end if;
        bit-vector-word(vector, last-word-offset) := word;
      end if;

      // Fill in the first word to be filled. If filling all the bits in the
      // word, don't do anything yet but let the word be filled in later by the
      // call to primitive-fill!. Otherwise need to do some bit manipulations.

      if (start-bit-offset ~= 0)
        let word :: <raw-machine-word> = bit-vector-word(vector, start-word-offset);
        if (value = 0)
          let mask = raw-mask-for-bits-strictly-below(start-bit-offset);
          word := primitive-machine-word-logand(word, mask);
	else
          let mask = raw-mask-for-bits-above(start-bit-offset);
          word := primitive-machine-word-logior(word, mask);
	end if;
        bit-vector-word(vector, start-word-offset) := word;
        let new-start-word-offset :: <integer> = start-word-offset + 1;
        start-word-offset := new-start-word-offset;
      end if;

      // At this point start-word and last-word indicate whole
      // words to be filled. start-word is inclusive, last-word
      // is exclusive.

      if ((last-word-offset - start-word-offset) > 0)
        let fill-word = if (value = 0)
                          $machine-word-zero
                        else
                          $machine-word-minus-one
                        end if;
	primitive-fill!(vector,
			primitive-repeated-slot-offset(vector),
                        integer-as-raw(start-word-offset),
                        integer-as-raw(last-word-offset - start-word-offset),
                        primitive-cast-raw-as-pointer
			  (primitive-unwrap-machine-word(fill-word)));
      end if;
    end if;
  end if;
  vector;
end method;


/* ******* UNFORTUNATELY, COPY-SEQUENCE ON <VECTOR> IS MISGUIDEDLY SEALED
 * ******* IN THE DYLAN LIBRARY, SO THIS WON'T WORK!

//
// COPY-SEQUENCE
//
define sealed method copy-sequence
    (vector :: <bit-vector>, #key start :: <integer> = 0,
                                  end: last = unsupplied())
 => (result :: <bit-vector>)

  // TODO: change this to a call to check-start-compute-end(start, last)
  let last :: <integer> = if (unsupplied?(last)) vector.size else last end;
  let result :: <bit-vector> = make(<bit-vector>, size: (last - start));

  let (start-word-offset, start-bit-offset)
    = compute-word-and-bit-offsets(start);

  if (start-bit-offset = 0)
    for (i :: <integer> from 0 below result.word-size)
      bit-vector-word(result, i)
        := bit-vector-word(vector, start-word-offset + i);
    end for;

  else
    let n = $word-size - start-bit-offset;
    let mask1 = raw-mask-for-bits-strictly-below(n);
    let mask2 = raw-mask-for-bits-above(n);

    let word1 :: <raw-machine-word> = bit-vector-word(vector, start-word-offset);

    for (i :: <integer> from 0 below result.word-size)
      let word2 :: <raw-machine-word> = bit-vector-word(vector, start-word-offset + i + 1);

      let mword1 = primitive-machine-word-logand(mask1,
                     primitive-machine-word-unsigned-shift-right
                       (word1, integer-as-raw(start-bit-offset)));
      let mword2 = primitive-machine-word-logand(mask2,
                     primitive-machine-word-shift-left-low
                       (word2, integer-as-raw(n)));
      let new-word = primitive-machine-word-logior(mword1, mword2);
      bit-vector-word(result, i) := new-word;
      word1 := word2;
    end for;
  end if;
  result;
end method;
******* */

//
// BIT-VECTOR-AND
//
define function bit-vector-and
    (vector1 :: <bit-vector>, vector2 :: <bit-vector>,
     #key pad1 :: <bit> = 0, pad2 :: <bit> = 0)
 => (result :: <bit-vector>, pad :: <bit>)

  let result-size :: <integer>
    = if (pad1 = 0)
	if (pad2 = 0)
	  min(vector1.size, vector2.size);
	else
	  vector1.size;
	end if;
      else
	if (pad2 = 0)
	  vector2.size;
	else
	  max(vector1.size, vector2.size);
	end if;
      end if;
  let result :: <bit-vector> = make(<bit-vector>, size: result-size);

  if (result-size > 0)
    // Sort vectors so that v1.size <= v2.size
    let (v1 :: <bit-vector>, v2 :: <bit-vector>, p1 :: <bit>, p2 :: <bit>)
      = if (vector1.size <= vector2.size)
          values(vector1, vector2, pad1, pad2);
        else
          values(vector2, vector1, pad2, pad1);
        end if;
    bit-vector-and-internal!(result, v1, v2, p1, p2);
  end if;
  values(result, logand(pad1, pad2));
end function;


//
// BIT-VECTOR-AND!
//
define function bit-vector-and!
    (vector1 :: <bit-vector>, vector2 :: <bit-vector>,
     #key pad1 :: <bit> = 0, pad2 :: <bit> = 0)
 => (result :: <bit-vector>, pad :: <bit>)

  let (result :: <bit-vector>, result-size :: <integer>)
    = if (vector2.size <= vector1.size | pad1 = 0)
	values(vector1, vector1.size);
      else
	values(make(<bit-vector>, size: vector2.size), vector2.size);
      end if;

  if (result-size > 0)
    // Sort vectors so that v1.size <= v2.size
    let (v1 :: <bit-vector>, v2 :: <bit-vector>, p1 :: <bit>, p2 :: <bit>)
      = if (vector1.size <= vector2.size)
          values(vector1, vector2, pad1, pad2);
        else
          values(vector2, vector1, pad2, pad1);
        end if;
    bit-vector-and-internal!(result, v1, v2, p1, p2);
  end if;
  values(result, logand(pad1, pad2));
end;


//
// BIT-VECTOR-ANDC2
//
define function bit-vector-andc2
    (vector1 :: <bit-vector>, vector2 :: <bit-vector>,
     #key pad1 :: <bit> = 0, pad2 :: <bit> = 0)
 => (result :: <bit-vector>, pad :: <bit>)

  let result-size = 
  if (pad1 = 0)
    if (pad2 = 0)
      vector1.size
    else
      min(vector1.size, vector2.size);
    end if;
  else
    if (pad2 = 0)
      max(vector1.size, vector2.size);
    else
      vector2.size;
    end if;
  end if;
  let result = make(<bit-vector>, size: result-size);

  if (result-size > 0)
    // Sort vectors so that v1.size <= v2.size
    if (vector1.size <= vector2.size)
      bit-vector-andc2-internal!(result, vector1, vector2, pad1, pad2);
    else
      bit-vector-andc2-internal-swapped!(result, vector2, vector1, pad2, pad1);
    end if;
  end if;
  values(result, logand(pad1, lognot(pad2)));
end;


//
// BIT-VECTOR-ANDC2!
//
define function bit-vector-andc2!
    (vector1 :: <bit-vector>, vector2 :: <bit-vector>,
     #key pad1 :: <bit> = 0, pad2 :: <bit> = 0)
 => (result :: <bit-vector>, pad :: <bit>)

  let (result :: <bit-vector>, result-size :: <integer>)
    = if (vector2.size <= vector1.size | pad1 = 0)
	values(vector1, vector1.size);
      else
	values(make(<bit-vector>, size: vector2.size), vector2.size);
      end if;

  if (result-size > 0)
    // Sort vectors so that v1.size <= v2.size
    if (vector1.size <= vector2.size)
      bit-vector-andc2-internal!(result, vector1, vector2, pad1, pad2);
    else
      bit-vector-andc2-internal-swapped!(result, vector2, vector1, pad2, pad1);
    end if;
  end if;
  values(result, logand(pad1, lognot(pad2)));
end;


//
// BIT-VECTOR-OR
//
define function bit-vector-or
    (vector1 :: <bit-vector>, vector2 :: <bit-vector>,
     #key pad1 :: <bit> = 0, pad2 :: <bit> = 0)
 => (result :: <bit-vector>, pad :: <bit>)

  let result-size = 
  if (pad1 = 0)
    if (pad2 = 0)
      max(vector1.size, vector2.size);
    else
      vector2.size;
    end if;
  else
    if (pad2 = 0)
      vector1.size;
    else
      min(vector1.size, vector2.size);
    end if;
  end if;
  let result = make(<bit-vector>, size: result-size);

  if (result-size > 0)
    // Sort vectors so that v1.size <= v2.size
    let (v1, v2, p1, p2)
      = if (vector1.size <= vector2.size)
          values(vector1, vector2, pad1, pad2);
        else
          values(vector2, vector1, pad2, pad1);
        end if;
    bit-vector-or-internal!(result, v1, v2, p1, p2);
  end if;
  values(result, logior(pad1, pad2));
end;


//
// BIT-VECTOR-OR!
//
define function bit-vector-or!
    (vector1 :: <bit-vector>, vector2 :: <bit-vector>,
     #key pad1 :: <bit> = 0, pad2 :: <bit> = 0)
 => (result :: <bit-vector>, pad :: <bit>)

  let (result :: <bit-vector>, result-size :: <integer>)
    = if (vector2.size <= vector1.size | pad1 = 1)
	values(vector1, vector1.size);
      else
	values(make(<bit-vector>, size: vector2.size), vector2.size);
      end if;

  if (result-size > 0)
    // Sort vectors so that v1.size <= v2.size
    let (v1, v2, p1, p2)
      = if (vector1.size <= vector2.size)
          values(vector1, vector2, pad1, pad2);
        else
          values(vector2, vector1, pad2, pad1);
        end if;
    bit-vector-or-internal!(result, v1, v2, p1, p2);
  end if;
  values(result, logior(pad1, pad2));
end;


//
// BIT-VECTOR-XOR
//
define function bit-vector-xor
    (vector1 :: <bit-vector>, vector2 :: <bit-vector>,
     #key pad1 :: <bit> = 0, pad2 :: <bit> = 0)
 => (result :: <bit-vector>, pad :: <bit>)

  let result-size = max(vector1.size, vector2.size);
  let result = make(<bit-vector>, size: result-size);

  if (result-size > 0)
    // Sort vectors so that v1.size <= v2.size
    let (v1, v2, p1, p2)
      = if (vector1.size <= vector2.size)
          values(vector1, vector2, pad1, pad2);
        else
          values(vector2, vector1, pad2, pad1);
        end if;
    bit-vector-xor-internal!(result, v1, v2, p1, p2);
  end if;
  values(result, logxor(pad1, pad2));
end;


//
// BIT-VECTOR-XOR!
//
define function bit-vector-xor!
    (vector1 :: <bit-vector>, vector2 :: <bit-vector>,
     #key pad1 :: <bit> = 0, pad2 :: <bit> = 0)
 => (result :: <bit-vector>, pad :: <bit>)

  let (result :: <bit-vector>, result-size :: <integer>)
    = if (vector2.size <= vector1.size)
	values(vector1, vector1.size);
      else
	values(make(<bit-vector>, size: vector2.size), vector2.size);
      end if;

  if (result-size > 0)
    // Sort vectors so that v1.size <= v2.size
    let (v1, v2, p1, p2)
      = if (vector1.size <= vector2.size)
          values(vector1, vector2, pad1, pad2);
        else
          values(vector2, vector1, pad2, pad1);
        end if;
    bit-vector-xor-internal!(result, v1, v2, p1, p2);
  end if;
  values(result, logxor(pad1, pad2));
end;


//
// BIT-VECTOR-NOT
//
define function bit-vector-not
    (vector :: <bit-vector>, #key pad :: <bit> = 0)
 => (result :: <bit-vector>, result-pad :: <bit>)
  let result :: <bit-vector> = make(<bit-vector>, size: vector.size);
  for (i :: <integer> from 0 below vector.word-size)
    bit-vector-word(result, i) := 
        primitive-machine-word-lognot(bit-vector-word(vector, i));
  end for;
  values(result, if (pad = 0) 1 else 0 end);
end;


//
// BIT-VECTOR-NOT!
//
define function bit-vector-not!
    (vector :: <bit-vector>, #key pad :: <bit> = 0)
 => (result :: <bit-vector>, result-pad :: <bit>)
  for (i :: <integer> from 0 below vector.word-size)
    bit-vector-word(vector, i) :=
        primitive-machine-word-lognot(bit-vector-word(vector, i));
  end for;
  values(vector, if (pad = 0) 1 else 0 end);
end;


//
// BIT-COUNT
//
define function bit-count
    (vector :: <bit-vector>, #key bit-value :: <bit> = 1)
 => (count :: <integer>)

  let count :: <integer> = 0;
  let vector-size :: <integer> = vector.size;
  let bit-offset = compute-bit-offset(vector-size);

  for (i :: <integer> from 0 below vector.word-size)
    let word :: <raw-machine-word> = bit-vector-word(vector, i);
    if (i = (vector.word-size - 1) & bit-offset ~= 0)
      // Mask off the tail bits in the final word if necessary
      word := primitive-machine-word-logand
                (word, raw-mask-for-bits-strictly-below(bit-offset));
    end if;

    // Count the bits which are 1 in this word
    let word-count :: <integer> =
      raw-as-integer(primitive-machine-word-logand(word, integer-as-raw(1)));
    for (j :: <integer> from 1 below $word-size)
      word := primitive-machine-word-unsigned-shift-right
                (word, integer-as-raw(1));
      let new-word-count :: <integer> = word-count +
        raw-as-integer(primitive-machine-word-logand(word, integer-as-raw(1)));
      word-count := new-word-count;
    end for;

    let new-count :: <integer> = count + word-count;
    count := new-count;
  end for;

  if (bit-value = 0)
    vector.size - count;
  else
    count;
  end if;
end function;


/*
//
// bit-vector-empty?
//
define function bit-vector-empty?
    (vector :: <bit-vector>) => (empty? :: <boolean>)
  block(return)
    for (i :: <integer> from 0 below vector.word-size)
      if (raw-as-integer(bit-vector-word(vector, i)) = 0)
        return(#f);
      end if;
    end for;
    return(#t);
  end block;
end function;
*/
