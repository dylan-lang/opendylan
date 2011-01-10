module:    native-rtg
Synopsis:  Basic Primitives for the Dylan runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND





/// Basic primitives ...



define runtime-primitive strlen
  // On entry:
  //    eax: a null-terminated C-style string
  // On exit:
  //    the size of the string as a raw integer

  arg0 arg0;
  result result;
  nreg base, ptr, word;
  tag found0, found1, found2, found3, word-loop;

  let big-endian? = be.big-endian?;

  ins--move(be, base, arg0);
  ins--move(be, ptr, arg0);
  //
  // Some strings might not be word aligned - so it might be
  // more efficient to test the early bytes here, before dropping 
  // into word-loop. We don't bother with that optimization just yet.
  //
  // Word aligned test
  ins--tag(be, word-loop);
  ins--ld(be, word, ptr, 0);

  if (big-endian?)
    ins--bnand(be, found0, word, #xff000000);
    ins--bnand(be, found1, word, #x00ff0000);
    ins--bnand(be, found2, word, #x0000ff00);
    ins--add(be, ptr, ptr, 4);
    ins--band(be, word-loop, word, #x000000ff);
  else
    ins--beq-byte(be, found0, word, 0);
    ins--bnand(be, found1, word, #x0000ff00);
    ins--bnand(be, found2, word, #x00ff0000);
    ins--add(be, ptr, ptr, 4);
    ins--band(be, word-loop, word, #xff000000);
  end if;

  // Start of exit cases
  ins--sub(be, ptr, ptr, 3);
  ins--tag(be, found2);
  ins--add(be, ptr, ptr, 1);
  ins--tag(be, found1);
  ins--add(be, ptr, ptr, 1);
  ins--tag(be, found0);
  ins--sub(be, result, ptr, base);
  ins--rts-and-drop(be, 0);
end runtime-primitive;




define runtime-primitive raw-as-string
/*
  On entry:
    Str  - a raw C string
  On exit:
    A pointer to a freshly boxed Dylan <byte-string>
*/

  arg0 arg0;
  result result;
  nreg str, len, total-size, rep-size, start;
  greg new;

  ins--move(be, str, arg0);
  ins--call(be, primitive-strlen-ref, 1);
  ins--move(be, len, result); // the size of the C string

  // now allocate the Dylan string
  let rep-size-slot = 1; // offset of repeated slot (in words)
  let base-size = 8;     // base size: 4 bytes for wrapper, 4 for size slot
  let base+null = base-size + 1; // base size + extra byte for null terminator

  ins--add(be, total-size, len, base+null + 3); // add the byte part & word align
  ins--and(be, total-size, total-size, ash(-1, 2));
  ins--move(be, rep-size, len);
  
  // call MM primitive
  op--allocate-byte-repeated-unfilled(be, new, total-size, dylan-byte-string-class, 
				      rep-size, rep-size-slot);

  ins--add(be, start, new, base-size);
  ins--add(be, len, len, 1); // copy the nul terminator as well as the real data
  ins--copy-bytes-down(be, start, str, len);

  ins--move(be, result, new);
  ins--rts-and-drop(be, 0);
end runtime-primitive;


define runtime-primitive random
  op--unimplemented-primitive(be);
end runtime-primitive;


define runtime-primitive arg-error
  op--unimplemented-primitive(be);
end runtime-primitive;



define runtime-primitive raw-as-single-float
/*
  On entry:
    X  - a raw single float (passed in ST)
  On exit:
    A pointer to a freshly boxed single-float
*/

  float-arg0 x;
  result result;
  sfreg sdata;
  nreg data;
  reg obj;
  
  // Laboriously move around the data until its in the right place:
  ins--fmove(be, sdata, x);

  // call MM primitive
  op--allocate-untraced(be, obj, 8, single-float-class);

  ins--move(be, result, obj);
  ins--move-from-sfreg(be, data, sdata);
  ins--st(be, data, result, 4);
  ins--rts-and-drop(be, 0);
end runtime-primitive;



define runtime-primitive raw-as-double-float
/*
  On entry:
    X  - a raw double float (passed in ST)
  On exit:
    A pointer to a freshly boxed double-float
*/

  float-arg0 x;
  result result;
  dfreg ddata;
  nreg low, high;
  reg obj;
  
  // Laboriously move around the data until its in the right place:
  ins--dmove(be, ddata, x);

  // call MM primitive
  op--allocate-untraced(be, obj, 12, double-float-class);
  ins--move(be, result, obj);
  ins--move-from-dfreg(be, low, high, ddata);
  let (first, second) =
    if (be.big-endian?) values(high, low) else values(low, high) end;
  ins--st(be, first, result, 4);
  ins--st(be, second, result, 8);
  ins--rts-and-drop(be, 0);
end runtime-primitive;



define used-by-client runtime-primitive wrap-machine-word
/*
  On entry:
    X  - a raw machine word
  On exit:
    A pointer to freshly allocated memory
*/

  arg0 arg0;
  result result;
  reg obj;
  nreg x;

  ins--move(be, x, arg0);
  
  // call MM primitive
  op--allocate-untraced(be, obj, 8, machine-word-class);
  ins--move(be, result, obj);
  ins--st(be, x, result, 4);

  ins--rts-and-drop(be, 0);
end runtime-primitive;



define used-by-client runtime-primitive wrap-c-pointer
/*
  On entry:
    class, x
  On exit:
    value (unless an error is signalled)
*/

  result result;
  nreg x;
  reg obj, class;

  op--load-arguments(be, class, x);
  op--allocate-untraced(be, obj, 8, class);
  ins--move(be, result, obj);
  ins--st(be, x, result, 4);

  op--rts-dropping-n-args(be, 2);
end runtime-primitive;



define method op--wrap-big-abstract-integer
    (be :: <harp-back-end>, dest :: <register>, raw :: <register>)
  // call MM primitive
  op--allocate-untraced(be, dest, 8, machine-word-class);
  ins--st(be, raw, dest, 4);
end method;



define constant $biggest-fixnum  =  #x1fffffff;

define constant $smallest-fixnum = -1 - $biggest-fixnum;


define runtime-primitive wrap-abstract-integer
/*
  On entry:
    X  - a raw integer
  On exit:
    An <integer> or an <extended-integer>
*/

  arg0 x;
  result result;
  tag bignum, done;
  nreg tmp;


  ins--bgt(be, bignum, x, $biggest-fixnum);
  ins--blt(be, bignum, x, $smallest-fixnum);
  op--taggify(be, result, x);
  ins--tag(be, done);
  ins--rts-and-drop(be, 0);

  ins--tag(be, bignum);
  ins--move(be, tmp, x);
  op--wrap-big-abstract-integer(be, result, tmp);
  ins--bra(be, done);

end runtime-primitive;



define runtime-primitive wrap-unsigned-abstract-integer
/*
  On entry:
    X  - a raw integer
  On exit:
    An <integer> or an <extended-integer>
*/

  arg0 x;
  result result;
  tag bignum, done;
  nreg tmp;

  ins--bhi(be, bignum, x, $biggest-fixnum);
  op--taggify(be, result, x);
  ins--tag(be, done);
  ins--rts-and-drop(be, 0);

  ins--tag(be, bignum);
  ins--move(be, tmp, x);
  op--wrap-big-abstract-integer(be, result, tmp);
  ins--bra(be, done);

end runtime-primitive;




define runtime-primitive unwrap-abstract-integer
/*
  On entry:
    X  - an abstract integer - either a fixnum or a bignum
  On exit:
    An <integer> or an <extended-integer>
*/

  arg0 x;
  nreg temp;
  result result;
  tag bignum, done;

  ins--and(be, temp, x, #b11);   // mask out the tag bits
  ins--beq(be, bignum, temp, 0); // If they're zero, it's a bignum
  op--untaggify(be, result, x);
  ins--tag(be, done);
  ins--rts-and-drop(be, 0);

  ins--tag(be, bignum);
  ins--ld(be, result, x, 4);     // internal data for the bignum
  ins--bra(be, done);

end runtime-primitive;



define byte runtime-literal low-zeros-table-ref = "low-zeros-table",
   data: #[4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0];


define runtime-primitive machine-word-count-low-zeros
/*
  On entry:
    X  - a raw machine word
  On exit:
    A raw machine word containing the number of low zeros in x
*/

  arg0 x;
  result result;
  nreg count, word;
  tag non-zero, byte0, byte1, byte2, nibble0;


  ins--bne(be, non-zero, x, 0); // handle the zero case early
  ins--move(be, result, 32);
  ins--rts-and-drop(be, 0);
  
  ins--tag(be, non-zero);
  ins--move(be, word, x);
  ins--move(be, count, 0);

  local method skip-zero-bits (n :: <integer>)
          // Skip bits by shuffling the higher bytes down, 
          // adding to count as we go
          ins--add(be, count, count, n);
          ins--lsr(be, word, word, n);
        end method;

  // First check for ones in individual bytes
  ins--bne-byte(be, byte0, word, 0);
  ins--band(be, byte1, word, #x0000ff00);
  ins--band(be, byte2, word, #x00ff0000);
  // Now start skipping bits for each byte
  skip-zero-bits(8);
  ins--tag(be, byte2);
  skip-zero-bits(8);
  ins--tag(be, byte1);
  skip-zero-bits(8);
  ins--tag(be, byte0);
  // Count now has a count of the zero bits in full bytes
  // Add the low zeros from the (non zero) low byte of word

  // Next look at the nibbles, just like we did for the bytes
  ins--band(be, nibble0, word, #x0f);
  skip-zero-bits(4);
  ins--tag(be, nibble0);

  // Now do a table lookup, based on the value of the remaining nibble
  ins--and(be, word, word, #x0f);  // mask out all but the nibble
  ins--ldb(be, word, low-zeros-table-ref, word);
  ins--add(be, result, count, word);
  ins--rts-and-drop(be, 0);
end runtime-primitive;



define byte runtime-literal high-zeros-table-ref = "high-zeros-table",
   data: #[4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0];


define runtime-primitive machine-word-count-high-zeros
/*
  On entry:
    X  - a raw machine word
  On exit:
    A raw machine word containing the number of high zeros in x
*/

  arg0 x;
  result result;
  nreg count, word;
  tag non-zero, byte3, byte2, byte1, nibble1;


  ins--bne(be, non-zero, x, 0); // handle the zero case early
  ins--move(be, result, 32);
  ins--rts-and-drop(be, 0);
  
  ins--tag(be, non-zero);
  ins--move(be, word, x);
  ins--move(be, count, 0);

  local method skip-zero-bits (n :: <integer>)
          // Skip bits by shuffling the higher bytes down, 
          // adding to count as we go
          ins--add(be, count, count, n);
          ins--asl(be, word, word, n);
        end method;

  // First check for ones in individual bytes
  ins--band(be, byte3, word, #xff000000);
  ins--band(be, byte2, word, #x00ff0000);
  ins--band(be, byte1, word, #x0000ff00);
  // Now start skipping bits for each byte
  skip-zero-bits(8);
  ins--tag(be, byte1);
  skip-zero-bits(8);
  ins--tag(be, byte2);
  skip-zero-bits(8);
  ins--tag(be, byte3);
  // Count now has a count of the zero bits in full bytes
  // Add the high zeros from the (non zero) high byte of word

  // Next look at the nibbles, just like we did for the bytes
  ins--band(be, nibble1, word, #xf0000000);
  skip-zero-bits(4);
  ins--tag(be, nibble1);

  // Now do a table lookup, based on the value of the topmost nibble
  ins--lsr(be, word, word, 32 - 4);  // mask out all but the nibble
  ins--ldb(be, word, high-zeros-table-ref, word);
  ins--add(be, result, count, word);
  ins--rts-and-drop(be, 0);
end runtime-primitive;



define runtime-primitive type-check
/*
  On entry:
    value, type
  On exit:
    value (unless an error is signalled)
*/

  result result;
  nreg value, type;
  tag done;

  op--load-arguments(be, value, type);
  op--primitive-instance?(be, done, value, type);
  // If that test failed, then call the error reporter
  op--call-iep(be, dylan-type-check-error, value, type);

  ins--tag(be, done);
  ins--move(be, result, value);
  op--rts-dropping-n-args(be, 2);
end runtime-primitive;

define runtime-primitive type-check-values
/*
  On entry:
    first-value, types, rest-type
    
    first-value is the first multiple value
    types are the list of specified types to be matched against
    rest-type is the type-constraint specified for the rest parameter

  On exit: (unless an error is signalled)
    first-value
    preserve multiple-values on entry
*/

  result result;
  stack stack;
  nreg first-value, types, rest-type,
       value, values-count, values-pointer, values-index,
       type, size-types, types-pointer, types-index;
  tag check, check-next, 
      check-rest, check-rest-internal, check-rest-next,
      done;

  op--load-arguments(be, first-value, types, rest-type);

  op--push-multiple-values(be);

  ins--ld(be, values-count, stack, 0);
  ins--add(be, values-pointer, stack, 4);
  ins--add(be, types-pointer, types, 8);
  op--vector-size-times-4(be, size-types, types);
  ins--move(be, values-index, 0);
  ins--move(be, types-index, 0);

  ins--tag(be, check);
  ins--beq(be, check-rest, types-index, size-types);
  ins--bge(be, check-rest, types-index, values-count); // this should never happen
  ins--ld(be, value, values-pointer, values-index);
  ins--ld(be, type, types-pointer, types-index);
  ins--beq(be, check-next, type, dylan-false);
  op--primitive-instance?(be, check-next, value, type);
  // If that test failed, then call the error reporter
  op--call-iep(be, dylan-type-check-error, value, type);

  ins--tag(be, check-next);
  ins--add(be, values-index, values-index, 4);
  ins--add(be, types-index, types-index, 4);
  ins--bra(be, check);

  ins--tag(be, check-rest);
  ins--beq(be, done, rest-type, dylan-false);

  ins--tag(be, check-rest-internal);
  ins--bge(be, done, values-index, values-count);
  ins--ld(be, value, values-pointer, values-index);
  op--primitive-instance?(be, check-rest-next, value, rest-type);
  // If that test failed, then call the error reporter
  op--call-iep(be, dylan-type-check-error, value, rest-type);

  ins--tag(be, check-rest-next);
  ins--add(be, values-index, values-index, 4);
  ins--bra(be, check-rest-internal);

  ins--tag(be, done);
  op--pop-multiple-values(be);
  op--rts-dropping-n-args(be, 3);
end runtime-primitive;

define runtime-primitive type-check-rest-values
/*
  On entry:
    first-value-index, rest-type
    
    first-value-index is the index of first multiple value of the #rest stuff
    rest-type is the type-constraint specified for the rest parameter

  On exit: (unless an error is signalled)
    preserve multiple-values on entry
*/

  result result;
  stack stack;
  nreg first-value-index, rest-type,
       value, values-count, values-pointer, values-index;
  tag check-rest, check-rest-next, done;

  op--load-arguments(be, first-value-index, rest-type);

  op--push-multiple-values(be);

  ins--ld(be, values-count, stack, 0);
  ins--add(be, values-pointer, stack, 4);
  ins--move(be, values-index, first-value-index);

  ins--tag(be, check-rest);
  ins--bge(be, done, values-index, values-count);
  ins--ld(be, value, values-pointer, values-index);
  op--primitive-instance?(be, check-rest-next, value, rest-type);
  // If that test failed, then call the error reporter
  op--call-iep(be, dylan-type-check-error, value, rest-type);

  ins--tag(be, check-rest-next);
  ins--add(be, values-index, values-index, 4);
  ins--bra(be, check-rest);

  ins--tag(be, done);
  op--pop-multiple-values(be);
  op--rts-dropping-n-args(be, 2);
end runtime-primitive;

define method op--primitive-instance?
    (be :: <harp-back-end>, true-tag :: <tag>, value, type)
  with-harp (be)
    nreg type-test;
    greg type-copy;
    result result;
  
    ins--move(be, type-copy, type); // help the colourer
    // Don't check if the type is <object>
    ins--beq(be, true-tag, type-copy, object-class-class);
    // Otherwise check by calling the predicate function in the type
    ins--ld(be, type-test, type-copy, be.type-instancep-function-offset);
    op--call-iep(be, type-test, value, type-copy);
    ins--bne(be, true-tag, result, dylan-false);
  end with-harp;
end method;

define method op--primitive-instance?-setting-result
    (be :: <harp-back-end>, value, type)
  with-harp (be)
    nreg type-test;
    greg type-copy;
    result result;
    tag done, full-test;
  
    ins--move(be, type-copy, type); // help the colourer
    // Don't check if the type is <object>
    ins--bne(be, full-test, type-copy, object-class-class);
    ins--move(be, result, dylan-true);
    ins--bra(be, done);
    ins--tag(be, full-test);
    // Otherwise check by calling the predicate function in the type
    ins--ld(be, type-test, type-copy, be.type-instancep-function-offset);
    op--call-iep(be, type-test, value, type-copy);
    ins--tag(be, done);
  end with-harp;
end method;


define runtime-primitive adjust-mv
  // On entry:
  //   wanted  - the MV count to set
  //           - THIS USES AN UNUSUAL CALLING CONVENTION
  //           - the argument is passed in MList so we don't clobber
  //           - any initial values.
  // On exit:
  //  The MV count will be set, and the MV area filled in

  mlist wanted;
  arg0 arg0;
  nreg count, extra, dest, mv-area;
  tag have-mvs, mvs-done, set-single, set-multiple;

  // First check whether we are single / multiple values now
  ins--bmvset(be, have-mvs);

  // Single values case
  // If we want 1 value anyway, then do nowt
  ins--beq(be, mvs-done, wanted, 1);
  // Otherwise, we set the count as a MV, and fill any extra values
  op--st-mv-count(be, wanted);
  ins--beq(be, set-single, wanted, 0);
  op--ld-mv-area-address(be, mv-area);
  ins--fill-words-w(be, mv-area, wanted, dylan-false);
  ins--st(be, arg0, mv-area, 0);  // The first value comes from EAX
  ins--tag(be, set-single);
  ins--reset-values(be);
  ins--bra(be, mvs-done);
  
  // Multiple values cases
  ins--tag(be, have-mvs);
  op--ld-mv-count(be, count);
  // First check if we need to change anything
  ins--beq(be, mvs-done, wanted, count);
  // If we do need to change, then there are numerous cases
  // Case 1:- the new count is 1 - if so, just set it
  ins--beq(be, set-single, wanted, 1);
  // Cases 2 & 3 -- store new MV count
  op--st-mv-count(be, wanted);
  // Case 2:- the new count is less than the old count, so set it as an MV
  ins--blt(be, set-multiple, wanted, count);
  // Case 3: the new count is greater than the old count, so top up with #f
  ins--sub(be, extra, wanted, count);   // calculate the amount to fill
  ins--move(be, dest, count);
  op--ld-mv-area-address(be, mv-area);
  ins--asl(be, dest, dest, 2);          // size of valid MV data in bytes
  ins--add(be, dest, dest, mv-area);    // address of first invalid MV slot
  ins--fill-words-w(be, dest, extra, dylan-false);

  ins--tag(be, set-multiple);
  ins--set-values(be);

  ins--tag(be, mvs-done);
  ins--rts-and-drop(be, 0);

end runtime-primitive;


define runtime-primitive adjust-mv-rest
  // On entry:
  //   wanted  - the MV count to set
  //           - THIS USES AN UNUSUAL CALLING CONVENTION
  //           - the argument is passed in MList so we don't clobber
  //           - any initial values.
  // On exit:
  //  The MV count will be set if it's bigger than before,
  //  and the MV area filled in

  mlist wanted;
  arg0 arg0;
  nreg count, extra, dest, mv-area;
  tag have-mvs, mvs-done, set-single, set-multiple;

  // First check whether we are single / multiple values now
  ins--bmvset(be, have-mvs);

  // Single values case
  // If we want 1 or less value anyway, then do nowt
  ins--ble(be, mvs-done, wanted, 1);
  // Otherwise, we set the count as a MV, and fill any extra values
  op--st-mv-count(be, wanted);
  op--ld-mv-area-address(be, mv-area);
  ins--fill-words-w(be, mv-area, wanted, dylan-false);
  ins--st(be, arg0, mv-area, 0);  // The first value comes from EAX
  ins--tag(be, set-single);
  ins--reset-values(be);
  ins--bra(be, mvs-done);
  
  // Multiple values cases
  ins--tag(be, have-mvs);
  op--ld-mv-count(be, count);
  // First check if we need to change anything
  ins--ble(be, mvs-done, wanted, count);
  // If we do need to change, then there are two cases
  // Case 1:- the new count is 1 - if so, just set it
  ins--beq(be, set-single, wanted, 1);
  // Case 2: the new count is greater than the old count, so top up with #f
  ins--sub(be, extra, wanted, count);   // calculate the amount to fill
  ins--move(be, dest, count);
  op--ld-mv-area-address(be, mv-area);
  ins--asl(be, dest, dest, 2);          // size of valid MV data in bytes
  ins--add(be, dest, dest, mv-area);    // address of first invalid MV slot
  ins--fill-words-w(be, dest, extra, dylan-false);
  ins--tag(be, set-multiple);
  op--st-mv-count(be, wanted);
  ins--set-values(be);

  ins--tag(be, mvs-done);
  ins--rts-and-drop(be, 0);

end runtime-primitive;

// This just pads extra required multiple-values with #f without 
// setting the MV count

define runtime-primitive pad-mv
  // On entry:
  //   wanted  - the number of locally required multiple values
  //           - THIS USES AN UNUSUAL CALLING CONVENTION
  //           - the argument is passed in MList so we don't clobber
  //           - any initial values.
  // On exit:
  //  The MV area padded with #f

  mlist wanted;
  nreg count, extra, dest, mv-area;
  tag have-mvs, mvs-done;

  // First check whether we are single / multiple values now
  ins--bmvset(be, have-mvs);

  // Single values case
  // If we want 1 value anyway, then do nowt
  ins--ble(be, mvs-done, wanted, 1);
  // Otherwise, fill any extra values
  op--ld-mv-area-address(be, mv-area);
  ins--fill-words-w(be, mv-area, wanted, dylan-false);
  ins--bra(be, mvs-done);
  
  // Multiple values cases
  ins--tag(be, have-mvs);
  op--ld-mv-count(be, count);
  // First check if we need to change anything
  ins--beq(be, mvs-done, wanted, 1);
  ins--ble(be, mvs-done, wanted, count);
  // The new count is greater than the old count, so top up with #f
  ins--sub(be, extra, wanted, count);   // calculate the amount to fill
  ins--move(be, dest, count);
  op--ld-mv-area-address(be, mv-area);
  ins--asl(be, dest, dest, 2);          // size of valid MV data in bytes
  ins--add(be, dest, dest, mv-area);    // address of first invalid MV slot
  ins--fill-words-w(be, dest, extra, dylan-false);

  ins--tag(be, mvs-done);
  ins--rts-and-drop(be, 0);

end runtime-primitive;


define runtime-primitive set-mv-from-vector
  // On entry:
  //   vec  - A <simple-object-vector> of multiple values
  // On exit:
  //  The MV count will be set and the MV area filled in
  //  Function is preserved

  arg0 arg0;
  function function;
  greg fn;

  ins--move(be, fn, function);
  op--restore-multiple-values-from-vector(be, arg0);
  ins--move(be, function, fn);
  ins--rts-and-drop(be, 0);

end runtime-primitive;

