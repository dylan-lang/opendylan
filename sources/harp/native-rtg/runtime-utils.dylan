module:    harp-native-rtg
Synopsis:  Utilities for the Dylan runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define method special-case?
    (be :: <harp-back-end>, num-args :: <integer>)
 => (special :: <boolean>)
  num-args < be.registers.arguments-passed-in-registers
end;

define method special-case?
    (be :: <harp-back-end>, num-args)
 => (special :: <boolean>)
  #f
end;

define macro argument-register
  { argument-register(?index:expression) } =>
  { ?=be.registers.reg-machine-arguments[?index] }
end macro;

define macro c-argument-register
  { c-argument-register(?index:expression) } =>
  { ?=be.registers.reg-c-machine-arguments[?index] }
end macro;

ignore(required-arguments-in-registers);

define inline method required-arguments-in-registers
    (be :: <harp-back-end>, requireds) => (r :: <integer>);
  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  if (special-case?(be, requireds))
    // The remaining register args are not required args
    requireds
  else
    // argument registers full of required args
    max-num-arg-regs
  end;
end method;

ignore(arguments-in-registers);

define inline method arguments-in-registers
    (be :: <harp-back-end>, num-args :: <integer>) => (r :: <integer>)
  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  min(num-args, max-num-arg-regs)
end method;

ignore(arguments-on-stack);

define inline method arguments-on-stack
    (be :: <harp-back-end>, num-args :: <integer>) => (r :: <integer>)
  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  max(num-args - max-num-arg-regs, 0)
end method;

/*
define inline method c-arguments-in-registers
    (be :: <harp-back-end>, num-args :: <integer>) => (r :: <integer>)
  let max-num-arg-regs = be.registers.c-arguments-passed-in-registers;
  min(num-args, max-num-arg-regs)
end method;
*/

define inline method c-arguments-on-stack
    (be :: <harp-back-end>, num-args :: <integer>) => (r :: <integer>)
  let max-num-arg-regs = be.registers.c-arguments-passed-in-registers;
  max(num-args - max-num-arg-regs, 0)
end method;

define method make-tags(be :: <harp-back-end>, num-cases :: <integer>)
 => (tags :: <simple-object-vector>)
  let tags :: <simple-object-vector> = make(<vector>, size: num-cases);
  for (i :: <integer> from 0 below num-cases)
    tags[i] := make-tag(be);
  end;
  tags
end method;

//// General object processing utilities


// TAG-AS-INTEGER
// Returns either an integer or a register which is the value 
// tagged as an integer

define method tag-as-integer 
    (be :: <harp-back-end>, value :: <register>) 
 => (reg :: <register>)
  let dest = make-n-register(be);
  op--taggify(be, dest, value);
  dest;
end method;


define method tag-as-integer 
    (be :: <harp-back-end>, value :: <integer>)
 => (val :: <integer>)
  (value * 4) + 1;
end method;


// TAG-AS-CHARACTER
// Returns either an integer or a register which is the value 
// tagged as an character

/*
define method tag-as-character 
    (be :: <harp-back-end>, value :: <register>) 
 => (reg :: <register>)
  let dest = make-n-register(be);
  op--taggify-as-character(be, dest, value);
  dest;
end method;


define method tag-as-character 
    (be :: <harp-back-end>, value :: <integer>)
 => (val :: <integer>)
  (value * 4) + 2;
end method;
*/



define method op--taggify
    (be :: <harp-back-end>, dylanint :: <register>, rawint :: <register>)
  ins--asl(be, dylanint, rawint, 2);         // shift by 2 ...
  ins--add(be, dylanint, dylanint, 1);       // ... and add 1 to tag the int
end method;


ignore(op--taggify-as-character);

define method op--taggify-as-character
    (be :: <harp-back-end>, dylanchar :: <register>, rawchar :: <register>)
  ins--asl(be, dylanchar, rawchar, 2);         // shift by 2 ...
  ins--add(be, dylanchar, dylanchar, 2);       // ... and add 2 to tag the char
end method;


define method op--untaggify
    (be :: <harp-back-end>, rawint :: <register>, dylanint :: <register>)
  ins--asr(be, rawint, dylanint, 2);         // shift by 2 ...
end method;


ignore(op--untaggify-times-4);

define method op--untaggify-times-4
    (be :: <harp-back-end>, rawint :: <register>, dylanint :: <register>)
  ins--and(be, rawint, dylanint, -4);         // just mask out the tag bits
end method;



// OP--BRANCH-IF-ODD-TAGGED-INTEGER
// branches to tag if the tagged integer is odd

ignore(op--branch-if-odd-tagged-integer);

define method op--branch-if-odd-tagged-integer
    (be :: <harp-back-end>, tag :: <tag>, integer)
  ins--bit(be, tag, integer, #b100, 0);
end method;


// OP--BRANCH-UNLESS-ODD-TAGGED-INTEGER
// is the reverse

ignore(op--branch-unless-odd-tagged-integer);

define method op--branch-unless-odd-tagged-integer
    (be :: <harp-back-end>, tag :: <tag>, integer)
  ins--nbit(be, tag, integer, #b100, 0);
end method;



// OP--VECTOR-SIZE-AS-TAGGED-INT loads a register with the tagged size of 
// the vector object

define method op--vector-size-as-tagged-int
    (be :: <harp-back-end>, dest :: <register>, vect :: <register>)
  ins--ld(be, dest, vect, 4);
end method;


// OP--SET-VECTOR-SIZE-AS-TAGGED-INT sets the size of a vector object
// to the given tagged int

define method op--set-vector-size-as-tagged-int
    (be :: <harp-back-end>, size, vect :: <register>)
  ins--st(be, size, vect, 4);
end method;


// OP--VECTOR-SIZE-TIMES-4 loads a register with the size of the vector
// as a raw integer multiplied by 4

ignore(op--vector-size-times-4);

define method op--vector-size-times-4
     (be :: <harp-back-end>, dest :: <register>, vect :: <register>)
  op--vector-size-as-tagged-int(be, dest, vect);
  ins--sub(be, dest, dest, 1); // remove the tag bit
end method;


// OP--VECTOR-SIZE loads a register with the size of the vector
// as a raw integer

define method op--vector-size
     (be :: <harp-back-end>, dest :: <register>, vect :: <register>)
  op--vector-size-as-tagged-int(be, dest, vect);
  op--untaggify(be, dest, dest); // remove the tag bit
end method;


// OP--SET-VECTOR-SIZE sets the size of a vector object to a tagged int
// derived from the given raw value.

ignore(op--set-vector-size);

define method op--set-vector-size
    (be :: <harp-back-end>, size :: <register>, vect :: <register>)
  let tagged-size = make-n-register(be);
  op--taggify(be, tagged-size, size);
  op--set-vector-size-as-tagged-int(be, tagged-size, vect);
end method;


define method op--set-vector-size
    (be :: <harp-back-end>, size :: <integer>, vect :: <register>)
  let tagged-size = size * 4 + 1;
  op--set-vector-size-as-tagged-int(be, tagged-size, vect);
end method;


// OP--HALF-VECTOR-SIZE loads a register with half the size of the vector
// as a raw integer

define method op--half-vector-size
     (be :: <harp-back-end>, dest :: <register>, vect :: <register>)
  op--vector-size-as-tagged-int(be, dest, vect);
  ins--asr(be, dest, dest, 3); // untag and divide by 2
end method;


// OP--SET-VECTOR-CLASS sets the class slot of a vector object to 
// <simple-object-vector>

ignore(op--set-vector-class);

define method op--set-vector-class
     (be :: <harp-back-end>, vect :: <register>)
  ins--st(be, dylan-sov-class, vect, 0)
end method;


// OP--METHOD-KEYWORDS loads a register with the keywords of the current
// function object

define method op--method-keywords
    (be :: <harp-back-end>, dest :: <register>,
     #key function = be.registers.reg-function)
  ins--ld(be, dest, function, be.function-keywords-offset);
end method;


// OP--KEYWORDS-SIZE loads a register with the raw size of the 
// number of keywords used by the current function object

define method op--keywords-size
    (be :: <harp-back-end>, dest :: <register>)
  let keys-vector = make-n-register(be);
  op--method-keywords(be, keys-vector);
  op--half-vector-size(be, dest, keys-vector);
end method;


// OP--KEYWORDS-SIZE-TIMES-4 loads a register with the raw size 
// times 4 of the number of keywords used by the current function 
// object

/*
define method op--keywords-size-times-4
    (be :: <harp-back-end>, dest :: <register>)
  op--method-keywords(be, dest);
  op--vector-size-as-tagged-int(be, dest, dest);
  // Size is stored multiplied by 2. and tagged. Divide by 2
  // to untag and leave result multiplied by 4.
  ins--asr(be, dest, dest, 1);
end method;
*/


/// Support for function signatures:


define constant number-required-mask = #x0000ff;
// define constant tagged-required-mask = #x0003ff;
// define constant number-values-mask   = #x00ff00;
// define constant tagged-values-mask   = #x03fc03;
// define constant number-values-offset = 8;
define constant key-p-mask           = #x010000;
ignore(tagged-key-p-mask);
define constant tagged-key-p-mask    = #x040000;
ignore(all-keys-p-mask);
define constant all-keys-p-mask      = #x020000;
define constant rest-p-mask          = #x040000;
define constant optionals-p-mask     = 
   logior(key-p-mask, rest-p-mask);
// define constant rest-value-p-mask    = #x080000;
// define constant next-p-mask          = #x100000;



// OP--FUNCTION-SIGNATURE loads a register with the signature object for the fn

define method op--function-signature
    (be :: <harp-back-end>, dest :: <register>,
     #key function = be.registers.reg-function)
  ins--ld(be, dest, function, be.function-signature-offset);
end method;


// OP--FUNCTION-PROPERTIES loads a register with the unprocessed
// function object properties

define method op--function-properties
    (be :: <harp-back-end>, dest :: <register>,
     #key function = be.registers.reg-function)
  op--function-signature(be, dest, function: function);
  ins--ld(be, dest, dest, be.signature-properties-offset);
end method;


// OP--FUNCTION-PROPERTIES-RAW loads register with  the
// function properties as a raw number.

define method op--function-properties-raw
     (be :: <harp-back-end>, dest :: <register>,
     #key function = be.registers.reg-function)
  op--function-properties(be, dest, function: function);
  op--untaggify(be, dest, dest);     // untag the properties
end method;


// OP--NUMBER-REQUIRED defines the other bytes too.

ignore(op--number-required);

define method op--number-required 
    (be :: <harp-back-end>, dest :: <register>,
     #key function = be.registers.reg-function)
  op--function-properties-raw(be, dest, function: function);
  ins--and(be, dest, dest, number-required-mask);     // mask out any upper bits
end method;


// OP--NUMBER-REQUIRED-AS-TAGGED-INT loads a  register with the
// number of required arguments as a tagged integer (fully safe)

/*
define method op--number-required-as-tagged-int
     (be :: <harp-back-end>, dest :: <register>,
     #key function = be.registers.reg-function)
  op--function-properties(be, dest, function: function);
  ins--and(be, dest, dest, tagged-required-mask); // mask out other bits
end method;
*/


// OP--NUMBER-REQUIRED-TIMES-4 loads a register with the
// number of required arguments as a raw integer multiplied by 4
// (fully safe)

ignore(op--number-required-times-4);

define method op--number-required-times-4
     (be :: <harp-back-end>, dest :: <register>,
     #key function = be.registers.reg-function)
  op--function-properties(be, dest, function: function);
  ins--and(be, dest, dest, ash(number-required-mask, 2)); // mask out other bits
end method;




// OP--BRANCH-IF-FUNCTION-WITH-OPTIONALS
// branches to tag if the function in reg-function has optionals

ignore(op--branch-if-function-with-optionals);

define method op--branch-if-function-with-optionals
    (be :: <harp-back-end>, tag :: <tag>,
     #key function = be.registers.reg-function)
  let properties = make-n-register(be);
  op--function-properties(be, properties, function: function);
  // mask out other bits
  ins--and(be, properties, properties, ash(optionals-p-mask, 2));
  ins--bne(be, tag, properties, 0);
end method;



// OP--BRANCH-IF-FUNCTION-WITH-ALL-KEYS
// branches to tag if the function in reg-function takes all-keys

/*
define method op--branch-if-function-with-all-keys
    (be :: <harp-back-end>, tag :: <tag>,
     #key function = be.registers.reg-function)
  let properties = make-n-register(be);
  op--function-properties(be, properties, function: function);
  // mask out other bits
  ins--and(be, properties, properties, ash(all-keys-p-mask, 2));
  ins--bne(be, tag, properties, 0);
end method;
*/



// OP--MULTIPLY-BY-4
// Returns either an integer or a register which is the value times 4

define method op--multiply-by-4
    (be :: <harp-back-end>, value :: <register>, #key may-be-temp?) 
     => (reg :: <register>)
  let dest = if (may-be-temp?) 
               be.registers.reg-tmp1; 
             else make-n-register(be);
             end;
  ins--asl(be, dest, value, 2);
  dest;
end method;


define method op--multiply-by-4
    (be :: <harp-back-end>, value :: <integer>, #key may-be-temp?)
     => (int :: <integer>)
  value * 4;
end method;


// OP--DIVIDE-BY-4
// Returns either an integer or a register which is the value div 4

define method op--divide-by-4
    (be :: <harp-back-end>, value :: <register>) => (reg :: <register>)
  let dest = make-n-register(be);
  ins--asr(be, dest, value, 2);
  dest;
end method;


define method op--divide-by-4
    (be :: <harp-back-end>, value :: <integer>) => (int :: <integer>)
  ash(value, -2);
end method;


// OP--ADD-4
// Returns either an integer or a register which is the value + 4

/*
define method op--add-4
    (be :: <harp-back-end>, value :: <register>) => (reg :: <register>)
  let dest = make-n-register(be);
  ins--add(be, dest, value, 4);
  dest;
end method;


define method op--add-4
    (be :: <harp-back-end>, value :: <integer>) => (int :: <integer>)
  value + 4;
end method;
*/


// OP--SUBTRACT-4
// Returns either an integer or a register which is the value - 4

ignore(op--subtract-4);

define method op--subtract-4
    (be :: <harp-back-end>, value :: <register>) => (reg :: <register>)
  let dest = make-n-register(be);
  ins--sub(be, dest, value, 4);
  dest;
end method;


define method op--subtract-4
    (be :: <harp-back-end>, value :: <integer>) => (int :: <integer>)
  value - 4;
end method;



// OP--DUPLICATE
// Returns a new register containing the old value. For explicit
// live range splitting.

define method op--duplicate
    (be :: <harp-back-end>, value :: <register>) => (reg :: <register>)
  let dest = make-n-register(be);
  ins--move(be, dest, value);
  dest;
end method;

define method op--duplicate
    (be :: <harp-back-end>, value :: <integer>) => (int :: <integer>)
  value;
end method;


// OP--SUB64 
// Performs a full 64bit subtraction

ignore(op--sub64);

define method op--sub64 
    (be :: <harp-back-end>,
     res-lo :: <register>, 
     res-hi :: <register>,
     from-lo, from-hi,
     by-lo, by-hi)
  with-harp (be)
    nreg borrow;

    ins--subcx(be, res-lo, borrow, from-lo, by-lo);
    ins--sub(be, res-hi, from-hi, borrow);
    ins--sub(be, res-hi, res-hi, by-hi);
  end with-harp;
end method;


define method op--add
    (back-end :: <harp-back-end>, result, x, y)
 => (result)
  let result = result | make-n-register(back-end);
  ins--add(back-end, result, x, y);
  result;
end method;


define method op--add
    (back-end :: <harp-back-end>, result, x :: <integer>, y :: <integer>)
 => (sum :: <integer>)
  x + y
end method;

define open generic op--load-index
    (back-end :: <harp-back-end>, result, base, index, offset :: <integer>)
 => ();

define method op--load-index
    (back-end :: <harp-back-end>, result, base, index, offset :: <integer>)
 => ()
  let offset = op--add(back-end, #f, index, offset);

  ins--ld(back-end, result, base, offset);

end method;

define open generic op--store-index
    (back-end :: <harp-back-end>, value, base, index, offset :: <integer>)
 => ();

define method op--store-index
    (back-end :: <harp-back-end>, value, base, index, offset :: <integer>)
 => ()
  let offset = op--add(back-end, #f, index, offset);

  ins--st(back-end, value, base, offset);

end method;

define open generic op--load-index-scaled
    (back-end :: <harp-back-end>, result, base, scaled-index, offset :: <integer>)
 => ();

define method op--load-index-scaled
    (back-end :: <harp-back-end>, result, base, scaled-index, offset :: <integer>)
 => ()
  let offset =
    op--add(back-end, #f, op--multiply-by-4(back-end, scaled-index), offset);

  ins--ld(back-end, result, base, offset);

end method;

define open generic op--store-index-scaled
    (back-end :: <harp-back-end>, value, base, scaled-index, offset :: <integer>)
 => ();

define method op--store-index-scaled
    (back-end :: <harp-back-end>, value, base, scaled-index, offset :: <integer>)
 => ()
  let offset =
    op--add(back-end, #f, op--multiply-by-4(back-end, scaled-index), offset);

  ins--st(back-end, value, base, offset);

end method;


define open generic op--load-byte-index
    (back-end :: <harp-back-end>, result, base, index, offset :: <integer>)
 => ();

define method op--load-byte-index
    (back-end :: <harp-back-end>, result, base, index, offset :: <integer>)
 => ()
  let offset = op--add(back-end, #f, index, offset);

  ins--ldb(back-end, result, base, offset);

end method;

define open generic op--store-byte-index
    (back-end :: <harp-back-end>, value, base, index, offset :: <integer>)
 => ();

define method op--store-byte-index
    (back-end :: <harp-back-end>, value, base, index, offset :: <integer>)
 => ()
  let offset = op--add(back-end, #f, index, offset);

  ins--stb(back-end, value, base, offset);

end method;

