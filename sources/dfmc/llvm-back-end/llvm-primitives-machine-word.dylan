Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Support routines

// Split a value twice the size of a <raw-machine-word> into two
// <raw-machine-word> values.
define function op--split-double-integer
    (be :: <llvm-back-end>, full :: <llvm-value>)
 => (low :: <llvm-value>, high :: <llvm-value>);
  let word-size = back-end-word-size(be);
  let word-bits = word-size * 8;
  let iDoubleWord-type = be.%type-table["iDoubleWord"];
  let raw-machine-word-type
    = llvm-reference-type(be, dylan-value(#"<raw-machine-word>"));

  let double-high
    = ins--lshr(be, full, make(<llvm-integer-constant>,
                               type: iDoubleWord-type,
                               integer: word-bits));
  values(ins--trunc(be, full, raw-machine-word-type),
         ins--trunc(be, double-high, raw-machine-word-type))
end function;

// Combine two <raw-machine-word> values into a single double-width value
define function op--double-integer-merge
    (be :: <llvm-back-end>, low, high)
 => (full :: <llvm-value>);
  let word-size = back-end-word-size(be);
  let word-bits = word-size * 8;
  let iDoubleWord-type = be.%type-table["iDoubleWord"];

  let double-low = ins--zext(be, low, iDoubleWord-type);
  let double-high = ins--zext(be, high, iDoubleWord-type);
  let double-high-shifted
    = ins--shl(be, double-high, make(<llvm-integer-constant>,
                                     type: iDoubleWord-type,
                                     integer: word-bits));
  ins--or(be, double-high-shifted, double-low)
end function;

// Allocate and initialize a <double-integer> heap instance
define function op--allocate-double-integer
    (be :: <llvm-back-end>, low, high)
 => (double-integer :: <llvm-value>);
  let class :: <&class> = dylan-value(#"<double-integer>");
  let double-integer = op--allocate-untraced(be, class);
  let low-slot-ptr
    = op--getslotptr(be, double-integer, class, #"%%double-integer-low");
  ins--store(be, low, low-slot-ptr);
  let high-slot-ptr
    = op--getslotptr(be, double-integer, class, #"%%double-integer-high");
  ins--store(be, high, high-slot-ptr);
  double-integer
end function;


/// Primitives

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-integer?
    (x :: <object>) => (result :: <boolean>)
  let raw-machine-word
    = ins--ptrtoint(be, x,
                    llvm-reference-type
                      (be, dylan-value(#"<raw-machine-word>")));
  let tag-bits = ins--and(be, raw-machine-word, ash(1, $dylan-tag-bits) - 1);
  let cmp = ins--icmp-eq(be, tag-bits, $dylan-tag-integer);
  op--boolean(be, cmp)
end;

define sign-extend side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-equal?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>);
  let cmp = ins--icmp-eq(be, x, y);
  op--boolean(be, cmp)
end;

define sign-extend side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-not-equal?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>);
  let cmp = ins--icmp-ne(be, x, y);
  op--boolean(be, cmp)
end;

define sign-extend side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-less-than?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>);
  let cmp = ins--icmp-slt(be, x, y);
  op--boolean(be, cmp)
end;

define sign-extend side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-not-less-than?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>);
  let cmp = ins--icmp-sge(be, x, y);
  op--boolean(be, cmp)
end;

define sign-extend side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-greater-than?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>);
  let cmp = ins--icmp-sgt(be, x, y);
  op--boolean(be, cmp)
end;

define sign-extend side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-not-greater-than?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>);
  let cmp = ins--icmp-sle(be, x, y);
  op--boolean(be, cmp)
end;

define side-effect-free stateless dynamic-extent mapped &runtime-primitive-descriptor primitive-wrap-machine-word
    (x :: <raw-machine-word>) => (result :: <machine-word>);
  let result = op--allocate-untraced(be, #"<machine-word>");
  let ptr
    = op--getslotptr(be, result, #"<machine-word>", #"%machine-word-data");
  ins--store(be, x, ptr, alignment: back-end-word-size(be));
  result
end;

define side-effect-free stateless dynamic-extent mapped &primitive-descriptor primitive-unwrap-machine-word
    (x :: <machine-word>) => (result :: <raw-machine-word>);
  let ptr = op--getslotptr(be, x, #"<machine-word>", #"%machine-word-data");
  ins--load(be, ptr, alignment: back-end-word-size(be))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-cast-integer-as-raw
    (x :: <integer>) => (result :: <raw-machine-word>);
  ins--ptrtoint(be, x,
                llvm-reference-type(be, dylan-value(#"<raw-machine-word>")))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-cast-raw-as-integer
    (x :: <raw-machine-word>) => (result :: <integer>);
  ins--inttoptr(be, x, $llvm-object-pointer-type)
end;

define side-effect-free stateless dynamic-extent &runtime-primitive-descriptor primitive-wrap-abstract-integer
    (x :: <raw-machine-word>) => (result :: <abstract-integer>);
  let word-size = back-end-word-size(be);
  let word-bits = word-size * 8;
  let maximum-fixed-integer
    = generic/-(generic/ash(1, word-bits - $dylan-tag-bits - 1), 1);
  let minimum-fixed-integer
    = generic/-(-1, maximum-fixed-integer);

  // Basic blocks
  let entry-block   = be.llvm-builder-basic-block;
  let below-check   = make(<llvm-basic-block>);
  let fixed         = make(<llvm-basic-block>);
  let common-alloc  = make(<llvm-basic-block>);
  let common-return = make(<llvm-basic-block>);

  // Check for greater than maximum-fixed-integer
  let cmp-above = ins--icmp-sgt(be, x, maximum-fixed-integer);
  ins--br(be, cmp-above, common-alloc, below-check);

  // Check for less than minimum-fixed-integer
  ins--block(be, below-check);
  let cmp-below = ins--icmp-slt(be, x, minimum-fixed-integer);
  ins--br(be, cmp-below, common-alloc, fixed);

  // Tag as a fixed integer
  ins--block(be, fixed);
  let shifted = ins--shl(be, x, $dylan-tag-bits);
  let tagged = ins--or(be, shifted, $dylan-tag-integer);
  let tagged-ptr = ins--inttoptr(be, tagged, $llvm-object-pointer-type);
  ins--br(be, common-return);

  // Allocate and initialize a <double-integer> instance
  ins--block(be, common-alloc);
  let high = ins--phi(be, 0, entry-block, -1, below-check);
  let double-integer = op--allocate-double-integer(be, x, high);
  let double-integer-ptr
    = ins--bitcast(be, double-integer, $llvm-object-pointer-type);
  ins--br(be, common-return);

  // Common return
  ins--block(be, common-return);
  ins--phi(be, tagged-ptr, fixed, double-integer-ptr, common-alloc);
end;

define side-effect-free stateless dynamic-extent &runtime-primitive-descriptor primitive-wrap-unsigned-abstract-integer
    (x :: <raw-machine-word>) => (result :: <abstract-integer>);
  let word-size = back-end-word-size(be);
  let word-bits = word-size * 8;
  let maximum-fixed-integer
    = generic/-(generic/ash(1, word-bits - $dylan-tag-bits - 1), 1);

  // Basic blocks
  let entry-block   = be.llvm-builder-basic-block;
  let fixed         = make(<llvm-basic-block>);
  let common-alloc  = make(<llvm-basic-block>);
  let common-return = make(<llvm-basic-block>);

  // Check for greater than maximum-fixed-integer
  let cmp-above = ins--icmp-ugt(be, x, maximum-fixed-integer);
  ins--br(be, cmp-above, common-alloc, fixed);

  // Tag as a fixed integer
  ins--block(be, fixed);
  let shifted = ins--shl(be, x, $dylan-tag-bits);
  let tagged = ins--or(be, shifted, $dylan-tag-integer);
  let tagged-ptr = ins--inttoptr(be, tagged, $llvm-object-pointer-type);
  ins--br(be, common-return);

  // Allocate and initialize a <double-integer> instance
  ins--block(be, common-alloc);
  let double-integer = op--allocate-double-integer(be, x, 0);
  let double-integer-ptr
    = ins--bitcast(be, double-integer, $llvm-object-pointer-type);
  ins--br(be, common-return);

  // Common return
  ins--block(be, common-return);
  ins--phi(be, tagged-ptr, fixed, double-integer-ptr, common-alloc);
end;

define side-effect-free stateless dynamic-extent &runtime-primitive-descriptor primitive-unwrap-abstract-integer
    (x :: <abstract-integer>) => (result :: <raw-machine-word>);
  // Basic blocks
  let return-untagged = make(<llvm-basic-block>);
  let return-double   = make(<llvm-basic-block>);
  let return-common   = make(<llvm-basic-block>);

  // Check the tag bits for an object pointer
  let raw-machine-word
    = ins--ptrtoint(be, x,
                    llvm-reference-type
                      (be, dylan-value(#"<raw-machine-word>")));
  let tag-bits = ins--and(be, raw-machine-word, ash(1, $dylan-tag-bits) - 1);
  let cmp = ins--icmp-eq(be, tag-bits, $dylan-tag-pointer);
  ins--br(be, cmp, return-double, return-untagged);

  // Tagged as $dylan-tag-integer: shift right to remove tag bits
  ins--block(be, return-untagged);
  let untagged = ins--ashr(be, raw-machine-word, $dylan-tag-bits);
  ins--br(be, return-common);

  // Tagged as an object pointer: Retrieve %%double-integer-low field
  ins--block(be, return-double);
  let double-integer = op--object-pointer-cast(be, x, #"<double-integer>");
  let ptr = op--getslotptr(be, double-integer, #"<double-integer>",
                           #"%%double-integer-low");
  let low = ins--load(be, ptr, alignment: back-end-word-size(be));
  ins--br(be, return-common);

  // Common return block
  ins--block(be, return-common);
  ins--phi(be, untagged, return-untagged, low, return-double)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-logand
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
  ins--and(be, x, y)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-logior
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
  ins--or(be, x, y)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-logxor
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
  ins--xor(be, x, y)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-lognot
    (x :: <raw-machine-word>) => (result :: <raw-machine-word>);
  ins--xor(be, x, -1)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-logbit?
    (index :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>);
  let mask = ins--shl(be, 1, index);
  let masked-y = ins--and(be, y, mask);
  let cmp = ins--icmp-ne(be, masked-y, 0);
  op--boolean(be, cmp)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-logbit-set
    (index :: <raw-machine-word>, y :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
  let mask = ins--shl(be, 1, index);
  ins--or(be, y, mask)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-logbit-clear
    (index :: <raw-machine-word>, y :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
  let mask = ins--shl(be, 1, index);
  let inv-mask = ins--xor(be, mask, -1);
  ins--and(be, y, inv-mask)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-bit-field-deposit
    (field :: <raw-machine-word>, offset :: <raw-machine-word>,
     size :: <raw-machine-word>, x :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
  let shifted-field = ins--shl(be, field, offset);
  let mask = ins--sub(be, ins--shl(be, 1, size), 1);
  let shifted-mask = ins--shl(be, mask, offset);
  let inv-shifted-mask = ins--xor(be, shifted-mask, -1);
  let masked-x = ins--and(be, x, inv-shifted-mask);
  ins--or(be, shifted-field, masked-x)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-bit-field-extract
    (offset :: <raw-machine-word>, size :: <raw-machine-word>,
     x :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
  let mask = ins--sub(be, ins--shl(be, 1, size), 1);
  let shifted-x = ins--lshr(be, x, offset);
  ins--and(be, shifted-x, mask)
end;

define side-effect-free stateless dynamic-extent &runtime-primitive-descriptor primitive-machine-word-count-low-zeros
    (x :: <raw-machine-word>) => (result :: <raw-machine-word>);
  ins--call-intrinsic(be, "llvm.cttz", vector(x))
end;

define side-effect-free stateless dynamic-extent &runtime-primitive-descriptor primitive-machine-word-count-high-zeros
    (x :: <raw-machine-word>) => (result :: <raw-machine-word>);
  ins--call-intrinsic(be, "llvm.ctlz", vector(x))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-add
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (sum :: <raw-machine-word>);
  ins--add(be, x, y)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-add-with-overflow
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (sum :: <raw-machine-word>, overflow? :: <boolean>);
  let result
    = ins--call-intrinsic(be, "llvm.sadd.with.overflow", vector(x, y));
  let sum = ins--extractvalue(be, result, 0);
  let overflow = ins--extractvalue(be, result, 1);
  values(sum, op--boolean(be, overflow))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-subtract
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (difference :: <raw-machine-word>);
  ins--sub(be, x, y)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-subtract-with-overflow
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (difference :: <raw-machine-word>, overflow? :: <boolean>);
  let result
    = ins--call-intrinsic(be, "llvm.ssub.with.overflow", vector(x, y));
  let sum = ins--extractvalue(be, result, 0);
  let overflow = ins--extractvalue(be, result, 1);
  values(sum, op--boolean(be, overflow))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-multiply-low
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (low :: <raw-machine-word>);
  ins--mul(be, x, y)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-multiply-high
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (high :: <raw-machine-word>)
  let word-size = back-end-word-size(be);
  let iDoubleWord-type = be.%type-table["iDoubleWord"];
  let raw-machine-word-type
    = llvm-reference-type(be, dylan-value(#"<raw-machine-word>"));

  // Extend the operands to double the word width and perform the multiply
  let double-x = ins--sext(be, x, iDoubleWord-type);
  let double-y = ins--sext(be, y, iDoubleWord-type);
  let full = ins--mul(be, double-x, double-y);

  // Extract the high word of the result
  let double-high
    = ins--ashr(be, full, make(<llvm-integer-constant>,
                               type: iDoubleWord-type,
                               integer: word-size * 8));
  ins--trunc(be, double-high, raw-machine-word-type)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-multiply-low/high
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (low :: <raw-machine-word>, high :: <raw-machine-word>);
  let word-size = back-end-word-size(be);
  let iDoubleWord-type = be.%type-table["iDoubleWord"];
  let raw-machine-word-type
    = llvm-reference-type(be, dylan-value(#"<raw-machine-word>"));

  // Extend the operands to double the word width and perform the multiply
  let double-x = ins--sext(be, x, iDoubleWord-type);
  let double-y = ins--sext(be, y, iDoubleWord-type);
  let full = ins--mul(be, double-x, double-y);

  // Extract the high and low words of the result
  op--split-double-integer(be, full)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-multiply-low-with-overflow
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (low :: <raw-machine-word>, overflow? :: <boolean>)
  let result
    = ins--call-intrinsic(be, "llvm.smul.with.overflow", vector(x, y));

  // Extract and return the results
  let product = ins--extractvalue(be, result, 0);
  let overflow = ins--extractvalue(be, result, 1);
  values(product, op--boolean(be, overflow))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-multiply-with-overflow
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (low :: <raw-machine-word>, high :: <raw-machine-word>,
     overflow? :: <boolean>);
  let word-size = back-end-word-size(be);
  let iDoubleWord-type = be.%type-table["iDoubleWord"];
  let raw-machine-word-type
    = llvm-reference-type(be, dylan-value(#"<raw-machine-word>"));

  // Extend the operands to double the word width and perform the multiply
  let double-x = ins--sext(be, x, iDoubleWord-type);
  let double-y = ins--sext(be, y, iDoubleWord-type);
  let result
    = ins--call-intrinsic(be, "llvm.smul.with.overflow",
                          vector(double-x, double-y));

  // Extract the results
  let full = ins--extractvalue(be, result, 0);
  let overflow = ins--extractvalue(be, result, 1);

  // Extract the high and low words of the result
  let (low :: <llvm-value>, high :: <llvm-value>)
    = op--split-double-integer(be, full);
  values(low, high, op--boolean(be, overflow))
end;

define sign-extend side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-negative
    (x :: <raw-machine-word>) => (result :: <raw-machine-word>);
  ins--sub(be, 0, x)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-negative-with-overflow
    (x :: <raw-machine-word>)
 => (result :: <raw-machine-word>, overflow? :: <boolean>);
  let result
    = ins--call-intrinsic(be, "llvm.ssub.with.overflow", vector(0, x));
  let sum = ins--extractvalue(be, result, 0);
  let overflow = ins--extractvalue(be, result, 1);
  values(sum, op--boolean(be, overflow))
end;

define sign-extend side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-abs
    (x :: <raw-machine-word>) => (result :: <raw-machine-word>);
  let neg = ins--icmp-slt(be, x, 0);
  let x-negative = ins--sub(be, 0, x, no-signed-wrap?: #t);
  ins--select(be, neg, x-negative, x)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-abs-with-overflow
    (x :: <raw-machine-word>)
 => (result :: <raw-machine-word>, overflow? :: <boolean>);
  let neg = ins--icmp-slt(be, x, 0);
  let x-negative-with-overflow
    = ins--call-intrinsic(be, "llvm.ssub.with.overflow", vector(0, x));
  let x-negative = ins--extractvalue(be, x-negative-with-overflow, 0);
  let overflow = ins--extractvalue(be, x-negative-with-overflow, 1);
  let result = ins--select(be, neg, x-negative, x);
  values(result, op--boolean(be, overflow))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-floor/-quotient
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
  let (quotient, remainder)
    = call-primitive(be, primitive-machine-word-floor/-descriptor,
                     dividend, divisor);
  quotient
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-floor/-remainder
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
  let (quotient, remainder)
    = call-primitive(be, primitive-machine-word-floor/-descriptor,
                     dividend, divisor);
  remainder
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-floor/
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
  // Basic blocks
  let entry               = be.llvm-builder-basic-block;
  let rem-nonzero         = make(<llvm-basic-block>);
  let divisor-negative    = make(<llvm-basic-block>);
  let divisor-nonnegative = make(<llvm-basic-block>);
  let decrease            = make(<llvm-basic-block>);
  let return              = make(<llvm-basic-block>);

  // Compute quotient and remainder
  let div = ins--sdiv(be, dividend, divisor);
  let rem = ins--srem(be, dividend, divisor);
  let rem-zero? = ins--icmp-eq(be, rem, 0);
  ins--br(be, rem-zero?, return, rem-nonzero);

  // Determine whether or not rounding down is necessary
  ins--block(be, rem-nonzero);
  let divisor-negative? = ins--icmp-slt(be, divisor, 0);
  ins--br(be, divisor-negative?, divisor-negative, divisor-nonnegative);

  ins--block(be, divisor-negative);
  let rem-positive? = ins--icmp-sgt(be, rem, 0);
  ins--br(be, rem-positive?, decrease, return);

  ins--block(be, divisor-nonnegative);
  let rem-negative? = ins--icmp-slt(be, rem, 0);
  ins--br(be, rem-negative?, decrease, return);

  // Round the quotient down
  ins--block(be, decrease);
  let dec = ins--add(be, div, -1, no-signed-wrap?: #t);
  let add = ins--add(be, rem, divisor, no-signed-wrap?: #t);
  ins--br(be, return);

  // Common return
  ins--block(be, return);
  values(ins--phi(be, dec, decrease,
                      div, divisor-negative,
                      div, divisor-nonnegative,
                      div, entry),
         ins--phi(be, add, decrease,
                      rem, divisor-negative,
                      rem, divisor-nonnegative,
                      0,   entry))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-ceiling/-quotient
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>)
  let (quotient, remainder)
    = call-primitive(be, primitive-machine-word-ceiling/-descriptor,
                     dividend, divisor);
  quotient
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-ceiling/-remainder
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
  let (quotient, remainder)
    = call-primitive(be, primitive-machine-word-ceiling/-descriptor,
                     dividend, divisor);
  remainder
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-ceiling/
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
  // Basic blocks
  let entry               = be.llvm-builder-basic-block;
  let rem-nonzero         = make(<llvm-basic-block>);
  let divisor-negative    = make(<llvm-basic-block>);
  let divisor-nonnegative = make(<llvm-basic-block>);
  let increase            = make(<llvm-basic-block>);
  let return              = make(<llvm-basic-block>);

  // Compute quotient and remainder
  let div = ins--sdiv(be, dividend, divisor);
  let rem = ins--srem(be, dividend, divisor);
  let rem-zero? = ins--icmp-eq(be, rem, 0);
  ins--br(be, rem-zero?, return, rem-nonzero);

  // Determine whether or not rounding up is necessary
  ins--block(be, rem-nonzero);
  let divisor-negative? = ins--icmp-slt(be, divisor, 0);
  ins--br(be, divisor-negative?, divisor-negative, divisor-nonnegative);

  ins--block(be, divisor-negative);
  let rem-negative? = ins--icmp-slt(be, rem, 0);
  ins--br(be, rem-negative?, increase, return);

  ins--block(be, divisor-nonnegative);
  let rem-positive? = ins--icmp-sgt(be, rem, 0);
  ins--br(be, rem-positive?, increase, return);

  // Round the quotient up
  ins--block(be, increase);
  let inc = ins--add(be, div, 1, no-signed-wrap?: #t);
  let sub = ins--sub(be, rem, divisor, no-signed-wrap?: #t);
  ins--br(be, return);

  // Common return
  ins--block(be, return);
  values(ins--phi(be, inc, increase,
                      div, divisor-negative,
                      div, divisor-nonnegative,
                      div, entry),
         ins--phi(be, sub, increase,
                      rem, divisor-negative,
                      rem, divisor-nonnegative,
                      0,   entry))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-round/-quotient
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
  let (quotient, remainder)
    = call-primitive(be, primitive-machine-word-round/-descriptor,
                     dividend, divisor);
  quotient
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-round/-remainder
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
  let (quotient, remainder)
    = call-primitive(be, primitive-machine-word-round/-descriptor,
                     dividend, divisor);
  remainder
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-round/
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
  // Basic blocks
  let entry               = be.llvm-builder-basic-block;
  let test-case1-a        = make(<llvm-basic-block>);
  let test-case1-b        = make(<llvm-basic-block>);
  let case1               = make(<llvm-basic-block>);
  let test-case2          = make(<llvm-basic-block>);
  let test-case2-a        = make(<llvm-basic-block>);
  let test-case2-b        = make(<llvm-basic-block>);
  let case2               = make(<llvm-basic-block>);
  let increase            = make(<llvm-basic-block>);
  let decrease            = make(<llvm-basic-block>);
  let return              = make(<llvm-basic-block>);

  // Compute quotient and remainder
  let div = ins--sdiv(be, dividend, divisor);
  let rem = ins--srem(be, dividend, divisor);
  let divisor-negative? = ins--icmp-slt(be, divisor, 0);
  let neg = ins--sub(be, 0, divisor, no-signed-wrap?: #t);
  let abs-divisor = ins--select(be, divisor-negative?, neg, divisor);
  let threshold = ins--sdiv(be, abs-divisor, 2);

  // Test if quotient is even
  let and = ins--and(be, div, 1);
  let quotient-even? = ins--icmp-eq(be, and, 0);

  // Test for case 1: rem > threshold | (rem = threshold & odd)
  let cmp1 = ins--icmp-sgt(be, rem, threshold);
  ins--br(be, cmp1, case1, test-case1-a);

  ins--block(be, test-case1-a);
  let cmp1a = ins--icmp-eq(be, rem, threshold);
  ins--br(be, cmp1a, test-case1-b, test-case2);

  ins--block(be, test-case1-b);
  ins--br(be, quotient-even?, test-case2, case1);

  // Case 1
  ins--block(be, case1);
  ins--br(be, divisor-negative?, decrease, increase);

  // Test for case 2: rem < -threshold | (rem = -threshold & odd)
  ins--block(be, test-case2);
  let neg-threshold = ins--sub(be, 0, threshold);
  let cmp2 = ins--icmp-slt(be, rem, neg-threshold);
  ins--br(be, cmp2, case2, test-case2-a);

  ins--block(be, test-case2-a);
  let cmp2a = ins--icmp-eq(be, rem, neg-threshold);
  ins--br(be, cmp2a, test-case2-b, return);

  ins--block(be, test-case2-b);
  ins--br(be, quotient-even?, return, case2);

  // Case 2
  ins--block(be, case2);
  ins--br(be, divisor-negative?, increase, decrease);

  // Round the quotient up
  ins--block(be, increase);
  let inc = ins--add(be, div, 1, no-signed-wrap?: #t);
  let sub = ins--sub(be, rem, divisor, no-signed-wrap?: #t);
  ins--br(be, return);

  // Round the quotient down
  ins--block(be, decrease);
  let dec = ins--add(be, div, -1, no-signed-wrap?: #t);
  let add = ins--add(be, rem, divisor, no-signed-wrap?: #t);
  ins--br(be, return);

  // Common return
  ins--block(be, return);
  values(ins--phi(be, inc, increase,
                      dec, decrease,
                      div, test-case2-b,
                      div, test-case2-a),
         ins--phi(be, sub, increase,
                      add, decrease,
                      rem, test-case2-b,
                      rem, test-case2-a))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-truncate/-quotient
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
  ins--sdiv(be, dividend, divisor)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-truncate/-remainder
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
  ins--srem(be, dividend, divisor)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-truncate/
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>)
  values(ins--sdiv(be, dividend, divisor),
         ins--srem(be, dividend, divisor))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-divide-quotient
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
  ins--sdiv(be, dividend, divisor)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-divide-remainder
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
  ins--srem(be, dividend, divisor)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-divide
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
  values(ins--sdiv(be, dividend, divisor),
         ins--srem(be, dividend, divisor))
end;

define sign-extend side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-shift-left-low
    (x :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (low :: <raw-machine-word>);
  ins--shl(be, x, shift)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-shift-left-high
    (x :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (high :: <raw-machine-word>);
  let (low, high)
    = call-primitive(be, primitive-machine-word-shift-left-low/high-descriptor,
                     x, shift);
  high
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-shift-left-low/high
    (x :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (low :: <raw-machine-word>, high :: <raw-machine-word>);
  // Extend the operands to double the word width and perform the multiply
  let iDoubleWord-type = be.%type-table["iDoubleWord"];
  let double-x = ins--zext(be, x, iDoubleWord-type);
  let double-shift = ins--sext(be, shift, iDoubleWord-type);
  let full = ins--shl(be, double-x, double-shift);

  // Extract the high and low words of the result
  op--split-double-integer(be, full)
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-shift-left-low-with-overflow
    (x :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (low :: <raw-machine-word>, overflow? :: <boolean>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-shift-left-with-overflow
    (x :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (low :: <raw-machine-word>, high :: <raw-machine-word>, overflow? :: <boolean>);
  //---*** Fill this in...
end;

define sign-extend side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-shift-right
    (x :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
  ins--ashr(be, x, shift)
end;

define overflow side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-add-signal-overflow
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (sum :: <raw-machine-word>);
  let trap = make(<llvm-basic-block>);
  let return = make(<llvm-basic-block>);

  let result
    = ins--call-intrinsic(be, "llvm.sadd.with.overflow", vector(x, y));
  let sum = ins--extractvalue(be, result, 0);
  let overflow = ins--extractvalue(be, result, 1);
  ins--br(be, overflow, trap, return);

  // Signal <arithmetic-overflow-error>
  ins--block(be, trap);
  op--overflow-trap(be);
  ins--unreachable(be);

  // Return the sum
  ins--block(be, return);
  sum
end;

define overflow side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-subtract-signal-overflow
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (difference :: <raw-machine-word>);
  let trap = make(<llvm-basic-block>);
  let return = make(<llvm-basic-block>);

  let result
    = ins--call-intrinsic(be, "llvm.ssub.with.overflow", vector(x, y));
  let difference = ins--extractvalue(be, result, 0);
  let overflow = ins--extractvalue(be, result, 1);
  ins--br(be, overflow, trap, return);

  // Signal <arithmetic-overflow-error>
  ins--block(be, trap);
  op--overflow-trap(be);
  ins--unreachable(be);

  // Return the difference
  ins--block(be, return);
  difference
end;

define overflow side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-multiply-signal-overflow
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (low :: <raw-machine-word>);
  let trap = make(<llvm-basic-block>);
  let return = make(<llvm-basic-block>);

  let result
    = ins--call-intrinsic(be, "llvm.smul.with.overflow", vector(x, y));
  let product = ins--extractvalue(be, result, 0);
  let overflow = ins--extractvalue(be, result, 1);
  ins--br(be, overflow, trap, return);

  // Signal <arithmetic-overflow-error>
  ins--block(be, trap);
  op--overflow-trap(be);
  ins--unreachable(be);

  // Return the difference
  ins--block(be, return);
  product
end;

define sign-extend overflow side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-negative-signal-overflow
    (x :: <raw-machine-word>) => (result :: <raw-machine-word>);
  let trap = make(<llvm-basic-block>);
  let return = make(<llvm-basic-block>);

  let result
    = ins--call-intrinsic(be, "llvm.ssub.with.overflow", vector(0, x));
  let difference = ins--extractvalue(be, result, 0);
  let overflow = ins--extractvalue(be, result, 1);
  ins--br(be, overflow, trap, return);

  // Signal <arithmetic-overflow-error>
  ins--block(be, trap);
  op--overflow-trap(be);
  ins--unreachable(be);

  // Return the difference
  ins--block(be, return);
  difference
end;

define sign-extend overflow side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-abs-signal-overflow
    (x :: <raw-machine-word>) => (result :: <raw-machine-word>);
  // Basic blocks
  let trap = make(<llvm-basic-block>);
  let return = make(<llvm-basic-block>);

  let neg = ins--icmp-slt(be, x, 0);
  let x-negative-with-overflow
    = ins--call-intrinsic(be, "llvm.ssub.with.overflow", vector(0, x));
  let x-negative = ins--extractvalue(be, x-negative-with-overflow, 0);
  let overflow = ins--extractvalue(be, x-negative-with-overflow, 1);
  ins--br(be, overflow, trap, return);

  // Signal <arithmetic-overflow-error>
  ins--block(be, trap);
  op--overflow-trap(be);
  ins--unreachable(be);

  // Return the absolute value
  ins--block(be, return);
  ins--select(be, neg, x-negative, x);
end;

define sign-extend overflow side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-shift-left-signal-overflow
    (x :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-double-floor/-quotient
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-double-floor/-remainder
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-double-floor/
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-double-ceiling/-quotient
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-double-ceiling/-remainder
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-double-ceiling/
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-double-round/-quotient
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-double-round/-remainder
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-double-round/
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-double-truncate/-quotient
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-double-truncate/-remainder
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-double-truncate/
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-double-divide-quotient
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-double-divide-remainder
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-double-divide
    (dividend-low :: <raw-machine-word>, dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-less-than?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>);
  let cmp = ins--icmp-ult(be, x, y);
  op--boolean(be, cmp)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-not-less-than?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>);
  let cmp = ins--icmp-uge(be, x, y);
  op--boolean(be, cmp)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-greater-than?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>);
  let cmp = ins--icmp-ugt(be, x, y);
  op--boolean(be, cmp)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-not-greater-than?
    (x :: <raw-machine-word>, y :: <raw-machine-word>) => (result :: <boolean>);
  let cmp = ins--icmp-ule(be, x, y);
  op--boolean(be, cmp)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-add-with-carry
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (sum :: <raw-machine-word>, carry :: <raw-machine-word>);
  let result
    = ins--call-intrinsic(be, "llvm.uadd.with.overflow", vector(x, y));
  values(ins--extractvalue(be, result, 0),
         ins--extractvalue(be, result, 1))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-subtract-with-borrow
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (difference :: <raw-machine-word>, borrow :: <raw-machine-word>);
  let result
    = ins--call-intrinsic(be, "llvm.usub.with.overflow", vector(x, y));
  values(ins--extractvalue(be, result, 0),
         ins--extractvalue(be, result, 1))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-multiply-high
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (high :: <raw-machine-word>);
  let word-size = back-end-word-size(be);
  let iDoubleWord-type = be.%type-table["iDoubleWord"];

  // Extend the operands to double the word width and perform the multiply
  let double-x = ins--zext(be, x, iDoubleWord-type);
  let double-y = ins--zext(be, y, iDoubleWord-type);
  let full = ins--mul(be, double-x, double-y);

  // Extract the high word of the result
  let double-high
    = ins--lshr(be, full, make(<llvm-integer-constant>,
                               type: iDoubleWord-type,
                               integer: word-size * 8));
  ins--trunc(be, double-high,
             llvm-reference-type(be, dylan-value(#"<raw-machine-word>")))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-multiply
    (x :: <raw-machine-word>, y :: <raw-machine-word>)
 => (low :: <raw-machine-word>, high :: <raw-machine-word>);
  let word-size = back-end-word-size(be);
  let iDoubleWord-type = be.%type-table["iDoubleWord"];

  // Extend the operands to double the word width and perform the multiply
  let double-x = ins--zext(be, x, iDoubleWord-type);
  let double-y = ins--zext(be, y, iDoubleWord-type);
  let full = ins--mul(be, double-x, double-y);

  // Extract the high and low words of the result
  op--split-double-integer(be, full)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-divide-quotient
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
  ins--udiv(be, dividend, divisor)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-divide-remainder
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
  ins--urem(be, dividend, divisor)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-divide
    (dividend :: <raw-machine-word>, divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
  values(ins--udiv(be, dividend, divisor),
         ins--urem(be, dividend, divisor))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-rotate-left
    (x :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
  let shl = ins--shl(be, x, shift);
  let sub = ins--sub(be, back-end-word-size(be) * 8, shift);
  let shr = ins--lshr(be, x, sub);
  ins--or(be, shr, shl)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-rotate-right
    (x :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
  let shr = ins--lshr(be, x, shift);
  let sub = ins--sub(be, back-end-word-size(be) * 8, shift);
  let shl = ins--shl(be, x, sub);
  ins--or(be, shl, shr)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-shift-right
    (x :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
  ins--lshr(be, x, shift)
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-unsigned-double-divide-quotient
    (dividend-low :: <raw-machine-word>,
     dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-unsigned-double-divide-remainder
    (dividend-low :: <raw-machine-word>,
     dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (remainder :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-unsigned-double-divide
    (dividend-low :: <raw-machine-word>,
     dividend-high :: <raw-machine-word>,
     divisor :: <raw-machine-word>)
 => (quotient :: <raw-machine-word>, remainder :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-unsigned-shift-left-high
    (x :: <raw-machine-word>, shift :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-double-shift-left-high
    (x-low :: <raw-machine-word>,
     x-high :: <raw-machine-word>,
     shift :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
  let (low, high)
    = call-primitive(be, primitive-machine-word-unsigned-double-shift-left-descriptor,
                     x-low, x-high, shift);
  high
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-double-shift-left
    (x-low :: <raw-machine-word>,
     x-high :: <raw-machine-word>,
     shift :: <raw-machine-word>)
 => (low :: <raw-machine-word>, high :: <raw-machine-word>);
  let iDoubleWord-type = be.%type-table["iDoubleWord"];
  let full = op--double-integer-merge(be, x-low, x-high);
  let shift-ext = ins--sext(be, shift, iDoubleWord-type);
  let full-shifted = ins--shl(be, full, shift-ext);

  // Extract the high and low words of the result
  op--split-double-integer(be, full-shifted)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-double-shift-right-low
    (x-low :: <raw-machine-word>,
     x-high :: <raw-machine-word>,
     shift :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
  let iDoubleWord-type = be.%type-table["iDoubleWord"];
  let full = op--double-integer-merge(be, x-low, x-high);
  let shift-ext = ins--sext(be, shift, iDoubleWord-type);
  let full-shifted = ins--lshr(be, full, shift-ext);
  ins--trunc(be, full-shifted,
             llvm-reference-type(be, dylan-value(#"<raw-machine-word>")))
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-machine-word-unsigned-double-shift-right-high
    (x-low :: <raw-machine-word>,
     x-high :: <raw-machine-word>,
     shift :: <raw-machine-word>)
 => (result :: <raw-machine-word>);
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-machine-word-unsigned-double-shift-right
    (x-low :: <raw-machine-word>,
     x-high :: <raw-machine-word>,
     shift :: <raw-machine-word>)
 => (low :: <raw-machine-word>, high :: <raw-machine-word>);
  let iDoubleWord-type = be.%type-table["iDoubleWord"];
  let full = op--double-integer-merge(be, x-low, x-high);
  let shift-ext = ins--sext(be, shift, iDoubleWord-type);
  let full-shifted = ins--lshr(be, full, shift-ext);

  // Extract the high and low words of the result
  op--split-double-integer(be, full)
end;
