Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Single Float

define constant $libm-attribute-list
  = make(<llvm-attribute-list>,
	 function-attributes:
	   llvm-attribute-merge($llvm-attribute-nounwind,
				$llvm-attribute-readnone));

define method op--call-libm
    (be :: <llvm-back-end>, name :: <string>, args :: <sequence>,
     #rest options)
  => (result :: <llvm-value>)
  unless (llvm-builder-global-defined?(be, name))
    let float-type
      = if (name.last == 'f') $llvm-float-type else $llvm-double-type end if;
    let function-type
      = make(<llvm-function-type>,
	     parameter-types:
	       make(<vector>, size: args.size, fill: float-type),
	     return-type: float-type,
	     varargs?: #f);
    let global
      = make(<llvm-function>,
	     name: name,
	     linkage: #"external",
	     type: llvm-pointer-to(be, function-type),
	     attribute-list: $libm-attribute-list);
    llvm-builder-define-global(be, name, global);
  end unless;
  apply(ins--call, be, llvm-builder-global(be, name), args,
	attribute-list: $libm-attribute-list,
	options)
end method;

define side-effect-free stateless dynamic-extent mapped &primitive-descriptor primitive-single-float-as-raw
    (x :: <single-float>) => (r :: <raw-single-float>);
  let ptr
    = op--getslotptr(be, x, #"<single-float>", #"%single-float-data");
  ins--load(be, ptr, alignment: back-end-word-size(be));
end;

define side-effect-free stateless dynamic-extent mapped &runtime-primitive-descriptor primitive-raw-as-single-float
    (r :: <raw-single-float>) => (x :: <single-float>);
  let result  = op--allocate-untraced(be, #"<single-float>");
  let ptr
    = op--getslotptr(be, result, #"<single-float>", #"%single-float-data");
  ins--store(be, r, ptr, alignment: back-end-word-size(be));
  result
end;

// NOTE: The Dylan library expects this primitive to round towards
// zero (i.e., truncate)
define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-as-integer
    (f :: <raw-single-float>) => (i :: <raw-integer>);
  let type = llvm-reference-type(be, dylan-value(#"<raw-integer>"));
  ins--fptosi(be, f, type)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-integer-as-single-float
    (x :: <raw-integer>) => (z :: <raw-single-float>);
  let type = llvm-reference-type(be, dylan-value(#"<raw-single-float>"));
  ins--sitofp(be, x, type)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-as-double-integer
    (f :: <raw-single-float>) => (low :: <raw-machine-word>, high :: <raw-machine-word>);
  let full = ins--fptosi(be, f, be.%type-table["iDoubleWord"]);
  op--split-double-integer(be, full)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-integer-as-single-float
    (low :: <raw-machine-word>, high :: <raw-machine-word>) => (f :: <raw-single-float>);
  let full = op--double-integer-merge(be, low, high);
  let type = llvm-reference-type(be, dylan-value(#"<raw-single-float>"));
  ins--sitofp(be, full, type)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-cast-single-float-as-machine-word
    (f :: <raw-single-float>) => (b :: <raw-machine-word>);
  let f-bits = ins--bitcast(be, f, $llvm-i32-type);
  if (back-end-word-size(be) = 4)
    f-bits
  else
    let type = llvm-reference-type(be, dylan-value(#"<raw-machine-word>"));
    ins--zext(be, f-bits, type)
  end if
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-cast-machine-word-as-single-float
    (b :: <raw-machine-word>) => (f :: <raw-single-float>);
  let f-bits
    = if (back-end-word-size(be) = 4)
        b
      else
        ins--trunc(be, b, $llvm-i32-type)
      end if;
  ins--bitcast(be, f-bits, $llvm-float-type)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-negate
    (x :: <raw-single-float>) => (negated :: <raw-single-float>);
  ins--fsub(be, -0.0s0, x)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-add
    (x :: <raw-single-float>, y :: <raw-single-float>) => (sum :: <raw-single-float>);
  ins--fadd(be, x, y)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-subtract
    (x :: <raw-single-float>, y :: <raw-single-float>) => (difference :: <raw-single-float>);
  ins--fsub(be, x, y);
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-multiply
    (x :: <raw-single-float>, y :: <raw-single-float>) => (product :: <raw-single-float>);
  ins--fmul(be, x, y)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-divide
    (x :: <raw-single-float>, y :: <raw-single-float>) => (ratio :: <raw-single-float>);
  ins--fdiv(be, x, y)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-equals?
    (x :: <raw-single-float>, y :: <raw-single-float>) => (equal? :: <boolean>);
  let cmp = ins--fcmp-oeq(be, x, y);
  op--boolean(be, cmp)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-less-than?
    (x :: <raw-single-float>, y :: <raw-single-float>) => (less? :: <boolean>);
  let cmp = ins--fcmp-olt(be, x, y);
  op--boolean(be, cmp)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-sqrt
    (x :: <raw-single-float>) => (z :: <raw-single-float>);
  ins--call-intrinsic(be, "llvm.sqrt", vector(x))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-log
    (x :: <raw-single-float>) => (z :: <raw-single-float>)
  ins--call-intrinsic(be, "llvm.log", vector(x))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-exp
    (x :: <raw-single-float>) => (z :: <raw-single-float>);
  ins--call-intrinsic(be, "llvm.exp", vector(x))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-expt
    (base :: <raw-single-float>, power :: <raw-single-float>) => (z :: <raw-single-float>);
  ins--call-intrinsic(be, "llvm.pow", vector(base, power))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-sin
    (x :: <raw-single-float>) => (z :: <raw-single-float>);
  ins--call-intrinsic(be, "llvm.sin", vector(x))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-cos
    (x :: <raw-single-float>) => (z :: <raw-single-float>);
  ins--call-intrinsic(be, "llvm.cos", vector(x))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-tan
    (x :: <raw-single-float>) => (z :: <raw-single-float>);
  op--call-libm(be, "tanf", vector(x))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-asin
    (x :: <raw-single-float>) => (z :: <raw-single-float>);
  op--call-libm(be, "asinf", vector(x))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-acos
    (x :: <raw-single-float>) => (z :: <raw-single-float>);
  op--call-libm(be, "acosf", vector(x))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-atan
    (x :: <raw-single-float>) => (z :: <raw-single-float>);
  op--call-libm(be, "atanf", vector(x))
end;


/// Double Float

define side-effect-free stateless dynamic-extent mapped &primitive-descriptor primitive-double-float-as-raw
    (x :: <double-float>) => (r :: <raw-double-float>);
  let ptr
    = op--getslotptr(be, x, #"<double-float>", #"%double-float-data");
  ins--load(be, ptr, alignment: back-end-word-size(be));
end;

define side-effect-free stateless dynamic-extent mapped &runtime-primitive-descriptor primitive-raw-as-double-float
    (r :: <raw-double-float>) => (x :: <double-float>);
  let result = op--allocate-untraced(be, #"<double-float>");
  let ptr
    = op--getslotptr(be, result, #"<double-float>", #"%double-float-data");
  ins--store(be, r, ptr, alignment: back-end-word-size(be));
  result
end;

// NOTE: The Dylan library expects this primitive to round towards
// zero (i.e., truncate)
define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-as-integer
    (f :: <raw-double-float>) => (i :: <raw-integer>);
  let type = llvm-reference-type(be, dylan-value(#"<raw-integer>"));
  ins--fptosi(be, f, type)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-integer-as-double-float
    (x :: <raw-integer>) => (z :: <raw-double-float>);
  let type = llvm-reference-type(be, dylan-value(#"<raw-double-float>"));
  ins--sitofp(be, x, type)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-as-double-integer
    (f :: <raw-double-float>)
 => (low :: <raw-machine-word>, high :: <raw-machine-word>);
  let full = ins--fptosi(be, f, be.%type-table["iDoubleWord"]);
  op--split-double-integer(be, full)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-integer-as-double-float
    (low :: <raw-machine-word>, high :: <raw-machine-word>)
 => (f :: <raw-double-float>);
  let full = op--double-integer-merge(be, low, high);
  let type = llvm-reference-type(be, dylan-value(#"<raw-double-float>"));
  ins--sitofp(be, full, type)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-cast-double-float-as-machine-words
    (f :: <raw-double-float>)
 => (low :: <raw-machine-word>, high :: <raw-machine-word>);
  let word-size = back-end-word-size(be);
  let type = llvm-reference-type(be, dylan-value(#"<raw-machine-word>"));

  let f-bits = ins--bitcast(be, f, $llvm-i64-type);
  if (word-size = 4)
    let low = ins--trunc(be, f-bits, type);
    let high-64 = ins--lshr(be, f-bits, i64(32));
    let high = ins--trunc(be, high-64, type);
    values(low, high)
  elseif (word-size = 8)
    values(f-bits, i64(0))
  else
    error("Unsupported word-size "
            "for primitive-cast-double-float-as-machine-words");
  end if
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-cast-machine-words-as-double-float
    (low :: <raw-machine-word>, high :: <raw-machine-word>)
 => (f :: <raw-double-float>);
  let word-size = back-end-word-size(be);
  let f-bits
    = if (word-size = 4)
        let low-64 = ins--zext(be, low, $llvm-i64-type);
        let high-64 = ins--zext(be, high, $llvm-i64-type);
        let high-64-shifted = ins--shl(be, high-64, i64(32));
        ins--or(be, high-64-shifted, low-64)
      elseif (word-size = 8)
        low
      end if;
  ins--bitcast(be, f-bits, $llvm-double-type)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-negate
    (x :: <raw-double-float>) => (negated :: <raw-double-float>);
  ins--fsub(be, -0.0d0, x)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-add
    (x :: <raw-double-float>, y :: <raw-double-float>)
 => (sum :: <raw-double-float>);
  ins--fadd(be, x, y)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-subtract
    (x :: <raw-double-float>, y :: <raw-double-float>)
 => (difference :: <raw-double-float>);
  ins--fsub(be, x, y);
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-multiply
    (x :: <raw-double-float>, y :: <raw-double-float>)
 => (product :: <raw-double-float>);
  ins--fmul(be, x, y)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-divide
    (x :: <raw-double-float>, y :: <raw-double-float>)
 => (ratio :: <raw-double-float>);
  ins--fdiv(be, x, y)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-equals?
    (x :: <raw-double-float>, y :: <raw-double-float>) => (equal? :: <boolean>);
  let cmp = ins--fcmp-oeq(be, x, y);
  op--boolean(be, cmp)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-less-than?
    (x :: <raw-double-float>, y :: <raw-double-float>) => (less? :: <boolean>);
  let cmp = ins--fcmp-olt(be, x, y);
  op--boolean(be, cmp)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-sqrt
    (x :: <raw-double-float>) => (z :: <raw-double-float>);
  ins--call-intrinsic(be, "llvm.sqrt", vector(x))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-log
    (x :: <raw-double-float>) => (z :: <raw-double-float>);
  ins--call-intrinsic(be, "llvm.log", vector(x))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-exp
    (x :: <raw-double-float>) => (z :: <raw-double-float>);
  ins--call-intrinsic(be, "llvm.exp", vector(x))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-expt
    (base :: <raw-double-float>, power :: <raw-double-float>)
 => (z :: <raw-double-float>);
  ins--call-intrinsic(be, "llvm.pow", vector(base, power))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-sin
    (x :: <raw-double-float>) => (z :: <raw-double-float>);
  ins--call-intrinsic(be, "llvm.sin", vector(x))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-cos
    (x :: <raw-double-float>) => (z :: <raw-double-float>);
  ins--call-intrinsic(be, "llvm.cos", vector(x))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-tan
    (x :: <raw-double-float>) => (z :: <raw-double-float>);
  op--call-libm(be, "tan", vector(x))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-asin
    (x :: <raw-double-float>) => (z :: <raw-double-float>);
  op--call-libm(be, "asin", vector(x))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-acos
    (x :: <raw-double-float>) => (z :: <raw-double-float>);
  op--call-libm(be, "acos", vector(x))
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-atan
    (x :: <raw-double-float>) => (z :: <raw-double-float>);
  op--call-libm(be, "atan", vector(x))
end;


/// Inter-float Conversions

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-as-double
    (s :: <raw-single-float>) => (d :: <raw-double-float>);
  let type = llvm-reference-type(be, dylan-value(#"<raw-double-float>"));
  ins--fpext(be, s, type)
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-as-single
    (d :: <raw-double-float>) => (s :: <raw-single-float>);
  let type = llvm-reference-type(be, dylan-value(#"<raw-single-float>"));
  ins--fptrunc(be, d, type)
end;


/// Float Classification (e.g. nan, infinity, zero, normal)

/*
define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-single-float-class
    (x :: <raw-single-float>) => (class :: <string>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &primitive-descriptor primitive-double-float-class
    (x :: <raw-double-float>) => (class :: <string>);
  //---*** Fill this in...
end;
*/
