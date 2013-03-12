Module: dfmc-java-back-end
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sealed generic gen-primitive (prim-name :: <symbol>, node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => () ;



define function gen-set-raw-bool (jbb, opcode, reverse?)
//format-out ("### in gen-set-raw-bool\n") ;
  java-branch-op (jbb, opcode, 7) ;
  emit-raw-expression-leaf (jbb, if (reverse?) 1 else 0 end) ;
  java-branch-op (jbb, j-goto, 4) ;
  if (*check-stack-types*)
    maintain-stack-types (jbb, $dummy-java-frag$, $one-int$, #())
  else
    maintain-stack-depth (jbb, -1, j-goto)  // fake because hidden control flow
  end;
  emit-raw-expression-leaf (jbb, if (reverse?) 0 else 1 end) ;
  1
end;


define function gen-set-boolean (jbb, opcode, reverse?)
//format-out ("### in gen-set-boolean\n") ;
  java-branch-op (jbb, opcode, 9) ;
  emit-expression-leaf (jbb, reverse?) ;
  java-branch-op (jbb, j-goto, 6) ;
  if (*check-stack-types*)
    maintain-stack-types (jbb, $dummy-java-frag$, $one-object$, #())
  else
    maintain-stack-depth (jbb, -1, j-goto)
  end;
  emit-expression-leaf (jbb, ~reverse?) ;
  1
end;



define method gen-primitive
    (prim-name == #"primitive-id?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
//format-out ("### in gen-primitive primitive-id?\n") ;
  gen-set-boolean (jbb, j-if-acmpeq, #f) ;
end;


define method gen-primitive
    (prim-name == #"primitive-not-id?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
//format-out ("### in gen-primitive primitive-not-id?\n") ;
  gen-set-boolean (jbb, j-if-acmpeq, #t) ;
end;

define method gen-primitive
    (prim-name == #"primitive-not",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // push dylan.<object>.False, compare, return 1 if eq
  emit-expression-leaf (jbb, #f) ;
  gen-set-boolean (jbb, j-if-acmpeq, #f) ;
end;

define method gen-primitive
    (prim-name == #"primitive-true?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  gen-set-boolean (jbb, j-ifeq, #t)
end;

define method gen-primitive
    (prim-name == #"primitive-false?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  gen-set-boolean (jbb, j-ifeq, #f)
end;

define method gen-primitive
    (prim-name == #"primitive-boolean-as-raw",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-expression-leaf (jbb, #f) ;
  gen-set-raw-bool (jbb, j-if-acmpeq, #t) ;
end;

define method gen-primitive
    (prim-name == #"primitive-raw-as-boolean",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  gen-set-boolean (jbb, j-ifeq, #t)
end;

define method gen-primitive
    (prim-name == #"primitive-as-boolean",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-expression-leaf (jbb, #f) ;
  gen-set-boolean (jbb, j-if-acmpeq, #t) 
end;

define method gen-primitive
    (prim-name == #"primitive-single-float-equals?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-fcmpl) ;
  gen-set-boolean (jbb, j-ifeq, #f)
end;

define method gen-primitive
    (prim-name == #"primitive-double-float-equals?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-dcmpl) ;
  gen-set-boolean (jbb, j-ifeq, #f)
end;

define method gen-primitive
    (prim-name == #"primitive-single-float-less-than?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-fcmpl) ;
  gen-set-boolean (jbb, j-iflt, #f)
end;

define method gen-primitive
    (prim-name == #"primitive-double-float-less-than?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-dcmpl) ;
  gen-set-boolean (jbb, j-iflt, #f)
end;

define constant  $small-integer-value-slot$ =
  slot-spec ($dylan-class-<integer>$, "val", $java-int-type$, #f) ;

define constant  $single-float-value-slot$ =
  slot-spec ($dylan-class-<single-float>$, "val", $java-float-type$, #f) ;

define constant  $double-float-value-slot$ =
  slot-spec ($dylan-class-<double-float>$, "val", $java-double-type$, #f) ;


define constant  $small-integer-small-int-meth$ =
  meth-spec ($dylan-class-<integer>$, "small_int", 
             meth-type ($dylan-class-<integer>$, $java-int-type$),
             j-invokestatic) ;

define constant  $single-float-sing-float-meth$ =
  meth-spec ($dylan-class-<single-float>$, "single_float",
             meth-type ($dylan-class-<single-float>$, $java-float-type$),
             j-invokestatic) ;

define constant  $double-float-doub-float-meth$ =
  meth-spec ($dylan-class-<double-float>$, "double_float",
             meth-type ($dylan-class-<double-float>$, $java-double-type$),
             j-invokestatic) ;


define function gen-raw-to-small-integer (jbb :: <java-basic-block>) => ()
  java-call (jbb, $small-integer-small-int-meth$)
end;
define function gen-small-integer-to-raw (jbb :: <java-basic-block>) => ()
  java-read (jbb, $small-integer-value-slot$)
end;

define method gen-primitive
    (prim-name == #"primitive-cast-integer-as-raw",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  gen-small-integer-to-raw (jbb)
end;

define method gen-primitive
    (prim-name == #"primitive-cast-raw-as-integer",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  gen-raw-to-small-integer (jbb)
end;

// HACK extra-linguistic primitive
define method gen-primitive
    (prim-name == #"primitive-cast-raw-as-pointer",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ;
  emit-expression-leaf (jbb, #f)
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-abs",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-dup (jbb) ;
  java-branch-op (jbb, j-ifge, 4) ;
  java-simple-op (jbb, j-ineg) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-abs-with-overflow",  // hacky, no overflow check
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-dup (jbb) ;
  java-branch-op (jbb, j-ifge, 4) ;
  java-simple-op (jbb, j-ineg) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-abs-signal-overflow",  // hacky, no overflow check
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-dup (jbb) ;
  java-branch-op (jbb, j-ifge, 4) ;
  java-simple-op (jbb, j-ineg) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-negative",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-ineg) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-negative-with-overflow",  // hacky, no overflow check
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-ineg) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-negative-signal-overflow",  // hacky, no overflow check
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-ineg) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-logand",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-iand) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-logior",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-ior) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-logxor",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-ixor) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-lognot",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-expression-leaf (jbb, -1) ;
  java-simple-op (jbb, j-ixor) ;
end;



define method gen-primitive
    (prim-name == #"primitive-machine-word-add",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-iadd) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-add-with-overflow",  // hacky, no overflow check
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-iadd) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-add-signal-overflow",  // hacky, no overflow check
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-iadd) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-subtract",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-isub) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-subtract-with-overflow",  // hacky, no overflow check
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-isub) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-subtract-signal-overflow",  // hacky, no overflow check
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-isub) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-multiply-high",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-i2l) ;
  java-simple-op (jbb, j-dup2-x1) ;
  java-simple-op (jbb, j-pop2) ;
  java-simple-op (jbb, j-i2l) ;
  java-simple-op (jbb, j-lmul) ;
  emit-java-int (jbb, 32) ;
  java-simple-op (jbb, j-lshr) ;
  java-simple-op (jbb, j-l2i)
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-multiply-low",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-imul) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-multiply-with-overflow",  // hacky, no overflow check
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-imul) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-multiply-signal-overflow",  // hacky, no overflow check
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-imul) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-divide",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-idiv) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-truncate/",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-idiv) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-floor/",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
format-out ("## used java division for floor/!!\n") ;
  java-simple-op (jbb, j-idiv) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-logbit?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // (i, val) => bool
  java-simple-op (jbb, j-swap) ;     // swap
  java-simple-op (jbb, j-iushr) ;    // x >>> i
  java-simple-op (jbb, j-iconst-1) ; // (x >>>i) & 1
  java-simple-op (jbb, j-iand) ;
  gen-set-boolean (jbb, j-ifeq, #t)
end;

define constant $wrap-mw-method$ =
  meth-spec ($dylan-class-<integer>$, "wrap_mw", 
             meth-type ($dylan-class-<integer>$, $java-int-type$),
             j-invokestatic) ;


define method gen-primitive
    (prim-name == #"primitive-wrap-machine-word",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-call (jbb, $wrap-mw-method$)
end;


define method gen-primitive
    (prim-name == #"primitive-unwrap-machine-word",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  gen-small-integer-to-raw (jbb)
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-logbit-set",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // (offset, old-val) => new-val
  java-simple-op (jbb, j-swap) ;
  emit-java-int (jbb, 1) ;
  java-simple-op (jbb, j-swap) ;
  java-simple-op (jbb, j-ishl) ;
  java-simple-op (jbb, j-ior)
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-logbit-clear",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // (offset, old-val) => new-val
  java-simple-op (jbb, j-swap) ;
  emit-java-int (jbb, 1) ;
  java-simple-op (jbb, j-swap) ;
  java-simple-op (jbb, j-ishl) ;
  emit-java-int (jbb, -1) ;
  java-simple-op (jbb, j-ixor) ;
  java-simple-op (jbb, j-iand)
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-bit-field-extract",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // (offset, size, value)
  let  valtemp = get-temp-local-var () ;
  let  sizetemp = get-temp-local-var () ;
  emit-pop-local (jbb, valtemp, j-int-code) ;
  emit-pop-local (jbb, sizetemp, j-int-code) ;
  emit-push-local (jbb, valtemp, j-int-code) ;
  java-simple-op (jbb, j-swap) ;
  // shift value right by offset
  java-simple-op (jbb, j-iushr) ;
  // generate mask of right size
  emit-java-int (jbb, -1) ;
  emit-dup (jbb) ;
  emit-push-local (jbb, sizetemp, j-int-code) ;
  java-simple-op (jbb, j-ishl) ;
  java-simple-op (jbb, j-ixor) ;
  // and the mask
  java-simple-op (jbb, j-iand)
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-bit-field-deposit",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // (field, offset, size, value) => new-value
  let valtemp = get-temp-local-var () ;
  let sizetemp = get-temp-local-var () ;
  let offsettemp = get-temp-local-var () ;
  emit-pop-local (jbb, valtemp, j-int-code) ;
  emit-pop-local (jbb, sizetemp, j-int-code) ;
  emit-dup (jbb) ;
  emit-pop-local (jbb, offsettemp, j-int-code) ;
  java-simple-op (jbb, j-ishl) ;   // leave f<<o on stack
  emit-java-int (jbb, -1) ;
  emit-dup (jbb) ;
  emit-push-local (jbb, offsettemp, j-int-code) ;
  java-simple-op (jbb, j-ishl) ;
  emit-dup (jbb) ;
  emit-push-local (jbb, sizetemp, j-int-code) ;
  java-simple-op (jbb, j-ishl) ;
  java-simple-op (jbb, j-ixor) ;  // make the mask  0001111000
  java-simple-op (jbb, j-ixor) ;  // make the mask  1110000111
  emit-push-local (jbb, valtemp, j-int-code) ;
  java-simple-op (jbb, j-iand) ;
  java-simple-op (jbb, j-ior)
end;



define method gen-primitive
    (prim-name == #"primitive-machine-word-shift-left",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-ishl) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-unsigned-rotate-left",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-dup2) ;  // word, shift, word, shift
  emit-java-int (jbb, 32) ;
  emit-swap (jbb) ;
  java-simple-op (jbb, j-isub) ;
  java-simple-op (jbb, j-iushr) ;
  java-simple-op (jbb, j-dup-x2) ;
  emit-pop (jbb) ;
  java-simple-op (jbb, j-ishl) ;
  java-simple-op (jbb, j-ior) 
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-shift-left-signal-overflow", // hacky
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-ishl) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-shift-right",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-ishr) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-equal?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  gen-set-boolean (jbb, j-if-icmpeq, #f) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-not-equal?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  gen-set-boolean (jbb, j-if-icmpne, #f) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-less-than?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  gen-set-boolean (jbb, j-if-icmplt, #f) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-not-less-than?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  gen-set-boolean (jbb, j-if-icmpge, #f) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-greater-than?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  gen-set-boolean (jbb, j-if-icmpgt, #f) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-not-greater-than?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  gen-set-boolean (jbb, j-if-icmple, #f) ;
end;

// the unsigned comparisons here, bit yukky in Java, do the simple
// thing of flipping sign bits

define function flip-int-sign-bit (jbb :: <java-basic-block>) => ()
  emit-java-int (jbb, 1) ;
  emit-java-int (jbb, 31) ;
  java-simple-op (jbb, j-ishl) ;
  java-simple-op (jbb, j-ixor) ;
end;

define function flip-int-sign-bits (jbb :: <java-basic-block>) => ()
  flip-int-sign-bit (jbb) ;
  java-simple-op (jbb, j-swap) ;
  flip-int-sign-bit (jbb) ;
  java-simple-op (jbb, j-swap) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-unsigned-less-than?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  flip-int-sign-bits (jbb) ;
  gen-set-boolean (jbb, j-if-icmplt, #f) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-unsigned-not-less-than?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  flip-int-sign-bits (jbb) ;
  gen-set-boolean (jbb, j-if-icmpge, #f) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-unsigned-greater-than?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  flip-int-sign-bits (jbb) ;
  gen-set-boolean (jbb, j-if-icmpgt, #f) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-unsigned-not-greater-than?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  flip-int-sign-bits (jbb) ;
  gen-set-boolean (jbb, j-if-icmple, #f) ;
end;

//
  
define constant  $dylan-object-class-meth$ =
  meth-spec ($dylan-class-<object>$, "object_class", 
             meth-type ($dylan-class-<class>$, $dylan-class-<object>$),
             j-invokevirtual) ;

define method gen-primitive
    (prim-name == #"primitive-object-class",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-call (jbb, $dylan-object-class-meth$) ;
end;

define constant $integer-low-zeros-meth$ =
  meth-spec ($dylan-class-<integer>$,
	     "count_low_zeros",
	     meth-type ($java-int-type$, $java-int-type$),
	     j-invokestatic) ;

define constant $integer-high-zeros-meth$ =
  meth-spec ($dylan-class-<integer>$,
	     "count_high_zeros",
	     meth-type ($java-int-type$, $java-int-type$),
	     j-invokestatic) ;

define method gen-primitive
    (prim-name == #"primitive-machine-word-count-low-zeros",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-call (jbb, $integer-low-zeros-meth$) 
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-count-high-zeros",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-call (jbb, $integer-high-zeros-meth$) 
end;


// single/double floats

define function gen-raw-to-single-float (jbb)
  java-call (jbb, $single-float-sing-float-meth$) ;
end;
define function gen-single-float-to-raw (jbb)
  java-read (jbb, $single-float-value-slot$) ;
end;
define function gen-raw-to-double-float (jbb)
  java-call (jbb, $double-float-doub-float-meth$) ;
end;
define function gen-double-float-to-raw (jbb)
  java-read (jbb, $double-float-value-slot$) ;
end;



define method gen-primitive
    (prim-name == #"primitive-single-float-as-raw",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  gen-single-float-to-raw (jbb)
end;
define method gen-primitive
    (prim-name == #"primitive-raw-as-single-float",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  gen-raw-to-single-float (jbb)
end;

// <double-float> object to raw double float
define method gen-primitive
    (prim-name == #"primitive-double-float-as-raw",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  gen-double-float-to-raw (jbb)
end;

// raw double to <double-float> object
define method gen-primitive
    (prim-name == #"primitive-raw-as-double-float",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  gen-raw-to-double-float (jbb)
end;

define method gen-primitive
    (prim-name == #"primitive-single-float-as-small-integer",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-f2i) ;
end;

define method gen-primitive
    (prim-name == #"primitive-single-float-negate",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-fneg) ;
end;

define method gen-primitive
    (prim-name == #"primitive-single-float-add",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-fadd) ;
end;

define method gen-primitive
    (prim-name == #"primitive-single-float-subtract",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-fsub) ;
end;

define method gen-primitive
    (prim-name == #"primitive-single-float-multiply",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-fmul) ;
end;

define method gen-primitive
    (prim-name == #"primitive-single-float-divide",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-fdiv) ;
end;

define constant $math-function-double-type$ = 
  meth-type ($java-double-type$, $java-double-type$) ;


define constant $math-method-sqrt$ =
  meth-spec ($java/lang/Math$, "sqrt",
             $math-function-double-type$, j-invokestatic) ;
define constant $math-method-log$ =
  meth-spec ($java/lang/Math$, "log",
             $math-function-double-type$, j-invokestatic) ;
define constant $math-method-exp$ =
  meth-spec ($java/lang/Math$, "exp",
             $math-function-double-type$, j-invokestatic) ;
define constant $math-method-sin$ =
  meth-spec ($java/lang/Math$, "sin",
             $math-function-double-type$, j-invokestatic) ;
define constant $math-method-cos$ =
  meth-spec ($java/lang/Math$, "cos",
             $math-function-double-type$, j-invokestatic) ;
define constant $math-method-tan$ =
  meth-spec ($java/lang/Math$, "tan",
             $math-function-double-type$, j-invokestatic) ;
define constant $math-method-asin$ =
  meth-spec ($java/lang/Math$, "asin",
             $math-function-double-type$, j-invokestatic) ;
define constant $math-method-acos$ =
  meth-spec ($java/lang/Math$, "acos",
             $math-function-double-type$, j-invokestatic) ;
define constant $math-method-atan$ =
  meth-spec ($java/lang/Math$, "atan",
             $math-function-double-type$, j-invokestatic) ;

define constant $math-method-expt$ =
  meth-spec ($java/lang/Math$, "pow",
             meth-type ($java-double-type$, $java-double-type$, $java-double-type$),
             j-invokestatic) ;


// duplicate // define thread variable *expression-tree-nodes* = #f ;


define function emit-single-float-monadic (jbb, node :: <primitive-call>, meth :: <java-method-spec>)
  let  temps = make (<stretchy-vector>) ;
  do-used-value-references
    (method (tt)
       temps := add! (temps, tt)
     end,
     node) ;
  emit-expression (jbb, temps[0], *expression-tree-nodes*, 1) ;
  java-simple-op (jbb, j-f2d) ;
  java-call (jbb, meth) ;
  java-simple-op (jbb, j-d2f)
end;


define function emit-single-float-dyadic (jbb, node :: <primitive-call>, meth :: <java-method-spec>)
  let  temps = make (<stretchy-vector>) ;
  do-used-value-references
    (method (tt)
       temps := add! (temps, tt)
     end,
     node) ;
  emit-expression (jbb, temps[0], *expression-tree-nodes*, 1) ;
  java-simple-op (jbb, j-f2d) ;
  emit-expression (jbb, temps[1], *expression-tree-nodes*, 1);
  java-simple-op (jbb, j-f2d) ;
  java-call (jbb, meth) ;
  java-simple-op (jbb, j-d2f)
end;





define method gen-primitive
    (prim-name == #"primitive-single-float-sqrt",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-single-float-monadic (jbb, node, $math-method-sqrt$)
end;

define method gen-primitive
    (prim-name == #"primitive-single-float-log",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-single-float-monadic (jbb, node, $math-method-log$)
end;

define method gen-primitive
    (prim-name == #"primitive-single-float-exp",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-single-float-monadic (jbb, node, $math-method-exp$)
end;

define method gen-primitive
    (prim-name == #"primitive-single-float-sin",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-single-float-monadic (jbb, node, $math-method-sin$)
end;

define method gen-primitive
    (prim-name == #"primitive-single-float-cos",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-single-float-monadic (jbb, node, $math-method-cos$)
end;

define method gen-primitive
    (prim-name == #"primitive-single-float-tan",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-single-float-monadic (jbb, node, $math-method-tan$)
end;

define method gen-primitive
    (prim-name == #"primitive-single-float-asin",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-single-float-monadic (jbb, node, $math-method-asin$)
end;

define method gen-primitive
    (prim-name == #"primitive-single-float-acos",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-single-float-monadic (jbb, node, $math-method-acos$)
end;

define method gen-primitive
    (prim-name == #"primitive-single-float-atan",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-single-float-monadic (jbb, node, $math-method-atan$)
end;


define method gen-primitive
    (prim-name == #"primitive-single-float-pow",  // CHECK THIS ONE
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-single-float-dyadic (jbb, node, $math-method-expt$)
end;



define method gen-primitive
    (prim-name == #"primitive-vector",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  let n = args ;
  emit-expression-leaf (jbb, n) ;
    java-op2 (jbb, j-newarray, array-type ($dylan-class-<object>$)) ;
						
  until (n = 0)
    n := n - 1 ;
    java-simple-op (jbb, j-dup-x1) ;
    emit-swap (jbb) ;
    emit-raw-expression-leaf (jbb, n) ;
    emit-swap (jbb) ;
    java-simple-op (jbb, j-aastore)
  end;
end;

define method gen-primitive
    (prim-name == #"primitive-values",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  format-out ("primitive-values used - WHY?\n");
end;

define method gen-primitive
    (prim-name == #"primitive-break",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-software) ;
end;


//  public abstract `<object>' J`element' (int index) ;
define constant  $dylan-sequence-element-low-meth$ =
  meth-spec ($dylan-class-<sequence>$, "Jelement", 
             meth-type ($dylan-class-<sequence>$, $java-int-type$),
             j-invokevirtual) ;



define method gen-primitive
    (prim-name == #"primitive-element",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ;  // byte offset irrelevant
  java-call (jbb, $dylan-sequence-element-low-meth$) ;
end;


// not sure this should be used - always going through element, which I catch for <collection>?
define method gen-primitive
    (prim-name == #"primitive-element-setter",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // should really generate args in right order!!
  emit-pop (jbb) ;    // lose byte offset
  java-simple-op (jbb, j-dup-x2) ;
  emit-pop (jbb) ;
  java-simple-op (jbb, j-dup-x2) ;
  emit-pop (jbb) ;
  java-simple-op (jbb, j-dup-x2) ;
  java-simple-op (jbb, j-aastore) ; // bizarre argument rotation code!
end;


define method gen-primitive
    (prim-name == #"primitive-byte-element",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ;  // byte offset superfluous
  java-simple-op (jbb, j-baload) ;
end;

define method gen-primitive
    (prim-name == #"primitive-byte-element-setter",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // should really generate args in right order!!
  emit-pop (jbb) ;
  java-simple-op (jbb, j-dup-x2) ;
  emit-pop (jbb) ;
  java-simple-op (jbb, j-dup-x2) ;
  emit-pop (jbb) ;
  java-simple-op (jbb, j-dup-x2) ;
  java-simple-op (jbb, j-bastore) ;
end;

define constant $byte-char-value-meth$ = 
  meth-spec ($dylan-class-<byte-character>$,
             "value",
             meth-type ($java-byte-type$),
             j-invokevirtual) ;
define constant $unicode-char-value-meth$ = 
  meth-spec ($dylan-class-<unicode-character>$,
             "value",
             meth-type ($java-char-type$),
             j-invokevirtual) ;

define method gen-primitive
     (prim-name == #"primitive-byte-character-as-raw",
      node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // convert byte character to raw
  java-call (jbb, $byte-char-value-meth$) ;
end;
// Dummy HACKY impl
define method gen-primitive
     (prim-name == #"primitive-raw-as-byte-character",
      node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // convert raw to byte character
  emit-pop (jbb) ;
  emit-expression-leaf (jbb, #f)
end;
// complete GUESS
define method gen-primitive
    (prim-name == #"primitive-unicode-character-as-raw",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-raw-expression-leaf (jbb, 2) ; // try taking the tag off?
  java-simple-op (jbb, j-iushr) ;
  java-simple-op (jbb, j-int2char) ;  // ensure 16 bits unsigned
end;

// Dummy HACKY impl
define method gen-primitive
     (prim-name == #"primitive-raw-as-unicode-character",
      node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // convert raw to byte character
  emit-pop (jbb) ;
  emit-expression-leaf (jbb, #f)
end;



// dummy arithmetic
// dummy single-float EXPT
define method gen-primitive
     (prim-name == #"primitive-single-float-expt",
      node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // convert raw to byte character
  emit-pop (jbb) ;
end;


//  public void J`replace!' (int dest-offset, <simple-object-vector> src, int src-offset, int count) ;
define constant  $dylan-sov-replace!-meth$ =
  meth-spec ($dylan-class-<simple-object-vector>$, 
	     concatenate ("J", "replace!".java-name-mangle),
	     meth-type ($java-void-type$, $java-int-type$, $dylan-class-<simple-object-vector>$, $java-int-type$, $java-int-type$),
	     j-invokevirtual) ;

define method gen-primitive
    (prim-name == #"primitive-replace!",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-call (jbb, $dylan-sov-replace!-meth$) ;
end;


define method gen-primitive
    (prim-name == #"primitive-function-parameter",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // use "this" as the function parameter - IS THIS RIGHT
  emit-push-local (jbb, 0, j-ref-code);
end;


// make no distinction between getter & setter, since single name in Java
// needs bringing up to date
define method java-rep (slot-desc :: <&slot-descriptor>) => (thing)
  let  (slot-kind, slot-type-class, reader-spec) = java-getter-and-setter (slot-desc) ;
  slot-spec (slot-desc.^slot-owner.java-class-for-thing,
             slot-desc.^slot-getter.^debug-name.java-name-mangle,
             slot-desc.^slot-type.java-class-for-slot,
             #f)
end;


// needs bringing up to date
define method java-rep (slot-desc :: <&repeated-slot-descriptor>) => (thing)
  let  (slot-kind, slot-type-class, reader-spec) = java-getter-and-setter (slot-desc) ;
  slot-spec (slot-desc.^slot-owner.java-class-for-thing,
             slot-desc.^slot-getter.^debug-name.java-name-mangle,
             array-type (slot-desc.^slot-type.java-class-for-slot),
             #f)
end;

define generic java-class-for-slot (type) => (jc :: <java-type>) ;

define method java-class-for-slot (type :: <&integer>) => (jc :: <java-type>)
  $java-int-type$
end;
define method java-class-for-slot (type :: <&single-float>) => (jc :: <java-type>)
  $java-float-type$
end;
define method java-class-for-slot (type :: <&double-float>) => (jc :: <java-type>)
  $java-double-type$
end;
  
define method java-class-for-slot (type) => (jc :: <java-type>)
  type.java-class-for-thing
end;



define function new-find-specializer (thing) => (type :: false-or (<&type>))
  let  te = type-estimate (thing) ;
  if (te)
//    format-out ("$$$ returning type-estimate %s\n", te) ;
    let  type-est = 
      if (instance? (te, <type-estimate-values>))
	let fixed = te.type-estimate-fixed-values ;
	if (~ (fixed.empty?))
	  fixed.first
	else
	  te.type-estimate-rest-values
	end
      else
	te
      end ;
    if (instance? (type-est, <type-estimate-bottom>))
      $java-void-type$
    elseif (instance? (type-est, <type-estimate-union>))
      $dylan-class-<object>$  // WRONG, but safe for now!
    else
      type-estimate-class (type-est)
    end
  else
    error ("$$$ no type-estimate, using find-specializer") ;
    find-specializer (thing)
  end
end;

// these are supposed to be unused - superseded by new-find-specializer
define function find-specializer (tmp :: <value-reference>) => (type :: false-or (<&type>))
  error ("find-specializer - out of date")
end;

/* these are supposed to be unused - superseded by new-find-specializer
define function find-specializer-comp (comp :: <computation>) => (type :: false-or (<&type>))
  error ("find-specializer-comp - out of date")
end;

define function find-specializer-prim (prim :: <symbol>) => (type :: false-or (<&type>))
  error ("find-specializer-prim - out of date")
end;

define function eval-type (thing) => (type :: false-or (<&type>))
  error ("eval-type - out of date")
end;
*/

/*
define sealed generic find-specializer (tmp :: <value-reference>) => (type :: false-or (<&type>))  ;

define sealed generic find-specializer-comp (comp :: <computation>) => (type :: false-or (<&type>))  ;

define sealed generic find-specializer-prim (prim :: <symbol>) => (type :: false-or (<&type>)) ;

define sealed generic eval-type (thing) => (type :: false-or (<&type>)) ;


define method find-specializer (tmp :: <value-reference>) => (type :: false-or (<&type>)) 
  tmp.reference-value.^object-class
end;

define method find-specializer (tmp :: <temporary>) => (type :: false-or (<&type>)) 
  find-specializer-comp (tmp.generator)
end;

define method find-specializer-comp (random :: <computation>) => (type :: false-or (<&type>)) 
  format-out ("  find-specializer-comp failed on %s\n", random.object-class);
  examine (random);
  #f
end;

// blocks seem not to return a value properly?
define method find-specializer-comp (blk :: <block>) => (type :: false-or (<&type>)) 
  #f
end;

define method find-specializer-comp (mkclos :: <make-closure>) => (type :: false-or (<&type>))
  mkclos.computation-closure-method.^object-class
end;
define method find-specializer-comp (mkclos :: <initialize-closure>) => (type :: false-or (<&type>))
  mkclos.computation-closure-method.^object-class
end;




define method find-specializer-comp (mb :: <make-cell>) => (type :: false-or (<&type>)) 
  find-specializer (mb.computation-value)
end;

define method find-specializer-comp (mb :: <get-cell-value>) => (type :: false-or (<&type>)) 
  find-specializer (mb.computation-cell)
end;


define method find-specializer-comp (guarantee :: <guarantee-type>) => (type :: false-or (<&type>)) 
  guarantee.guaranteed-type
end;

define method find-specializer (lex :: <lexical-variable>) => (type :: false-or (<&type>)) 
//  format-out ("  found spec %s for var %s\n", lex.specializer, lex);
  lex.specializer
end;

define method find-specializer-comp (tt :: <temporary-transfer>) => (type :: false-or (<&type>)) 
//  format-out ("  find-specializer-comp <temporary-transfer>\n");
  find-specializer (tt.computation-value)
end;



define method find-specializer-comp (m :: <binary-merge>) => (type :: false-or (<&type>)) 
//  format-out ("  find-specializer-comp <merge>\n");
  let  left-res = find-specializer (m.merge-left-value) ;
  let  right-res = find-specializer (m.merge-right-value) ;
  if (left-res == right-res)
    left-res
  else
    #f
  end
end; 


define method find-specializer-comp (esv :: <extract-single-value>) => (type :: false-or (<&type>)) 
//  format-out ("  find-specializer-comp <extract-single-value>\n");
  let  ind = esv.index ;
  if (ind == 0)
    find-specializer (esv.computation-value)
  else
    find-mv-specializer (esv.computation-value, ind)
  end
end;

define function find-mv-specializer (node, index) => (type :: false-or (<&type>)) 
//  format-out ("  fmvs\n");
  if (index == 0)
    find-specializer (node)
  else
    format-out ("fmvs failed on %s[%d]\n", node, index);
    #f
  end
end;

define method find-specializer-comp (ct :: <check-type>) => (type :: false-or (<&type>)) 
//  format-out ("  find-specializer-comp <check-type>\n");
  eval-type (ct.type)
end;
define method find-specializer-comp (ct :: <multiple-value-check-type-computation>) => (type :: false-or (<&type>)) 
//  format-out ("  find-specializer-comp <multiple-value-check-type-computation>\n");
  eval-type (ct.types[0])
end;


define method eval-type (thing :: <object>) => (type :: false-or (<&type>)) 
  format-out (" eval-type failed on %s\n", thing) ;
  #f
end;

define method eval-type (tmp :: <temporary>) => (type :: false-or (<&type>)) 
//  format-out (" eval-type <temporary>\n");
  eval-type (tmp.generator)
end;

define method eval-type (oref :: <object-reference>) => (type :: false-or (<&type>)) 
  let  some-type = oref.computation-value ;
  format-out (" eval-type <object-reference> -> %s\n", some-type);
  some-type
end;




define method find-specializer-comp (call :: <primitive-call>) => (type :: false-or (<&type>))
  let  prim = call.primitive ;
  let  prim-name = as (<symbol>, prim.binding-name) ;
  find-specializer-prim (prim-name)
end;


define method find-specializer-prim (prim :: <symbol>) => (type :: false-or (<&type>))
  #f
end;

define method find-specializer-prim (prim == #"primitive-raw-as-boolean") => (type :: false-or (<&type>))
  <&boolean>
end;

define method find-specializer-prim (prim == #"primitive-cast-raw-as-integer") => (type :: false-or (<&type>))
  <&integer>
end;

define method find-specializer-prim (prim == #"primitive-raw-as-single-float") => (type :: false-or (<&type>))
  <&single-float>
end;

define method find-specializer-prim (prim == #"primitive-raw-as-double-float") => (type :: false-or (<&type>))
  <&double-float>
end;

define method find-specializer-prim (prim == #"primitive-single-float-as-small-integer") => (type :: false-or (<&type>))
  <&integer>
end;
*/


// nearly obsolete? - new slot-value computations
define method gen-primitive
    (prim-name == #"primitive-initialized-slot-value",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // use "this" as the function parameter - this seems correct to me
  let  temps = node.arguments ;
  let  inst-temp = temps[0] ;
  let  ind-temp = temps[1] ;
  let  claz = new-find-specializer (inst-temp) ;  // really needs to be type!
//  format-out ("  initialized-slot-value: temps %s %s, claz %s\n", inst-temp, ind-temp, claz) ;
  emit-pop (jbb) ;  // lose the stupid slot-index
  if (instance? (claz, <&class>) & ~ (claz.^direct-superclasses.empty?))
    if (instance? (ind-temp, <value-reference>))
      let  index = ind-temp.reference-value.^raw-object-value ;  // yukky yukky yukky
      let  slot-desc = claz.^instance-slot-descriptors [index] ;
  //    format-out ("  slot-desc %d for %s is %s\n", index, claz, slot-desc) ;
      java-read (jbb, slot-desc.java-rep)
    else
      format-out ("$$$$$$ slot-value dynamic case!\n") ;
      
    end
  else
    // here we call the generic
    error ("  Hoist low-level slot %s in absence of type info\n", ind-temp);
    //emit-expression-leaf (jbb, slot-desc.^slot-getter.java-rep) ;
    //emit-swap (jbb) ;
    //java-call (jbb, dylan-xep-invoke-method (#f, 1)) ;
  end
end;

/* duplicate
define constant $dylan-unbound-slot-meth$ =
  meth-spec ($dylan-class-<class>$, "unbound_slot_error", meth-type ($java-void-type$), j-invokestatic) ;
*/

define method gen-primitive
    (prim-name == #"primitive-slot-value",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // use "this" as the function parameter - this seems correct to me
  gen-primitive (#"primitive-initialized-slot-value", node, args, jbb) ;
  emit-dup (jbb) ;
  java-branch-op (jbb, j-ifnonnull, 7) ;
  emit-pop (jbb) ;
  java-call (jbb, $dylan-unbound-slot-meth$)
end;


define method gen-primitive
    (prim-name == #"primitive-initialized-slot-value-setter",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // use "this" as the function parameter - this seems correct to me
  let  temps = node.arguments ;
  let  inst-temp = temps[1] ;
  let  ind-temp = temps[2] ;
//  format-out ("  initialized-slot-value-setter: temps %s %s\n", inst-temp, ind-temp) ;
  let  claz = new-find-specializer (inst-temp) ;
  emit-pop (jbb) ; // lose index
  emit-swap (jbb) ; // swap inst and newval
  if (instance? (claz, <&class>))  // should check field is known about in class too
    java-simple-op (jbb, j-dup-x1) ; // save newval for return val
//    let  index = ind-temp.generator.computation-value.^raw-object-value ;  // yukky yukky yukky
    if (instance? (ind-temp, <value-reference>))
      let  index = ind-temp.reference-value.^raw-object-value ;  // yukky yukky yukky
      let  slot-desc = claz.^instance-slot-descriptors [index] ;
  //    format-out ("  slot-desc %d for %s is %s\n", index, claz, slot-desc) ;
      java-write (jbb, slot-desc.java-rep)
    else
      format-out ("$$$$$$  slot-desc dynamic case!!!\n") ;
      emit-pop (jbb) ; emit-pop (jbb)
    end
  else
    // here we call the setter
    error ("  Hoist low-level slot %s in absence of type info\n", ind-temp);
    //emit-expression-leaf (jbb, slot-desc.^slot-setter.java-rep) ;
    //java-simple-op (jbb, j-dup-x2) ;
    //emit-pop (jbb) ;
    //java-call (jbb, dylan-xep-invoke-method (#f, 2))
  end
end;

define method gen-primitive
    (prim-name == #"primitive-slot-value-setter",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // use "this" as the function parameter - this seems correct to me
  gen-primitive (#"primitive-initialized-slot-value-setter", node, args, jbb)
end;

//*/

define method gen-primitive
    (prim-name == #"primitive-next-methods-parameter",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-call (jbb, $next-methods-getter$)
end;

define method gen-primitive
    (prim-name == #"primitive-next-methods-parameter-setter",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-expression-leaf (jbb, (node.arguments)[0]) ;
  java-call (jbb, $next-methods-setter$) ;
  // not sure if should leave #next on stack or not?
end;


define constant  $dylan-sov-copy-vector-meth$ =
  meth-spec ($dylan-class-<simple-object-vector>$,
	     "copy_vector",
	     meth-type ($dylan-class-<simple-object-vector>$, 
			$dylan-class-<integer>$), 
	     j-invokevirtual) ;

define method gen-primitive
    (prim-name == #"primitive-copy-vector",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-expression-leaf (jbb, 0) ;  // fake length arg?
  java-call (jbb, $dylan-sov-copy-vector-meth$)
end;

define constant $dylan-instance-meth$ =
  meth-spec ($dylan-class-<object>$, "instanceQ", meth-type ($dylan-class-<boolean>$,
							     $dylan-class-<object>$, 
							     $dylan-class-<class>$),
	     j-invokestatic) ;

define method gen-primitive
    (prim-name == #"primitive-instance?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  let  type-arg = node.arguments [1] ;
  block (return)
    if (instance? (type-arg, <object-reference>))
      let  type = type-arg.reference-value ;
      if (instance? (type, <&class>))
        emit-pop (jbb) ;
        java-op2 (jbb, j-instanceof, java-class-for-thing (type-arg.reference-value)) ;
        gen-set-boolean (jbb, j-ifne, #f) ;
        return ()
      elseif (instance? (type, <&singleton>))
        emit-pop (jbb) ;
        emit-expression-leaf (jbb, java-rep (type.^singleton-object)) ;
        gen-set-boolean (jbb, j-if-acmpeq, #f) ;
        return ()
      end
    end;
    java-call (jbb, $dylan-instance-meth$)
  end
end;


define method gen-primitive
    (prim-name == #"primitive-resolve-symbol",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-call (jbb, $symbol-fixer-method2$) ;  
end;



// various TEMPORARY HACKS to allow stuff to run!

// totally HACKed version!! just to allow back end to run
define method gen-primitive
    (prim-name == #"primitive-mep-apply-with-optionals",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  for (i :: <integer> from 0 below args)
    emit-pop (jbb)
  end;
  emit-expression-leaf (jbb, #f) // fake as always false for now
end;
// totally HACKed version!! just to allow back end to run
define method gen-primitive
    (prim-name == #"primitive-engine-node-apply-with-optionals",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  for (i :: <integer> from 0 below args)
    emit-pop (jbb)
  end;
  emit-expression-leaf (jbb, #f) // fake as always false for now
end;
// totally HACKed version!! just to allow back end to run
define method gen-primitive
    (prim-name == #"primitive-initialize-engine-node",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ; // lose engine node
end;


// FAKED - returns False
define method gen-primitive
    (prim-name == #"primitive-inside-debugger?",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-expression-leaf (jbb, #f) // fake as always false for now
end;



// totally HACKed version!! just to allow back end to run
define method gen-primitive
    (prim-name == #"primitive-allocate-in-awl-pool",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  for (i :: <integer> from 0 below args)
    emit-pop (jbb)
  end;  // force stack to right size!
  emit-expression-leaf (jbb, #f) // fake as always false for now
end;
// totally HACKed version!! just to allow back end to run
define method gen-primitive
    (prim-name == #"primitive-allocate-weak-in-awl-pool",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  for (i :: <integer> from 0 below args)
    emit-pop (jbb)
  end;  // force stack to right size!
  emit-expression-leaf (jbb, #f) // fake as always false for now
end;

// totally HACKed version!! just to allow back end to run
define method gen-primitive
    (prim-name == #"primitive-untraced-allocate",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb) ;
  emit-expression-leaf (jbb, #f) // fake as always false for now
end;
// totally HACKed version!! just to allow back end to run
define method gen-primitive
    (prim-name == #"primitive-byte-allocate-filled",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  for (i :: <integer> from 0 below args)
    emit-pop (jbb)
  end;  // force stack to right size!
  emit-expression-leaf (jbb, #f) // fake as always false for now
end;
// totally HACKed version!! just to allow back end to run
define method gen-primitive
    (prim-name == #"primitive-byte-allocate-filled-terminated",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  for (i :: <integer> from 0 below args)
    emit-pop (jbb)
  end;  // force stack to right size!
  emit-expression-leaf (jbb, #f) // fake as always false for now
end;
// totally HACKed version!! just to allow back end to run
define method gen-primitive
    (prim-name == #"primitive-double-byte-allocate-filled",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  for (i :: <integer> from 0 below args)
    emit-pop (jbb)
  end;  // force stack to right size!
  emit-expression-leaf (jbb, #f) // fake as always false for now
end;
// totally HACKed version!! just to allow back end to run
define method gen-primitive
    (prim-name == #"primitive-word-allocate-filled",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  for (i :: <integer> from 0 below args)
    emit-pop (jbb)
  end;  // force stack to right size!
  emit-expression-leaf (jbb, #f) // fake as always false for now
end;
// totally HACKed version!! just to allow back end to run
define method gen-primitive
    (prim-name == #"primitive-single-float-allocate-filled",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  for (i :: <integer> from 0 below args)
    emit-pop (jbb)
  end;  // force stack to right size!
  emit-expression-leaf (jbb, #f) // fake as always false for now
end;
// totally HACKed version!! just to allow back end to run
define method gen-primitive
    (prim-name == #"primitive-double-float-allocate-filled",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  for (i :: <integer> from 0 below args)
    emit-pop (jbb)
  end;  // force stack to right size!
  emit-expression-leaf (jbb, #f) // fake as always false for now
end;


define method gen-primitive
    (prim-name == #"primitive-unwrap-abstract-integer",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb);
  emit-java-int (jbb, 0) // fake as always false for now
end;
define method gen-primitive
    (prim-name == #"primitive-wrap-unsigned-abstract-integer",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  emit-pop (jbb);
  emit-expression-leaf (jbb, #f) // fake as always false for now
end;


// NOTE THIS IS HACKED at moment.  ie  Broken!!
define method gen-primitive
    (prim-name == #"primitive-cast-pointer-as-raw",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // fake with a zero for now
  emit-pop (jbb) ;
  emit-java-int (jbb, 0)
end;

// NOTE THIS IS HACKED at moment.  ie  Broken!!
define method gen-primitive
    (prim-name == #"primitive-cast-single-float-as-machine-word",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // fake with a zero for now
  emit-pop (jbb) ;
  emit-java-int (jbb, 0)
end;

// NOTE THIS IS HACKED at moment.  ie  Broken!!
define method gen-primitive
    (prim-name == #"primitive-preboot-symbols",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // fake with a zero for now
  emit-expression-leaf (jbb, #[])
end;


// NOTE THIS IS HACKED at moment.  ie  Broken!!
define method gen-primitive
    (prim-name == #"primitive-apply",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  // fake with a zero for now
  for (n :: <integer> from 0 below args)
    emit-pop (jbb) 
  end;
  emit-expression-leaf (jbb, #f)
end;


// don't know if right semantics:
define method gen-primitive
    (prim-name == #"primitive-integer-as-single-float",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-i2f) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-floor/-quotient",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-idiv) ;
end;

define method gen-primitive
    (prim-name == #"primitive-machine-word-floor/-remainder",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-imod) ;
end;

// not correct yet
define method gen-primitive
    (prim-name == #"primitive-machine-word-shift-left-with-overflow",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-ishl) ;
  // should really set a second value with overflow
end;

// not correct yet?
define method gen-primitive
    (prim-name == #"primitive-machine-word-unsigned-shift-right",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-simple-op (jbb, j-iushr) ;
  // should really set a second value with overflow
end;

// not correct yet?
define method gen-primitive
    (prim-name == #"primitive-machine-word-unsigned-double-shift-left-high",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  let  tmp = get-temp-local-var ();
  emit-dup (jbb) ;
  emit-pop-local (jbb, tmp, j-int-code) ;  // save the shift amount
  java-simple-op (jbb, j-ishl) ;
  emit-swap (jbb) ;
  emit-java-int (jbb, 32) ;
  emit-push-local (jbb, tmp, j-int-code) ;
  java-simple-op (jbb, j-isub) ;
  java-simple-op (jbb, j-iushr) ;
  java-simple-op (jbb, j-ior) 
end;


// HACKED - this is extra-linguistic primitive
define method gen-primitive
    (prim-name == #"primitive-fill!",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  for (i :: <integer> from 1 below args)
    emit-pop (jbb) ;
  end;
  // return just the target argument?
end;

// HACKED - this is extra-linguistic primitive
define method gen-primitive
    (prim-name == #"primitive-replace-bytes!",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  for (i :: <integer> from 1 below args)
    emit-pop (jbb) ;
  end;
  // return just the target argument?
end;

// HACKED - this is extra-linguistic primitive
define method gen-primitive
    (prim-name == #"primitive-set-generic-function-entrypoints",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
//  emit-pop (jbb) ;
end;

// HACKED - this is extra-linguistic primitive
define method gen-primitive
    (prim-name == #"primitive-set-generic-function-xep",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
//  emit-pop (jbb) ;
end;

// convert string to symbol - needs checking
define method gen-primitive
    (prim-name == #"primitive-string-as-symbol",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
  java-call (jbb, $symbol-fixer-method2$)
end;

define method gen-primitive
    (prim-name == #"primitive-runtime-module-handle",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
     emit-expression-leaf (jbb, #f)
end;

/* template
define method gen-primitive
    (prim-name == #"primitive-",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
end;
*/


// now the stuff to generate <boolean> and <raw-boolean> stuff in
// an <if> context.  Pass down the true and false labels, take
// care to punt if we go outside the current expression-tree

define sealed generic gen-raw-if-primitive (prim-name :: <symbol>,
				     node :: <primitive-call>,
				     temps :: <stretchy-vector>,
				     nodes :: <stretchy-vector>,
				     jbb :: <java-basic-block>,
				     true-dest,
				     false-dest,
                                     fall-through) => () ;


define sealed generic gen-obj-if-primitive (prim-name :: <symbol>,
				     node :: <primitive-call>,
				     temps :: <stretchy-vector>,
				     nodes :: <stretchy-vector>,
				     jbb :: <java-basic-block>,
				     true-dest,
				     false-dest,
                                     fall-through) => () ;


define method gen-raw-if-primitive (prim-name :: <symbol>,
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
  // default is to punt to normal case - evaluate then test
  for (tt in temps, n from 0)
    emit-expression (jbb, tt, nodes, #t) 
  end;
  gen-primitive (prim-name, node, temps.size, jbb) ;
  let  m = jbb.meth ;
  emit-smart-if-branch (jbb, j-ifne, j-ifeq, true-dest, false-dest, fall-through)
end;

define method gen-obj-if-primitive (prim-name :: <symbol>,
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
  // default is to punt to normal case - evaluate then test
  for (tt in temps, n from 0)
    emit-expression (jbb, tt, nodes, #t) 
  end;
  gen-primitive (prim-name, node, temps.size, jbb) ;
  emit-expression-leaf (jbb, #f) ;
  emit-smart-if-branch (jbb, j-if-acmpne, j-if-acmpeq, true-dest, false-dest, fall-through)
end;


define method gen-raw-if-primitive (prim-name == #"primitive-boolean-as-raw",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
  let  arg = temps[0] ;
  emit-obj-if-expression (jbb, arg, nodes, true-dest, false-dest, fall-through)
end;

define method gen-obj-if-primitive (prim-name == #"primitive-raw-as-boolean",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
  let  arg = temps[0] ;
  emit-raw-if-expression (jbb, arg, nodes, true-dest, false-dest, fall-through)
end;


define function gen-id? (jbb, nodes, tmp0, tmp1, true-dest, false-dest, fall-through)
  let  arg0 = tmp0.generator ;
  let  arg1 = tmp1.generator ;
  if (instance? (arg0, <object-reference>) & instance? (arg1, <object-reference>))
    let  right-dest = if (arg0.computation-value == arg1.computation-value)
                        true-dest
                      else
                        false-dest
                      end ;
    if (right-dest ~== fall-through)
      java-branch-op (jbb, j-goto, right-dest) 
    end;
    0
  else
    let  arg = #f ;
    if (instance? (arg0, <object-reference>))  format-out ("### %s %s\n", arg0.reference-value) end;
    if (instance? (arg1, <object-reference>))  format-out ("### %s %s\n", arg1.reference-value) end;
    if (instance? (arg0, <object-reference>) & (arg0.reference-value == #f))
      arg := tmp1
    end;
    if (instance? (arg1, <object-reference>) & (arg1.reference-value == #f))
      arg := tmp0
    end;
    if (arg)
      // case where one arm is #f, can punt to if on the other
      emit-obj-if-expression (jbb, arg, nodes, false-dest, true-dest, fall-through)
    else
      emit-expression (jbb, tmp0, nodes, 1) ;
      emit-expression (jbb, tmp1, nodes, 1) ;
      let  m = jbb.meth ;
      emit-smart-if-branch (jbb, j-if-acmpeq, j-if-acmpne, true-dest, false-dest, fall-through)
    end
  end      
end;  

define method gen-obj-if-primitive (prim-name == #"primitive-instance?",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
format-out ("### in gen-obj-if-primitive PRIMITIVE-INSTANCE?\n") ;
  let  obj-temp = temps [0] ;
  let  type-temp = temps [1] ;
  emit-expression (jbb, obj-temp, nodes, 1) ;
  block (return)
    if (instance? (type-temp, <object-reference>))
format-out ("### PRIMITIVE-INSTANCE? of <object-reference>\n") ;
      let  type = type-temp.reference-value ;
      if (instance? (type, <&class>))
format-out ("### PRIMITIVE-INSTANCE? of <&class> <object-reference>\n") ;
        java-op2 (jbb, j-instanceof, java-class-for-thing (type)) ;
        emit-smart-if-branch (jbb, j-ifne, j-ifeq, true-dest, false-dest, fall-through) ;
        return ()
      elseif (instance? (type, <&singleton>))
format-out ("### PRIMITIVE-INSTANCE? of <&singleton> <object-reference>\n") ;
        emit-expression-leaf (jbb, java-rep (type.^singleton-object)) ;
        emit-smart-if-branch (jbb, j-if-acmpeq, j-if-acmpne, true-dest, false-dest, fall-through) ;
        return () ;
      end
    end;
format-out ("### PRIMITIVE-INSTANCE? general case, call out of line\n") ;
    emit-expression (jbb, type-temp, nodes, 1) ;
    java-call (jbb, $dylan-instance-meth$) ;
    emit-expression-leaf (jbb, #f) ;
    emit-smart-if-branch (jbb, j-if-acmpne, j-if-acmpeq, true-dest, false-dest, fall-through)
  end
end;



define method gen-obj-if-primitive (prim-name == #"primitive-id?",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
format-out ("### in gen-obj-if-primitive PRIMITIVE-ID?\n") ;
  gen-id? (jbb, nodes, temps[0], temps[1], true-dest, false-dest, fall-through)
end;

define method gen-obj-if-primitive (prim-name == #"primitive-not-id?",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
format-out ("### in gen-obj-if-primitive PRIMITIVE-NOT-ID?\n") ;
  gen-id? (jbb, nodes, temps[0], temps[1], false-dest, true-dest, fall-through)
end;


define method gen-obj-if-primitive (prim-name == #"primitive-true?",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest, 
                                    fall-through) => ()
format-out ("### in gen-obj-if-primitive PRIMITIVE-TRUE?\n") ;
  emit-raw-if-expression (jbb, temps[0], nodes, true-dest, false-dest, fall-through)
end;

define method gen-obj-if-primitive (prim-name == #"primitive-false?",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
format-out ("### in gen-obj-if-primitive PRIMITIVE-FALSE?\n") ;
  emit-raw-if-expression (jbb, temps[0], nodes, false-dest, true-dest, fall-through)
end;

define method gen-obj-if-primitive (prim-name == #"primitive-not",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
format-out ("### in gen-obj-if-primitive PRIMITIVE-NOT\n") ;
  emit-obj-if-expression (jbb, temps[0], nodes, false-dest, true-dest, fall-through)
end;


define function gen-int-compare (arg0, arg1, jbb, nodes, true-opcode, false-opcode, true-dest, false-dest, fall-through)
  emit-expression (jbb, arg0, nodes, #t) ;
  emit-expression (jbb, arg1, nodes, #t) ;
  emit-smart-if-branch (jbb, true-opcode, false-opcode, true-dest, false-dest, fall-through)
end;

define function gen-unsigned-int-compare (arg0, arg1, jbb, nodes, true-opcode, false-opcode, true-dest, false-dest, fall-through)
  emit-expression (jbb, arg0, nodes, #t) ;
  flip-int-sign-bit (jbb) ;
  emit-expression (jbb, arg1, nodes, #t) ;
  flip-int-sign-bit (jbb) ;
  emit-smart-if-branch (jbb, true-opcode, false-opcode, true-dest, false-dest, fall-through)
end;


define method gen-obj-if-primitive (prim-name == #"primitive-machine-word-equal?",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
  gen-int-compare (temps[0], temps[1], jbb, nodes, j-if-icmpeq, j-if-icmpne, true-dest, false-dest, fall-through)
end;

define method gen-obj-if-primitive (prim-name == #"primitive-machine-word-not-equal?",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
  gen-int-compare (temps[0], temps[1], jbb, nodes, j-if-icmpne, j-if-icmpeq, true-dest, false-dest, fall-through)
end;

define method gen-obj-if-primitive (prim-name == #"primitive-machine-word-less-than?",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
  gen-int-compare (temps[0], temps[1], jbb, nodes, j-if-icmplt, j-if-icmpge, true-dest, false-dest, fall-through)
end;
define method gen-obj-if-primitive (prim-name == #"primitive-machine-word-unsigned-less-than?",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
  gen-unsigned-int-compare (temps[0], temps[1], jbb, nodes, j-if-icmplt, j-if-icmpge, true-dest, false-dest, fall-through)
end;

define method gen-obj-if-primitive (prim-name == #"primitive-machine-word-not-less-than?",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest, 
                                    fall-through) => ()
  gen-int-compare (temps[0], temps[1], jbb, nodes, j-if-icmpge, j-if-icmplt, true-dest, false-dest, fall-through)
end;
define method gen-obj-if-primitive (prim-name == #"primitive-machine-word-unsigned-not-less-than?",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
  gen-unsigned-int-compare (temps[0], temps[1], jbb, nodes, j-if-icmpge, j-if-icmplt, true-dest, false-dest, fall-through)
end;

define method gen-obj-if-primitive (prim-name == #"primitive-machine-word-greater-than?",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
  gen-int-compare (temps[0], temps[1], jbb, nodes, j-if-icmpgt, j-if-icmple, true-dest, false-dest, fall-through)
end;
define method gen-obj-if-primitive (prim-name == #"primitive-machine-word-unsigned-greater-then?",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
  gen-unsigned-int-compare (temps[0], temps[1], jbb, nodes, j-if-icmpgt, j-if-icmple, true-dest, false-dest, fall-through)
end;

define method gen-obj-if-primitive (prim-name == #"primitive-machine-word-not-greater-than?",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
  gen-int-compare (temps[0], temps[1], jbb, nodes, j-if-icmple, j-if-icmpgt, true-dest, false-dest, fall-through)
end;
define method gen-obj-if-primitive (prim-name == #"primitive-machine-word-unsigned-not-greater-than?",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
  gen-unsigned-int-compare (temps[0], temps[1], jbb, nodes, j-if-icmple, j-if-icmpgt, true-dest, false-dest, fall-through)
end;


define function gen-float-compare (arg0, arg1, jbb, nodes, true-opcode, false-opcode, true-dest, false-dest, fall-through)
  emit-expression (jbb, arg0, nodes, #t) ;
  emit-expression (jbb, arg1, nodes, #t) ;
  let  m = jbb.meth ;
  java-simple-op (jbb, j-fcmpl) ;
  emit-smart-if-branch (jbb, true-opcode, false-opcode, true-dest, false-dest, fall-through)
end;

define method gen-obj-if-primitive (prim-name == #"primitive-single-float-equals?",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest, 
                                    fall-through) => ()
  gen-float-compare (temps[0], temps[1], jbb, nodes, j-ifeq, j-ifne, true-dest, false-dest, fall-through)
end;

define method gen-obj-if-primitive (prim-name == #"primitive-single-float-less-than?",
				    node :: <primitive-call>,
				    temps :: <stretchy-vector>,
				    nodes :: <stretchy-vector>,
				    jbb :: <java-basic-block>,
				    true-dest,
				    false-dest,
                                    fall-through) => ()
  gen-float-compare (temps[0], temps[1], jbb, nodes, j-iflt, j-ifge, true-dest, false-dest, fall-through)
end;

// temp HACK
define method gen-primitive
    (prim-name == #"primitive-object-allocate-filled",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
 for (i :: <integer> from 0 below args)
   emit-pop (jbb)
 end;
 emit-expression-leaf (jbb, 42) ;
 // hmm..
end;

define method gen-primitive
    (prim-name == #"primitive-allocate-wrapper",
     node :: <primitive-call>, args :: <integer>, jbb :: <java-basic-block>) => ()
 for (i :: <integer> from 0 below args)
   emit-pop (jbb)
 end;
 emit-expression-leaf (jbb, 42) ;
 // hmm..
end;


/*
left to do


define &primitive-descriptor primitive-single-float-as-big-integer;
define &primitive-descriptor primitive-big-integer-as-single-float;
define &primitive-descriptor primitive-single-float-as-bits;
define &primitive-descriptor primitive-bits-as-single-float;

define &primitive-descriptor primitive-vector-element;
define &primitive-descriptor primitive-vector-element-setter;



define &primitive-descriptor primitive-vector-size;
define &primitive-descriptor primitive-vector-as-raw;
define &primitive-descriptor primitive-raw-as-vector;

// String.
define &primitive-descriptor primitive-strlen;
define &primitive-descriptor primitive-string-as-raw;
define &primitive-descriptor primitive-raw-as-string;

define &primitive-descriptor primitive-lambda-parameter;

define &primitive-descriptor primitive-next-methods-parameter-setter;
define &primitive-descriptor primitive-set-generic-function-xep;
define &primitive-descriptor primitive-set-generic-function-entrypoints;

// Apply.
define &primitive-descriptor primitive-xep-apply;
define &primitive-descriptor primitive-mep-apply;
define &primitive-descriptor primitive-mep-apply-with-optionals;
define &primitive-descriptor primitive-iep-apply;
define &primitive-descriptor primitive-apply;

// Symbol boot.
define &primitive-descriptor primitive-resolve-symbol;
define &primitive-descriptor primitive-string-as-symbol;

// Terminal - need to find details of semantics
define &primitive-descriptor primitive-open;
define &primitive-descriptor primitive-input-terminal;
define &primitive-descriptor primitive-output-terminal;
define &primitive-descriptor primitive-input;
define &primitive-descriptor primitive-output;
define &primitive-descriptor primitive-force-output;
define &primitive-descriptor primitive-close;


define &primitive-descriptor primitive-integer?;
define &primitive-descriptor primitive-box-integer;
define &primitive-descriptor primitive-unbox-integer;
define &primitive-descriptor primitive-wrap-abstract-integer;
define &primitive-descriptor primitive-wrap-unsigned-abstract-integer;
define &primitive-descriptor primitive-unwrap-abstract-integer;
define &primitive-descriptor primitive-machine-word-boole;

define &primitive-descriptor primitive-machine-word-multiply-low/high;
define &primitive-descriptor primitive-machine-word-multiply-low-with-overflow;

define &primitive-descriptor primitive-machine-word-floor/-quotient;
define &primitive-descriptor primitive-machine-word-floor/-remainder;

define &primitive-descriptor primitive-machine-word-ceiling/-quotient;
define &primitive-descriptor primitive-machine-word-ceiling/-remainder;
define &primitive-descriptor primitive-machine-word-ceiling/;
define &primitive-descriptor primitive-machine-word-round/-quotient;
define &primitive-descriptor primitive-machine-word-round/-remainder;
define &primitive-descriptor primitive-machine-word-round/;
define &primitive-descriptor primitive-machine-word-truncate/-quotient;
define &primitive-descriptor primitive-machine-word-truncate/-remainder;

define &primitive-descriptor primitive-machine-word-divide-quotient;
define &primitive-descriptor primitive-machine-word-divide-remainder;

define &primitive-descriptor primitive-machine-word-shift-left-low;
define &primitive-descriptor primitive-machine-word-shift-left-high;
define &primitive-descriptor primitive-machine-word-shift-left-low/high;
define &primitive-descriptor primitive-machine-word-shift-left-low-with-overflow;
define &primitive-descriptor primitive-machine-word-shift-left-with-overflow;

define &primitive-descriptor primitive-machine-word-unsigned-add-with-carry;
define &primitive-descriptor primitive-machine-word-unsigned-subtract-with-borrow;
define &primitive-descriptor primitive-machine-word-unsigned-multiply-high;
define &primitive-descriptor primitive-machine-word-unsigned-multiply;
define &primitive-descriptor primitive-machine-word-unsigned-divide-quotient;
define &primitive-descriptor primitive-machine-word-unsigned-divide-remainder;
define &primitive-descriptor primitive-machine-word-unsigned-divide;
define &primitive-descriptor primitive-machine-word-unsigned-rotate-left;
define &primitive-descriptor primitive-machine-word-unsigned-rotate-right;
define &primitive-descriptor primitive-machine-word-unsigned-shift-right;
define &primitive-descriptor primitive-machine-word-unsigned-double-divide-quotient;
define &primitive-descriptor primitive-machine-word-unsigned-double-divide-remainder;
define &primitive-descriptor primitive-machine-word-unsigned-double-divide;
define &primitive-descriptor primitive-machine-word-unsigned-shift-left-high;
define &primitive-descriptor primitive-machine-word-unsigned-double-shift-left-high;
define &primitive-descriptor primitive-machine-word-unsigned-double-shift-left;
define &primitive-descriptor primitive-machine-word-unsigned-double-shift-right-low;
define &primitive-descriptor primitive-machine-word-unsigned-double-shift-right-high;
define &primitive-descriptor primitive-machine-word-unsigned-double-shift-right;

define &primitive-descriptor primitive-machine-word-bit-field-extract ;
define &primitive-descriptor primitive-machine-word-bit-field-deposit ;
*/
