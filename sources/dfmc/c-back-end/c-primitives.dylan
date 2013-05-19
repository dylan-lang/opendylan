module: dfmc-c-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define generic emit-primitive-call 
  (b :: <c-back-end>, s :: <stream>, d :: <integer>, 
   c :: <primitive-call>, f :: <&primitive>);

// define method emitter (primitive :: <&primitive>) => (emitter :: <function>)
//   primitive.primitive-descriptor-getter(current-back-end()).primitive-emitter
// end method;
// 
// define constant $default-primitive-descriptor = make(<primitive-descriptor>, emitter: emit-primitive-call);
// 
// define macro &primitive-descriptor-definer
//   { define &primitive-descriptor ?:name, 
//       #key ?emitter:expression = emit-primitive-call }
//     => { define method ?name ## "-descriptor-getter" (back-end :: <c-back-end>) => (descriptor :: <primitive-descriptor>)
//            $default-primitive-descriptor
//          end method; }
// end macro;
// 
// // // full generality below but currently special emitters are not used
// // 
// // define macro &primitive-descriptor-definer
// //   { define &primitive-descriptor ?:name, 
// //       #key ?emitter:expression = emit-primitive-call }
// //     => { define constant ?name ## "-descriptor"
// //            = make(<primitive-descriptor>, emitter: ?emitter);
// //          define method ?name ## "-descriptor-getter" (back-end :: <c-back-end>) => (descriptor :: <primitive-descriptor>)
// //            ?name ## "-descriptor"
// //          end method; }
// // end macro;
// 
// // Support
// define &primitive-descriptor primitive-break;
// define &primitive-descriptor primitive-invoke-debugger;
// define &primitive-descriptor primitive-debug-message;
// 
// // Machine
// define &primitive-descriptor primitive-word-size;
// define &primitive-descriptor primitive-header-size;
//     
// // Allocation.
// define &primitive-descriptor primitive-allocate;
// define &primitive-descriptor primitive-byte-allocate;
// define &primitive-descriptor primitive-allocate-wrapper;
// define &primitive-descriptor primitive-object-allocate-filled;
// define &primitive-descriptor primitive-byte-allocate-filled;
// define &primitive-descriptor primitive-double-byte-allocate-filled;
// define &primitive-descriptor primitive-word-allocate-filled;
// define &primitive-descriptor primitive-double-word-allocate-filled;
// define &primitive-descriptor primitive-single-float-allocate-filled;
// define &primitive-descriptor primitive-double-float-allocate-filled;
// define &primitive-descriptor primitive-allocate-in-awl-pool;
// define &primitive-descriptor primitive-allocate-weak-in-awl-pool;
// define &primitive-descriptor primitive-untraced-allocate;
// 
// // Accessors
// define &primitive-descriptor primitive-element;
// define &primitive-descriptor primitive-element-setter;
// define &primitive-descriptor primitive-byte-element;
// define &primitive-descriptor primitive-byte-element-setter;
// define &primitive-descriptor primitive-fill!;
// define &primitive-descriptor primitive-fill-bytes!;
// define &primitive-descriptor primitive-replace!;
// define &primitive-descriptor primitive-replace-bytes!;
// 
// // GC
// define &primitive-descriptor primitive-gc-state;
//   
// // Byte Character.
// define &primitive-descriptor primitive-byte-character-as-raw;
// define &primitive-descriptor primitive-raw-as-byte-character;
// 
// // Byte Character.
// define &primitive-descriptor primitive-unicode-character-as-raw;
// define &primitive-descriptor primitive-raw-as-unicode-character;
// 
// // Integer.
// 
// // Small integer.
// 
// // Big Integer.
// 
// // Machine Integer.
// 
// // Unsigned-Machine Integer.
// 
// // Address.
// 
// // Pointer.
// define &primitive-descriptor primitive-cast-pointer-as-raw;
// define &primitive-descriptor primitive-cast-raw-as-pointer;
// 
// // Machine-words
// define &primitive-descriptor primitive-integer?;
// define &primitive-descriptor primitive-machine-word-equal?;
// define &primitive-descriptor primitive-machine-word-not-equal?;
// define &primitive-descriptor primitive-machine-word-less-than?;
// define &primitive-descriptor primitive-machine-word-less-than?;
// define &primitive-descriptor primitive-machine-word-greater-than?;
// define &primitive-descriptor primitive-machine-word-not-greater-than?;
// define &primitive-descriptor primitive-wrap-machine-word;
// define &primitive-descriptor primitive-unwrap-machine-word;
// define &primitive-descriptor primitive-box-integer;
// define &primitive-descriptor primitive-unbox-integer;
// define &primitive-descriptor primitive-cast-integer-as-raw;
// define &primitive-descriptor primitive-cast-raw-as-integer;
// define &primitive-descriptor primitive-wrap-abstract-integer;
// define &primitive-descriptor primitive-wrap-unsigned-abstract-integer;
// define &primitive-descriptor primitive-unwrap-abstract-integer;
// define &primitive-descriptor primitive-machine-word-boole;
// define &primitive-descriptor primitive-machine-word-logand;
// define &primitive-descriptor primitive-machine-word-logior;
// define &primitive-descriptor primitive-machine-word-logxor;
// define &primitive-descriptor primitive-machine-word-lognot;
// define &primitive-descriptor primitive-machine-word-logbit?;
// define &primitive-descriptor primitive-machine-word-bit-field-extract;
// define &primitive-descriptor primitive-machine-word-bit-field-deposit;
// define &primitive-descriptor primitive-machine-word-count-low-zeros;
// define &primitive-descriptor primitive-machine-word-count-high-zeros;
// define &primitive-descriptor primitive-machine-word-add;
// define &primitive-descriptor primitive-machine-word-add-with-overflow;
// define &primitive-descriptor primitive-machine-word-subtract;
// define &primitive-descriptor primitive-machine-word-subtract-with-overflow;
// define &primitive-descriptor primitive-machine-word-multiply-low;
// define &primitive-descriptor primitive-machine-word-multiply-high;
// define &primitive-descriptor primitive-machine-word-multiply-low/high;
// define &primitive-descriptor primitive-machine-word-multiply-low-with-overflow;
// define &primitive-descriptor primitive-machine-word-multiply-with-overflow;
// define &primitive-descriptor primitive-machine-word-negative;
// define &primitive-descriptor primitive-machine-word-negative-with-overflow;
// define &primitive-descriptor primitive-machine-word-abs;
// define &primitive-descriptor primitive-machine-word-abs-with-overflow;
// define &primitive-descriptor primitive-machine-word-floor/-quotient;
// define &primitive-descriptor primitive-machine-word-floor/-remainder;
// define &primitive-descriptor primitive-machine-word-floor/;
// define &primitive-descriptor primitive-machine-word-ceiling/-quotient;
// define &primitive-descriptor primitive-machine-word-ceiling/-remainder;
// define &primitive-descriptor primitive-machine-word-ceiling/;
// define &primitive-descriptor primitive-machine-word-round/-quotient;
// define &primitive-descriptor primitive-machine-word-round/-remainder;
// define &primitive-descriptor primitive-machine-word-round/;
// define &primitive-descriptor primitive-machine-word-truncate/-quotient;
// define &primitive-descriptor primitive-machine-word-truncate/-remainder;
// define &primitive-descriptor primitive-machine-word-truncate/;
// define &primitive-descriptor primitive-machine-word-divide-quotient;
// define &primitive-descriptor primitive-machine-word-divide-remainder;
// define &primitive-descriptor primitive-machine-word-divide;
// define &primitive-descriptor primitive-machine-word-shift-left-low;
// define &primitive-descriptor primitive-machine-word-shift-left-high;
// define &primitive-descriptor primitive-machine-word-shift-left-low/high;
// define &primitive-descriptor primitive-machine-word-shift-left-low-with-overflow;
// define &primitive-descriptor primitive-machine-word-shift-left-with-overflow;
// define &primitive-descriptor primitive-machine-word-shift-right;
// define &primitive-descriptor primitive-machine-word-add-signal-overflow;
// define &primitive-descriptor primitive-machine-word-subtract-signal-overflow;
// define &primitive-descriptor primitive-machine-word-multiply-signal-overflow;
// define &primitive-descriptor primitive-machine-word-negative-signal-overflow;
// define &primitive-descriptor primitive-machine-word-abs-signal-overflow;
// define &primitive-descriptor primitive-machine-word-shift-left-signal-overflow;
// define &primitive-descriptor primitive-machine-word-double-floor/-quotient;
// define &primitive-descriptor primitive-machine-word-double-floor/-remainder;
// define &primitive-descriptor primitive-machine-word-double-floor/;
// define &primitive-descriptor primitive-machine-word-double-ceiling/-quotient;
// define &primitive-descriptor primitive-machine-word-double-ceiling/-remainder;
// define &primitive-descriptor primitive-machine-word-double-ceiling/;
// define &primitive-descriptor primitive-machine-word-double-round/-quotient;
// define &primitive-descriptor primitive-machine-word-double-round/-remainder;
// define &primitive-descriptor primitive-machine-word-double-round/;
// define &primitive-descriptor primitive-machine-word-double-truncate/-quotient;
// define &primitive-descriptor primitive-machine-word-double-truncate/-remainder;
// define &primitive-descriptor primitive-machine-word-double-truncate/;
// define &primitive-descriptor primitive-machine-word-double-divide-quotient;
// define &primitive-descriptor primitive-machine-word-double-divide-remainder;
// define &primitive-descriptor primitive-machine-word-double-divide;
// define &primitive-descriptor primitive-machine-word-unsigned-less-than?;
// define &primitive-descriptor primitive-machine-word-unsigned-not-less-than?;
// define &primitive-descriptor primitive-machine-word-unsigned-greater-than?;
// define &primitive-descriptor primitive-machine-word-unsigned-not-greater-than?;
// define &primitive-descriptor primitive-machine-word-unsigned-add-with-carry;
// define &primitive-descriptor primitive-machine-word-unsigned-subtract-with-borrow;
// define &primitive-descriptor primitive-machine-word-unsigned-multiply-high;
// define &primitive-descriptor primitive-machine-word-unsigned-multiply;
// define &primitive-descriptor primitive-machine-word-unsigned-divide-quotient;
// define &primitive-descriptor primitive-machine-word-unsigned-divide-remainder;
// define &primitive-descriptor primitive-machine-word-unsigned-divide;
// define &primitive-descriptor primitive-machine-word-unsigned-rotate-left;
// define &primitive-descriptor primitive-machine-word-unsigned-rotate-right;
// define &primitive-descriptor primitive-machine-word-unsigned-shift-right;
// define &primitive-descriptor primitive-machine-word-unsigned-double-divide-quotient;
// define &primitive-descriptor primitive-machine-word-unsigned-double-divide-remainder;
// define &primitive-descriptor primitive-machine-word-unsigned-double-divide;
// define &primitive-descriptor primitive-machine-word-unsigned-shift-left-high;
// define &primitive-descriptor primitive-machine-word-unsigned-double-shift-left-high;
// define &primitive-descriptor primitive-machine-word-unsigned-double-shift-left;
// define &primitive-descriptor primitive-machine-word-unsigned-double-shift-right-low;
// define &primitive-descriptor primitive-machine-word-unsigned-double-shift-right-high;
// define &primitive-descriptor primitive-machine-word-unsigned-double-shift-right;
// 
// // single-float
// define &primitive-descriptor primitive-single-float-as-raw;
// define &primitive-descriptor primitive-raw-as-single-float;
// define &primitive-descriptor primitive-single-float-as-integer;
// define &primitive-descriptor primitive-integer-as-single-float;
// define &primitive-descriptor primitive-single-float-as-double-integer;
// define &primitive-descriptor primitive-double-integer-as-single-float;
// define &primitive-descriptor primitive-cast-single-float-as-machine-word;
// define &primitive-descriptor primitive-cast-machine-word-as-single-float;
// define &primitive-descriptor primitive-single-float-negate;
// define &primitive-descriptor primitive-single-float-add;
// define &primitive-descriptor primitive-single-float-subtract;
// define &primitive-descriptor primitive-single-float-multiply;
// define &primitive-descriptor primitive-single-float-divide;
// define &primitive-descriptor primitive-single-float-equals?;
// define &primitive-descriptor primitive-single-float-less-than?;
// define &primitive-descriptor primitive-single-float-sqrt;
// define &primitive-descriptor primitive-single-float-log;
// define &primitive-descriptor primitive-single-float-exp;
// define &primitive-descriptor primitive-single-float-expt;
// define &primitive-descriptor primitive-single-float-sin;
// define &primitive-descriptor primitive-single-float-cos;
// define &primitive-descriptor primitive-single-float-tan;
// define &primitive-descriptor primitive-single-float-asin;
// define &primitive-descriptor primitive-single-float-acos;
// define &primitive-descriptor primitive-single-float-atan;
// 
// // double-float
// define &primitive-descriptor primitive-double-float-as-raw;
// define &primitive-descriptor primitive-raw-as-double-float;
// define &primitive-descriptor primitive-double-float-as-integer;
// define &primitive-descriptor primitive-integer-as-double-float;
// define &primitive-descriptor primitive-double-float-as-double-integer;
// define &primitive-descriptor primitive-double-integer-as-double-float;
// define &primitive-descriptor primitive-cast-double-float-as-machine-words;
// define &primitive-descriptor primitive-cast-machine-words-as-double-float;
// define &primitive-descriptor primitive-double-float-negate;
// define &primitive-descriptor primitive-double-float-add;
// define &primitive-descriptor primitive-double-float-subtract;
// define &primitive-descriptor primitive-double-float-multiply;
// define &primitive-descriptor primitive-double-float-divide;
// define &primitive-descriptor primitive-double-float-equals?;
// define &primitive-descriptor primitive-double-float-less-than?;
// define &primitive-descriptor primitive-double-float-sqrt;
// define &primitive-descriptor primitive-double-float-log;
// define &primitive-descriptor primitive-double-float-exp;
// define &primitive-descriptor primitive-double-float-expt;
// define &primitive-descriptor primitive-double-float-sin;
// define &primitive-descriptor primitive-double-float-cos;
// define &primitive-descriptor primitive-double-float-tan;
// define &primitive-descriptor primitive-double-float-asin;
// define &primitive-descriptor primitive-double-float-acos;
// define &primitive-descriptor primitive-double-float-atan;
// 
// // Float conversions
// define &primitive-descriptor primitive-single-float-as-double;
// define &primitive-descriptor primitive-double-float-as-single;
// 
// // Checks
// define &primitive-descriptor primitive-instance?;
// define &primitive-descriptor primitive-type-check;
// define &primitive-descriptor primitive-range-check;
// 
// // Comparisons.
// define &primitive-descriptor primitive-raw-as-boolean;
// define &primitive-descriptor primitive-boolean-as-raw;
// define &primitive-descriptor primitive-as-boolean;
// define &primitive-descriptor primitive-not;
// define &primitive-descriptor primitive-id?;
// define &primitive-descriptor primitive-not-id?;
// 
// // Repeated Slots.
// define &primitive-descriptor primitive-repeated-slot-as-raw;
// define &primitive-descriptor primitive-repeated-slot-offset;
// 
// // Vector.
// define &primitive-descriptor primitive-vector;
// define &primitive-descriptor primitive-copy-vector;
// define &primitive-descriptor primitive-vector-element;
// define &primitive-descriptor primitive-vector-element-setter;
// define &primitive-descriptor primitive-vector-size;
// define &primitive-descriptor primitive-vector-as-raw;
// define &primitive-descriptor primitive-raw-as-vector;
// 
// // String.
// define &primitive-descriptor primitive-strlen;
// define &primitive-descriptor primitive-string-as-raw;
// define &primitive-descriptor primitive-raw-as-string;
// 
// // Instance.
// define &primitive-descriptor primitive-object-class;
// define &primitive-descriptor primitive-slot-value;
// define &primitive-descriptor primitive-initialized-slot-value;
// define &primitive-descriptor primitive-slot-value-setter;
// 
// // Calling Convention.
// define &primitive-descriptor primitive-function-parameter;
// define &primitive-descriptor primitive-next-methods-parameter;
// define &primitive-descriptor primitive-set-generic-function-entrypoints;
// define &primitive-descriptor primitive-set-accessor-method-xep;
// 
// // Apply.
// define &primitive-descriptor primitive-xep-apply;
// define &primitive-descriptor primitive-mep-apply;
// define &primitive-descriptor primitive-mep-apply-with-optionals;
// define &primitive-descriptor primitive-engine-node-apply-with-optionals;
// define &primitive-descriptor primitive-iep-apply;
// define &primitive-descriptor primitive-apply;
// 
// // Discriminator/Engine-node Initialization
// define &primitive-descriptor primitive-initialize-engine-node;
// define &primitive-descriptor primitive-initialize-discriminator;
// 
// // Multiple-Values.
// define &primitive-descriptor primitive-values;
// 
// // Symbol boot.
// define &primitive-descriptor primitive-resolve-symbol;
// define &primitive-descriptor primitive-string-as-symbol;
// define &primitive-descriptor primitive-preboot-symbols;
// 
// // Terminal
// 
// // Operating System
// define &primitive-descriptor primitive-exit-application;
// define &primitive-descriptor primitive-start-timer;
// define &primitive-descriptor primitive-stop-timer;
// 
// // Float
// 
// define macro at-primitive-definer
//   {define at-primitive ?:name }
//     => { define &primitive-descriptor "primitive-" ## ?name ## "-at";
//          define &primitive-descriptor "primitive-" ## ?name ## "-at-setter"
//        }
// end;
// 
// define at-primitive single-float;
// define at-primitive double-float;
// define at-primitive extended-float;
// 
// // C ffi types
// 
// define at-primitive c-unsigned-char;
// define at-primitive c-signed-char;
// define at-primitive c-char;
// define at-primitive c-unsigned-short;
// define at-primitive c-signed-short;
// define at-primitive c-unsigned-long;
// define at-primitive c-signed-long;
// define at-primitive c-unsigned-int;
// define at-primitive c-signed-int;
// define at-primitive c-float;
// define at-primitive c-double;
// define at-primitive c-long-double;
// define at-primitive c-pointer;
// 
// define macro c-primitive-field-definer
//   {define c-primitive-field ?:name }
//     => { define &primitive-descriptor "primitive-" ## ?name ## "-field";
//          define &primitive-descriptor
//              "primitive-" ## ?name ## "-field-setter"
//        }
// end;
// 
// define c-primitive-field c-unsigned;
// define c-primitive-field c-signed;
// define c-primitive-field c-int;
// 
// define &primitive-descriptor primitive-unwrap-c-pointer;
// define &primitive-descriptor primitive-wrap-c-pointer;
