Module:   dfmc-modeling
Synopsis: Model object definitions
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define &library dylan
  export dylan;
  export dylan-excluding-arithmetic;
  export dylan-arithmetic;
  export dylan-primitives;
  export dylan-internal;
  export dylan-extensions;
  export dylan-hygiene-glitches;
  export dylan-c-ffi;
  export dylan-direct-c-ffi;
  export dylan-incremental;
  export threads;
  export threads-extensions;
  export threads-primitives;
  export finalization;
  export dispatch-engine;
  export machine-word-lowlevel;
  export simple-debugging;
end &library;

define &module dylan-primitives

  // the <top> type, so the compiler can handle raw types cleanly
  create
    <top>;

  create
    <primitive>;

  // dylan raw types
  create
    <raw-object>,
    <raw-boolean>,
    <raw-byte-character>,
    <raw-byte>,
    <raw-double-byte>,
    <raw-integer>,
    <raw-pointer>,
    <raw-address>,
    <raw-byte-string>,
    <raw-machine-word>,
    // <raw-double-integer>,
    <raw-single-float>,
    <raw-double-float>
    // <raw-extended-float>,
    ;

  // support
  create
    primitive-break,
    primitive-invoke-debugger,
    primitive-inside-debugger?,
    primitive-debug-message;

  // machine
  create
    primitive-word-size,
    primitive-header-size;

  // Object Representation.
  create
    primitive-object-class,
    primitive-slot-value,
    primitive-initialized-slot-value,
    primitive-slot-value-setter,
    primitive-repeated-slot-value,
    primitive-repeated-slot-value-setter;
    
  // Allocation.
  create
    primitive-allocate,
    primitive-allocate-wrapper,
    primitive-byte-allocate,
    primitive-object-allocate-filled,
    primitive-byte-allocate-filled,
    primitive-byte-allocate-filled-terminated,
    primitive-byte-allocate-leaf-filled,
    primitive-byte-allocate-leaf-filled-terminated,
    primitive-double-byte-allocate-filled,
    primitive-word-allocate-filled,
    primitive-double-word-allocate-filled,
    primitive-single-float-allocate-filled,
    primitive-double-float-allocate-filled,
    primitive-allocate-in-awl-pool,
    primitive-allocate-weak-in-awl-pool,
    primitive-untraced-allocate,
    primitive-manual-allocate,
    primitive-manual-free,
    primitive-set-class-breakpoint,
    primitive-clear-class-breakpoint,
    primitive-display-class-breakpoints;

  // Accessors
  create
    primitive-element,
    primitive-element-setter,
    primitive-byte-element,
    primitive-byte-element-setter,
    primitive-bit-element,
    primitive-bit-element-setter,
    primitive-bit-field,
    primitive-bit-field-setter,
    primitive-fill!,
    primitive-fill-bytes!,
    primitive-replace!,
    primitive-replace-bytes!;

  // GC
  create
    primitive-pin-object,
    primitive-unpin-object,
    primitive-mps-finalize,
    primitive-mps-finalization-queue-first,
    primitive-mps-park,
    primitive-mps-clamp,
    primitive-mps-release,
    primitive-mps-collect,
    primitive-mps-collection-stats,
    primitive-mps-enable-gc-messages,
    primitive-mps-committed,
    primitive-mps-begin-ramp-alloc,
    primitive-mps-end-ramp-alloc,
    primitive-mps-begin-ramp-alloc-all,
    primitive-mps-end-ramp-alloc-all,
    primitive-mps-ld-reset,
    primitive-mps-ld-add,
    primitive-mps-ld-merge,
    primitive-mps-ld-isstale,
    primitive-allocation-count,
    primitive-initialize-allocation-count,
    primitive-begin-heap-alloc-stats,
    primitive-end-heap-alloc-stats;

  // Support for keyboard-break handling
  create
    primitive-keyboard-interrupt-signaled,
    primitive-keyboard-interrupt-signaled-setter,
    primitive-keyboard-interrupt-polling,
    primitive-keyboard-interrupt-polling-setter,
    primitive-keyboard-interrupt-polling-thread,
    primitive-keyboard-interrupt-polling-thread-setter;

  // DLL Support
  create
    primitive-runtime-module-handle;

  // Unicode Character.
  create
    primitive-unicode-character-as-raw,
    primitive-raw-as-unicode-character;

  // Byte Character.
  create
    primitive-byte-character-as-raw,
    primitive-raw-as-byte-character;

  // Integer.

  // Small integer.
    
  // Big Integer.

  // Machine Integer.

  // Unsigned-Machine Integer.

  // Single Float.
  create
    primitive-single-float-as-raw,
    primitive-raw-as-single-float,
    // primitive-single-float-at,
    // primitive-single-float-at-setter,
    primitive-single-float-as-integer,
    primitive-integer-as-single-float,
    primitive-single-float-as-double-integer,
    primitive-double-integer-as-single-float,
    primitive-cast-single-float-as-machine-word,
    primitive-cast-machine-word-as-single-float,
    primitive-single-float-negate,
    primitive-single-float-add,
    primitive-single-float-subtract,
    primitive-single-float-multiply,
    primitive-single-float-divide,
    primitive-single-float-equals?,
    primitive-single-float-less-than?,
    primitive-single-float-sqrt,
    primitive-single-float-log,
    primitive-single-float-exp,
    primitive-single-float-expt,
    primitive-single-float-sin,
    primitive-single-float-cos,
    primitive-single-float-tan,
    primitive-single-float-asin,
    primitive-single-float-acos,
    primitive-single-float-atan;

  // Double Float.
  create
    primitive-double-float-as-raw,
    primitive-raw-as-double-float,
    // primitive-double-float-at,
    // primitive-double-float-at-setter,
    primitive-integer-as-double-float,
    primitive-double-float-as-integer,
    primitive-double-integer-as-double-float,
    primitive-double-float-as-double-integer,
    primitive-cast-double-float-as-machine-words,
    primitive-cast-machine-words-as-double-float,
    primitive-double-float-negate,
    primitive-double-float-add,
    primitive-double-float-subtract,
    primitive-double-float-multiply,
    primitive-double-float-divide,
    primitive-double-float-equals?,
    primitive-double-float-less-than?,
    primitive-double-float-sqrt,
    primitive-double-float-log,
    primitive-double-float-exp,
    primitive-double-float-expt,
    primitive-double-float-sin,
    primitive-double-float-cos,
    primitive-double-float-tan,
    primitive-double-float-asin,
    primitive-double-float-acos,
    primitive-double-float-atan;

  // Extended Float.
//  create
//    primitive-extended-float-as-raw,
//    primitive-raw-as-extended-float,
//    primitive-extended-float-at,
//    primitive-extended-float-at-setter,
//    primitive-integer-as-extended-float,
//    primitive-extended-float-as-integer,
//    primitive-double-integer-as-extended-float,
//    primitive-extended-float-as-double-integer,
//    primitive-quad-integer-as-extended-float,
//    primitive-extended-float-as-quad-integer,
//    primitive-cast-extended-float-as-machine-words,
//    primitive-cast-machine-words-as-extended-float,
//    primitive-extended-float-negate,
//    primitive-extended-float-add,
//    primitive-extended-float-subtract,
//    primitive-extended-float-multiply,
//    primitive-extended-float-divide,
//    primitive-extended-float-equals?,
//    primitive-extended-float-less-than?,
//    primitive-extended-float-sqrt,
//    primitive-extended-float-log,
//    primitive-extended-float-exp,
//    primitive-extended-float-expt,
//    primitive-extended-float-sin,
//    primitive-extended-float-cos,
//    primitive-extended-float-tan,
//    primitive-extended-float-asin,
//    primitive-extended-float-acos,
//    primitive-extended-float-atan;

  // Float conversions
  create
    primitive-single-float-as-double,
    primitive-double-float-as-single;
//    primitive-single-float-as-extended,
//    primitive-double-float-as-extended,
//    primitive-extended-float-as-double,
//    primitive-extended-float-as-single;

  create
    primitive-single-float-class,
    primitive-double-float-class;

  // Address.

  // Pointer.
  create
    primitive-cast-pointer-as-raw,
    primitive-cast-raw-as-pointer;

  // Machine-words.
  create
    primitive-integer?,
    primitive-machine-word-equal?,
    primitive-machine-word-not-equal?,
    primitive-machine-word-less-than?,
    primitive-machine-word-not-less-than?,
    primitive-machine-word-greater-than?,
    primitive-machine-word-not-greater-than?,
    primitive-wrap-machine-word,
    primitive-unwrap-machine-word,
    primitive-box-integer,
    primitive-unbox-integer,
    primitive-cast-integer-as-raw,
    primitive-cast-raw-as-integer,
    primitive-wrap-abstract-integer,
    primitive-wrap-unsigned-abstract-integer,
    primitive-unwrap-abstract-integer,
    primitive-machine-word-boole,
    primitive-machine-word-logand,
    primitive-machine-word-logior,
    primitive-machine-word-logxor,
    primitive-machine-word-lognot,
    primitive-machine-word-logbit?,
    primitive-machine-word-logbit-set,
    primitive-machine-word-logbit-clear,
    primitive-machine-word-bit-field-extract,
    primitive-machine-word-bit-field-deposit,
    primitive-machine-word-count-low-zeros,
    primitive-machine-word-count-high-zeros,
    primitive-machine-word-add,
    primitive-machine-word-add-with-overflow,
    primitive-machine-word-subtract,
    primitive-machine-word-subtract-with-overflow,
    primitive-machine-word-multiply-low,
    primitive-machine-word-multiply-high,
    primitive-machine-word-multiply-low/high,
    primitive-machine-word-multiply-low-with-overflow,
    primitive-machine-word-multiply-with-overflow,
    primitive-machine-word-negative,
    primitive-machine-word-negative-with-overflow,
    primitive-machine-word-abs,
    primitive-machine-word-abs-with-overflow,
    primitive-machine-word-floor/-quotient,
    primitive-machine-word-floor/-remainder,
    primitive-machine-word-floor/,
    primitive-machine-word-ceiling/-quotient,
    primitive-machine-word-ceiling/-remainder,
    primitive-machine-word-ceiling/,
    primitive-machine-word-round/-quotient,
    primitive-machine-word-round/-remainder,
    primitive-machine-word-round/,
    primitive-machine-word-truncate/-quotient,
    primitive-machine-word-truncate/-remainder,
    primitive-machine-word-truncate/,
    primitive-machine-word-divide-quotient,
    primitive-machine-word-divide-remainder,
    primitive-machine-word-divide,
    primitive-machine-word-shift-left-low,
    primitive-machine-word-shift-left-high,
    primitive-machine-word-shift-left-low/high,
    primitive-machine-word-shift-left-low-with-overflow,
    primitive-machine-word-shift-left-with-overflow,
    primitive-machine-word-shift-right,
    primitive-machine-word-add-signal-overflow,
    primitive-machine-word-subtract-signal-overflow,
    primitive-machine-word-multiply-signal-overflow,
    primitive-machine-word-negative-signal-overflow,
    primitive-machine-word-abs-signal-overflow,
    primitive-machine-word-shift-left-signal-overflow,
    primitive-machine-word-double-floor/-quotient,
    primitive-machine-word-double-floor/-remainder,
    primitive-machine-word-double-floor/,
    primitive-machine-word-double-ceiling/-quotient,
    primitive-machine-word-double-ceiling/-remainder,
    primitive-machine-word-double-ceiling/,
    primitive-machine-word-double-round/-quotient,
    primitive-machine-word-double-round/-remainder,
    primitive-machine-word-double-round/,
    primitive-machine-word-double-truncate/-quotient,
    primitive-machine-word-double-truncate/-remainder,
    primitive-machine-word-double-truncate/,
    primitive-machine-word-double-divide-quotient,
    primitive-machine-word-double-divide-remainder,
    primitive-machine-word-double-divide,
    primitive-machine-word-unsigned-less-than?,
    primitive-machine-word-unsigned-not-less-than?,
    primitive-machine-word-unsigned-greater-than?,
    primitive-machine-word-unsigned-not-greater-than?,
    primitive-machine-word-unsigned-add-with-carry,
    primitive-machine-word-unsigned-subtract-with-borrow,
    primitive-machine-word-unsigned-multiply-high,
    primitive-machine-word-unsigned-multiply,
    primitive-machine-word-unsigned-divide-quotient,
    primitive-machine-word-unsigned-divide-remainder,
    primitive-machine-word-unsigned-divide,
    primitive-machine-word-unsigned-rotate-left,
    primitive-machine-word-unsigned-rotate-right,
    primitive-machine-word-unsigned-shift-right,
    primitive-machine-word-unsigned-double-divide-quotient,
    primitive-machine-word-unsigned-double-divide-remainder,
    primitive-machine-word-unsigned-double-divide,
    primitive-machine-word-unsigned-shift-left-high,
    primitive-machine-word-unsigned-double-shift-left-high,
    primitive-machine-word-unsigned-double-shift-left,
    primitive-machine-word-unsigned-double-shift-right-low,
    primitive-machine-word-unsigned-double-shift-right-high,
    primitive-machine-word-unsigned-double-shift-right;

  // Checks.
  create
    primitive-instance?,
    primitive-type-check,
    primitive-range-check;

  // Comparisons.
  create
    primitive-raw-as-boolean,
    primitive-boolean-as-raw,
    primitive-as-boolean,
    primitive-not,
    primitive-id?,
    primitive-not-id?,
    primitive-compare-bytes,
    primitive-compare-words;

  // Repeated slots
  create
    primitive-repeated-slot-offset,
    primitive-repeated-slot-as-raw;

  // Vector.
  create
    primitive-copy-vector,
    primitive-vector,
    primitive-vector-element,
    primitive-vector-element-setter,
    primitive-vector-size,
    primitive-vector-as-raw,
    primitive-raw-as-vector;

  // String.
  create
    primitive-strlen,
    primitive-string-as-raw,
    primitive-raw-as-string;

  // Calling Convention.
  create
    primitive-function-parameter,
    primitive-lambda-parameter,
    primitive-next-methods-parameter,
    primitive-next-methods-parameter-setter;

  // Simple Streams and Streams

  // Operating System
  create
    primitive-run-application,
    primitive-exit-application,
    primitive-start-timer,
    primitive-stop-timer;

  // threads
  create
    primitive-make-thread,
    primitive-destroy-thread,
    primitive-thread-join-single,
    primitive-thread-join-multiple,
    primitive-thread-yield,
    primitive-current-thread,
    primitive-wait-for-simple-lock,
    primitive-wait-for-recursive-lock,
    primitive-wait-for-semaphore,
    primitive-wait-for-notification,
    primitive-wait-for-simple-lock-timed,
    primitive-wait-for-recursive-lock-timed,
    primitive-wait-for-semaphore-timed,
    primitive-wait-for-notification-timed,
    primitive-release-simple-lock,
    primitive-release-recursive-lock,
    primitive-release-semaphore,
    primitive-release-notification,
    primitive-release-all-notification,
    primitive-make-recursive-lock,
    primitive-destroy-recursive-lock,
    primitive-make-simple-lock,
    primitive-destroy-simple-lock,
    primitive-owned-simple-lock,
    primitive-owned-recursive-lock,
    primitive-make-semaphore,
    primitive-destroy-semaphore,
    primitive-make-notification,
    primitive-destroy-notification,
    primitive-sleep,
    primitive-allocate-thread-variable,
    primitive-read-thread-variable,
    primitive-write-thread-variable,
    primitive-initialize-current-thread,
    primitive-initialize-special-thread,
    primitive-detach-thread,
    primitive-unlock-simple-lock,
    primitive-unlock-recursive-lock,
    primitive-sequence-point,
    primitive-synchronize-side-effects;

  // Apply.
  create
    primitive-xep-apply,
    primitive-mep-apply,
    primitive-mep-apply-with-optionals,
    primitive-engine-node-apply-with-optionals,
    primitive-iep-apply,
    primitive-apply;

  // Discriminator/engine-node initialization
  create
    primitive-set-generic-function-entrypoints,
    primitive-initialize-engine-node,
    primitive-initialize-discriminator;

  create
    primitive-set-accessor-method-xep;

  // Multiple-Values.
  create
    primitive-values;

  // Symbol Booting
  create
    primitive-resolve-symbol,
    primitive-string-as-symbol,
    primitive-preboot-symbols;
      
  // Threads.
  create
    \%conditional-update-variable;

  // C FFI.
  create
    primitive-c-unsigned-field,
    primitive-c-signed-field,
    primitive-c-int-field,
    primitive-c-unsigned-field-setter,
    primitive-c-signed-field-setter,
    primitive-c-int-field-setter,

    primitive-unwrap-c-pointer,
    primitive-wrap-c-pointer,

    primitive-c-float-at,
    primitive-c-float-at-setter,
    primitive-c-double-at,
    primitive-c-double-at-setter,

    primitive-c-unsigned-long-at-setter,    
    primitive-c-signed-long-at-setter,    
    primitive-c-unsigned-short-at-setter,    
    primitive-c-signed-short-at-setter,    
    primitive-c-unsigned-char-at-setter,    
    primitive-c-signed-char-at-setter,
    primitive-c-signed-int-at-setter,
    primitive-c-unsigned-int-at-setter,
    primitive-c-unsigned-long-at,    
    primitive-c-signed-long-at,    
    primitive-c-unsigned-short-at,    
    primitive-c-signed-short-at,    
    primitive-c-unsigned-char-at,    
    primitive-c-signed-char-at,
    primitive-c-signed-int-at,
    primitive-c-unsigned-int-at,

    primitive-c-float-at,
    primitive-c-float-at-setter,
    primitive-c-double-at,
    primitive-c-double-at-setter,
    primitive-c-long-double-at,
    primitive-c-long-double-at-setter,

    primitive-c-unsigned-long-long-at,
    primitive-c-unsigned-long-long-at-setter,
    primitive-c-signed-long-long-at,
    primitive-c-signed-long-long-at-setter,

    primitive-c-pointer-at,
    primitive-c-pointer-at-setter;

  // C FFI raw types.
  create
    // <raw-c-char>,
    <raw-c-signed-char>,
    <raw-c-unsigned-char>,
    <raw-c-signed-short>,
    <raw-c-unsigned-short>,
    <raw-c-signed-int>,
    <raw-c-unsigned-int>,
    <raw-c-signed-long>,
    <raw-c-unsigned-long>,
    <raw-c-signed-long-long>,
    <raw-c-unsigned-long-long>,
    <raw-c-float>,
    <raw-c-double>,
    // <raw-c-long-double>,
    <raw-c-void>,
    <raw-c-pointer>;

end &module;


  // the <top> type, so the compiler can handle raw types cleanly

// TODO: For now, I'm keeping the glitch modules but not re-exporting them
// since hygiene should actually be working. I want to keep track of the
// names, though, since they probably do need to be marked.

define &module dylan-hygiene-glitches
  use dylan-primitives, export: all;
  create
    \%inlineable,
    \%let-handler,
    \%with-afterwards,
    \%with-cleanup,
    \%with-exit,
    \%with-exception,
    \%dynamic-extent,
    \%stack-vector,
    \%return-from,
    \%method-call,
    \%method-apply,
    \%guarantee-type,
    \%initialize-binding,
    \%signature,
    \generic-method,
    \constant-method,
    byte-slot-element, byte-slot-element-setter,
    \iterate-aux,

    <raw-type>,
    <top-type>,
    <virtual-class>;
end &module;


define &module simple-debugging
  create
    debugging?, debugging?-setter,
    debug-parts, debug-parts-setter,
    debugging-part?,
    debug-message,
    \debug-out,
    debug-out-function, debug-out-function-setter,
    \assert, \debug-assert;
end &module;

define &module dylan-extensions
  use simple-debugging,
    import: { \assert, \debug-assert },
    export: all;

  create
    <bottom>, <bottom-type>;

  create
    %current-library;

  create // not really an extension but support for fip set-by
    %curry-current-element-setter;

  create 
    *permissibly-ambiguous-generics*;

  create
    variable->class,
    class->variable;

  create
    version-checks?, version-checks?-setter;

  create
    address-of,
    as-object,
    <unicode-character>,
    <byte-character>;

  // Integers
  create
    <abstract-integer>,
    <big-integer>,
    generic-logior, generic-binary-logior,
    generic-logxor, generic-binary-logxor,
    generic-logand, generic-binary-logand,
    <double-integer>,
    %double-integer-low, %double-integer-low-setter,
    %double-integer-high, %double-integer-high-setter,
    double-integer-is-integer?,
    <machine-word>,
    $machine-word-size,
    $maximum-signed-machine-word,
    $minimum-signed-machine-word,
    $maximum-unsigned-machine-word,
    $minimum-unsigned-machine-word,
    bit-field-deposit, bit-field-extract, 
    logbit-deposit,
    as-unsigned,
    integer-as-raw, raw-as-integer,
    abstract-integer-as-raw, raw-as-abstract-integer,
    integer-divide-by-0;

  // Floats
  create
    // classify-float,
    decode-single-float, encode-single-float,
    decode-double-float, encode-double-float,
    float-divide-by-0, float-overflow, float-underflow;

  create
    immutable-vector,
    immutable-type-vector;

  create
    system-allocate-simple-instance, 
    system-allocate-repeated-instance,
    system-allocate-repeated-object-instance,
    system-allocate-repeated-byte-instance,
    system-allocate-repeated-byte-instance-terminated, 
    system-allocate-weak-repeated-instance,
    system-allocate-strong-repeated-instance,
    system-allocate-simple-instance-i, 
    system-allocate-repeated-object-instance-i,
    system-allocate-repeated-byte-instance-i,
    system-allocate-repeated-byte-instance-terminated-i, 
    system-allocate-weak-repeated-instance-i,
    system-allocate-strong-repeated-instance-i,
    system-allocate-wrapper;

  create
    \last-handler-definer,
    <format-string-condition>,
    <simple-condition>, // HACK: COMPATIBILITY
      <stack-overflow-error>,
      <arithmetic-error>,
        <division-by-zero-error>,
        <arithmetic-overflow-error>,
        <arithmetic-underflow-error>;

  create
    <bit>,
    <byte>,
    <double-byte>;

  create
    <object-with-elements>,
    <mutable-object-with-elements>;

  create
    <limited-collection>,
    element-type;

  create
    <limited-collection-type>,
      limited-collection-class,
      limited-collection-concrete-class,
      limited-collection-element-type,
      limited-collection-size,
      limited-collection-dimensions,
    <limited-explicit-key-collection-type>,
    <limited-mutable-collection-type>,
    <limited-stretchy-collection-type>,
    <limited-mutable-explicit-key-collection-type>,
    <limited-sequence-type>,
    <limited-mutable-sequence-type>,
    <limited-vector-type>,
    <limited-string-type>,
    <limited-deque-type>,
    <limited-array-type>,
    <limited-table-type>,
    <limited-set-type>,
    limited-collection-instance?;

  create
    <simple-element-type-vector>,
    <simple-byte-vector>,
    <simple-double-byte-vector>,
    <simple-integer-vector>,
    <simple-machine-word-vector>,
    <simple-single-float-vector>,
    <simple-double-float-vector>;

  create
    <simple-element-type-array>,
    <simple-byte-array>,
    <simple-double-byte-array>,
    <simple-integer-array>,
    <simple-machine-word-array>,
    <simple-single-float-array>,
    <simple-double-float-array>;

  create
    <stretchy-mutable-sequence>,
    <stretchy-element-type-vector>,
    <stretchy-byte-character-vector>;
    // <stretchy-unicode-character-vector>,
    // <stretchy-integer-vector>;

  create
    \limited-vector-definer,
    \limited-array-definer,
    \limited-stretchy-vector-definer,
    <limited-stretchy-vector>,
      <limited-stretchy-vector-representation>,
      stretchy-representation,
      stretchy-representation-setter,
      stretchy-representation-type,
      stretchy-vector-element,
      stretchy-vector-element-setter,
      collection-fill,
    limited-stretchy-vector,
    limited-array,
    limited-vector,
    empty;

  create
    \limited-list-definer,
    limited-list,
    <limited-list>,
      <non-empty-limited-list>,
      <empty-limited-list>,
    limited-list-first, limited-list-first-setter,
    limited-list-rest, limited-list-rest-setter,
    empty,
    prepend,
    rest;

  create
    \with-fip-of,
    identity-copy-state,
    sequence-next-state,
    sequence-previous-state,
    sequence-finished-state?,
    sequence-current-element-setter,
    sequence-current-key;

  create
    <stretchy-sequence>;

  create
    power-of-two-ceiling;

  create
    <not-found-error>;

  create
    <unbound>, unbound, unbound?;

  create
    <slot-descriptor>,
      slot-getter, 
      slot-setter,
      slot-type,
      init-keyword, init-keyword-setter,
    <repeated-slot-descriptor>,
      size-slot-descriptor,
    repeated-slot-descriptor,
      slot-value, slot-value-setter,
      repeated-slot-value, repeated-slot-value-setter;

  create
    allocate-instance;

  // Used by make methods.

  create
    make-method-init-function-value;

  create
    debug-name, debug-name-setter,
    singleton-object,
    <union>, union-type1, union-type2,
    <limited-integer>, limited-integer-min, limited-integer-max;

  /// TEMPORARILY FOR METHOD DISPATCH

  create
    \pointer-id?, 
    vector-element, vector-element-setter,
    concatenate-2;

  /// EXPORTED FOR FASTER BYTE-VECTOR AND STREAMS

  create
    element-range-check,
    element-range-error, \without-bounds-checks, check-start-compute-end,
    element-no-bounds-check, element-no-bounds-check-setter;

  /// PACKED-SLOTS

  export
    $end-count-<object>,
    packed-slots-end-count,
    pack-tristate,  unpack-tristate,
    pack-quadstate, unpack-quadstate,
    pack-boolean,   unpack-boolean,
    initialize-packed-slots,
    compute-initial-packed-slot,
    packed-slots-definer;
    
  // Namespaces
  create
    <namespace>, 
      namespace-name,
    <library-version>,
      library-major-version,
      library-minor-version,
      library-build-count,
    <library>,
      used-libraries,
      %library-version-check,
      library-defined-generics, library-defined-generics-setter,
      library-number-static-dispatches, library-number-static-dispatches-setter,
      library-number-dynamic-dispatches, library-number-dynamic-dispatches-setter,
    <used-library>,
      used-library,
      used-library-binding,
    <module>,
      home-library;

  create
    // variable to enable dispatch profiling
    *dispatch-profiling-enabled?*,
    *class-profiling-enabled?*;

  create
    runtime-module,
    lookup-runtime-module;

  create   // Memory Management functions
    <out-of-memory-condition>,
    signal-low-memory;

  create  // Support for keyboard-break handling
    <keyboard-interrupt>,
    keyboard-break-handler,
    keyboard-interrupt?,
    keyboard-interrupt?-setter,
    keyboard-interrupt-polling?,
    keyboard-interrupt-polling?-setter,
    keyboard-interrupt-polling-thread?,
    keyboard-interrupt-polling-thread?-setter;


  create  // used by dispatch-engine;  clarify interface later...
    // get-absent-engine-node,
    \tag-bits, \indirect-object-tag-bits?, \indirect-object?, \value-object?,
    \%mm-wrapper-implementation-class, \%mm-wrapper-implementation-class-setter,
    \instance-header, \instance-header-setter, 
    \indirect-object-mm-wrapper, \indirect-object-class, \direct-object-class-with-tag-bits,
    \indirect-object-implementation-class,
    \object-implementation-class,
    mm-wrapper-subtype-mask, mm-wrapper-subtype-mask-setter,
    \direct-object-class, \direct-object-mm-wrapper-with-tag-bits, \direct-object-mm-wrapper,
    object-mm-wrapper,
    %method-apply-with-optionals, reconstruct-args-from-mepargs,
    keyword-specifiers,
    <signature>, signature-required, signature-number-required, signature-number-values, 
    signature-key?, signature-all-keys?, signature-rest?, signature-rest-value?,
    signature-optionals?, signature-sealed-domain?,
    signature-complete?, signature-complete?-setter,
    <object-signature>,
    <keyword-signature>,
    function-next?,
    <simple-method>,
    // I'd like to put this here, but don't have the time to figure out if anything
    // will break since it's already exported from internal.
    // <simple-closure-method>, 
    <closure-method-mixin>,
      environment-element, environment-element-setter,
    <lambda>, <keyword-method>, <keyword-closure-method>, 
    <accessor-method>, <getter-accessor-method>, <setter-accessor-method>,
    <single-accessor-method>, <repeated-accessor-method>,
//    <incremental-method-mixin>, %incr-method-library, %incr-method-library-setter,
//    <incremental-simple-method>, <incremental-keyword-method>,
//    <incremental-closure-method>, <incremental-keyword-closure-method>,
//    <incremental-getter-method>, <incremental-setter-method>,
//    <incremental-repeated-getter-method>, <incremental-repeated-setter-method>,
    instance?-function,
    grounded-instance?, grounded-has-instances?, grounded-subtype?, unbound?, <limited-type>,
    merge-types, has-instances?, concrete-subtype?,
    disjoint-types?, disjoint-types-1?, incomplete-classes, type-complete?,
    debug-name-setter, function-signature, function-signature-setter, xep, xep-setter,
    mep, mep-setter, iep, iep-setter, %gf-cache, %gf-cache-setter, 
    generic-function-sealed?,
    generic-function-methods-setter,
    <domain>, <method-domain>, <standalone-domain>,
      domain-type, domain-number-required, domain-library, domain-match?, domain-types, domain-disjoint?,
    <sealed-generic-function>,
    <incremental-generic-function>, 
      incremental-gf-module, incremental-gf-module-setter,
      incremental-gf-domain-info, incremental-gf-domain-info-setter,
      incremental-gf-sealed?, incremental-gf-sealed?-setter,
      incremental-gf-method-complete?, incremental-gf-method-complete?-setter,
      incremental-gf-signatured?, incremental-gf-signatured?-setter,
      incremental-gf-method-libraries, incremental-gf-method-libraries-setter,
    call-to-type-incomplete-generic,
    decache-gf,
    initialized-slot-element, slot-element, slot-element-setter,
    // byte-slot-element, byte-slot-element-setter,
    <slot-descriptor>, <any-instance-slot-descriptor>, <any-class-slot-descriptor>,
    <repeated-slot-descriptor>, <instance-slot-descriptor>, <inherited-slot-descriptor>,
    <init-arg-descriptor>, <slot-keyword-initialization-descriptor>, 
    slot-storage-size,
    inherited-slot-getter, init-arg-type, init-keyword, init-keyword-required?, init-data-slot,
    method-slot-descriptor, slot-allocation, slot-offset, slot-offset-i, slot-owner, size-slot-descriptor,

    ambiguous-method-error, no-applicable-method-error,
    unbound-instance-slot, unbound-class-slot, unbound-repeated-slot,
    repeated-slot-getter-index-out-of-range-trap, repeated-slot-setter-index-out-of-range-trap,
    invalid-keyword-trap, odd-number-of-keyword-args-trap;

  create
    <obsolete-instance>,
    <miscellaneous-obsolete-instance>,
    <implementation-class>,
      iclass-class, iclass-class-setter,
      class-mm-wrapper, class-mm-wrapper-setter,
      repeated-slot-descriptor, repeated-slot-descriptor-setter,
      instance-slot-descriptors, instance-slot-descriptors-setter,
      iclass-dispatch-key, iclass-dispatch-key-setter, 
      direct-superclasses-setter,
      all-superclasses-setter,
      direct-subclasses-setter,
      direct-slot-descriptors, direct-slot-descriptors-setter,
      slot-descriptors, slot-descriptors-setter,
      direct-inherited-slot-descriptors, direct-inherited-slot-descriptors-setter,
      direct-initialization-argument-descriptors, direct-initialization-argument-descriptors-setter,
      direct-methods, direct-methods-setter,
      class-abstract?, class-abstract?-setter,
      class-primary?, class-primary?-setter,
      class-sealed?, class-sealed?-setter,
      iclass-type-complete?, iclass-type-complete?-setter,
      iclass-instantiable?, iclass-instantiable?-setter,
      class-complete?, class-complete?-setter,
      instance-storage-size, instance-storage-size-setter,
      class-slot-descriptors, class-slot-descriptors-setter,
      defaulted-initialization-arguments-slot, defaulted-initialization-arguments-slot-setter,
      class-slot-storage, class-slot-storage-setter,
      class-known-joint, class-known-joint-setter,
      class-rcpl-vector, class-rcpl-vector-setter,
      class-rcpl-position, class-rcpl-position-setter, 
      class-rcpl-other-positions, class-rcpl-other-positions-setter,
      repeated-slot-descriptor,
      // class-mangled-name,

      class-instance?-count, class-instance?-count-setter,
      get-class-instance?-counts,

    %register-subclass-dependent-generic,
    %register-subclasses-dependent-generic,
      iclass-subclass-dependent-generics, iclass-subclass-dependent-generics-setter, 
    %add-class,

    iclass-dependent-generics, iclass-dependent-generics-setter,
    iclass-subclasses-fixed?, iclass-subclasses-fixed?-setter,

    class-implementation-class, class-implementation-class-setter,
    class-subtype-bit, class-subtype-bit-setter,
    class-module, class-module-setter;

  create 
    \for-each-superclass;

  // You might think that these should be part of dylan-incremental...  But they're needed
  // for those "dumb" functions that only come in an scu version, such as has-instances?.
  // In practice they're rarely used on anything but the $empty-subjunctive-class-universe,
  // so much of the code is optimzed with that in mind.
  create 
    <subjunctive-class-universe>,
    $empty-subjunctive-class-universe,
    make-empty-subjunctive-class-universe,
    scu-entry?, scu-entry, scu-entry-setter, scu-converter,
    scu-initialize-all, scu-force-initialization,
    scu-do, all-iclass-superclasses;

  create
    $runtime-module, $runtime-library;

  create
    range-from, range-by;

  create
    ignore, ignorable,
    subclass, <subclass>, subclass-class,
    false-or, one-of, \iterate, \when;

  create
    <set>, <object-set>;

  create
    <object-deque>, <stretchy-object-vector>;

  create
    table-vector, 
    <table-vector>, 
      grow-size-function, default-grow-size, 
      hash-function, test-function,
    rehash-table, 
    <string-table>, <hash-state>, 
    collection-hash, sequence-hash, 
    values-hash, string-hash, case-insensitive-string-hash,
    case-insensitive-equal, remove-all-keys!;


  // ordered collections
  create
    <ordered-object-table>,
    <ordered-string-table>,
    <ordered-object-set>;

  // Alternate interface, allows substrings
  create
    make-symbol;

  // Special converters used in the compiler.

  create
    &definition-definer,
    &converter-definer,
    &macro-definer,
    \macro-case,
    \macro-template;

  // define more refined version

  create
    \copy-down-method-definer;

  // debugger

  create
    inside-debugger?;

  // Shut-down

  create
    register-application-exit-function,
    call-application-exit-functions;


  create
    <slot-type-error>,
      slot-type-error-slot-descriptor,
    <class-incomplete-error>,
      class-incomplete-error-class,
    <sealed-generic-function-error>,
      sealed-generic-function-error-generic,
      sealed-generic-function-error-operation,
      sealed-generic-function-error-arguments,
    <domain-sealed-generic-function-error>,
      sealed-generic-function-error-domain;

  create
    <ambiguous-methods>,
      ambiguous-methods-generic,
      ambiguous-methods-ordered,
      ambiguous-methods-ambiguous,
      ambiguous-methods-arguments;

  create
    <object-incomplete>,
      incomplete-object,
    <function-type-incomplete-error>,
      incomplete-types;

end &module;

define &module dylan-direct-c-ffi
  use dylan-primitives, export: all;
  use dylan-extensions,
    import: {<machine-word>,
	     integer-as-raw, raw-as-integer,
	     abstract-integer-as-raw, raw-as-abstract-integer},
    export: all;
  create
    \%call-c-function,
    \%call-c-function-indirect,
    \%c-callable-function,
    \%c-variable-pointer;
end &module;

define &module dylan-c-ffi-hygiene-glitches
  // all of these are due to macro hygiene failure and should be removed
  // when macro hygiene actually works
  use dylan-primitives, export: all;
  use dylan-direct-c-ffi, export: all;
  create
    \with-stack-block,
    \c-function-body,
    \make-c-callable,
    \export-type-for,
    \import-type-for,
    \export-type-for-reference,
    \import-type-for-reference,
    \install-pointer-type,
    \low-level-type-for,
    \export-function-body,
    \import-function-body,
    \pointer-value-body,
    \pointer-value-setter-body,
    \slot-accessor-body,
    \%c-struct-slot-offset,
    \%pointer-replace-bytes,
    \%c-dereference-bitfield,
    \%c-dereference-bitfield-setter,
    \boxer-for-designator,
    box-c-signed-char,
    box-c-unsigned-char,
    box-c-signed-short,
    box-c-unsigned-short;

  create
    check-designator-defined,
    check-import-type-defined,
    check-export-type-defined,
    check-c-address-designator,
    check-c-variable-designator;

  create
    abstract-pointer-type,
    concrete-pointer-type;

  create
    <raw-struct-type>,
    <raw-union-type>,
    <c-callable-function>,
    <c-statically-typed-function-pointer>,
    <c-pointer-type-class>;

  create
    <instantiation-of-c-void*>,
    <instantiation-of-c-raw-unsigned-char*>,
      <instantiation-of-c-raw-unsigned-char**>,
    <instantiation-of-c-raw-signed-char*>,
      <instantiation-of-c-raw-signed-char**>,
    <instantiation-of-c-raw-unsigned-short*>,
      <instantiation-of-c-raw-unsigned-short**>,
    <instantiation-of-c-raw-signed-short*>,
      <instantiation-of-c-raw-signed-short**>,
    <instantiation-of-c-raw-unsigned-int*>,
      <instantiation-of-c-raw-unsigned-int**>,
    <instantiation-of-c-raw-signed-int*>,
      <instantiation-of-c-raw-signed-int**>,
    <instantiation-of-c-raw-int*>,
      <instantiation-of-c-raw-int**>,
    <instantiation-of-c-raw-unsigned-long*>,
      <instantiation-of-c-raw-unsigned-long**>,
    <instantiation-of-c-raw-signed-long*>,
      <instantiation-of-c-raw-signed-long**>,
    <instantiation-of-c-float*>,
    <instantiation-of-c-double*>
    
//    \make-c-pointer-internal,
  ;
end &module;


define &module dylan-c-ffi
  use dylan-extensions,
    import: {<machine-word>},
    export: all;

/* 
[gts, 11/97, wait until harp backend ready]
  create
    \with-stack-structure;   // now handled by conversion (gts,9/97)
[gts, 11/97, wait until harp backend ready]
*/
  create
    c-pointer-type-definer,
    c-struct-definer,
    c-union-definer,
    c-mapped-subtype-definer,
    c-subtype-definer,
    c-function-definer,
    c-callable-wrapper-definer,
    c-address-definer,
    c-variable-definer;
  create
    referenced-type,
    size-of,
    alignment-of,
    // cooked-pointer-address,
    // cooked-pointer-address-setter,
    pointer-value,
    pointer-value-setter,
    pointer-value-address;
  create
    // HMMMM.
    simple-c-mapped-subtype-definer;
  create
    <C-raw-int>,
    <C-raw-signed-int>,
    <C-raw-unsigned-int>,
    <C-raw-signed-long>,
    <C-raw-unsigned-long>,
    <C-raw-signed-short>,
    <C-raw-unsigned-short>,
    <C-raw-char>,
    <C-raw-signed-char>,
    <C-raw-unsigned-char>,
    <C-raw-int*>,
    <C-raw-signed-int*>,
    <C-raw-unsigned-int*>,
    <C-raw-signed-long*>,
    <C-raw-unsigned-long*>,
    <C-raw-signed-short*>,
    <C-raw-unsigned-short*>,
    <C-raw-char*>,
    <C-raw-signed-char*>,
    <C-raw-unsigned-char*>,
    <C-raw-int**>,
    <C-raw-signed-int**>,
    <C-raw-unsigned-int**>,
    <C-raw-signed-long**>,
    <C-raw-unsigned-long**>,
    <C-raw-signed-short**>,
    <C-raw-unsigned-short**>,
    <C-raw-signed-char**>,
    <C-raw-unsigned-char**>,

    <C-number>,
    <C-struct>,
    <C-union>,
    <C-pointer>,
    <C-pointer-to-pointer>,
    <C-function-pointer>,
    <C-void>,
    <C-void*>,
    <C-untyped-pointer>,
    <C-statically-typed-pointer>,
    <C-value>,
    <C-float>,
    <C-float*>,
    <C-double>,
    <C-double*>;
    // <C-long-double>,
    // <C-long-double*>;

  // TODO: Names used directly by the c-ffi runtime library/streams , which
  // re-exports all the bindings from dylan-c-ffi. Should these escape
  // in a special library instead?
  export
    <designator-class>,
    \make-c-pointer,
    make-c-pointer-internal,
    concrete-class,
    concrete-class-setter,
    raw-pointer-address;

  // !@#$ temporarily export these directly
  //      until I get a chance to debug raw struct by value..
  export
    \raw-struct-type-definer,
    \raw-union-type-definer;

end &module;



/// Last checked: 19th Jan 96, against DRM Draft of September 29, 1995.
/// Modified: 27 Mar 97 to add function-definer, an approved new feature, by GMP.
/// Modified: 8 Apr 97 to rename <small-integer> => <integer>, by GMP.

define &module dylan

  create

    // Defining forms.

    // TODO: CORRECTNESS: Should we worry about this incompatibility?

    macro-definer, \macro,

    variable-definer,
    constant-definer,
    generic-definer,
    method-definer,
    class-definer,
    module-definer,
    library-definer,
    domain-definer,
    function-definer;

  create

    // Local declarations.
    // TODO: Make conformant with the spec - these should be core words.

    \let,
    \local;

  create

    // Statements.

    \if,
    \unless,
    \case,
    \select,
    \while,
    \until,
    \for,
    \begin,
    \block,
    \method;

  create

    // Special operators.

    \&,
    \|,
    \:=;

  create

    // Evaluation.

    values,
    \~,
    apply,

    // Objects.

    <object>,
    make,
    initialize,
    slot-initialized?,

    // Equality and Magnitute Comparisons.

    \==,
    \~==,
    \=,
    \~=,
    \<,
    \>,
    \<=,
    \>=,

    min,
    max;

  create

    // Functions.

    <function>,
    <method>,
    <generic-function>,
    add-method,
    generic-function-methods,
    generic-function-mandatory-keywords,
    function-specializers,
    function-arguments,
    function-return-values,
    applicable-method?,
    sorted-applicable-methods,
    find-method,
    remove-method;

  create

    /// Types and Classes.

    <type>,
    <class>,
    instance?,
    subtype?,

    object-class,
    as,
    shallow-copy,
    type-for-copy,
    all-superclasses,
    direct-superclasses,
    direct-subclasses,

    <singleton>,
    singleton,

    type-union,
    limited;

  create
       
    /// Collections.

    // Iteration protocol.

    forward-iteration-protocol,
    backward-iteration-protocol,

    key-test,
    key-sequence,
       
    element,
    element-setter,

    // Classes.

   <collection>,
   <mutable-collection>,
   <stretchy-collection>,

   size,
   size-setter,
   empty?,
   do,
   map,
   map-as,
   map-into,
   any?,
   every?,
   reduce,
   reduce1,
   member?,
   find-key,
   replace-elements!,
   fill!,

   <sequence>,
   <mutable-sequence>,

   add,
   add!,
   add-new,
   add-new!,
   remove,
   remove!,
   choose,
   choose-by,
   intersection,
   union,
   remove-duplicates,
   remove-duplicates!,
   copy-sequence,
   concatenate-as,
   concatenate,
   replace-subsequence!,
   reverse,
   reverse!,
   sort,
   sort!,
   first,
   second,
   third,
   last,
   first-setter,
   second-setter,
   third-setter,
   last-setter,
   subsequence-position,

   <explicit-key-collection>,
   <mutable-explicit-key-collection>,

   <array>,

   aref,
   aref-setter,
       
   rank,
   row-major-index,
   dimensions,
   dimension,

   <deque>,
       
   push,
   pop,
   push-last,
   pop-last,
   add!,
   remove!,
       
   <list>,
   <empty-list>,
   <pair>,

   pair,
   head,
   tail,
   head-setter,
   tail-setter,
   list,

   <range>,

   range,

   <string>,
   <byte-string>,
   <unicode-string>,

   as-lowercase,
   as-lowercase!,
   as-uppercase,
   as-uppercase!,

   <table>,
   <object-table>,

   remove-key!,

   table-protocol,
   merge-hash-ids,
   object-hash,

   <vector>,
   <stretchy-vector>,
   <simple-vector>,
   <simple-object-vector>,

   vector;

 create

   /// Conditions.

   <condition>,
   <serious-condition>,
   <error>,
   <simple-error>,
   <type-error>,
   <sealed-object-error>,
   <warning>,
   <simple-warning>,
   <restart>,
   <simple-restart>,
   <abort>,

   signal,
   error,
   cerror,
   break,
   check-type,
   abort,

   default-handler,
   restart-query,
   return-query,

   do-handlers,
   return-allowed?,
   return-description,

   condition-format-string,
   condition-format-arguments,

   type-error-value,
   type-error-expected-type;

  create

    /// Numbers.

   <number>,

   <complex>,
   <real>,
   <rational>,

   <float>,
   <single-float>,
   <double-float>,
   <extended-float>,

   <integer>,
   $minimum-integer, $maximum-integer,

   odd?,
   even?,
   zero?,
   positive?,
   negative?,
   integral?,

   \+,
   \*,
   \-,
   \/,
       
   negative,

   floor,
   ceiling,
   round,
   truncate,

   floor/,
   ceiling/,
   round/,
   truncate/,
       
   modulo,
   remainder,
       
   \^,

   abs,

   logior,
   logxor,
   logand,
   lognot,
   logbit?,
   ash,
   lsh,

   lcm,
   gcd;

 create

   /// Symbols.

   <symbol>,

   /// Characters.

   <character>,

   as-uppercase,
   as-lowercase,

   /// Functional Operators.

   compose,
   complement,
   disjoin,
   conjoin,
   curry,
   rcurry,
   always,

   /// Miscellaneous.

   <boolean>,

   identity;

end &module;


/// See the Integer proposal in Dylan Notebook\DylanWorks\Runtime\Integers\Integers ...

define &module dylan-excluding-arithmetic
  use dylan,
    exclude: {<integer>,
	      $minimum-integer, $maximum-integer,
	      range, <range>,
	      \+, \-, \*, \/,
	      negative,
	      floor,  ceiling,  round,  truncate,
	      floor/, ceiling/, round/, truncate/,
	      modulo, remainder,
	      \^,
	      abs,
	      logior, logxor, logand, lognot,
	      logbit?,
	      ash, lsh,
	      lcm, gcd,
	      \for},
    export: all;
  //---*** Anything from dylan-extensions?
end &module;


define &module dylan-arithmetic
  use dylan,
    import: {<integer>,
	     $minimum-integer, $maximum-integer,
	     range, <range>,
	     \+, \-, \*, \/,
	     negative,
	     floor,  ceiling,  round,  truncate,
	     floor/, ceiling/, round/, truncate/,
	     modulo, remainder,
	     \^,
	     abs,
	     logior, logxor, logand, lognot,
	     logbit?,
	     ash, lsh,
	     lcm, gcd,
	     \for},
    export: all;
  //---*** Anything from dylan-extensions?
end &module;


// Incremental entry points

define &module dylan-incremental

  create
    %define-class,
    %define-complex-class,
    %define-generic,
    %define-method,
    %define-sealed-method,
    %define-domain;

  create
    %add-a-method, %add-dynamic-method, %add-nonsiblinged-method,
    %add-method,
    remove-method-via-specializers, %remove-method-from-library, %remove-method;

  create
    %add-method-domain, %add-domains,
    %add-domain, %add-nonsiblinged-domain,
    %remove-domain;

  // Actually interactive.
  create
    %redefine-class,
    %redefine-complex-class,
    %redefine-generic;

  // Stuff for development outside the Dylan library.
  create
    reinitialize;

end &module;


define &module threads
  create

    // <Thread>
    <thread>, <synchronous-thread>,
    thread-name, 
    join-thread,
    thread-yield,
    current-thread,
    $low-priority,
    $background-priority,
    $normal-priority,
    $interactive-priority,
    $high-priority,

    // <Synchronization>
    <synchronization>,
    wait-for,
    release,
    synchronization-name,
    
    // <Lock>
    <lock>,
    \with-lock,

    // <Semaphore>
    <semaphore>,
    $semaphore-maximum-count-limit,
    
    // <Exclusive-lock>
    <exclusive-lock>,
    <recursive-lock>,
    <simple-lock>,
    <read-write-lock>,
    owned?,
    owned-for-reading?,

    // <Notification>
    <notification>,
    associated-lock,
    release-all,
    
    // Timers
    sleep,

    // dynamic binding
    \dynamic-bind,
    \%dynamic-bind-variable, // HACK: HYGIENE GLITCH

    // Conditions,
    <thread-error>,
    <thread-creation-error>,
    <thread-inactive-error>,
    <duplicate-join-error>,
    <count-exceeded-error>,
    <not-owned-error>,
    <timeout-expired>;

  // And the stuff that used to be in threads-extensions but has now come home:

  create

    // Low level synchronization
    synchronize-side-effects,
    sequence-point,

    // Conditional update
    \conditional-update!,
    \conditional-update-aux, // HACK: HYGIENE GLITCH
    \atomic-increment!,
    \atomic-decrement!,
    <conditional-update-error>

end &module;


define &module threads-extensions
  create

    // Conditional set variable
    \conditional-set-variable!
    
end &module;




define &module threads-primitives
  use dylan-primitives, export: all;
  use dylan;

  create
    make-simple-lock,
    make-notification,
    make-foreign-thread,
    internal-initialize-thread,
    *dylan-library-initialized?*;

  export
    <optional-name>,
    <portable-container>, handle1, handle1-setter,
    <portable-double-container>, handle2, handle2-setter,
    $success,
    $general-error,
    $timeout,
    $unlocked,
    $pre-locked,
    $count-exceeded,
    $creation-error,
    $priority-error,
    $false,
    $true;
end &module;


define &module finalization
  create
    finalize-when-unreachable,
    finalize,
    drain-finalization-queue,
    automatic-finalization-enabled?,
    automatic-finalization-enabled?-setter;
end &module;


define &module threads-internal
  use dylan;
  use dylan-primitives;
  use dylan-extensions;
  use finalization;
  use threads;
  use threads-extensions;
  use threads-primitives;
  use simple-debugging;
end &module;


define &module finalization-internal
  use dylan;
  use dylan-primitives;
  use dylan-extensions;
  use threads;
  use finalization;
  use simple-debugging;
end &module;


define &module dispatch-engine
  create			// @@@@ Temporary... 
      todays-dispatch-report,
//    *cached-gfs*,
    dbg;
  // HACK: TEMPORARY
  create
    profile-all-terminal-engine-nodes?, profile-all-terminal-engine-nodes?-setter,
    call-site-caches-enabled?, call-site-caches-enabled?-setter, 
    sharing-partial-dispatch-cache-headers?, sharing-partial-dispatch-cache-headers?-setter,
    partial-dispatch-megamorphic-punt?, partial-dispatch-megamorphic-punt?-setter,
    partial-dispatch?, partial-dispatch?-setter;

  create
    engine-node$k-absent,
    engine-node$k-inapplicable,
    engine-node$k-unkeyed-single-method,
    engine-node$k-implicit-keyed-single-method,
    engine-node$k-explicit-keyed-single-method,
    engine-node$k-unrestricted-keyed-single-method,
    engine-node$k-reserved-terminal-n-a,
    engine-node$k-reserved-terminal-n-b,
    engine-node$k-reserved-terminal-n-c,
    engine-node$k-reserved-terminal-n-d,
    engine-node$k-reserved-terminal-n-e,
    engine-node$k-reserved-terminal-n-f,
    engine-node$k-reserved-terminal-n-g,
    engine-node$k-profiling-cache-header,
    engine-node$k-cache-header,
    engine-node$k-ambiguous-methods,
    engine-node$k-first-slot-engine-node,
    engine-node$k-boxed-instance-slot-getter,
    engine-node$k-boxed-instance-slot-setter,
    engine-node$k-boxed-repeated-instance-slot-getter,
    engine-node$k-boxed-repeated-instance-slot-setter,
    engine-node$k-boxed-class-slot-getter,
    engine-node$k-boxed-class-slot-setter,
    engine-node$k-raw-byte-repeated-instance-slot-getter,
    engine-node$k-raw-byte-repeated-instance-slot-setter,
    engine-node$k-reserved-slot-a-getter,
    engine-node$k-reserved-slot-a-setter,
    engine-node$k-reserved-slot-b-getter,
    engine-node$k-reserved-slot-b-setter,
    engine-node$k-reserved-repeated-slot-a-getter,
    engine-node$k-reserved-repeated-slot-a-setter,
    engine-node$k-reserved-repeated-slot-b-getter,
    engine-node$k-reserved-repeated-slot-b-setter,
    engine-node$k-slot-engine-node-count,
    engine-node$k-typecheck,
    engine-node$k-if-type,
    engine-node$k-linear-by-class,
    engine-node$k-hashed-by-class,
    engine-node$k-linear-by-singleton-class,
    engine-node$k-hashed-by-singleton-class,
    engine-node$k-immediate-linear-singleton,
    engine-node$k-immediate-hashed-noreloc-singleton,
    engine-node$k-immediate-hashed-singleton,
    engine-node$k-value-object-linear-singleton,
    engine-node$k-monomorphic-by-class,
    engine-node$k-reserved-discriminator-a,
    engine-node$k-reserved-discriminator-b,
    engine-node$k-reserved-discriminator-c,
    engine-node$k-reserved-discriminator-d,
    engine-node$k-reserved-discriminator-e,
    engine-node$k-reserved-discriminator-f,
    engine-node$k-reserved-discriminator-g,
    engine-node$k-reserved-discriminator-h,
    engine-node$k-reserved-discriminator-i,
    engine-node$k-reserved-discriminator-j,
    engine-node$k-reserved-discriminator-k,
    engine-node$k-reserved-discriminator-l,
    engine-node$k-reserved-discriminator-m,
    engine-node$k-reserved-discriminator-n,
    engine-node$k-reserved-discriminator-o,
    engine-node$k-reserved-discriminator-p,
    engine-node$k-reserved-discriminator-q,
    engine-node$k-reserved-discriminator-r,
    engine-node$k-reserved-discriminator-s,
    engine-node$k-reserved-discriminator-t,
    engine-node$k-reserved-discriminator-u;
  create
    $inapplicable-engine-node,
    $absent-engine-node;
  create
    properties$m-entry-type,
    properties$s-entry-type,
    properties$v-entry-type,
    properties$v-data,
    engine-node$v-data-start,
    smen$v-nrequired,
    smen$s-nrequired,
    smen$m-nrequired,
    smen$v-restp,
    smen$m-restp,
    smen$v-data-start,
    $simple-typechecked-cache-arguments-limit,
    stchen$v-checkedmask,
    stchen$s-checkedmask,
    stchen$m-checkedmask,
    $partial-dispatch-arguments-limit,
    pdisp$v-typemask,
    pdisp$s-typemask,
    pdisp$m-typemask,
    discriminator$v-argnum,
    discriminator$s-argnum,
    discriminator$m-argnum,
    discriminator$v-nrequired,
    discriminator$s-nrequired,
    discriminator$m-nrequired,
    discriminator$v-restp,
    discriminator$m-restp,
    discriminator$v-data-start;
  create
    discriminator, discriminator-setter;        // This is the <generic-function> accessor.
  create
    <properties-provider>,
      properties, properties-setter;
  create
    <engine-node>,
      engine-node-callback, engine-node-callback-setter,
      engine-node-entry-point, engine-node-entry-point-setter,
      engine-node-raw-integer, engine-node-raw-integer-setter;
  create
    $dispatch-key-lock, *next-unique-dispatch-key*, *implementation-classes-by-key*,
    iclass-unique-key, class-unique-key,
    iclass-number-to-key, iclass-key-to-number,
    initialize-class-dispatch-keys, 
    initialize-class-dispatch-keys-vectored, 
    ensure-key-to-iclass-storage,
    object-class-unique-key, implementation-class-from-key;
  create
    %make-simple-vector, %load-byte, %method-specializer,
    %method-number-required, %gf-number-required,
    expand-mask, compress-mask,
    <dispatch-engine-invocable>,
    discriminator-argnum,
    *engine-node-callbacks*,
    engine-node-function-code,
    %invoke-engine-node,
    %invoke-generic-function, %invoke-generic-function-mepized,
    <dispatch-starter>,	// type-union(<generic-function>, <cache-header-engine-node>)
    parent-gf, %restart-dispatch;
  create
    \with-object-lock, \with-multiple-object-lock,
    bletch, bletch-stack,
    bootstrap-allocate-engine-node,
    bootstrap-typed-allocate-engine-node,
    bootstrap-allocate-and-initialize-engine-node,
    standard-discriminator-bits,
    $standard-discriminator-bit-mask,
    bootstrap-allocate-discriminator,
    make-single-class-singleton-discriminator,
    *dispatch-miss-count*, *dispatch-computation-count*,
    handle-missed-dispatch, handle-missed-dispatch-1;
  create
    <terminal-engine-node>,
    <discriminator>,
    <singular-terminal-engine-node>,
    <uniquified-terminal-engine-node>,
    <unshared-terminal-engine-node>,
    <absent-engine-node>,
      %gf-dispatch-absent,
    <inapplicable-engine-node>,
      %gf-dispatch-inapplicable,
    <ambiguous-methods-engine-node>,
      ambiguous-methods-engine-node-ambig, ambiguous-methods-engine-node-ambig-setter,
      ambiguous-methods-engine-node-ordered, ambiguous-methods-engine-node-ordered-setter,
      %gf-dispatch-ambiguous-methods,
      make-ambiguous-methods-engine-node,
      make-ambiguous-methods-next-method,
    <single-method-engine-node>,
    <unkeyed-single-method-engine-node>,
    <keyed-single-method-engine-node>,
    <explicit-keyed-single-method-engine-node>,
    <implicit-keyed-single-method-engine-node>,
    <unrestricted-keyed-single-method-engine-node>,
      single-method-engine-node-method, single-method-engine-node-method-setter,
      single-method-engine-node-data, single-method-engine-node-data-setter,
      single-method-engine-node-keys, single-method-engine-node-keys-setter,
      make-single-method-engine-node,
      make-linear-singleton-discriminator,
    <cache-header-engine-node>,
      cache-header-engine-node-next, cache-header-engine-node-next-setter, 
      cache-header-engine-node-parent, cache-header-engine-node-parent-setter,
    <common-root-cache-header-engine-node>,			    
    <simple-typechecked-cache-header-engine-node>,
      stchen-checkedmask,
    <partial-dispatch-cache-header-engine-node>,
    <simple-call-site-cache-header-engine-node>,
    <profiling-call-site-cache-header-engine-node>,
      profiling-call-site-cache-header-engine-node-count-1, profiling-call-site-cache-header-engine-node-count-1-setter,
      profiling-call-site-cache-header-engine-node-count-2, profiling-call-site-cache-header-engine-node-count-2-setter,
      %profile-count-low,  %profile-count-low-setter,     
      %profile-count-high, %profile-count-high-setter,     
      profiling-call-site-cache-header-engine-node-id, profiling-call-site-cache-header-engine-node-id-setter,
      profiling-call-site-cache-header-engine-node-library, profiling-call-site-cache-header-engine-node-library-setter,
    <class-keyed-discriminator>,
      class-keyed-discriminator-table-element, class-keyed-discriminator-table-element-setter,
      class-keyed-discriminator-table-size,
      class-keyed-discriminator-default, class-keyed-discriminator-default-setter,
      ckd$v-log2size,
      ckd$s-log2size,
      // <table-base>,
      // grounded-class-keyed-discriminator-table-base,
      ckd-ref, ckd-ref-setter, ckd-size,			    
      %ckd-ref, %ckd-ref-setter, %ckd-size, %ckd-mask, $ckd-empty,
      grounded-class-keyed-discriminator-default,
      %second-hash-values,
    <linear-class-keyed-discriminator>,
      lckd-index, lckd-index-setter,
      lckd-hits, lckd-hits-setter,
    <hashed-class-keyed-discriminator>,
      %hckd-count, %hckd-count-setter,
      %hckd-limit, %hckd-limit-setter,
    <by-class-discriminator>,
    <by-singleton-class-discriminator>,
    <linear-by-class-discriminator>,
    <hashed-by-class-discriminator>,
    <linear-by-singleton-class-discriminator>,
    <hashed-by-singleton-class-discriminator>,
    <typecheck-discriminator>,
      typecheck-discriminator-type, typecheck-discriminator-type-setter,
      typecheck-discriminator-next, typecheck-discriminator-next-setter,
      %gf-dispatch-typecheck,
      make-typecheck-discriminator,
    <monomorphic-by-class-discriminator>,
      monomorphic-by-class-discriminator-key,  monomorphic-by-class-discriminator-key-setter,
      monomorphic-by-class-discriminator-next, monomorphic-by-class-discriminator-next-setter,
      make-monomorphic-by-class-discriminator,
    <if-type-discriminator>,
      %gf-dispatch-if-type, make-if-type-discriminator,
      if-type-discriminator-then, if-type-discriminator-then-setter,
      if-type-discriminator-else, if-type-discriminator-else-setter,
      if-type-discriminator-type, if-type-discriminator-type-setter,
    <singleton-discriminator>,
      singleton-discriminator-element, singleton-discriminator-element-setter,
      singleton-discriminator-default, singleton-discriminator-default-setter,
      singleton-discriminator-table, singleton-discriminator-table-setter,
    <linear-singleton-discriminator>,
      linear-singleton-discriminator-element-setter,
      lsd-index, lsd-index-setter,
      lsd-hits,  lsd-hits-setter,
    <immediate-linear-singleton-discriminator>,
      immediate-linear-singleton-discriminator-element,
      %gf-dispatch-immediate-linear-singleton,
    <value-object-linear-singleton-discriminator>,
      value-object-linear-singleton-discriminator-element,
      %gf-dispatch-value-object-linear-singleton;
  create
    <slot-access-engine-node>,
    <slot-setter-engine-node>,
    <slot-getter-engine-node>,
    <single-slot-access-engine-node>,
    <repeated-slot-access-engine-node>,
    <instance-slot-engine-node>,
    <class-slot-engine-node>,
    <boxed-instance-slot-engine-node>,
    <byte-slot-engine-node>,
    <boxed-instance-slot-getter-engine-node>,
    <boxed-instance-slot-setter-engine-node>,
    <boxed-repeated-instance-slot-getter-engine-node>,
    <boxed-repeated-instance-slot-setter-engine-node>,
    <byte-slot-getter-engine-node>,
    <byte-slot-setter-engine-node>,
    <repeated-byte-slot-getter-engine-node>,
    <repeated-byte-slot-setter-engine-node>,
    <boxed-class-slot-engine-node>,
    <boxed-class-slot-getter-engine-node>,
    <boxed-class-slot-setter-engine-node>,
    callback-slot-engine-node-offset,			    
    slot-engine-node-size-offset, slot-engine-node-size-offset-setter;
  create
    <gf-cache-info>,
      gf-cache-info-users, gf-cache-info-users-setter,
    <simple-typechecked-gf-cache-info>,
      simple-typechecked-gf-cache-info-entries, simple-typechecked-gf-cache-info-entries-setter,
      simple-typechecked-gf-cache-info-argmask, simple-typechecked-gf-cache-info-argmask-setter,
    <partial-dispatch-gf-cache-info>,
      partial-dispatch-gf-cache-info-caches, partial-dispatch-gf-cache-info-caches-setter;
  create
    slot-location,
    make-slot-accessing-next-method-chain;
  create
    compute-sorted-applicable-methods,
    same-specializer?,
    same-specializers?;
  create
    sort-applicable-methods,
    sort-applicable-methods-desperately,
    *gracefully-dispatch-to-ambiguous-methods*,
    <ambiguous-methods-warning>,
    <ambiguous-methods-error>,
    same-specializers-spread?;
  create
    *gf-invalid-keyword-error-is-warning*;
end &module;


define &module machine-word-lowlevel
  // environment enquiry
  create word-size;
  // error signaling
  create machine-word-overflow;
  // conversion
  create interpret-machine-word-as-integer,
         interpret-integer-as-machine-word,
         coerce-machine-word-to-integer,
         coerce-integer-to-machine-word,
         coerce-machine-word-to-abstract-integer,
         coerce-abstract-integer-to-machine-word,
         coerce-machine-word-to-unsigned-abstract-integer,
	 strip-integer-tag,
	 insert-integer-tag,
	 force-integer-tag;
  // comparison
  create machine-word-equal?,
         machine-word-less-than?;
  // logical
  create machine-word-logior,
         machine-word-logxor,
         machine-word-logand,
         machine-word-lognot,
         machine-word-logbit?,
         machine-word-count-low-zeros,
         machine-word-count-high-zeros;
  // arithmetic
  create machine-word-add-with-overflow,
         machine-word-subtract-with-overflow,
         machine-word-multiply-with-overflow,
         machine-word-negative-with-overflow,
         machine-word-abs-with-overflow;
  // division
  create machine-word-floor/,
         machine-word-ceiling/,
         machine-word-round/,
         machine-word-truncate/,
         machine-word-divide;
  // shift
  create machine-word-shift-left-with-overflow,
         machine-word-shift-right;
  // unsigned
  create machine-word-unsigned-add-with-carry,
         machine-word-unsigned-subtract-with-borrow,
         machine-word-unsigned-multiply,
         machine-word-unsigned-divide,
         machine-word-unsigned-rotate-left,
         machine-word-unsigned-rotate-right,
         machine-word-unsigned-shift-left,
         machine-word-unsigned-shift-right,
         machine-word-unsigned-less-than?;
  // signal overflow
  create machine-word-add-signal-overflow,
         machine-word-subtract-signal-overflow,
         machine-word-multiply-signal-overflow,
         machine-word-negative-signal-overflow,
         machine-word-abs-signal-overflow,
         machine-word-shift-left-signal-overflow;
  // double division
  create machine-word-double-floor/,
         machine-word-double-ceiling/,
         machine-word-double-round/,
         machine-word-double-truncate/,
         machine-word-double-divide;
  // unsigned double
  create machine-word-unsigned-double-divide,
         machine-word-unsigned-double-shift-left,
         machine-word-unsigned-double-shift-right;
end &module;


define &module dispatch-engine-internal
  use dylan;
  use dylan-extensions;
  use dylan-primitives;
  use dispatch-engine;
  use threads;
  use threads-extensions;
  use threads-primitives;
  use machine-word-lowlevel;
  use simple-debugging;
end &module;


define &module internal
  use dylan;
  use dylan-primitives;
  use dylan-extensions;
  use dylan-direct-c-ffi;
  use dylan-c-ffi;
  use dylan-hygiene-glitches;
  use dylan-c-ffi-hygiene-glitches;
  use dylan-incremental;
  use simple-debugging;
  use threads;
  use threads-extensions;
  use threads-primitives;
  use machine-word-lowlevel;
  use dispatch-engine,
    export: 
      { <absent-engine-node>,
        <simple-typechecked-cache-header-engine-node>,
        $inapplicable-engine-node,
        %gf-dispatch-inapplicable,
        $absent-engine-node,
        %gf-dispatch-absent,
        initialize-class-dispatch-keys-vectored };

  export
    <C-automatic-pointer-designator-class>,
    <C-union-designator-class>,
    <C-struct-designator-class>,
    <C-mapped-designator-class>,
    <C-function-pointer-instantiation>,
    <temp-pointer-type-class>;

  export
    $direct-object-classes;

  // value cells - referenced from runtime.
  export
    <untraceable-double-value-cell>,
    <untraceable-value-cell>,
    <traceable-value-cell>;

  export
    %true, %false, %empty-list, %empty-vector, %empty-string;

  export
    <handler>, *current-handlers*, make-handler;

  export
    %shared-dylan-symbols, %shared-streams-symbols;

  // Magic optimizers.
  export
    binary-logior, binary-logxor, binary-logand,
    // generic-binary-logior, generic-binary-logxor, generic-binary-logand,
    do-one, any?-one, every?-one, map-as-one,
    binary-min, binary-max;

  export
    <function-class>, 
      <value-class>, value-class-comparitor, value-class-comparitor-setter, 
      default-class-constructor,
      <getter-method>, <setter-method>,
      <copy-down-method>,
      <simple-copy-down-method>,
      <keyword-copy-down-method>,
      <repeated-getter-method>, <repeated-setter-method>,
      <simple-closure-method>;

  export
    <virtual-slot-descriptor>,
      <class-slot-descriptor>, <each-subclass-slot-descriptor>;

  export
    <signature+values>, 
      <signature+rest-value>,
      <signature+values+rest-value>,
    <keyword-signature+values>,
      <keyword-signature+rest-value>,
      <keyword-signature+values+rest-value>,

    make-<keyword-signature>,
    make-<signature>,

    function-required-type,
    function-key-type,
    function-value-type,
    function-rest-value-type,

    $signature-<object>-no-rest-value-0,
    $signature-<object>-no-rest-value-1,
    $signature-<object>-no-rest-value-2,
    $signature-<object>-no-rest-value-3,
    $signature-<object>-no-rest-value-4,
    $signature-<object>-no-rest-value-5,
    $signature-<object>-no-rest-value-6,
    $signature-<object>-no-rest-value-7,
    $signature-<object>-no-rest-value-8,
    $signature-<object>-no-rest-value-9,
    $signature-<object>-no-rest-value-10,
    $signature-<object>-no-rest-value-11,
    $signature-<object>-no-rest-value-12,
    $signature-<object>-no-rest-value-13,
    $signature-<object>-no-rest-value-14,
    $signature-<object>-no-rest-value-15,

    $signature-<object>-object-rest-value-0,
    $signature-<object>-object-rest-value-1,
    $signature-<object>-object-rest-value-2,
    $signature-<object>-object-rest-value-3,
    $signature-<object>-object-rest-value-4,
    $signature-<object>-object-rest-value-5,
    $signature-<object>-object-rest-value-6,
    $signature-<object>-object-rest-value-7,
    $signature-<object>-object-rest-value-8,
    $signature-<object>-object-rest-value-9,
    $signature-<object>-object-rest-value-10,
    $signature-<object>-object-rest-value-11,
    $signature-<object>-object-rest-value-12,
    $signature-<object>-object-rest-value-13,
    $signature-<object>-object-rest-value-14,
    $signature-<object>-object-rest-value-15,

    $signature-<single-float>-types,
    $signature-<slot-descriptor>-types,
    $signature-<sealed-object-error>-types,
    $signature-<unbound>-types,
    $signature-<double-integer>-types,
    $signature-<limited-type>-types,
    $signature-<object-deque>-types,
    $signature-<unicode-string>-types,
    $signature-<limited-integer>-types,
    $signature-<simple-error>-types,
    $signature-<double-float>-types,
    $signature-<error>-types,
    $signature-<array>-types,
    $signature-<empty-list>-types,
    $signature-<class>-types,
    $signature-<float>-types,
    $signature-<simple-warning>-types,
    $signature-<union>-types,
    $signature-<deque>-types,
    $signature-<machine-word>-types,
    $signature-<simple-restart>-types,
    $signature-<miscellaneous-obsolete-instance>-types,
    $signature-<range>-types,
    $signature-<table>-types,
    $signature-<abort>-types,
    $signature-<signature>-types,
    $signature-<simple-object-vector>-types,
    $signature-<explicit-key-collection>-types,
    $signature-<mutable-sequence>-types,
    $signature-<obsolete-instance>-types,
    $signature-<sequence>-types,
    $signature-<condition>-types,
    $signature-<format-string-condition>-types,
    $signature-<rational>-types,
    $signature-<singleton>-types,
    $signature-<mutable-explicit-key-collection>-types,
    $signature-<function>-types,
    $signature-<repeated-slot-descriptor>-types,
    $signature-<set>-types,
    $signature-<string-table>-types,
    $signature-<simple-condition>-types,
    $signature-<collection>-types,
    $signature-<object-set>-types,
    $signature-<mutable-collection>-types,
    $signature-<bottom-type>-types,
    $signature-<big-integer>-types,
    $signature-<byte-character>-types,
    $signature-<hash-state>-types,
    $signature-<stretchy-sequence>-types,
    $signature-<serious-condition>-types,
    $signature-<table-vector>-types,
    $signature-<implementation-class>-types,
    $signature-<abstract-integer>-types,
    $signature-<object-table>-types,
    $signature-<type-error>-types,
    $signature-<warning>-types,
    $signature-<not-found-error>-types,
    $signature-<method>-types,
    $signature-<object>-types,
    $signature-<bottom>-types,
    $signature-<stretchy-vector>-types,
    $signature-<subclass>-types,
    $signature-<stretchy-collection>-types,
    $signature-<boolean>-types,
    $signature-<vector>-types,
    $signature-<complex>-types,
    $signature-<integer>-types,
    $signature-<restart>-types,
    $signature-<list>-types,
    $signature-<generic-function>-types,
    $signature-<stretchy-object-vector>-types,
    $signature-<pair>-types,
    $signature-<real>-types,
    $signature-<string>-types,
    $signature-<type>-types,
    $signature-<number>-types,
    $signature-<simple-vector>-types,
    $signature-<byte-string>-types;

  export
    augment-class-known-joint,
    augment-rcpl-position-data-multiple,
    augment-rcpl-position-data-kludgey;

  export
    uninitialized-instance?-function,

    <object>-class-instance?,
    never-instance?-function,
    class-instance?-initial,
    masked-class-instance?,
    general-rcpl-class-instance?,
    class-instance?-rcpl-single-small,
    class-instance?-rcpl-single-large,

    singleton-instance?,
    singleton-pointer-id?-instance?,
    singleton-value-object-instance?,

    union-instance?,

    limited-integer-instance?-function,

    subclass-instance?;

  export
    defaulted-initialization-arguments,
    install-and-return-make-method-init-data,
    make-method-init-value;

  export
    assertion-failure,
    debug-assertion-failure;

  export
    %resolve-symbol, %install-boot-symbols;

  // Run-time callbacks.

  export
    argument-count-error, odd-keyword-arguments-error,
    unknown-keyword-argument-error, stack-overflow-error, 
    argument-count-overflow-error,
    type-check-error,
    %slotacc-single-q-instance-getter,
    %slotacc-single-q-instance-setter,
    %slotacc-single-q-class-getter,
    %slotacc-single-q-class-setter,
    %slotacc-repeated-instance-getter,
    %slotacc-repeated-instance-setter;

  // Spy functions.

  export
    spy-resolve-keyword,
    spy-invoke-dylan-under-coded-restart,
    spy-invoke-numbered-restart,
    spy-weak-free-remote-object,
    spy-free-remote-object,
    spy-weak-remote-object-value,
    spy-remote-object-value,
    spy-register-weak-remote-object,
    spy-register-remote-object,
    spy-format-string-keyword,
    spy-format-arguments-keyword,
    spy-create-application-thread;

end &module;

define &module dylan-internal
  use internal, export: all;
end &module;
