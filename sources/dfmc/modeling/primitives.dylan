module: dfmc-modeling
author: jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///
/// PRIMITIVES
///

define class <dood-force-binding-value-proxy> (<dood-binding-value-proxy>)
end class;

define method dood-make-binding-value-proxy
    (dood :: <dood>, object :: <&primitive>) => (proxy)
  make(<dood-force-binding-value-proxy>, 
       binding: model-variable-binding(object))
end method;

define method dood-make-binding-value-proxy
    (dood :: <dood>, object :: <&raw-type>) => (proxy)
  make(<dood-force-binding-value-proxy>, binding: model-variable-binding(object))
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-force-binding-value-proxy>) => (object)
  with-dood-context (dood-root(dood))
  without-dependency-tracking
    let defn = binding-definition(dood-proxy-binding(proxy));
    let object 
      = binding-model-object(dood-proxy-binding(proxy));
    if (instance?(object, <dood-cross-model-proxy>))
      break("CIRCULARITY %=", proxy);
    end if;
    object
  end without-dependency-tracking;
  end with-dood-context;
end method;

define method dood-disk-object 
    (dood :: <dood>, object :: <&primitive>)
 => (proxy :: type-union(<dood-force-binding-value-proxy>, <&primitive>))
  // real primitive versus (ffi function)
  if (model-has-definition?(object))
    dood-as-proxy(dood, object, dood-make-binding-value-proxy)
  else
    object
  end if
end method;

define method dood-disk-object 
    (dood :: <dood>, object :: <&raw-type>)
 => (proxy :: type-union(<dood-force-binding-value-proxy>, <&raw-type>))
  dood-as-proxy(dood, object, dood-make-binding-value-proxy)
end method;

define method dood-disk-object 
    (dood :: <dood>, object :: <&raw-object>) => (res :: <&raw-object>)
  object
end method;

// Reminder: here are the recognised adjectives ...
//
//   side-effecting, side-effect-free, 
//   stateless, stateful,
//   dynamic-extent, indefinite-extent
//     indefinite-extent if the primitive may embed one of the arguments
//     in another object or an argument is returned by the primitive,
//     and the argument may be heap allocated.  
//
// We don't use defaults for these as previously this was error-prone.

/* !@#$ needed? */

define method make-compile-time-literal (object :: <function>)
  make(<&primitive>, value: object)
end method;

define sideways method compile-stage (object) => (object) object end;
define sideways method run-stage (object) => (object) object end;

define method compile-stage (object :: <&primitive>) => (object)
  object.primitive-value
end;

/// SUPPORT

define side-effecting stateful dynamic-extent &primitive primitive-break () => ();
define side-effecting stateful dynamic-extent &primitive primitive-invoke-debugger 
    (format-string :: <byte-string>, arguments :: <simple-object-vector>)
 => ();
define side-effecting stateless dynamic-extent &primitive primitive-inside-debugger? ()
 => (debugging? :: <boolean>);
define side-effecting stateless dynamic-extent &primitive primitive-debug-message
    (format-string :: <byte-string>, arguments :: <simple-object-vector>)
 => ();

/// MACHINE

define side-effect-free stateless dynamic-extent &primitive-and-override primitive-word-size
    () => (word-size :: <raw-integer>)
  make-raw-literal(word-size()) 
end;
define side-effect-free stateless dynamic-extent &primitive primitive-header-size
    () => (header-size :: <raw-integer>);

/// RAW-TYPE

/// ALLOCATION

define side-effect-free stateful indefinite-extent &primitive primitive-allocate
    (number-words :: <raw-integer>) => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent &primitive primitive-untraced-allocate
    (number-bytes :: <raw-integer>) => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent &primitive primitive-manual-allocate
    (number-bytes :: <integer>) => (pointer :: <object>);
define side-effecting indefinite-extent &primitive primitive-manual-free
    (pointer :: <object>) => ();
define side-effect-free stateful indefinite-extent &primitive primitive-allocate-wrapper
    (number-words :: <raw-integer>,
     class-wrapper :: <object>,
     number-slots :: <raw-integer>,
     fill-value :: <object>, 
     repeated-size :: <raw-integer>,
     repeated-size-offset :: <raw-integer>)
  => (object :: <object>);
define side-effect-free stateful indefinite-extent &primitive primitive-byte-allocate-filled-terminated
    (number-words :: <raw-integer>, number-bytes :: <raw-integer>,
     class-wrapper :: <object>,
     number-slots :: <raw-integer>,
     fill-value :: <raw-integer>,
     repeated-size :: <raw-integer>,
     repeated-size-offset :: <raw-integer>) 
  => (object :: <object>);
define side-effect-free stateful indefinite-extent &primitive primitive-byte-allocate-leaf-filled-terminated
    (number-words :: <raw-integer>, number-bytes :: <raw-integer>,
     class-wrapper :: <object>,
     number-slots :: <raw-integer>,
     fill-value :: <raw-integer>,
     repeated-size :: <raw-integer>,
     repeated-size-offset :: <raw-integer>) 
  => (object :: <object>);

define macro repeated-allocator-primitive-definer
  { define repeated-allocator-primitive (?:name, ?type:name) }
    => { define side-effect-free stateful indefinite-extent &primitive 
               "primitive-" ## ?name ## "-allocate-filled"
	     (number-words :: <raw-integer>,
	      class-wrapper :: <object>,
	      number-slots :: <raw-integer>,
	      fill-value :: <object>,
	      repeated-size :: <raw-integer>,
	      repeated-size-offset :: <raw-integer>,
              repeated-fill-value :: ?type) 
          => (object :: <object>) }
end macro;

define repeated-allocator-primitive(object,       <object>);
define repeated-allocator-primitive(byte,         <raw-byte>);
define repeated-allocator-primitive(double-byte,  <raw-double-byte>);
define repeated-allocator-primitive(word,         <raw-machine-word>);
define repeated-allocator-primitive(double-word,  <raw-double-integer>);
define repeated-allocator-primitive(single-float, <raw-single-float>);
define repeated-allocator-primitive(double-float, <raw-double-float>);

define side-effect-free stateful indefinite-extent &primitive primitive-byte-allocate-leaf-filled
  (number-words :: <raw-integer>,
   class-wrapper :: <object>,
   number-slots :: <raw-integer>,
   fill-value :: <object>,
   repeated-size :: <raw-integer>,
   repeated-size-offset :: <raw-integer>,
   repeated-fill-value :: <raw-byte>) 
  => (object :: <object>);

define side-effect-free stateful indefinite-extent &primitive primitive-allocate-in-awl-pool
    (number-words :: <raw-integer>,
     class-wrapper :: <object>,
     number-slots :: <raw-integer>,
     fill-value :: <object>,
     repeated-size :: <raw-integer>,
     repeated-size-offset :: <raw-integer>,
     assoc-link :: <object>) 
  => (object :: <object>);

define side-effect-free stateful indefinite-extent &primitive primitive-allocate-weak-in-awl-pool
    (number-words :: <raw-integer>,
     class-wrapper :: <object>,
     number-slots :: <raw-integer>,
     fill-value :: <object>,
     repeated-size :: <raw-integer>,
     repeated-size-offset :: <raw-integer>,
     assoc-link :: <object>) 
  => (object :: <object>);


/// Breakpoints on class allocation

define side-effecting &primitive primitive-set-class-breakpoint
  (dylan-class :: <object>, count :: <integer>) => ();
define side-effecting &primitive primitive-clear-class-breakpoint (dylan-class :: <object>) => ();
define side-effecting &primitive primitive-display-class-breakpoints (string-buffer :: <raw-byte-string>) => (number-written :: <raw-integer>);


/// ACCESSORS

define side-effect-free dynamic-extent &primitive primitive-element
    (x :: <object>, offset :: <raw-integer>, byte-offset :: <raw-integer>) 
 => (obj :: <object>);
define side-effecting stateless dynamic-extent &primitive primitive-element-setter
    (new-value :: <object>,
     x :: <object>, offset :: <raw-integer>, byte-offset :: <raw-integer>)
 => (obj :: <object>);
define side-effect-free dynamic-extent &primitive primitive-byte-element
    (x :: <object>, offset :: <raw-integer>, byte-offset :: <raw-integer>) 
 => (obj :: <raw-byte-character>);
define side-effecting stateless dynamic-extent &primitive primitive-byte-element-setter
    (new-value :: <raw-byte-character>,
     x :: <object>, offset :: <raw-integer>, byte-offset :: <raw-integer>) 
 => (obj :: <raw-byte-character>);
define side-effect-free dynamic-extent &primitive primitive-bit-element
    (x :: <object>, word-offset :: <raw-integer>, byte-offset :: <raw-integer>,
     bit-offset :: <raw-integer>) 
 => (obj :: <raw-integer>);
define side-effecting stateless dynamic-extent &primitive primitive-bit-element-setter
    (new-value :: <raw-integer>,
     x :: <object>, offset :: <raw-integer>, byte-offset :: <raw-integer>,
     bit-offset :: <raw-integer>) 
 => (obj :: <raw-integer>);


define side-effect-free dynamic-extent &primitive primitive-bit-field
    (pointer :: <raw-pointer>, 
     bit-offset :: <raw-integer>, 
     bit-size :: <raw-integer>) 
 => (field :: <raw-integer>);

define side-effecting stateless dynamic-extent &primitive primitive-bit-field-setter
    (new-field :: <raw-integer>,
     pointer :: <raw-pointer>, 
     bit-offset :: <raw-integer>, 
     bit-size :: <raw-integer>) 
 => (new-field :: <raw-integer>);



define side-effecting stateless dynamic-extent &primitive primitive-fill!
    (dst :: <object>, base-offset :: <raw-integer>, offset :: <raw-integer>, size :: <raw-integer>, 
     value :: <object>)
 => ();
define side-effecting stateless dynamic-extent &primitive primitive-fill-bytes!
    (dst :: <object>, base-offset :: <raw-integer>, offset :: <raw-integer>, size :: <raw-integer>, 
     value :: <raw-byte-character>)
 => ();
define side-effecting stateless dynamic-extent &primitive primitive-replace!
    (dst :: <object>, dst-base-offset :: <raw-integer>, dst-offset :: <raw-integer>,
     src :: <object>, src-base-offset :: <raw-integer>, src-offset :: <raw-integer>,
     size :: <raw-integer>)
 => ();
define side-effecting stateless dynamic-extent &primitive primitive-replace-bytes!
    (dst :: <object>, dst-base-offset :: <raw-integer>, dst-offset :: <raw-integer>,
     src :: <object>, src-base-offset :: <raw-integer>, src-offset :: <raw-integer>,
     size :: <raw-integer>)
 => ();

/// GC

define side-effecting stateless dynamic-extent &primitive primitive-pin-object
    (object :: <object>) => (object :: <object>);
define side-effecting stateless dynamic-extent &primitive primitive-unpin-object
    (object :: <object>) => ();
define side-effecting &primitive primitive-mps-finalize (object :: <object>) => ();
define side-effecting stateful &primitive primitive-mps-finalization-queue-first
  () => (object :: <object>);
define side-effecting stateful &primitive primitive-mps-park () => ();
define side-effecting stateful &primitive primitive-mps-clamp () => ();
define side-effecting stateful &primitive primitive-mps-release () => ();

define side-effecting stateful &primitive primitive-mps-collect (print-stats? :: <raw-boolean>) => ();
define side-effecting stateful &primitive primitive-mps-collection-stats
  (object :: <raw-pointer>) => (found? :: <raw-boolean>);
define side-effecting stateful &primitive primitive-mps-enable-gc-messages () => ();

define side-effecting stateful &primitive primitive-mps-committed () => (bytes :: <raw-integer>);

define side-effecting stateful &primitive primitive-mps-begin-ramp-alloc () => ();
define side-effecting stateful &primitive primitive-mps-end-ramp-alloc () => ();

define side-effecting stateful &primitive primitive-mps-begin-ramp-alloc-all () => ();
define side-effecting stateful &primitive primitive-mps-end-ramp-alloc-all () => ();

define side-effecting stateless dynamic-extent &primitive primitive-mps-ld-reset 
    (primitive-hash-state) => ();
define side-effecting stateless dynamic-extent &primitive primitive-mps-ld-add 
    (primitive-hash-state, object) => ();
define side-effecting stateless dynamic-extent &primitive primitive-mps-ld-merge 
    (primitive-hash-state-1, primitive-hash-state-2) => ();
define side-effect-free stateless dynamic-extent &primitive primitive-mps-ld-isstale 
    (primitive-hash-state) => (is-stale? :: <raw-integer>);

define side-effect-free stateful &primitive primitive-allocation-count 
  () => (count :: <raw-integer>);
define side-effecting stateful &primitive primitive-initialize-allocation-count () => ();

define side-effecting stateful &primitive primitive-begin-heap-alloc-stats () => ();
define side-effecting stateful &primitive primitive-end-heap-alloc-stats
  (string-buffer :: <raw-byte-string>) => (number-read :: <raw-integer>);


// DLL Support

define side-effecting &primitive primitive-runtime-module-handle () => (handle :: <machine-word>);


// Support for keyboard-break handling

define side-effect-free &primitive primitive-keyboard-interrupt-signaled ()
  => (interrupt? :: <raw-boolean>);

define side-effecting &primitive primitive-keyboard-interrupt-signaled-setter
  (interrupt? :: <raw-boolean>)  => ();

define side-effect-free &primitive primitive-keyboard-interrupt-polling ()
  => (interrupt-polling? :: <raw-boolean>);

define side-effecting &primitive primitive-keyboard-interrupt-polling-setter
  (interrupt-polling? :: <raw-boolean>) => ();

define side-effect-free &primitive primitive-keyboard-interrupt-polling-thread (hThread :: <raw-pointer>)
  => (interrupt-polling? :: <raw-boolean>);

define side-effecting &primitive primitive-keyboard-interrupt-polling-thread-setter
  (interrupt-polling? :: <raw-boolean>, hThread :: <raw-pointer>) => ();


/// UNICODE CHARACTER

// TODO: NEED UNICODE SUPPORT IN COMPILER's RUNTIME
define side-effect-free stateless dynamic-extent &primitive primitive-unicode-character-as-raw
    (x :: <unicode-character>) => (r :: <raw-integer>);
define side-effect-free stateless dynamic-extent &primitive primitive-raw-as-unicode-character
     (r :: <raw-integer>) => (x :: <unicode-character>);

/// BYTE-CHARACTER

define side-effect-free stateless dynamic-extent &primitive-and-override primitive-byte-character-as-raw
    (x :: <byte-character>) => (r :: <raw-integer>)
  make-raw-literal(as(<integer>, x))
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-raw-as-byte-character
     (r :: <raw-integer>) => (x :: <byte-character>)
  as(<byte-character>, ^raw-object-value(r))
end;

/// INTEGER

/// SMALL-INTEGER

/// BIG-INTEGERS

/// MACHINE-INTEGERS

/// UNSIGNED-MACHINE-INTEGERS

/// ADDRESSES

/// POINTER

define side-effect-free stateless indefinite-extent &primitive primitive-cast-pointer-as-raw
    (x :: <raw-pointer>) => (z :: <raw-address>);
define side-effect-free stateless indefinite-extent &primitive primitive-cast-raw-as-pointer
    (x :: <raw-address>) => (z :: <raw-pointer>);

/// TYPE-CHECKS

define side-effect-free stateless dynamic-extent &primitive-and-override primitive-instance? 
    (x :: <object>, t :: <type>) => (true? :: <boolean>)
  ^instance?(x, t)
end;
define side-effect-free stateless dynamic-extent &primitive primitive-type-check
    (x :: <object>, t :: <type>) => (true? :: <boolean>);
define side-effect-free stateless dynamic-extent &primitive primitive-range-check
    (i :: <raw-integer>, low :: <raw-integer>, high :: <raw-integer>)
 => (true? :: <boolean>);

/// COMPARISONS

define side-effect-free stateless dynamic-extent &primitive-and-override primitive-raw-as-boolean
    (x :: <raw-boolean>) => (true? :: <boolean>)
  ^raw-object-value(x)
end;

define side-effect-free stateless dynamic-extent &primitive-and-override primitive-boolean-as-raw
    (x) => (true? :: <raw-boolean>)
  make-raw-literal(if (x) 1 else 0 end)
end;

define side-effect-free stateless dynamic-extent &primitive-and-override primitive-as-boolean
    (x) => (true? :: <boolean>)
  x ~== #f
end;

define side-effect-free stateless dynamic-extent &primitive-and-override primitive-not
    (x :: <object>) => (not-x :: <boolean>)
  ~(^raw-object-value(x))
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-id?
    (x :: <object>, y :: <object>) => (id? :: <boolean>)
  x == y
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-not-id?
    (x :: <object>, y :: <object>) => (not-id? :: <boolean>)
  x ~== y
end;

define side-effect-free stateless dynamic-extent &primitive primitive-compare-bytes
    (base1 :: <raw-pointer>, offset1 :: <raw-integer>,
     base2 :: <raw-pointer>, offset2 :: <raw-integer>,
     size-in-bytes :: <raw-integer>)
 => (same? :: <boolean>);

define side-effect-free stateless dynamic-extent &primitive primitive-compare-words
    (base1 :: <raw-pointer>, offset1 :: <raw-integer>,
     base2 :: <raw-pointer>, offset2 :: <raw-integer>,
     size-in-words :: <raw-integer>)
 => (same? :: <boolean>);

/// REPEATED

define side-effect-free stateless indefinite-extent &primitive primitive-repeated-slot-as-raw
    (x :: <object>, offset :: <raw-integer>) => (r :: <raw-pointer>);
define side-effect-free stateless indefinite-extent &primitive primitive-repeated-slot-offset
    (x :: <object>) => (r :: <raw-integer>);

/// VECTOR

define side-effect-free stateless indefinite-extent &primitive-and-override primitive-vector
    (size :: <integer>, #rest arguments) => (value :: <simple-object-vector>)
  as(<vector>, arguments)
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-copy-vector
    (x :: <simple-object-vector>) => (value :: <simple-object-vector>)
  copy-sequence(x)
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-vector-element 
    (x :: <simple-object-vector>, index :: <raw-integer>) => (value :: <object>)
  element(x, index)
end;
define side-effecting stateless indefinite-extent &primitive-and-override primitive-vector-element-setter
    (new-value :: <object>, x :: <simple-object-vector>, index :: <raw-integer>)
 => (value :: <object>)
  element(x, index) := new-value;
end;
define side-effect-free stateless dynamic-extent &primitive-and-override primitive-vector-size 
    (x :: <simple-object-vector>) => (size :: <raw-integer>)
  size(x)
end;
define side-effect-free stateless indefinite-extent &primitive primitive-vector-as-raw
    (x :: <simple-object-vector>) => (r :: <raw-pointer>);
define side-effect-free stateless indefinite-extent &primitive primitive-raw-as-vector
    (n :: <raw-integer>, r :: <raw-pointer>) => (x :: <simple-object-vector>);

/// STRING

define side-effect-free stateless dynamic-extent &primitive-and-override primitive-strlen
    (x :: <raw-byte-string>) => (size :: <raw-integer>)
  size(x)
end;

define side-effect-free stateless indefinite-extent &primitive primitive-string-as-raw
    (x :: <byte-string>) => (r :: <raw-byte-string>);
define side-effect-free stateless indefinite-extent &primitive primitive-raw-as-string
    (r :: <raw-byte-string>) => (x :: <byte-string>);

/// INSTANCE

define side-effect-free stateless dynamic-extent &primitive-and-override primitive-object-class
    (x :: <object>) => (c :: <class>)
  &object-class(x)
end;

define side-effect-free stateless dynamic-extent &primitive primitive-slot-value
    (x :: <object>, position :: <raw-integer>) => (value :: <object>);
define side-effect-free stateless dynamic-extent &primitive primitive-initialized-slot-value
    (x :: <object>, position :: <raw-integer>) => (value :: <object>);
define side-effecting stateless indefinite-extent &primitive primitive-slot-value-setter
    (value :: <object>, x :: <object>, position :: <raw-integer>) => (value :: <object>);

define side-effect-free stateless dynamic-extent &primitive primitive-repeated-slot-value
    (x :: <object>, base-position :: <raw-integer>, position :: <raw-integer>)
 => (value :: <object>);
define side-effecting stateless indefinite-extent &primitive primitive-repeated-slot-value-setter
    (value :: <object>,
     x :: <object>, base-position :: <raw-integer>, position :: <raw-integer>)
 => (value :: <object>);

/// CALLING CONVENTION

define side-effect-free stateless dynamic-extent &primitive primitive-function-parameter
    () => (fn :: <function>);

// !@#$ where used? - in conversion/convert.dylan and also optimizer!
define side-effect-free stateless dynamic-extent &primitive primitive-next-methods-parameter // ??
    () => (nm :: <list>); 

define side-effecting stateless dynamic-extent &primitive primitive-set-generic-function-entrypoints
    (gf :: <generic-function>) => ();

define side-effecting stateless dynamic-extent &primitive primitive-set-accessor-method-xep
  (accessor-method :: <accessor-method>) => (accessor-method :: <accessor-method>);


/// APPLY

define side-effecting stateless indefinite-extent &primitive primitive-xep-apply
    (function :: <object>, buffer-size :: <raw-integer>, buffer :: <object>)
 => (#rest values);

define side-effecting stateless indefinite-extent &primitive primitive-mep-apply
    (function :: <object>, next-methods :: <object>, args :: <simple-object-vector>)
 => (#rest values);

define side-effecting stateless indefinite-extent &primitive primitive-mep-apply-with-optionals
    (function :: <object>, next-methods :: <object>, args :: <object>)
 => (#rest values);

define side-effecting stateless indefinite-extent &primitive primitive-engine-node-apply-with-optionals
    (function :: <object>, next-methods :: <object>, args :: <object>)
 => (#rest values);

define side-effecting stateless indefinite-extent &primitive primitive-iep-apply
    (function :: <object>, buffer-size :: <raw-integer>, buffer :: <object>)
 => (#rest values);

// !@#$ needs to be built-in
define side-effecting stateless indefinite-extent &primitive primitive-apply // !@#$ broken
    (fn :: <function>, size :: <integer>, #rest arguments)
 => (#rest values);


/// DISCRIMINATOR/ENGINE-NODE INITIALIZATION

define side-effecting stateless dynamic-extent &primitive primitive-initialize-engine-node
    (engine-node :: <engine-node>) => (single-value :: <engine-node>);

define side-effecting stateless dynamic-extent &primitive primitive-initialize-discriminator
    (discriminator :: <discriminator>) => (single-value :: <discriminator>);



/// MULTIPLE-VALUES

define side-effect-free stateless dynamic-extent &primitive-and-override primitive-values
    (size :: <integer>, #rest arguments) => (#rest values)
  apply(values, arguments)
end;

//// SYMBOL BOOTING

define side-effecting stateless indefinite-extent &primitive primitive-resolve-symbol
    (uninterned-symbol :: <symbol>) => (canonical-symbol :: <symbol>);

define side-effect-free stateless dynamic-extent &primitive primitive-string-as-symbol
    (string :: <byte-string>) => (symbol :: <symbol>);

define side-effect-free stateless dynamic-extent &primitive primitive-preboot-symbols
    () => (res :: <simple-object-vector>);

/// C-FFI

define side-effect-free stateless dynamic-extent &primitive primitive-unwrap-c-pointer
    (pointer :: <C-pointer>) => (p :: <raw-c-pointer>);

define side-effect-free stateless dynamic-extent &primitive primitive-wrap-c-pointer
    (wrapper :: <mm-wrapper>, pointer :: <raw-c-pointer>) => (p :: <C-pointer>);


define macro raw-field-primitive-definer
  { define raw-field-primitive ?:name ?raw-value-type:name}
    => { define side-effect-free stateless dynamic-extent &primitive "primitive-" ## ?name ## "-field"
             (pointer :: <raw-pointer>,
	      byte-offset :: <raw-integer>,
	      bit-offset :: <raw-integer>,
	      bit-size :: <raw-integer>)
          => (value :: ?raw-value-type);
        define side-effecting stateless dynamic-extent &primitive 
          "primitive-" ## ?name ## "-field-setter"
             (new :: ?raw-value-type,
	      pointer :: <raw-pointer>,
	      byte-offset :: <raw-integer>,
	      bit-offset :: <raw-integer>,
	      bit-size :: <raw-integer>)
          => (value :: ?raw-value-type) }
end macro;

define raw-field-primitive c-unsigned <raw-c-unsigned-long>;
define raw-field-primitive c-signed <raw-c-signed-long>;
define raw-field-primitive c-int <raw-c-unsigned-int>;
//define raw-field-primitive c-unsigned <raw-c-size-t>;
//define raw-field-primitive c-signed <raw-c-ssize-t>;

/// OPERATING SYSTEM

define side-effecting stateless dynamic-extent &primitive primitive-exit-application
    (code :: <raw-integer>) => ();
