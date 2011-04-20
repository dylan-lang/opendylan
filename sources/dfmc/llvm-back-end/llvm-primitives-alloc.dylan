Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Auxiliary routines in the C portion of the runtime

define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor dylan--malloc--misc
    (number-bytes :: <raw-integer>) => (pointer :: <raw-pointer>);

define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor mps--malloc
    (number-bytes :: <raw-integer>) => (pointer :: <raw-pointer>);
define side-effecting indefinite-extent auxiliary &c-primitive-descriptor mps--free
    (pointer :: <raw-pointer>) => ();

define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-leaf
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>) => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-leaf-r
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>)
 => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-leaf-rf
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>,
     rep-fill :: <raw-pointer>)
 => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-leaf-rbf
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>,
     rep-fill :: <raw-integer>)
 => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-leaf-rhf
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>,
     rep-fill :: <raw-integer>)
 => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-leaf-rsff
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>,
     rep-fill :: <raw-single-float>)
 => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-leaf-rdff
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>,
     rep-fill :: <raw-double-float>)
 => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-leaf-rbfz
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>,
     byte-fill :: <raw-integer>)
 => (pointer :: <raw-pointer>);


define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-leaf-s-r
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     number-to-fill :: <raw-integer>, fill :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>)
 => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-leaf-s-rbf
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     number-to-fill :: <raw-integer>, fill :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>,
     byte-fill :: <raw-integer>)
 => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-leaf-s-rbfz
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     number-to-fill :: <raw-integer>, fill :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>,
     byte-fill :: <raw-integer>)
 => (pointer :: <raw-pointer>);

define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-exact-awl-s-r
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     assoc :: <raw-pointer>,
     number-to-fill :: <raw-integer>, fill :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>)
 => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-weak-awl-s-r
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     assoc :: <raw-pointer>,
     number-to-fill :: <raw-integer>, fill :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>)
 => (pointer :: <raw-pointer>);

define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>)
 => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-s1
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     data1 :: <raw-pointer>)
 => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-s2
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     data1 :: <raw-pointer>, data2 :: <raw-pointer>)
 => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-s
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     number-to-fill :: <raw-integer>, fill :: <raw-pointer>)
 => (pointer :: <raw-pointer>);

define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-rf
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>,
     rep-fill :: <raw-pointer>)
 => (pointer :: <raw-pointer>);

define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-s-rf
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     number-to-fill :: <raw-integer>, fill :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>,
     rep-fill :: <raw-pointer>)
 => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-s-rbf
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     number-to-fill :: <raw-integer>, fill :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>,
     byte-fill :: <raw-integer>)
 => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-s-rhf
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     number-to-fill :: <raw-integer>, fill :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>,
     rep-fill :: <raw-integer>)
 => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-s-rsff
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     number-to-fill :: <raw-integer>, fill :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>,
     rep-fill :: <raw-single-float>)
 => (pointer :: <raw-pointer>);
define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-alloc-s-rdff
    (number-bytes :: <raw-integer>, wrapper :: <raw-pointer>,
     number-to-fill :: <raw-integer>, fill :: <raw-pointer>,
     rep-size :: <raw-integer>, rep-size-slot :: <raw-integer>,
     rep-fill :: <raw-double-float>)
 => (pointer :: <raw-pointer>);

define side-effect-free stateful indefinite-extent auxiliary &c-primitive-descriptor primitive-copy
    (number-bytes :: <raw-integer>, template :: <raw-pointer>)
 => (pointer :: <raw-pointer>);


/// Allocation operations

define method slot-storage-bytes
    (be :: <llvm-back-end>, slot-type :: <&raw-type>) => (bytes :: <integer>);
  slot-type.raw-type-size
end method;

define method slot-storage-bytes
    (be :: <llvm-back-end>, slot-type) => (bytes :: <integer>);
  back-end-word-size(be)
end method;

define method instance-storage-bytes
    (be :: <llvm-back-end>, class :: <&class>) => (bytes :: <integer>);
  let word-size = back-end-word-size(be);
  for (slotd in class.^instance-slot-descriptors,
       instance-bytes = dylan-value(#"$number-header-words") * word-size
         then instance-bytes + slot-storage-bytes(be, slotd.^slot-type))
  finally
    instance-bytes
  end for;
end method;

// Allocate memory for a leaf object instance
define method op--allocate-untraced
    (be :: <llvm-back-end>, class :: <&class>)
 => (pointer :: <llvm-value>);
  let module = be.llvm-builder-module;
  let pointer
    = call-primitive(be, primitive-alloc-leaf-descriptor,
                     instance-storage-bytes(be, class),
                     emit-reference(be, module, ^class-mm-wrapper(class)));
  op--object-pointer-cast(be, pointer, class)
end method;

define method op--allocate-untraced
    (be :: <llvm-back-end>, class-name :: <symbol>)
 => (pointer :: <llvm-value>);
  let class :: <&class> = dylan-value(class-name);
  op--allocate-untraced(be, class)
end method;

// Allocate memory for a traced object instance
/*
define method op--allocate-traced
    (be :: <llvm-back-end>, class-name :: <symbol>, #rest slots)
 => (pointer :: <llvm-value>);
  let class :: <&class> = dylan-value(class-name);
  let module = be.llvm-builder-module;

  let byte-size = instance-storage-bytes(be, class);
  let wrapper = emit-reference(be, module, ^class-mm-wrapper(class));

  let pointer
    = select (slots.size)
        0 =>
          call-primitive(be, primitive-alloc-descriptor, byte-size, wrapper);
        1 =>
          apply(call-primitive, be, primitive-alloc-s1-descriptor,
                byte-size, wrapper, slots);
        2 =>
          apply(call-primitive, be, primitive-alloc-s2-descriptor,
                byte-size, wrapper, slots);
        otherwise =>
          error("Not handling %d-slot op--allocate-traced yet", slots.size);
      end select;
  op--object-pointer-cast(be, pointer, class)
end method;
*/


/// Allocation primitives

/*
define side-effect-free stateful indefinite-extent &runtime-primitive-descriptor primitive-allocate
    (number-words :: <raw-integer>) => (pointer :: <raw-pointer>);
  //---*** Fill this in...
end;

define side-effect-free stateful indefinite-extent &runtime-primitive-descriptor primitive-byte-allocate
    (number-words :: <raw-integer>, number-bytes :: <raw-integer>)
 => (pointer :: <raw-pointer>)
  //---*** Fill this in...
end;
*/

// Allocate a block of memory untraced by the garbage collector
define side-effect-free stateful indefinite-extent &runtime-primitive-descriptor primitive-untraced-allocate
    (number-bytes :: <raw-integer>) => (pointer :: <raw-pointer>);
  call-primitive(be, dylan--malloc--misc-descriptor, number-bytes)
end;

// Allocate an explicitly-freeable block of memory; used as the
// default allocator for C-FFI <C-pointer>
define side-effect-free stateful indefinite-extent mapped-result &runtime-primitive-descriptor primitive-manual-allocate
    (number-bytes :: <abstract-integer>) => (pointer :: <machine-word>);
  let bytes
    = call-primitive(be, primitive-unwrap-abstract-integer-descriptor,
                     number-bytes);
  let raw-pointer = call-primitive(be, mps--malloc-descriptor, bytes);
  let raw-machine-word
    = ins--ptrtoint(be, raw-pointer,
                    llvm-reference-type
                      (be, dylan-value(#"<raw-machine-word>")));
  call-primitive(be, primitive-wrap-machine-word-descriptor, raw-machine-word)
end;

// Free a block of memory allocated by primitive-manual-allocate; used
// as the default deallocator for C-FFI <C-pointer>
define side-effecting indefinite-extent mapped &runtime-primitive-descriptor primitive-manual-free
    (pointer :: <machine-word>) => ();
  let raw
    = call-primitive(be, primitive-unwrap-machine-word-descriptor, pointer);
  let raw-pointer
    = ins--inttoptr(be, raw,
                    llvm-reference-type(be, dylan-value(#"<raw-pointer>")));
  call-primitive(be, mps--free-descriptor, raw-pointer);
end;

define side-effect-free stateful indefinite-extent &runtime-primitive-descriptor primitive-allocate-wrapper
    (number-words :: <raw-integer>,
     class-wrapper :: <object>,
     number-slots :: <raw-integer>,
     fill-value :: <object>,
     repeated-size :: <raw-integer>,
     repeated-size-offset :: <raw-integer>)
  => (object :: <object>);
  let number-bytes = ins--mul(be, number-words, back-end-word-size(be));
  let wrapper = op--raw-pointer-cast(be, class-wrapper);
  let fill = op--raw-pointer-cast(be, fill-value);
  let ptr = call-primitive(be, primitive-alloc-leaf-s-r-descriptor,
                           number-bytes, wrapper, number-slots, fill,
                           repeated-size, repeated-size-offset);
  ptr
end;

// Byte allocators:
// Assume that these can be allocated in the leaf pool. This is
// essential for strings. If we ever need an object with both
// traceable slots and a byte repeated slot, then we'll need
// additional allocation primitives.

/*
define side-effect-free stateful indefinite-extent &primitive-descriptor primitive-byte-allocate-filled-terminated
    (number-words :: <raw-integer>, number-bytes :: <raw-integer>,
     class-wrapper :: <object>,
     number-slots :: <raw-integer>,
     fill-value :: <raw-integer>,
     repeated-size :: <raw-integer>,
     repeated-size-offset :: <raw-integer>)
  => (object :: <object>)
  //---*** Fill this in...
end;
*/

define side-effect-free stateful indefinite-extent &primitive-descriptor primitive-byte-allocate-leaf-filled-terminated
    (number-words :: <raw-integer>,
     number-bytes :: <raw-integer>,
     class-wrapper :: <object>,
     number-slots :: <raw-integer>,
     fill-value :: <raw-integer>,
     repeated-size :: <raw-integer>,
     repeated-size-offset :: <raw-integer>)
  => (object :: <object>);
  let number-bytes = ins--mul(be, number-words, back-end-word-size(be));
  let word-size = back-end-word-size(be);
  let repeated-byte-size = ins--mul(be, number-words, word-size);
  let total-size = ins--add(be, repeated-byte-size, repeated-size);
  let total-size-rounded = op--round-up-to-word(be, total-size);

  let byte-fill = op--untag-character(be, fill-value);

  let raw-number-slots
    = instance?(number-slots, <llvm-integer-constant>)
    & number-slots.llvm-integer-constant-integer;
  select (raw-number-slots)
    0 =>
      // Allocate a byte-repeated leaf object with no fixed slots
      call-primitive(be, primitive-alloc-leaf-rbfz-descriptor,
                     total-size, class-wrapper,
                     repeated-size, repeated-size-offset, byte-fill);
    otherwise =>
      // Allocate a byte-repeated leaf object with fixed slots
      call-primitive(be, primitive-alloc-leaf-s-rbfz-descriptor,
                     total-size, class-wrapper,
                     number-slots, fill-value,
                     repeated-size, repeated-size-offset, byte-fill);
  end select
end;

/*
*/

define macro repeated-allocate-primitive-definer
  { define repeated-allocate-primitive (?be:name, ?:name,
                                        ?alloc:name, ?alloc-s:name,
                                        ?type:name) }
    => { define side-effect-free stateful indefinite-extent &primitive-descriptor
               "primitive-" ## ?name ## "-allocate-filled"
	     (number-words :: <raw-integer>,
	      class-wrapper :: <object>,
	      number-slots :: <raw-integer>,
	      fill-value :: <object>,
	      repeated-size :: <raw-integer>,
	      repeated-size-offset :: <raw-integer>,
              repeated-fill-value :: ?type)
          => (object :: <object>);
           let number-bytes
             = ins--mul(?be, number-words, back-end-word-size(?be));
           let repeated-byte-size
             = ins--mul(?be, repeated-size,
                        slot-storage-bytes(?be, dylan-value(?#"type")));
           let byte-size
             = ins--add(?be, number-bytes, repeated-byte-size);

           let raw-repeated-size-offset
             = instance?(repeated-size-offset, <llvm-integer-constant>)
             & repeated-size-offset.llvm-integer-constant-integer;
           let raw-number-slots
             = instance?(number-slots, <llvm-integer-constant>)
             & number-slots.llvm-integer-constant-integer;

           if (raw-repeated-size-offset = 0)
             select (raw-number-slots)
               0 =>
                 call-primitive(?be, primitive-alloc-descriptor,
                                byte-size, class-wrapper);
               1 =>
                 call-primitive(?be, primitive-alloc-s1-descriptor,
                                byte-size, class-wrapper,
                                fill-value);
               2 =>
                 call-primitive(?be, primitive-alloc-s2-descriptor,
                                byte-size, class-wrapper,
                                fill-value, fill-value);
               otherwise =>
                 call-primitive(?be, primitive-alloc-s-descriptor,
                                byte-size, class-wrapper,
                                number-slots, fill-value);
             end select
           else
             select (raw-number-slots)
               0 =>
                 call-primitive(?be,
                                "primitive-alloc-" ## ?alloc ## "-descriptor",
                                byte-size, class-wrapper,
                                repeated-size, repeated-size-offset,
                                repeated-fill-value);
               otherwise =>
                 call-primitive(?be,
                                "primitive-alloc-" ## ?alloc-s ## "-descriptor",
                                byte-size, class-wrapper,
                                number-slots, fill-value,
                                repeated-size, repeated-size-offset,
                                repeated-fill-value);
             end select
           end if
         end }
end macro;

define repeated-allocate-primitive(be, object,       rf,        s-rf,
                                   <object>);
define repeated-allocate-primitive(be, byte,         leaf-rbf,  s-rbf,
                                   <raw-byte>);
define repeated-allocate-primitive(be, double-byte,  leaf-rhf,  s-rhf,
                                   <raw-double-byte>);
define repeated-allocate-primitive(be, word,         leaf-rf,   s-rf,
                                   <raw-machine-word>);
//define repeated-allocate-primitive(be, double-word,  leaf-rdwf, s-rdwf,
//                                   <raw-double-integer>);
define repeated-allocate-primitive(be, single-float, leaf-rsff, leaf-rsff,
                                   <raw-single-float>);
define repeated-allocate-primitive(be, double-float, leaf-rdff, leaf-rdff,
                                   <raw-double-float>);

define side-effect-free stateful indefinite-extent &primitive-descriptor primitive-byte-allocate-leaf-filled
  (number-words :: <raw-integer>,
   class-wrapper :: <object>,
   number-slots :: <raw-integer>,
   fill-value :: <object>,
   repeated-size :: <raw-integer>,
   repeated-size-offset :: <raw-integer>,
   repeated-fill-value :: <raw-byte>)
  => (object :: <object>)
  let number-bytes = ins--mul(be, number-words, back-end-word-size(be));
  let word-size = back-end-word-size(be);
  let repeated-byte-size = ins--mul(be, number-words, word-size);
  let total-size = ins--add(be, repeated-byte-size, repeated-size);
  let total-size-rounded = op--round-up-to-word(be, total-size);

  let byte-fill = op--untag-character(be, fill-value);

  let raw-number-slots
    = instance?(number-slots, <llvm-integer-constant>)
    & number-slots.llvm-integer-constant-integer;
  select (raw-number-slots)
    0 =>
      // Allocate a byte-repeated leaf object with no fixed slots
      call-primitive(be, primitive-alloc-leaf-rbf-descriptor,
                     total-size, class-wrapper,
                     repeated-size, repeated-size-offset, byte-fill);
    otherwise =>
      // Allocate a byte-repeated leaf object with fixed slots
      call-primitive(be, primitive-alloc-leaf-s-rbf-descriptor,
                     total-size, class-wrapper,
                     number-slots, fill-value,
                     repeated-size, repeated-size-offset, byte-fill);
  end select
end;

define side-effect-free stateful indefinite-extent &runtime-primitive-descriptor primitive-allocate-in-awl-pool
    (number-words :: <raw-integer>,
     class-wrapper :: <object>,
     number-slots :: <raw-integer>,
     fill-value :: <object>,
     repeated-size :: <raw-integer>,
     repeated-size-offset :: <raw-integer>,
     assoc-link :: <object>)
  => (object :: <object>);
  let number-bytes = ins--mul(be, number-words, back-end-word-size(be));
  let wrapper = op--raw-pointer-cast(be, class-wrapper);
  let fill = op--raw-pointer-cast(be, fill-value);
  let assoc = op--raw-pointer-cast(be, assoc-link);
  call-primitive(be, primitive-alloc-exact-awl-s-r-descriptor,
                 number-bytes, wrapper, assoc, number-slots, fill,
                 repeated-size, repeated-size-offset);
end;

define side-effect-free stateful indefinite-extent &runtime-primitive-descriptor primitive-allocate-weak-in-awl-pool
    (number-words :: <raw-integer>,
     class-wrapper :: <object>,
     number-slots :: <raw-integer>,
     fill-value :: <object>,
     repeated-size :: <raw-integer>,
     repeated-size-offset :: <raw-integer>,
     assoc-link :: <object>)
  => (object :: <object>)
  let number-bytes = ins--mul(be, number-words, back-end-word-size(be));
  let wrapper = op--raw-pointer-cast(be, class-wrapper);
  let fill = op--raw-pointer-cast(be, fill-value);
  let assoc = op--raw-pointer-cast(be, assoc-link);
  call-primitive(be, primitive-alloc-weak-awl-s-r-descriptor,
                 number-bytes, wrapper, assoc, number-slots, fill,
                 repeated-size, repeated-size-offset);
end;


/// Breakpoints on class allocation

define side-effecting &c-primitive-descriptor primitive-set-class-breakpoint
  (dylan-class :: <object>, count :: <integer>) => ();
define side-effecting &c-primitive-descriptor primitive-clear-class-breakpoint
  (dylan-class :: <object>) => ();
define side-effecting &c-primitive-descriptor primitive-display-class-breakpoints
  (string-buffer :: <raw-byte-string>) => (number-written :: <raw-integer>);

/// GC

define side-effecting stateless dynamic-extent &primitive-descriptor primitive-pin-object
    (object :: <object>) => (object :: <object>);
  object                        // FIXME
end;

define side-effecting stateless dynamic-extent &primitive-descriptor primitive-unpin-object
    (object :: <object>) => ()
  //---*** Fill this in...
end;

define side-effecting &c-primitive-descriptor primitive-mps-finalize
  (object :: <object>) => ();
define side-effecting stateful &c-primitive-descriptor primitive-mps-finalization-queue-first
  () => (object :: <object>);
/*
define side-effecting stateful &c-primitive-descriptor primitive-mps-park
  () => ();
define side-effecting stateful &c-primitive-descriptor primitive-mps-clamp
  () => ();
*/
define side-effecting stateful &c-primitive-descriptor primitive-mps-release
  () => ();
define side-effecting stateful &c-primitive-descriptor primitive-mps-collect
  (print-stats? :: <raw-boolean>) => ();
define side-effecting stateful &c-primitive-descriptor primitive-mps-collection-stats
  (object :: <raw-pointer>) => (found? :: <raw-boolean>);
define side-effecting stateful &c-primitive-descriptor primitive-mps-enable-gc-messages
  () => ();

define side-effecting stateful &c-primitive-descriptor primitive-mps-committed
  () => (bytes :: <raw-integer>);

define side-effecting stateful &c-primitive-descriptor primitive-mps-begin-ramp-alloc
  () => ();
define side-effecting stateful &c-primitive-descriptor primitive-mps-end-ramp-alloc
  () => ();

define side-effecting stateful &c-primitive-descriptor primitive-mps-begin-ramp-alloc-all
  () => ();
define side-effecting stateful &c-primitive-descriptor primitive-mps-end-ramp-alloc-all
  () => ();

define side-effecting stateless dynamic-extent &c-primitive-descriptor primitive-mps-ld-reset
    (primitive-hash-state) => ();
define side-effecting stateless dynamic-extent &c-primitive-descriptor primitive-mps-ld-add
    (primitive-hash-state, object) => ();
define side-effecting stateless dynamic-extent &c-primitive-descriptor primitive-mps-ld-merge
    (primitive-hash-state-1, primitive-hash-state-2) => ();
define side-effect-free stateless dynamic-extent &c-primitive-descriptor primitive-mps-ld-isstale
    (primitive-hash-state) => (is-stale? :: <raw-integer>);

define side-effect-free stateful &unimplemented-primitive-descriptor primitive-allocation-count
  () => (count :: <raw-integer>);
  //---*** Fill this in...
end;

define side-effecting stateful &unimplemented-primitive-descriptor primitive-initialize-allocation-count
    () => ()
  //---*** Fill this in...
end;

define side-effecting stateful &c-primitive-descriptor primitive-begin-heap-alloc-stats
  () => ();
define side-effecting stateful &c-primitive-descriptor primitive-end-heap-alloc-stats
  (string-buffer :: <raw-byte-string>) => (number-read :: <raw-integer>);
