Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2014 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// SYMBOL BOOTING

// A <simple-object-vector> of symbols interned during the
// initialization of the dylan library
define runtime-variable %oblist :: <simple-object-vector> = &unbound,
  section: #"ambiguous-data";

// Size of the %oblist vector in elements
define runtime-variable %oblist_size :: <raw-integer>
  = make-raw-literal(0);

// Element index of the first unused vector element
define runtime-variable %oblist_cursor :: <raw-integer>
  = make-raw-literal(0);

define method op--lookup-symbol
    (be :: <llvm-back-end>, name :: <llvm-value>)
 => (interned-obj :: <llvm-value>);
  let m = be.llvm-builder-module;
  let word-size = back-end-word-size(be);

  // Obtain the looked-up name string and its length
  let name-cast = op--object-pointer-cast(be, name, #"<byte-string>");
  let size-slot-ptr
    = op--getslotptr(be, name-cast, #"<byte-string>", #"size");
  let name-len = ins--load(be, size-slot-ptr, alignment: word-size);
  let name-len-raw = op--untag-integer(be, name-len);
  let name-string
    = op--getslotptr(be, name-cast, #"<byte-string>", #"string-element", 0);

  // Retrieve the oblist vector and the cursor index value
  let oblist
    = ins--load(be, llvm-runtime-variable(be, m, %oblist-descriptor));
  let oblist-cast
    = op--object-pointer-cast(be, oblist, #"<simple-object-vector>");
  let cursor
    = ins--load(be, llvm-runtime-variable(be, m, %oblist_cursor-descriptor));

  ins--iterate loop (be, i = 0)
    let cmp = ins--icmp-ult(be, i, cursor);
    ins--if (be, cmp)
      let i-inc = ins--add(be, i, 1);

      // Retrieve the oblist symbol and its name
      let e
        = call-primitive(be, primitive-vector-element-descriptor,
                         oblist-cast, i);
      let sym = op--object-pointer-cast(be, e, #"<symbol>");
      let sym-name-ptr
        = op--getslotptr(be, sym, #"<symbol>", #"symbol-name");
      let sym-name = ins--load(be, sym-name-ptr, alignment: word-size);
      let sym-name-cast
        = op--object-pointer-cast(be, sym-name, #"<byte-string>");

      // Retrieve the oblist symbol
      let sym-name-size-ptr
        = op--getslotptr(be, sym-name-cast, #"<byte-string>", #"size");
      let sym-name-len = ins--load(be, sym-name-size-ptr, alignment: word-size);

      // Compare name lengths
      let name-len-cmp = ins--icmp-eq(be, name-len, sym-name-len);
      ins--if (be, name-len-cmp)
        // Compare name strings
        let sym-name-string
          = op--getslotptr(be, sym-name-cast, #"<byte-string>",
                           #"string-element", 0);
        let name-strcmp
          = call-primitive(be, primitive-strncasecmp-descriptor,
                           name-string, sym-name-string, name-len-raw);
        let name-cmp = ins--icmp-eq(be, name-strcmp, 0);
        ins--if (be, name-cmp)
          e                     // This was it
        ins--else
          loop(i-inc)
        end ins--if
      ins--else
        loop(i-inc)
      end ins--if
    ins--else
      emit-reference(be, m, &false)
    end ins--if
  end
end method;

define method op--as-lowercase
    (be :: <llvm-back-end>, c :: <llvm-value>)
 => (lower :: <llvm-value>);
  let c-ext = ins--zext(be, c, be.%type-table["iWord"]);
  let c-off = ins--sub(be, c-ext, as(<integer>, 'A'));
  let c-cmp = ins--icmp-ult(be, c-off, 26);
  let c-add = ins--add(be, c-ext, as(<integer>, 'a') - as(<integer>, 'A'));
  ins--select(be, c-cmp, c-add, c-ext)
end method;

define side-effect-free stateless dynamic-extent auxiliary &runtime-primitive-descriptor primitive-strncasecmp
    (s1 :: <raw-byte-string>, s2 :: <raw-byte-string>, n :: <raw-integer>)
 => (result :: <raw-integer>)
  ins--iterate loop (be, n = n, s1 = s1, s2 = s2)
    let cmp-n = ins--icmp-ne(be, n, 0);
    ins--if (be, cmp-n)
      // Compare lowercased corresponding characters
      let s1-c = ins--load(be, s1);
      let s1-c-lc = op--as-lowercase(be, s1-c);
      let s2-c = ins--load(be, s2);
      let s2-c-lc = op--as-lowercase(be, s2-c);
      let sub = ins--sub(be, s1-c-lc, s2-c-lc);
      let cmp = ins--icmp-eq(be, sub, 0);
      ins--if (be, cmp)
        // Still equal, so advance and continue
        let n-dec = ins--sub(be, n, 1);
        let s1-inc = ins--gep-inbounds(be, s1, 1);
        let s2-inc = ins--gep-inbounds(be, s2, 1);
        loop(n-dec, s1-inc, s2-inc)
      ins--else
        sub
      end ins--if
    ins--else
      llvm-back-end-value-function(be, 0)
    end ins--if
  end ins--iterate
end;

define method op--register-symbol
    (be :: <llvm-back-end>, symbol :: <llvm-value>) => ();
  let m = be.llvm-builder-module;
  let word-size = back-end-word-size(be);

  let oblist
    = ins--load(be, llvm-runtime-variable(be, m, %oblist-descriptor));
  let oblist-cast
    = op--object-pointer-cast(be, oblist, #"<simple-object-vector>");

  let cursor-ref = llvm-runtime-variable(be, m, %oblist_cursor-descriptor);
  let cursor = ins--load(be, cursor-ref);

  // Will it fit?
  let oblist-size-ref = llvm-runtime-variable(be, m, %oblist_size-descriptor);
  let oblist-size = ins--load(be, oblist-size-ref);
  let cmp = ins--icmp-ult(be, cursor, oblist-size);
  let oblist-cast
    = ins--if (be, cmp)
        oblist-cast
      ins--else
        // Allocate a new oblist vector, the old one wasn't big enough
        let cmp-zero = ins--icmp-eq(be, oblist-size, 0);
        let expanded-size
          = ins--if (be, cmp-zero)
              // Initial oblist size
              llvm-back-end-value-function(be, 256)
            ins--else
              // Grow existing oblist by 50%
              let thrice = ins--mul(be, oblist-size, 3);
              ins--ashr(be, thrice, 1)
            end ins--if;
        ins--store(be, expanded-size, oblist-size-ref);
        let new-oblist = op--allocate-vector(be, expanded-size);

        // Copy elements of the old vector into the new one
        let sov-class :: <&class> = dylan-value(#"<simple-object-vector>");
        let new-oblist-cast
          = op--object-pointer-cast(be, new-oblist, sov-class);
        let dst-slot-ptr
          = op--getslotptr(be, new-oblist-cast, sov-class, #"vector-element");
        let dst-byte-ptr = ins--bitcast(be, dst-slot-ptr, $llvm-i8*-type);

        let src-slot-ptr
          = op--getslotptr(be, oblist-cast, sov-class, #"vector-element");
        let src-byte-ptr = ins--bitcast(be, src-slot-ptr, $llvm-i8*-type);

        let oblist-byte-size = ins--mul(be, cursor, word-size);
        ins--call-intrinsic(be, "llvm.memcpy",
                            vector(dst-byte-ptr, src-byte-ptr, oblist-byte-size,
                                   i32(word-size), $llvm-false));
        new-oblist-cast
      end ins--if;

  // Store the symbol in the oblist
  call-primitive(be, primitive-vector-element-setter-descriptor,
                 symbol, oblist-cast, cursor);
  let cursor-inc = ins--add(be, cursor, 1);
  ins--store(be, cursor-inc, cursor-ref);
end method;

define side-effecting stateless indefinite-extent &runtime-primitive-descriptor primitive-resolve-symbol
    (symbol :: <symbol>) => (canonical-symbol :: <symbol>);
  let module = be.llvm-builder-module;

  // Get the name
  let symbol-cast = op--object-pointer-cast(be, symbol, #"<symbol>");
  let symbol-name-slot-ptr
    = op--getslotptr(be, symbol-cast, #"<symbol>", #"symbol-name");
  let name-obj
    = ins--load(be, symbol-name-slot-ptr, alignment: back-end-word-size(be));
  let name = op--object-pointer-cast(be, name-obj, #"<byte-string>");

  // Look to see if we already have the symbol
  let interned-obj = op--lookup-symbol(be, name);
  let cmp = ins--icmp-ne(be, interned-obj, emit-reference(be, module, &false));
  ins--if (be, cmp)
    // A symbol with this name was already interned
    interned-obj
  ins--else
    // If we don't, then we must intern the one we have
    op--register-symbol(be, symbol);
    symbol
  end ins--if
end;

define side-effect-free stateless dynamic-extent &runtime-primitive-descriptor primitive-string-as-symbol
    (string :: <byte-string>) => (symbol :: <symbol>);
  let header-words = dylan-value(#"$number-header-words");
  let symbol-class :: <&class> = dylan-value(#"<symbol>");
  let m = be.llvm-builder-module;

  // Look to see if we already have the symbol
  let interned-obj = op--lookup-symbol(be, string);
  let cmp = ins--icmp-ne(be, interned-obj, emit-reference(be, m, &false));
  ins--if (be, cmp)
    // A symbol with this name was already interned
    interned-obj
  ins--else
    // If we don't, then we must intern a new one
    let total-size = header-words + ^instance-storage-size(symbol-class);
    let byte-size = total-size * back-end-word-size(be);
    let symbol
      = call-primitive(be, primitive-alloc-s1-descriptor,
                       byte-size,
                       emit-reference(be, m, ^class-mm-wrapper(symbol-class)),
                       string);
    op--register-symbol(be, symbol);
    symbol
  end ins--if
end;

define side-effect-free stateless dynamic-extent &runtime-primitive-descriptor primitive-preboot-symbols
    () => (res :: <simple-object-vector>);
  let sov-class :: <&class> = dylan-value(#"<simple-object-vector>");
  let m = be.llvm-builder-module;
  let word-size = back-end-word-size(be);

  let oblist
    = ins--load(be, llvm-runtime-variable(be, m, %oblist-descriptor));
  let oblist-cast = op--object-pointer-cast(be, oblist, sov-class);

  let cursor-ref = llvm-runtime-variable(be, m, %oblist_cursor-descriptor);
  let cursor = ins--load(be, cursor-ref);

  // Allocate a new vector with the right number of elements
  let res = op--allocate-vector(be, cursor);

  // Copy elements of the old vector into the new one
  let res-cast = op--object-pointer-cast(be, res, sov-class);
  let dst-slot-ptr
    = op--getslotptr(be, res-cast, sov-class, #"vector-element");
  let dst-byte-ptr = ins--bitcast(be, dst-slot-ptr, $llvm-i8*-type);

  let src-slot-ptr
    = op--getslotptr(be, oblist-cast, sov-class, #"vector-element");
  let src-byte-ptr = ins--bitcast(be, src-slot-ptr, $llvm-i8*-type);

  let oblist-byte-size = ins--mul(be, cursor, word-size);
  ins--call-intrinsic(be, "llvm.memcpy",
                      vector(dst-byte-ptr, src-byte-ptr, oblist-byte-size,
                             i32(word-size), $llvm-false));
  res
end;
