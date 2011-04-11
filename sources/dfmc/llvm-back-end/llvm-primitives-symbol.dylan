Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
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

define side-effecting stateless indefinite-extent mapped &runtime-primitive-descriptor primitive-resolve-symbol
    (symbol :: <symbol>) => (canonical-symbol :: <symbol>);
  let module = be.llvm-builder-module;

  // basic blocks
  let interned-found = make(<llvm-basic-block>);
  let interned-not-found = make(<llvm-basic-block>);
  let return-common = make(<llvm-basic-block>);

  // Get the name
  let symbol-name-slot-ptr
    = op--getslotptr(be, symbol, #"<symbol>", #"symbol-name");
  let name-obj
    = ins--load(be, symbol-name-slot-ptr, alignment: back-end-word-size(be));
  let name
    = op--object-pointer-cast(be, name-obj, #"<byte-string>");

  // Look to see if we already have the symbol
  let interned-obj = op--lookup-symbol(be, name);
  let cmp = ins--icmp-ne(be, interned-obj, emit-reference(be, module, &false));
  ins--br(be, cmp, interned-found, interned-not-found);

  // A symbol with this name was already interned
  ins--block(be, interned-found);
  let interned = op--object-pointer-cast(be, interned-obj, #"<symbol>");
  ins--br(be, return-common);

  // If we don't, then we must intern the one we have
  ins--block(be, interned-not-found);
  op--register-symbol(be, symbol);
  ins--br(be, return-common);

  // Now we have an interned symbol; return it
  ins--block(be, return-common);
  ins--phi(be, interned, interned-found, symbol, interned-not-found)
end;

define method op--lookup-symbol
    (be :: <llvm-back-end>, name :: <llvm-value>)
 => (interned-obj :: <llvm-value>);
  let module = be.llvm-builder-module;

  //---*** Fill this in...
  emit-reference(be, module, &false)
end method;

define method op--register-symbol
    (be :: <llvm-back-end>, symbol :: <llvm-value>) => ();
  //---*** Fill this in...
end method;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-string-as-symbol
    (string :: <byte-string>) => (symbol :: <symbol>);
  //---*** Fill this in...
end;

define side-effect-free stateless dynamic-extent &unimplemented-primitive-descriptor primitive-preboot-symbols
    () => (res :: <simple-object-vector>);
  //---*** Fill this in...
end;
