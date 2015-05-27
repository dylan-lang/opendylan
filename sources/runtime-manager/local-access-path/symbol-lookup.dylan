module:     access-path-implementation
synopsis:   Functions for looking up remote symbols
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// NEAREST-SYMBOLS-FROM-NUB

define method nearest-symbols-from-nub 
    (conn :: <local-access-connection>, address :: <remote-value>) 
      => (any-luck? :: <boolean>,
          where-is-it? :: <NUBLIBRARY>,
          gimme-handle :: <NUBHANDLE>)

  // Wraps up the debugger nub function which does a brute-force
  // search in the debug map for the three nearest symbols and
  // constructs an opaque lookup table which is passed back later
  // to get information about the symbols.

  let worked = #f;
  let (success, lib, table) = nub-nearest-symbols (conn.connection-process, address);
  if (success = 1)
    worked := #t
  end if;
  values (worked, lib, table);
end method;


///// COLLECT-NEAREST-SYMBOLS

define method collect-nearest-symbols 
    (conn :: <local-access-connection>, lib :: <remote-library>,
     lookups :: <NUBHANDLE>)
       => (closest :: <remote-symbol>,
           prev :: <remote-symbol>,
           nxt :: <remote-symbol>)

  // For nearest-symbols, the nub creates a quick lookup table with
  // three entries: 1. The nearest symbol, 2. The closest symbol
  // preceding the nearest, 3. The closest symbol following the
  // nearest (in that order).

  // Pull nub information for the nearest symbol, and make a
  // <remote-symbol> object to represent it.

  // Symbol #1 is the closest
       
  let name-length1
    = nub-lookup-symbol-name-length(conn.connection-process, lookups, 1);
  let name1 
    = make(<byte-string>,
           size: name-length1); 
  let addr1 
    = nub-lookup-symbol-address (conn.connection-process, lookups, 1);
  let is-func1
    = nub-symbol-is-function (conn.connection-process, lookups, 1);
  let sym1 = #f;
  nub-lookup-symbol-name(conn.connection-process, lookups, 1, name-length1, name1);
  let lang1 = classify-symbolic-name(conn, name1);
  if (is-func1 == 1)
    let debug-start 
      = nub-lookup-function-debug-start(conn.connection-process, lookups, 1);
    let debug-end
      = nub-lookup-function-debug-end(conn.connection-process, lookups, 1);
    let last-addr
      = nub-lookup-function-end(conn.connection-process, lookups, 1);
    sym1 := make (<remote-function>,
                  name: name1,
                  address: addr1,
                  language: lang1,
                  library: lib,
                  debug-start: debug-start,
                  debug-end: debug-end,
                  absolute-end: last-addr);
  else
    sym1 := make (<remote-symbol>,
                  name: name1,
                  address: addr1,
                  language: lang1,
                  library: lib);
  end if;

  let name-length2
    = nub-lookup-symbol-name-length(conn.connection-process, lookups, 2);
  let name2
    = make(<byte-string>,
           size: name-length2); 
  let addr2
    = nub-lookup-symbol-address (conn.connection-process, lookups, 2);
  let is-func2
    = nub-symbol-is-function (conn.connection-process, lookups, 2);
  let sym2 = #f;
  nub-lookup-symbol-name(conn.connection-process, lookups, 2, name-length2, name2);
  let lang2 = classify-symbolic-name(conn, name2);
  if (is-func2 == 1)
    let debug-start 
      = nub-lookup-function-debug-start(conn.connection-process, lookups, 2);
    let debug-end
      = nub-lookup-function-debug-end(conn.connection-process, lookups, 2);
    let last-addr
      = nub-lookup-function-end(conn.connection-process, lookups, 2);
    sym2 := make (<remote-function>,
                  name: name2,
                  address: addr2,
                  language: lang2,
                  library: lib,
                  debug-start: debug-start,
                  debug-end: debug-end,
                  absolute-end: last-addr);
  else
    sym2 := make (<remote-symbol>,
                  name: name2,
                  address: addr2,
                  language: lang2,
                  library: lib);
  end if;

  let name-length3
    = nub-lookup-symbol-name-length(conn.connection-process, lookups, 3);
  let name3 
    = make(<byte-string>,
           size: name-length3); 
  let addr3 
    = nub-lookup-symbol-address (conn.connection-process, lookups, 3);
  let is-func3
    = nub-symbol-is-function (conn.connection-process, lookups, 3);
  let sym3 = #f;
  nub-lookup-symbol-name(conn.connection-process, lookups, 3, name-length3, name3);
  let lang3 = classify-symbolic-name(conn, name3);
  if (is-func3 == 1)
    let debug-start 
      = nub-lookup-function-debug-start(conn.connection-process, lookups, 3);
    let debug-end
      = nub-lookup-function-debug-end(conn.connection-process, lookups, 3);
    let last-addr
      = nub-lookup-function-end(conn.connection-process, lookups, 3);
    sym3 := make (<remote-function>,
                  name: name3,
                  address: addr3,
                  language: lang3,
                  library: lib,
                  debug-start: debug-start,
                  debug-end: debug-end,
                  absolute-end: last-addr);
  else
    sym3 := make (<remote-symbol>,
                  name: name3,
                  address: addr3,
                  language: lang3,
                  library: lib);
  end if;

  values (sym1, sym2, sym3);
end method;


///// FIND-SYMBOL-IN-LIBRARY

define method find-symbol-in-library 
    (conn :: <local-access-connection>,
     lib :: <remote-library>,
     name :: <string>) 
       => (maybe-sym :: false-or(<remote-symbol>))

  let (found, address, type, is-func, debug-start, debug-end, lang, lasta)
    = nub-find-symbol-in-library (conn.connection-process, lib.nub-descriptor,
                                  size(name), name);

  if (found == 1)
    if (is-func == 1)
      make (<remote-function>, name: name, address: address,
            language: classify-symbolic-name(conn, name),
            library: lib, debug-start: debug-start,
            debug-end: debug-end, absolute-end: lasta);
    else
      make (<remote-symbol>, name: name, address: address,  
            language: classify-symbolic-name(conn, name), library: lib);
    end if;
  else
    #f
  end if
end method;


///// SYMBOL-RELATIVE-ADDRESS
//    Given an address as a <remote-value>, attempts to convert this to
//    a pair of <remote-symbol> and integer byte offset.

define method symbol-relative-address-on-connection
  (conn :: <local-access-connection>, 
   path :: <access-path>, address :: <remote-value>)
     => (sym-if-found :: false-or(<remote-symbol>),
         offset       :: <ffi-integer>)

  // Call the debugger nub.

  let (foundit :: <ffi-integer>,
       lib :: <NUBLIBRARY>,
       addr :: <remote-value>,
       offset :: <ffi-integer>,
       name-length :: <ffi-integer>,
       type :: <ffi-integer>,
       is-function :: <ffi-integer>,
       debug-start :: <remote-value>,
       debug-end :: <remote-value>,
       language :: <ffi-integer>,
       last-address :: <remote-value>) = 
          nub-closest-symbol(conn.connection-process, address);

  if (foundit == 1)
    let remote-lib = find-or-make-library(path, lib);
    let sym = #f;
    let sym-name = make(<byte-string>, size: name-length);
    nub-closest-symbol-name(conn.connection-process, name-length, sym-name);
    let language-classification = classify-symbolic-name(conn, sym-name);
    if (is-function == 1)
      sym := make(<remote-function>,
                  name: sym-name,
                  address: addr,
                  library: remote-lib,
                  language: language-classification,
                  debug-start: debug-start,
                  debug-end: debug-end,
                  absolute-end: last-address);
    else
      sym := make(<remote-symbol>,
                  name: sym-name,
                  address: addr,
                  library: remote-lib,
                  language: language-classification);
    end if;
    values(sym, offset);
  else
    values(#f, 0);
  end if
end method;

///// FUNCTION-BOUNDING-ADDRESSES
//    Returns the addresses that delimit the definition of a function.

define method function-bounding-addresses-on-connection
    (conn :: <local-access-connection>, address :: <remote-value>)
       => (lowerbound :: <remote-value>, upperbound :: <remote-value>)
  let (lowerbound, upperbound)
    = nub-function-bounding-addresses(conn.connection-process, address);
  values(lowerbound, upperbound);
end method;
