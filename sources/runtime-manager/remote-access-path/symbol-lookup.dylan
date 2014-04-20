module:     remote-access-path
synopsis:   Functions for looking up remote symbols
author:     Paul Howard, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



///// NEAREST-SYMBOLS-FROM-NUB

define method nearest-symbols-from-nub 
    (conn :: <remote-access-connection>, address :: <remote-value>) 
      => (any-luck? :: <boolean>,
          where-is-it? :: <NUBLIBRARY>,
          gimme-handle :: <NUBHANDLE>)

  // Wraps up the debugger nub function which does a brute-force
  // search in the debug map for the three nearest symbols and
  // constructs an opaque lookup table which is passed back later
  // to get information about the symbols.

  let worked = #f;
  let (success, lib :: <RNUBLIBRARY>, table :: <RNUBHANDLE>) =
    Rtmgr/RemoteNub/nearest-symbols(conn.nub, as-integer(address));
  if (success = 1)
    worked := #t
  end if;
  values (worked, as-remote-pointer(lib), as-remote-pointer(table));
end method;


///// COLLECT-NEAREST-SYMBOLS

define method collect-nearest-symbols 
    (conn :: <remote-access-connection>, lib :: <remote-library>,
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

  let lookups = as-integer(lookups);
  let addr1 :: <RTARGET-ADDRESS>
    = Rtmgr/RemoteNub/lookup-symbol-address (conn.nub, lookups, 1);
  let is-func1
    = Rtmgr/RemoteNub/symbol-is-function (conn.nub, lookups, 1);
  let sym1 = #f;
  let name1 =
    Rtmgr/RemoteNub/lookup-symbol-name(conn.nub, lookups, 1);
  let lang1 = classify-symbolic-name(conn, name1);
  if (is-func1 == 1)
    let debug-start :: <RTARGET-ADDRESS>
      = Rtmgr/RemoteNub/lookup-function-debug-start(conn.nub, lookups, 1);
    let debug-end :: <RTARGET-ADDRESS>
      = Rtmgr/RemoteNub/lookup-function-debug-end(conn.nub, lookups, 1);
    let last-addr :: <RTARGET-ADDRESS>
      = Rtmgr/RemoteNub/lookup-function-end(conn.nub, lookups, 1);
    sym1 := make (<remote-function>,
                  name: name1,
                  address: as-remote-value(addr1),
                  language: lang1,
                  library: lib,
                  debug-start: as-remote-value(debug-start),
		  debug-end: as-remote-value(debug-end),
                  absolute-end: as-remote-value(last-addr));
  else
    sym1 := make (<remote-symbol>,
                  name: name1,
                  address: as-remote-value(addr1),
                  language: lang1,
                  library: lib);
  end if;

  let addr2 :: <RTARGET-ADDRESS>
    = Rtmgr/RemoteNub/lookup-symbol-address (conn.nub, lookups, 2);
  let is-func2
    = Rtmgr/RemoteNub/symbol-is-function (conn.nub, lookups, 2);
  let sym2 = #f;
  let name2 =
    Rtmgr/RemoteNub/lookup-symbol-name(conn.nub, lookups, 2);
  let lang2 = classify-symbolic-name(conn, name2);
  if (is-func2 == 1)
    let debug-start :: <RTARGET-ADDRESS>
      = Rtmgr/RemoteNub/lookup-function-debug-start(conn.nub, lookups, 2);
    let debug-end :: <RTARGET-ADDRESS>
      = Rtmgr/RemoteNub/lookup-function-debug-end(conn.nub, lookups, 2);
    let last-addr :: <RTARGET-ADDRESS>
      = Rtmgr/RemoteNub/lookup-function-end(conn.nub, lookups, 2);
    sym2 := make (<remote-function>,
                  name: name2,
                  address: as-remote-value(addr2),
                  language: lang2,
                  library: lib,
                  debug-start: as-remote-value(debug-start),
		  debug-end: as-remote-value(debug-end),
                  absolute-end: as-remote-value(last-addr));
  else
    sym2 := make (<remote-symbol>,
                  name: name2,
                  address: as-remote-value(addr2),
                  language: lang2,
                  library: lib);
  end if;

  let addr3 :: <RTARGET-ADDRESS>
    = Rtmgr/RemoteNub/lookup-symbol-address (conn.nub, lookups, 3);
  let is-func3
    = Rtmgr/RemoteNub/symbol-is-function (conn.nub, lookups, 3);
  let sym3 = #f;
  let name3 =
    Rtmgr/RemoteNub/lookup-symbol-name(conn.nub, lookups, 3);
  let lang3 = classify-symbolic-name(conn, name3);
  if (is-func3 == 1)
    let debug-start :: <RTARGET-ADDRESS>
      = Rtmgr/RemoteNub/lookup-function-debug-start(conn.nub, lookups, 3);
    let debug-end :: <RTARGET-ADDRESS>
      = Rtmgr/RemoteNub/lookup-function-debug-end(conn.nub, lookups, 3);
    let last-addr :: <RTARGET-ADDRESS>
      = Rtmgr/RemoteNub/lookup-function-end(conn.nub, lookups, 3);
    sym3 := make (<remote-function>,
                  name: name3,
                  address: as-remote-value(addr3),
                  language: lang3,
                  library: lib,
                  debug-start: as-remote-value(debug-start),
		  debug-end: as-remote-value(debug-end),
                  absolute-end: as-remote-value(last-addr));
  else
    sym3 := make (<remote-symbol>,
                  name: name3,
                  address: as-remote-value(addr3),
                  language: lang3,
                  library: lib);
  end if;

  values (sym1, sym2, sym3);
end method;


///// FIND-SYMBOL-IN-LIBRARY

define method find-symbol-in-library 
    (conn :: <remote-access-connection>,
     lib :: <remote-library>,
     name :: <string>) 
       => (maybe-sym :: false-or(<remote-symbol>))

  let (found, address :: <RTARGET-ADDRESS>, type, is-func,
       debug-start :: <RTARGET-ADDRESS>, debug-end :: <RTARGET-ADDRESS>,
       lang, lasta :: <RTARGET-ADDRESS>)
    = Rtmgr/RemoteNub/find-symbol-in-library
      (conn.nub, lib.rnub-descriptor, size(name), name);

  if (found == 1)
    if (is-func == 1)
      make (<remote-function>, name: name, address: as-remote-value(address),
            language: classify-symbolic-name(conn, name),
            library: lib, debug-start: as-remote-value(debug-start),
            debug-end: as-remote-value(debug-end), absolute-end: as-remote-value(lasta));
    else
      make (<remote-symbol>, name: name, address: as-remote-value(address),  
            language: classify-symbolic-name(conn, name), library: lib);
    end if;
  else
    #f
  end if
end method;


///// SYMBOL-RELATIVE-ADDRESS

define method symbol-relative-address-on-connection
  (conn :: <remote-access-connection>, 
   path :: <access-path>, address :: <remote-value>)
     => (sym-if-found :: false-or(<remote-symbol>),
         offset       :: <abstract-integer>)

  // Call the debugger nub.

  let (foundit :: <abstract-integer>,
       lib :: <RNUBLIBRARY>,
       addr :: <RTARGET-ADDRESS>,
       offset :: <abstract-integer>,
       name-length :: <abstract-integer>,
       type :: <abstract-integer>,
       is-function :: <abstract-integer>,
       debug-start :: <RTARGET-ADDRESS>,
       debug-end :: <RTARGET-ADDRESS>,
       language :: <abstract-integer>,
       last-address :: <RTARGET-ADDRESS>) = 
          Rtmgr/RemoteNub/closest-symbol(conn.nub, as-integer(address));

  if (foundit == 1)
    let lib = as-remote-pointer(lib);
    let addr = as-remote-value(addr);
    let debug-start = as-remote-value(debug-start);
    let debug-end = as-remote-value(debug-end);
    let last-address = as-remote-value(last-address);
    let remote-lib = find-or-make-library(path, lib);
    let sym = #f;
    let sym-name =
      Rtmgr/RemoteNub/closest-symbol-name(conn.nub, name-length);
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

define method function-bounding-addresses-on-connection
    (conn :: <remote-access-connection>, address :: <remote-value>)
       => (lowerbound :: <remote-value>, upperbound :: <remote-value>)
  let (lowerbound, upperbound)
    = Rtmgr/RemoteNub/function-bounding-addresses(conn.nub, as-integer(address));
  values(as-remote-value(lowerbound), as-remote-value(upperbound));
end method;
