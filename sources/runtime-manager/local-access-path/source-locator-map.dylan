module:       access-path-implementation
synopsis:     Presenting the runtime view of source locations.
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// CONSTRUCT-SOURCE-LOCATION-MAP
//    Call the nub to read in the runtime's view of source locations.

define method construct-source-location-map
    (conn :: <local-access-connection>, sym :: <remote-symbol>) => ()

  if (instance?(sym, <remote-function>))
    // Okay, so we could have specialized on <remote-function> here,
    // and saved the nasty use of instance?. However, there's a
    // lurking potential for ambiguity, since we've already
    // specialized on <local-access-connection>.

    // We have a function, so build the map of source locations
    // if possible.

    let sl-table 
      = nub-fetch-source-locations(conn.connection-process, sym.remote-symbol-address,
                                   sym.remote-function-end);
    let sl-count
      = nub-number-of-source-locations(conn.connection-process, sl-table);

    if (sl-count > 0)
      let sl-vector = make(<vector>, size: sl-count);
      let fname-length
        = nub-source-location-filename-length(conn.connection-process, sl-table);
      let fname 
        = make(<byte-string>, size: fname-length);
      nub-source-location-filename
        (conn.connection-process, sl-table, fname-length, fname);

      // Fetch all source locations and store them in the vector.
      for (i from 0 below sl-count)
        let linenum = nub-source-location-linenumber
              (conn.connection-process, sl-table, i);
        let addr = nub-source-location-address
              (conn.connection-process, sl-table, i);
        sl-vector[i] := pair(linenum, addr)
      end for;

      let topline = head(sl-vector[0]);
      let topaddr = sym.remote-symbol-address;

      sym.remote-symbol-source-location-map :=
        make(<source-location-map>,
             filename: fname,
             function-symbol: sym,
             location-count: sl-count,
             base-linenumber: topline,
             base-address: topaddr,
             pairs: sl-vector);
    else
      sym.definitely-no-source-locations := #t;
      sym.remote-symbol-source-location-map := #f;
    end if;
    nub-dispose-source-locations(conn.connection-process, sl-table);
  else
    // For now, lets be uncooperative about non-function symbols,
    // and assume we won't want locators for them.
    sym.definitely-no-source-locations := #t;
    sym.remote-symbol-source-location-map := #f;
  end if;
end method;


///// RESOLVE-SOURCE-LOCATION
//    Using runtime information only, convert the given location in source
//    code to an instruction address in memory, if possible.
//    Paths should be a sequence of <string> objects describing pathnames.
//    Library should be a <remote-library> (if supplied), and searching will
//    be restricted to information stored for that library.

define method resolve-source-location-on-connection
    (conn :: <local-access-connection>, lib :: <remote-library>,
     filename :: <string>, line :: <integer>, col :: <integer>,
     paths :: <sequence>)
       => (code-location :: false-or(<remote-value>), exact? :: <boolean>)
  let (code-location :: <remote-value>,
       valid? :: <integer>,
       path? :: <integer>,
       search-path,
       precise? :: <integer>)
    = nub-resolve-source-location
         (conn.connection-process, 
          lib.nub-descriptor, 
          filename, 
          line, 
          col);
  if (valid? == 1)
    values(code-location, precise? == 1)
  else
    values(#f, #f);
  end if;
end method;
