module:       remote-access-path
synopsis:     Presenting the runtime view of source locations.
author:       Paul Howard, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// CONSTRUCT-SOURCE-LOCATION-MAP

define method construct-source-location-map
    (conn :: <remote-access-connection>, sym :: <remote-symbol>) => ()

  if (instance?(sym, <remote-function>))

    // We have a function, so build the map of source locations
    // if possible.

    let sl-table :: <RNUBHANDLE>
      = Rtmgr/RemoteNub/fetch-source-locations
          (conn.nub, as-integer(sym.remote-symbol-address),
           as-integer(sym.remote-function-end));
    let sl-count
      = Rtmgr/RemoteNub/number-of-source-locations(conn.nub, sl-table);

    if (sl-count > 0)
      let sl-vector = make(<vector>, size: sl-count);
      let fname :: <string> =
	Rtmgr/RemoteNub/source-location-filename(conn.nub, sl-table);

      // Fetch all source locations and store them in the vector.
      for (i :: <integer> from 0 below sl-count)
        let linenum :: <integer> =
	  Rtmgr/RemoteNub/source-location-linenumber(conn.nub, sl-table, i);
        let addr :: <integer> =
	  Rtmgr/RemoteNub/source-location-address(conn.nub, sl-table, i);
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
    Rtmgr/RemoteNub/dispose-source-locations(conn.nub, sl-table);
  else
    // For now, lets be uncooperative about non-function symbols,
    // and assume we won't want locators for them.
    sym.definitely-no-source-locations := #t;
    sym.remote-symbol-source-location-map := #f;
  end if;
end method;


///// RESOLVE-SOURCE-LOCATION

define method resolve-source-location-on-connection
    (conn :: <remote-access-connection>, lib :: <remote-library>,
     filename :: <string>, line :: <integer>, col :: <integer>,
     paths :: <sequence>)
       => (code-location :: false-or(<remote-value>), exact? :: <boolean>)
  let (code-location :: <RTARGET-ADDRESS>,
       valid? :: <integer>,
       path? :: <integer>,
       search-path,
       precise? :: <integer>)
    = Rtmgr/RemoteNub/resolve-source-location
         (conn.nub, 
          lib.rnub-descriptor, 
          filename, 
          line, 
          col);
  if (valid? == 1)
    values(as-remote-value(code-location), precise? == 1)
  else
    values(#f, #f);
  end if;
end method;