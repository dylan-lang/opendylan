module:       access-path-implementation
synopsis:     Presenting the runtime view of source locations.
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <SOURCE-LOCATION-MAP>
//    Encapsulates the source locations for a function. They must all
//    be in the same file (surely!!).

define sealed class <source-location-map> (<object>)

  constant slot source-filename :: <byte-string>,
    required-init-keyword: filename:;

  constant slot associated-function :: <remote-symbol>,
    required-init-keyword: function-symbol:;

  constant slot number-of-locations :: <integer>,
    required-init-keyword: location-count:;

  constant slot base-linenumber :: <integer>,
    required-init-keyword: base-linenumber:;

  constant slot base-address :: <remote-value>,
    required-init-keyword: base-address:;

  constant slot line-offset-pairs :: <vector>,
    required-init-keyword: pairs:;

end class;


///// EXPORTED GENERIC FUNCTIONS

define generic source-location-description
    (slm :: <source-location-map>, i :: <integer>)
       => (function-relative-linenumber :: <integer>,
           absolute-address :: <remote-value>);

define generic nearest-source-locations
    (ap :: <access-path>, slm :: <source-location-map>, ip :: <remote-value>)
       => (exact :: false-or(<integer>),
           nearest-ahead :: false-or(<integer>),
           nearest-behind :: false-or(<integer>));

define generic function-source-location-map
    (ap :: <access-path>, sym :: <remote-symbol>)
       => (maybe-slm :: false-or(<source-location-map>));


/*
///// EXTRACT-PATHNAME-LEAF
//    We are only interested in actual filenames. Any pathname information
//    should be stripped. Work backwards from the right until encountering
//    a slash (in either direction), or the start of the string.

define method extract-pathname-leaf (s :: <string>) => (l :: <string>)
  let l = "";
  let i = size(s) - 1;
  while ((i >= 0) & (s[i] ~== '/') & (s[i] ~== '\\'))
    l := concatenate(add!("", s[i]), l);
    i := i - 1;
  end while;
  l;
end method;
*/

///// CONSTRUCT-SOURCE-LOCATION-MAP
//    Call the nub to read in the runtime's view of source locations.

define open generic construct-source-location-map
    (conn :: <access-connection>, sym :: <remote-symbol>) => ();

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



///// FUNCTION-SOURCE-LOCATION-MAP

define method function-source-location-map
    (ap :: <access-path>, sym :: <remote-symbol>)
      => (slm :: false-or(<source-location-map>))
  if (sym.remote-symbol-source-location-map)
    sym.remote-symbol-source-location-map
  elseif (sym.definitely-no-source-locations)
    #f
  else
    construct-source-location-map(ap.connection, sym);
    sym.remote-symbol-source-location-map;
  end if;
end method;


///// FUNCTION-RECORDED-SOURCE-LOCATIONS
//    A convenience function.
//    If the given symbol denotes a function, and the runtime debug tables
//    contain one or more source location positions belonging to that 
//    function, this will return:
//      - The name of the source file, exactly how it is represented in the
//        debug info. This contains a completely undefined amount of
//        path qualification.
//      - The linenumber in that file for the function's first source location.
//      - The address of the function.
//      - Two parallel sequences: line-offsets from the base linenumber, and
//        corresponding code offsets from the base address.

define method function-recorded-source-locations
    (ap :: <access-path>, sym :: <remote-symbol>)
       => (filename :: false-or(<byte-string>),
           base-linenumber :: <integer>,
           base-address :: <remote-value>,
           line-positions :: <sequence>,
           code-offsets :: <sequence>)
  unless (sym.remote-symbol-source-location-map)
    unless (sym.definitely-no-source-locations)
      construct-source-location-map(ap.connection, sym);
    end unless
  end unless;
  if (sym.remote-symbol-source-location-map)
    let slm = sym.remote-symbol-source-location-map;
    values
      (slm.source-filename,
       slm.base-linenumber,
       sym.remote-symbol-address,
       map(method (line-offset-pair :: <pair>) => (rel-line :: <integer>)
             let absolute-line = head(line-offset-pair);
             absolute-line - slm.base-linenumber
           end method,
           slm.line-offset-pairs),
       map(tail, slm.line-offset-pairs))
  else
    values(#f, 0, sym.remote-symbol-address, #[], #[])
  end if
end method;


///// SOURCE-LOCATION-DESCRIPTION

define method source-location-description
    (slm :: <source-location-map>, i :: <integer>)
      => (function-relative-linenumber :: <integer>,
          address :: <remote-value>)
  if ((i < slm.number-of-locations) & (i >= 0))
    values(head(slm.line-offset-pairs[i]) - slm.base-linenumber,
           byte-indexed-remote-value
              (slm.associated-function.remote-symbol-address,
               if (instance?(tail(slm.line-offset-pairs[i]), <integer>))
                 tail(slm.line-offset-pairs[i])
               else
                 0
               end if));
  else
    error("Tried to find more source locations than contained in table");
  end if;
end method;


define method nearest-source-locations
    (ap :: <access-path>, slm :: <source-location-map>, ip :: <remote-value>)
       => (exact :: false-or(<integer>),
           nearest-ahead :: false-or(<integer>),
           nearest-behind :: false-or(<integer>))
  let exact = #f;
  let nearest-ahead = #f;
  let address-ahead = #f;
  let nearest-behind = #f;
  let address-behind = #f;
  let i = 0;
  for (line-offset-pair in slm.line-offset-pairs)
    let this-address = 
       byte-indexed-remote-value
           (slm.associated-function.remote-symbol-address,
            if (instance?(tail(line-offset-pair), <integer>))
              tail(line-offset-pair)
            else
              0
            end if);
    if (this-address = ip)
      exact := i;
    elseif (this-address < ip)
      if (nearest-behind)
        if (address-behind < this-address)
          nearest-behind := i;
          address-behind := this-address;
        end if;
      else
        nearest-behind := i;
        address-behind := this-address;
      end if
    else
      if (nearest-ahead)
        if (this-address < address-ahead)
          nearest-ahead := i;
          address-ahead := this-address;
        end if;
      else
        nearest-ahead := i;
        address-ahead := this-address;
      end if
    end if;
    i := i + 1;
  end for;
  values(exact, nearest-ahead, nearest-behind);
end method;


///// RESOLVE-SOURCE-LOCATION
//    Using runtime information only, convert the given location in source
//    code to an instruction address in memory, if possible.
//    Paths should be a sequence of <string> objects describing pathnames.
//    Library should be a <remote-library> (if supplied), and searching will
//    be restricted to information stored for that library.

define method resolve-source-location
    (ap :: <access-path>, filename :: <string>,
     #key line = 0, column = 0, library = #f, paths = #())
       => (code-location :: false-or(<remote-value>), exact? :: <boolean>)
  if (library)
    resolve-source-location-from-library
      (ap, library, filename, line, column, paths);
  else
    let found = #f;
    let exact? = #f;
    let i = 0;
    while ((~found) & (i < size(ap.libraries)))
      let (loc, precise?)
        = resolve-source-location-from-library
                (ap, ap.libraries[i], filename, line, column, paths);
      if (loc)
        exact? := precise?;
        found := loc;
      else
        i := i + 1;
      end if;
    end while;
    values(found, exact?);
  end if;
end method;


define method resolve-source-location-from-library
    (ap :: <access-path>, lib :: <remote-library>,
     filename :: <string>, line :: <integer>, col :: <integer>,
     paths :: <sequence>)
       => (code-location :: false-or(<remote-value>), exact? :: <boolean>)
  resolve-source-location-on-connection
      (ap.connection, lib, filename, line, col, paths);
end method;


define open generic resolve-source-location-on-connection
    (conn :: <access-connection>, lib :: <remote-library>,
     filename :: <string>, line :: <integer>, col :: <integer>,
     paths :: <sequence>)
 => (code-location :: false-or(<remote-value>), exact? :: <boolean>);

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
