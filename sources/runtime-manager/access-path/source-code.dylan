module:       access-path-implementation
synopsis:     Conversion between application addresses and source code
              locators
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// SOURCE-LOCATOR
//    This is a place holder. Eventually, there will be some canonical
//    representation in the development environment.

define class <source-locator> (<object>)

       slot source-locator-symbol :: <remote-symbol>,
            required-init-keyword: symbol:;

       slot source-location :: <string>,
            required-init-keyword: source-location:;

       slot source-locator-linenumber :: <integer>,
            required-init-keyword: linenumber:;

       slot source-locator-address :: <remote-value>,
            required-init-keyword: address:;

end class;


///// EXPORTED GENERICS

define generic address-to-source-locator 
    (ap :: <access-path>, ra :: <remote-value>)
      => (_ :: <source-locator>);

define generic source-locator-filename 
    (ap :: <access-path>, locator :: <source-locator>)
      => (_ :: <string>);

define generic do-source-locators 
    (function :: <function>, ap :: <access-path>,
     symbol :: <remote-symbol>) 
       => ();


///// SOURCE-LOCATOR-FILE
//    Converts a filename string to a source file locator

define method source-locator-filename 
    (ap :: <access-path>, locator :: <source-locator>)
       => (_ :: <string>)
  locator.source-location;
end method;


///// CONNECTION-GET-SOURCE-LOCATORS
//    Accesses the debugger nub to read out all known source locations for
//    the definition of a symbol (function).

define method connection-get-source-locators
    (conn :: <local-access-connection>, sym :: <remote-symbol>,
     start-addr :: <remote-value>, end-addr :: <remote-value>)
       => (_ :: <sequence>)

  let table = nub-fetch-source-locations
                     (conn.connection-process, start-addr, end-addr);

  let filename-length = nub-source-location-filename-length
                     (conn.connection-process, table);

  let Cfilename = make (<byte-string>, size: filename-length);

  let count = nub-number-of-source-locations
                     (conn.connection-process, table);

  let locations = make (<vector>, size: count);
  
  nub-source-location-filename (conn.connection-process, table, filename-length,
                                Cfilename);

  for (i from 0 below count)
      locations[i] := make (<source-locator>,
                            symbol: sym,
                            source-location: Cfilename,
                            linenumber:
                              nub-source-location-linenumber
                                  (conn.connection-process, table, i),
                            address:
                              nub-source-location-address
                                  (conn.connection-process, table, i));
  end for;

  nub-dispose-source-locations (conn.connection-process, table);

  locations;
end method;

define method do-new-source-locators 
    (f :: <function>, ap :: <access-path>,
     symbol :: <remote-symbol>) => ()

  // To get the source locators, we need to know the expanse of this
  // definition. At the moment, the only way to do this is using
  // nearest-symbols.

  let (actual, prev, nxt) = nearest-symbols (ap, symbol.remote-symbol-address);

  // "Actual" should be the self-same remote symbol as "symbol" (but they
  // won't be ID to each other). We could check, but let's not bother...

  // The address range we are interested in is from the address of "symbol"
  // to the address of "nxt". We need to harvest all source locations
  // in this range.

  // If we couldn't get a "next" symbol, then we effectively don't know
  // the length of "symbol"'s definition. This is a problem, since it
  // means that do-source-locators just won't work for whatever poor
  // function happens to be last!! This is one reason why improvements
  // are needed here.

  if (nxt)
    let start-address = remote-symbol-address (symbol);
    let end-address = remote-symbol-address (nxt);
    let locators = connection-get-source-locators
                       (ap.connection, symbol, start-address, end-address);
    symbol.remote-symbol-source-locations := locators;

    // And do the iterating, of course...

    for (locator in locators)
        f(locator)
    end for;
  end if;

end method;

define method do-source-locators 
    (f :: <function>, ap :: <access-path>,
     symbol :: <remote-symbol>) 
       => ()
  if (symbol.remote-symbol-source-locations ~= #[])
    for (locator in symbol.remote-symbol-source-locations)
      f(locator)
    end for
  else
    do-new-source-locators (f, ap, symbol)
  end if
end method;

