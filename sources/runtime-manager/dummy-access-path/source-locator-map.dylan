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

  slot source-filename :: <string>,
    required-init-keyword: filename:;

  slot associated-function :: <remote-symbol>,
    required-init-keyword: function-symbol:;

  slot number-of-locations :: <integer>,
    required-init-keyword: location-count:;

  slot base-linenumber :: <integer>,
    required-init-keyword: base-linenumber:;

  slot base-address :: <remote-value>,
    required-init-keyword: base-address:;

  slot line-address-pairs :: <vector>,
    required-init-keyword: pairs:;

end class;


///// EXPORTED GENERIC FUNCTIONS

define generic source-location-description
    (slm :: <source-location-map>, i :: <integer>)
       => (function-relative-linenumber :: <integer>,
           absolute-address :: <remote-value>);

define generic nearest-source-locations
    (ap :: <access-path>, slm :: <source-location-map>, addr :: <remote-value>)
       => (exact :: false-or(<integer>),
           nearest-ahead :: false-or(<integer>),
           nearest-behind :: false-or(<integer>));

define generic function-source-location-map
    (ap :: <access-path>, sym :: <remote-symbol>)
       => (maybe-slm :: false-or(<source-location-map>));


///// CONSTRUCT-SOURCE-LOCATION-MAP
//    Call the nub to read in the runtime's view of source locations.

define method construct-source-location-map
    (conn :: <local-access-connection>, sym :: <remote-symbol>) => ()

  if (instance?(sym, <remote-function>))
    let address = sym.remote-symbol-address;
    let debug-function = map-eip-to-function(conn.process, address);
    let filename = debug-function.source-filename;
    let baseline = debug-function.source-linenumber;
    let location-count = size(debug-function.line-offset-pairs);
    let pairs = make(<vector>, size: location-count);
    for (i from 0 below location-count)
      pairs[i] := pair(head(debug-function.line-offset-pairs[i]),
                       address + tail(debug-function.line-offset-pairs[i]));
    end for;
    sym.remote-symbol-source-location-map :=
      make(<source-location-map>,
           filename: filename,
           function-symbol: sym,
           location-count: location-count,
           base-address: address,
           base-linenumber: baseline,
           pairs: pairs);
  else
    sym.remote-symbol-source-location-map := #f;
    sym.definitely-no-source-locations := #t;
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


///// SOURCE-LOCATION-DESCRIPTION

define method source-location-description
    (slm :: <source-location-map>, i :: <integer>)
      => (function-relative-linenumber :: <integer>,
          address :: <remote-value>)
  if ((i < slm.number-of-locations) & (i >= 0))
    values(head(slm.line-address-pairs[i]),
           tail(slm.line-address-pairs[i]));
  else
    error("Tried to find more source locations than contained in table");
  end if;
end method;


///// NEAREST-SOURCE-LOCATIONS

define method nearest-source-locations
    (ap :: <access-path>, slm :: <source-location-map>, addr :: <remote-value>)
       => (exact :: false-or(<integer>),
           nearest-ahead :: false-or(<integer>),
           nearest-behind :: false-or(<integer>))
  let exact = #f;
  let nearest-ahead = #f;
  let nearest-behind = #f;
  let exact-index = #f;
  let nearest-ahead-index = #f;
  let nearest-behind-index = #f;
  for (i from 0 below slm.number-of-locations)
    let address = tail(slm.line-address-pairs[i]);
    if (address = addr)
      exact := address;
      exact-index := i;
    elseif (address < addr)
      if (nearest-behind)
        if (address > nearest-behind)
          nearest-behind := address;
          nearest-behind-index := i;
        end if;
      else
        nearest-behind := address;
        nearest-behind-index := i;
      end if;
    else
      if (nearest-ahead)
        if (address < nearest-ahead)
          nearest-ahead := address;
          nearest-ahead-index := i;
        end if;
      else
        nearest-ahead := address;
        nearest-ahead-index := i;
      end if;
    else
    end if
  end for;
  values(exact-index, nearest-ahead-index, nearest-behind-index);
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
  values(#f, #f)
end method;
