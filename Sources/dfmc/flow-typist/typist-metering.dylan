Module: dfmc-typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

*top-level+optimizer+typist-metering* := access(dfmc-debug, *top-level+optimizer+typist-metering*);

define metering-set *my-dfmc-typist-metering*
  include *top-level+optimizer+typist-metering*;
  modules  dfmc-conversion, dfmc-modeling, dfmc-flow-graph, my-dfmc-typist;
  functions
    ^subtype? in dfmc-modeling,
    make in internal,
      dylan-make in-package dylan,
    every?, 
      dylan-every?   in-package dylan,
      dylan-every?-1 in-package dylan,
    any?,   
      dylan-any?   in-package dylan,
      dylan-any?-1 in-package dylan;
/*    do, 
    map, 
      map in-package dylan,
    map-into,
    map-as, 
    key-sequence,
    do-1-collection-by-value in internal,
    do-2-collections-by-key  in internal,
    do-collections-by-key    in internal,
*/
end metering-set;

