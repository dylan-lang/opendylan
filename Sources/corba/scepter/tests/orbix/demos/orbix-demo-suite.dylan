Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *orbix-demo-files* = make(<table>);

define suite orbix-demo-idl (setup-function: method ()
                                        add-cpp-include-path!("./");
                                        setup-idl-files(*orbix-demo-files*);
                                      end method,
                      cleanup-function: method ()
                                          cleanup-idl-files(*orbix-demo-files*);
                                        end method)
  test orbix-demo-idl-Xwin-label;
  test orbix-demo-idl-any_demo-test1;
  test orbix-demo-idl-caching-caching;
  test orbix-demo-idl-colocate-BANK;
  test orbix-demo-idl-encrypt-enc;
  test orbix-demo-idl-filters-filter;
  test orbix-demo-idl-grid-grid;
  test orbix-demo-idl-grid_iiop-grid;
  test orbix-demo-idl-grid_tie-grid;
  test orbix-demo-idl-idl_demo-BANK;
  test orbix-demo-idl-ir_demo-BANK;
//  test orbix-demo-idl-opaque-opaq;
  test orbix-demo-idl-per_mast-BANK;
  test orbix-demo-idl-per_mult-BANK;
  test orbix-demo-idl-per_simp-BANK;
  test orbix-demo-idl-timer-perf;
end suite;
