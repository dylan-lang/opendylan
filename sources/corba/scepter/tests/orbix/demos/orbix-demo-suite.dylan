Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite orbix-demo-idl (setup-function: method ()
                                        add-cpp-include-path!("./");
                                        setup-idl-files(*orbix-demo-files*);
                                      end method,
                      cleanup-function: method ()
                                          cleanup-idl-files(*orbix-demo-files*);
                                        end method)
  test orbix-demo-idl-any_demo-test1;
  test orbix-demo-idl-filters-filter;
  test orbix-demo-idl-grid-grid;
  test orbix-demo-idl-grid_iiop-grid;
  test orbix-demo-idl-grid_tie-grid;
  test orbix-demo-idl-timer-perf;
end suite;
