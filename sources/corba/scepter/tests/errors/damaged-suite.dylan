Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *damaged-files* = make(<table>);

define suite damaged-idl (setup-function: method ()
                                        add-cpp-include-path!("./");
                                        setup-idl-files(*damaged-files*);
                                      end method,
                      cleanup-function: method ()
                                          cleanup-idl-files(*damaged-files*);
                                        end method)
  test damaged-idl-array;
end suite;
