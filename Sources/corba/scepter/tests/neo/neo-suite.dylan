Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *neo-files* = make(<table>);

define suite neo-idl (setup-function: method ()
                                        add-cpp-include-path!("./");
                                        setup-idl-files(*neo-files*);
                                      end method,
                      cleanup-function: method ()
                                          cleanup-idl-files(*neo-files*);
                                        end method)
  test neo-idl-array;
  test neo-idl-constant;
  test neo-idl-dif2;
  test neo-idl-inherit;
  test neo-idl-module;
  test neo-idl-primtypes;
  test neo-idl-sequence;
  test neo-idl-simple;
  test neo-idl-simple2;
  test neo-idl-struct;
  test neo-idl-union;
  test neo-idl-union2;
end suite;
