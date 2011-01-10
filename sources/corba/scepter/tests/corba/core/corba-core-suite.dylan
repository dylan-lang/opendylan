Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *corba-core-files* = make(<table>);

define suite corba-core-idl (setup-function: method ()
                                        add-cpp-include-path!("./");
                                        setup-idl-files(*corba-core-files*);
                                        scepter-case-sensitive-reserved-words?(get-scepter()) := #t;
                                      end method,
                      cleanup-function: method ()
                                          cleanup-idl-files(*corba-core-files*);
                                          scepter-reset-options(get-scepter());
                                        end method)
  test corba-core-idl-CORBA;
  test corba-core-idl-IOP;
  test corba-core-idl-DCE_CIOP;
  test corba-core-idl-GIOP;
  test corba-core-idl-IIOP;
end suite;
