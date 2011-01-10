Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *corba-services-files* = make(<table>);

define suite corba-services-idl (setup-function: method ()
                                        add-cpp-include-path!("./");
                                        setup-idl-files(*corba-services-files*);
                                      end method,
                      cleanup-function: method ()
                                          cleanup-idl-files(*corba-services-files*);
                                        end method)
  test corba-services-idl-CompoundExternalization;
  test corba-services-idl-CompoundLifeCycle;
  test corba-services-idl-Concurrency;
  test corba-services-idl-Containment;
  test corba-services-idl-CosEventChannel;
  test corba-services-idl-CosEventComm;
  test corba-services-idl-CosExternalization;
  test corba-services-idl-CosExternalizationContainment;
  test corba-services-idl-CosExternalizationReference;
  test corba-services-idl-CosLifeCycleContainment;
  test corba-services-idl-CosLifeCycleReference;
  test corba-services-idl-CosLifeCycleService;
  test corba-services-idl-CosPersistenceDDO;
  test corba-services-idl-CosPersistenceDS_CLI;
  test corba-services-idl-CosPersistencePDS;
  test corba-services-idl-CosPersistencePDS_DA;
  test corba-services-idl-CosPersistencePID;
  test corba-services-idl-CosPersistencePO;
  test corba-services-idl-CosPersistencePOM;
  test corba-services-idl-CosTransactions;
  test corba-services-idl-CosTypedEventChannel;
  test corba-services-idl-CosTypedEventComm;
  test corba-services-idl-Graphs;
  test corba-services-idl-LifeCycle;
  test corba-services-idl-Naming;
  test corba-services-idl-ObjectIdentity;
  test corba-services-idl-Reference;
  test corba-services-idl-Relationships;
  test corba-services-idl-Stream;
  test corba-services-idl-TSInteroperation;
  test corba-services-idl-TSPortability;
end suite;
