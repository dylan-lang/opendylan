Module:    parameter-type-tests
Synopsis:  Tests of parameter types for COM interfaces.
Author:    Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method setup-ole-tests ()  
  OLE-initialize();
end method setup-ole-tests;

define method cleanup-ole-tests ()
  OLE-uninitialize();
end method cleanup-ole-tests;

define test vtable-parm-test
	(name: "vtable-parm-test",
	 description: "test parameter types in in-process vtable interface")
  let server = #f;
  check-false("start in-process server",
	      null-pointer?(server := 
		create-COM-instance($VtableParmTest-uuid,
				    interface-id: $IID-<S-IVtableParmTest>)));
  server := pointer-cast(<C-IVtableParmTest>, server);
  test-IVtableParmTest(server);
  /*check-equal("release of in-process server",*/ Release(server) /*, 0)*/ ;
end test vtable-parm-test;

define suite custom-interface-suite 
  (setup-function: setup-ole-tests,
   cleanup-function: cleanup-ole-tests)
  test vtable-parm-test;
end suite;

define method run-suite ()
  run-test-application(custom-interface-suite);
end method run-suite;

run-suite();
