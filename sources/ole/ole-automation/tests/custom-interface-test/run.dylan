Module:    custom-interface-test
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method setup-ole-tests ()  
  OLE-initialize();
end method setup-ole-tests;

define method cleanup-ole-tests ()
  OLE-uninitialize();
end method cleanup-ole-tests;

define suite custom-interface-suite 
  (setup-function: setup-ole-tests,
   cleanup-function: cleanup-ole-tests)
  test custom-interface-test;
  test vtable-interface-test;
  test dual-interface-test;
  test internal-coclass-test;
  test typelib-test;
  test inproc-server-test;
  test external-coclass-test;
end suite;

define method run-suite ()
  if ( OLE-util-register-only?() ) // "/RegServer" or "/UnRegServer"
    block ()
      format-out(if (OLE-util-unregister?())
		   "Un-Registering...\n"
		 else "Registering...\n"
		 end if);
      register-this-server();
      format-out("Finished.\n");
//  exception ( condition :: <error> )
//    format-out("Error during registration:\n%s\n", condition);
    end block;
  elseif ( OLE-util-automation?() ) // "/Automation"
    run-as-server();
  else // normal testworks run
    run-test-application(custom-interface-suite);
  end if;
end method run-suite;

run-suite();
