Module: ole-test
Synopsis:  Test the "com" and "ole-automation" libraries.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method setup-ole-tests ()
end method; 

define method cleanup-ole-tests ()
  revoke-registration(*factory*);
  Release(*disp-interface*);
  OLE-uninitialize();
end method cleanup-ole-tests;

define suite ole-suite 
  (setup-function: setup-ole-tests,
   cleanup-function: cleanup-ole-tests)
  test ole-argument-test;
  test ole-function-test;
  test ole-array-test;
end suite ole-suite;

define method run-suite ()
  make-ole-server-typeinfo();
  if ( OLE-util-register-only?())
    block ()
      format-out(if (OLE-util-unregister?())
		   "Un-Registering...\n"
		 else "Registering...\n"
		 end if);
      register-automation-server($class-id, "HQNexamples.OleCommTest",
				 "Dylan OLE Automation communication test",
				 versioned-prog-id:
				   "HQNexamples.OleCommTest.1");
      register-type-library(*ole-server-typeinfo*);
      format-out("Finished.\n");
    exception ( condition :: <error> )
      format-out("Error during registration:\n%s\n", condition);
    end block;
  else 
    // This initialization seems like it should be in the setup-function, but
    // that doesn't get run when the "-test" option is used to run just
    // one test.  (Bug 1196)
    OLE-initialize();
    *factory* :=  make-object-factory(*ole-server-typeinfo*);
    let disp-interface = create-dispatch($class-id);
    // create-dispatch might have returned the Dylan server object, but
    // what we need to test is a C interface pointer, so de-optimize:
    *disp-interface* := 
      make(<interface>, address: pointer-address(disp-interface));

/*
    // make testworks output results into a file instead of *standard-output*
    with-open-file ( *file-stream* = "ole-test-output.txt",
		    if-exists: #"replace",
		    direction: #"output",
		    buffer-size: 4)
      *format-function* := method (string :: <string>, #rest args) 
			     apply(format, *file-stream*, string, args);
			   end method;
      // perform-suite(ole-suite, debug?: #t);
      run-test-application(ole-suite);
    end with-open-file;
*/
  run-test-application(ole-suite);
  end if;
end method run-suite;

run-suite();


