Author: James Kirsch
Module: ole-coclass-test
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// macro for a check-not-crash that traps errors when the occur

define macro check-not-crash
  { check-not-crash
     (?check-name:expression, ?check-expression:expression) }
    => { check-true (?check-name, begin
				    ?check-expression;
				    #t
				  end) }
end macro check-not-crash;


define test ole-coclass-test 
    (name: "ole-coclass-test",
     description: "test ole coclasses")
  if (*factory*)    
    // check passing an integer in the first interface
    check-not-crash("setting disp-id", 
		    *disp-id* := get-id-of-name(*disp-interface-1*, "integer"));
    check-not-crash("setting integer in interface-1",
		    set-property(*disp-interface-1*, *disp-id*, 12345));
    check-equal("checking integer in interface-1", 
		get-property(*disp-interface-1*, *disp-id*), 12345);
    
    // check passing a boolean in the second interface
    check-not-crash("setting disp-id", 
		    *disp-id* := get-id-of-name(*disp-interface-2*, "boolean"));
    check-not-crash("setting boolean in interface-2",
		    set-property(*disp-interface-2*, *disp-id*, #t));
    check-true("checking boolean in interface-2", 
	       get-property(*disp-interface-2*, *disp-id*));
    
    // check passing a character in the second interface
    check-not-crash("setting disp-id", 
		    *disp-id* := get-id-of-name(*disp-interface-2*, "character"));
    check-not-crash("setting character in interface-2",
		    set-property(*disp-interface-2*, *disp-id*, 'm'));
    check-equal("checking character in interface-2", 
		get-property(*disp-interface-2*, *disp-id*), 'm');
    
    // check passing an integer in the third interface
    check-not-crash("setting disp-id", 
		    *disp-id* := get-id-of-name(*disp-interface-3*, "another-integer"));
    check-not-crash("setting integer in interface-3",
		    set-property(*disp-interface-3*, *disp-id*, 56789));
    check-equal("checking integer in interface-3", 
		get-property(*disp-interface-3*, *disp-id*), 56789);
    check-not-crash("setting disp-id", 
		    *disp-id* := get-id-of-name(*disp-interface-1*, "integer"));
  

    // make sure ole server is diferentiating between the interfaces
    check-equal("checking integer again in interface-1",
		get-property(*disp-interface-1*, *disp-id*), 12345);
    
    // check calling functions on different interfaces
    // check calling function on interface-1
    check-not-crash("setting disp-id",
		    *disp-id* := get-id-of-name(*disp-interface-1*, 
						"double-internal-integer-value")); 
    check-not-crash("call simple method double-internal-integer-value",
		    call-simple-method(*disp-interface-1*, *disp-id*));
    check-equal("check double-internal-integer-value",
		get-property(*disp-interface-1*, "integer"),
		12345 * 2);
    
    // check calling function on interface-2
    check-not-crash("setting disp-id", 
		    *disp-id* := get-id-of-name(*disp-interface-2*,
						"does-string-equal-hello?"));
    check-true("check does-string-equal-hello?",
	       call-simple-method(*disp-interface-2*, *disp-id*, "hello"));
    
    // check calling function on interface-3
    check-not-crash("setting disp-id", 
		    *disp-id* := get-id-of-name(*disp-interface-3*, "multiply"));
    check-equal("call simple method multiply on interface-3 passing 2 integers",
		call-simple-method(*disp-interface-3*, *disp-id*, 4, 7),
		28);
  end if;
end test ole-coclass-test;


define test coclass-typelib-test
    (name: "coclass-typelib-test",
     description: "test accessing the type library of a coclass")
  
  let typeinfo = #f;
  let typelib = #f;
  check-equal("get dispatch type info",
	     begin
	       let ( status, info ) =
		 IDispatch/GetTypeInfo(*disp-interface-1*, 0, 0);
	       typeinfo := info;
	       status
	     end,
	     $S-OK);
  check-equal("get type library",
	      begin
		let ( status, lib, index ) =
		  ITypeInfo/GetContainingTypeLib(typeinfo);
		typelib := lib;
		status
	      end,
	      $S-OK);
  check-equal("type library typeinfo count",
	      ITypeLib/GetTypeInfoCount(typelib),
	      4);
end test coclass-typelib-test;

define method setup-ole-tests ()  
    make-test-coclass-type-info();
    OLE-initialize();
    *factory* := make-object-factory(*test-coclass-type-info*);
    let disp-interface-1 = create-dispatch($class-id);
    // create-dispatch might have returned the Dylan server object, but
    // what we need to test is a C interface pointer, so de-optimize:
    *disp-interface-1* := 
      make(<interface>, address: pointer-address(disp-interface-1));
    let (status, disp-interface-2) =
      QueryInterface(*disp-interface-1*, $interface-id-2);
    check-true("Did QueryInterface return disp-interface-2?",
	       SUCCEEDED?(status));
    let (status, disp-interface-3) =
      QueryInterface(*disp-interface-1*, $interface-id-3);
    check-true("Did QueryInterface return disp-interface-3?",
	       SUCCEEDED?(status));
    *disp-interface-2* := disp-interface-2;
    *disp-interface-3* := disp-interface-3;
end method; 

define method cleanup-ole-tests ()
  if (*factory*) 
    revoke-registration(*factory*);
    Release(*disp-interface-1*);
    Release(*disp-interface-2*);
    Release(*disp-interface-3*);
    OLE-uninitialize();
  end if;
end method cleanup-ole-tests;

define suite ole-coclass-suite 
  (setup-function: setup-ole-tests,
   cleanup-function: cleanup-ole-tests)
  test ole-coclass-test;
  test coclass-typelib-test;
end suite;

define method run-suite ()
  if ( OLE-util-register-only?()) // just register the type information
    block ()
      make-test-coclass-type-info();
      format-out(if (OLE-util-unregister?())
		   "Un-Registering...\n"
		 else "Registering...\n"
		 end if);
      register-automation-server
	($class-id, "HQNexamples.OleCoclassCommTest",
	 "Dylan OLE Automation coclass communication test",
	 versioned-prog-id: "HQNexamples.OleCoclassCommTest.1");
      register-type-library(*test-coclass-type-info*);
      format-out("Finished.\n");
    exception ( condition :: <error> )
      format-out("Error during registration:\n%s\n", condition);
    end block;
  else // run the test program
    run-test-application(ole-coclass-suite);
    // close(*file-stream*);
  end if;
end method run-suite;

run-suite();


