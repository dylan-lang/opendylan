Module: ole-macro-test
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

// Global variables

define variable *factory* = #f;

define variable *disp-interface-1* :: false-or(<disp-type-info-1>) = #f;
define variable *disp-interface-2* :: false-or(<disp-type-info-2>) = #f;
define variable *disp-interface-3* :: false-or(<disp-type-info-3>) = #f;
define variable *disp-interface-4* :: false-or(<disp-type-info-4>) = #f;


define test ole-macro-test 
    (name: "ole-macro-test",
     description: "test ole macros")
  if (*factory*)    
    // check passing an integer in the first interface
    check-not-crash("setting integer in interface-1",
		    *disp-interface-1*.disp-type-info-1/integer := 12345);
    check-equal("checking integer in interface-1", 
		*disp-interface-1*.disp-type-info-1/integer, 12345);
    
    // check passing a boolean in the second interface
    check-not-crash("setting boolean in interface-2",
		    *disp-interface-2*.disp-type-info-2/boolean := #t);
    check-true("checking boolean in interface-2", 
	       *disp-interface-2*.disp-type-info-2/boolean);
    
    // check passing a character in the second interface
    check-not-crash("setting character in interface-2",
		    *disp-interface-2*.disp-type-info-2/character := 'm');
    check-equal("checking character in interface-2", 
		*disp-interface-2*.disp-type-info-2/character, 'm');
    
    // check passing an integer in the third interface
    check-not-crash("setting integer in interface-3",
		    *disp-interface-3*.disp-type-info-3/another-integer 
			:= 56789);
    check-equal("checking integer in interface-3", 
		*disp-interface-3*.disp-type-info-3/another-integer, 56789);
  
    // make sure ole server is diferentiating between the interfaces
    check-equal("checking integer again in interface-1",
		*disp-interface-1*.disp-type-info-1/integer, 12345);
    
    // check calling functions on different interfaces
    // check calling function on interface-1
    check-not-crash("call simple method double-internal-integer-value",
	    disp-type-info-1/double-internal-integer-value(*disp-interface-1*));
    check-equal("check double-internal-integer-value",
		*disp-interface-1*.disp-type-info-1/integer, 12345 * 2);
    
    // check calling function on interface-2
    check-true("check does-string-equal-hello?",
       disp-type-info-2/does-string-equal-hello?(*disp-interface-2*, "hello"));
    
    // check calling function on interface-3
    check-equal("call multiply on interface-3 passing 2 integers",
		disp-type-info-3/multiply(*disp-interface-3*, 4, 7), 28);

    // test inheritance
    check-not-crash("setting integer in interface-4",
		  *disp-interface-4*.disp-type-info-4/another-integer := -4040);
    check-equal("checking integer in interface-4", 
		*disp-interface-4*.disp-type-info-4/another-integer, -4040);
    check-equal("get id in interface-4",
		*disp-interface-4*.disp-type-info-4/id, 707);

  end if;
end test ole-macro-test;

define method setup-ole-tests ()  
  OLE-initialize();
  *factory* := make-object-factory(*test-coclass-type-info*);
  multiple-assign((*disp-interface-1*, *disp-interface-2*, 
		   *disp-interface-3*, *disp-interface-4*) := 
		  make-test-OLE-macros());
end method setup-ole-tests;

define method cleanup-ole-tests ()
  if (*factory*) 
    revoke-registration(*factory*);
    Release(*disp-interface-1*);
    Release(*disp-interface-2*);
    Release(*disp-interface-3*);
    Release(*disp-interface-4*);
    OLE-uninitialize();
  end if;
end method cleanup-ole-tests;

define suite ole-macro-suite 
  (setup-function: setup-ole-tests,
   cleanup-function: cleanup-ole-tests)
  test ole-macro-test;
end suite;

define method run-suite ()
  if ( OLE-util-register-only?() )
    block ()
      format-out(if (OLE-util-unregister?())
		   "Un-Registering...\n"
		 else "Registering...\n"
		 end if);
      register-automation-server
	($class-id, "HQNexamples.OleMacroTest",
	 "Dylan OLE Automation macro test");
      register-type-library(*test-coclass-type-info*);
      format-out("Finished.\n");
    exception ( condition :: <error> )
      format-out("Error during registration:\n%s\n", condition);
    end block;
  else 
    run-test-application(ole-macro-suite);
  end if;
end method run-suite;

run-suite();
