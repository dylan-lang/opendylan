Module:    ole-test
Synopsis:  OLE Automation dispatch methods test.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/* ole-function-test excutes functions from the server. It checks
   that the functions are called and return the expected results. */

define method bare-method
    (test :: <values>) => (status :: <HRESULT>)
  $S-OK
end method bare-method;

define method string-returned
    (test :: <values>) => (status :: <HRESULT>, string :: <string>)
  values($S-OK, "passed")
end method string-returned;

define method string-to-boolean
    (test :: <values>, string :: <BSTR>) => 
    (status :: <HRESULT>, passed :: <boolean>)
  values($S-OK, string = "hello")
end method;
      
define method multiply
    (test :: <values>, arg1 :: <integer>, arg2 :: <integer>) =>
    (status :: <HRESULT>, result :: <integer>)
  values($S-OK, arg1 * arg2)
end method multiply;

define method in-out-square
    (test :: <values>, ptr :: <C-pointer>) =>
    (status :: <HRESULT>)
  let value = pointer-value(ptr);
  pointer-value(ptr) := value * value;
  $S-OK
end;

define method ole-reverse
    (test :: <values>, list :: <ole-vector>) =>
    (status :: <HRESULT>, reverse-list :: <vector>)
  // Note: probably the type of the second result value should be
  // <ole-vector>, but see:
  //   BUG 1452: type-for-copy returns <simple-object-vector> for all vectors
  // -- DNG 10/21/97
  values($S-OK, reverse(list))
end method ole-reverse;

define method raise-exception
    (test :: <values>) => (status :: <HRESULT>)
  $E-FAIL
end method raise-exception;

define constant $my-special-status :: <HRESULT> = MAKE-SCODE(1, 19, 66);

define method raise-exception2
    (test :: <values>) => (status :: <HRESULT>)
  exit-invoke($my-special-status);
  $S-FALSE // shouldn't get here
end method raise-exception2;

define constant the-sequence = 
  list(1, 2, list(3, "four"), list("five"), 6, #t);

define method test-copy-automation-value
    (test :: <values>, passed-sequence :: <ole-vector>) => 
    (status :: <HRESULT>, result :: <boolean>) 
  let returned-sequence = copy-automation-value(passed-sequence);
  if (instance?(returned-sequence, <simple-vector>) &
	(as(<vector>, returned-sequence) = as(<vector>, the-sequence)))
    values($S-OK, #t)
  else
    values($S-OK, #f)
  end if
end method test-copy-automation-value;

define test ole-function-test
  (name: "ole-function-test",
   description: 
     "tests passing arguments from a OLE controller to a OLE server")  
  // check status-codes
  check-true("$E-UNEXPECTED", instance?($E-UNEXPECTED, <SCODE>));
  check-true("$E-NOTIMPL", instance?($E-UNEXPECTED, <SCODE>));
  check-true("$E-OUTOFMEMORY", instance?($E-OUTOFMEMORY, <SCODE>));
  check-true("$E-INVALIDARG", instance?($E-INVALIDARG, <SCODE>));
  check-true("$E-POINTER", instance?($E-POINTER, <SCODE>));
  check-true("$E-HANDLE", instance?($E-HANDLE, <SCODE>));
  check-true("$E-ABORT", instance?($E-ABORT, <SCODE>));
  check-true("$E-FAIL", instance?($E-FAIL, <SCODE>));
  check-true("$E-ACCESSDENIED", instance?($E-ACCESSDENIED, <SCODE>));
  
  // checks if client can call a basic function on the server
  check-not-crash("check getting the dispatch id",
		  *disp-id* := 
		    get-id-of-name(*disp-interface*, "bare-method"));
  // Note: the following check is currently failing, probably because of:
  //    Bug 788: extra let-bound variables default to random values, not false
  //  -- DNG 10/21/97
  check-false("call simple method bare-method",
	      call-simple-method(*disp-interface*, *disp-id*));
  check-false("call simple method bare-method using string *disp-id*",
	      call-simple-method(*disp-interface*, "bare-method"));

  // check if function on the server can return a result
  check-not-crash("check getting the dispatch id",
		  *disp-id* := 
		    get-id-of-name(*disp-interface*, "string-returned"));
  check-equal("call simple method string-returned",
	      call-simple-method(*disp-interface*, *disp-id*),
	      "passed");
    
  // check if function can accept an argument and return a result
  check-not-crash("check getting the dispatch id",
		  *disp-id* := 
		    get-id-of-name(*disp-interface*, "string-to-boolean"));
  let temp-boolean = #f;
  check-equal("call-simple-method string-to-boolean passing string 'hello'",
	      temp-boolean := 
		call-simple-method(*disp-interface*, *disp-id*, "hello"),
	      #t);

  // check if function can accept two arguments
  check-not-crash("check getting the dispatch id",
		  *disp-id* := get-id-of-name(*disp-interface*, "multiply"));
  check-equal("call simple method multiply passing 2 integers",
	      call-simple-method(*disp-interface*, *disp-id*, 4, 7),
	      28);
	   
  // check if function can return a sequence
  check-not-crash("check getting the dispatch id",
		  *disp-id* :=
		    get-id-of-name(*disp-interface*, "ole-reverse"));
  let temp-vector = vector(1, 2, "three");
  check-equal("call simple method ole-reverse",
	      call-simple-method(*disp-interface*, *disp-id*, temp-vector),
	      vector("three", 2, 1));

  // check if copy-automation-value function works
  check-not-crash("check getting the dispatch id",
		  *disp-id* := 
		    get-id-of-name(*disp-interface*, 
				   "test-copy-automation-value"));
  check-equal("check if test-copy-automation-value is true",
	      call-simple-method(*disp-interface*, *disp-id*, the-sequence),
	      #t);

  // check exception protocol
  check-not-crash("check getting the dispatch id",
		    *disp-id* := 
		    get-id-of-name(*disp-interface*, "raise-exception"));
  check-condition("call simple method raise-exception", <ole-error>,
		  call-simple-method(*disp-interface*, *disp-id*));
  check-equal("test exit-invoke",
	      block()
		call-simple-method(*disp-interface*, "raise-exception2");
	      exception ( err :: <ole-error> )
		ole-error-status(err);
	      end block,
	      $my-special-status);

  // try a by-ref parameter
  with-stack-structure( iptr :: <C-long*> )
    pointer-value(iptr) := 7;
    check-equal("by-ref in-out parameter",
		begin
		  call-simple-method(*disp-interface*, "square", iptr);
		  pointer-value(iptr)
		end,
		49);
    let ref = inout-ref(70, type: <C-long>);
    check-equal("inout-ref parameter",
		begin
		  call-simple-method(*disp-interface*, "square", ref);
		  arg-spec-value(ref)
		end,
		4900);
  end with-stack-structure;
end test ole-function-test;
