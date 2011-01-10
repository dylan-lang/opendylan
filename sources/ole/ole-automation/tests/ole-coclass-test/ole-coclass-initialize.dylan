Author: James Kirsch
Module: ole-coclass-test
Synopsis: Tests that the OLE Automation library works with an OLE object 
          with multiple interfaces.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
// make testworks output results into a file instead of *standard-output*

define variable *file-stream* = make(<file-stream>, 
				     locator: "ole-coclass-test-output", 
				     if-exists: #"replace",
				     direction: #"output",
				     buffer-size: 4);

*format-function* := method (string :: <string>, #rest args) 
		       apply(format, *file-stream*, string, args);
		     end method;
*/

/* ole-coclass-initialize contains the information for creating the ole-server
   that the tests are run off of. */

// Global variables

define variable *disp-id* = #f;

define variable *disp-interface-1* = #f;

define variable *disp-interface-2* = #f;

define variable *disp-interface-3* = #f;

define variable *test-coclass-type-info* = #f;

define variable *factory* = #f;
 
// We need four uuids - one for each interface of the ole-server,
// and one for the object as a whole.

// f52e7090-fe28-11d0-b2c1-0000c09a70b7

define constant $class-id =
  make-GUID(#xf52e7090, #xfe28, #x11D0, #xb2, #xc1, #x00, #x00,
	    #xc0, #x9a, #x70, #xb7);

define constant $interface-id-1 =
  make-GUID(#xf52e7091, #xfe28, #x11D0, #xb2, #xc1, #x00, #x00,
	    #xc0, #x9a, #x70, #xb7);

// a7750270-fe31-11d0-b2c1-0000c09a70b7

define constant $interface-id-2 =
  make-GUID(#xa7750270, #xfe31, #x11d0, #xb2, #xc1, #x00, #x00, 
	    #xc0, #x9a, #x70, #xb7);


// d9605f80-fe31-11d0-b2c1-0000c09a70b7

define constant $interface-id-3 =
  make-GUID(#xd9605f80, #xfe31, #x11d0, #xb2, #xc1, #x00, #x00, 
	   #xc0, #x9a, #x70, #xb7);

// The functions for the ole-server interfaces

define method double-internal-integer-value
    (test :: <dispatch-object-1>) => (status :: <HRESULT>)
  test.integer-value := test.integer-value * 2;
  $S-OK
end method double-internal-integer-value;

define method does-string-equal-hello?
    (test :: <dispatch-object-2>, string :: <string>) 
 => (status :: <HRESULT>, boolean :: <boolean>)
  values($S-OK, string = "hello")
end method does-string-equal-hello?;

define method multiply
    (test :: <dispatch-object-3>, arg1 :: <integer>, arg2 :: <integer>)
 => (status :: <HRESULT>, result :: <integer>)
  values($S-OK, arg1 * arg2)
end method multiply;

// The variables for the ole-server interfaces

define COM-Interface <dispatch-object-1> (<simple-dispatch>)
  slot integer-value :: <integer>;
end COM-Interface <dispatch-object-1>;

define COM-Interface <dispatch-object-2> (<simple-dispatch>)
  slot boolean-value :: <boolean>;
  slot character-value :: <character>;
end COM-Interface <dispatch-object-2>;

define COM-Interface <dispatch-object-3> (<simple-dispatch>)
  slot another-integer-value :: <integer>;
end COM-Interface <dispatch-object-3>;

define method make-test-coclass-type-info ()
  let disp-type-info-1 =
    make(<disp-type-info>,
	 name: "disp-type-info-1",
	 uuid: $interface-id-1,
	 documentation: "disp-type-info-1 for testing purposes",
	 properties: vector(make(<variable-description>,
				 name: "integer",
				 getter: integer-value,
				 setter: integer-value-setter,
				 type: <C-Short>)), 
	 methods: vector(make(<function-description>,
			      name: "double-internal-integer-value",
			      function: double-internal-integer-value))
	   );
  let disp-type-info-2 =
    make(<disp-type-info>,
	 name: "disp-type-info-2",
	 documentation: "disp-type-info-2 for testing purposes",
	 properties: vector(make(<variable-description>,
				 name: "boolean",
				 getter: boolean-value,
				 setter: boolean-value-setter,
				 type: <VARIANT-BOOL>),
			    make(<variable-description>,
				 name: "character",
				 getter: character-value,
				 setter: character-value-setter,
				 type: <C-character>)),
	 methods: vector(make(<function-description>,
			      name: "does-string-equal-hello?",
			      function: does-string-equal-hello?,
			      argument-names: vector("string"),
			      argument-types: vector(<BSTR>),
			      result-type: <VARIANT-BOOL>)), 
	 uuid: $interface-id-2);
  let disp-type-info-3 =
    make(<disp-type-info>,
	 name: "disp-type-info-3",
	 documentation: "disp-type-info-3 for testing purposes",
	 properties: vector(make(<variable-description>,
				 name: "another-integer",
				 getter: another-integer-value,
				 setter: another-integer-value-setter,
				 type: <C-short>)),
	  methods: vector(make(<function-description>,
			      name: "multiply",
			      function: multiply,
			      argument-names: 
				 vector("arg1", "arg2"),
			      argument-types:
				vector(<C-short>, <C-short>),
			      result-type: <C-short>)), 
	 uuid: $interface-id-3);
   *test-coclass-type-info* :=
    make(<coclass-type-info>,
	 name: "test-coclass-type-info",
	 uuid: $class-id,
	 interfaces: vector(make(<component-interface-description>,
				 class: <dispatch-object-1>,
				 typeinfo: disp-type-info-1),
			    make(<component-interface-description>,
				 class: <dispatch-object-2>,
				 typeinfo: disp-type-info-2),
			    make(<component-interface-description>,
				 class: <dispatch-object-3>,
				 typeinfo: disp-type-info-3)));
end method make-test-coclass-type-info;
















