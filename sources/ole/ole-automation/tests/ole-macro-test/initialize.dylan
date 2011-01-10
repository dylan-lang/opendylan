Module: ole-macro-test
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

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

define dispatch-interface <dispatch-object-1> ( <simple-dispatch> )
  uuid "{94CCDC4E-92AD-11D1-9A5E-006097C90313}";
  property integer-value :: <integer>, name: "integer";
  name "disp-type-info-1";
  documentation "disp-type-info-1 for testing purposes";
  member-function double-internal-integer-value,
	name: "double-internal-integer-value";
end <dispatch-object-1>;

define dispatch-interface <dispatch-object-2> (<simple-dispatch>)
  name "disp-type-info-2";
  documentation "disp-type-info-2 for testing purposes";
  uuid as(<REFIID>, "{94CCDC4B-92AD-11D1-9A5E-006097C90313}");
  property boolean-value :: <boolean>, name: "boolean";
  property character-value :: <character>, name: "character";
  member-function does-string-equal-hello?
	(string :: <string>) => (boolean :: <boolean>);
end <dispatch-object-2>;

define dispatch-interface <dispatch-object-3> (<simple-dispatch>)
  name "disp-type-info-3";
  documentation "disp-type-info-3 for testing purposes";
  uuid as(<REFIID>, "{94CCDC4C-92AD-11D1-9A5E-006097C90313}");
  property another-integer :: <integer>;
  member-function multiply 
    (arg1 :: <integer>, arg2 :: <integer>) => (result :: <integer>);
end <dispatch-object-3>;

// test inheritance:
define dispatch-interface <dispatch-object-4> (<dispatch-object-3>)
  name "disp-type-info-4";
  uuid as(<REFIID>, "{94CCDC4D-92AD-11D1-9A5E-006097C90313}");
  constant property id = 707;
end <dispatch-object-4>;

define constant $class-id = 
	as(<REFIID>, "{94CCDC4A-92AD-11D1-9A5E-006097C90313}");

define coclass *test-coclass-type-info*
  name "ole-macro-test";
  uuid $class-id;
  interface <dispatch-object-2>;
  default interface <dispatch-object-1>;
  interface <dispatch-object-3>;
  interface <dispatch-object-4>;
end;

