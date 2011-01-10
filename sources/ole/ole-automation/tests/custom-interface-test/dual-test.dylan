Module:    custom-interface-test
Synopsis:  Test the "define dual-interface" macro.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// minimal macro call
define dual-interface <IMinimal> ( <simple-dispatch> )
  uuid "{0844835F-265D-11D2-9A67-006097C90313}";
end;  

define constant $IID-IDonkey = 
  as(<REFCLSID>, "{08448354-265D-11D2-9A67-006097C90313}");

define constant $donkey-code = 192837;

define dual-interface <IDonkey> ( <simple-dispatch> )
  uuid $IID-IDonkey;
  name "Donkey";
  documentation "simple test of dual interface";
  major-version 1; minor-version 1;

  member-function IDonkey/Bar ( n :: <C-void*> ) => ();
  member-function IDonkey/Square ( n :: <integer> ) => (n :: <integer>);
  virtual property IDonkey/Size :: <integer>, name: "size";

  constant property Donkey-const :: <single-float>, name: "const";
  constant property Donkey-code :: <integer> = $donkey-code,
	disp-id: $DISPID-VALUE;
end;  

define constant <C-IDonkey> = <Interface>;

define COM-interface <my-Donkey> ( <IDonkey> )
  slot IDonkey/Size :: <integer> = 0, init-keyword: size:;
end <my-Donkey>;

define method IDonkey/Bar (This :: <my-Donkey>, n :: <C-void*>)
 => (status :: <HRESULT>);
  if ( null-pointer?(n) )
    $S-FALSE
  else
    $S-OK
  end if
end method;

define method IDonkey/Square (This :: <my-Donkey>, n :: <integer>)
 => (status :: <HRESULT>, square :: <integer>);
  values( $S-OK, n * n )
end method;

define constant $Donkey-const = 8.75;

define method Donkey-const (This :: <my-Donkey>) => (value :: <single-float>)
  $Donkey-const
end;

define variable *di1* :: <interface> = $null-interface;
define variable *di2* :: <interface> = $null-interface;
define variable *di3* :: <interface> = $null-interface;

define variable *dual-typeinfo* :: <interface> = $null-interface;

define constant $magic-number = -3021;

define test dual-interface-test 
		    (name: "dual-interface-test",
		     description: "test dual interface")

  check-true("make(<my-donkey>)",
	     instance?(*di1* := make(<my-donkey>, size: $default-size),
		       <my-donkey>));

  check-equal("QueryInterface(*di1*, $IID-IDonkey)",
	      begin
		let ( status, interface2 ) =
		  QueryInterface(*di1*, $IID-IDonkey);
		*di2* := interface2;
		status
	      end,
	      $S-OK);

  check-equal("*di1* = *di2*", *di1*, *di2*);

  *dual-typeinfo* := $null-interface;
  check-equal("type-information from class or instance",
	      *dual-typeinfo* := type-information(<my-donkey>),
	      type-information(*di1*));

  check-true("typeinfo is ITypeinfo", instance?(*dual-typeinfo*, <ITypeinfo>));

  check-equal("typeinfo class", object-class(*dual-typeinfo*),
	      <dual-type-info>);

  check-equal("dispatch typeinfo from dual",
	      object-class(dispatch-type-information(*di1*)),
	      <disp-type-info>);

  check-equal("dispatch-type-information from class or instance",
	      dispatch-type-information(<my-donkey>),
	      dispatch-type-information(*di1*));

  check-equal("GetDocumentation",
	      begin
		let (status, name, doc-string, HelpContext, HelpFile ) =
		  ITypeInfo/GetDocumentation(*dual-typeinfo*, $MEMBERID-NIL);
		vector(name, doc-string)
	      end,
	      vector("Donkey",
		     "simple test of dual interface"));

  check-equal("set *di3*",
	      begin
		*di3* := make(<interface>, address: pointer-address(*di2*));
		pointer-address(*di3*)
	      end,
	      pointer-address(*di2*));

  check-equal("IDonkey/Square by dispatch",
	      call-simple-method(*di3*, "IDonkey/Square", 70),
	      70 * 70);

  check-equal("IDonkey/Square by vtable",
	      begin
		let ( status, value ) = IDonkey/Square(*di3*, 11);
		value
	      end,
	      11 * 11);

  check-equal("IDonkey/Size",
	      begin
		IDonkey/Size(*di3*) := $magic-number;
		IDonkey/Size(*di3*)
	      end,
	      $magic-number);

  check-equal("code by vtable",
	      Donkey-code(*di3*),
	      $donkey-code);

  check-equal("code by dispatch",
	      get-property(*di3*, "Donkey-code"),
	      $donkey-code);

  check-equal("IDonkey/Size by dispatch",
	      get-property(*di3*, "size"),
	      $magic-number);

  check-not-crash("Release(*di2*)",
		  Release(*di2*));
end test;

