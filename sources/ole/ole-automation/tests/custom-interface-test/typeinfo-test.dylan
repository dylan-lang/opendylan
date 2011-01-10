Module:    custom-interface-test
Synopsis:  Test the "define vtable-interface" macro.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// minimal macro call
define vtable-interface <INothing> ( <IUnknown> )
  uuid "{08448352-265D-11D2-9A67-006097C90313}";
end;

define constant $IID-IMonkey = "{08448353-265D-11D2-9A67-006097C90313}";

define vtable-interface <IMonkey> ( <IUnknown> )
  client-class <C-IMonkey>;
  uuid $IID-IMonkey;
  name "Monkey";
  documentation "simple test of custom interface with type info";
  major-version 1; minor-version 1;

  member-function IMonkey/Bar ( n :: <C-void*> ) => ();
  member-function IMonkey/Num ( n :: <integer> ) => ();
  vtable-member IMonkey/Special ( )
	 => ( x :: <C-int>, y :: <integer>, z :: <C-short> );
  property IMonkey/Size :: <integer>;
  constant property Monkey-const :: <single-float>;
end;  

// Update the following if any members added above.
define constant $number-of-monkey-members = 9;

define COM-interface <my-Monkey> ( <IMonkey> )
  slot IMonkey/Size :: <integer> = 0, init-keyword: size:;
end <my-Monkey>;

define method IMonkey/Bar (This :: <my-Monkey>, n :: <C-void*>)
 => (status :: <HRESULT>);
  if ( null-pointer?(n) )
    $S-FALSE
  else
    $S-OK
  end if
end method;

define method IMonkey/Num (This :: <my-Monkey>, n :: <integer>)
 => (status :: <HRESULT>);
  if ( zero?(n) )
    $S-FALSE
  else
    $S-OK
  end if
end method;

define method IMonkey/Special (This :: <my-Monkey>)
 => ( x :: <integer>, y :: <integer>, z :: <integer> );
  values( -301, -302, -303)
end;

define constant $Monkey-const = 1.234;

define method Monkey-const (This :: <my-Monkey>) => (value :: <single-float>)
  $Monkey-const
end;

define variable *vi1* :: <interface> = $null-interface;
define variable *vi2* :: <interface> = $null-interface;
define variable *vi3* :: <interface> = $null-interface;

define variable *typeinfo* :: <interface> = $null-interface;
define variable *attr* = $null-interface;

define test vtable-interface-test 
		    (name: "vtable-interface-test",
		     description: "test custom interface with type info")

  check-true("type-information(<IUnknown>)",
	     instance?(*typeinfo* := type-information(<IUnknown>),
		       <ITypeinfo>));

  check-equal("GetTypeAttr",
	      begin
		let ( status, attributes ) = ITypeInfo/GetTypeAttr(*typeinfo*);
		*attr* := attributes;
		status
	      end,
	      $S-OK);
  check-equal("typekind", *attr*.typekind-value, $TKIND-INTERFACE);
  check-equal("number of IUnknown members",
	      *attr*.cFuncs-value, 3);
  check-equal("size of IUnknown vtable",
	      *attr*.cbSizeVft-value,
	      3 * size-of(<C-int*>));
  check-not-crash("ReleaseTypeAttr",
		  begin
		    ITypeInfo/ReleaseTypeAttr(*typeinfo*, *attr*);
		    *attr* := $null-interface;
		  end );

  check-true("make(<my-monkey>)",
	     instance?(*vi1* := make(<my-monkey>, size: $default-size),
		       <my-monkey>));

  check-equal("QueryInterface(*vi1*, $IID-IMonkey)",
	      begin
		let ( status, interface2 ) =
		  QueryInterface(*vi1*, $IID-IMonkey);
		*vi2* := interface2;
		status
	      end,
	      $S-OK);

  check-equal("*vi1* = *vi2*", *vi1*, *vi2*);

  *typeinfo* := $null-interface;
  check-equal("type-information from class or instance",
	      *typeinfo* := type-information(<my-monkey>),
	      type-information(*vi1*));

  check-true("typeinfo class", instance?(*typeinfo*, <ITypeinfo>));

  check-equal("GetDocumentation",
	      begin
		let (status, name, doc-string, HelpContext, HelpFile ) =
		  ITypeInfo/GetDocumentation(*typeinfo*, $MEMBERID-NIL);
		vector(name, doc-string)
	      end,
	      vector("Monkey",
		     "simple test of custom interface with type info"));

  check-equal("set *vi3*",
	      begin
		*vi3* := make(<C-IMonkey>, address: pointer-address(*vi2*));
		pointer-address(*vi3*)
	      end,
	      pointer-address(*vi2*));

  check-equal("IMonkey/Size",
	      begin
		IMonkey/Size(*vi3*) := -3021;
		IMonkey/Size(*vi3*)
	      end,
	      -3021);

  check-not-crash("Release(*vi2*)",
		  Release(*vi2*));
end test;

