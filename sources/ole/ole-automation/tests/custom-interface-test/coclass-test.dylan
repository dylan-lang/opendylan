Module:    custom-interface-test
Synopsis:  Test use of custom interfaces in a coclass.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $class-id = 
	as(<REFIID>, "{08448350-265D-11D2-9A67-006097C90313}");

define COM-interface <my-coclass-object> ( <simple-component-object> )
end;

define coclass $test-coclass-type-info
  name "CustomCoclassTest";
  documentation "Test Dylan coclass with custom and dual interfaces";
  uuid $class-id;
  component-class <my-coclass-object>;
  interface <my-monkey>;
  interface <my-donkey>;
end;

define variable *registered?* :: <boolean> = #f;

define function register-this-server () => ()
  register-coclass($test-coclass-type-info, "HQNexamples.CustomComTest");
  *registered?* := #t;
end;

define variable *remote-monkey* :: <interface> = $null-interface;
define variable *remote-donkey* :: <interface> = $null-interface;
define variable *remote-dispatch* :: <interface> = $null-interface;
define variable *remote-server* :: <interface> = $null-interface;
define variable *internal-server* :: <interface> = $null-interface;

define variable *typelib* :: <interface> = $null-interface;
define variable *xtypeinfo* :: <interface> = $null-interface;

define test internal-coclass-test 
		    (name: "internal-coclass-test",
		     description: "test coclass with custom interfaces")
  AddRef($test-coclass-type-info);

  check-not-crash("create coclass object",
	  *internal-server* := make(<my-coclass-object>,
				    typeinfo: $test-coclass-type-info));

  test-server(*internal-server*);  
end test internal-coclass-test;

define test typelib-test 
		    (name: "external-coclass-test",
		     description: "test coclass in separate process")

  unless ( *registered?* )
    check-not-crash("register server",
		    register-this-server());
  end unless;

  check-equal("load type library",
	      begin
		let ( status :: <HRESULT>, typelib :: <LPTYPELIB> ) =
		  LoadRegTypeLib($class-id, 0, 0, 0);
		*typelib* := typelib;
		status
	      end,
	      $S-OK);
  unless ( null?(*typelib*) )
    check-equal("type info count",
		ITypeLib/GetTypeInfoCount(*typelib*),
		3); // 1 coclass and two interfaces

    check-equal("get typeinfo from typelib",
		begin
		  let (status :: <HRESULT>, typeinfo :: <LPTYPEINFO>) = 
		    ITypeLib/GetTypeInfoOfGuid (*typelib*,
						as(<REFIID>, $IID-IMonkey));
		  *xtypeinfo* := typeinfo;
		  status
		end,
		$S-OK);

    unless ( null?(*xtypeinfo*) )
    check-equal("GetTypeAttr",
		begin
		  let ( status, attributes ) =
		    ITypeInfo/GetTypeAttr(*xtypeinfo*);
		  *attr* := attributes;
		  status
		end,
		$S-OK);
    check-equal("typekind", *attr*.typekind-value, $TKIND-INTERFACE);
    check-equal("number of monkey members",
		*attr*.cFuncs-value, $number-of-monkey-members);
    check-equal("size of monkey vtable",
		*attr*.cbSizeVft-value,
		$number-of-monkey-members * size-of(<C-int*>));
    check-not-crash("ReleaseTypeAttr",
		    begin
		      ITypeInfo/ReleaseTypeAttr(*xtypeinfo*, *attr*);
		      *attr* := $null-interface;
		    end );
    check-not-crash("release type info",
		    Release(*xtypeinfo*));
    end unless;

    check-not-crash("release type library",
		    Release(*typelib*));
    *typelib* := $null-interface;
  end unless;
end test typelib-test;


define test external-coclass-test 
		    (name: "external-coclass-test",
		     description: "test coclass in separate process")

  unless ( *registered?* )
    check-not-crash("register server",
		    register-this-server());
  end unless;
  // Start another instance of this program to act as the server.
  check-false("start server process",
    null-pointer?(
      *remote-server* := create-COM-instance($class-id,
					     context: $CLSCTX-LOCAL-SERVER)));

  unless ( null?(*remote-server*) )
    test-server(*remote-server*);
  end; // not null server
end test external-coclass-test;


define variable *the-server* :: <interface> = $null-interface;

define constant $arbitrary-number = 6037;

define function test-server( server :: <LPUNKNOWN> ) => ()

    *the-server* := server;
    check-not-crash("AddRef server",
		    AddRef(*the-server*));

    check-equal("get monkey",
		begin
		  let (status, ci) =
		    QueryInterface(*the-server*, $IID-IMonkey);
		  *remote-monkey* := pointer-cast(<C-Imonkey>, ci);
		  status
		end,
		$S-OK);

    unless( null?(*remote-monkey*) )
      check-equal("monkey const",
		  Monkey-const(*remote-monkey*),
		  $Monkey-const);
    end;

    check-equal("get IDispatch",
		begin
		  let (status, di) =
		    QueryInterface(*the-server*, $IID-IDispatch);
		  *remote-dispatch* := pointer-cast(<LPDISPATCH>, di);
		  status
		end,
		$S-OK);

   check-equal("Donkey-const",
	       get-property(*remote-dispatch*, "const"),
	       $Donkey-const);

    check-equal("get donkey",
		begin
		  let (status, di) =
		    QueryInterface(*the-server*, $IID-IDonkey);
		  *remote-donkey* := pointer-cast(<C-Idonkey>, di);
		  status
		end,
		$S-OK);

    unless ( null?(*remote-donkey*) )

      check-equal("IDonkey/Square by dispatch",
		  call-simple-method(*remote-donkey*, "IDonkey/Square", 77),
		  77 * 77);

      check-equal("IDonkey/Square by vtable",
		  begin
		    let ( status, value ) =
		      IDonkey/Square(*remote-donkey*, 111);
		    value
		  end,
		  111 * 111);

      check-equal("code by vtable",
		  Donkey-code(*remote-donkey*),
		  $donkey-code);

      check-equal("code by dispatch",
		  get-property(*remote-donkey*, "Donkey-code"),
		  $donkey-code);

      check-equal("set IDonkey/Size",
		  IDonkey/Size(*remote-donkey*) := $arbitrary-number,
		  $arbitrary-number);
      
      check-equal("get IDonkey/Size",
		  get-property(*remote-donkey*, "Size"),
		  $arbitrary-number);

    end;

  check-not-crash("release interfaces",
	      begin
		Release(*remote-dispatch*);
		Release(*remote-monkey*);
		Release(*remote-donkey*);
	      end);
  *remote-dispatch* := $null-interface;
  *remote-monkey* := $null-interface;
  *remote-donkey* := $null-interface;

  check-not-crash("release server",
		  Release(*the-server*));

end;

define function run-as-server () => ();
  let factory = make-object-factory($test-coclass-type-info);
  let pmsg :: <PMSG> = make(<PMSG>);
  while( GetMessage(pmsg, $NULL-HWND, 0, 0) )
    TranslateMessage(pmsg);
    DispatchMessage(pmsg);
  end while;
  revoke-registration(factory);
end;

define method terminate (this :: <my-coclass-object>) => ()
  next-method();
  PostQuitMessage(0); // exit the event loop
end;
