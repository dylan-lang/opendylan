Module:    OLE-Control-Framework
Synopsis:  DLL initialization for OLE Controls
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro initialize-ole-control
  { initialize-ole-control( ?coclass-type-info:expression, ?prog-id:expression,
			     ?options:* ) }
 => { define variable ?=ole-control%*local-data* :: <pair> = list(#f);
      initialize-in-process-server(register-fn: register-ocx,
				   unregister-fn: unregister-ocx,
				   get-factory-fn:
				     curry(DLL-get-factory,
					   ?=ole-control%*local-data*),
				   unloading-fn:
				     curry(dll-unloading,
					   ?=ole-control%*local-data*),
				   validate-fn:
				     validate-initialize-ole-control-options,
				   module-handle-fn: record-module-handle,
				   args: vector(typeinfo: ?coclass-type-info,
						prog-id: ?prog-id,
						?options) ) }
end macro;

define function register-or-unregister( unregister? :: <boolean>,
				       module-handle, #key
				       typeinfo,
				       prog-id, title, short-title,
				       misc-status,
				       versioned-prog-id, versioned-title,
				       #all-keys )
 => ( status :: <HRESULT> );

  register-control-coclass(typeinfo,
			   prog-id,
			   unregister?: unregister?,
			   module-handle: module-handle,
			   title: title, short-name: short-title,
			   full-name: title,
			   misc-status: misc-status,
			   versioned-prog-id: versioned-prog-id,
			   versioned-title: versioned-title)
end register-or-unregister;

define function register-ocx( #rest options )
 => ( status :: <HRESULT> );
  Output-Debug-String("DllRegisterServer\r\n");
  apply(register-or-unregister, #f, options)
end;

define function unregister-ocx( #rest options )
 => ( status :: <HRESULT> );
  Output-Debug-String("DllUnregisterServer\r\n");
  apply(register-or-unregister, #t, options)
end;


define function record-module-handle (dll-module-handle :: <HMODULE>)
 => ();
  if ( null-handle?(*error-module-handle*) )
    // First Dylan DLL; remember it for use in reporting errors.
    *error-module-handle* := dll-module-handle;
  else
    // More than one Dylan DLL; have to revert to using the EXE name.
    *error-module-handle* := application-instance-handle();
  end if;
end;

define function dll-unloading (ocx-local-data :: <pair>, #rest args) => ()
  let typeinfo = ocx-local-data.head;
  if ( instance?(typeinfo, <interface>) )
    Release(typeinfo);
    ocx-local-data.head := #f;
  end if;
end;

define function validate-initialize-ole-control-options
    (#key typeinfo :: <ITypeinfo>,
	  title :: false-or(<string>), short-title :: false-or(<string>),
          prog-id :: <string>,
	  misc-status :: <integer> = 0,
          versioned-prog-id :: false-or(<string>),
          versioned-title :: false-or(<string>),
          verbs :: false-or(<vector>) )

 => ();
  // This function doesn't actually do anything, we just want to get an
  // error if any invalid options are passed.
end;

define function DLL-get-factory (ocx-local-data :: <pair>,
				 rclsid :: <REFCLSID>,
				 riid :: <REFIID>,
				 dll-local-data,
				 #key typeinfo :: <coclass-Type-Info>,
				      server-module-handle,
				 #all-keys
				 )
 => ( status :: <HRESULT>, factory :: <Interface> );

  Output-Debug-String("DllGetClassObject\r\n");
  let class-id = typeinfo.guid-value;
  if ( ~ IsEqualIID?(rclsid, class-id) )
    // asked for a class other than the one that we support.
    values ( $CLASS-E-CLASSNOTAVAILABLE, $null-interface )
  else
    if ( ocx-local-data.head == #f )
      AddRef(typeinfo);
      ocx-local-data.head := typeinfo;
    end if;
    let factory =
      make-object-factory(typeinfo,
			  server-context: $CLSCTX-INPROC-SERVER,
			  dll-local-data: dll-local-data,
			  server-module-handle: server-module-handle);
    QueryInterface(factory, riid)
  end if
end DLL-get-factory;
 
