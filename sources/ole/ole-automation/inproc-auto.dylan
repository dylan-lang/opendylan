Module:    OLE-Automation
Synopsis:  Initialization, registration, and activation of in-process server.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function make-automation-factory (rclsid :: <REFCLSID>,
					 riid :: <REFIID>,
					 dll-local-data,
					 #key typeinfo, class-id,
					 class = #f, args = #f,
					 server-module-handle,
					 #all-keys )
 => ( status :: <HRESULT>, factory :: <Interface> );

  Output-Debug-String("DllGetClassObject\r\n");

  let class-id :: <REFCLSID> = 
    if (class-id) as(<REFCLSID>, class-id) else typeinfo.guid-value end if;
  if (~IsEqualIID?(rclsid, class-id))
    values ($CLASS-E-CLASSNOTAVAILABLE, $null-interface) // wrong class ID
  else
    let factory =
      apply(make, <class-factory-with-lock>,
	    clsid: class-id, // not rclsid because it is stack-allocated!
	    class: class | if ( instance?(typeinfo, <coclass-Type-Info>) )
			     typeinfo.instance-class
			   else
			     <Simple-Dispatch>
			   end if,
	    server-context: $CLSCTX-INPROC-SERVER,
	    dll-local-data: dll-local-data,
	    typeinfo: typeinfo,
	    server-module-handle: server-module-handle,
	    args | if ( instance?(typeinfo, <coclass-Type-Info>) )
		     typeinfo.instance-args
		   else
		     #[]
		   end if);
    QueryInterface(factory, riid)
  end if
end make-automation-factory;


define function register-or-unregister (unregister? :: <boolean>,
					module-handle,
					#key typeinfo,
					prog-id, title = #f,
					versioned-prog-id, versioned-title,
					class-id = #f,
					#all-keys )
 => ( status :: <HRESULT> );

  register-coclass (typeinfo, prog-id, title: title,
		    class-id: class-id,
		    versioned-prog-id: versioned-prog-id,
		    versioned-title: versioned-title,
		    module-handle: module-handle,
		    unregister?: unregister?);
  $S-OK
end register-or-unregister;

define function register-auto-server (module-handle, #rest options)
 => ( status :: <HRESULT> );
  Output-Debug-String("DllRegisterServer\r\n");
  apply(register-or-unregister, #f, module-handle, options)
end;

define function unregister-auto-server (module-handle, #rest options)
 => ( status :: <HRESULT> );
  Output-Debug-String("DllUnregisterServer\r\n");
  apply(register-or-unregister, #t, module-handle, options)
end;



define macro in-process-automation-server
  { in-process-automation-server( ?all-args:* ) }
 => { initialize-in-process-server(register-fn: register-auto-server,
				   unregister-fn: unregister-auto-server,
				   get-factory-fn: make-automation-factory,
				   unloading-fn: unloading,
				   validate-fn:
				     validate-automation-server-options,
				   args: vector(?all-args) ) }
end macro;

define function unloading(#key typeinfo, #all-keys) => ();
  Release(typeinfo);
end function;

define function validate-automation-server-options
    (#key typeinfo :: <ITypeInfo>,
	  title :: false-or(<string>),
          prog-id :: <string>,
          class :: <class> = <simple-component-object>,
          args :: <sequence> = #[],
          versioned-prog-id :: false-or(<string>),
          versioned-title :: false-or(<string>),
          class-id :: type-union(<REFCLSID>, <string>) = $IID-NULL)
 => ();
  // This function doesn't actually do anything, we just want to get an
  // error if any invalid options are passed.
  
  // Note that the default values used here are just for the sake of type
  // validation; they are not necessarily the same default values that will
  // be actually used.

  AddRef(typeinfo);
end;
