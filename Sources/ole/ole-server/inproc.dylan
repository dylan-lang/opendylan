Module:    OLE-Server
Synopsis:  Additional code for "in-process" servers (i.e. DLL instead of EXE).
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define open COM-interface <ole-in-process-server> ( <ole-server-framework>,
						    <object-with-dll-lock> )

  slot data-cache-iunknown :: <LPUNKNOWN> = $null-interface;
  slot server-IViewObject  :: false-or(<CViewObject>) = #f;

end <ole-in-process-server>;

define method after-initialize ( obj :: <ole-in-process-server> ) => ();

  // Note: not clear whether the second argument below should be 
  //  obj.obj-class-ID -- the Microsoft documentation says that it is usually 
  //  CLSID_NULL (but doesn't say when it should be otherwise), but the
  //  examples show using the object's class ID.
  let ( status, cache ) =
    CreateDataCache(obj.controlling-unknown, $CLSID-NULL, $IID-IUnknown);
  check-ole-status(status, "CreateDataCache", obj);
  obj.data-cache-iunknown := pointer-cast(<LPUNKNOWN>, cache);

  obj.server-IPersistStorage := make(<inproc-PersistStorage>,
				     server-framework: obj,
				     controlling-unknown: obj);
  next-method();
  obj.server-IViewObject := make(<CViewObject>,
				 server-framework: obj,
				 controlling-unknown: obj);
end after-initialize;  

define method terminate (obj :: <ole-in-process-server>) => ();
  next-method();
  IUnknown/Release(obj.data-cache-iunknown);
  obj.data-cache-iunknown := $null-interface;

  if ( *initialized-hatch* )
    // Need to unregister the Windows class before the DLL is unloaded.
    // This call will fail if some other object still has an instance of
    // a hatch window, but that's OK -- we'll do this again when that 
    // object terminates.
    if ( UnregisterClass($hatch-class-name, application-instance-handle()) )
      Output-Debug-String("unregistered hatch\r\n"); // temporary???
      *initialized-hatch* := #f;
    end if;
  end if;

  values()
end method terminate;

define method ole-local-server? (obj :: <ole-in-process-server>)
 => (local-server? :: <boolean>);
  #f // in-process server (.DLL file, not .EXE file)
end;

define method IUnknown/QueryInterface( obj :: <ole-in-process-server>,
				       rrid :: <REFIID> )
		=> ( status :: <HRESULT> , Object :: <Interface> );
  
  let ( status, interface ) = next-method();
/*
  if( $enable-debug-messages )
    debug-out("QueryInterface %s => %=\n", as(<string>, rrid), interface);
  end if;
*/
  if ( (status = $E-NOINTERFACE) & ~ null-pointer?(obj.data-cache-iunknown) )
    // for IOleCache[2]
    IUnknown/QueryInterface(obj.data-cache-iunknown, rrid)
  else
    values(status, interface)
  end if
end method;

define method query-cache-interface ( obj :: <ole-in-process-server>,
				      interface-id :: <REFIID> )
 => ( interface :: <interface> );
  let ( status, interface ) =
    IUnknown/QueryInterface(obj.data-cache-iunknown, interface-id);
  if ( FAILED?(status) )
    ole-cerror(status, "query cache", obj, as(<string>, interface-id));
  else
    // Because the cache is aggregated on our object, it has incremented its
    // ref count, which needs to be un-done in order to know when the object
    // is no longer being used.  Use `SubRef' instead of `Release' to 
    // decrement back to 0 without terminating.  (This gets used during
    // initialization before the real reference is established.)
    SubRef(obj);
  end if;
  interface
end query-cache-interface;



define function make-inproc-factory (rclsid :: <REFCLSID>,
				     riid :: <REFIID>,
				     dll-local-data,
				     #key class-id,
				     class,
				     server-module-handle,
				     args = #[],
				     #all-keys )
 => ( status :: <HRESULT>, factory :: <Interface> );

  Output-Debug-String("DllGetClassObject\r\n");

  let class-id :: <REFCLSID> = as(<REFCLSID>, class-id);
  if ( ~ IsEqualIID?(rclsid, class-id) )
    values ( $CLASS-E-CLASSNOTAVAILABLE, $null-interface ) // wrong class ID
  else
    let factory =
      apply(make, <class-factory-with-lock>,
	    clsid: class-id, // not rclsid because it is stack-allocated!
	    class: class,
	    server-context: $CLSCTX-INPROC-SERVER,
	    dll-local-data: dll-local-data,
	    server-module-handle: server-module-handle,
	    args);
    QueryInterface(factory, riid)
  end if
end make-inproc-factory;


define function register-or-unregister (unregister? :: <boolean>,
					module-handle,
					#key
					prog-id, title,
					class-id,
					misc-status = 0,
					full-name, short-name, app-name,
					verbs,
					#all-keys )
 => ( status :: <HRESULT> );

  register-ole-server(class-id, prog-id, title,
		      module-handle: module-handle,
		      unregister?: unregister?,
		      full-name: full-name,
		      short-name: short-name,
		      app-name: app-name,
		      misc-status: misc-status,
		      verbs: verbs);
  $S-OK
end register-or-unregister;

define function register-inproc-server (module-handle, #rest options)
 => ( status :: <HRESULT> );
  Output-Debug-String("DllRegisterServer\r\n");
  apply(register-or-unregister, #f, module-handle, options)
end;

define function unregister-inproc-server (module-handle, #rest options)
 => ( status :: <HRESULT> );
  Output-Debug-String("DllUnregisterServer\r\n");
  apply(register-or-unregister, #t, module-handle, options)
end;



define macro initialize-ole-server
  { initialize-ole-server(?class:expression, ?class-id:expression,
			  ?prog-id:expression, ?title-string:expression,
			  ?options:* ) }
 => { initialize-in-process-server(register-fn: register-inproc-server,
				   unregister-fn: unregister-inproc-server,
				   get-factory-fn: make-inproc-factory,
				   validate-fn: validate-inproc-server-options,
				   args: vector(class: ?class,
						class-id: ?class-id,
						prog-id: ?prog-id,
						title: ?title-string,
						?options) ) }
end macro;


define function validate-inproc-server-options
    (#key title :: <string>,
	  prog-id :: <string>,
	  full-name :: false-or(<string>),
	  short-name :: false-or(<string>),
	  app-name :: false-or(<string>),
	  class :: <class> = <ole-in-process-server>,
	  args :: <sequence> = #[],
	  class-id :: type-union(<REFCLSID>, <string>) = $IID-NULL,
	  misc-status :: <integer> = 0,
          verbs :: false-or(<vector>))
 => ();
  // This function doesn't actually do anything, we just want to get an
  // error if any invalid options are passed.
  
  // Note that the default values used here are just for the sake of type
  // validation; they are not necessarily the same default values that will
  // be actually used.
end;
