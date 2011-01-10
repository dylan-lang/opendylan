Module:    DUIM-OLE-Control
Synopsis:  Initialization, registration, and activation of a DUIM OLE Control.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function make-ocx-factory (ocx-local-data :: <pair>,
				  rclsid :: <REFCLSID>,
				  riid :: <REFIID>,
				  dll-local-data,
				  #key typeinfo = $unsupplied,
				  class-id = #f, value-type = #f,
				  frame-class, frame-options = #[],
				  title = #f, short-title = #f, name = #f,
				  disp-typeinfo-options = #[],
				  server-module-handle,
				  #all-keys )
 => ( status :: <HRESULT>, factory :: <Interface> );

  Output-Debug-String("DllGetClassObject\r\n");

  let class-id :: <REFCLSID> = 
    if ( class-id ) as(<REFCLSID>, class-id) else typeinfo.guid-value end if;
  if ( ~ IsEqualIID?(rclsid, class-id) )
    values ( $CLASS-E-CLASSNOTAVAILABLE, $null-interface ) // wrong class ID
  else
    if ( typeinfo == $unsupplied )
      if ( instance?(ocx-local-data.head, <ITypeInfo>) )
	typeinfo := ocx-local-data.head;
      else
	// Need to instantiate the frame in order to examine its attributes.
	let frame :: duim/<frame> = apply(make, frame-class,
					  container:
					    get-foster-parent-window(),
					  frame-options);
	let app :: <DUIM-server-app> = make(<DUIM-server-app>, frame: frame);
	let doc :: <DUIM-server-doc> = app.get-doc;
	typeinfo :=
	  compute-type-info(doc.doc-sheet,
			    (title | frame.duim/frame-title | short-title),
			    (name | short-title),
			    class-id, value-type,
			    disp-typeinfo-options);
	// It is too bad to have to throw away this frame and make another
	// later.  It would be more efficient to keep and use this frame,
	// but that is a little tricky to do in a way that will still
	// make a new frame for the second instance created by the class
	// factory.  Just get it to work for now, and optimize later.
	//	-- DNG 10/2/97				???
	duim/destroy-frame(frame);
      end if;
    end if;
    if ( ocx-local-data.head == #f )
      AddRef(typeinfo);
      ocx-local-data.head := typeinfo;
    end if;
    let class :: <class> = typeinfo.instance-class;
    if ( subtype?(<DUIM-OCX>, class) )
      // typeinfo just has default value; override with subclass.
      class := <DUIM-OCX>;
    elseif ( ~ subtype?(class, <DUIM-OCX>) )
      error("%= is not a subclass of %=", class, <DUIM-OCX>);
    end if;
    let factory =
      apply(make, <class-factory-with-lock>,
	    clsid: class-id, // not rclsid because it is stack-allocated!
	    class: class,
	    server-context: $CLSCTX-INPROC-SERVER,
	    dll-local-data: dll-local-data,
	    typeinfo: typeinfo,
	    frame-class: frame-class,
	    frame-options: frame-options,
	    server-module-handle: server-module-handle,
	    typeinfo.instance-args);
    QueryInterface(factory, riid)
  end if
end make-ocx-factory;


define method make-prog-id( typeinfo, title )
 => (prog-id :: <string>);
  invent-prog-id(typeinfo.guid-value,
		 title | coclass-title-string(typeinfo));
end;

define function register-or-unregister( unregister? :: <boolean>,
				       module-handle, #key
				       typeinfo = $unsupplied,
				       prog-id, title, short-title, name,
				       misc-status = $unsupplied,
				       versioned-prog-id, versioned-title,
				       frame-class, frame-options = #[],
				       value-type = #f,
				       class-id = $unsupplied,
				       disp-typeinfo-options = #[],
				       #all-keys )
 => ( status :: <HRESULT> );

  if ( misc-status == $unsupplied | typeinfo == $unsupplied )
    // Need to instantiate the frame to examine its attributes,
    // even though it will not be actually displayed on the screen.
    let frame :: duim/<frame> = apply(make, frame-class,
				      container:
					get-foster-parent-window(),
				      frame-options);
    let app :: <DUIM-server-app> = make(<DUIM-server-app>, frame: frame);
    let doc :: <DUIM-server-doc> = app.get-doc;
    let doc-sheet = doc.doc-sheet;

    if ( misc-status == $unsupplied )
      misc-status := compute-misc-status(frame, doc-sheet);
    end if;

    if ( title == #f )
      title := frame.duim/frame-title | short-title;
    end if;

    if ( typeinfo == $unsupplied )
      if ( class-id == $unsupplied )
	error("OLE Control must specify either typeinfo or class-id.");
      end if;
      typeinfo := compute-type-info(doc-sheet, title, name | short-title,
				    class-id, value-type,
				    disp-typeinfo-options);
    end if;
    duim/destroy-frame(frame);
  end if;

  register-control-coclass(typeinfo,
			   prog-id | make-prog-id(typeinfo, title),
			   unregister?: unregister?,
			   module-handle: module-handle,
			   title: title, short-name: short-title | name,
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



define macro initialize-ole-control
  { initialize-ole-control( ?all-args:* ) }
 => { define variable ?=DUIM-OLE-Control%*local-data* :: <pair> = list(#f);
      initialize-in-process-server(register-fn: register-ocx,
				   unregister-fn: unregister-ocx,
				   get-factory-fn:
				     curry(make-ocx-factory,
					   ?=DUIM-OLE-Control%*local-data*),
				   unloading-fn:
				     curry(dll-unloading,
					   ?=DUIM-OLE-Control%*local-data*),
				   validate-fn:
				     validate-initialize-ole-control-options,
				   module-handle-fn: record-module-handle,
				   args: vector(?all-args) ) }
end macro;

define function record-module-handle (dll-module-handle :: <HMODULE>)
 => ();
  if ( null-handle?(*error-module-handle*) )
    // First Dylan DLL; remember it for use in reporting errors.
    *error-module-handle* := dll-module-handle;
  else
    // More than one Dylan DLL; have to revert to using the EXE name.
    *error-module-handle* := w/application-instance-handle();
  end if;
end;

// This function will be called just before the OCX is unloaded.
define function dll-unloading (ocx-local-data :: <pair>, #rest args) => ()
  let typeinfo = ocx-local-data.head;
  if ( instance?(typeinfo, <interface>) )
    Release(typeinfo);
    ocx-local-data.head := #f;
  end if;
  // This may not be the best place for this???
  let foster-window = *foster-window*;
  if ( foster-window )
    *foster-window* := #f;
    let ok? = w/DestroyWindow(foster-window);
    duim/check-result("DestroyWindow [foster window]", ok?);
  end if;
  if ( zero?(*duim-user-count*) )
    // Have DUIM release its resources before it is unloaded so that
    // a subsequent reload will work.  (Fixes Bug 1870.)
    // (Note: the test may need to be updated when we support DUIM containers.
    //  Or, at least, the container library will also need to increment 
    //  the count.  Or maybe we should have DUIM only shut down if there are
    //  no mapped frames on the display.)
    Output-Debug-String("shutdown-win32-duim\r\n");
    duim/shutdown-win32-duim();
  end if;
end;

define function validate-initialize-ole-control-options
    (#key typeinfo :: <interface> = $null-interface,
          frame-class :: <class>, frame-options :: <sequence> = #[],
	  title :: false-or(<string>), short-title :: false-or(<string>),
          name :: false-or(<string>),
          prog-id :: false-or(<string>),
	  misc-status :: <integer> = 0,
          versioned-prog-id :: false-or(<string>),
          versioned-title :: false-or(<string>),
          value-type :: <type> = <object>,
	  disp-typeinfo-options :: <sequence> = #[],
          class-id :: type-union(<REFCLSID>, <string>) = $IID-NULL )
 => ();
  // This function doesn't actually do anything, we just want to get an
  // error if any invalid options are passed.
  
  // Note that the default values used here are just for the sake of type
  // validation; they are not necessarily the same default values that will
  // be actually used.

end;
