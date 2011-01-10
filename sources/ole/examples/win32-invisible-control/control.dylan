Module:    win32-invisible-control
Synopsis:  server object class and methods for OLE Control.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// This is the "OLE object":
define COM-interface <sample-control-object> ( <ole-control-framework> )

  // Windows data

  slot app-window :: <HWND> = $NULL-HWND;   // main (frame) window handle
  slot doc-window :: <HWND> = $NULL-HWND;   // document (image) window
  constant slot app-module-handle :: <HMODULE>,
    required-init-keyword: server-module-handle:;

  // OLE Control state information

  slot storage-stream :: <LPSTREAM> = $null-istream;
  slot OLE-part-dirty? :: <boolean> = #f;
  slot design-mode? :: <boolean> = #f;

  // Application data (just an example, not actually used here)

  slot dummy-integer-data :: <integer> = 42;
  slot dummy-string-data :: <string> = "Hi";

end <sample-control-object>;

// For use by the window message functions:
define variable *the-application* :: false-or(<sample-control-object>) = #f;

define method initialize ( obj :: <sample-control-object>,
			  #rest args, #key, #all-keys) => ();
  debug-message("initialize %=\n", obj);

  next-method();

  // Note: to really support multiple instances, this should be stored
  // in the window object (using register-C-Dylan-object from the C-FFI
  // library) instead of in a global variable.
  *the-application* := obj;
  
  // app initialization
  register-window-classes(obj.app-module-handle);

  // instance initialization
  let ( main-wnd, doc-wnd ) = create-windows(obj.app-module-handle);
  obj.app-window := main-wnd;
  obj.doc-window := doc-wnd;

  values();
end initialize;

define method terminate (obj :: <sample-control-object>) => ();

  debug-message("terminate %=\n", obj);

  next-method();

  unless ( null-handle?(obj.doc-window) )
    DestroyWindow(obj.doc-window);
    obj.doc-window := $NULL-HWND;
  end;

  unless ( null-handle?(obj.app-window) )
    DestroyWindow(obj.app-window);
    obj.app-window := $NULL-HWND;
  end;

  unregister-window-classes(obj.app-module-handle);

  if ( *the-application* == obj )
    *the-application* := #f;
  end if;
  values()
end method terminate ;


// The OLE-part-... methods are called by the "ole-control-framework" library.

// Return the document window:
define method OLE-part-doc-window ( obj :: <sample-control-object> )
			 => (document-window :: <HWND>)
  obj.doc-window
end method;

define method OLE-part-Create-Streams(obj :: <sample-control-object>,
				      storage :: <LPSTORAGE>)
 => ();

  OLE-part-Release-Streams(obj);

  // create a stream to save the persistent properties
  obj.storage-stream := OLE-util-Create-Stream(storage, $data-stream-name);

  values()
end method;

define method OLE-part-Open-Streams(obj :: <sample-control-object>,
				    storage :: <LPSTORAGE>)
 => ();
  OLE-part-Release-Streams(obj);
  obj.storage-stream := OLE-util-open-stream(storage, $data-stream-name);
  values();
end method;

define method OLE-part-Release-Streams(obj :: <sample-control-object>) => ();

  Release(obj.storage-stream);
  obj.storage-stream := $null-istream;

  values()
end method;

define method OLE-part-draw(obj :: <sample-control-object>, hDC :: <HDC>,
			    rect :: <LPRECTL>)
 => status :: <HRESULT>;
  draw-control(obj, hDC, rect);
  $S-OK;
end method;

define method OLE-part-max-save-size (obj :: <sample-control-object>)
 => (max-size :: <integer>)
  24
end method;

define method OLE-part-ambient-properties ( obj :: <sample-control-object> )
 => ( properties :: <sequence> )
  add-new(next-method(), $DISPID-AMBIENT-USERMODE)
end method;

define method OLE-part-set-ambient-property
    ( obj :: <sample-control-object>,
     disp-id == $DISPID-AMBIENT-USERMODE, user-mode? :: <boolean> ) => ()
  debug-message("%s mode from %s mode\n",
		if (user-mode?) "User" else "Design" end if,
		if (obj.design-mode?) "Design" else "User" end if);
  obj.design-mode? := ~ user-mode?;
end method;




define method data-changed(obj :: <sample-control-object>) => ()
  debug-message("data-changed\n");
  OLE-part-dirty?(obj) := #t;

  // If the data affected the displayed image, we would also need to
  // do the following:
  //
  // InvalidateRect(obj.doc-window, $NULL-RECT, #t); // force repaint of window
  // OLE-util-view-changed(obj); // force update of container's copy
end method;


// Draw the object into an arbitrary DC
define method draw-control(obj :: <sample-control-object>,
		   hDC :: <HDC>, rect :: <LPRECT>)
 => ();

  if ( null-pointer?(rect) )
    let ( x-size, y-size ) = OLE-util-current-size(obj);
    draw-image(hDC, 0, 0, x-size, y-size);
  else
    let x-org :: <integer> = rect.left-value;
    let y-org :: <integer> = rect.top-value;
    draw-image(hDC, x-org, y-org,
	       rect.right-value - x-org, rect.bottom-value - y-org);
  end if;
end method draw-control;


define method OLE-part-draw-metafile (obj :: <sample-control-object>,
				      hDC :: <HDC>)
 => status :: <HRESULT>;
  draw-control(obj, hDC, $NULL-RECT);
  $S-OK;
end method;

define method OLE-part-requested-size ( obj :: <sample-control-object> ) 
 => (width :: <integer>, height :: <integer>);

  values( $image-width, $image-height )
end method;



define constant $data-stream-name :: <LPOLESTR> =
  as(<LPOLESTR>, "InvsCtl"); // arbitrary name for internal identification

define method OLE-part-Save-To-Storage(obj :: <sample-control-object>,
				       storage :: <Interface>,
				       same-as-load? :: <boolean>) => ();

  debug-message("OLE-part-Save-To-Storage\n");

  let data-stream :: <LPSTREAM> = $null-istream;

  if ( ~ same-as-load? )
    data-stream := OLE-util-Create-Stream(storage, $data-stream-name);
  else
    data-stream := obj.storage-stream;
    AddRef(data-stream);
  end if;

  OLE-part-dirty?(obj) := #f;

  // discard old stream contents and prepare to write from the beginning
  istream-rewrite(data-stream);
  OLE-part-Save-To-Stream(obj, data-stream);
  Release(data-stream);

  values()
end method;

define method OLE-part-Save-To-Stream (obj :: <sample-control-object>,
				       data-stream :: <LPSTREAM>) => ()
  // write the persistent properties to the stream
  istream-write-integer(data-stream, obj.dummy-integer-data);
  istream-write-string(data-stream, obj.dummy-string-data);
end method;

define method OLE-part-Load-From-Storage(obj :: <sample-control-object>,
					 width :: <integer>,
					 height :: <integer>) => ();

  debug-message("OLE-part-Load-From-Storage\n");
  let stream = obj.storage-stream;
  OLE-part-Load-From-Stream(obj, stream);
end method;

define method OLE-part-Load-From-Stream (obj :: <sample-control-object>,
					 stream :: <LPSTREAM>) => ()
  // load the persistent properties from the stream
  obj.dummy-integer-data := istream-read-integer(stream);
  obj.dummy-string-data  := istream-read-string(stream);

  // At this point we could do any additional application initialization
  // that depended on the values of the persistent properties.
end method;

define method OLE-part-init-new (obj :: <sample-control-object>)
 => (status :: <HRESULT>);
  next-method();
  // Here we could do any corresponding initialization needed for
  // the case where OLE-part-Load-From-Stream will not be called.

  // Even though it isn't doing anything in this example, this method
  // does need to be defined in order to work around library Bug 3956.
  $S-OK
end;



// IDispatch interface

define dispatch-interface <my-dispinterface> (<simple-dispatch>)
  uuid "{0988C9D7-4F41-11D2-9A6A-006097C90313}";
  virtual property dummy-string-property :: <string>,
    name: "Text", disp-id: $DISPID-TEXT;
  virtual property dummy-integer-property :: <integer>, name: "integer";
  function dummy-function (n :: <integer>) => (result :: <integer>);
  // non-persistent properties and slots could be defined here.
end dispatch-interface;

define method dummy-integer-property ( d :: <my-dispinterface> )
 => ( value :: <integer> );
  let obj :: <sample-control-object> = d.controlling-unknown;
  obj.dummy-integer-data
end method;

define method dummy-integer-property-setter ( value :: <integer>,
					     d :: <my-dispinterface> )
 => ( value :: <integer> );
  let obj :: <sample-control-object> = d.controlling-unknown;
  obj.dummy-integer-data := value;
  data-changed(obj);
  value
end method;

define method dummy-string-property ( d :: <my-dispinterface> )
 => ( string :: <string> );
  let obj :: <sample-control-object> = d.controlling-unknown;
  obj.dummy-string-data
end method;

define method dummy-string-property-setter ( string :: <string>,
					    d :: <my-dispinterface> )
 => ( string :: <string> );
  let obj :: <sample-control-object> = d.controlling-unknown;
  obj.dummy-string-data := as(<byte-string>, string);
  data-changed(obj);
  string
end method;

define method dummy-function ( d :: <my-dispinterface> , n :: <integer> )
 => ( status :: <SCODE>, result :: <integer> )
  let obj :: <sample-control-object> = d.controlling-unknown;

  // If this were a real application, here we would perform some action
  // for the client, which would likely involve the persistent properties
  // (represented here by obj.dummy-integer-data and obj.dummy-string-data).

  values( $S-OK, 0 )
end method;

define coclass my-type-info
  name "InvisCtl";
  documentation "Dylan invisible-at-runtime control example";
  uuid "{0988C9D6-4F41-11D2-9A6A-006097C90313}";
  interface <my-dispinterface>;
  component-class <sample-control-object>;
end coclass;

initialize-ole-control(my-type-info, "HQN.DylanInvisCtl.1",
		       title: "win32-invisible-control",
		       misc-status: logior($OLEMISC-INSIDEOUT,
					   $OLEMISC-ACTIVATEWHENVISIBLE,
					   $OLEMISC-INVISIBLEATRUNTIME),
		       verbs: #[]
		       );

