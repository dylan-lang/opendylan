Module:    DUIM-OLE-Control
Synopsis:  Classes <ocx-frame> and <DUIM-OCX> and their methods.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The OLE representation of the application:

define open COM-interface <DUIM-OCX> ( <DUIM-OLE-server>,
				       <ole-control-framework> )
  sealed slot deferred-accelerator :: <boolean> = #f;
end;

define method initialize ( this :: <DUIM-OCX>, #rest ignore,
			  #key frame, frame-class, frame-options, #all-keys )
 => ();
  Output-Debug-String("initialize <DUIM-OCX>\r\n");
  // Do this first because needed in initialize(<ocx-dispatch>)
  this.app-frame := frame | apply(make, frame-class,
				  container: get-foster-parent-window(),
				  frame-options);
  next-method();
  unless ( slot-initialized?(this, get-doc) )
    let app :: <DUIM-server-app> =
      make(<DUIM-server-app>, frame: this.app-frame, interface: this);
    this.get-doc := app.get-doc;
  end unless;
  values()
end initialize;

// The window to be embedded in the container application has to have style
// $WS-CHILD in order to work properly, but Windows does not allow us to 
// create a child window without specifying a parent, but DUIM creates the
// mirrors long before we get a window handle from the container.  So create
// a window to server as a temporary parent.  This window will never be 
// mapped to the screen.
define constant $window-class-name = "DUIM-top-level-window";
define variable *foster-window* :: false-or(<HWND>)= #f;
define function get-foster-parent-window() => window :: <HWND>;
  *foster-window* | 
  begin
  let port = duim/default-port(); // force registration of window classes
  let handle :: <HWND>
    = w/CreateWindowEx
        (w/$WS-EX-CONTROLPARENT,
	 $window-class-name,		// See RegisterClass call in win32-duim
	 $window-class-name,		// dummy title (won't be seen)
	 w/$WS-OVERLAPPEDWINDOW,	// Style for a normal top-level window
	 w/$CW-USEDEFAULT,		// x position
	 w/$CW-USEDEFAULT,		// y position
	 400,				// width
	 400,				// height
	 $NULL-HWND,			// No parent
	 $null-hMenu,			// Use the window class menu
	 w/application-instance-handle(),
	 $NULL-VOID);			// No data in our WM_CREATE
  duim/check-result("CreateWindow [foster parent]", handle);
  *foster-window* := handle
  end
end;

// The DUIM representation of the application:
/*  // Currently no distinguishing functionality, so this may be obsolete.
define open class <ocx-frame> ( <embeddable-frame> )
end class;
*/
define constant <ocx-frame> :: <class> = <embeddable-frame>;

/*   // if it were a subclass:
// In a DLL, start-frame should not be used; the application is
// controlled by `DllGetClassObject' and `DllRegisterServer' instead.
define method duim/port-start-frame
    (port :: duim/<win32-port>, frame :: <ocx-frame>)
 => (status-code :: false-or(<integer>))

  block()
    cerror("", "start-frame not applicable to <ocx-frame> %=", frame);
  exception (<abort>)
  end block;
  #f
end method;
*/

define sideways method duim/frame-top-level-sheet-class
    (frame :: <ocx-frame>, #key) => (class :: <class>)
  // Note: we wouldn't need this test if <ocx-frame> were
  //	distinct from <embeddable-frame>.
  if ( instance?(frame.frame-ole-object, <DUIM-OCX>) )
    duim/<embedded-top-level-sheet>
  else
    next-method()
  end if
end method;

/* // only useful if <ocx-frame> ~=== <embeddable-frame>
define sideways method duim/frame-wrapper
    (framem :: duim/<win32-frame-manager>, frame :: <ocx-frame>,
     sheet :: false-or(duim/<sheet>))
 => (wrapper :: false-or(duim/<sheet>))
  // Embedded frames don't have any decorations
  sheet
end method;
*/

define sideways method duim/handle-event
    (frame :: <ocx-frame>, event :: duim/<frame-focus-event>) => ()
  next-method();
  let obj = frame.frame-ole-object;
  if ( instance?(obj, <ole-control-framework>) )
    OLE-util-on-focus(obj, #t);
  end if;
end method;

// Unfortunatly, DUIM does not yet seem to have any corresponding event
// for when the focus is lost.		???


define generic ocx-frame ( obj :: <object> ) => ( frame :: <ocx-frame> );

define inline method ocx-frame ( obj :: <DUIM-OCX> )
 => ( frame :: <ocx-frame> );
  obj.app-frame
end method;

define method ocx-frame ( disp :: <simple-dispatch> )
 => ( frame :: <ocx-frame> );
  disp.controlling-unknown.app-frame
end method;

define open COM-interface <ocx-dispatch> ( <simple-dispatch> )
  slot ocx-frame :: <ocx-frame>;
  slot ocx-gadget :: false-or(duim/<gadget>) = #f; // cache wrapped gadget
end;

define method initialize ( disp :: <ocx-dispatch>,
			   #rest args, #key) => ();
  next-method();
  // copy the frame locally for efficiency
  disp.ocx-frame := disp.controlling-unknown.app-frame;
  values()
end method initialize;
