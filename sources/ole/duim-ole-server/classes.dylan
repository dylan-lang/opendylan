Module:    DUIM-OLE-server
Synopsis:  Define the classes and accessors.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <fixnum> = <integer>; // formerly known as <small-integer>

// The "application" object:
define open primary class <DUIM-server-app> ( <object> )

  slot app-frame :: <embeddable-frame>, required-init-keyword: frame:;
        
  slot app-window :: <HWND> = $NULL-HWND;   // top-level frame window handle
        
  slot started-by-ole? :: <boolean> = #f; // true if process started by OLE

  slot dummy-object :: <interface> = $null-interface;
        
  slot get-doc :: false-or(<DUIM-server-doc>) = #f;

  slot get-factory :: false-or(<class-factory>) = #f;

end class <DUIM-server-app>;


// The "document" object:
define open primary class <DUIM-server-doc> ( <object> )
        
  constant slot get-app :: <DUIM-server-app>, required-init-keyword: app:;
  slot get-object  :: false-or(<basic-DUIM-OLE-server>) = #f,
    init-keyword: interface:;
        
  slot doc-window  :: <HWND> = $NULL-HWND;
  slot doc-sheet   :: false-or(duim/<sheet>) = #f;
        
end class <DUIM-server-doc>;


define method app-window(this :: <DUIM-server-doc>) => value :: <HWND>; 
  this.get-app.app-window
end method app-window;



// The OLE "object" object:

// This class is common to OLE compound document server and OLE control:
define open COM-interface <basic-DUIM-OLE-server> ( <basic-ole-server> )

  slot get-doc :: <DUIM-server-doc>, init-keyword: doc:;
  slot app-frame :: <embeddable-frame>, init-keyword: frame:;
end <basic-DUIM-OLE-server>;

// This class is just for an OLE compound document server:
define open COM-interface <DUIM-OLE-server> ( <basic-DUIM-OLE-server>,
					 <ole-server-framework> )

  slot user-data-stream :: <LPSTREAM> = $null-istream;

end <DUIM-OLE-server>;

// We can call `shutdown-win32-duim' which this is decremented back to 0:
define variable *duim-user-count* :: <integer> = 0;

define method initialize ( this :: <basic-DUIM-OLE-server>,
			  #rest args, #key, #all-keys) => ();
  Output-Debug-String("initialize <basic-DUIM-OLE-server>\r\n");

  *duim-user-count* := *duim-user-count* + 1;
  next-method();
  if ( slot-initialized?(this, get-doc) )
    this.get-doc.get-object := this;
  end if;
  if ( slot-initialized?(this, app-frame) )
    this.app-frame.frame-ole-object := this;
  end if;
  values();
end initialize;


define method terminate (this :: <basic-DUIM-OLE-server>) => ();

  Output-Debug-String("terminate(<basic-DUIM-OLE-server>)\r\n");

  let doc :: <DUIM-server-doc> = this.get-doc;
  let app :: <DUIM-server-app> = doc.get-app;

  if ( started-by-ole?(app) )
    // For a local server started by OLE, post ourselves a close message
    // to terminate the application process.
    PostMessage(app-window(doc), $WM-SYSCOMMAND, $SC-CLOSE, 0);
  elseif ( instance?(this, <ole-in-process-server>) )
    // For an in-process server that has completed and is eligible to be
    // unloaded, need to destroy all of its windows before the window
    // callback function goes away.
    duim/destroy-frame(app.app-frame);
  end if;

  // clear the OBJ ptr in the doc class
  doc.get-object := #f;

  *duim-user-count* := *duim-user-count* - 1;
  next-method()
end method terminate ;


define inline method doc-sheet ( obj :: <basic-DUIM-OLE-server> )
 => (sheet :: false-or(duim/<sheet>))
  obj.get-doc.doc-sheet
end method;

define inline method doc-window ( obj :: <basic-DUIM-OLE-server> )
 => (window :: <HWND>)
  obj.get-doc.doc-window
end method;

