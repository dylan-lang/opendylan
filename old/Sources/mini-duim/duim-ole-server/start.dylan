Module:    OLE-Server-Framework
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This line is needed for the debug utilities in OLESTD
OLEDBGDATA-MAIN("DUIMSVR");

// May not need these variables anymore???
define variable *CSimpSvrApp* :: false-or(<CSimpSvrApp>) = #f;
define variable *lpClassFactory* :: <Interface> = $null-interface;


// The application's main program should look like:
//
//	define frame <foo-frame> ...
//    ...
//	start-ole-server(class-ID, <foo-frame>, title: "Foo");


define class <OLE-server-frame-manager> ( duim/<frame-manager> )
end class;

define method duim/frame-wrapper
    (framem :: <OLE-server-frame-manager>, frame :: duim/<frame>, panes)
 => (layout :: duim/<sheet>)
  // For in-line display as an OLE part, omit the menu bar.
  panes
end method duim/frame-wrapper;

define method start-ole-server(class-ID :: <REFGUID>,
			       frame-class :: <class>, #rest frame-args)
 => value :: <fixnum>;
  start-ole-server-frame(class-ID,
			 apply(make, frame-class, frame-args),
			 #t)
end method;

define method start-ole-server-frame (class-ID :: <REFGUID>,
				      frame :: duim/<frame>,
				      activate? :: <boolean>)
 => value :: false-or(<fixnum>);

  block(return)

    let App :: <CSimpSvrApp> = make(<CSimpSvrApp>, id: class-ID);
    *CSimpSvrApp* := App;
    AddRef(App);      // need the app ref. count at 1 to hold the
	                         // app alive.

    ParseCommandLine(App, application-command-line());

    // app initialization
    if ( ~ fInitApplication(App, application-instance-handle()) )
      return(GetLastError());
    end if;

    // instance initialization
    let ( ok :: <boolean>, factory :: <Interface> ) =
      fInitInstance(App,
		    application-instance-handle(),
		    application-show-window(),
		    frame);
    if ( ~ ok )
      return(GetLastError());
    else *lpClassFactory* := factory;
    end if;

    if ( ~ activate? )	// Just process the existing messages
      duim/process-messages(duim/find-port())
    else		// Process until the window quits
      with-stack-structure( msg :: <LPMSG> )
	// message loop
	while ( GetMessage(msg, $NULL-HWND, 0, 0) )
	  block(continue)
	    if ( IsInPlaceActive(App) )

	      // Only key messages need to be sent to OleTranslateAccelerator.
	      // Any other message would result in an extra FAR call to occur
	      // for that message processing...

	      let message :: <integer> = msg.message-value;
	      if ( (message >= $WM-KEYFIRST) & (message <= $WM-KEYLAST) )
		// OleTranslateAccelerator MUST be called, even though this
		// application does not have an accelerator table.  This has to
		// be done in order for the mnemonics for the top level menu
		// items to work properly.
		let doc = GetDoc(App);
		unless ( null?(doc) )
		  let svr-obj :: <CSimpSvrObj> = GetObj(doc);
		  if ( OleTranslateAccelerator(GetInPlaceFrame(svr-obj),
					       GetFrameInfo(svr-obj),
					       msg) = $NOERROR )
		    continue();
		  end if;
		end unless;
	      end if;
	    end if;

	    TranslateMessage(msg);    /* Translates virtual key codes     */ 
	    DispatchMessage(msg);     /* Dispatches message to window     */ 

	  end block;
	end while;
        if ( IsStartedByOle(App) )
          close-app(App);
	end if;
        msg.wParam-value    /* Returns the value from PostQuitMessage */ 
      end with-stack-structure;
   end if;
 end block;
end method start-ole-server-frame;



define open class <ole-server-frame> ( duim/<frame> )
  slot ole-frame-doc-obj :: false-or(<CSimpSvrDoc>) = #f;
  slot ole-frame-dirty? :: <boolean> = #t; // data not saved to storage?
  slot ole-frame-class-id :: <REFGUID>, required-init-keyword: class-id:;
end class;

define method duim/start-frame (frame :: <ole-server-frame>,
				#key activate? = #t);
  start-ole-server-frame(frame.ole-frame-class-id, frame, activate?);
end method;


// Called by the application program to notify the OLE container that the
// application data has changed.
define method ole-data-changed ( frame :: <ole-server-frame> ) => ();
  frame.ole-frame-dirty? := #t;
  let doc = frame.ole-frame-doc-obj;
  if ( doc & IsStartedByOle(doc.m-lpApp) )
    SendOnDataChange(doc.m-lpObj);
  end if;
  values()
end method;

// dummy methods for ordinary frame:

define method ole-data-changed ( frame :: duim/<frame> ) => ();
  values()
end method;

define method ole-frame-dirty? ( frame :: duim/<frame> )
			=> dirty? :: <boolean>;
  #t
end method;

define method ole-frame-dirty?-setter ( value :: <boolean>,
				        frame :: duim/<frame> );
  value
end method;

define method ole-frame-doc-obj-setter ( value, frame :: duim/<frame> );
  value
end method;


// (Originally, these actions were in response to a WM_CLOSE message.)
define method close-app ( App :: <CSimpSvrApp> ) => ();

	Output-Debug-String("*** In close-app *** \r\n");
	
	// if there is still a document
	let doc = GetDoc(App);
	if ( ~ null?(doc) )

	  // if there is still an object within a document
	  if ( ~ null?( GetObj(doc) ) )
	    // this case occurs if there is still
	    // an outstanding Ref count on the object
	    // when the app is trying to go away.
	    // typically this case will occur in
	    // the "open" editing mode.
	    //  Close the document
	    Close-server(GetObj(doc));
	  end if;
	end if;

  /* // no, the window has already been closed.
	// hide the app window
	HideAppWnd(App);
   */
	// if we were started by OLE, unregister the class factory, otherwise
	// remove the ref count on our dummy OLE object
	if ( IsStartedByOle(App) )
	  CoRevokeClassObject(GetRegisterClass(App)); 
	else
	  Release(GetOleObject(App));
	end if;

	Release(App);  // This should close the app.
  values()
end method;


/* //---- this was part of the doc window function; don't
   //		know where it goes now???
      $WM-MENUSELECT => 
	SetStatusText(*CSimpSvrApp*);
*/
