Module:    OLE-Container
Synopsis:  Class <container-app> and its methods.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define inline constant <fixnum> = <integer>; // formerly <small-integer>
define inline constant <unsigned-fixnum> = <integer>;

define open generic container-main-menu ( app :: <container-app> )
 => ( menu :: <HMENU>);

define open primary COM-interface <container-app> ( <object-with-dll-lock>,
						    <lib-init-mixin>,
						    <IUnknown> )

  slot container-frame-window :: <HWND> = $NULL-HWND,  // main window handle
			init-keyword: window-handle:;
  slot container-accelerator-table :: <HACCEL> = null-pointer(<HACCEL>),
			init-keyword: accel:;

  // IOleInPlaceFrame interface implemented by the container:
  slot app-ole-in-place-frame :: <container-ole-in-place-frame>;

  slot container-storage :: <LPSTORAGE> = $null-interface;
	
  slot container-documents :: <list> = #(); // document objects
	
  slot context-help-mode :: <boolean> = #f;
	
  slot help-menu-mode :: <boolean> = #f;

  slot container-main-menu :: <HMENU> = $NULL-HMENU;

  // IOleInPlaceActiveObject interface implemented by the server:
  slot in-place-active-object :: <LPOLEINPLACEACTIVEOBJECT> = $null-interface;
  slot active-object-name :: <byte-string> = "";
  slot active-object-window :: <HWND> = $NULL-HWND; // HWND of UIActive Object
	
  // Color palette used by container
  slot app-standard-palette :: <HPALETTE> = null-handle(<HPALETTE>);
	
end <container-app>;

define method initialize ( app :: <container-app>, #rest ignore,
			   #key palette, #all-keys ) => ();

  next-method();
  OutputDebugString("initialize(<container-app>)\r\n");

  app.app-ole-in-place-frame := make(<container-ole-in-place-frame>, App: app,
				 controlling-unknown: app);

  app.app-standard-palette := palette | create-standard-palette();

  values()
end method initialize;

define method terminate (app :: <container-app>) => ();

  OutputDebugString( "terminate(<container-app>)\r\n");
  container-destroy-documents(app);
  // Release the Storage
  unless ( null?(app.container-storage) ) 
    Release(app.container-storage);
    app.container-storage := $null-interface;
  end unless;
  unless ( null-handle?(app.app-standard-palette) )
    DeleteObject(app.app-standard-palette);
  end unless;
  next-method();
  values()
end method terminate  ;

define method container-destroy-documents(app :: <container-app>) => ();
  // copy the list because it will be modified as documents are closed.
  let docs :: <simple-vector> = as(<vector>, app.container-documents);
  for ( doc in docs )
    block()
      close-doc-object(doc);
    exception (<abort>)
      values();
    end block;
  end for;
  values()
end;

define open generic container-add-frame-ui(app :: <object>);

define method container-add-frame-ui(app :: <container-app>) => ();
	
  IOleInPlaceFrame/SetMenu(app.app-ole-in-place-frame,
			   $NULL-HMENU, null-pointer(<HOLEMENU>), $NULL-HWND);
  container-release-border-space(app);
  values()
end method container-add-frame-ui;

define open generic container-release-border-space(app :: <object>);

// This default method is suitable for a container application that does not
// have any tool bar of its own.  Applications that do have a tool bar
// should provide an override method.
define method container-release-border-space (app :: <container-app>) => ()

  let frame-window = app.container-frame-window;
  unless ( null-handle?(frame-window) )
    container-set-border-space(app, 0, 0, 0, 0);
    InvalidateRect(frame-window, $NULL-RECT, #t);
  end unless;
  values()
end method container-release-border-space;


// A container should use this instead of TranslateAccelerator in its
// event loop in order to properly handle accelerator keys belonging to 
// either the container or to an active embedded object.
// Returns #t if an accelerator was handled, or #f if the caller
// should call TranslateMessage and DispatchMessage.
define method container-handle-accelerators(app :: <container-app>,
					    msg :: <LPMSG>)
	=> handled? :: <boolean>;

  // Only key messages need special handling.
  let message :: <fixnum> = msg.message-value;
  if ( (message < $WM-KEYFIRST) | (message > $WM-KEYLAST) )
    #f
  else
    let activeobj = app.in-place-active-object;
    if ( (~ null?(activeobj) ) & 
       // if we have an in-place active object, give it first chance
       // at handling the event.
       (IOleInPlaceActiveObject/TranslateAccelerator(activeobj, msg)
	// Not `SUCCEEDED?' because returns $S-FALSE when not done.
	= $S-OK) )
       #t
     else
	// otherwise check against the container's accelerators.
	let accel = app.container-accelerator-table;
	(~ null-handle?(accel) &
	   (~ zero?(TranslateAccelerator(msg.hwnd-value, accel, msg))))
     end if
  end if
end method container-handle-accelerators;

//**********************************************************************
//
// CSimpleApp::ContextSensitiveHelp
//
// Purpose:
//      Used in supporting context sensitive help at the app level.
//
//
// Parameters:
//
//      BOOL fEnterMode    -   Entering/Exiting Context Sensitive
//                             help mode.
//
// Return Value:
//
//      None
//
// Function Calls:
//      Function                                    Location
//
//      IOleInPlaceActiveObject::QueryInterface     Object
//      IOleInPlaceObject::ContextSensitiveHelp     Object
//      IOleInPlaceObject::Release                  Object
//
// Comments:
//
//      This function isn't used because we don't support Shift+F1
//      context sensitive help.  Be sure to look at the technotes
//      in the OLE 2.0 toolkit.
//
//********************************************************************


define method IOleWindow/ContextSensitiveHelp(app :: <container-app>,
					      enter-mode? :: <boolean>)
 => (status :: <HRESULT>)

  let result :: <HRESULT> = $S-OK;
  if ( app.context-help-mode ~= enter-mode? )
	        
    app.context-help-mode := enter-mode?;

    // this code "trickles" the context sensitive help via shift+f1
    // to the inplace active object.  See the technotes for implementation
    // details.
    let activeobj = app.in-place-active-object;
    unless ( null?(activeobj) )
      let ( status, interface ) =
	QueryInterface(activeobj, $IID-IOleInPlaceObject);
      let in-place-object :: <LPOLEINPLACEOBJECT> =
	pointer-cast(<LPOLEINPLACEOBJECT>, interface);
      result := IOleWindow/ContextSensitiveHelp(in-place-object, enter-mode?);
      Release(in-place-object);
    end unless;
  end if;
  result
end method IOleWindow/ContextSensitiveHelp;


define method container-context-help ( app :: <container-app> )
 => (do-help? :: <boolean>);
  if ( app.help-menu-mode | app.context-help-mode )
    if ( app.context-help-mode )
	                
      // clear context sensitive help flag
      app.context-help-mode := #f;

      // if there is an InPlace active object, call its context sensitive help
      // method with the FALSE parameter to bring the object out of the
      // CSH state.  See the technotes for details.
      let activeobj = app.in-place-active-object;
      unless ( null?(activeobj) )
	let ( status, interface ) = 
	  QueryInterface(activeobj, $IID-IOleInPlaceObject);
	unless ( FAILED?(status) )
	  let in-place-object :: <LPOLEINPLACEOBJECT> =
	    pointer-cast(<LPOLEINPLACEOBJECT>, interface);
	  IOleWindow/ContextSensitiveHelp(in-place-object, #f);
	  Release(in-place-object);
	end unless;
      end unless;
    end if;

    // See the Microsoft technotes for details on implementing
    // context sensitive help.
    if ( app.help-menu-mode )
      app.help-menu-mode := #f;
      let activeobj = app.in-place-active-object;
      unless ( null?(activeobj) )
	IOleWindow/ContextSensitiveHelp(activeobj, #f);
      end unless;
    end if;
    #t
  else
    #f
  end if;
end method container-context-help;


/* OLE2NOTE: forward the WM_QUERYNEWPALETTE message (via
**    SendMessage) to UIActive in-place object if there is one.
**    this gives the UIActive object the opportunity to select
**    and realize its color palette as the FOREGROUND palette.
**    this is optional for in-place containers. if a container
**    prefers to force its color palette as the foreground
**    palette then it should NOT forward the this message. or
**    the container can give the UIActive object priority; if
**    the UIActive object returns 0 from the WM_QUERYNEWPALETTE
**    message (ie. it did not realize its own palette), then
**    the container can realize its palette.
**    (see ContainerDoc_ForwardPaletteChangedMsg for more info)
**
**    (It is a good idea for containers to use the standard
**    palette even if they do not use colors themselves. this
**    will allow embedded object to get a good distribution of
**    colors when they are being drawn by the container)
**
*/


define method container-query-new-palette(app :: <container-app>)
 => value :: <fixnum>;

  if ( (~ null?(app.active-object-window) )
	& ( ~ zero?(SendMessage(app.active-object-window,
				$WM-QUERYNEWPALETTE, 0, 0)) ) )
    // Object selected its palette as foreground palette
    1
  else
    select-palette(app.container-frame-window, app.app-standard-palette,
		   #f /* fBackground */ )
  end if
end method container-query-new-palette;


// called by application when $WM-PALETTECHANGED received:
define method container-palette-changed ( app :: <container-app>,
					  app-window :: <HWND>,
					  wParam, lParam )
 => ( handled? :: <boolean> );

  let hWndPalChg :: <HWND> = c-type-cast(<HWND>, wParam);
	  
  if ( app-window ~= hWndPalChg )
    select-palette(app-window, app.app-standard-palette,
		   #t // fBackground
		     );
  end if;

  // OLE2NOTE: always forward the WM_PALETTECHANGED message (via
  //    SendMessage) to any in-place objects that currently have
  //    their window visible. this gives these objects the chance
  //    to select their palettes. this is
  //    REQUIRED by all in-place containers independent of
  //    whether they use color palettes themselves--their objects
  //    may use color palettes.
  //    (see ContainerDoc_ForwardPaletteChangedMsg for more info)
  // 
  let handled? :: <boolean> = #f;
  for ( site in app.container-documents )
    if( ~ null?(site.document-ui-active-window) )
      SendMessage(site.document-ui-active-window, $WM-PALETTECHANGED,
		  wParam, lParam);
      handled? := #t;
    end if
  end for;
  handled?
end container-palette-changed;

define method container-activate-application ( app :: <container-app>,
					       active? :: <boolean> ) => ()
  if ( active? )
    container-query-new-palette(app);
  end if;
  let activeobj = app.in-place-active-object;
  unless ( null?(activeobj) )
    IOleInPlaceActiveObject/OnFrameWindowActivate(activeobj, active?);
  end unless;
end;

define method ole-warning (status :: <HRESULT>, context, instance, #rest args)
 => ()
  if ( FAILED?(status) )
    debug-message("%s\n", make-ole-error(status, context, instance, args));
  end if;
end ole-warning;
