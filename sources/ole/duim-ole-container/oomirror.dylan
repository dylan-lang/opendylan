Module:    DUIM-OLE-Container
Synopsis:  OLE Object mirror - a <mirror> for an <ole-gadget>
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define open primary class <ole-embedded-mirror> (duim/<mirror>)
  sealed slot duim/mirror-sheet :: <ole-gadget>,
    required-init-keyword: sheet:;
  sealed slot %region :: duim/<bounding-box>, init-keyword: region:;
  sealed slot contained-object :: <duim-contained-object>;
end class <ole-embedded-mirror>;

/*
define sealed method duim/do-make-mirror
    (_port :: duim/<win32-port>, sheet :: <ole-gadget>)
 => (mirror :: <ole-embedded-mirror>)
  ignore(_port);
  make(<ole-embedded-mirror>, sheet: sheet)
end method duim/do-make-mirror;
*/

// temporary work-around because do-make-mirror is sealed
define method duim/make-mirror
    (_port :: duim/<port>, sheet :: <ole-gadget>) 
 => (mirror :: <ole-embedded-mirror>)
  let mirror
    = duim/sheet-direct-mirror(sheet)
      | (duim/sheet-direct-mirror(sheet) := 
	     make(<ole-embedded-mirror>, sheet: sheet));
  mirror
end method duim/make-mirror;

define method initialize (mirror :: <ole-embedded-mirror>,
			  #rest args, #key obj, #all-keys) => ();
  next-method();
  let sheet :: <ole-gadget> = mirror.duim/mirror-sheet;
  let obj = sheet.sheet-contained-object;
  if ( obj == #f ) // OLE object not yet created, initiate the server now.
    let parent-sheet :: duim/<sheet> = sheet.duim/sheet-parent;
    let app :: <duim-container-app> = parent-sheet.frame-container-app;
    let parent-handle = duim/window-handle(parent-sheet);
    let options :: <sequence> = sheet.sheet-insert-options;
    let (x :: <integer>, y :: <integer>) = duim/sheet-position(sheet);
    obj :=
      if ( sheet.sheet-ole-file-name ) 
	apply(insert-object-from-file, app, parent-handle,
	      sheet.sheet-ole-file-name, x: x, y: y, options);
      else
	apply(insert-object-by-class, app, parent-handle,
	      sheet.sheet-ole-class-id, x: x, y: y, options);
      end if;
    sheet.sheet-contained-object := obj;
  end if;
  AddRef(obj);
  mirror.contained-object := obj;
  let (left, top, right, bottom ) = document-edges(obj);
  mirror.%region := duim/make-bounding-box(left, top, right, bottom);
  duim/set-sheet-edges(sheet, left, top, right, bottom);
  obj.alive? := #t; // finished initializing
end method initialize;


define sealed method duim/destroy-mirror 
    (_port :: duim/<win32-port>, sheet :: duim/<sheet>,
     mirror :: <ole-embedded-mirror>) => ()
  duim/sheet-direct-mirror(sheet) := #f; // do this first to avoid recursion
  let obj = mirror.contained-object;
  Release(obj);
  close-doc-object(obj);
end method duim/destroy-mirror;


/// Mirror manipulation

// For non-top-level sheets, we just show the window
define sealed method duim/map-mirror
    (_port :: duim/<win32-port>, sheet :: duim/<sheet>,
     mirror :: <ole-embedded-mirror>) => ()
  // ???
/*
  let hWnd :: w/<HWND> = duim/window-handle(mirror);
  w/ShowWindow(hWnd, w/$SW-SHOWNORMAL);	// no status code for this
  // Sends WM_PAINT message and returns status
  duim/check-result("UpdateWindow", w/UpdateWindow(hWnd))
*/
end method duim/map-mirror;

define sealed method duim/unmap-mirror
    (_port :: duim/<win32-port>, sheet :: duim/<sheet>,
     mirror :: <ole-embedded-mirror>) => ()
  // ???
/*
  let hWnd :: w/<HWND> = duim/window-handle(mirror);
  w/ShowWindow(hWnd, w/$SW-HIDE)		// no status code for this
*/
end method duim/unmap-mirror;

define sealed method duim/raise-mirror 
    (_port :: duim/<win32-port>, sheet :: duim/<sheet>,
     mirror :: <ole-embedded-mirror>, #key activate? = #t) => ()
  ignore(activate?);
  let obj = mirror.contained-object;
  if ( obj )
    let hWnd = document-ui-active-window (obj);
    unless ( w/null-handle?(hWnd) )
      duim/check-result("SetWindowPos ($HWND-TOP)",
			w/SetWindowPos(hWnd, w/$HWND-TOP, 0, 0, 0, 0,
				       %logior(w/$SWP-NOMOVE, w/$SWP-NOSIZE)))
    end unless;
  end if;
end method duim/raise-mirror;

define sealed method duim/lower-mirror
    (_port :: duim/<win32-port>, sheet :: duim/<sheet>,
     mirror :: <ole-embedded-mirror>) => ()
  let obj = mirror.contained-object;
  if ( obj )
    let hWnd = document-ui-active-window (obj);
    unless ( w/null-handle?(hWnd) )
      duim/check-result("SetWindowPos ($HWND-BOTTOM)",
			w/SetWindowPos(hWnd, w/$HWND-BOTTOM, 0, 0, 0, 0,
				       %logior(w/$SWP-NOMOVE, w/$SWP-NOSIZE)))
    end unless;
  end if;
end method duim/lower-mirror;

define sealed method duim/mirror-visible? 
    (_port :: duim/<win32-port>, sheet :: duim/<sheet>,
     mirror :: <ole-embedded-mirror>)
 => (visible? :: <boolean>)
  let obj = mirror.contained-object;
  if ( obj == #f )
    #f
  else
    let hWnd = document-ui-active-window (obj);
    if ( w/null-handle?(hWnd) )
      #f
    else
      w/IsWindowVisible(hWnd)
    end if
  end if
end method duim/mirror-visible?;


/// Window mirrors

define sealed method duim/mirror-edges
    (_port :: duim/<win32-port>, sheet :: duim/<sheet>,
     mirror :: <ole-embedded-mirror>)
 => (left :: <integer>, top :: <integer>,
     right :: <integer>, bottom :: <integer>)
  duim/box-edges(mirror.%region)
end method duim/mirror-edges;

define sealed method duim/set-mirror-edges
    (_port :: duim/<win32-port>, sheet :: <ole-gadget>,
     mirror :: <ole-embedded-mirror>,
     left  :: <integer>, top    :: <integer>,
     right :: <integer>, bottom :: <integer>) => ()
  duim/duim-debug-message("Setting mirror edges for %= to %d x %d at %d,%d",
			  sheet, right - left, bottom - top, left, top);
  mirror.%region := 
    duim/set-box-edges(mirror.%region, left, top, right, bottom); // ???
  let obj = mirror.contained-object;
  if ( obj.alive? )
    // don't yet have a way to specify changed size ???
    document-move(obj, left, top);
  end if;
end method duim/set-mirror-edges;


