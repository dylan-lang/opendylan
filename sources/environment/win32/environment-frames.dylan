Module:    win32-environment
Synopsis:  Win32-specific <frame> handling
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Place a window in Z order

/*
define constant <relative-order> = one-of(#"bottom", #"top");
define constant <frame-order>    = type-union(<relative-order>, <frame>);
*/
define constant <sheet-order>    = type-union(<relative-order>, <sheet>);
define constant <mirror-order>   = type-union(<relative-order>, <mirror>);

define sealed method reorder-mirror 
    (_port  :: <win32-port>,
     sheet  :: <sheet>, mirror :: <window-mirror>, where  :: <mirror-order>) => ()
  local method dbg-msg (where-to :: <string>) => ()
	  debug-message("reorder-mirror: placing mirror for frame \"%s\" %s",
			mirror.mirror-sheet.sheet-frame.frame-title, where-to);
	end method;
  let where-handle
    = case
	instance?(where, <window-mirror>) =>
	  dbg-msg(concatenate("behind frame \"",
			      where.mirror-sheet.sheet-frame.frame-title,
			      "\""));
	  window-handle(where);
	where = #"top" =>
	  dbg-msg("at top using $HWND-TOP");
	  $HWND-TOP; // $HWND-NOTOPMOST;
	where = #"bottom" =>
	  dbg-msg("at bottom using $HWND-BOTTOM");
	  $HWND-BOTTOM;
      end;
  let handle :: <HWND> = window-handle(mirror);
  //---*** cpage: 1998.07.07 Experiment with this flag.
  let activate-flag = if (where = #"top") 0 else $SWP-NOACTIVATE end;
  if (activate-flag = 0)
    debug-message("                Activating (activate-flag = 0)")
  else
    debug-message("                Not activating (activate-flag = $SWP-NOACTIVATE)")
  end;
  //--- cpage: 1998.07.20 Let's try using SetForegroundWindow or SetActiveWindow
  //           for the front window.
  if (where = #"top")
    check-result("SetForegroundWindow", SetForegroundWindow(handle));
    // check-result("SetActiveWindow", SetActiveWindow(handle));
  else
    check-result("SetWindowPos",
		 SetWindowPos(handle, where-handle, 0, 0, 0, 0,
			      %logior($SWP-NOMOVE, $SWP-NOSIZE, activate-flag)))
  end
end method reorder-mirror;

//--- Unlike DUIM's current implementation of raise-sheet, we don't
//--- handle child sheets. That would require altering DUIM's method
//--- on do-raise-sheet, apparently.
define sealed method reorder-sheet
    (sheet :: <sheet>, where :: <sheet-order>) => (sheet :: <sheet>)
  let mirror = sheet-direct-mirror(sheet);
  when (mirror)
    let mirror-where
      = if (instance?(where, <sheet>))
	  sheet-direct-mirror(where)
	else
	  where
	end;
    when (mirror-where)
      reorder-mirror(port(sheet), sheet, mirror, mirror-where)
    end
  end;
  sheet
end method reorder-sheet;

define sealed sideways method reorder-frame
    (frame :: <frame>, where :: <frame-order>) => (frame :: <frame>)
  let top-sheet = top-level-sheet(frame);
  assert(top-sheet & sheet-mapped?(top-sheet),
         "Attempted to reorder %=, which isn't mapped",
         frame);
  let sheet-where
    = if (instance?(where, <frame>))
	let where-top-sheet = top-level-sheet(where);
	assert(where-top-sheet & sheet-mapped?(where-top-sheet),
	       "Attempted to reorder below %=, which isn't mapped",
	       where);
	where-top-sheet
      else
	where
      end;
  reorder-sheet(top-sheet, sheet-where);
  frame
end method reorder-frame;

// Set the Z order of more than one frame at a time
define sealed sideways method order-frames
    (frames :: <sequence>) => ()
  // Be lenient when getting window handles.  Because of multithreading,
  // a frame's mirror may be gone before we operate on it.
  local method frame-window-handle (frame :: <frame>) => (handle :: false-or(<HWND>))
	  let sheet  = top-level-sheet(frame);
	  let mirror = sheet & sheet-direct-mirror(sheet);
	  mirror & window-handle(mirror)
	end method;
  let handles = remove(map(frame-window-handle, frames), #f);
  let defer-handle :: <HDWP> = BeginDeferWindowPos(size(frames));
  check-result("BeginDeferWindowPos", defer-handle);
  for (handle :: <HWND> in handles,
       i :: <integer> from 0)
    let (where :: <HWND>, activate-flag)
      = if (i = 0)
	  values($HWND-TOP, 0)
	else
	  values(handles[i - 1], $SWP-NOACTIVATE)
	end;
    defer-handle := DeferWindowPos(defer-handle, handle, where,
				   0, 0, 0, 0,
				   %logior($SWP-NOMOVE, $SWP-NOSIZE, activate-flag));
    check-result("DeferWindowPos", defer-handle);
  end;
  check-result("EndDeferWindowPos",
	       EndDeferWindowPos(defer-handle));
end method order-frames;

// Restore a frame from minimized/maximized state without bringing
// it to the top or activating it.
define sealed sideways method restore-frame
    (frame :: <frame>) => (frame :: <frame>)
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  when (mirror)
    let handle :: <HWND> = window-handle(mirror);
    ShowWindow(handle, $SW-SHOWNOACTIVATE); // no status code for this
  end;
  frame
end method restore-frame;
