Module:    win32-duim
Synopsis:  Win32 frame manager implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Win32 frame management

define sealed class <win32-frame-manager> (<basic-frame-manager>)
end class <win32-frame-manager>;

define sealed domain make (singleton(<win32-frame-manager>));
define sealed domain initialize (<win32-frame-manager>);

define sealed method make-frame-manager
    (_port :: <win32-port>,
     #key palette, class = <win32-frame-manager>, #all-keys)
 => (framem :: <frame-manager>)
  make(class, port: _port, palette: palette)
end method make-frame-manager;


define sealed method frame-manager-do-frames
    (function :: <function>, framem :: <win32-frame-manager>,
     #key z-order :: <z-order> = #f) => ()
  if (z-order)
    // First, collect the windows that are associated with DUIM frames.
    // We collect them before calling the function, since it may alter the
    // Z ordering or the set of windows.
    let frames :: <stretchy-object-vector> = make(<stretchy-vector>);
    for (handle :: <HWND> = GetTopWindow($NULL-HWND)
           then GetNextWindow(handle, $GW-HWNDNEXT),
         until: null-handle?(handle))
      let sheet = handle-sheet(handle);
      let frame = sheet & sheet-frame(sheet);
      when (frame & frame-state(frame) ~== #"destroyed")
        add!(frames, frame)
      end;
    end;
    // Now put the frames into the desired order
    when (z-order == #"bottom-up")
      frames := reverse!(frames)
    end;
    do(function, frames)
  else
    let frames = copy-sequence(frame-manager-frames(framem));
    do(function, frames)
  end
end method frame-manager-do-frames;


define method note-frame-title-changed
    (framem :: <win32-frame-manager>, frame :: <frame>) => ()
  // Update the title in the top-level window
  let top-sheet = top-level-sheet(frame);
  when (top-sheet)
    let mirror = sheet-direct-mirror(top-sheet);
    when (mirror)
      SetWindowText(window-handle(mirror), frame-title(frame) | "")
    end
  end
end method note-frame-title-changed;


define method note-frame-icon-changed
    (framem :: <win32-frame-manager>, frame :: <frame>) => ()
  // Update the icon in the top-level window
  let top-sheet = top-level-sheet(frame);
  when (top-sheet)
    let mirror = sheet-direct-mirror(top-sheet);
    when (mirror)
      update-mirror-icon(mirror, frame-icon(frame))
    end
  end
end method note-frame-icon-changed;

define method update-mirror-icon
    (mirror :: <top-level-mirror>, icon :: false-or(<image>)) => ()
  let hIcon = select (icon by instance?)
                <win32-icon>  => image-handle(icon);
                singleton(#f) => null-pointer(<HICON>);
                otherwise     => #f;
              end;
  when (hIcon)
    let handle = window-handle(mirror);
    SendMessage(handle, $WM-SETICON, $ICON-SMALL, pointer-address(hIcon));
    SendMessage(handle, $WM-SETICON, $ICON-BIG,   pointer-address(hIcon))
  end
end method update-mirror-icon;


//--- We try hard to obey Windows guidelines so that DUIM frames don't
//--- piggishly pop up in front of everything else, but Windows rewards
//--- us by putting them behind everything.  So we have to resort to this.
define method note-frame-mapped
    (framem :: <win32-frame-manager>, frame :: <basic-frame>) => ()
  next-method();
  raise-frame(frame)
end method note-frame-mapped;


define method do-frame-occluded?
    (framem :: <win32-frame-manager>, frame :: <basic-frame>)
 => (occluded? :: <boolean>)
  let sheet  = top-level-sheet(frame);
  let handle = sheet & window-handle(sheet);
  // If it's not attached, just pretend it's occluded
  // If we're not the foreground window, assume we're occluded
  //--- It would be better to use GetNextWindow to loop over all of
  //--- the windows higher in the Z-order, and return #t only if
  //--- there is a window whose GetWindowRect overlaps our own...
  handle ~= GetForegroundWindow()
end method do-frame-occluded?;


define method note-frame-enabled
    (framem :: <win32-frame-manager>, frame :: <basic-frame>) => ()
  let sheet  = top-level-sheet(frame);
  let handle = sheet & window-handle(sheet);
  // Check IsWindowEnabled first to avoid circularity that arises
  // from handling the ensuing $WM-ENABLE message
  handle & ~IsWindowEnabled(handle) & EnableWindow(handle, #t)
end method note-frame-enabled;

define method note-frame-disabled
    (framem :: <win32-frame-manager>, frame :: <basic-frame>) => ()
  let sheet  = top-level-sheet(frame);
  let handle = sheet & window-handle(sheet);
  // Check IsWindowEnabled first to avoid circularity that arises
  // from handling the ensuing $WM-ENABLE message
  handle & IsWindowEnabled(handle) & EnableWindow(handle, #f)
end method note-frame-disabled;


define sealed method note-frame-iconified
    (framem :: <win32-frame-manager>, frame :: <simple-frame>) => ()
  next-method();                                // update the frame's state
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  when (mirror)
    let handle :: <HWND> = window-handle(mirror);
    ShowWindow(handle, $SW-MINIMIZE)            // no status code for this
  end
end method note-frame-iconified;

define sealed method note-frame-deiconified
    (framem :: <win32-frame-manager>, frame :: <simple-frame>) => ()
  next-method();                                // update the frame's state
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  when (mirror)
    let handle :: <HWND> = window-handle(mirror);
    ShowWindow(handle, $SW-RESTORE)             // no status code for this
  end
end method note-frame-deiconified;


define sealed method note-frame-maximized
    (framem :: <win32-frame-manager>, frame :: <simple-frame>) => ()
  next-method();                                // update the frame's state
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  when (mirror)
    let handle :: <HWND> = window-handle(mirror);
    ShowWindow(handle, $SW-MAXIMIZE)            // no status code for this
  end
end method note-frame-maximized;

define sealed method note-frame-unmaximized
    (framem :: <win32-frame-manager>, frame :: <simple-frame>) => ()
  next-method();                                // update the frame's state
  let sheet  = top-level-sheet(frame);
  let mirror = sheet & sheet-direct-mirror(sheet);
  when (mirror)
    let handle :: <HWND> = window-handle(mirror);
    ShowWindow(handle, $SW-RESTORE)             // no status code for this
  end
end method note-frame-unmaximized;
