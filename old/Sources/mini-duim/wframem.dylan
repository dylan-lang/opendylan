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

define method make-frame-manager
    (_port :: <win32-port>,
     #key palette, class = <win32-frame-manager>, #all-keys)
 => (framem :: <frame-manager>)
  make(class, port: _port, palette: palette)
end method make-frame-manager;

//---*** This needs more thought to get it right, particularly with
//---*** respect to sizing and spacing issues
define method frame-wrapper
    (framem :: <win32-frame-manager>, 
     frame :: <simple-frame>,
     layout :: false-or(<sheet>))
 => (wrapper :: false-or(<sheet>))
  let children = make(<stretchy-vector>);
  let menu-bar   = frame-menu-bar(frame);
  let tool-bar   = frame-tool-bar(frame);
  let status-bar = frame-status-bar(frame);
  if (menu-bar)   add!(children, menu-bar)   end;
  if (tool-bar)   add!(children, tool-bar)   end;
  if (layout)     add!(children, layout)     end;
  if (status-bar) add!(children, status-bar) end;
  unless (empty?(children))
    make-pane(<column-layout>, children: children)
  end
end method frame-wrapper;


/// Glue to frames

define method note-title-changed
    (framem :: <win32-frame-manager>, frame :: <frame>) => ()
  // Update the title in the window
  let top-sheet = top-level-sheet(frame);
  if (top-sheet)
    let mirror = sheet-direct-mirror(top-sheet);
    if (mirror)
      SetWindowText(mirror.%window-handle, frame-title(frame) | "")
    end
  end
end method note-title-changed;

define method update-frame-layout
    (framem :: <win32-frame-manager>, frame :: <simple-frame>) => ()
  update-frame-wrapper(framem, frame)
end method update-frame-layout;

define method update-frame-wrapper
    (framem :: <win32-frame-manager>, frame :: <simple-frame>) => ()
  let top-level-sheet = top-level-sheet(frame);
  if (top-level-sheet)
    #f	//---*** Do it
  end  
end method update-frame-wrapper;
