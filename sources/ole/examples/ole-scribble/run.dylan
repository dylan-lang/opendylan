Module:    scribble
Author:    Scott McKay and David Gray
Synopsis:  Simple OLE scribble application
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Run the Scribble application as an OLE server

// Class ID
define constant $Scribble-class-ID = "{E714C9C0-B2C4-11D0-8A14-02070119F639}";

// For in-place activation, don't include "Exit" in the menu.
define method frame-container-menus 
    (frame :: <scribble-frame>) => (edit-menus, object-menus, help-menus)
  values(make(<menu>,
	      label: "Scribble",
	      children: vector(make(<menu-button>,
				    label: "&Clear",
				    documentation: "Erase the drawing",
				    selection-mode: #"none",
				    activate-callback:
				      method (button)
					let frame = button.sheet-frame;
					clear-surface(frame.surface);
				      end))),
	 #f, #f)
end method frame-container-menus;

define method scribble () => (frame :: <scribble-frame>)
  let frame
    = make(<scribble-frame>,
	   title: "Scribble",			// window title
	   class-id: $Scribble-class-ID,
           prog-id: "HQN.DUIMScribble.test.1",
	   object-title: "Dylan DUIM Scribble");	// for "insert object" dialog
  start-frame(frame);
  frame
end method scribble;

scribble();
