module:    scribble
Author:    Scott McKay and David Gray
Synopsis:  Simple OLE control (OCX) scribble application
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Run the Scribble application as an OLE control

// Class ID
define constant $Scribble-class-ID = "{1FBD4702-8F73-11D0-8A10-02070119F639}";

// Dispatch methods
define method clear-function
    (dispatch :: <scribble-dispatch>) => (status)
  returning-error-status
    let frame :: <scribble-frame> = dispatch.ocx-frame;
    clear-surface(frame.surface);
  end;
end method clear-function;

// Dispatch properties
define method n-lines-property 
    (dispatch :: <scribble-dispatch>) => (n-lines :: <integer>)
  let frame :: <scribble-frame> = dispatch.ocx-frame;
  let sheet = frame.surface;
  let segments = sheet.scribble-segments;
  size(segments)
end method n-lines-property;

define dispatch-interface <scribble-dispatch> (<ocx-dispatch>)
  name "scribble_controls";
  uuid "{1FBD4701-8F73-11D0-8A10-02070119F639}";
  virtual property n-lines-property, setter: #f, name: "nlines", type: <C-int>,
    documentation: "Number of lines";
  function clear-function, name: "clear",
    documentation: "Erase the scribble pane";
end;

// Composite object description
define coclass scribble-type-info
  name "Scribble";
  uuid $Scribble-class-ID;
  documentation "Dylan DUIM OCX Scribble";
  interface <scribble-dispatch>;
end;

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

// Initialize the application
initialize-ole-control(frame-class: <scribble-frame>,
		       typeinfo: scribble-type-info);
