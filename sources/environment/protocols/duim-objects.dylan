Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DUIM objects
///
/// Any object in a DUIM hierarchy.

define sealed class <duim-object> (<user-object>)
end class <duim-object>;

define open generic duim-object-composite?
    (server :: <server>, duim-object :: <duim-object>)
 => (composite? :: <boolean>);

define open generic do-duim-object-children
    (function :: <function>, 
     server :: <server>,
     duim-object :: <duim-object>)
 => ();


/// DUIM IDs

define constant $duim-sheets-library-id
  = make(<library-id>,
	 name: "duim-sheets");

define constant $duim-sheets-module-id
  = make(<module-id>, 
	 name: "duim-sheets", 
	 library: $duim-sheets-library-id);

define constant $duim-sheets-internals-module-id
  = make(<module-id>, 
	 name: "duim-sheets-internals", 
	 library: $duim-sheets-library-id);

define constant $port-displays-id
  = make(<definition-id>, 
	 name: "port-displays",
	 module: $duim-sheets-internals-module-id);

define constant $port-frame-managers-id
  = make(<definition-id>, 
	 name: "port-frame-managers",
	 module: $duim-sheets-internals-module-id);

define constant $frame-manager-frames-id
  = make(<definition-id>, 
	 name: "frame-manager-frames",
	 module: $duim-sheets-module-id);

define constant $sheet-children-id
  = make(<definition-id>, 
	 name: "sheet-children",
	 module: $duim-sheets-module-id);

define constant $duim-gadgets-library-id
  = make(<library-id>,
	 name: "duim-gadgets");

define constant $duim-gadgets-module-id
  = make(<module-id>, 
	 name: "duim-gadgets", 
	 library: $duim-gadgets-library-id);

define constant $gadget-label-id
  = make(<definition-id>, 
	 name: "gadget-label",
	 module: $duim-gadgets-module-id);

define constant $duim-frames-library-id
  = make(<library-id>,
	 name: "duim-frames");

define constant $duim-frames-module-id
  = make(<module-id>, 
	 name: "duim-frames", 
	 library: $duim-frames-library-id);

define constant $duim-frames-internals-module-id
  = make(<module-id>, 
	 name: "duim-frames-internals",
	 library: $duim-frames-library-id);

define constant $frame-title-id
  = make(<definition-id>, 
	 name: "frame-title",
	 module: $duim-frames-module-id);

define constant $frame-menu-bar-id
  = make(<definition-id>, 
	 name: "%menu-bar",
	 module: $duim-frames-internals-module-id);

define constant $frame-tool-bar-id
  = make(<definition-id>, 
	 name: "%tool-bar",
	 module: $duim-frames-internals-module-id);

define constant $frame-layout-id
  = make(<definition-id>, 
	 name: "%layout",
	 module: $duim-frames-internals-module-id);

define constant $frame-status-bar-id
  = make(<definition-id>, 
	 name: "%status-bar",
	 module: $duim-frames-internals-module-id);


// DUIM classes

define user-object-class <duim-port> (<duim-object>)
  binding <port>, module: duim-sheets, library: duim-sheets;
end user-object-class <duim-port>;

define user-object-class <duim-frame-manager> (<duim-object>)
  binding <frame-manager>, module: duim-sheets, library: duim-sheets;
end user-object-class <duim-frame-manager>;

define user-object-class <duim-frame> (<duim-object>)
  binding <frame>, module: duim-sheets, library: duim-sheets;
end user-object-class <duim-frame>;

define user-object-class <duim-sheet> (<duim-object>)
  binding <sheet>, module: duim-sheets, library: duim-sheets;
end user-object-class <duim-sheet>;

define user-object-class <duim-gadget> (<duim-sheet>)
  binding <gadget>, module: duim-gadgets, library: duim-gadgets;
end user-object-class <duim-gadget>;


/// Project dispatching methods

define method duim-object-composite?
    (server :: <server>, object :: <duim-object>)
 => (composite? :: <boolean>)
  #t
end method duim-object-composite?;

define method duim-object-composite?
    (server :: <server>, sheet :: <duim-sheet>)
 => (composite? :: <boolean>)
  //---*** Need a way to test if the slot exists
  user-object-slot-value(server, sheet, $sheet-children-id) ~= #f
end method duim-object-composite?;

define method do-duim-object-children
    (function :: <function>, server :: <server>, object :: <duim-object>)
 => ()
  #f
end method do-duim-object-children;

define method do-duim-object-children
    (function :: <function>, server :: <server>, sheet :: <duim-port>)
 => ()
  let displays = user-object-slot-value(server, sheet, $port-displays-id);
  displays & do-collection-elements(function, server, displays);
  let frame-managers = user-object-slot-value(server, sheet, $port-frame-managers-id);
  frame-managers & do-collection-elements(function, server, frame-managers);
end method do-duim-object-children;

define method do-duim-object-children
    (function :: <function>, server :: <server>, sheet :: <duim-frame-manager>)
 => ()
  let children = user-object-slot-value(server, sheet, $frame-manager-frames-id);
  children & do-collection-elements(function, server, children)
end method do-duim-object-children;

define method do-duim-object-children
    (function :: <function>, server :: <server>, sheet :: <duim-sheet>)
 => ()
  let children = user-object-slot-value(server, sheet, $sheet-children-id);
  children & do-collection-elements(function, server, children)
end method do-duim-object-children;

define method do-duim-object-children
    (function :: <function>, server :: <server>, frame :: <duim-frame>)
 => ()
  local method maybe-call
	    (object :: false-or(<environment-object>)) => ()
	  if (instance?(object, <duim-object>))
	    function(object)
	  end
	end method maybe-call;
  maybe-call(user-object-slot-value(server, frame, $frame-menu-bar-id));
  maybe-call(user-object-slot-value(server, frame, $frame-tool-bar-id));
  maybe-call(user-object-slot-value(server, frame, $frame-layout-id));
  maybe-call(user-object-slot-value(server, frame, $frame-status-bar-id));
end method do-duim-object-children;

define method get-environment-object-primitive-name
    (project :: <project-object>, frame :: <duim-object>)
 => (name :: false-or(<string>))
  #f
end method get-environment-object-primitive-name;

define method get-environment-object-primitive-name
    (project :: <project-object>, frame :: <duim-frame>)
 => (name :: false-or(<string>))
  let object = user-object-slot-value(project, frame, $frame-title-id);
  if (instance?(object, <string-object>))
    environment-object-primitive-name(project, object)
  end
end method get-environment-object-primitive-name;

define method get-environment-object-primitive-name
    (project :: <project-object>, gadget :: <duim-gadget>)
 => (name :: false-or(<string>))
  let object = user-object-slot-value(project, gadget, $gadget-label-id);
  if (instance?(object, <string-object>))
    environment-object-primitive-name(project, object)
  end
end method get-environment-object-primitive-name;


/// Some convenience functions built on these protocols

define function duim-object-children
    (server :: <server>, object :: <duim-object>)
 => (children :: <sequence>)
  collect-environment-objects(do-duim-object-children, server, object)
end function duim-object-children;


/// Printing support

define method environment-object-type-name
    (object :: <duim-sheet>) => (name :: <string>)
  "Sheet"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <duim-frame>) => (name :: <string>)
  "Frame"
end method environment-object-type-name;

define method environment-object-type-name
    (object :: <duim-gadget>) => (name :: <string>)
  "Gadget"
end method environment-object-type-name;
