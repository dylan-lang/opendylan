Module:    mini-duim
Synopsis:  Mini-DUIM frame managers
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Frame managers

define open abstract class <frame-manager> (<object>)
end class <frame-manager>;

define open abstract class <basic-frame-manager> (<frame-manager>)
  slot %frames = make(<stretchy-vector>);
end class <basic-frame-manager>;

define open generic make-frame-manager
    (port :: <port>, #key, #all-keys) => (framem :: <frame-manager>);

define method find-frame-manager
    (#key port: _port, class)
 => (framem :: <frame-manager>)
  unless (_port)
    _port := find-port()
  end;
  block (return)
    for (framem in port-frame-managers(_port))
      // Find a framem of an exactly-matching class
      if (~class | object-class(framem) == class)
	return(framem)
      end
    end;
    let framem = if (class)
		   make-frame-manager(_port, class: class)
		 else
		   make-frame-manager(_port)
		 end;
    add!(port-frame-managers(_port), framem);
    framem
  end
end method find-frame-manager;


/// Pane creation

define macro with-frame-manager
  { with-frame-manager (?framem:expression) ?:body end }
    => { begin
	   ?body
	 end }
end macro with-frame-manager;

// Here for compatibility and self-documentation...
define function make-pane 
    (pane-class :: <class>, #rest pane-options, #key, #all-keys)
 => (pane)	// type-union(<sheet>, <gadget>)
  apply(make, pane-class, pane-options)
end function make-pane;

define method make
    (pane-class :: subclass(<sheet>),
     #rest pane-options, #key frame-manager: framem) => (pane)
  let framem = framem | find-frame-manager();
  let (concrete-class, concrete-options)
    = apply(class-for-make-pane, framem, pane-class, pane-options);
  // If there's a mapping from the abstract pane class to a concrete pane
  // class, then use it.  Otherwise just try to create a class named by the
  // abstract pane class.
  if (concrete-class == pane-class)
    apply(next-method, pane-class,
	  frame-manager: framem, pane-options)
  else
    apply(make, concrete-class,
	  frame-manager: framem,
	  concrete-options | pane-options)
  end
end method make;

define open generic class-for-make-pane
    (framem :: <frame-manager>, pane-class,
     #rest pane-options, #key, #all-keys)
 => (class :: <class>, options :: false-or(<sequence>));

define method class-for-make-pane
    (framem :: <frame-manager>, pane-class :: <class>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(pane-class, #f)
end method class-for-make-pane;


/// Some high level "chooser" functions

define constant <notification-style>
  = one-of(#"information", #"question",
	   #"error", #"serious-error", #"warning");

// Returns #t on normal exit, or #f on "cancel"
// STYLE is one of #"error", #"warning", #"information", #"question", etc.
define method notify-user
    (message-string :: <string>, #rest options,
     #key frame, owner, title, documentation, exit-boxes, name,
          style :: <notification-style> = #"information",
          foreground, background, text-style)
 => (value :: <boolean>)
  ignore(title, documentation, exit-boxes, name, 
	 foreground, background, text-style);
  let (framem, owner) = get-frame-manager-and-owner(frame, owner);
  apply(do-notify-user, framem, owner, message-string, style, options)
end method notify-user;

define open generic do-notify-user
    (framem :: <frame-manager>, 
     owner :: <sheet>,
     message-string :: <string>,
     style :: <notification-style>,
     #key title, documentation, name,
     #all-keys)
 => (value :: <boolean>);


// ITEMS can be a sequence or a <menu>
define method choose-from-menu
    (items, #rest options,
     #key frame, owner, title, default-item,
          name-key, value-key,
          foreground, background, text-style) => (value, success?)
  ignore(title, default-item, name-key, value-key,
	 foreground, background, text-style);
  let (framem, owner) = get-frame-manager-and-owner(frame, owner);
  apply(do-choose-from-menu, framem, owner, items, options)
end method choose-from-menu;

define open generic do-choose-from-menu
    (framem :: <frame-manager>, owner :: <sheet>, items,
     #key title, default-item, name-key, value-key,
     #all-keys)
 => (value, success? :: <boolean>);


// ITEMS can be a sequence or a <menu>
define method choose-from-dialog
    (items, #rest options,
     #key frame, owner, title, default-item,
          name-key, value-key,
          gadget-class, gadget-options,
          foreground, background, text-style) => (value, success?)
  ignore(title, default-item, name-key, value-key,
	 gadget-class, gadget-options,
	 foreground, background, text-style);
  let (framem, owner) = get-frame-manager-and-owner(frame, owner);
  apply(do-choose-from-dialog, framem, owner, items, options)
end method choose-from-dialog;

define open generic do-choose-from-dialog
    (framem :: <frame-manager>, owner :: <sheet>, items,
     #key title, default-item, name-key, value-key,
     #all-keys)
 => (value, success? :: <boolean>);


// Returns the pathname on normal exit, or #f on "cancel"
define method choose-file
    (#rest options,
     #key frame, owner, title, direction, documentation, exit-boxes,
          name, default)
 => (locator)
  ignore(title, documentation, exit-boxes, name, default);
  let (framem, owner) = get-frame-manager-and-owner(frame, owner);
  apply(do-choose-file, framem, owner, direction, options)
end method choose-file;

define open generic do-choose-file
    (framem :: <frame-manager>, owner :: <sheet>,
     direction :: one-of(#"input", #"output"),
     #key title, documentation, exit-boxes, name, default,
     #all-keys)
 => (locator);


// Returns the pathname on normal exit, or #f on "cancel"
define method choose-directory
    (#rest options,
     #key frame, owner, title, documentation, exit-boxes, name, default)
 => (locator)
  ignore(title, documentation, exit-boxes, name, default);
  let (framem, owner) = get-frame-manager-and-owner(frame, owner);
  apply(do-choose-directory, framem, owner, options)
end method choose-directory;

define open generic do-choose-directory
    (framem :: <frame-manager>, owner :: <sheet>,
     #key title, documentation, exit-boxes, name, default,
     #all-keys)
 => (locator);


// Returns the color on normal exit, or #f on "cancel"
define method choose-color
    (#rest options,
     #key frame, owner, title, documentation, exit-boxes,
          name, default)
 => (color :: false-or(<color>))
  ignore(title, documentation, exit-boxes, name, default);
  let (framem, owner) = get-frame-manager-and-owner(frame, owner);
  apply(do-choose-color, framem, owner, options)
end method choose-color;

define open generic do-choose-color
    (framem :: <frame-manager>, owner :: <sheet>,
     #key title, documentation, exit-boxes, name, default,
     #all-keys)
 => (color :: false-or(<color>));


define method get-frame-manager-and-owner
    (frame, owner) 
 => (framem :: <frame-manager>, owner :: <sheet>)
  let frame = frame
              | (instance?(owner, <sheet>) & sheet-frame(owner))
              | (instance?(owner, <frame>) & owner);
  let owner = owner | frame;
  let framem = frame & frame-manager(frame);
  values(framem, if (instance?(owner, <frame>)) top-level-sheet(owner) else owner end)
end method get-frame-manager-and-owner;
