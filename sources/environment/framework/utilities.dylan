Module:    environment-framework
Synopsis:  Environment Framework
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Make a keyboard gesture

define function make-keyboard-gesture
    (keysym :: <symbol>, #rest modifiers)
 => (gesture :: <keyboard-gesture>)
  make(<keyboard-gesture>, keysym: keysym, modifiers: modifiers)
end function make-keyboard-gesture;


//---*** cpage: 1998.08.12 These are (hopefully) temporary hacks to
//              allow me to experiment with using Windows APIs better
//              without having to modify/build DUIM. Sideways methods
//              are defined on these in win32-environment

define constant <relative-order> = one-of(#"bottom", #"top");
define constant <frame-order>    = type-union(<relative-order>, <frame>);
define open generic reorder-frame
    (frame :: <frame>, where :: <frame-order>) => (frame :: <frame>);
define open generic order-frames
    (frames :: <sequence>) => ();
define open generic restore-frame
    (frame :: <frame>) => (frame :: <frame>);


/// Command handling

define open generic command-available?
    (frame :: <frame>, class :: subclass(<command>))
 => (available? :: <boolean>);

define method command-available?
    (frame :: <frame>, class :: subclass(<command>))
 => (available? :: <boolean>)
  #t
end method command-available?;

define method update-command-availability
    (frame :: <frame>, class :: subclass(<command>)) => ()
  //--- Surely we can do this more efficiently?
  do-frame-commands
    (method (gadget :: <gadget>, command :: <class>)
       ignore(gadget);
       if (subtype?(command, class))
	 command-enabled?(command, frame)
	   := command-available?(frame, command)
       end
     end,
     frame,
     test: rcurry(instance?, <class>),
     menu-bar?: #t, tool-bar?: #t, owned-menus?: #f)
end method update-command-availability;
