Module:       vanilla-duim
Synopsis:     Vanilla back-end
Author:	   Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Vanilla frame manager

define sealed class <vanilla-frame-manager> (<basic-frame-manager>)
end class <vanilla-frame-manager>;

define method make-frame-manager
    (_port :: <vanilla-port>,
     #key palette, class = <vanilla-frame-manager>, #all-keys)
 => (framem :: <frame-manager>)
  make(class, port: _port, palette: palette)
end method make-frame-manager;


define method frame-wrapper
    (framem :: <vanilla-frame-manager>, 
     frame :: <simple-frame>,
     layout :: false-or(<sheet>))
 => (wrapper :: false-or(<sheet>))
  let menu-bar   = frame-menu-bar(frame);
  let tool-bar   = frame-tool-bar(frame);
  let status-bar = frame-status-bar(frame);
  //--- Build up a sheet hierarchy and return the containing sheet
end method frame-wrapper;


/// Glue to frames

define method note-frame-title-changed
    (framem :: <vanilla-frame-manager>, frame :: <frame>) => ()
  //--- Update the title in the window
end method note-frame-title-changed;

define method note-frame-icon-changed
    (framem :: <vanilla-frame-manager>, frame :: <frame>) => ()
  //--- Update the icon in the window
end method note-frame-icon-changed;
