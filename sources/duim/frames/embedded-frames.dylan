Module:       duim-frames-internals
Synopsis:     DUIM frames
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Embedded frames

define open abstract primary class <basic-embedded-frame> (<basic-frame>)
  sealed slot %layout :: false-or(<sheet>) = #f,
    init-keyword: layout:;
end class <basic-embedded-frame>;


define method frame-menu-bar
    (frame :: <basic-embedded-frame>) => (menu-bar :: singleton(#f))
  #f
end method frame-menu-bar;

define method frame-tool-bar
    (frame :: <basic-embedded-frame>) => (menu-bar :: singleton(#f))
  #f
end method frame-tool-bar;

define method frame-status-bar
    (frame :: <basic-embedded-frame>) => (menu-bar :: singleton(#f))
  #f
end method frame-status-bar;


define open abstract class <embedded-top-level-sheet>
    (<top-level-sheet>)
end class <embedded-top-level-sheet>;

define method frame-top-level-sheet-class
    (frame :: <basic-embedded-frame>, #key) => (class :: <class>)
  <embedded-top-level-sheet>
end method frame-top-level-sheet-class;


// Embedded frames don't have any decorations
define method frame-wrapper
    (framem :: <frame-manager>, frame :: <basic-embedded-frame>, sheet :: false-or(<sheet>))
 => (wrapper :: false-or(<sheet>))
  sheet
end method frame-wrapper;
