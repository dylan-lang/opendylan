Module:    gtk-environment
Synopsis:  GTK Environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Icon and bitmap initialization

define function initialize-bitmaps ()
  #f
end function initialize-bitmaps;


/// Deuce initialization

define function initialize-deuce ()
  local method make-deuce-color (color) => (deuce-color)
	  let (r, g, b) = color-rgb(color);
	  deuce/make-color(floor(r * 255.0), floor(g * 255.0), floor(b * 255.0))
	end method;
  /*---*** andrewa: where should this come from?
  $region-marking-color        := make-deuce-color($default-face-color);
  $dylan-definition-line-color := make-deuce-color($default-shadow-color)
  */
end function initialize-deuce;


/// Editor initialization

define function initialize-editors ()
  //--- It would be nice to register Emacs and some DDE-based editor, too
  register-editor-class(<deuce-editor>);
  // Initial default editor for the Win32 environment is Deuce
  current-editor()
    := find-editor-of-class(<deuce-editor>)
end function initialize-editors;


/// Source control initialization

define function initialize-source-control ()
  #f
end function initialize-source-control;


/// Locator opening

define sideways method frame-open-object
    (frame  :: <frame>, locator :: <url>) => ()
  notify-user("Unable to open URLs", owner: frame)
end method frame-open-object;

define sideways method frame-open-object
    (frame  :: <frame>, locator :: <file-locator>) => ()
  notify-user("Unable to open non-text files", owner: frame)
end method frame-open-object;

define sideways method frame-hardcopy-object
    (frame  :: <frame>, locator :: <file-locator>) => ()
  notify-user("Printing not yet implemented!", owner: frame)
end method frame-hardcopy-object;

/*---*** andrewa: do we need a method here?
define sideways method frame-cascade-offset
    (framem :: <gtk-frame-manager>, frame :: <frame-cascading-window-mixin>)
 => (x :: <integer>, y :: <integer>)
  values(10, 10)
end method frame-cascade-offset;
*/

define sideways method register-opened-file
    (filename :: <file-locator>) => ()
  //--- Do nothing
  #f
end method register-opened-file;

