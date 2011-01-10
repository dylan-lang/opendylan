Module:    environment-framework
Synopsis:  Environment Framework
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Undo protocols

define open abstract class <frame-undo-mixin> (<frame-selection-mixin>)
end class <frame-undo-mixin>;

define method initialize
    (frame :: <frame-undo-mixin>, #key) => ()
  next-method();
  command-enabled?(<frame-undo-command>, frame) := #f;
  command-enabled?(<frame-redo-command>, frame) := #f;
end method initialize;


/// Undo commands

define class <frame-undo-command> (<frame-selection-command>)
end class <frame-undo-command>;

define class <frame-redo-command> (<frame-selection-command>)
end class <frame-redo-command>;

define method command-available-for-focus?
    (sheet :: <sheet>, command == <frame-undo-command>)
 => (available? :: <boolean>)
  #f
end method command-available-for-focus?;

define method command-available-for-focus?
    (sheet :: <sheet>, command == <frame-redo-command>)
 => (available? :: <boolean>)
  #f
end method command-available-for-focus?;


/// Undo command table

define variable $undo-bitmap :: <label-type> = "Undo";
define variable $redo-bitmap :: <label-type> = "Redo";

define constant $undo-doc = "Undoes the last action.";
define constant $redo-doc = "Undoes the last Undo operation.";

define command-table *undo-command-table* (*global-command-table*)
  menu-item "Undo" = <frame-undo-command>,
    documentation: $undo-doc;
  menu-item "Redo" = <frame-redo-command>,
    documentation: $redo-doc;
end command-table *undo-command-table*;

define method make-undo-tool-bar-buttons
    (frame :: <frame-undo-mixin>)
 => (buttons :: <sequence>)
  vector(make(<button>,
              label: $undo-bitmap,
	      documentation: "Undo",
              command: <frame-undo-command>),
	 make(<button>,
              label: $redo-bitmap,
	      documentation: "Redo",
              command: <frame-redo-command>))
end method make-undo-tool-bar-buttons;
