Module:       interface-builder
Author:       Andy Armstrong
Synopsis:     DUIM interface builder
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function make-keyboard-gesture
    (key :: <symbol>, #rest modifiers :: <symbol>)
  make(<keyboard-gesture>, keysym: key, modifiers: modifiers)
end function make-keyboard-gesture;

define command-table *file-io-command-table* (*global-command-table*)
  menu-item "New" = frame-new-frame;
  // menu-item "Open..." = frame-open-file;
  menu-item "Save" = frame-save-file;
  menu-item "Save As..." = frame-save-as-file;
end command-table *file-io-command-table*;

define command-table *file-command-table* (*global-command-table*)
  include *file-io-command-table*;
  menu-item "Exit" = exit-frame;
end command-table *file-command-table*;

define command-table *edit-command-table* (*global-command-table*)
  menu-item "Add..." = frame-add-sheet;
  menu-item "Delete" = frame-remove-sheet,
    accelerator:   make-keyboard-gesture(#"delete"),
    documentation: "Remove the selected pane from the hierarchy.";
end command-table *edit-command-table*;

define command-table *view-command-table* (*global-command-table*)
  menu-item "Refresh" = frame-refresh-representation,
    accelerator:   make-keyboard-gesture(#"f5"),
    documentation: "Refresh";
end command-table *view-command-table*;

define command-table *frame-command-table* (*global-command-table*)
  menu-item "File" = *file-command-table*;
  menu-item "Edit" = *edit-command-table*;
  menu-item "View" = *view-command-table*;
end command-table *frame-command-table*;

define command-table *model-command-table* (*global-command-table*)
  menu-item "Add" = frame-add-sheet;
  menu-item "Remove" = frame-remove-sheet;
  menu-item "Properties" = frame-sheet-properties;
end command-table *model-command-table*;

