Module:       windows-viewer
Author:       Andy Armstrong
Synopsis:     Windows viewer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function make-keyboard-gesture
    (key :: <symbol>, #rest modifiers :: <symbol>)
  make(<keyboard-gesture>, keysym: key, modifiers: modifiers)
end function make-keyboard-gesture;

define command-table *file-command-table* (*global-command-table*)
  menu-item "Exit" = exit-frame;
end command-table *file-command-table*;

define command-table *edit-command-table* (*global-command-table*)
end command-table *edit-command-table*;

define command-table *view-command-table* (*global-command-table*)
  menu-item "Refresh" = refresh-windows-viewer,
    accelerator:   make-keyboard-gesture(#"f5"),
    documentation: "Updates the display to reflect any changes";
end command-table *view-command-table*;

define command-table *help-command-table* (*global-command-table*)
  menu-item "About Windows Viewer..." = about-windows-viewer,
    documentation: "Displays program information, version and copyright.";
end command-table *help-command-table*;

define command-table *windows-viewer-command-table* (*global-command-table*)
  menu-item "File" = *file-command-table*;
  menu-item "Edit" = *edit-command-table*;
  menu-item "View" = *view-command-table*;
  menu-item "Help" = *help-command-table*;
end command-table *windows-viewer-command-table*;
