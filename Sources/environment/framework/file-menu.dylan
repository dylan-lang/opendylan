Module:    environment-framework
Synopsis:  Environment Framework
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// File handling generic functions

define open generic frame-new-file
    (frame :: <frame>) => ();
define open generic frame-open-file
    (frame :: <frame>, #rest keys, #key filename, deuce-frame, #all-keys) => ();
define open generic frame-revert-file
    (frame :: <frame>, #key filename) => ();
define open generic frame-close-file
    (frame :: <frame>, #key filename) => ();
define open generic frame-save-file
    (frame :: <frame>, #key filename) => ();
define open generic frame-save-file-as
    (frame :: <frame>, #key filename) => ();
define open generic frame-save-all
    (frame :: <frame>) => ();
define open generic frame-exit-application
    (frame :: <frame>) => (exited? :: <boolean>);


/// Open command table

define variable $new-bitmap  :: <label-type> = "New";
define variable $open-bitmap :: <label-type> = "Open";

define command-table *file-open-command-table* (*global-command-table*)
  menu-item "New..."  = frame-new-file,
    accelerator:   make-keyboard-gesture(#"n", #"control"),
    documentation: "Creates a new document.";
  menu-item "Open..." = frame-open-file,
    accelerator:   make-keyboard-gesture(#"o", #"control"),
    documentation: "Opens an existing document.";
  menu-item "Revert"  = frame-revert-file,
    documentation: "Discards unsaved changes to the document.";
  menu-item "Close"   = frame-close-file,
    accelerator:   make-keyboard-gesture(#"w", #"control"),
    documentation: "Closes the document.";
end command-table *file-open-command-table*;


/// Save command table

define variable $save-bitmap     :: <label-type> = "Save";
define variable $save-all-bitmap :: <label-type> = "Save All";

define command-table *file-save-command-table* (*global-command-table*)
  menu-item "Save"       = frame-save-file,
    documentation: "Saves the document.";
  menu-item "Save As..." = frame-save-file-as,
    documentation: "Saves the document with a new name.";
  menu-item "Save All"   = frame-save-all,
    accelerator:   make-keyboard-gesture(#"s", #"shift", #"control"),
    documentation: "Saves all open documents.";
end command-table *file-save-command-table*;


/// Exit command table

define command-table *exit-command-table* (*global-command-table*)
  menu-item "Exit" = frame-exit-application,
    documentation: "Quits the application; prompts to save documents.";
end command-table *exit-command-table*;


/// File menus

define command-table *file-command-table* (*global-command-table*)
  include *print-command-table*;
  include *exit-command-table*;
end command-table *file-command-table*;

define command-table *file-input-command-table* (*global-command-table*)
  include *file-open-command-table*;
  include *print-command-table*;
  include *exit-command-table*;
end command-table *file-input-command-table*;

define command-table *file-io-command-table* (*global-command-table*)
  include *file-open-command-table*;
  include *file-save-command-table*;
  include *print-command-table*;
  include *exit-command-table*;
end command-table *file-io-command-table*;
