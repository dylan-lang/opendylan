Module:       duim-frames-internals
Synopsis:     DUIM frames
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Standard commands and command tables

/// "File"

// The document protocol, such as it is
define protocol <<document>> ()
  function new-document
    (frame :: <abstract-frame>) => (#rest values);
  function open-document
    (frame :: <abstract-frame>, #key document) => (#rest values);
  function close-document
    (frame :: <abstract-frame>, #key document) => (#rest values);
  function save-document
    (frame :: <abstract-frame>, #key document) => (#rest values);
  function save-document-as
    (frame :: <abstract-frame>, #key document) => (#rest values);
  function save-all-documents
    (frame :: <abstract-frame>) => (#rest values);
  function note-document-changed
    (frame :: <abstract-frame>, document :: <object>) => ();
end protocol <<document>>;

define command-table *standard-file-command-table* (*global-command-table*)
  menu-item "New..."     = new-document;
  menu-item "Open..."    = open-document;
  menu-item "Close"      = close-document;
  separator;
  menu-item "Save"       = save-document;
  menu-item "Save As..." = save-document-as;
  menu-item "Save All"   = save-all-documents;
  separator;
  menu-item "Exit"       = exit-frame;
end command-table *standard-file-command-table*;


/// "Edit"

define open generic command-undo
    (frame :: <abstract-frame>) => (#rest values);

define open generic command-redo
    (frame :: <abstract-frame>) => (#rest values);

define open generic clipboard-cut
    (frame :: <abstract-frame>) => (#rest values);

define open generic clipboard-copy
    (frame :: <abstract-frame>) => (#rest values);

define open generic clipboard-paste
    (frame :: <abstract-frame>) => (#rest values);

define open generic clipboard-clear
    (frame :: <abstract-frame>) => (#rest values);

define command-table *standard-edit-command-table* (*global-command-table*)
  menu-item "Undo"  = command-undo;
  menu-item "Redo"  = command-redo;
  separator;
  menu-item "Cut"   = clipboard-cut;
  menu-item "Copy"  = clipboard-copy;
  menu-item "Paste" = clipboard-paste;
  menu-item "Clear" = clipboard-clear;
end command-table *standard-edit-command-table*;


/// "View"

define command-table *standard-view-command-table* (*global-command-table*)
end command-table *standard-view-command-table*;


/// "Windows"

define command-table *standard-windows-command-table* (*global-command-table*)
end command-table *standard-windows-command-table*;


/// "Help"

define command-table *standard-help-command-table* (*global-command-table*)
  menu-item "Topics..." = <help-on-topics>;
  separator;
  menu-item "About..."  = <help-on-version>;
end command-table *standard-help-command-table*;
