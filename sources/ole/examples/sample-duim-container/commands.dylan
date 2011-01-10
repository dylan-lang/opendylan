Module:    sample-duim-container
Synopsis:  Example of a simple OLE Container using DUIM
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// doesn't yet handle enabling and disabling "Insert Object" command ???

/// "File" command table

define command-table *file-command-table* (*global-command-table*)
  menu-item "New"     = new-document,
    documentation: "Erase the current document";
  menu-item "Open..."    = open-file,
    documentation: "Open an existing file";
  separator;
  menu-item "Exit"       = exit-frame,
    documentation: "Exit the application";
end command-table *file-command-table*;


/// "Edit" command table

define command-table *edit-command-table* (*global-command-table*)
  menu-item "Insert Object ..." = insert-object,
    documentation: "Insert an OLE compound document object";
end command-table *edit-command-table*;


/// "Help" command table

define command-table *help-command-table* (*global-command-table*)
  menu-item "About..."  = <help-on-version>,
    documentation: "Show the About box for this application";
end command-table *help-command-table*;


/// Application command table

define command-table *container-command-table* (*global-command-table*)
  menu-item "File" = *file-command-table*;
  menu-item "Edit" = *edit-command-table*;
  menu-item "Help" = *help-command-table*;
end command-table;



define method new-document
    (frame :: <sample-container-frame>) => ()
  do(destroy-sheet, sheet-children(frame.main-pane));
end method new-document;

define method open-file
    (frame :: <sample-container-frame>) => ()
  let file = choose-file(direction: #"input", owner: frame);
  if ( file )
    let main-sheet = frame.main-pane; // pane to paste into
    let embedded-sheet = insert-from-file(main-sheet, file);
  end if;
end method open-file;


define method insert-object
    (frame :: <sample-container-frame>) => ()
  let main-sheet = frame.main-pane; // pane to paste into
  let embedded-sheet = insert-from-dialog(main-sheet);
end method insert-object;


/// Help commands

define method do-execute-command
    (frame :: <sample-container-frame>, command :: <help-on-version>) => ()
  // put up "about" box
  notify-user(application-full-name(), owner: frame)
end method do-execute-command;
