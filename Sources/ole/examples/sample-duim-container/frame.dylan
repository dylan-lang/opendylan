Module:    sample-duim-container
Synopsis:  Example of a simple OLE Container using DUIM
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Application frame

define class <container-pane> ( <container-sheet-mixin>, <drawing-pane> )
end class;

define frame <sample-container-frame> (<container-frame-mixin>, <simple-frame>)
  pane main-pane (frame)
    make(<container-pane>, enabled?: #f,
         width: 350, height: 250);
  layout (frame)
    vertically () frame.main-pane end;
  command-table (frame)
    *container-command-table*;
  /* tool-bar (frame)
       make-command-tool-bar(frame-manager(frame), frame); */
  status-bar (frame)
    make(<status-bar>);
  keyword title: = $application-name;
end frame <sample-container-frame>;


/// Start the program

define method start-program () => ()
  start-frame(make(<sample-container-frame>));
end method start-program;
