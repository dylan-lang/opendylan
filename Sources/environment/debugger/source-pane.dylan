Module:    environment-debugger
Author:    Bill Chiles, Jason Trenouth, Hugh Greene, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// $DEBUGGER-SOURCE-DOC

define constant $debugger-source-doc :: <string> = "Source";

/// <DEBUGGER-SOURCE-PANE> (internal)

define sealed pane <debugger-source-pane> ()
  sealed slot %source-gadget :: false-or(<gadget>) = #f;
  pane %source-displayer (pane)
    begin
      // ---*** Perhaps we should use an <application...>, not a <project...>?
      // The source-pane will have no sheet-frame just now, so use the
      // dynamically-bound *debugger* instead.
      let (viewer, gadget)
	= make-code-viewer(project: *debugger*.frame-project);
      gadget.gadget-documentation := $debugger-pane-tooltips? & $debugger-source-doc;
      pane.%source-gadget := gadget;
      viewer
    end;
  layout (pane)
      pane.%source-displayer
end pane;


/// UPDATE-DEBUGGER-SOURCE-PANE (internal)

define function update-debugger-source-pane 
    (debugger :: <debugger>, #key refresh? :: <boolean> = #f)
  => ()
  let source-pane = debugger.debugger-source-pane;
  let window = source-pane.%source-gadget;
  redisplay-debugger-editor-window(window, refresh?: refresh?)
end function;
