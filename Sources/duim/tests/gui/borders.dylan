Module:       duim-gui-test-suite
Author:       Andy Armstrong
Synopsis:     An interactive test-suite for DUIM objects
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Border tests

define frame <border-test-frame> (<simple-frame>)
  pane flat-border (frame)
    with-border (type: #"flat")   make-ellipse-with-space() end;
  pane raised-border (frame)
    with-border (type: #"raised") make-ellipse-with-space() end;
  pane sunken-border (frame)
    with-border (type: #"sunken") make-ellipse-with-space() end;
  pane ridge-border (frame)
    with-border (type: #"ridge")  make-ellipse-with-space() end;
  pane groove-border (frame)
    with-border (type: #"groove") make-ellipse-with-space() end;
  pane group-box-border (frame) 
    grouping ("Test") make(<ellipse-pane>) end;
  layout (frame)
    make(<table-layout>,
         columns: 2,
         height: 500,
         x-alignment: #[#"right", #"left"], y-alignment: #"center",
	 x-spacing: 4, y-spacing: 4,
         children: vector(make(<label>, label: "Flat:"),
                          frame.flat-border,
                          make(<label>, label: "Raised:"),
                          frame.raised-border,
                          make(<label>, label: "Sunken:"),
                          frame.sunken-border,
                          make(<label>, label: "Ridged:"),
                          frame.ridge-border,
                          make(<label>, label: "Groove:"),
                          frame.groove-border,
                          make(<label>, label: "Group:"),
                          frame.group-box-border))
end frame <border-test-frame>;

define function make-ellipse-with-space
    () => (sheet :: <sheet>)
  with-spacing (spacing: 4)
    make(<ellipse-pane>)
  end
end function make-ellipse-with-space;

install-test(<border-test-frame>, "Borders");
