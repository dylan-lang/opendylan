Module:       duim-gui-test-suite
Author:       Andy Armstrong
Synopsis:     An interactive test-suite for DUIM objects
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Color tests

define frame <color-test-frame> (<simple-frame>)
  pane red-ellipse (frame)
    grouping ("Red Ellipse")
      make(<ellipse-pane>, foreground: $red)
    end;
  pane green-ellipse (frame)
    grouping ("Green On Blue Ellipse")
      make(<ellipse-pane>, foreground: $green, background: $blue)
    end;
  pane red-button (frame)
    grouping ("Red Button")
      make(<push-button>, label: "Hello", foreground: $red)
    end;
  layout (frame)
    vertically (spacing: 4)
      frame.red-ellipse;
      frame.green-ellipse;
      frame.red-button;
    end;
end frame <color-test-frame>;

install-test(<color-test-frame>, "Colors");
