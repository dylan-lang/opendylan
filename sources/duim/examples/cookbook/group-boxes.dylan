Module:       duim-examples
Author:       Andy Armstrong, Scott McKay
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Group box handling

define frame <group-box-frame> (<simple-frame>)
  pane first-layout (frame)
    vertically (spacing: 4)
      labelling ("Color Type:")
        make(<check-box>, items: #("Color", "Monochrome"))
      end;
      labelling ("Color:")
        make(<check-box>, items: #("Red", "Green", "Blue"))
      end
    end;
  pane second-layout (frame)
    labelling ("Text:")
      make(<text-field>)
    end;
  pane main-layout (frame)
    vertically (spacing: 5)
      grouping ("Colors:") frame.first-layout end;
      grouping ("Text:")   frame.second-layout end;
    end;
  layout (frame) frame.main-layout;
end frame <group-box-frame>;

install-example(<group-box-frame>, "Group Boxes");
