Module:       duim-gui-test-suite
Author:       Andy Armstrong
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Dynamic layouts

define frame <dynamic-layout-frame> (<simple-frame>)
  pane add-button (frame)
    make(<button>,
         label: "Add sheet",
         activate-callback: dynamic-layout-add-sheet);
  pane replace-button (frame)
    make(<button>,
	 label: "Replace sheet",
	 activate-callback: dynamic-layout-replace-sheet);
  pane remove-button (frame)
    make(<button>,
	 label: "Remove sheet",
	 activate-callback: dynamic-layout-remove-sheet);
  pane dynamic-layout (frame)
    vertically (spacing: 2)
      make(<text-field>)
    end;
  pane main-layout (frame)
    vertically (spacing: 5, x-alignment: #"centre")
      horizontally (spacing: 2)
        frame.add-button;
        frame.replace-button;
        frame.remove-button;
      end;
      frame.dynamic-layout
    end;
  layout (frame) frame.main-layout;
end frame <dynamic-layout-frame>;

define method dynamic-layout-add-sheet
    (sheet :: <sheet>) => ()
  let frame = sheet-frame(sheet);
  let layout = frame.dynamic-layout;
  let new-child = make(<text-field>, parent: layout);
  relayout-parent(layout);
  sheet-mapped?(new-child) := #t
end method dynamic-layout-add-sheet;

define function find-child-of-class
    (sheet :: <sheet>, class :: <class>)
 => (child :: false-or(<sheet>))
  block (return)
    for (child in sheet-children(sheet))
      if (instance?(child, class))
        return(child)
      end
    end
  end
end function find-child-of-class;

define method dynamic-layout-replace-sheet 
    (sheet :: <sheet>) => ()
  let frame = sheet-frame(sheet);
  let layout = frame.dynamic-layout;
  let child = find-child-of-class(layout, <text-field>);
  if (child)
    let new-child = make(<scroll-bar>);
    replace-child(layout, child, new-child);
    relayout-parent(layout);
    sheet-mapped?(new-child) := #t;
  end
end method dynamic-layout-replace-sheet;

define method dynamic-layout-remove-sheet
    (sheet :: <sheet>) => ()
  let frame = sheet-frame(sheet);
  let layout = frame.dynamic-layout;
  let children = sheet-children(layout);
  unless (empty?(children))
    remove-child(layout, children[0]);
    relayout-parent(layout)
  end
end method dynamic-layout-remove-sheet;

install-test(<dynamic-layout-frame>, "Dynamic layouts");
