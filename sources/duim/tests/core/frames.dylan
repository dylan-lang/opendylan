Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Frame class tests

define sideways method make-test-instance
    (class :: subclass(<frame>)) => (frame :: <frame>)
  //--- Be careful, since <frame> isn't instantiable!
  if (class == <frame>)
    next-method()
  else
    make(class, frame-manager: find-test-frame-manager())
  end
end method make-test-instance;

define duim-sheets class-test <frame> ()
  //---*** Fill this in...
end class-test <frame>;

define duim-sheets class-test <frame-manager> ()
  //---*** Fill this in...
end class-test <frame-manager>;

define duim-frames class-test <simple-frame> ()
  //---*** Fill this in...
end class-test <simple-frame>;


/// Frame layout tests

define method expected-named-size
    (frame :: <frame>, name, #rest args, #key width, height)
 => (size :: <integer>)
  ignore(width, height);
  apply(expected-named-size, top-level-sheet(frame), name, args)
end method expected-named-size;

define method sheet-alternative-size 
    (sheet) => (width :: false-or(<integer>), height :: false-or(<integer>))
  let (old-width, old-height) = box-size(sheet);
  let (width, height) = expected-constrained-size(sheet,
                                                  old-width + 100,
                                                  old-height + 100);
  unless (width = old-width & height = old-height)
    values(width, height)
  end
end method sheet-alternative-size;

define method check-frame-resize (frame, name) => ()
  let top-sheet = top-level-sheet(frame);
  let (width, height) = sheet-alternative-size(top-sheet);
  if (width & height)
    let new-region = make-bounding-box(0, 0, width, height);
    top-level-sheet-region(top-sheet) := new-region;
    distribute-event(port(frame),
                     make(<window-configuration-event>,
                          sheet: top-sheet,
                          region: new-region));
    check-layout-pane-layout(top-sheet, concatenate("resized ", name),
                             allocate-space?: #f,
                             width: width,
                             height: height)
  end;
end method check-frame-resize;

define method check-frame-layout 
    (frame, layout, name, #key width, height) => ()
  let top-sheet = top-level-sheet(frame);
  check-equal(concatenate(name, " layout"), frame-layout(frame), layout);
  if (layout)
    check-true(concatenate(name, " has top-level-sheet"), 
               instance?(top-sheet, <top-level-sheet>));
    check-layout-pane-layout(top-sheet, name,
                             width: width | expected-width(top-sheet), 
                             height: height | expected-height(top-sheet),
                             allocate-space?: #f);
    check-frame-resize(frame, name);
  else
    check-false(concatenate(name, " has no top-level-sheet"), top-sheet);
  end;
  frame
end method check-frame-layout;

define method test-frame-layout 
    (name, layout, #key width, height) => (frame :: <frame>)
  let frame
    = make-test-frame(<test-frame>,
		      layout: layout, width: width, height: height);
  let name = concatenate(name, " ", gadget-class-name(<simple-frame>));
  check-frame-layout(frame, layout, name, 
                     width: width, height: height);
  frame
end method test-frame-layout;

define test frame-layouts-test ()
  test-frame-layout("empty", #f);
  test-frame-layout("fixed layout", 
		    make-test-pane(<spacing>, child: make-test-pane(<button>)));
  test-frame-layout("non-fixed layout", 
		    make-test-pane(<spacing>, child: make-test-pane(<list-box>)));
  test-frame-layout("explicit width",
		    make-test-pane(<spacing>, child: make-test-pane(<list-box>)),
		    width: 500);
  test-frame-layout("explicit height",
		    make-test-pane(<spacing>, child: make-test-pane(<list-box>)),
		    height: 600);
  test-frame-layout("explicit size",
		    make-test-pane(<spacing>, child: make-test-pane(<list-box>)),
		    width: 400,
		    height: 500);
end test frame-layouts-test;


/// Frame wrapper tests

define test frame-wrappers-test ()
  let layout = make-test-pane(<border-pane>, child: make-test-pane(<button>));
  let tool-bar = make-test-pane(<tool-bar>);
  let menu-bar = make-test-pane(<menu-bar>);
  let status-bar = make-test-pane(<status-bar>);
  let frame
    = make-test-frame(<test-frame>,
		      layout: layout,
		      tool-bar: tool-bar,
		      menu-bar: menu-bar,
		      status-bar: status-bar);
  check-equal("Frame menu bar installed",
	      frame-menu-bar(frame), menu-bar);
  check-equal("Frame tool bar installed",
	      frame-tool-bar(frame), tool-bar);
  check-equal("Frame layout installed",
	      frame-layout(frame), layout);
  check-equal("Frame status bar installed",
	      frame-status-bar(frame), status-bar);
end test frame-wrappers-test;


/// Define the frames test suite

define suite duim-frames-suite ()
  test frame-layouts-test;
  test frame-wrappers-test;
end suite duim-frames-suite;
