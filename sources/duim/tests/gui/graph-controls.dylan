Module:       duim-gui-test-suite
Author:       Andy Armstrong
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Number graph controls

define frame <number-graph-frame> (<simple-frame>)
  pane roots-pane (frame)
    make(<check-box>, 
         items: #[1, 2, 3, 4],
         value: #[1],
	 value-changed-callback: method (gadget)
                                   tree-control-roots(frame.tree-pane)
                                     := sort(gadget-value(gadget));
                                   tree-control-roots(frame.tree-graph-pane)
                                     := sort(gadget-value(gadget));
                                   tree-control-roots(frame.dag-graph-pane)
                                     := sort(gadget-value(gadget));
                                 end);
  pane tree-pane (frame)
    make(<tree-control>,
         roots: #[1],
         depth: 2,
	 children-generator: method (x) vector(x * 2, x * 2 + 1) end,
	 min-height: 150, min-width: 250);
  pane tree-graph-pane (frame)
    make(<graph-control>,
	 graph-type: #"tree",
         roots: #[1],
         depth: 2,
         children-generator: method (x) vector(x * 2, x * 2 + 1) end,
	 min-height: 150, min-width: 250);
  pane dag-graph-pane (frame)
    make(<graph-control>,
	 graph-type: #"dag",
         roots: #[1],
         depth: 2,
         children-generator: method (x) vector(x * 2, x * 2 + 1) end,
	 min-height: 150, min-width: 250);
  pane main-layout (frame)
    vertically (spacing: 2)
      frame.roots-pane;
      make(<separator>);
      frame.tree-pane;
      frame.tree-graph-pane;
      frame.dag-graph-pane;
    end;
  layout (frame) frame.main-layout;
end frame <number-graph-frame>;


/// Class graph controls

define frame <class-graph-frame> (<simple-frame>)
  pane tree-pane (frame)
    make(<tree-control>,
         roots: vector(<graph-control>),
         depth: 2,
         activate-callback: tab-control-activate-callback,
	 children-generator: direct-superclasses,
	 label-key: method (c) as(<string>, debug-name(c)) end,
	 min-height: 150, min-width: 250);
  pane tree-graph-pane (frame)
    make(<graph-control>,
	 graph-type: #"tree",
         roots: vector(<graph-control>),
         depth: 2,
         activate-callback: tab-control-activate-callback,
	 children-generator: direct-superclasses,
	 label-key: method (c) as(<string>, debug-name(c)) end,
	 min-height: 150, min-width: 250);
  pane dag-graph-pane (frame)
    make(<graph-control>,
	 graph-type: #"dag",
         roots: vector(<graph-control>),
         depth: 2,
         activate-callback: tab-control-activate-callback,
	 children-generator: direct-superclasses,
	 label-key: method (c) as(<string>, debug-name(c)) end,
	 min-height: 150, min-width: 250);
  pane main-layout (frame)
    vertically (spacing: 2)
      frame.tree-pane;
      frame.tree-graph-pane;
      frame.dag-graph-pane;
    end;
  layout (frame) frame.main-layout;
end frame <class-graph-frame>;


/// Install the test

define variable $graph-control-tests
  = vector(vector("Number graphs", <number-graph-frame>),
	   vector("Class graphs",  <class-graph-frame>));

define frame <graph-control-frame> (<simple-frame>)
  pane examples (frame)
    make(<list-control>,
         scroll-bars: #"none",
	 documentation: "Double-click on a test name to run it",
	 items: $graph-control-tests,
	 lines: size($graph-control-tests),
	 label-key: first,
	 activate-callback: method (gadget :: <gadget>)
                              let frame = sheet-frame(gadget);
                              let value = gadget-value(gadget);
                              let title = first(value);
                              let class = second(value);
                              let test-frame = make(class, title: title, owner: frame);
                              start-frame(test-frame)
			    end);
  pane main-layout (frame)
    frame.examples;
  layout (frame) frame.main-layout;
end frame <graph-control-frame>;

install-test(<graph-control-frame>, "Graph controls");
