Module:       duim-gui-test-suite
Author:       Andy Armstrong
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Tab control tests

define frame <tab-control-frame> (<simple-frame>)
  slot first-tab-pages :: false-or(<sequence>) = #f;
  slot second-tab-pages :: false-or(<sequence>) = #f;
  pane which-pages (frame)
    make(<radio-box>,
	 items: #[#["First pages",  1],
		  #["Second pages", 2]],
         label-key: first,
         value-key: second,
	 value-changed-callback: method (gadget)
                                   let frame = sheet-frame(gadget);
                                   update-tab-control-pages(frame, gadget-value(gadget))
                                 end);
  pane button (frame)
    make(<button>, 
         activate-callback: tab-control-activate-callback,
         label: "Press Me");
  pane list-box (frame)
    make(<list-box>, 
         activate-callback: tab-control-activate-callback,
         items: range(from: 1, to: 10));
  pane tree-control (frame)
    make(<tree-control>,
         roots: #(1),
	 children-generator: method (x) vector(x * 2, 1 + (x * 2)) end,
         activate-callback: tab-control-activate-callback);
  pane table-control (frame)
    make(<table-control>,
         items: range(from: 1, to: 10),
         activate-callback: tab-control-activate-callback,
         headings: #("Identity", "Doubled", "Squared"),
         generators: vector(identity,
                            method (x) x + x end,
                            method (x) x * x end));
  pane ellipse-pane (frame)
    make(<ellipse-pane>);
  pane table-layout-pane (frame)
    make(<table-layout>,
         columns: 2,
	 x-alignment: #[#"right", #"left"],
	 y-alignment: #"center",
	 children: vector(make(<label>, label: "Name:"),
			  make(<text-field>),
			  make(<label>, label: "Organization:"),
			  make(<text-field>)));
  pane tab-control (frame)
    begin
      let first-pages
	= vector(make(<tab-control-page>,
		      label: "Button",   child: frame.button),
		 make(<tab-control-page>,
		      label: "List Box", child: frame.list-box),
		 make(<tab-control-page>,
		      label: "Layout",   child: frame.table-layout-pane));
      let second-pages
	= vector(make(<tab-control-page>,
		      label: "Tree",     child: frame.tree-control),
		 make(<tab-control-page>,
		      label: "Table",    child: frame.table-control),
		 make(<tab-control-page>,
		      label: "Ellipse",  child: frame.ellipse-pane));
      frame.first-tab-pages  := first-pages;
      frame.second-tab-pages := second-pages;
      make(<tab-control>, pages: first-pages)
    end;
  pane main-layout (frame)
    vertically (y-spacing: 4, height: 300)
      make(<table-layout>,
           columns: 2,
           x-alignment: #(#"right", #"left"),
           children: vector(make(<label>, label: "Pages:"),
                            frame.which-pages));
      make(<separator>);
      frame.tab-control
    end;
  layout (frame) frame.main-layout;
  status-bar (frame) make(<status-bar>);
end frame <tab-control-frame>;

define function tab-control-activate-callback
    (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  frame-status-message(frame)
    := format-to-string("Activated gadget: value %=", gadget-value(gadget))
end function tab-control-activate-callback;

define method update-tab-control-pages
    (frame :: <tab-control-frame>, value :: <integer>) => ()
  let gadget = tab-control(frame);
  tab-control-pages(gadget)
    := select (value)
	 1 => frame.first-tab-pages;
	 2 => frame.second-tab-pages;
       end;
end method update-tab-control-pages;


/// Install the test

install-test(<tab-control-frame>, "Tab controls");
