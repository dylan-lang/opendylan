Module:       duim-examples
Author:       Andy Armstrong, Scott McKay
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DUIM version of windows-ffi-example

define constant $message = "Dylan lives!";
define constant $title = "Simple Example";
define constant $long-dash-pen = make(<pen>, dashes: #[18, 6]);

define method draw-graphics 
    (pane :: <drawing-pane>, medium :: <medium>, region :: <region>)
 => ()
  draw-text(medium, $message, 100, 50);
  with-drawing-options (medium, brush: $green)
    draw-rectangle(medium, 20, 20, 70, 70);
  end;
  draw-rectangle(medium, 20, 20, 70, 70, filled?: #f);
  with-drawing-options (medium, pen: $long-dash-pen)
    draw-line(medium, 100, 100, 250, 100);
  end;
end method draw-graphics;

define frame <simple-window> (<simple-frame>)
  pane drawing-surface (frame)
    make(<drawing-pane>,
	      display-function: draw-graphics);
  pane file-component (frame)
    make(<menu-box>,
	      children: vector(make(<menu-button>,
					 label: "&New",
					 enabled?: #f),
			       make(<menu-button>,
					 label: "&Open...",
					 enabled?: #f),
			       make(<menu-button>,
					 label: "&Save",
					 enabled?: #f),
			       make(<menu-button>,
					 label: "Save &As...",
					 enabled?: #f)));
  pane print-component (frame)
    make(<menu-box>,
	      children: vector(make(<menu-button>,
					 label: "&Print...",
					 enabled?: #f),
			       make(<menu-button>,
					 label: "P&rint Setup...",
					 enabled?: #f)));
  pane exit-button (frame)
    make(<menu-button>,
	      label: "E&xit",
	      activate-callback: method (sheet)
				   exit-frame(sheet-frame(sheet))
				 end);
  pane file-menu (frame)
    make(<menu>,
	      label: "&File",
	      children: vector(frame.file-component,
			       frame.print-component ,
			       frame.exit-button));
  pane undo-button (frame)
    make(<menu-button>,
	      label: "&Undo",
	      enabled?: #f);
  pane clipboard-component (frame)
    make(<menu-box>,
	      children: vector(make(<menu-button>,
					 label: "Cu&t",
					 enabled?: #f),
			       make(<menu-button>,
					 label: "&Copy",
					 enabled?: #f),
			       make(<menu-button>,
					 label: "&Paste",
					 enabled?: #f),
			       make(<menu-button>,
					 label: "Paste &Link",
					 enabled?: #f)));
  pane links-button (frame)
    make(<menu-button>,
	      label: "Lin&ks...",
	      enabled?: #f);
  pane edit-menu (frame)
    make(<menu>,
	      label: "&Edit",
	      children: vector(frame.undo-button,
			       frame.clipboard-component,
			       frame.links-button));
  pane help-component (frame)
    make(<menu-box>,
	      children: vector(make(<menu-button>,
					 label: "&Contents"),
			       make(<menu-button>,
					 label: "&Search for Help On..."),
			       make(<menu-button>,
					 label: "&How to Use Help")));
  pane about-button (frame)
    make(<menu-button>,
	      label: "&About Example...",
	      activate-callback: method (sheet)
				   about-example(sheet-frame(sheet))
				 end);
  pane help-menu (frame)
    make(<menu>,
	      label: "&Help",
	      children: vector(frame.help-component,
			       frame.about-button));
  layout (frame) frame.drawing-surface;
  menu-bar (frame)
    make(<menu-bar>,
	      children: vector(frame.file-menu,
			       frame.edit-menu,
			       frame.help-menu));
end frame <simple-window>;

define method about-example (frame :: <simple-window>) => ()
  with-frame-manager (frame-manager(frame))
    let text-layout
      = make(<column-layout>,
                  x-alignment: #"center",
                  children: vector(make(<label>,
                                             label: "Functional Objects, Inc."),
                                   make(<label>,
                                             label: "Dylan example program"),
                                   make(<label>,
                                             label: "Version 0.0")));
    make(<dialog-frame>,
	 layout: text-layout,
	 title: "About Example",
	 owner: frame,
	 cancel-function: #f,
	 mapped?: #t);
  end
end method about-example;

install-example(<simple-window>, $title);
