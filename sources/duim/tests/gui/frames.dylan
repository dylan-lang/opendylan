Module:       duim-gui-test-suite
Author:       Andy Armstrong
Synopsis:     DUIM testing code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Frame tests

define abstract frame <abstract-test-frame> (<frame>)
  constant slot frame-destroyed-callback :: false-or(<function>) = #f,
    init-keyword: destroyed-callback:;
end frame <abstract-test-frame>;

define method frame-can-exit?
    (frame :: <abstract-test-frame>) => (can-exit? :: <boolean>)
  notify-user("Really exit?", frame: frame, style: #"question")
end method frame-can-exit?;

define method handle-event
    (frame :: <abstract-test-frame>, event :: <frame-created-event>) => ()
  notify-user(format-to-string("Frame %= created", frame))
end method handle-event;

define method handle-event
    (frame :: <abstract-test-frame>, event :: <frame-mapped-event>) => ()
  notify-user(format-to-string("Frame %= mapped", frame))
end method handle-event;

define method handle-event
    (frame :: <abstract-test-frame>, event :: <frame-unmapped-event>) => ()
  notify-user(format-to-string("Frame %= unmapped", frame))
end method handle-event;

define method handle-event
    (frame :: <abstract-test-frame>, event :: <frame-exited-event>) => ()
  notify-user(format-to-string("Frame %= exited", frame))
end method handle-event;

define method handle-event
    (frame :: <abstract-test-frame>, event :: <application-exited-event>) => ()
  notify-user(format-to-string("Application %= exited", frame))
end method handle-event;

define method handle-event
    (frame :: <abstract-test-frame>, event :: <frame-destroyed-event>) => ()
  let callback = frame.frame-destroyed-callback;
  notify-user(format-to-string("Frame %= destroyed", frame));
  if (callback)
    callback(frame)
  end
end method handle-event;

define frame <test-simple-frame> (<abstract-test-frame>, <simple-frame>)
end frame <test-simple-frame>;

define frame <test-dialog-frame> (<abstract-test-frame>, <dialog-frame>)
end frame <test-dialog-frame>;


/// Frame tests

define frame <frame-test-frame> (<simple-frame>)
  slot frame-test-frame :: false-or(<frame>) = #f;
  pane frame-type-pane (frame)
    make(<radio-box>,
         orientation: #"vertical",
         items: vector(pair("Simple Frame", <test-simple-frame>),
                       pair("Dialog",       <test-dialog-frame>)),
         value-changed-callback: method (gadget)
                                   let enabled? = (gadget-value(gadget) == <test-simple-frame>);
                                   gadget-enabled?(frame.frame-extras-pane) := enabled?
                                 end,
         label-key:  head,
         value-key: tail);
  pane frame-title-pane (frame)
    make(<text-field>, text: "My Frame");
  pane frame-extras-pane (frame)
    make(<check-box>,
         orientation: #"vertical",
         items: vector(pair("Menu Bar",   #"menu-bar"),
                       pair("Tool Bar",   #"tool-bar"),
                       pair("Status Bar", #"status-bar")),
         label-key:  head,
         value-key: tail);
  pane frame-layout-pane (frame)
    make(<radio-box>,
         orientation: #"vertical",
         items: vector(pair("None", #f),
                       pair("Button", <button>),
                       pair("Tree", <tree-control>)),
         label-key:  head,
         value-key: tail);
  pane frame-x-pane (frame)
    make(<text-field>, value-type: <integer>, value: 0);
  pane frame-y-pane (frame)
    make(<text-field>, value-type: <integer>, value: 100);
  pane frame-width-pane (frame)
    make(<text-field>, value-type: <integer>, value: 200);
  pane frame-height-pane (frame)
    make(<text-field>, value-type: <integer>, value: 300);
  pane make-frame-button (frame)
    make(<push-button>, 
         label: "Make the frame",
         activate-callback: method (gadget)
                              frame-test-make-frame(sheet-frame(gadget))
                            end);
  pane update-frame-button (frame)
    make(<push-button>, 
         label: "Update it",
         enabled?: #f,
         activate-callback: method (gadget)
                              frame-test-update-frame(sheet-frame(gadget))
                            end,
         default?: #t);
  pane main-layout (frame)
    vertically (y-spacing: 4)
      make(<table-layout>,
           columns: 2,
           x-alignment: #(#"right", #"left"),
           y-alignment: #"center",
           y-spacing: 5,
           children: vector(make(<label>, label: "Frame type:"),
                            frame.frame-type-pane,
                            make(<label>, label: "Title:"),
                            frame.frame-title-pane,
                            make(<label>, label: "Gadgets:"),
                            frame.frame-extras-pane,
                            make(<label>, label: "Layout:"),
                            frame.frame-layout-pane,
			    make(<label>, label: "X:"),
			    frame.frame-x-pane,
			    make(<label>, label: "Y:"),
			    frame.frame-y-pane,
			    make(<label>, label: "Width:"),
			    frame.frame-width-pane,
			    make(<label>, label: "Height:"),
			    frame.frame-height-pane));
      make(<separator>);
      horizontally (spacing: 4)
        frame.make-frame-button;
        frame.update-frame-button;
      end
    end;
  layout (frame) frame.main-layout;
  status-bar (frame) make(<status-bar>);
end frame <frame-test-frame>;

define method frame-test-make-frame (frame :: <frame-test-frame>) => ()
  let x             = gadget-value(frame.frame-x-pane);
  let y             = gadget-value(frame.frame-y-pane);
  let width         = gadget-value(frame.frame-width-pane);
  let height        = gadget-value(frame.frame-height-pane);
  let title         = gadget-value(frame.frame-title-pane);
  let class         = gadget-value(frame.frame-type-pane);
  let extras        = gadget-value(frame.frame-extras-pane);
  let layout-option = gadget-value(frame.frame-layout-pane);
  let status-bar = member?(#"status-bar", extras) & make(<status-bar>);
  let tool-bar
    = member?(#"tool-bar", extras)
        & make(<tool-bar>,
               child: horizontally ()
                        make(<button>, label: "Shortcut...");
                      end);
  let menu-bar
    = if (member?(#"menu-bar", extras))
	let menu
	  = make(<menu>, 
		 label: "Menu",
		 children: vector(make(<menu-box>, items: #(1, 2, 3))));
        make(<menu-bar>, children: vector(menu))
      end;
  let layout
    = select (layout-option)
        <button> =>
          make(<button>, label: "Press Me!");
        <tree-control> =>
          make(<tree-control>,
	       roots: #(1), 
               children-generator: method (x) 
                                    vector(x * 2, 1 + (x * 2))
                                  end);
        #f =>
          #f;
      end;
  let new-frame
    = make(class,
           owner: frame,
           title: title,
           layout: layout,
           menu-bar: menu-bar, tool-bar: tool-bar, status-bar: status-bar,
           x: x, y: y, width: width, height: height,
           destroyed-callback: method (test-frame)
                                 gadget-enabled?(frame.update-frame-button) := #f
                               end);
  frame-test-frame(frame) := new-frame;
  gadget-enabled?(frame.update-frame-button) := #t;
  start-frame(new-frame)
end method frame-test-make-frame;

define method frame-test-update-frame (frame :: <frame-test-frame>) => ()
  let x             = gadget-value(frame.frame-x-pane);
  let y             = gadget-value(frame.frame-y-pane);
  let width         = gadget-value(frame.frame-width-pane);
  let height        = gadget-value(frame.frame-height-pane);
  let title         = gadget-value(frame.frame-title-pane);
  let class         = gadget-value(frame.frame-type-pane);
  let extras        = gadget-value(frame.frame-extras-pane);
  let layout-option = gadget-value(frame.frame-layout-pane);
  let test-frame = frame.frame-test-frame;
  frame-title(test-frame) := title;
  let current-layout-option
    = select (frame-layout(test-frame) by instance?)
	<button>       => <button>;
	<tree-control> => <tree-control>;
	otherwise      => #f;
      end;
  if (layout-option ~= current-layout-option)
    let layout
      = select (layout-option)
	  <button> =>
	    make(<button>, label: "Press Me!");
	  <tree-control> =>
	    make(<tree-control>,
		 roots: #(1), 
		 children-generator: method (x) 
				      vector(x * 2, 1 + (x * 2))
				    end);
	  #f =>
	    #f;
	end;
    frame-layout(test-frame) := layout;
  end;
  if (x & y)
    set-frame-position(test-frame, x, y);
  end;
  if (width & height)
    set-frame-size(test-frame, width, height)
  end;
end method frame-test-update-frame;


/// Multiple layout frame

define frame <multiple-layout-frame> (<simple-frame>)
  slot current-layout = #"debugging";
  pane file-menu (frame)
    make(<menu>,
	 label: "File",
	 children: vector(frame.switch-button,
			  frame.exit-button));
  pane switch-button (frame)
    make(<menu-button>,
	 label: "Switch layouts",
	 activate-callback: method (button)
			      select (frame.current-layout)
         			#"debugging" =>
         			  frame.current-layout := #"interacting";
         			  frame-layout(frame)  := frame.interacting-layout;
         			#"interacting" =>
         			  frame.current-layout := #"debugging";
         			  frame-layout(frame)  := frame.debugging-layout;
			      end
			    end);
  pane exit-button (frame)
    make(<menu-button>,
	 label: "Exit",
	 activate-callback: method (button)
			      exit-frame(sheet-frame(button))
			    end);
  pane context-pane (frame)
    make(<drawing-pane>,
	 display-function:
	   method (pane, medium, region)
	     draw-text(medium, "[Error message]", 0, 0,
		       align-x: #"left", align-y: #"top")
	   end);
  pane stack-pane (frame)
    make(<drawing-pane>,
	 display-function:
	   method (pane, medium, region)
	     draw-text(medium, "[Stack trace]", 0, 0,
		       align-x: #"left", align-y: #"top")
	   end);
  pane source-pane (frame)
    make(<drawing-pane>,
	 display-function:
	   method (pane, medium, region)
	     draw-text(medium, "[Source code]", 0, 0,
		       align-x: #"left", align-y: #"top")
	   end);
  pane interactor-pane (frame)
    make(<drawing-pane>,
	 display-function:
	   method (pane, medium, region)
	     draw-text(medium, "[Interactor]", 0, 0,
		       align-x: #"left", align-y: #"top")
	   end,
         height: $fill);
  pane message-pane (frame)
    make(<drawing-pane>,
	 display-function:
	   method (pane, medium, region)
	     draw-text(medium, "[Other messages]", 0, 0,
		       align-x: #"left", align-y: #"top")
	   end);
  pane debugging-layout (frame)
    vertically ()
      with-border (type: #"sunken") frame.context-pane end;
      horizontally ()
	with-border (type: #"sunken") frame.stack-pane end;
	with-border (type: #"sunken") frame.source-pane end;
      end;
      with-border (type: #"sunken") frame.interactor-pane end;
      with-border (type: #"sunken") frame.message-pane end;
    end;
  pane interacting-layout (frame)
    vertically ()
      with-border (type: #"sunken") frame.interactor-pane end;
      with-border (type: #"sunken") frame.message-pane end;
    end;
  layout (frame)
    frame.debugging-layout;
  menu-bar (frame) 
    make(<menu-bar>,
	 children: vector(frame.file-menu));
  keyword width:  = 300;
  keyword height: = 400;
end frame <multiple-layout-frame>;


/// Install the tests

install-test(<frame-test-frame>, "Frames");
install-test(<multiple-layout-frame>, "Multiple layout frame");
