Module:       duim-gui-test-suite
Author:       Andy Armstrong
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Menu handling

define class <my-drawing-pane> (<drawing-pane>)
end class <my-drawing-pane>;

define method handle-event 
    (client :: <my-drawing-pane>, event :: <button-release-event>) => ()
  if (event-button(event) == $right-button)
    let frame = sheet-frame(client);
    let menu = popup-menu(frame);
    display-menu(menu, x: event.event-x, y: event.event-y)
  end
end method handle-event;

define frame <menu-test-frame> (<simple-frame>)
  pane drawing-pane (frame)
    make(<my-drawing-pane>,
         client: frame,
         width: 300, height: 100);
  pane test-button-state-field (frame)
    make(<radio-box>,
         items: #(#("Enabled", #t), #("Disabled", #f)),
         label-key: first,
         value-key: second,
	 documentation: "Choose whether to enable or disable the test menu button",
         value: #t,
         value-changed-callback: method (button)
                                   let frame = sheet-frame(button);
                                   gadget-enabled?(frame.test-menu-button)
                                     := gadget-value(button)
                                 end);
  pane test-button-label-field (frame)
    make(<text-field>,
         value: "Test Button",
	 documentation: "Change the text here to update the test menu button's label",
         value-changing-callback: method (gadget)
                                    let frame = sheet-frame(gadget);
                                    gadget-label(frame.test-menu-button)
                                      := gadget-value(gadget)
                                  end);
  pane button-modifiers (frame)
    make(<table-layout>,
         x-alignment: #[#"right", #"left"],
         y-alignment: #"center",
         columns: 2,
         children: vector(make(<label>, label: "State:"),
                          frame.test-button-state-field,
                          make(<label>, label: "Label:"),
                          frame.test-button-label-field));
  pane main-layout (frame)
    vertically (y-spacing: 5)
      vertically ()
        make(<label>, label: "Test button state:");
        with-spacing (spacing: 8)
          frame.button-modifiers
        end
      end;
      make(<separator>);
      vertically ()
        make(<label>, label: "Right click below for a popup menu:");
        with-border (type: #"raised")
          frame.drawing-pane
        end
      end
    end;
  pane file-menu (frame)
    make(<menu>,
         label: "File",
         children: vector(make(<menu-button>,
                               label: "Close",
			       documentation: "Closes this window",
                               activate-callback: method (sheet)
                                                    exit-frame(sheet-frame(sheet))
                                                  end)));
  pane test-menu-button (frame)
    make(<menu-button>,
         label: "Test Button");
  pane dynamic-menu-box (frame)
    make(<push-menu-box>,
	 items: #[1, 2, 3, 4, 5, 6, 7, 8, 9],
	 update-callback: method (box :: <menu-box>)
			    let items = gadget-items(box);
			    let last-index = size(items) - 1;
			    let new-items
			      = concatenate
			          (vector(items[last-index]),
				   copy-sequence(items, end: last-index));
			    gadget-items(box) := new-items
			  end,
	 activate-callback: method (box :: <menu-box>)
			      notify-user(format-to-string
					    ("Pressed button %=", 
					     gadget-value(box)),
					  owner: frame)
			    end);
  pane dynamic-menu (frame)
    make(<menu>,
         label: "Dynamic",
         children: vector(frame.test-menu-button, frame.dynamic-menu-box));
  pane submenu (frame)
    make(<menu>,
         label: "Submenu",
	 documentation: "The color submenu",
         children: vector(make(<radio-menu-box>,
                               items: #("Red", "Green", "Blue"),
			       documentation: "Changes the color",
                               value-changed-callback: change-color-callback)));
  pane popup-submenu (frame)
    make(<menu>,
         label: "Submenu",
	 documentation: "The color submenu",
         children: vector(make(<radio-menu-box>,
                               items: #("Red", "Green", "Blue"),
			       documentation: "Changes the color",
                               value-changed-callback: change-color-callback)));
  pane popup-menu (frame)
    make(<menu>,
         owner: frame.drawing-pane,
         children: vector(make(<menu-button>, 
			       label: "Action One",
			       documentation: "Does action one!",
                               activate-callback: action-callback),
			  make(<menu-button>, 
			       label: "Action Two",
			       documentation: "Does action two!",
			       default?: #t,
                               activate-callback: action-callback),
			  make(<menu-button>, 
			       label: "Action Three",
			       documentation: "Does action three!",
                               activate-callback: action-callback),
                          make(<radio-menu-box>,
                               items: #("Red", "Green", "Blue"),
			       documentation: "Changes the color",
                               value-changed-callback: change-color-callback),
                          frame.popup-submenu,
                          make(<check-menu-box>,
                               items: #("One", "Two", "Three"),
			       documentation: "Select a number!",
                               value-changed-callback: new-check-value-callback)));
  pane kitchen-sink-menu (frame)
    make(<menu>,
	 label: "Kitchen Sink",
         children: vector(make(<menu-button>, 
			       label: "Action One",
			       documentation: "Does action one!",
                               activate-callback: action-callback),
			  make(<menu-button>, 
			       label: "Action Two",
			       documentation: "Does action two!",
			       default?: #t,
                               activate-callback: action-callback),
			  make(<menu-button>, 
			       label: "Action Three",
			       documentation: "Does action three!",
                               activate-callback: action-callback),
                          make(<radio-menu-box>,
                               items: #("Red", "Green", "Blue"),
			       documentation: "Changes the color",
                               value-changed-callback: change-color-callback),
                          frame.submenu,
                          make(<check-menu-box>,
                               items: #("One", "Two", "Three"),
			       documentation: "Select a number!",
                               value-changed-callback: new-check-value-callback)));
  pane status (frame)
    make(<status-bar>);
  menu-bar (frame)
    make(<menu-bar>,
         children: vector(frame.file-menu, 
			  frame.kitchen-sink-menu,
			  frame.dynamic-menu));
  layout (frame) frame.main-layout;
  status-bar (frame) frame.status;
end frame <menu-test-frame>;

define method sheet-message 
    (sheet :: <sheet>, message :: <string>, #rest format-args) => ()
  let frame = sheet-frame(sheet);
  gadget-label(status(frame)) := apply(format-to-string, message, format-args)
end method sheet-message;

define method action-callback (gadget :: <gadget>) => ()
  sheet-message(gadget, "Activated gadget '%s'", gadget-label(gadget))
end method action-callback;

define method change-color-callback (gadget :: <gadget>) => ()
  sheet-message(gadget, "Changed color to %=", gadget-value(gadget))
end method change-color-callback;

define method new-check-value-callback (gadget :: <gadget>) => ()
  sheet-message(gadget, "Check box value %=", gadget-value(gadget))
end method new-check-value-callback;

install-test(<menu-test-frame>, "Menus");
