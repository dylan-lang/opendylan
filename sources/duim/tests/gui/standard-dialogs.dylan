Module:       duim-gui-test-suite
Author:       Andy Armstrong
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Standard dialogs frame

define frame <standard-dialogs-frame> (<simple-frame>)
  pane ellipse-pane (frame)
    make(<ellipse-pane>, foreground: $red);
  pane text-pane (frame)
    make(<text-pane>);
  pane new-file-button (frame)
    make(<menu-button>,
         label: "Open...",
	 documentation: "Test of the standard file 'Open' dialog",
         activate-callback: method (button)
                              let file = choose-file(direction: #"input", owner: frame);
                              if (file)
                                frame-status-message(frame)
                                  := format-to-string("Opened file %s", file);
                              end
                            end);
  pane open-file-button (frame)
    make(<menu-button>,
         label: "Save As...",
	 documentation: "Test of the standard file 'Save As' dialog",
         activate-callback: method (button)
                              let file = choose-file(direction: #"output", owner: frame);
                              if (file)
                                frame-status-message(frame)
                                  := format-to-string("Saved file as %s", file);
                              end
                            end);
  pane choose-directory-button (frame)
    make(<menu-button>,
	 label: "Choose Directory...",
	 documentation: "Choose a directory using the standard dialog",
	 activate-callback: method (button)
                              let directory = choose-directory(owner: frame);
                              if (directory)
                                frame-status-message(frame)
                                  := format-to-string("Directory: %s", 
						      directory)
                              end
                            end);
  pane choose-color-button (frame)
    make(<menu-button>,
         label: "Choose Color...",
	 documentation: "Test of the standard 'choose color' dialog",
         activate-callback: method (button)
                              let color = choose-color(owner: frame);
                              color & change-ellipse-color(frame, color)
                            end);
  pane choose-font-button (frame)
    make(<menu-button>,
         label: "Choose Font...",
	 documentation: "Test of the standard 'choose font' dialog",
         activate-callback: method (button)
                              let font = choose-text-style(owner: frame);
                              font & change-text-font(frame, font)
                            end);
  pane exit-button (frame)
    make(<menu-button>,
	 documentation: "Exit this example",
         label: "Exit",
         activate-callback: method (button)
                              exit-frame(sheet-frame(button))
                            end);
  pane about-button (frame)
    make(<menu-button>,
         label: "About Standard Dialogs Test",
	 documentation: "Show information about this test window",
         activate-callback: method (button)
                              notify-user("Standard Dialogs Test", owner: frame)
                            end);
  pane file-menu-box (frame)
    make(<menu-box>,
         children: vector(frame.new-file-button, 
			  frame.open-file-button,
			  frame.choose-directory-button));
  pane file-menu (frame)
    make(<menu>,
         label: "File",
	 documentation: "The standard File menu",
         children: vector(frame.file-menu-box,
                          frame.exit-button));
  pane edit-menu (frame)
    make(<menu>,
         label: "Edit",
	 documentation: "The standard Edit menu",
         children: vector(frame.choose-color-button,
                          frame.choose-font-button));
  pane help-menu (frame)
    make(<menu>,
         label: "Help",
	 documentation: "The standard Help menu",
         children: vector(frame.about-button));
  menu-bar (frame)
    make(<menu-bar>, 
	 children: vector(frame.file-menu, frame.edit-menu, frame.help-menu));
  layout (frame)
    vertically (spacing: 4)
      with-border (type: #"sunken")
        frame.text-pane
      end;
      with-border (type: #"sunken")
        frame.ellipse-pane
      end;
    end;
  status-bar (frame)
    make(<status-bar>);
end frame <standard-dialogs-frame>;

define function change-ellipse-color
    (frame :: <standard-dialogs-frame>, color :: <color>) => ()
  let pane = frame.ellipse-pane;
  ellipse-foreground(pane) := color;
  repaint-sheet(pane, $everywhere);
  let (red, green, blue) = color-rgb(color);
  frame-status-message(frame)
    := format-to-string("Ellipse color is now %d, %d, %d",
			red, green, blue)
end function change-ellipse-color;

define function change-text-font
    (frame :: <standard-dialogs-frame>, text-style :: <text-style>) => ()
  let pane = frame.text-pane;
  pane-text-style(pane) := text-style;
  clear-box*(pane, $everywhere);
  repaint-sheet(pane, $everywhere);
  frame-status-message(frame) := format-to-string("Font is %s", text-style)
end function change-text-font;

install-test(<standard-dialogs-frame>, "Standard dialogs");
