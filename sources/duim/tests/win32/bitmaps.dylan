Module:       win32-duim-gui-test-suite
Author:       Andy Armstrong, Scott McKay
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Bitmaps test

define frame <bitmap-test-frame> (<simple-frame>)
  pane %clipboard-text (frame)
    make(<text-field>);
  pane %label-pane (frame)
    make(<label>, label: $wizard-icon | "Whoops, wizard icon should be here!");
  pane %push-button (frame)
    make(<push-button>, label: $cut-icon | "X");
  pane %push-box (frame)
    make(<push-box>,
         items: vector($cut-icon, $copy-icon, $paste-icon),
         label-key: identity);
  pane %radio-box (frame)
    make(<radio-box>,
         items: vector($cut-icon, $copy-icon, $paste-icon),
         label-key: identity);
  pane %check-box (frame)
    make(<check-box>,
         items: vector($cut-icon, $copy-icon, $paste-icon),
         label-key: identity);
  pane %drawing-pane (frame)
    make(<drawing-pane>, 
	 width:     200,   height:     100,
	 max-width: $fill, max-height: $fill);
  pane cut-button (frame)
    make(<button>,
         label: $cut-icon | "X",
         documentation: "Cut",
         activate-callback: method (button)
                              frame-cut(frame)
                            end method);
  pane copy-button (frame)
    make(<button>,
         label: $copy-icon | "C",
         documentation: "Copy",
         activate-callback: method (button)
                              frame-copy(frame)
                            end method);
  pane paste-button (frame)
    make(<button>,
         label: $paste-icon | "P",
         documentation: "Paste",
         activate-callback: method (button)
                              frame-paste(frame)
                            end method);
  pane rotate-button (frame)
    make(<button>,
         label: $values-bitmap | ">",
         documentation: "Rotate images",
         activate-callback: method (button)
                              frame-rotate-images(frame)
                            end);
  layout (frame)
    vertically (spacing: 2)
      tabling (columns: 2, 
               x-spacing: 4, y-spacing: 6,
               x-alignment: #[#"right", #"left"], y-alignment: #"center")
        make(<label>, label: "Clipboard:");
        frame.%clipboard-text;
        make(<label>, label: "Label:");
        frame.%label-pane;
        make(<label>, label: "Button:");
        frame.%push-button;
        make(<label>, label: "Buttons:");
        frame.%push-box;
        make(<label>, label: "Radio box:");
        frame.%radio-box;
        make(<label>, label: "Check box:");
        frame.%check-box;
      end;
      make(<label>, label: "Drawing pane:");
      with-border (type: #"sunken")
        frame.%drawing-pane
      end
    end;
  command-table (frame)
    *bitmap-command-table*;
  tool-bar (frame)
    make(<tool-bar>,
	 child: horizontally (spacing: 6)
                  horizontally (spacing: 0)
                    frame.cut-button;
                    frame.copy-button;
                    frame.paste-button;
                  end;
                  frame.rotate-button;
                end);
  keyword icon: = $paste-icon;
end frame <bitmap-test-frame>;


define command-table *file-command-table* (*global-command-table*)
  menu-item "Draw &icons"   = frame-draw-icons,
    documentation: "Draw the icons";
  menu-item "Draw &bitmaps" = frame-draw-bitmaps,
    documentation: "Draw the bitmaps";
  separator;
  menu-item "Close" = exit-frame,
    documentation: "Close the window";
end command-table *file-command-table*;

define command-table *edit-command-table* (*global-command-table*)
  menu-item "Cut"   = frame-cut,
    image: $cut-icon   | "X",
    documentation: "Cut to the clipboard";
  menu-item "Copy"  = frame-copy,
    image: $copy-icon  | "C",
    documentation: "Copy to the clipboard";
  menu-item "Paste" = frame-paste,
    image: $paste-icon | "P",
    documentation: "Paste from the clipboard";
end command-table *edit-command-table*;

define command-table *bitmap-command-table* (*global-command-table*)
  menu-item "File"    = *file-command-table*;
  menu-item "Edit"    = *edit-command-table*;
end command-table *bitmap-command-table*;



define method frame-cut
    (frame :: <bitmap-test-frame>) => (success? :: <boolean>)
  let gadget = frame.%clipboard-text;
  when (frame-copy(frame))
    gadget-value(gadget) := "";
    #t
  end
end method frame-cut;

define method frame-copy 
    (frame :: <bitmap-test-frame>) => (success? :: <boolean>)
  let gadget = frame.%clipboard-text;
  with-clipboard (clipboard = gadget)
    if (clipboard)
      if (add-clipboard-data(clipboard, gadget-value(gadget)))
	#t
      else
	notify-user("Failed to put text onto clipboard", owner: gadget);
	#f
      end
    else
      notify-user("Clipboard not available", owner: gadget);
      #f
    end
  end
end method frame-copy;

define method frame-paste 
    (frame :: <bitmap-test-frame>) => (success? :: <boolean>)
  let gadget = frame.%clipboard-text;
  with-clipboard (clipboard = gadget)
    if (clipboard)
      let text = get-clipboard-data-as(<string>, clipboard);
      if (text)
	gadget-value(gadget) := text;
	#t
      else
	notify-user("No text on clipboard", owner: gadget);
	#f
      end
    else
      notify-user("Clipboard not available", owner: gadget);
      #f
    end
  end
end method frame-paste;

define method frame-rotate-images
    (frame :: <bitmap-test-frame>) => ()
  frame.%label-pane.gadget-label
    := if (frame.%label-pane.gadget-label == $paste-icon)
         $wizard-icon
       else
         $paste-icon
       end;
  frame.%push-button.gadget-label
    := if (frame.%push-button.gadget-label == $cut-icon)
         $copy-icon
       else
         $cut-icon
       end;
  rotate-gadget-items(frame.%push-box);
  rotate-gadget-items(frame.%radio-box);
  rotate-gadget-items(frame.%check-box);
end method frame-rotate-images;

define method rotate-gadget-items
    (gadget :: <collection-gadget>) => ()
  let items = gadget.gadget-items;
  gadget.gadget-items
    := concatenate(copy-sequence(items, start: 1), vector(items[0]))
end method rotate-gadget-items;

define method frame-draw-icons
    (frame :: <bitmap-test-frame>) => ()
  let sheet = frame.%drawing-pane;
  if ($cut-icon & $copy-icon & $paste-icon)
    clear-box*(sheet, sheet-region(sheet));
    draw-image(sheet, $cut-icon,   10, 10);
    draw-image(sheet, $copy-icon,  10, 40);
    draw-image(sheet, $paste-icon, 10, 70)
  else
    notify-user("Icons did not get loaded", owner: sheet);   
  end
end method frame-draw-icons;

define method frame-draw-bitmaps
    (frame :: <bitmap-test-frame>) => ()
  let sheet = frame.%drawing-pane;
  if ($location-bitmap & $prompt-bitmap & $values-bitmap)
    clear-box*(sheet, sheet-region(sheet));
    draw-image(sheet, $location-bitmap, 10, 10);
    draw-image(sheet, $prompt-bitmap,   10, 40);
    draw-image(sheet, $values-bitmap,   10, 70)
  else
    notify-user("Bitmaps did not get loaded", owner: sheet);   
  end
end method frame-draw-bitmaps;



/// Install the test
install-test(<bitmap-test-frame>, "Bitmaps and Icons");
