Module:    win32-duim-regression-test-suite
Author:    Andy Armstrong, Scott McKay
Synopsis:  An interactive test-suite for Win32 DUIM
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Some useful callbacks

define method test-command
    (frame :: <frame>) => ()
  notify-user("Command executed!", owner: frame)
end method test-command;

define method test-activate-callback
    (gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  let message
    = format-to-string("Activated %=", 
		       gadget-label(gadget) | gadget);
  frame-status-message(frame) := message
end method test-activate-callback;

define method test-value-changed-callback
    (gadget :: <value-gadget>) => ()
  let frame = sheet-frame(gadget);
  let message = format-to-string("Value now %=", gadget-value(gadget));
  frame-status-message(frame) := message
end method test-value-changed-callback;

/*---*** not used yet...
define method test-value-changing-callback
    (gadget :: <value-gadget>) => ()
  let frame = sheet-frame(gadget);
  let message = format-to-string("Value changing to %=", gadget-value(gadget));
  frame-status-message(frame) := message
end method test-value-changing-callback;
*/

define method test-popup-menu-callback
    (gadget :: <value-gadget>, target :: <object>, #key x, y, #all-keys) => ()
  display-menu(make-test-menu(owner: sheet-frame(gadget)), x: x, y: y)
end method test-popup-menu-callback;


/// Some useful test gadgets

define method make-test-menu
    (#key owner) => (menu :: <menu>)
  make(<menu>,
       label: "Test",
       owner: owner,
       children: vector(make(<menu-box>, items: #(1, 2, 3, 4, 5))),
       activate-callback: test-activate-callback)
end method make-test-menu;


/// A useful graphic class

define class <ellipse-pane> (<drawing-pane>)
  constant slot ellipse-foreground :: <color> = $red,
    init-keyword: foreground:;
end class <ellipse-pane>;

define method handle-repaint
    (sheet :: <ellipse-pane>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  let (left, top, right, bottom) = box-edges(sheet);
  let center-x = floor/(right + left - 1, 2);
  let center-y = floor/(top + bottom - 1, 2);
  let x-radius = center-x - left;
  let y-radius = center-y - top;
  with-drawing-options (medium, brush: ellipse-foreground(sheet))
    draw-ellipse(medium, center-x, center-y, x-radius, 0, 0, y-radius)
  end
end method handle-repaint;


/// A useful text class

define class <text-pane> (<drawing-pane>)
  constant slot pane-text-style :: <text-style> = $default-text-style,
    init-keyword: text-style:;
  keyword min-width:  = 200;
  keyword min-height: = 50;
  keyword max-height: = 50;
end class <text-pane>;

define method handle-repaint
    (pane :: <text-pane>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  with-drawing-options (medium, text-style: pane-text-style(pane))
    draw-text(medium, "abcdefgABCDEFG", 0, 0, align-y: #"top")
  end
end method handle-repaint;
