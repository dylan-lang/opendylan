Module:    win32-duim-regression-test-suite
Author:    Andy Armstrong, Scott McKay
Synopsis:  A regression test-suite for Win32 DUIM
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Bug 4254: DUIM: draw-line and draw-text don't play very well together

define frame <bug-4254-frame> (<simple-frame>)
  pane drawing-pane (frame)
    make(<line-and-text-pane>, width: 300, height: 200);
  layout (frame)
    frame.drawing-pane;
end frame <bug-4254-frame>;

define class <line-and-text-pane> (<drawing-pane>)
end class <line-and-text-pane>;

define method handle-repaint
    (sheet :: <line-and-text-pane>, medium :: <medium>, region :: <region>)
 => ()
  ignore(region);
  draw-line(medium, 10, 10, 20, 20);
  draw-text(medium, "Hello", 10, 60);
  draw-line(medium, 10, 80, 20, 80);
  draw-text(medium, "Hello", 10, 100);
end method handle-repaint;

install-test
  (<bug-4254-frame>, 4254,
   "DUIM: draw-line and draw-text don't play very well together",
   "Make sure that both two lines and two pieces of text are displayed.");
