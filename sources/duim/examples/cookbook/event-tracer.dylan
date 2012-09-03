Module:       duim-examples
Author:       Andy Armstrong, Scott McKay
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Event tracing pane

define class <event-tracing-pane> (<drawing-pane>)
  slot event-report-function :: <function> = format-out,
    init-keyword: report-function:;
end class <event-tracing-pane>;

define method handle-event
    (sheet :: <event-tracing-pane>, event :: <event>) => ()
  sheet.event-report-function("  Handled event: %=\n", event)
end method handle-event;

define method handle-repaint
    (sheet :: <event-tracing-pane>, medium :: <medium>, region :: <region>) => ()
  #f
end method handle-repaint;

define method dispatch-event 
    (sheet :: <event-tracing-pane>, event :: <event>) => ()
  block ()
    next-method();
  afterwards
    sheet.event-report-function("  Dispatched event: %=\n", event)
  end
end method dispatch-event;

/// Event recording pane

define pane <event-recording-pane> ()
  pane event-column (pane)
    make(<column-layout>);
  layout (frame)
    scrolling ()
      frame.event-column
    end;
end pane <event-recording-pane>;

define method record-event 
    (sheet :: <event-recording-pane>, message :: <string>) => ()
  let column = event-column(sheet);
  let new-child = make(<label-pane>, label: message);
  add-child(column, new-child);
  relayout-parent(new-child);
  //---*** Why do I need to do this?
  sheet-mapped?(new-child) := #t;
  // let (x, y) = scroll-position(column);
  // let (left, top, right, bottom) = sheet-viewport-region(column);
  // set-scroll-position(column, x, bottom);
  repaint-sheet(column, $everywhere)
end method record-event;


/// Event tracing 

define frame <event-tracer> (<simple-frame>)
  pane event-pane (frame)
    make(<event-tracing-pane>,
              background: $red,
              report-function: method (string, #rest args)
                                 record-event(frame.event-recorder-pane,
                                              apply(format-to-string, string, args))
                               end,
	      width: 500, height: 300);
  pane event-recorder-pane (frame)
    make(<event-recording-pane>,
	      width: 500, height: 300);
  pane main-layout (frame)
    vertically ()
      frame.event-pane;
      frame.event-recorder-pane
    end;
  layout (frame) frame.main-layout;
end frame <event-tracer>;

install-example(<event-tracer>, "Event Tracer");
