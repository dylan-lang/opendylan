Module: dylan-framework
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Dummy definitions:

define constant $blackColor = black:;
define constant $cyanColor = cyan:;

define class <point> (<object>) 
  slot x, init-keyword: x:;
  slot y, init-keyword: y:;
end;

define constant point = method (x, y) make(<point>, x: x, y: y) end;

define class <rectangle> (<object>)
  slot top, init-keyword: top:;
  slot bottom, init-keyword: bottom:;
  slot left, init-keyword: left:;
  slot right, init-keyword: right:;
end class;

define class <region> (<object>) 
  slot x, init-keyword: x:;
  slot y, init-keyword: y:;
  slot width, init-keyword: width:;
  slot height, init-keyword: height:;
end;

define constant <QDRect> = <rectangle>;

define method as-rect (r :: <region>)
  make(<rectangle>,
       top:    r.y,
       bottom: r.y + r.height,
       left:   r.x,
       right:  r.x + r.width)
end method;

// Horizontal and vertical extent?

define class <extent> (<object>)
  slot h, init-keyword: h:;
  slot v, init-keyword: v:;
end class;

define macro with-stack-structure
  { with-stack-structure (?name(?type:expression), ?code:expression) }
    => { let ?name = make(?type);
         ?code }
end macro;

define class <view> (<object>) 
  slot extent,
    init-function: method () make(<extent>, h: 100, v: 100) end;
end;

define macro with-focused-view 
  { with-focused-view (?view:expression) ?body end } 
    => { call-with-focused-view(?view, method () ?body end) }
end macro;

define method call-with-focused-view (view, body)
  block ()
    body()
  afterwards
    representation-force-output(view.representation);
  end
end method;

define generic do-setup-menus (view) => ();

define class <tracker> (<object>) end;

define generic track-feedback 
    (tracker, last-point, current-point, mode) => ();

define class <event-handler> (<object>) end;

define class <event> (<object>) end;

define generic do-event 
    (view, event, id) => ();

define class <mouse-down-event> (<event>) end;
define class <menu-event> (<event>) end;

define class <behavior> (<object>) end;

define generic behavior-setup-menus
    (behavior, next-behaviors, event-handler) => ();

define generic behavior-event
    (behavior, next-behaviors, event-handler, event, id) => ();

define method define-framework-library (name) end;

define method set-library-init-function (name) end; // Apple?

define method make-scrolling-window (views, #rest stuff)
  head(views)
end method;

// eof
