Module: simple-draw
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Simple geometry objects:

define class <point> (<object>)
  slot x, init-keyword: x:;
  slot y, init-keyword: y:;
end class;

define constant point 
  = method (x, y) make(<point>, x: x, y: y) end;

define variable h = #f; h := x; // Attempt to defeat aliasing bug
define variable v = #f; v := y; 

define class <rectangle> (<object>)
  slot top, init-keyword: top:;
  slot bottom, init-keyword: bottom:;
  slot left, init-keyword: left:;
  slot right, init-keyword: right:;
end class;

define class <QDrect> (<rectangle>) end;

define class <region> (<object>) 
  slot x, init-keyword: x:;
  slot y, init-keyword: y:;
  slot width, init-keyword: width:;
  slot height, init-keyword: height:;
end;

define method as-rect (r :: <region>)
  make(<rectangle>,
       top:    r.y,
       bottom: r.y + r.height,
       left:   r.x,
       right:  r.x + r.width)
end method;

define method SetRect (r :: <rectangle>, _left, _top, _right, _bottom)
  r.left   := _left;
  r.top    := _top;
  r.right  := _right;
  r.bottom := _bottom;
end method;

define class <view> (<object>) 
  slot representation, 
    init-keyword: representation:;
  slot extent, 
    init-keyword: extent:;
end;

define generic extent (v :: <view>) => (p :: <point>);
define generic draw (v :: <view>, r :: <region>) => ();
define generic idle (v :: <view>) => ();
define generic invalidate-all (v :: <view>) => ();

define macro with-focused-view 
  { with-focused-view (?view:expression) ?body end } 
    => { call-with-focused-view(?view, method () ?body end) }
end macro;

define method call-with-focused-view (view :: <view>, body)
  body();
  // representation-force-output(view.representation);
end method;

define generic ForeColor (color);
define generic PenMode (mode);
define generic MoveTo (x, y);
define generic Line (dx, dy);
define generic LineTo (x, y);
define generic FrameRect (r :: <rectangle>);

define variable $patCopy = #f; $patCopy := #"copy";
define variable $patBic  = #f; $patBic := #"bic";

define variable $blackColor = #f; $blackColor := #"black";
define variable $cyanColor = #f; $cyanColor := #"cyan";

// Simple random number generator lifted from LispWorks

define constant random-constant-a = 8373;
define constant random-constant-c = 101010101;
define constant random-upper-bound = 134217726;
define constant random-max = 54;
define variable rand-seed = 0;

define method Random ()
  rand-seed 
    := modulo(rand-seed * random-constant-a + random-constant-c, 
              random-upper-bound + 1)
end method;

// eof
