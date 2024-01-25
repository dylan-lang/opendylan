Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Geometry tests

define sideways method make-test-instance
    (class == <point>) => (instance :: <point>)
  make(<point>, x: 10, y: 20)
end method make-test-instance;

define test test-$largest-coordinate ()
  check-true("$smallest-coordinate < $largest-coordinate",
             $smallest-coordinate < $largest-coordinate);
  check-true("$largest-coordinate > 10000",
             $largest-coordinate > 10000)
end test;

define test test-$smallest-coordinate ()
  check-true("$smallest-coordinate < -10000",
             $smallest-coordinate < -10000)
end test;


/// Function tests

define test test-do-coordinates ()
  //---*** Fill this in...
end test;

define test test-do-endpoint-coordinates ()
  //---*** Fill this in...
end test;

define test test-fix-coordinate ()
  //---*** Fill this in...
end test;

define test test-bounding-box ()
  //---*** Fill this in...
end test;

define test test-bounding-box? ()
  //---*** Fill this in...
end test;

define test test-box-edges ()
  //---*** Fill this in...
end test;

define test test-set-box-edges ()
  //---*** Fill this in...
end test;

define test test-box-position ()
  //---*** Fill this in...
end test;

define test test-set-box-position ()
  //---*** Fill this in...
end test;

define test test-box-size ()
  //---*** Fill this in...
end test;

define test test-set-box-size ()
  //---*** Fill this in...
end test;

define test test-box-left ()
  //---*** Fill this in...
end test;

define test test-box-top ()
  //---*** Fill this in...
end test;

define test test-box-right ()
  //---*** Fill this in...
end test;

define test test-box-bottom ()
  //---*** Fill this in...
end test;

define test test-box-height ()
  //---*** Fill this in...
end test;

define test test-box-width ()
  //---*** Fill this in...
end test;

define test test-make-bounding-box ()
  //---*** Fill this in...
end test;


/// Tests
define test coordinates-test ()
  let x-count = 0;
  let y-count = 0;
  do-coordinates(method (x, y)
                   x-count := x-count + x;
                   y-count := y-count + y;
                 end method,
                 vector(10, 100,
                        90, 200));
  check-true("do-coordinates in x", x-count = 100);
  check-true("do-coordinates in y", y-count = 300);
  let top-count = 0;
  let left-count = 0;
  let bottom-count = 0;
  let right-count = 0;
  do-endpoint-coordinates
    (method (left, top, right, bottom)
       left-count   := left-count   + left;
       top-count    := top-count    + top;
       right-count  := right-count  + right;
       bottom-count := bottom-count + bottom;
     end method,
     vector(10,  100,
            150, 300,
            90,  200,
            250, 400));
  check-true("do-endpoint-coordinates for left",   left-count = 100);
  check-true("do-endpoint-coordinates for top",    top-count = 300);
  check-true("do-endpoint-coordinates for right",  right-count = 400);
  check-true("do-endpoint-coordinates for bottom", bottom-count = 700);
end test coordinates-test;


/// Geometry class tests

define sideways method make-test-instance
    (class == <bounding-box>) => (box :: <bounding-box>)
  make(<bounding-box>, left: 0, top: 0, right: 100, bottom: 100)
end method make-test-instance;

define sideways method make-test-instance
    (class == <standard-point>) => (box :: <standard-point>)
  make(<standard-point>, x: 100, y: 150)
end method make-test-instance;

define sideways method make-test-instance
    (class == <reflection-underspecified>)
 => (instance :: <reflection-underspecified>)
  make(<reflection-underspecified>, points: #())
end method make-test-instance;

define sideways method make-test-instance
    (class == <singular-transform>) => (instance :: <singular-transform>)
  make(<singular-transform>, transform: make-transform(0, 0, 0, 0, 0, 0))
end method make-test-instance;

define sideways method make-test-instance
    (class == <transform-underspecified>)
 => (instance :: <transform-underspecified>)
  make(<transform-underspecified>, points: #())
end method make-test-instance;

define test test-<bounding-box> ()
  let box = make-bounding-box(50, 100, 150, 300);
  check-true("bounding-box?(box)", bounding-box?(box));
  check-false("bounding-box?(100)", bounding-box?(100));
  check-equal("box-left",   box-left(box),   50);
  check-equal("box-top",    box-top(box),    100);
  check-equal("box-right",  box-right(box),  150);
  check-equal("box-bottom", box-bottom(box), 300);
  check-equal("box-width",  box-width(box), 100);
  check-equal("box-height", box-height(box), 200);
  let (x, y) = box-position(box);
  check-equal("box-position x", x, 50);
  check-equal("box-position y", y, 100);
  let (left, top, right, bottom) = box-edges(box);
  check-equal("box-edges left",   left,   50);
  check-equal("box-edges top",    top,    100);
  check-equal("box-edges right",  right,  150);
  check-equal("box-edges bottom", bottom, 300);
  set-box-size(box, 400, 450);
  check-equal("set-box-size keeps old left",   box-left(box),   50);
  check-equal("set-box-size keeps old top",    box-top(box),    100);
  check-equal("set-box-size new width", box-width(box), 400);
  check-equal("set-box-size new height", box-height(box), 450);
  set-box-position(box, 0, 50);
  check-equal("set-box-position new left",         box-left(box),   0);
  check-equal("set-box-position new top",          box-top(box),    50);
  check-equal("set-box-position keeps old width",  box-width(box),  400);
  check-equal("set-box-position keeps old height", box-height(box), 450);
  set-box-edges(box, 100, 150, 200, 400);
  check-equal("set-box-edges box-left",   box-left(box),   100);
  check-equal("set-box-edges box-top",    box-top(box),    150);
  check-equal("set-box-edges box-right",  box-right(box),  200);
  check-equal("set-box-edges box-bottom", box-bottom(box), 400);
  let new-box = bounding-box(box);
  check-false("bounding-box creates a new box", new-box == box);
  check-equal("bounding-box creates identical box", new-box, box);
end test;

define test test-<area> ()
  //---*** Fill this in...
end test;

define test test-<path> ()
  //---*** Fill this in...
end test;

define test test-<point> ()
  //---*** Fill this in...
end test;

define test test-<region-set> ()
  //---*** Fill this in...
end test;

define test test-<region> ()
  //---*** Fill this in...
end test;

define test test-<standard-point> ()
  //---*** Fill this in...
end test;

define test test-<singular-transform> ()
  //---*** Fill this in...
end test;

define test test-<transform-error> ()
  //---*** Fill this in...
end test;

define test test-<transform> ()
  //---*** Fill this in...
end test;

define test test-<polygon> ()
  //---*** Fill this in...
end test;

define test test-<line> ()
  //---*** Fill this in...
end test;

define test test-<elliptical-arc> ()
  //---*** Fill this in...
end test;

define test test-<rectangle> ()
  //---*** Fill this in...
end test;

define test test-<ellipse> ()
  //---*** Fill this in...
end test;

define test test-<polyline> ()
  //---*** Fill this in...
end test;


/// Install the geometry test suite

define suite duim-geometry-suite ()
  test coordinates-test;
end suite duim-geometry-suite;
