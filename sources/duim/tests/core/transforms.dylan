Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Transform constant tests

define duim-geometry constant-test $identity-transform ()
  test-transform-position
    ("Identity transform", $identity-transform, 10, 20, 10, 20);
  check-transform-box
    ("Identity transform", $identity-transform,
     0,  10,  100, 200,
     0,  10,  100, 200)
end constant-test $identity-transform;


/// Transform test suites

define constant *translate-up-10+10*   = make-translation-transform(+10,+10);
define constant *translate-down-10+10* = make-translation-transform(-10,-10);

define constant *scale-up-4x4*   = make-scaling-transform(4,4);
define constant *scale-down-4x4* = make-scaling-transform(0.25,0.25);

define constant *rotate-up-1/4*   = make-rotation-transform(+0.25);
define constant *rotate-down-1/4* = make-rotation-transform(-0.25);


define constant *transforms-to-invert*
  = list(list($identity-transform, $identity-transform),
	 list(*translate-up-10+10*, *translate-down-10+10*),
         list(*scale-up-4x4*, *scale-down-4x4*),
         list(*rotate-up-1/4*, *rotate-down-1/4*));

define method normalize-transform-component (component)
  floor(component * 10000) / 10000.0
end method normalize-transform-component;

define method normalize-transform (transform)
  let (mxx, mxy, myx, myy, tx, ty) = transform-components(transform);
  make-transform(normalize-transform-component(mxx),
                 normalize-transform-component(mxy),
                 normalize-transform-component(myx),
                 normalize-transform-component(myy),
                 normalize-transform-component(tx),
                 normalize-transform-component(ty))
end method normalize-transform;

define method test-invert-transform (tr1, tr2)
  check-true(format-to-string("invert-transform(%=) = transform(%=)",
                               printable-value(tr1), printable-value(tr2)),
             transform-equal(normalize-transform(invert-transform(tr1)),
                             normalize-transform(tr2)))
end method test-invert-transform;

define test invert-transforms-test ()
  for (elt in *transforms-to-invert*)
    let tr1 = elt[0];
    let tr2 = elt[1];
    test-invert-transform(tr1, tr2);
    unless (tr1 == tr2)
      test-invert-transform(tr2, tr1)
    end
  end
end test invert-transforms-test;


define method check-transform-box
    (name :: <string>, transform :: <transform>,
     l1 :: <real>, t1 :: <real>, r1 :: <real>, b1 :: <real>, 
     l2 :: <real>, t2 :: <real>, r2 :: <real>, b2 :: <real>) => ()
  check-true(format-to-string("%s transform-box", name),
             begin
               let (left, top, right, bottom) = transform-box(transform, l1, t1, r1, b1);
               left = l2 & top = t2 & right = r2 & bottom = b2
             end)
end method check-transform-box; 

define test transform-box-test ()
  check-transform-box
    ("Translation transform", make-translation-transform(10, 20),
     0,  10,  100, 200,
     10, 30,  110, 220);
  check-transform-box
    ("Scaling transform", make-scaling-transform(10, 20),
     0,  10,  100,  200,
     0, 200, 1000, 4000);
  check-transform-box
    ("Reflection transform",
     make-reflection-transform*(make-point(0, 10), make-point(10, 10)),
     0,  10,   100, 200,
     0,  -180, 100, 10);
end test transform-box-test;


define constant *transform-position-tests*
  = list(list(make-translation-transform(10, 10),
              make-point(0,0), make-point(10,10)),
         list(make-scaling-transform(10, 10),
              make-point(10,10), make-point(100,100)),
         list(make-reflection-transform*(make-point(10, 0), make-point(10, 10)),
              make-point(20,10), make-point(0,10)));

define method printable-value
    (transform :: <transform>) => (string :: <string>)
  format-to-string("%=", transform)
end method printable-value;

define function test-transform-position
    (name :: <string>, transform :: <transform>,
     start-x :: <real>, start-y :: <real>,
     end-x :: <real>, end-y :: <real>) => ()
  let (new-x, new-y) = transform-position(transform, start-x, start-y);
  check-true(format-to-string("%s of %d, %d", name, start-x, start-y),
	     new-x = end-x & new-y = end-y)
end function test-transform-position;

define method transform-position-test-ok? (data)
  let transform   = data[0];
  let start-point = data[1];
  let end-point   = data[2];
  let test-name = format-to-string("Transform %s", printable-value(transform));
  test-transform-position(test-name, transform,
			  point-x(start-point), point-y(start-point),
			  point-x(end-point),   point-y(end-point))
end method transform-position-test-ok?;

define test transform-position-test ()
  for (test in *transform-position-tests*)
    transform-position-test-ok?(test)
  end
end test transform-position-test;


/// Sheet delta transforms

define test sheet-delta-transform-test ()
  let sheet = make-test-pane(<simple-pane>, x: 50, y: 100);
  let first-parent
    = make-test-pane(<pinboard-layout>,
                     children: vector(sheet),
                     x: 200, y: 300);
  let second-parent
    = make-test-pane(<pinboard-layout>,
                     children: vector(first-parent));
  check-equal("Sheet delta transform identity",
              sheet-delta-transform(sheet, sheet),
              $identity-transform);
  check-equal("Sheet delta transform identity",
              sheet-delta-transform(sheet, first-parent),
              make-translation-transform(50, 100));
  check-equal("Sheet delta transform one-level",
              sheet-delta-transform(sheet, second-parent),
              make-translation-transform(250, 400));
end test sheet-delta-transform-test;


/// Define the transforms test suite

define suite duim-transforms-suite ()
  test invert-transforms-test;
  test transform-box-test;
  test transform-position-test;
  test sheet-delta-transform-test;
end suite duim-transforms-suite;
