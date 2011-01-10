Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Region constant tests

define duim-geometry constant-test $everywhere ()
  check-false("$everywhere is not empty", region-empty?($everywhere))
end constant-test $everywhere;

define duim-geometry constant-test $nowhere ()
  check-true("$nowhere is empty", region-empty?($nowhere))
end constant-test $nowhere;


/// Region tests

define method printable-value (point :: <point>) => (value)
  vector("POINT", point-x(point), point-y(point))
end method printable-value;

define method printable-value (line :: <line>) => (value)
  let (x1, y1) = point-position(line-start-point(line));
  let (x2, y2) = point-position(line-end-point(line));
  vector("LINE", x1, y1, x2, y2)
end method printable-value;

define method printable-value (rectangle :: <rectangle>) => (value)
  let (left, right, top, bottom) = rectangle-edges(rectangle);
  vector("RECTANGLE", left, right, top, bottom)
end method printable-value;

define constant *regions-for-region-equal-tests*
  = list($nowhere,
	 $everywhere,
	 make-point(0, 0),
	 make-point(1, 1),
         make-line(0, 0, 10, 10),
	 make-line(10, 10, 20, 20),
         make-rectangle(0, 0, 10, 10),
	 make-rectangle(10, 10, 20, 20),
         make-polygon(#[0, 0, 10, 10, 20, 20, 30, 30, 0, 0]),
         make-polygon(#[0, 0, 10, 10, 20, 20, 30, 30, 40, 40, 0, 0]),
         make-ellipse(20, 20, 10, 0, 0, 10),
         make-ellipse(20, 20, 20, 0, 0, 10));

define test region-equal-test ()
  for (region1 in *regions-for-region-equal-tests*)
    for (region2 in *regions-for-region-equal-tests*)
      let name = format-to-string("%= = %=?", 
                                  printable-value(region1),
                                  printable-value(region2));
      check-equal(name, region-equal(region1, region2), region1 == region2)
    end
  end;
end test region-equal-test;

// region, position, expected result
define constant *regions-for-region-contains-position-tests*
  = list(list($nowhere, make-point(0, 0), #f),
         list($nowhere, make-point(5, 5), #f),
         list($everywhere, make-point(0, 0), #t),
         list($everywhere, make-point(5, 5), #t),
         list(make-point(0, 0), make-point(0, 0), #t),
         list(make-point(0, 0), make-point(5, 5), #f),
         list(make-line(0, 0, 10, 10), make-point(0, 0), #t),
         list(make-line(0, 0, 10, 10), make-point(5, 5), #t),
         list(make-line(0, 0, 10, 10), make-point(10, 10), #t),
         list(make-line(5, 0, 5, 10), make-point(0, 0), #f),
         list(make-line(5, 0, 5, 10), make-point(5, 5), #t),
         list(make-rectangle(0, 0, 10, 10), make-point(0, 0), #t),
         list(make-rectangle(0, 0, 10, 10), make-point(5, 5), #t),
         list(make-rectangle(0, 0, 10, 10), make-point(10, 10), #t),
         list(make-rectangle(0, 0, 10, 10), make-point(20, 20), #f),
         list(make-rectangle(10, 10, 20, 20), make-point(5, 5), #f),
         list(make-rectangle(10, 10, 20, 20), make-point(10, 10), #t));

define test region-contains-position-test ()
  for (x in *regions-for-region-contains-position-tests*)
    let region = x[0];
    let point = x[1];
    let expected = x[2];
    let result = region-contains-position?(region, point-x(point), point-y(point));
    let name = format-to-string("%= %s point %=",
                                printable-value(region),
                                case
                                  expected  => "contains";
                                  otherwise => "does not contain";
                                end,
                                printable-value(point));
    check-equal(name, result, expected)
  end;
end test region-contains-position-test;

define constant *regions-for-region-contains-region-tests*
  = list(list($nowhere, $everywhere, #f, #t),
         list($nowhere, make-point(0, 0), #f, #t),
         list($everywhere, make-point(0, 0), #t, #f),
         list(make-point(0, 0), make-point(1, 1), #f, #f),
         list(make-point(0, 0), make-line(0, 0, 10, 10), #f, #t),
         list(make-point(5, 5), make-line(0, 0, 10, 10), #f, #t),
         list(make-point(5, 5), make-line(5, 0, 5, 10), #f, #t),
         list($nowhere, make-line(0, 0, 10, 10), #f, #t),
         list($everywhere, make-line(0, 0, 10, 10), #t, #f),
         list(make-line(0, 0, 10, 10), make-line(10, 10, 20, 20), #f, #f),
         list(make-rectangle(0, 0, 10, 10), make-point(0, 0), #t, #f),
         list(make-rectangle(0, 0, 10, 10), make-point(1, 1), #t, #f),
         list(make-rectangle(0, 0, 10, 10), make-line(0, 0, 10, 10), #t, #f),
         list(make-rectangle(0, 0, 10, 10), make-rectangle(2, 3, 6, 7), #t, #f),
         list(make-rectangle(0, 0, 10, 10), make-rectangle(10, 10, 20, 20), #f, #f));

define test region-contains-region-test ()
  for (x in *regions-for-region-contains-region-tests*)
    let region1 = x[0];
    let region2 = x[1];
    let expected1 = x[2];
    let expected2 = x[3];
    let result = region-contains-region?(region1, region2);
    let name = format-to-string("%= %s %=",
                                printable-value(region1),
                                case
                                  expected1 => "contains";
                                  otherwise => "does not contain";
                                end,
                                printable-value(region2));
    check-equal(name, result, expected1);
    let result = region-contains-region?(region2, region1);
    let name = format-to-string("%= %s %=",
                                printable-value(region2),
                                case
                                  expected1 => "contains";
                                  otherwise => "does not contain";
                                end,
                                printable-value(region1));
    check-equal(name, result, expected2)
  end
end test region-contains-region-test;


// region1, region2, 1-contains-2, 2-contains-1
// region1, region2, expected result
define constant *regions-for-region-intersects-region-tests*
  = list(list(make-point(0, 0), make-point(1, 1), #f),
         list(make-point(0, 0), make-point(5, 5), #f),
         list(make-point(0, 0), make-line(0, 0, 10, 10), #t),
         list(make-point(5, 5), make-line(0, 0, 10, 10), #t),
         list(make-point(5, 5), make-line(5, 0, 5, 10), #t),
         list(make-line(0, 0, 10, 10), make-line(10, 10, 20, 20), #t),
         list(make-rectangle(0, 0, 10, 10), make-point(0, 0), #t),
         list(make-rectangle(0, 0, 10, 10), make-point(1, 1), #t),
         list(make-rectangle(0, 0, 10, 10), make-line(0, 0, 10, 10), #t),
         list(make-rectangle(0, 10, 20, 20), make-line(0, 0, 20, 30), #t),
         list(make-rectangle(0, 0, 10, 10), make-rectangle(2, 3, 6, 7), #t),
         list(make-rectangle(0, 0, 10, 10), make-rectangle(20, 20, 30, 30), #f),
         list(make-rectangle(0, 0, 10, 10), make-rectangle(10, 10, 20, 20), #t));

define test region-intersects-region-test ()
  for (x in *regions-for-region-intersects-region-tests*)
    let region1 = x[0];
    let region2 = x[1];
    let expected = x[2];
    check-equal(format-to-string("intersects?(%=, %=)",
                                 printable-value(region1),
                                 printable-value(region2)),
                region-intersects-region?(region1, region2),
                expected);
    check-equal(format-to-string("intersects?(%=, %=)",
                                 printable-value(region2),
                                 printable-value(region1)),
                region-intersects-region?(region2, region1),
                expected);
    check-true(format-to-string("%= intersects $everywhere", 
                                printable-value(region1)),
               region-intersects-region?(region1, $everywhere));
    check-false(format-to-string("%= doesn't intersect $nowhere",
                                 printable-value(region1)),
                region-intersects-region?(region1, $nowhere));
  end
end test region-intersects-region-test;


/// LTRB test harness

define method printable-value (box :: <bounding-box>) => (value)
  let (left, top, right, bottom) = box-edges(box);
  vector(floor(left), floor(top), floor(right), floor(bottom))
end method printable-value;

define method printable-value (sequence :: <sequence>) => (value)
  map-as(<vector>, printable-value, sequence)
end method printable-value;

define method expected-ltrb-equals-boxes? 
    (expected, boxes)
 => (equal? :: <boolean>)
  expected = boxes
end method expected-ltrb-equals-boxes?;

define method expected-ltrb-equals-boxes?
    (expected :: <bounding-box>, boxes :: <sequence>)
 => (equal? :: <boolean>)
  size(boxes) = 1 & boxes[0] = expected
end method expected-ltrb-equals-boxes?;

// This method has to allow for the boxes to be in a different order
define method expected-ltrb-equals-boxes? 
    (expected :: <sequence>, boxes :: <sequence>)
 => (equal? :: <boolean>)
  size(expected) = size(boxes)
    & every?(method (box)
               member?(box, boxes, test: \=)
             end,
             expected)
end method expected-ltrb-equals-boxes?;

define method ltrb-transform-result 
    (transform :: <transform>, value) => (value)
  value
end method ltrb-transform-result;

define method ltrb-transform-result
    (transform :: <transform>, box :: <bounding-box>) => (region)
  transform-region(transform, box)
end method ltrb-transform-result;

define method ltrb-transform-result 
    (transform :: <transform>, value :: <sequence>) => (result)
  map-as(<vector>, curry(ltrb-transform-result, transform), value)
end method ltrb-transform-result;

define method ltrb-apply-with-transformed-box
    (function :: <function>, transform :: <transform>,
     l1, t1, r1, b1, l2, t2, r2, b2, 
     expected)
  let box1 = make-bounding-box(l1, t1, r1, b1);
  let box2 = make-bounding-box(l2, t2, r2, b2);
  let tbox1 = transform-region(transform, box1);
  let tbox2 = transform-region(transform, box2);
  let transformed-expected = expected
                               & ltrb-transform-result(transform, expected);
  let (l1, t1, r1, b1) = box-edges(tbox1);
  let (l2, t2, r2, b2) = box-edges(tbox2);
  function(l1, t1, r1, b1, l2, t2, r2, b2, transformed-expected)
end method ltrb-apply-with-transformed-box;

define method ltrb-apply-function
    (transform :: <transform>, function :: <function>, test :: <sequence>)
  ltrb-apply-with-transformed-box
     (function, transform, 
      test[0], test[1], test[2], test[3],
      test[4], test[5], test[6], test[7],
      test[8])
end method ltrb-apply-function;

define method ltrb-apply-to-x-and-y 
    (transform :: <transform>, function :: <function>, test :: <sequence>)
  ltrb-apply-with-transformed-box
     (function, transform, 
      test[0], test[1], test[2], test[3],
      test[4], test[5], test[6], test[7],
      test[8])
  & ltrb-apply-with-transformed-box
       (function, transform,
        test[4], test[5], test[6], test[7],
        test[0], test[1], test[2], test[3],
        test[8])
end method ltrb-apply-to-x-and-y;

define variable *ltrb-transformations*
  = list($identity-transform,
         make-transform(0, 1, 1, 0, 0, 0),
         make-transform(-1, 0, 0, 1, 0, 0),
         make-transform(1, 0, 0, -1, 0, 0),
         make-translation-transform(20, 20));

define method do-ltrb-tests
    (function :: <function>, tests :: <sequence>, #key commutative? = #t) => ()
  do(method (test)
       do(rcurry(case
                   commutative? => ltrb-apply-to-x-and-y;
                   otherwise    => ltrb-apply-function;
                 end,
                 function, test),
              *ltrb-transformations*)
     end,
     tests)
end method do-ltrb-tests;


/// <or-result>
/// An object that has a number of equally valid test results

define class <or-result> (<object>)
  constant slot or-values, init-keyword: values:;
end class <or-result>;

define method or-result 
    (#rest values) => (or-result :: <or-result>)
  make(<or-result>, values: values)
end method or-result;

define method printable-value (value :: <or-result>) => (value)
  format-to-string("OR %=", 
                   map-as(<vector>, printable-value, or-values(value)))
end method printable-value;

define method expected-ltrb-equals-boxes? 
    (expected :: <or-result>, boxes) => (equal? :: <boolean>)
  any?(method (or-value)
         expected-ltrb-equals-boxes?(or-value, boxes)
       end,
       or-values(expected))
end method expected-ltrb-equals-boxes?;

define method ltrb-transform-result 
    (transform :: <transform>, value :: <or-result>)
 => (result :: <or-result>)
  make(<or-result>,
       values: map-as(<vector>,
                      curry(ltrb-transform-result, transform),
                      or-values(value)))
end method ltrb-transform-result;


/// ltrb tests

// note that the or-result value here is because there are two equally
// valid possible values for the difference, and the one that gets chosen
// depends on how the ltrbs are transformed. Also it is better to have
// all possible values known by the test, even if one of them is never
// returned with the current algorithm.
define variable *ltrb-difference-tests*
  = list(list(0, 0, 0, 0, 0, 0, 0, 0,
              #f),
         list(0, 0, 10, 10, 20, 20, 30, 30,
              make-bounding-box(0, 0, 10, 10)),
         list(10, 0, 30, 20, 20, 10, 40, 30,
              or-result(vector(make-bounding-box(10, 10, 20, 20),
                               make-bounding-box(10, 0,  30, 10)),
                        vector(make-bounding-box(20, 0,  30, 10),
                               make-bounding-box(10, 0,  20, 20)))));


define method check-ltrb-test
    (name :: <string>, l1, t1, r1, b1, l2, t2, r2, b2, value, expected) => ()
  let args = format-to-string("(%d, %d, %d, %d, %d, %d, %d, %d)",
                              floor(l1), floor(t1), floor(r1), floor(b1),
                              floor(l2), floor(t2), floor(r2), floor(b2));
  check-equal(concatenate(name, args), value, expected)
end method check-ltrb-test;

define method expected-ltrb-difference?
    (l1, t1, r1, b1, l2, t2, r2, b2, expected)
  let difference = ltrb-difference(l1, t1, r1, b1, l2, t2, r2, b2);
  check-ltrb-test("ltrb-difference", l1, t1, r1, b1, l2, t2, r2, b2,
                  expected-ltrb-equals-boxes?(expected, difference),
                  #t)
end method expected-ltrb-difference?;

define test ltrb-difference-test ()
  do-ltrb-tests(expected-ltrb-difference?, *ltrb-difference-tests*,
                commutative?: #f)
end test ltrb-difference-test;

define variable *ltrb-equals-ltrb?-tests*
  = list(list(0, 0, 0, 0, 0, 0, 0, 0, #t),
         list(100, 0, 0, 0, 0, 0, 0, 0, #f),
         list(0, 100, 0, 0, 0, 0, 0, 0, #f),
         list(0, 0, 100, 0, 0, 0, 0, 0, #f),
         list(0, 0, 0, 100, 0, 0, 0, 0, #f),
         list(0, 0, 0, 0, 100, 0, 0, 0, #f),
         list(0, 0, 0, 0, 0, 100, 0, 0, #f),
         list(0, 0, 0, 0, 0, 0, 100, 0, #f),
         list(0, 0, 0, 0, 0, 0, 0, 100, #f),
         list(100, 100, 200, 200, 100, 100, 200, 200, #t));

define test ltrb-equals-ltrb?-test ()
  for (test in *ltrb-equals-ltrb?-tests*)
    check-ltrb-test("ltrb-equals-ltrb?",
                    test[0], test[1], test[2], test[3],
                    test[4], test[5], test[6], test[7],
                    ltrb-equals-ltrb?(test[0], test[1], test[2], test[3],
                                      test[4], test[5], test[6], test[7]),
                    test[8])
  end;
end test ltrb-equals-ltrb?-test;

define variable *ltrb-intersection-tests*
  = list(list(0, 0, 0, 0, 0, 0, 0, 0, make-bounding-box(0, 0, 0, 0)),
         list(0, 0, 20, 20, 0, 0, 20, 20,
              make-bounding-box(0, 0, 20, 20)),
         list(0, 0, 40, 40, 10, 10, 30, 30,
              make-bounding-box(10, 10, 30, 30)),
         list(0, 0, 100, 100, 50, 50, 200, 200,
              make-bounding-box(50, 50, 100, 100)),
         list(0, 0, 100, 100, 200, 200, 300, 300,
              #f),
         list(0, 0, 20, 20, 10, 10, 30, 30,
              make-bounding-box(10, 10, 20, 20)),
         list(0, 10, 20, 30, 10, 0, 30, 20,
              make-bounding-box(10, 10, 20, 20)),
         list(0, 0, 40, 20, 10, 10, 30, 30,
              make-bounding-box(10, 10, 30, 20)),
         list(0, 10, 40, 30, 10, 0, 30, 20,
              make-bounding-box(10, 10, 30, 20)),
         list(0, 0, 100, 100, 200, 200, 300, 300,
              #f),
         list(0, 0, 100, 100, 100, 100, 200, 200,
              make-bounding-box(100, 100, 100, 100)));

define method expected-ltrb-intersection?
    (l1, t1, r1, b1, l2, t2, r2, b2, expected)
  let intersection = ltrb-intersection(l1, t1, r1, b1, l2, t2, r2, b2);
  check-ltrb-test("ltrb-intersection", l1, t1, r1, b1, l2, t2, r2, b2,
                  intersection, expected)
end method expected-ltrb-intersection?;

define test ltrb-intersection-test ()
  do-ltrb-tests(expected-ltrb-intersection?, *ltrb-intersection-tests*)
end test ltrb-intersection-test;

define method expected-ltrb-intersects-ltrb?
    (l1, t1, r1, b1, l2, t2, r2, b2, expected)
  let expected = if (expected) #t else #f end;
  let intersection = ltrb-intersects-ltrb?(l1, t1, r1, b1, l2, t2, r2, b2);
  check-ltrb-test("ltrb-intersects-ltrb?", l1, t1, r1, b1, l2, t2, r2, b2,
                  expected, intersection)
end method expected-ltrb-intersects-ltrb?;

define test ltrb-intersects-ltrb?-test ()
  do-ltrb-tests(expected-ltrb-intersects-ltrb?, *ltrb-intersection-tests*)
end test ltrb-intersects-ltrb?-test;


define variable *ltrb-union-tests*
  = list(list(0,0,100,100, 0, 0, 200, 200,
              make-bounding-box(0, 0, 200, 200)),
         list(0,0,100,100, 100, 100, 200, 200,
              vector(make-bounding-box(0, 0, 100, 100),
                     make-bounding-box(100, 100, 200, 200))));

define method expected-ltrb-union?
    (l1, t1, r1, b1, l2, t2, r2, b2, expected-union)
  let union = ltrb-union(l1, t1, r1, b1, l2, t2, r2, b2);
  check-ltrb-test("ltrb-union", l1, t1, r1, b1, l2, t2, r2, b2,
                  expected-ltrb-equals-boxes?(expected-union, union),
                  #t)
end method expected-ltrb-union?;

define test ltrb-union-test ()
  do-ltrb-tests(expected-ltrb-union?, *ltrb-union-tests*)
end test ltrb-union-test;

define test regions-test ()
  check-true("region-empty? on an empty region",
             region-empty?(make-bounding-box(0, 0, 0, 0)));
  check-false("region-empty? false on a non-empty region",
              region-empty?(make-bounding-box(0, 0, 100, 100)));
end test regions-test;


/// Define the regions test suite

define suite duim-regions-suite ()
  test region-equal-test;
  test region-contains-position-test;
  test region-contains-region-test;
  test region-intersects-region-test;
  test ltrb-difference-test;
  test ltrb-equals-ltrb?-test;
  test ltrb-intersection-test;
  test ltrb-intersects-ltrb?-test;
  test ltrb-union-test;
  test regions-test;
end suite duim-regions-suite;
