Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Graphic classes

define duim-graphics class-test <pixmap> ()
  //---*** Fill this in...
end class-test <pixmap>;

define duim-graphics class-test <pixmap-medium> ()
  //---*** Fill this in...
end class-test <pixmap-medium>;



/// Graphic modelling

define method record-operation 
    (medium :: <test-medium>, type, #rest args) => ()
  add!(graphic-operations(medium), apply(list, type, args))
end method record-operation;


define method draw-point
    (medium :: <test-medium>, x, y) => (record)
  record-operation(medium, #"point", x, y);
  #f
end method draw-point;

define method draw-points 
    (medium :: <test-medium>, coord-seq :: <coordinate-sequence>) => (record)
  record-operation(medium, #"points", coord-seq);
  #f
end method draw-points;

define method draw-line
    (medium :: <test-medium>, x1, y1, x2, y2) => (record)
  record-operation(medium, #"line", x1, y1, x2, y2);
  #f
end method draw-line;

define method draw-lines 
    (medium :: <test-medium>, coord-seq :: <coordinate-sequence>) => (record)
  record-operation(medium, #"lines", coord-seq);
  #f
end method draw-lines;

define method draw-rectangle
    (medium :: <test-medium>, left, top, right, bottom,
     #key filled? = #t) => (record)
  record-operation(medium, #"rectangle", left, top, right, bottom, filled?);
  #f
end method draw-rectangle;

define method draw-rectangles
    (medium :: <test-medium>, coord-seq :: <coordinate-sequence>,
     #key filled? = #t) => (record)
  record-operation(medium, #"rectangles", coord-seq, filled?);
  #f
end method draw-rectangles;

define method draw-rounded-rectangle
    (medium :: <test-medium>, left, top, right, bottom, 
     #key filled? = #t, radius) => (record)
  record-operation
    (medium, #"rectangle", left, top, right, bottom, filled?, radius);
  #f
end method draw-rounded-rectangle;

define method draw-polygon
    (medium :: <test-medium>, coord-seq :: <coordinate-sequence>,
     #key closed? = #t, filled? = #t) => (record)
  record-operation(medium, #"polygon", coord-seq, closed?, filled?);
  #f
end method draw-polygon;

define method draw-ellipse
    (medium :: <test-medium>, center-x, center-y, radius-1-dx,
     radius-1-dy, radius-2-dx, radius-2-dy,
     #key start-angle, end-angle, filled? = #t) => (record)
  record-operation(medium, #"ellipse", center-x, center-y, 
                   radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
                   start-angle | 0.0, end-angle | $2pi, filled?);
  #f
end method draw-ellipse;

define method draw-pixmap
    (medium :: <test-medium>, pixmap :: <pixmap>, x :: <real>, y :: <real>,
     #key function = $boole-1) => (record)
  ignore(function);
  record-operation(medium, #"pixmap", pixmap, x, y);
  #f
end method draw-pixmap;

define method draw-text
    (medium :: <test-medium>, string :: <string>, x, y,
     #key start, end: _end, align-x = #"left", align-y = #"baseline", do-tabs?,
          towards-x, towards-y, transform-glyphs?) => (record)
  record-operation(medium, #"text", string, x, y);
  #f
end method draw-text;

define method draw-text
    (medium :: <test-medium>, character :: <character>, x, y,
     #key start, end: _end, align-x = #"left", align-y = #"baseline", do-tabs?,
          towards-x, towards-y, transform-glyphs?) => (record)
  record-operation(medium, #"character", character, x, y);
  #f
end method draw-text;


define method start-path (medium :: <test-medium>) => (record)
  record-operation(medium, #"start-path")
end method start-path;

define method end-path (medium :: <test-medium>) => (record)
  record-operation(medium, #"end-path")
end method end-path;

define method abort-path (medium :: <test-medium>) => (record)
  record-operation(medium, #"abort-path")
end method abort-path;

define method close-path (medium :: <test-medium>) => (record)
  record-operation(medium, #"close-path")
end method close-path;

define method stroke-path (medium :: <test-medium>, #key filled?) => (record)
  ignore(filled?);
  record-operation(medium, #"stroke-path")
end method stroke-path;

define method fill-path (medium :: <test-medium>) => (record)
  record-operation(medium, #"fill-path")
end method fill-path;

define method clip-from-path
    (medium :: <test-medium>, #key function = $boole-and) => (record)
  record-operation(medium, #"clip-from-path")
end method clip-from-path;

define method save-clipping-region (medium :: <test-medium>) => (record)
  record-operation(medium, #"save-clipping-region")
end method save-clipping-region;

define method restore-clipping-region (medium :: <test-medium>) => (record)
  record-operation(medium, #"restore-clipping-region")
end method restore-clipping-region;

define method move-to (medium :: <test-medium>, x, y) => (record)
  record-operation(medium, #"move-to", x, y)
end method move-to;

define method line-to (medium :: <test-medium>, x, y) => (record)
  record-operation(medium, #"line-to", x, y)
end method line-to;

define method arc-to 
    (medium :: <test-medium>, center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #key start-angle, end-angle) => (record)
  record-operation(medium, #"arc-to", center-x, center-y,
		   radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy)
end method arc-to;

define method curve-to 
    (medium :: <test-medium>, x1, y1, x2, y2, x3, y3) => (record)
  record-operation(medium, #"curve-to", x1, y1, x2, y2, x3, y3)
end method curve-to;


/// Graphic tests

define method make-graphics-test-sheet ()
  let sheet = make-test-pane(<drawing-pane>);
  let frame = make-test-frame(<test-frame>, layout: sheet);
  sheet
end method make-graphics-test-sheet;

define method check-operation 
    (name, medium :: <test-medium>, type, #rest args)
  let operations = graphic-operations(medium);
  check(concatenate(name, " caused correct number of operations"),
        \=, size(operations), 1);
  check(name, \=,
        size(operations) > 0 & operations[0], apply(list, type, args))
end method check-operation;

define method check-operations-happened (name, medium)
  check-true(name, size(graphic-operations(medium)) > 0)
end method check-operations-happened;

define method check-ellipse-operation 
    (name, medium :: <test-medium>, type, #rest args)
  let operations = graphic-operations(medium);
  let operation = operations[0];
  check(name, \=,
        normalize-ellipse-operation(operation),
        normalize-ellipse-operation(apply(list, type, args)))
end method check-ellipse-operation;

define method mod (value, divisor)
  let (integer, modulo) = floor/(value, divisor);
  ignore(integer);
  modulo
end method mod;

define method normalize-ellipse-angle (angle)
  let new-angle = mod(angle, $pi);
  floor(new-angle * 10000)
end method normalize-ellipse-angle;

define method normalize-ellipse-operation (operation)
  if (operation[0] = #"ellipse")
    operation[7] := normalize-ellipse-angle(operation[7]);
    operation[8] := normalize-ellipse-angle(operation[8]);
  end;
  operation
end method normalize-ellipse-operation;

define method test-draw-rectangle (#key filled?)
  let sheet = make-graphics-test-sheet();
  with-sheet-medium (medium = sheet)
    let name
      = select (filled?)
          #t => "Draw Filled Rectangle";
          #f => "Draw Rectangle";
        end;
    draw-rectangle*(medium, make-point(100, 100), make-point(200, 200),
		    filled?: filled?);
    check-operation(name, medium,
                    #"rectangle", 100, 100, 200, 200, filled?);
    clear-graphic-operations(medium);
    draw-rectangle(medium, 100, 100, 200, 200, filled?: filled?);
    check-operation(concatenate("Split ", name), medium,
                    #"rectangle", 100, 100, 200, 200, filled?);
    medium
  end
end method test-draw-rectangle;

define method test-draw-rectangles (#key filled?) => ()
  let sheet = make-graphics-test-sheet();
  with-sheet-medium (medium = sheet)
    let name
      = select (filled?)
          #t => "Draw Filled Rectangles";
          #f => "Draw Rectangles";
        end;
    let points
      = vector(make-point(100, 150), make-point(200, 250),
	       make-point(300, 350), make-point(400, 450));
    draw-rectangles*(medium, points, filled?: filled?);
    check-operation(name, medium,
                    #"rectangles", 
                    vector(100, 150, 200, 250, 300, 350, 400, 450),
                    filled?);
    clear-graphic-operations(medium);
    let coordinates = vector(100, 150, 200, 250, 300, 350, 400, 450);
    draw-rectangles(medium, coordinates, filled?: filled?);
    check-operation(concatenate("Split ", name), medium,
                    #"rectangles", 
                    coordinates,
                    filled?);
  end
end method test-draw-rectangles;

define method test-polygons (#key filled?, closed?) => ()
  let sheet = make-graphics-test-sheet();
  with-sheet-medium (medium = sheet)
    let name = "Polygon";
    if (filled?) name := concatenate("Filled ", name) end;
    if (closed?) name := concatenate("Closed ", name) end; 
    draw-polygon*(medium,
		  vector(make-point(100, 100),
			 make-point(200, 200),
			 make-point(100, 300)),
		  closed?: closed?,
		  filled?: filled?);
    check-operation(name, medium,
                    #"polygon", vector(100, 100, 200, 200, 100, 300),
                    closed?, filled?);
    clear-graphic-operations(medium);
    draw-polygon(medium,
		 vector(100, 100,
			200, 200,
			100, 300),
		 closed?: closed?,
		 filled?: filled?);
    check-operation(concatenate("Split ", name), medium,
                    #"polygon", vector(100, 100, 200, 200, 100, 300),
                    closed?, filled?);
    medium
  end
end method test-polygons;

define method test-ellipses (#key filled?, start-angle, end-angle) => ()
  let sheet = make-graphics-test-sheet();
  with-sheet-medium (medium = sheet)
    let name = "Ellipse";
    if (filled?) name := concatenate("Filled ", name) end;
    if (start-angle) name := concatenate(name, " [start]") end;
    if (end-angle) name := concatenate(name, " [end]") end;
    draw-ellipse*(medium,
		  make-point(100, 100),
		  100, 150, 200, 250,
		  start-angle: start-angle,
		  end-angle: end-angle,
		  filled?: filled?);
    check-ellipse-operation(name, medium,
			    #"ellipse", 100, 100, 100, 150, 200, 250,
			    start-angle | 0.0, end-angle | $2pi, filled?);
    clear-graphic-operations(medium);
    draw-ellipse(medium,
		 100, 100,
		 100, 150, 200, 250,
		 start-angle: start-angle,
		 end-angle: end-angle,
		 filled?: filled?) ;
    check-ellipse-operation(concatenate("Split ", name), medium,
			    #"ellipse", 100, 100, 100, 150, 200, 250,
			    start-angle | 0.0, end-angle | $2pi, filled?);
    medium
  end
end method test-ellipses;

define method test-circles (#key filled?, start-angle, end-angle) => ()
  let sheet = make-graphics-test-sheet();
  with-sheet-medium (medium = sheet)
    let name = "Circle";
    if (filled?) name := concatenate("Filled ", name) end;
    if (start-angle) name := concatenate(name, " [start]") end;
    if (end-angle) name := concatenate(name, " [end]") end;
    draw-circle*(medium,
		 make-point(50, 100),
		 150,
		 start-angle: start-angle,
		 end-angle: end-angle,
		 filled?: filled?);
    check-ellipse-operation(name, medium,
			    #"ellipse", 50, 100, 150, 0, 0, 150,
			    start-angle | 0.0, end-angle | $2pi, filled?);
    clear-graphic-operations(medium);
    draw-circle(medium,
		50, 100, 150,
		start-angle: start-angle,
		end-angle: end-angle,
		filled?: filled?);
  check-ellipse-operation(concatenate("Split ", name), medium,
			  #"ellipse", 50, 100, 150, 0, 0, 150,
			  start-angle | 0.0, end-angle | $2pi, filled?);
  medium
end
  end method test-circles;

define method test-ovals (#key filled?, start-angle, end-angle) => ()
  let sheet = make-graphics-test-sheet();
  with-sheet-medium (medium = sheet)
    let name = "Ellipse";
  if (filled?) name := concatenate("Filled ", name) end;
  if (start-angle) name := concatenate(name, " [start]") end;
  if (end-angle) name := concatenate(name, " [end]") end;
  draw-oval*(medium,
	     make-point(200, 200),
	     100, 150,
	     start-angle: start-angle,
	     end-angle: end-angle,
	     filled?: filled?) ;
    check-operations-happened(name, medium);
    clear-graphic-operations(medium);
    draw-oval(medium,
	      200, 200,
	      100, 150,
	      start-angle: start-angle,
	      end-angle: end-angle,
	      filled?: filled?);
    check-operations-happened(concatenate("Split ", name), medium);
    medium
  end
end method test-ovals;

define method test-text-drawing 
    (string-or-char :: type-union(<string>, <character>)) => ()
  let sheet = make-graphics-test-sheet();
  with-sheet-medium (medium = sheet)
    let name
      = if (instance?(string-or-char, <string>))
          "Text"
        else
          "Character"
        end;
    let type
      = if (instance?(string-or-char, <string>))
          #"text"
        else
          #"character"
        end;
    draw-text*(medium, string-or-char, make-point(100, 100));
    check-operation(name, medium, type, string-or-char, 100, 100);
    clear-graphic-operations(medium);
    draw-text(medium, string-or-char, 100, 100);
    check-operation(concatenate("Split ", name), medium, type,
                    string-or-char, 100, 100);
    medium
  end
end method test-text-drawing;

define method test-pixmaps () => ()
  let sheet = make-graphics-test-sheet();
  with-sheet-medium (medium = sheet)
    let name = "Pixmap";
    let pixmap = make-pixmap(medium, 100, 100);
    draw-pixmap*(medium, pixmap, make-point(100, 100));
    check-operation(name, medium, #"pixmap", pixmap, 100, 100);
    clear-graphic-operations(medium);
    draw-pixmap(medium, pixmap, 100, 100);
    check-operation(concatenate("Split ", name), medium,
                    #"pixmap", pixmap, 100, 100);
    medium
  end
end method test-pixmaps;

define method test-arrow-drawing (name, #rest args) => ()
  let sheet = make-graphics-test-sheet();
  with-sheet-medium (medium = sheet)
    apply(draw-arrow*, medium, 
          make-point(100, 150), make-point(200, 250),
          args);
    check-operations-happened(name, medium);
    clear-graphic-operations(medium);
    apply(draw-arrow, medium, 100, 150, 200, 250, args);
    check-operations-happened(concatenate("Split ", name), medium);
    medium
  end
end method test-arrow-drawing;

define method test-triangle-drawing (name, #rest args)
  let sheet = make-graphics-test-sheet();
  with-sheet-medium (medium = sheet)
    apply(draw-triangle*, medium, 
          make-point(100, 150), make-point(200, 250), make-point(300, 350),
          args);
    check-operations-happened(name, medium);
    clear-graphic-operations(medium);
    apply(draw-triangle, medium, 100, 150, 200, 250, 300, 350, args);
    check-operations-happened(concatenate("Split ", name), medium);
    medium
  end
end method test-triangle-drawing;

define method test-bezier-curves (name, #rest args)
  let sheet = make-graphics-test-sheet();
  with-sheet-medium (medium = sheet)
    apply(draw-bezier-curve*, medium, 
          vector(make-point(100, 150), make-point(200, 250),
                 make-point(300, 350), make-point(400, 450)),
          args);
    check-operations-happened(name, medium);
    apply(draw-bezier-curve, medium,
          vector(100, 150, 200, 250, 300, 350, 400, 450),
          args);
    check-operations-happened(concatenate("Split ", name), medium);
    medium
  end
end method test-bezier-curves;

define method test-draw-regular-polygon (name, edges, #rest args)
  let sheet = make-graphics-test-sheet();
  with-sheet-medium (medium = sheet)
    apply(draw-regular-polygon*, medium, 
          make-point(100, 150), make-point(200, 250),
          edges,
          args);
    check-operations-happened(name, medium);
    apply(draw-regular-polygon, medium,
          100, 150, 200, 250, edges,
          args);
    check-operations-happened(concatenate("Split ", name), medium);
    medium
  end
end method test-draw-regular-polygon;

define method test-draw-lines ()
  let sheet = make-graphics-test-sheet();
  with-sheet-medium (medium = sheet)
    let name = "Draw Lines";
    draw-lines*(medium, 
		vector(make-point(100, 150), make-point(200, 250),
		       make-point(300, 350), make-point(400, 450)));
    check-operation(name, medium, #"lines",
                    vector(100, 150, 200, 250, 300, 350, 400, 450));
    clear-graphic-operations(medium);
    draw-lines(medium,
	       vector(100, 150, 200, 250, 300, 350, 400, 450));
    check-operation(concatenate("Split ", name), medium, #"lines",
                    vector(100, 150, 200, 250, 300, 350, 400, 450));
    medium
  end
end method test-draw-lines;

define method test-draw-points ()
  let sheet = make-graphics-test-sheet();
  with-sheet-medium (medium = sheet)
    let name = "Draw Points";
    draw-points*(medium,
		 vector(make-point(100, 150), make-point(200, 250),
			make-point(300, 350), make-point(400, 450)));
    check-operation(name, medium, #"points",
                    vector(100, 150, 200, 250, 300, 350, 400, 450));
    clear-graphic-operations(medium);
    draw-points(medium,
		vector(100, 150, 200, 250, 300, 350, 400, 450));
    check-operation(concatenate("Split ", name), medium, #"points",
                    vector(100, 150, 200, 250, 300, 350, 400, 450));
    medium
  end
end method test-draw-points;

define test simple-graphics-test ()
  let sheet = make-graphics-test-sheet();
  with-sheet-medium (medium = sheet)
    draw-point*(medium, make-point(100, 100));
    check-operation("Draw Point", medium, #"point", 100, 100);
    clear-graphic-operations(medium);
    draw-point(medium, 100, 100);
    check-operation("Split Draw Point", medium, #"point", 100, 100);
    clear-graphic-operations(medium);
    draw-line*(medium, make-point(100, 100), make-point(200, 200));
    check-operation("Draw Line", medium, #"line", 100, 100, 200, 200);
    clear-graphic-operations(medium);
    draw-line(medium, 100, 100, 200, 200);
    check-operation("Split Draw Line", medium, #"line", 100, 100, 200, 200);
    clear-graphic-operations(medium);
  end;
end test simple-graphics-test;

define test arrow-drawing-test ()
  test-arrow-drawing("Draw Arrow");
  test-arrow-drawing("Draw Reverse Arrow",
                     from-head?: #t, to-head?: #f);
  test-arrow-drawing("Draw Bi-directional Arrow",
                     from-head?: #t, to-head?: #t);
  test-arrow-drawing("Draw Long Arrow", head-length: 50);
  test-arrow-drawing("Draw Wide Arrow", head-width: 50);
end test arrow-drawing-test;

define test bezier-curve-drawing-test ()
  test-bezier-curves("Draw Bezier Curve");
  test-bezier-curves("Draw Filled Bezier Curve", filled?: #t);
end test bezier-curve-drawing-test;

define test circle-drawing-test ()
  test-circles();
  test-circles(filled?: #t);
  test-circles(start-angle: 0.0);
  test-circles(end-angle: $pi);
  test-circles(start-angle: 0.0, end-angle: $pi);
end test circle-drawing-test;

define test ellipse-drawing-test ()
  test-ellipses();
  test-ellipses(filled?: #t);
  test-ellipses(start-angle: 0.0);
  test-ellipses(end-angle: $pi);
  test-ellipses(start-angle: 0.0, end-angle: $pi);
end test ellipse-drawing-test;

define test line-drawing-test ()
  test-draw-lines();
end test line-drawing-test;

define test oval-drawing-test ()
  test-ovals();
  test-ovals(filled?: #t);
  test-ovals(start-angle: 0.0);
  test-ovals(end-angle: $pi);
  test-ovals(start-angle: 0.0, end-angle: $pi);
end test oval-drawing-test;

define test pixmap-drawing-test ()
  test-pixmaps();
end test pixmap-drawing-test;

define test points-drawing-test ()
  test-draw-points();
end test points-drawing-test;

define test polygon-drawing-test ()
  test-polygons();
  test-polygons(filled?: #t);
  test-polygons(closed?: #t);
  test-polygons(closed?: #t, filled?: #t);
end test polygon-drawing-test;

define test rectangle-drawing-test ()
  test-draw-rectangle();
  test-draw-rectangle(filled?: #t);
  test-draw-rectangles();
  test-draw-rectangles(filled?: #t);
end test rectangle-drawing-test;

define test regular-polygon-drawing-test ()
  test-draw-regular-polygon("Regular Pentagon", 5);
  test-draw-regular-polygon("Regular Hexagon", 6);
  test-draw-regular-polygon("Right-handed Regular Hexagon", 6,
                            handedness: #"right");
end test regular-polygon-drawing-test;

define test text-drawing-test ()
  test-text-drawing("Hello");
  test-text-drawing('A');
end test text-drawing-test;

define test triangle-drawing-test ()
  test-triangle-drawing("Draw Triangle", filled?: #f);
  test-triangle-drawing("Draw Filled Triangle", filled?: #t);
end test triangle-drawing-test;


/// Define the graphics test suite

define suite duim-graphics-suite ()
  test simple-graphics-test;
  test arrow-drawing-test;
  test bezier-curve-drawing-test;
  test circle-drawing-test;
  test ellipse-drawing-test;
  test line-drawing-test;
  test oval-drawing-test;
  test pixmap-drawing-test;
  test points-drawing-test;
  test polygon-drawing-test;
  test rectangle-drawing-test;
  test regular-polygon-drawing-test;
  test text-drawing-test;
  test triangle-drawing-test;
end suite duim-graphics-suite;
