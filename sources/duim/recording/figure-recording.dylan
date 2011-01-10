Module:       DUIM-Recording-Internals
Synopsis:     DUIM output recording
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Figure graphics output recording

// The base class for graphical displayed output records
// NB: we overload 'medium-state-pen' to indicate whether a figure is filled or not
define open abstract class <graphics-record> (<basic-leaf-record>)
end class <graphics-record>;

define protocol-predicate graphics-record;


/// 'draw-point'

define sealed class <point-record> (<graphics-record>)
  sealed slot %x, required-init-keyword: x:;
  sealed slot %y, required-init-keyword: y:;
end class <point-record>;

define sealed domain make (singleton(<point-record>));
define sealed domain initialize (<point-record>);

// See 'draw-line' for commentary on what exactly is happening here
define method draw-point
    (sheet :: <output-recording-mixin>, x, y) => (record)
  let record = #f;
  when (sheet-recording?(sheet))
    let medium = sheet-medium(sheet);
    let medium-state = sheet-medium-state(sheet);
    transform-coordinates!(medium-transform(medium), x, y);
    let (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
      = begin
          let thickness = pen-width(medium-pen(medium));
          fix-box(x, y, x + thickness, y + thickness)
        end;
    let (rx, ry) = point-position(sheet-output-record-position(sheet));
    let transform = make(<mutable-translation-transform>, tx: left - rx, ty: top - ry);
    let region    = make-bounding-box(0, 0, right - left, bottom - top);
    record := make(<point-record>,
		   x: x - left, y: y - top,
		   region: region, transform: transform,
		   medium-state: medium-state);
    add-output-record(sheet, record)
  end;
  when (sheet-drawing?(sheet))
    next-method()
  end;
  record
end method draw-point;

define method handle-repaint
    (record :: <point-record>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  when (default-background(record))
    repaint-background(record, medium)
  end;
  with-record-medium-state (medium, record)
    draw-point(medium, record.%x, record.%y)
  end
end method handle-repaint;


/// 'draw-points'

define sealed class <points-record> (<graphics-record>)
  sealed slot %coord-seq, required-init-keyword: coord-seq:;
end class <points-record>;

define sealed domain make (singleton(<points-record>));
define sealed domain initialize (<points-record>);

// See 'draw-lines' for commentary on what exactly is happening here
define method draw-points
    (sheet :: <output-recording-mixin>, coord-seq :: <coordinate-sequence>) => (record)
  let record = #f;
  when (sheet-recording?(sheet))
    let medium = sheet-medium(sheet);
    let medium-state = sheet-medium-state(sheet);
    let coord-seq
      = transform-coordinate-sequence
          (medium-transform(medium), coord-seq, copy?: #t);
    FOR (I FROM 0 BELOW SIZE(COORD-SEQ))	//---*** until 'coordinate-sequence-box' is fixed
      COORD-SEQ[I] := FLOOR(COORD-SEQ[I])
    END;
    let (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
      = begin
          let thickness = pen-width(medium-pen(medium));
          coordinate-sequence-box(coord-seq, thickness: thickness)
        end;
    let (rx, ry) = point-position(sheet-output-record-position(sheet));
    let transform = make(<mutable-translation-transform>, tx: left - rx, ty: top - ry);
    let region    = make-bounding-box(0, 0, right - left, bottom - top);
    translate-coordinate-sequence!(-left, -top, coord-seq);
    record := make(<points-record>,
		   coord-seq: coord-seq,
		   region: region, transform: transform,
		   medium-state: medium-state);
    add-output-record(sheet, record)
  end;
  when (sheet-drawing?(sheet))
    next-method()
  end;
  record
end method draw-points;

define method handle-repaint
    (record :: <points-record>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  when (default-background(record))
    repaint-background(record, medium)
  end;
  with-record-medium-state (medium, record)
    draw-points(medium, record.%coord-seq)
  end
end method handle-repaint;

define method refined-position-test
    (record :: <points-record>, x, y) => (true? :: <boolean>)
  block (return)
    let pen = medium-state-pen(record-medium-state(record));
    let thickness = pen-width(pen);
    local method position-in-ltrb? (px, py) => ()
	    when (ltrb-contains-position?(px - thickness, py - thickness, px, py,
					  x, y))
	      return(#t)
	    end
	  end method;
    do-coordinates(position-in-ltrb?, record.%coord-seq);
    #f
  end
end method refined-position-test;

define method highlight-output-record
    (record :: <points-record>, sheet :: <output-recording-mixin>, state) => ()
  ignore(state);
  let medium = sheet-medium(sheet);
  let pen = medium-state-pen(record-medium-state(record));
  let thickness = pen-width(pen);
  let transform = sheet-device-transform(record);
  with-drawing-options (medium, /* ---*** brush: $xor-brush, */ pen: $highlighting-pen)
    local method highlight (px, py) => ()
	    transform-coordinates!(transform, px, py);
	    draw-rectangle(medium,
			   px - thickness, py - thickness, px, py,
			   filled?: #f)
	  end method;
    do-coordinates(highlight, record.%coord-seq)
  end
end method highlight-output-record;


/// 'draw-line'

define sealed class <line-record> (<graphics-record>)
  sealed slot %x1, required-init-keyword: x1:;
  sealed slot %y1, required-init-keyword: y1:;
  sealed slot %x2, required-init-keyword: x2:;
  sealed slot %y2, required-init-keyword: y2:;
end class <line-record>;

define sealed domain make (singleton(<line-record>));
define sealed domain initialize (<line-record>);

define method draw-line
    (sheet :: <output-recording-mixin>, x1, y1, x2, y2) => (record)
  let record = #f;
  when (sheet-recording?(sheet))
    let medium = sheet-medium(sheet);
    let medium-state = sheet-medium-state(sheet);
    // Apply the medium transform to user-supplied coordinates
    transform-coordinates!(medium-transform(medium), x1, y1, x2, y2);
    // Compute the bounding box in the sheet's coordinate system
    let (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
      = begin
          let thickness = pen-width(medium-pen(medium)) - 1;
          // The convention is that we stroke paths on the outside,
          // where "outside" is biased to the left and top
          fix-box(min(x1, x2) - thickness, min(y1, y2) - thickness,
		  max(x1, x2) - thickness, max(y1, y2) - thickness)
        end;
    // Get the position of the parent record
    let (rx, ry) = point-position(sheet-output-record-position(sheet));
    // Now compute the region and transform for this record, making
    // them be 0-based to get the most efficient representation, with
    // the bounding box coordinates relative to the parent record
    let transform = make(<mutable-translation-transform>, tx: left - rx, ty: top - ry);
    let region    = make-bounding-box(0, 0, right - left, bottom - top);
    // Create the record with all user-supplied coordinates relative
    // to the bounding box
    record := make(<line-record>,
		   x1: x1 - left, y1: y1 - top, x2: x2 - left, y2: y2 - top,
		   region: region, transform: transform,
		   medium-state: medium-state);
    add-output-record(sheet, record)
  end;
  when (sheet-drawing?(sheet))
    next-method()
  end;
  record
end method draw-line;

define method handle-repaint
    (record :: <line-record>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  when (default-background(record))
    repaint-background(record, medium)
  end;
  with-record-medium-state (medium, record)
    draw-line(medium, record.%x1, record.%y1, record.%x2, record.%y2)
  end
end method handle-repaint;

define method refined-position-test
    (record :: <line-record>, x, y) => (true? :: <boolean>)
  let pen = medium-state-pen(record-medium-state(record));
  position-close-to-line?(x, y,
		          record.%x1, record.%y1, record.%x2, record.%y2,
		          thickness: pen-width(pen))
end method refined-position-test;

define method highlight-output-record
    (record :: <line-record>, sheet :: <output-recording-mixin>, state) => ()
  ignore(state);
  let medium = sheet-medium(sheet);
  let pen = medium-state-pen(record-medium-state(record));
  let transform = sheet-device-transform(record);
  let (x1, y1) = transform-position(transform, record.%x1, record.%y1);
  let (x2, y2) = transform-position(transform, record.%x2, record.%y2);
  outline-line-with-hexagon(medium, x1, y1, x2, y2,
			    thickness: pen-width(pen))
end method highlight-output-record;

define method outline-line-with-hexagon
    (drawable, x1, y1, x2, y2, #key thickness = 1)
  let distance = round/(thickness, 2) + 1;
  let (x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6)
    = case
        negative?(x2 - x1) == negative?(y2 - y1) =>
          values
            (x1 - distance, y1 - distance, x1 - distance, y1 + distance,
             x2 - distance, y2 + distance, x2 + distance, y2 + distance,
             x2 + distance, y2 - distance, x1 + distance, y1 - distance);
        otherwise =>
          when (y2 > y1)
            // Make line go down and to the right
            swap!(x2, x1);
            swap!(y2, y1)
          end;
          values
            (x1 - distance, y1 + distance, x1 - distance, y1 - distance,
             x2 - distance, y2 - distance, x2 + distance, y2 - distance,
             x2 + distance, y2 + distance, x1 + distance, y1 + distance)
      end;
  with-drawing-options (drawable, /* ---*** brush: $xor-brush, */ pen: $highlighting-pen)
    draw-line(drawable, x1, y1, x2, y2);
    draw-line(drawable, x2, y2, x3, y3);
    draw-line(drawable, x3, y3, x4, y4);
    draw-line(drawable, x4, y4, x5, y5);
    draw-line(drawable, x5, y5, x6, y6);
    draw-line(drawable, x6, y6, x1, y1)
  end
end method outline-line-with-hexagon;


// Group all the bits of the arrow together
define method draw-arrow 
    (sheet :: <output-recording-mixin>, x1, y1, x2, y2,
     #key from-head?, to-head? = #t, head-length, head-width) => (record)
  ignore(from-head?, to-head?, head-length, head-width);  
  with-new-output-record (sheet)
    next-method()
  end
end method draw-arrow;


/// 'draw-lines'

define sealed class <lines-record> (<graphics-record>)
  sealed slot %coord-seq, required-init-keyword: coord-seq:;
end class <lines-record>;

define sealed domain make (singleton(<lines-record>));
define sealed domain initialize (<lines-record>);

define method draw-lines
    (sheet :: <output-recording-mixin>, coord-seq :: <coordinate-sequence>) => (record)
  let record = #f;
  when (sheet-recording?(sheet))
    let medium = sheet-medium(sheet);
    let medium-state = sheet-medium-state(sheet);
    // Apply the medium transform to user-supplied coordinates
    let coord-seq
      = transform-coordinate-sequence
          (medium-transform(medium), coord-seq, copy?: #t);
    FOR (I FROM 0 BELOW SIZE(COORD-SEQ))	//---*** until 'coordinate-sequence-box' is fixed
      COORD-SEQ[I] := FLOOR(COORD-SEQ[I])
    END;
    // Compute the bounding box in the sheet's coordinate system
    let (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
      = begin
          let thickness = pen-width(medium-pen(medium));
          // The convention is that we stroke paths on the outside,
          // where "outside" is biased to the left and top
          coordinate-sequence-box(coord-seq, thickness: thickness)
        end;
    // Get the position of the parent record
    let (rx, ry) = point-position(sheet-output-record-position(sheet));
    // Now compute the region and transform for this record, making
    // them be 0-based to get the most efficient representation, with
    // the bounding box coordinates relative to the parent record
    let transform = make(<mutable-translation-transform>, tx: left - rx, ty: top - ry);
    let region    = make-bounding-box(0, 0, right - left, bottom - top);
    // Make all user-supplied coordinates relative to bounding box
    translate-coordinate-sequence!(-left, -top, coord-seq);
    record := make(<lines-record>,
		   coord-seq: coord-seq,
		   region: region, transform: transform,
		   medium-state: medium-state);
    add-output-record(sheet, record)
  end;
  when (sheet-drawing?(sheet))
    next-method()
  end;
  record
end method draw-lines;

define method handle-repaint
    (record :: <lines-record>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  when (default-background(record))
    repaint-background(record, medium)
  end;
  with-record-medium-state (medium, record)
    draw-lines(medium, record.%coord-seq)
  end
end method handle-repaint;

define method refined-position-test
    (record :: <lines-record>, x, y) => (true? :: <boolean>)
  block (return)
    let pen = medium-state-pen(record-medium-state(record));
    let thickness = pen-width(pen);
    local method position-in-ltrb? (x1, y1, x2, y2) => ()
	    when (ltrb-contains-position?
		    (min(x1, x2) - thickness, min(y1, y2) - thickness,
		     max(x1, x2), max(y1, y2), x, y)
		  & position-close-to-line?(x, y, x1, y1, x2, y2,
					    thickness: thickness))
	      return(#t)
	    end
	  end method;
    do-endpoint-coordinates(position-in-ltrb?, record.%coord-seq);
    #f
  end
end method refined-position-test;

define method highlight-output-record
    (record :: <lines-record>, sheet :: <output-recording-mixin>, state) => ()
  ignore(state);
  let medium = sheet-medium(sheet);
  let pen = medium-state-pen(record-medium-state(record));
  let thickness = pen-width(pen);
  let transform = sheet-device-transform(record);
  with-drawing-options (medium, /* ---*** brush: $xor-brush, */ pen: $highlighting-pen)
    local method highlight (x1, y1, x2, y2) => ()
	    transform-coordinates!(transform, x1, y1, x2, y2);
	    outline-line-with-hexagon(medium, x1, y1, x2, y2,
				      thickness: thickness)
	  end method;
    do-endpoint-coordinates(highlight, record.%coord-seq)
  end
end method highlight-output-record;


/// 'draw-rectangle'

define sealed class <rectangle-record> (<graphics-record>)
  sealed slot %x1, required-init-keyword: x1:;
  sealed slot %y1, required-init-keyword: y1:;
  sealed slot %x2, required-init-keyword: x2:;
  sealed slot %y2, required-init-keyword: y2:;
  sealed slot %filled? = #t, init-keyword: filled?:;
end class <rectangle-record>;

define sealed domain make (singleton(<rectangle-record>));
define sealed domain initialize (<rectangle-record>);

// See 'draw-line' for commentary on what exactly is happening here
define method draw-rectangle
    (sheet :: <output-recording-mixin>, x1, y1, x2, y2,
     #key filled? = #t) => (record)
  let medium = sheet-medium(sheet);
  if (~rectilinear-transform?(medium-transform(medium)))
    with-stack-vector (coords = x1, y1, x2, y1, x2, y2, x1, y2)
      draw-polygon(sheet, coords, closed?: #t, filled?: filled?)
    end
  else
    let record = #f;
    when (sheet-recording?(sheet))
      let medium-state = sheet-medium-state(sheet);
      transform-coordinates!(medium-transform(medium), x1, y1, x2, y2);
      let (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
        = begin
            let thickness
              = if (filled?) 0 else pen-width(medium-pen(medium)) end;
            fix-box(min(x1, x2) - thickness, min(y1, y2) - thickness,
		    max(x1, x2) + thickness, max(y1, y2) + thickness)
          end;
      let (rx, ry) = point-position(sheet-output-record-position(sheet));
      let transform = make(<mutable-translation-transform>, tx: left - rx, ty: top - ry);
      let region    = make-bounding-box(0, 0, right - left, bottom - top);
      record
        := make(<rectangle-record>,
		x1: x1 - left, y1: y1 - top, x2: x2 - left, y2: y2 - top,
                filled?: filled?,
		region: region, transform: transform,
                medium-state: medium-state);
      add-output-record(sheet, record)
    end;
    when (sheet-drawing?(sheet))
      next-method()
    end;
    record
  end
end method draw-rectangle;

define method handle-repaint
    (record :: <rectangle-record>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  when (default-background(record))
    repaint-background(record, medium)
  end;
  with-record-medium-state (medium, record)
    draw-rectangle(medium,
		    record.%x1, record.%y1, record.%x2, record.%y2,
		    filled?: record.%filled?)
  end
end method handle-repaint;

define method refined-position-test
    (record :: <rectangle-record>, x, y) => (true? :: <boolean>)
  record.%filled?
  | begin
      let thickness = pen-width(medium-state-pen(record-medium-state(record)));
      ~(record.%x1 + thickness <= x
        & record.%y1 + thickness <= y
        & record.%x2 - thickness >= x
        & record.%y2 - thickness >= y)
    end
end method refined-position-test;

define method highlight-output-record
    (record :: <rectangle-record>, sheet :: <output-recording-mixin>, state) => ()
  ignore(state);
  let medium = sheet-medium(sheet);
  let thickness
    = if (record.%filled?) 0 else pen-width(medium-state-pen(record-medium-state(record))) end;
  let transform = sheet-device-transform(record);
  let (x1, y1) = transform-position(transform, record.%x1, record.%y1);
  let (x2, y2) = transform-position(transform, record.%x2, record.%y2);
  with-drawing-options (medium, /* ---*** brush: $xor-brush, */ pen: $highlighting-pen)
    draw-rectangle(medium, 
		   x1 - thickness - 1, y1 - thickness - 1,
		   x2 + thickness + 1, y2 + thickness + 1,
		   filled?: #f)
  end
end method highlight-output-record;


/// 'draw-rectangles'

define sealed class <rectangles-record> (<graphics-record>)
  sealed slot %coord-seq, required-init-keyword: coord-seq:;
  sealed slot %filled? = #t, init-keyword: filled?:;
end class <rectangles-record>;

define sealed domain make (singleton(<rectangles-record>));
define sealed domain initialize (<rectangles-record>);

// See 'draw-lines' for commentary on what exactly is happening here
define method draw-rectangles
    (sheet :: <output-recording-mixin>, coord-seq :: <coordinate-sequence>,
     #key filled? = #t) => (record)
  let medium = sheet-medium(sheet);
  if (~rectilinear-transform?(medium-transform(medium)))
    do-endpoint-coordinates
      (method (x1, y1, x2, y2)
	 with-stack-vector (coords = x1, y1, x2, y1, x2, y2, x1, y2)
	   draw-polygon(sheet, coords, closed?: #t, filled?: filled?)
         end
       end,
      coord-seq)
  else
    let record = #f;
    when (sheet-recording?(sheet))
      let medium-state = sheet-medium-state(sheet);
      let coord-seq
        = transform-coordinate-sequence
            (medium-transform(medium), coord-seq, copy?: #t);
      FOR (I FROM 0 BELOW SIZE(COORD-SEQ))	//---*** until 'coordinate-sequence-box' is fixed
        COORD-SEQ[I] := FLOOR(COORD-SEQ[I])
      END;
      let (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
        = begin
            let thickness
	      = if (filled?) 0 else pen-width(medium-pen(medium)) end;
            coordinate-sequence-box(coord-seq, thickness: thickness)
          end;
      let (rx, ry) = point-position(sheet-output-record-position(sheet));
      let transform = make(<mutable-translation-transform>, tx: left - rx, ty: top - ry);
      let region    = make-bounding-box(0, 0, right - left, bottom - top);
      translate-coordinate-sequence!(-left, -top, coord-seq);
      record := make(<rectangles-record>,
		     coord-seq: coord-seq,
		     filled?: filled?,
		     region: region, transform: transform,
		     medium-state: medium-state);
      add-output-record(sheet, record)
    end;
    when (sheet-drawing?(sheet))
      next-method()
    end;
    record
  end
end method draw-rectangles;

define method handle-repaint
    (record :: <rectangles-record>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  when (default-background(record))
    repaint-background(record, medium)
  end;
  with-record-medium-state (medium, record)
    draw-rectangles(medium, record.%coord-seq, filled?: record.%filled?)
  end
end method handle-repaint;

define method refined-position-test
    (record :: <rectangles-record>, x, y) => (true? :: <boolean>)
  block (return)
    let thickness = pen-width(medium-state-pen(record-medium-state(record)));
    local method position-in-ltrb? (x1, y1, x2, y2) => ()
	    when (if (record.%filled?)
		    ltrb-contains-position?(x1, y1, x2, y2, x, y)
		  else
		    ~(x1 + thickness <= x
		      & y1 + thickness <= y
		      & x2 - thickness >= x
		      & y2 - thickness >= y)
		  end)
	      return(#t)
	    end
	  end method;
    do-endpoint-coordinates(position-in-ltrb?, record.%coord-seq);
    #f
  end
end method refined-position-test;

define method highlight-output-record
    (record :: <rectangles-record>, sheet :: <output-recording-mixin>, state) => ()
  ignore(state);
  let medium = sheet-medium(sheet);
  let thickness
    = if (record.%filled?) 0 else pen-width(medium-state-pen(record-medium-state(record))) end;
  let transform = sheet-device-transform(record);
  with-drawing-options (medium, /* ---*** brush: $xor-brush, */ pen: $highlighting-pen)
    local method highlight (x1, y1, x2, y2) => ()
	    transform-coordinates!(transform, x1, y1, x2, y2);
	    draw-rectangle(medium,
			   x1 - thickness - 1, y1 - thickness - 1,
			   x2 + thickness + 1, y2 + thickness + 1,
			   filled?: #f)
	  end method;
    do-endpoint-coordinates(highlight, record.%coord-seq)
  end
end method highlight-output-record;


/// 'draw-polygon'

define sealed class <polygon-record> (<graphics-record>)
  sealed slot %coord-seq, required-init-keyword: coord-seq:;
  sealed slot %filled? = #t, init-keyword: filled?:;
  sealed slot %closed? = #t, init-keyword: closed?:;
end class <polygon-record>;

define sealed domain make (singleton(<polygon-record>));
define sealed domain initialize (<polygon-record>);

// See 'draw-lines' for commentary on what exactly is happening here
define method draw-polygon
    (sheet :: <output-recording-mixin>, coord-seq :: <coordinate-sequence>,
     #key filled? = #t, closed? = #t) => (record)
  let record = #f;
  when (sheet-recording?(sheet))
    let medium = sheet-medium(sheet);
    let medium-state = sheet-medium-state(sheet);
    let coord-seq
      = transform-coordinate-sequence
          (medium-transform(medium), coord-seq, copy?: #t);
    FOR (I FROM 0 BELOW SIZE(COORD-SEQ))	//---*** until 'coordinate-sequence-box' is fixed
      COORD-SEQ[I] := FLOOR(COORD-SEQ[I])
    END;
    let (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
      = begin
          let thickness
	    = if (filled?) 0 else pen-width(medium-pen(medium)) end;
          coordinate-sequence-box(coord-seq, thickness: thickness)
        end;
    let (rx, ry) = point-position(sheet-output-record-position(sheet));
    let transform = make(<mutable-translation-transform>, tx: left - rx, ty: top - ry);
    let region    = make-bounding-box(0, 0, right - left, bottom - top);
    translate-coordinate-sequence!(-left, -top, coord-seq);
    record := make(<polygon-record>,
		   coord-seq: coord-seq,
		   filled?: filled?, closed?: closed?,
		   region: region, transform: transform,
		   medium-state: medium-state);
    add-output-record(sheet, record)
  end;
  when (sheet-drawing?(sheet))
    next-method()
  end;
  record
end method draw-polygon;

define method handle-repaint
    (record :: <polygon-record>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  when (default-background(record))
    repaint-background(record, medium)
  end;
  with-record-medium-state (medium, record)
    draw-polygon(medium,
		 record.%coord-seq,
		 filled?: record.%filled?, closed?: record.%closed?)
  end
end method handle-repaint;

define method refined-position-test
    (record :: <polygon-record>, x, y) => (true? :: <boolean>)
  block (return)
    if (record.%filled?)
      position-inside-polygon?(x, y, record.%coord-seq,
			       closed?: record.%closed?)
    else
      let thickness = pen-width(medium-state-pen(record-medium-state(record)));
      local method position-on-line? (x1, y1, x2, y2) => ()
	      when (ltrb-contains-position?
		      (min(x1, x2) - thickness, min(y1, y2) - thickness,
		       max(x1, x2) + thickness, max(y1, y2) + thickness, x, y)
		    & position-close-to-line?(x, y, x1, y1, x2, y2,
					      thickness: thickness))
		return(#t)
	      end
	    end method;
      let ncoords = size(record.%coord-seq) - 1;
      let x1 = record.%coord-seq[0];
      let y1 = record.%coord-seq[1];
      let x = x1;
      let y = y1;
      let i = 1;
      while (#t)
        position-on-line?(x, y,
			  x := record.%coord-seq[inc!(i)], y := record.%coord-seq[inc!(i)]);
        when (i = ncoords)
          return(when (record.%closed?)
		   position-on-line?(x, y, x1, y1)
		 end)
        end
      end
    end;
    #f
  end
end method refined-position-test;

//--- This needs a proper highlighting function
define method highlight-output-record
    (record :: <polygon-record>, sheet :: <output-recording-mixin>, state) => ()
  ignore(state);
  next-method()
end method highlight-output-record;


/// 'draw-ellipse'

define sealed class <ellipse-record> (<graphics-record>)
  sealed slot %center-x, required-init-keyword: center-x:;
  sealed slot %center-y, required-init-keyword: center-y:;
  sealed slot %radius-1-dx, required-init-keyword: radius-1-dx:;
  sealed slot %radius-1-dy, required-init-keyword: radius-1-dy:;
  sealed slot %radius-2-dx, required-init-keyword: radius-2-dx:;
  sealed slot %radius-2-dy, required-init-keyword: radius-2-dy:;
  sealed slot %start-angle = #f, init-keyword: start-angle:;
  sealed slot %end-angle = #f, init-keyword: end-angle:;
  sealed slot %filled? = #t, init-keyword: filled?:;
end class <ellipse-record>;

define sealed domain make (singleton(<ellipse-record>));
define sealed domain initialize (<ellipse-record>);

// See 'draw-line' for commentary on what exactly is happening here
define method draw-ellipse
    (sheet :: <output-recording-mixin>, center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #key start-angle, end-angle, filled? = #t) => (record)
  let record = #f;
  when (sheet-recording?(sheet))
    let medium = sheet-medium(sheet);
    let medium-state = sheet-medium-state(sheet);
    let transform = medium-transform(medium);
    transform-coordinates!(transform, center-x, center-y);
    transform-distances!(transform,
			 radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy);
    when (start-angle | end-angle)
      let (_start-angle, _end-angle)
	= transform-angles(transform, start-angle | 0.0, end-angle | $2pi);
      start-angle := _start-angle;
      end-angle := _end-angle
    end;
    let thickness
      = if (filled?) 0 else pen-width(medium-pen(medium)) end;
    let (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
      = elliptical-arc-box(center-x, center-y,
			   radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
			   start-angle: start-angle, end-angle: end-angle,
			   thickness: thickness);
    let (rx, ry) = point-position(sheet-output-record-position(sheet));
    let transform = make(<mutable-translation-transform>, tx: left - rx, ty: top - ry);
    let region    = make-bounding-box(0, 0, right - left, bottom - top);
    record := make(<ellipse-record>,
		   center-x: center-x - left, center-y: center-y - top,
		   radius-1-dx: radius-1-dx, radius-1-dy: radius-1-dy,
		   radius-2-dx: radius-2-dx, radius-2-dy: radius-2-dy,
		   start-angle: start-angle, end-angle: end-angle,
		   filled?: filled?,
		   region: region, transform: transform,
		   medium-state: medium-state);
    add-output-record(sheet, record)
  end;
  when (sheet-drawing?(sheet))
    next-method()
  end;
  record
end method draw-ellipse;

define method handle-repaint
    (record :: <ellipse-record>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  when (default-background(record))
    repaint-background(record, medium)
  end;
  with-record-medium-state (medium, record)
    draw-ellipse(medium,
		 record.%center-x, record.%center-y,
		 record.%radius-1-dx, record.%radius-1-dy,
		 record.%radius-2-dx, record.%radius-2-dy,
		 start-angle: record.%start-angle, end-angle: record.%end-angle,
		 filled?: record.%filled?)
  end
end method handle-repaint;

define method refined-position-test
    (record :: <ellipse-record>, x, y) => (true? :: <boolean>)
  if (record.%filled?)
    position-inside-ellipse?(x - record.%center-x, y - record.%center-y,
			     record.%radius-1-dx, record.%radius-1-dy,
			     record.%radius-2-dx, record.%radius-2-dy)
  else
    let thickness = pen-width(medium-state-pen(record-medium-state(record)));
    position-on-thick-ellipse?(x - record.%center-x, y - record.%center-y,
			       record.%radius-1-dx, record.%radius-1-dy,
                               record.%radius-2-dx, record.%radius-2-dy,
                               thickness: thickness)
  end
end method refined-position-test;

define method highlight-output-record
    (record :: <ellipse-record>, sheet :: <output-recording-mixin>, state) => ()
  ignore(state);
  let medium = sheet-medium(sheet);
  let delta = 2;
  let radius-1
    = sqrt(record.%radius-1-dx * record.%radius-1-dx
           + record.%radius-1-dy * record.%radius-1-dy);
  let radius-2
    = sqrt(record.%radius-2-dx * record.%radius-2-dx
           + record.%radius-2-dy * record.%radius-2-dy);
  unless (record.%filled?)
    inc!(delta, pen-width(medium-state-pen(record-medium-state(record))))
  end;
  let transform = sheet-device-transform(record);
  with-drawing-options (medium, /* ---*** brush: $xor-brush, */ pen: $highlighting-pen)
    let r1dx = record.%radius-1-dx + round/(delta * record.%radius-1-dx, radius-1);
    let r1dy = record.%radius-1-dy + round/(delta * record.%radius-1-dy, radius-1);
    let r2dx = record.%radius-2-dx + round/(delta * record.%radius-2-dx, radius-2);
    let r2dy = record.%radius-2-dy + round/(delta * record.%radius-2-dy, radius-2);
    let (cx, cy) = transform-position(transform, record.%center-x, record.%center-y);
    draw-ellipse(medium, cx, cy, r1dx, r1dy, r2dx, r2dy,
		 start-angle: record.%start-angle, end-angle: record.%end-angle,
		 filled?: #f)
  end
end method highlight-output-record;


// Group all the bits of the oval together
define method draw-oval 
    (sheet :: <output-recording-mixin>, center-x, center-y, x-radius, y-radius,
     #rest keys, #key filled? = #t, #all-keys) => (record)
  dynamic-extent(keys);
  ignore(filled?);
  with-new-output-record (sheet)
    next-method()
  end
end method draw-oval;


/// 'draw-text'

define sealed class <text-record> (<graphics-record>)
  sealed slot %text, required-init-keyword: text:;
  sealed slot %x, required-init-keyword: x:;
  sealed slot %y, required-init-keyword: y:;
end class <text-record>;

define sealed domain make (singleton(<text-record>));
define sealed domain initialize (<text-record>);

define sealed class <aligned-text-record> (<text-record>)
  sealed slot %align-x = #"left", init-keyword: align-x:;
  sealed slot %align-y = #"baseline", init-keyword: align-y:;
end class <aligned-text-record>;

define sealed domain make (singleton(<aligned-text-record>));
define sealed domain initialize (<aligned-text-record>);

define sealed class <oriented-text-record> (<aligned-text-record>)
  // Baseline runs from (X,Y) to (TOWARDS-X, TOWARDS-Y)
  sealed slot %towards-x, required-init-keyword: towards-x:;
  sealed slot %towards-y, required-init-keyword: towards-y:;
  sealed slot %transform-glyphs?, required-init-keyword: transform-glyphs?:;
end class <oriented-text-record>;

define sealed domain make (singleton(<oriented-text-record>));
define sealed domain initialize (<oriented-text-record>);

// See 'draw-line' for commentary on what exactly is happening here
define method draw-text
    (sheet :: <output-recording-mixin>, text, x, y,
     #key start: _start, end: _end, align-x, align-y, do-tabs? = #f,
          towards-x, towards-y, transform-glyphs?) => (record)
  let record = #f;
  when (sheet-recording?(sheet))
    let medium = sheet-medium(sheet);
    let medium-state = sheet-medium-state(sheet);
    // Copy iff it's a substring, otherwise ensure it's a string
    let text
      = if (_start | _end)
          copy-sequence(text, start: _start | 0, end: _end)
        else
          as(<string>, text)
        end;
    transform-coordinates!(medium-transform(medium), x, y);
    when (towards-x | towards-y)
      transform-coordinates!(medium-transform(medium), towards-x, towards-y)
    end;
    let (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
      = text-bounding-box(medium, text, x, y,
			  align-x: align-x | #"left", align-y: align-y | #"baseline",
			  do-tabs?: do-tabs?,
			  towards-x: towards-x, towards-y: towards-y,
			  transform-glyphs?: transform-glyphs?);
    when (towards-x | towards-y)
      translate-coordinates!(-left, -top, towards-x, towards-y)
    end;
    let (rx, ry) = point-position(sheet-output-record-position(sheet));
    let transform = make(<mutable-translation-transform>, tx: left - rx, ty: top - ry);
    let region    = make-bounding-box(0, 0, right - left, bottom - top);
    record := case
		towards-x | towards-y | transform-glyphs? =>
		  make(<oriented-text-record>,
		       text: text, x: x - left, y: y - top,
		       towards-x: towards-x, towards-y: towards-y,
		       transform-glyphs?: transform-glyphs?,
		       align-x: align-x | #"left", align-y: align-y | #"baseline",
		       region: region, transform: transform,
		       medium-state: medium-state);
		align-x | align-y =>
		  make(<aligned-text-record>,
		       text: text, x: x - left, y: y - top,
		       align-x: align-x | #"left", align-y: align-y | #"baseline",
		       region: region, transform: transform,
		       medium-state: medium-state);
		otherwise =>
		  make(<text-record>,
		       text: text, x: x - left, y: y - top,
		       region: region, transform: transform,
		       medium-state: medium-state)
	      end;
    add-output-record(sheet, record)
  end;
  when (sheet-drawing?(sheet))
    next-method()
  end;
  record
end method draw-text;

define method handle-repaint
    (record :: <text-record>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  when (default-background(record))
    repaint-background(record, medium)
  end;
  with-record-medium-state (state = medium, record)
    dynamic-bind (medium-merged-text-style(medium) = medium-state-text-style(state))
      draw-text(medium, record.%text, record.%x, record.%y,
		align-x: #"left", align-y: #"baseline")
    end
  end
end method handle-repaint;

define method handle-repaint
    (record :: <aligned-text-record>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  when (default-background(record))
    repaint-background(record, medium)
  end;
  with-record-medium-state (state = medium, record)
    dynamic-bind (medium-merged-text-style(medium) = medium-state-text-style(state))
      draw-text(medium,
		record.%text, record.%x, record.%y,
		align-x: record.%align-x, align-y: record.%align-y)
    end
  end
end method handle-repaint;

define method handle-repaint
    (record :: <oriented-text-record>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  when (default-background(record))
    repaint-background(record, medium)
  end;
  with-record-medium-state (state = medium, record)
    dynamic-bind (medium-merged-text-style(medium) = medium-state-text-style(state))
      draw-text(medium,
		record.%text, record.%x, record.%y,
		align-x: record.%align-x, align-y: record.%align-y,
		towards-x: record.%towards-x, towards-y: record.%towards-y,
		transform-glyphs?: record.%transform-glyphs?)
    end
  end
end method handle-repaint;

define method text-bounding-box
    (medium :: <basic-medium>, string, x, y,
     #key text-style = medium-merged-text-style(medium),
          align-x = #"left", align-y = #"baseline", do-tabs? = #f, do-newlines? = #f,
          towards-x, towards-y, transform-glyphs?)
  let (font, width, height, ascent, descent)
    = font-metrics(text-style, port(medium));
  ignore(width, height);
  let width = text-size(medium, string, text-style: text-style,
			do-newlines?: do-newlines?, do-tabs?: do-tabs?);
  let height = ascent + descent;
  let vl = #f;
  let vt = #f;
  let vr = #f;
  let vb = #f;
  select (align-x)
    #"left" =>
      vl := x;
      vr := x + width;
    #"right" =>
      vl := x - width;
      vr := x;
    #"center" =>
      vl := x - floor/(width, 2);
      vr := x + ceiling/(width, 2)
  end;
  select (align-y)
    #"baseline" =>
      vt := y - height;
      vb := y + descent;
    #"top" =>
      vt := y;
      vb := y + height;
    #"bottom" =>
      vt := y - height;
      vb := y;
    #"center" =>
      vt := y - floor/(height, 2);
      vb := y + ceiling/(height, 2)
  end;
  fix-box(vl, vt, vr, vb)
end method text-bounding-box;


/// 'draw-bezier-curve'

define sealed class <bezier-curve-record> (<graphics-record>)
  sealed slot %coord-seq, required-init-keyword: coord-seq:;
  sealed slot %filled? = #t, init-keyword: filled?:;
end class <bezier-curve-record>;

define sealed domain make (singleton(<bezier-curve-record>));
define sealed domain initialize (<bezier-curve-record>);

// See 'draw-lines' for commentary on what exactly is happening here
define method draw-bezier-curve
    (sheet :: <output-recording-mixin>, coord-seq :: <coordinate-sequence>,
     #key filled? = #t) => (record)
  let record = #f;
  when (sheet-recording?(sheet))
    let medium = sheet-medium(sheet);
    let medium-state = sheet-medium-state(sheet);
    let coord-seq
      = transform-coordinate-sequence
          (medium-transform(medium), coord-seq, copy?: #t);
    FOR (I FROM 0 BELOW SIZE(COORD-SEQ))	//---*** until 'coordinate-sequence-box' is fixed
      COORD-SEQ[I] := FLOOR(COORD-SEQ[I])
    END;
    let (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
      = begin
          let thickness
            = if (filled?) 0 else pen-width(medium-pen(medium)) end;
          coordinate-sequence-box(coord-seq, thickness: thickness)
        end;
    let (rx, ry) = point-position(sheet-output-record-position(sheet));
    let transform = make(<mutable-translation-transform>, tx: left - rx, ty: top - ry);
    let region    = make-bounding-box(0, 0, right - left, bottom - top);
    translate-coordinate-sequence!(-left, -top, coord-seq);
    record := make(<bezier-curve-record>,
		   coord-seq: coord-seq,
		   filled?: filled?,
		   region: region, transform: transform,
		   medium-state: medium-state);
    add-output-record(sheet, record)
  end;
  when (sheet-drawing?(sheet))
    next-method()
  end;
  record
end method draw-bezier-curve;

define method handle-repaint
    (record :: <bezier-curve-record>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  when (default-background(record))
    repaint-background(record, medium)
  end;
  with-record-medium-state (medium, record)
    draw-bezier-curve(medium, record.%coord-seq, filled?: record.%filled?)
  end
end method handle-repaint;


/// 'draw-image'

define sealed class <image-record> (<graphics-record>)
  sealed slot %image, required-init-keyword: image:;
  sealed slot %x, required-init-keyword: x:;
  sealed slot %y, required-init-keyword: y:;
end class <image-record>;

define sealed domain make (singleton(<image-record>));
define sealed domain initialize (<image-record>);

// See 'draw-line' for commentary on what exactly is happening here
define method draw-image
    (sheet :: <output-recording-mixin>, image :: <image>, x, y) => (record)
  let record = #f;
  when (sheet-recording?(sheet))
    let medium = sheet-medium(sheet);
    let medium-state = sheet-medium-state(sheet);
    transform-coordinates!(medium-transform(medium), x, y);
    let (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
      = fix-box(x, y, x + image-width(image), y + image-height(image));
    let (rx, ry) = point-position(sheet-output-record-position(sheet));
    let transform = make(<mutable-translation-transform>, tx: left - rx, ty: top - ry);
    let region    = make-bounding-box(0, 0, right - left, bottom - top);
    record := make(<image-record>,
		   image: image, x: x - left, y: y - top,
		   region: region, transform: transform,
		   medium-state: medium-state);
    add-output-record(sheet, record)
  end;
  when (sheet-drawing?(sheet))
    next-method()
  end;
  record
end method draw-image;

define method handle-repaint
    (record :: <image-record>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  when (default-background(record))
    repaint-background(record, medium)
  end;
  with-record-medium-state (medium, record)
    draw-image(medium, record.%image, record.%x, record.%y)
  end
end method handle-repaint;


/// 'draw-pixmap'

define sealed class <pixmap-record> (<graphics-record>)
  sealed slot %pixmap, required-init-keyword: pixmap:;
  sealed slot %x, required-init-keyword: x:;
  sealed slot %y, required-init-keyword: y:;
  sealed slot %function = $boole-1, init-keyword: function:;
end class <pixmap-record>;

define sealed domain make (singleton(<pixmap-record>));
define sealed domain initialize (<pixmap-record>);

// See 'draw-line' for commentary on what exactly is happening here
define method draw-pixmap
    (sheet :: <output-recording-mixin>, pixmap :: <pixmap>, x, y,
     #key function = $boole-1) => (record)
  let record = #f;
  when (sheet-recording?(sheet))
    let medium = sheet-medium(sheet);
    let medium-state = sheet-medium-state(sheet);
    transform-coordinates!(medium-transform(medium), x, y);
    let (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
      = fix-box(x, y, x + image-width(pixmap), y + image-height(pixmap));
    let (rx, ry) = point-position(sheet-output-record-position(sheet));
    let transform = make(<mutable-translation-transform>, tx: left - rx, ty: top - ry);
    let region    = make-bounding-box(0, 0, right - left, bottom - top);
    record := make(<pixmap-record>,
		   pixmap: pixmap, x: x - left, y: y - top,
		   function: function,
		   region: region, transform: transform,
		   medium-state: medium-state);
    add-output-record(sheet, record)
  end;
  when (sheet-drawing?(sheet))
    next-method()
  end;
  record
end method draw-pixmap;

define method handle-repaint
    (record :: <pixmap-record>, medium :: <medium>, region :: <region>) => ()
  ignore(region);
  when (default-background(record))
    repaint-background(record, medium)
  end;
  with-record-medium-state (medium, record)
    draw-pixmap(medium,
		record.%pixmap, record.%x, record.%y,
		function: record.%function)
  end
end method handle-repaint;
