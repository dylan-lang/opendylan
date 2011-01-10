Module:       DUIM-Recording-Internals
Synopsis:     DUIM output recording
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Output record element mixin

// A mixin for output records that can be stored by other output records --
// in practice, _all_ output records use this.
// Note that this must follow <composite-output-record-mixin> in any CPL.
//---*** Do we need a port/graft/sheet slot for use by gadget records, etc?
define open abstract class <output-record-element-mixin> (<abstract-output-record>)
  sealed slot sheet-parent :: false-or(<output-record>) = #f,
    init-keyword: parent:,
    setter: %parent-setter;
  sealed slot sheet-region :: <region> = make-bounding-box(0, 0, 0, 0),
    init-keyword: region:,
    setter: %region-setter;
  sealed slot sheet-transform :: <transform> = make-translation-transform(0, 0),
    init-keyword: transform:,
    setter: %transform-setter;
  // We cache the device transform, but the device region is so rarely
  // used it's not worth the extra slot
  sealed slot %device-transform :: false-or(<transform>) = #f;
  sealed slot default-background :: false-or(<ink>) = #f,
    init-keyword: background:,
    setter: %background-setter;
  sealed slot record-redisplay-state = #f;
end class <output-record-element-mixin>;

define method do-destroy-sheet (record :: <output-record-element-mixin>) => ()
  #f
end method do-destroy-sheet;

define method default-background-setter
    (background :: false-or(<ink>), record :: <output-record-element-mixin>)
 => (background :: false-or(<ink>))
  record.%background := background;
  // If we're removing background highlighting, fill in the old background by hand
  unless (background)
    let sheet = top-level-sheet(record);
    let medium = sheet-medium(sheet);
    dynamic-bind (medium-brush(medium) = medium-background(medium),
		  medium-pen(medium)   = $solid-pen,
		  medium-clipping-region(medium) = $everywhere)
      let (left, top, right, bottom) = box-edges(record);
      transform-coordinates!(sheet-device-transform(record),
			     left, top, right, bottom);
      draw-rectangle(medium, left, top, right, bottom, filled?: #t)
    end
  end;
  repaint-sheet(record, $everywhere);
  background
end method default-background-setter;

// Most 'handle-repaint' methods start with this in order to display their
// backgrounds correctly.  Note that this function gets called with the
// medium's transform set up to cause drawing to happen in the right place.
define method repaint-background
    (record :: <output-record-element-mixin>, medium :: <medium>) => ()
  dynamic-bind (medium-brush(medium) = default-background(record),
		medium-pen(medium)   = $solid-pen,
		//---*** This is not right, methinks (see 'with-record-medium-state')
		medium-clipping-region(medium) = $everywhere)
    let (left, top, right, bottom) = box-edges(record);
    draw-rectangle(medium, left, top, right, bottom, filled?: #t)
  end
end method repaint-background;


define method sheet-parent-setter
    (parent :: <output-record>, record :: <output-record-element-mixin>)
 => (parent :: <output-record>)
  add-child(parent, record);
  parent
end method sheet-parent-setter;

define method sheet-parent-setter
    (parent == #f, record :: <output-record-element-mixin>)
 => (parent :: singleton(#f))
  remove-child(sheet-parent(record), record);
  #f
end method sheet-parent-setter;

// By default, output records have no children
define method sheet-children
    (record :: <output-record-element-mixin>) => (children :: <vector>)
  #[]
end method sheet-children;

define method sheet-child-count
    (record :: <output-record-element-mixin>, #key fast?) => (count :: <integer>)
  ignore(fast?);
  0
end method sheet-child-count;


//---*** Shouldn't allow setting the region of a leaf record this way!
define method sheet-region-setter
    (region :: <region>, record :: <output-record-element-mixin>) => (region :: <region>)
  record.%region := region;
  note-region-changed(record);
  region
end method sheet-region-setter;

define method note-region-changed
    (record :: <output-record-element-mixin>) => ()
  // Don't do anything.  Let the layout protocol take care of notifying
  // our parent of relevant changes.
  #f
end method note-region-changed;

define method box-edges
    (record :: <output-record-element-mixin>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  box-edges(sheet-region(record))
end method box-edges;


define method sheet-transform-setter
    (transform :: <transform>, record :: <output-record-element-mixin>)
 => (transform :: <transform>)
  record.%transform := transform;
  record.%device-transform := #f;
  note-transform-changed(record);
  transform
end method sheet-transform-setter;

define method note-transform-changed
    (record :: <output-record-element-mixin>) => ()
  // Don't do anything.  Let the layout protocol take care of notifying
  // our parent of relevant changes.
  #f
end method note-transform-changed;


define method sheet-device-region
    (record :: <output-record>) => (region :: <region>)
  transform-region(sheet-device-transform(record), sheet-region(record))
end method sheet-device-region;

define method invalidate-cached-region (record :: <output-record>) => ()
  #f
end method invalidate-cached-region;


define method sheet-device-transform
    (record :: <output-record>) => (transform :: <transform>)
  record.%device-transform
  | begin
      let dx = 0;
      let dy = 0;
      let rec = record;
      let parent = record;
      block (return)
	while (#t)
	  parent := sheet-parent(parent);
	  unless (parent)
	    return()
	  end;
	  // If this record has a parent, transform the (dx,dy) by the
	  // sheet->parent transform
	  transform-coordinates!(sheet-transform(rec), dx, dy);
	  rec := parent
	end
      end;
      // Cache and return the result
      record.%device-transform := make-translation-transform(dx, dy)
    end
end method sheet-device-transform;

define method invalidate-cached-transform (record :: <output-record>) => ()
  record.%device-transform := #f
end method invalidate-cached-transform;


define method sheet-delta-transform
    (record :: <output-record>, ancestor :: <output-record>) => (transform :: <transform>)
  if (sheet-parent(record) == ancestor)
    sheet-transform(record)
  else
    let dx = 0;
    let dy = 0;
    let parent = record;
    block (return)
      while (#t)
        parent := sheet-parent(parent);
        unless (parent)
          error("The sheet %= has no parent", record)
        end;
        transform-coordinates!(sheet-transform(record), dx, dy);
        record := parent;
        when (record == ancestor)
          return()
        end
      end
    end;
    make-translation-transform(dx, dy)
  end
end method sheet-delta-transform;


define method do-sheet-children
    (function :: <function>, record :: <output-record>,
     #key z-order :: <z-order> = #f) => ()
  #f
end method do-sheet-children;

define method do-sheet-tree (function :: <function>, record :: <output-record>) => ()
  dynamic-extent(function);
  function(record)
end method do-sheet-tree;


// For output records, this returns the sheet that holds the output history
define method top-level-sheet
    (record :: <output-record-element-mixin>) => (sheet :: false-or(<sheet>))
  block (return)
    while (#t)
      unless (record)
        return(#f)
      end;
      when (output-history?(record))
        return(output-history-sheet(record))
      end;
      record := sheet-parent(record)
    end
  end
end method top-level-sheet;


define method sheet-attached?
    (record :: <output-record-element-mixin>)
 => (mapped? :: <boolean>)
  top-level-sheet(record) & #t
end method sheet-attached?;

define method sheet-mapped?
    (record :: <output-record-element-mixin>)
 => (mapped? :: <boolean>)
  top-level-sheet(record) & #t
end method sheet-mapped?;

define method sheet-mapped?-setter
    (mapped? :: <boolean>, record :: <output-record-element-mixin>, 
     #key do-repaint? = #t, clear? = do-repaint?)
 => (mapped? :: <boolean>)
  ignore(mapped?, do-repaint?, clear?);
  error("You can't explicitly set the 'mapped?' flag for output records")
end method sheet-mapped?-setter;


define method sheet-cursor
    (record :: <output-record-element-mixin>) => (cursor :: <cursor>)
  #"default"
end method sheet-cursor;

define method sheet-cursor-setter
    (cursor :: <cursor>, record :: <output-record-element-mixin>) => (cursor :: <cursor>)
  cursor
end method sheet-cursor-setter;


define method sheet-caret
    (record :: <output-record-element-mixin>) => (caret :: singleton(#f))
  #f
end method sheet-caret;


define method raise-sheet
    (record :: <output-record-element-mixin>, #key activate? = #t)
 => (record :: <output-record>)
  let parent = sheet-parent(record);
  when (parent)
    do-raise-sheet(parent, record, activate?: activate?)
  end;
  repaint-sheet(parent, record);
  record
end method raise-sheet;

define method lower-sheet
    (record :: <output-record-element-mixin>) => (record :: <output-record>)
  let parent = sheet-parent(record);
  when (parent)
    do-lower-sheet(parent, record)
  end;
  repaint-sheet(parent, record);
  record
end method lower-sheet;


// For specialization by graphics output records, for example
// (X,Y) is in the coordinate system of the record
define open generic refined-position-test
    (record :: <abstract-output-record>, x, y) => (true? :: <boolean>);

define method refined-position-test
    (record :: <output-record-element-mixin>, x, y) => (true? :: <boolean>)
  ignore(x, y);
  #t
end method refined-position-test;

define constant $highlighting-pen :: <standard-pen> = make(<pen>, width: 1);

define open generic highlight-output-record
    (record :: <abstract-output-record>, sheet :: <abstract-sheet>, state) => ();

// For specialization by graphics output records, for example
define method highlight-output-record
    (record :: <output-record-element-mixin>, sheet :: <output-recording-mixin>, state) => ()
  // State is HIGHLIGHT: or UNHIGHLIGHT:
  ignore(state);
  let medium = sheet-medium(sheet);
  let (left, top, right, bottom) = box-edges(record);
  //--- Would it be better to recursively maintain the proper transform
  //--- on the medium itself?  This would require 'do-highlight' methods...
  let transform = sheet-device-transform(record);
  transform-coordinates!(transform, left, top, right, bottom);
  with-drawing-options (medium, /* ---*** brush: $xor-brush, */ pen: $highlighting-pen)
    draw-rectangle(medium, left, top, right, bottom, filled?: #f)
  end
end method highlight-output-record;


/// Getting and setting output record positions

// Returns sheet's edges relative to its parent
define method sheet-edges
    (record :: <output-record-element-mixin>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let (left, top, right, bottom) = box-edges(record);
  transform-box(sheet-transform(record), left, top, right, bottom)
end method sheet-edges;

// Sets the sheet's edges.  LTRB is relative to parent
define method set-sheet-edges
    (record :: <output-record-element-mixin>,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>) => ()
  // Untransform the edges w.r.t. the parent transform
  let (left, top, right, bottom)
    = untransform-box(sheet-transform(record), left, top, right, bottom);
  // Modify the existing region (if possible) and note the change
  sheet-region(record)
    := set-box-edges(sheet-region(record), left, top, right, bottom);
  // We don't cache regions, but we have to invalidate transforms
  invalidate-cached-transforms(record)
end method set-sheet-edges;


// Returns the sheet position relative to its parent
define method sheet-position
    (record :: <output-record-element-mixin>)
 => (x :: <integer>, y :: <integer>)
  let (x, y) = box-position(sheet-region(record));
  transform-position(sheet-transform(record), x, y)
end method sheet-position;

// Sets the sheet's position by hacking its transform.  XY is relative to parent
define method set-sheet-position
    (record :: <output-record-element-mixin>, x :: <integer>, y :: <integer>) => ()
  // Modify the existing transform (if possible) and note the change.
  // Note that, since the position is encoded in both the transform and
  // the region, we need to be a bit careful
  let (old-x, old-y) = sheet-position(record);
  let dx = x - old-x;
  let dy = y - old-y;
  sheet-transform(record)
    := compose-translation-into!(dx, dy, sheet-transform(record));
  invalidate-cached-transforms(record)
end method set-sheet-position;


define method sheet-size
    (record :: <output-record-element-mixin>)
 => (width :: <integer>, height :: <integer>)
  box-size(sheet-region(record))
end method sheet-size;

define method set-sheet-size
    (record :: <output-record-element-mixin>, width :: <integer>, height :: <integer>) => ()
  let (left, top, right, bottom) = box-edges(record);
  ignore(right, bottom);
  let right  = left + width;
  let bottom = top  + height;
  // Modify the existing region (if possible) and note the change.
  // The new size gets added to the right and bottom
  sheet-region(record)
    := set-box-edges(sheet-region(record), left, top, right, bottom)
end method set-sheet-size;


/// Composite output record mixin

// Note that this must precede <output-record-element-mixin> in any CPL.
define open abstract class <composite-output-record-mixin> (<abstract-output-record>)
end class <composite-output-record-mixin>;

define method add-child
    (record :: <output-record>, child :: <output-record>, #rest keys, #key index)
 => (record :: <output-record>)
  ignore(index);
  assert(~sheet-parent(child),
         "The record %= already has a parent", child);
  apply(do-add-child, record, child, keys);
  note-child-added(record, child);
  record
end method add-child;

define method note-child-added
    (record :: <output-record>, child :: <output-record>) => ()
  note-child-added-1(record, child)
end method note-child-added;

define method note-child-added
    (record :: <composite-output-record-mixin>, child :: <output-record>) => ()
  next-method();
  update-region-for-new-child(record, child)
end method note-child-added;

define method note-child-added-1
    (record :: <output-record>, child :: <output-record>) => ()
  #f
end method note-child-added-1;

define method note-child-added-1
    (record :: <output-record>, child :: <output-record-element-mixin>) => ()
  next-method();
  child.%parent := record
end method note-child-added-1;


define method remove-child
    (record :: <output-record>, child :: <output-record>)
 => (record :: <output-record>)
  assert(sheet-parent(child) == record,
         "The sheet %= is not a child of %=", child, record);
  do-remove-child(record, child);
  note-child-removed(record, child);
  record
end method remove-child;

define method note-child-removed
    (record :: <output-record>, child :: <output-record>) => ()
  note-child-removed-1(record, child)
end method note-child-removed;

define method note-child-removed
    (record :: <composite-output-record-mixin>, child :: <output-record>) => ()
  next-method();
  let (cleft, ctop, cright, cbottom) = box-edges(child);
  transform-coordinates!(sheet-transform(child), cleft, ctop, cright, cbottom);
  update-region-for-changed-child(record, child, cleft, ctop, cright, cbottom)
end method note-child-removed;

define method note-child-removed-1
    (record :: <output-record>, child :: <output-record>) => ()
  #f
end method note-child-removed-1;

define method note-child-removed-1
    (record :: <output-record>, child :: <output-record-element-mixin>) => ()
  next-method();
  child.%parent := #f
end method note-child-removed-1;


define method replace-child
    (record :: <output-record>, old-child :: <output-record>, new-child :: <output-record>)
 => (record :: <output-record>)
  do-replace-child(record, old-child, new-child);
  note-child-removed(record, old-child);
  note-child-added(record, new-child);
  record
end method replace-child;


define method child-containing-position
    (record :: <output-record>, x :: <real>, y :: <real>)
 => (child :: false-or(<output-record>))
  block (return)
    do-children-containing-position
      (method (child) return(child) end,
       record, x, y);
    #f
  end
end method child-containing-position;

define method children-overlapping-region
    (record :: <output-record>, region :: <region>) => (children :: <sequence>)
  let children :: <stretchy-object-vector> = make(<stretchy-vector>);
  do-children-overlapping-region
    (method (child) add!(children, child) end,
     record, region);
  children
end method children-overlapping-region;


//---*** Does setting the edges of a composite need to notify any kids?
define method set-sheet-edges
    (record :: <composite-output-record-mixin>,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>) => ()
  next-method()
end method set-sheet-edges;

//---*** Does setting the edges of a composite need to notify any kids?
//---*** Yes, in order to cause gadget records to move properly...
define method set-sheet-position
    (record :: <composite-output-record-mixin>, x :: <integer>, y :: <integer>) => ()
  next-method()
end method set-sheet-position;

//---*** Does setting the size of a composite need to notify any kids?
define method set-sheet-size
    (record :: <composite-output-record-mixin>, width :: <integer>, height :: <integer>) => ()
  next-method()
end method set-sheet-size;


define method raise-sheet
    (record :: <output-record>, #key activate? = #t)
 => (record :: <output-record>)
  let parent = sheet-parent(record);
  when (parent)
    do-raise-sheet(parent, record, activate?: activate?)
  end;
  record
end method raise-sheet;

define method do-raise-sheet
    (parent :: <output-record>, record :: <output-record>, #key activate? = #t) => ()
  ignore(activate?);
  #f
end method do-raise-sheet;

define method lower-sheet (record :: <output-record>) => (record :: <output-record>)
  let parent = sheet-parent(record);
  when (parent)
    do-lower-sheet(parent, record)
  end;
  record
end method lower-sheet;

define method do-lower-sheet
    (parent :: <output-record>, record :: <output-record>) => ()
  #f
end method do-lower-sheet;


/// Repainting

// Repainting an output record first normalizes the drawing state, then
// calls 'handle-repaint'
// This could establish a clipping region for the supplied region, but I
// think it's better to be fast.  The user can easily do that himself.
define method repaint-sheet
    (record :: <output-record>, region :: <region>, #key medium, force?) => ()
  ignore(force?);
  let sheet = top-level-sheet(record);
  let medium = medium | sheet-medium(sheet);
  let region = normalize-repaint-region(region, sheet); 
  // Put a mutable transform into the sheet's medium so that the 
  // 'handle-repaint' method on composite records can be more efficient
  let (tx, ty) = transform-position(sheet-device-transform(record), 0, 0);
  let transform = make(<mutable-translation-transform>, tx: tx, ty: ty);
  dynamic-bind (medium-transform(medium) = transform)
    handle-repaint(record, medium, region)
  end;
  let _port = port(sheet);
  when (_port)
    force-display(_port)
  end
end method repaint-sheet;

// Return a new (mutable) bounding box for the region if possible,
// otherwise just return an immutable bounded region
define method normalize-repaint-region
    (region, sheet :: <sheet>) => (region :: <region>)
  case
    everywhere?(region) =>
      // If we've been asked to replay everything, set the replay
      // region to the viewport
      let region = sheet-viewport-region(sheet) | sheet-region(sheet);
      if (bounding-box?(region)) bounding-box(region) else region end;
    output-record?(region) =>
      // If the replay region is itself an output record, make a
      // new region in the proper coordinate system
      let (left, top, right, bottom) = box-edges(region);
      transform-coordinates!(sheet-device-transform(region),
			     left, top, right, bottom);
      make-bounding-box(left, top, right, bottom);
    otherwise =>
      if (bounding-box?(region)) bounding-box(region) else region end;
  end
end method normalize-repaint-region;

// All we do here is call 'handle-repaint' on the children within the region,
// keeping the medium transform set up correctly.
define method handle-repaint
    (record :: <composite-output-record-mixin>, medium :: <medium>, region :: <region>) => ()
  // If there's a non-default background, fill it in now
  when (default-background(record))
    repaint-background(record, medium)
  end;
  do-children-overlapping-region
    (method (child)
       let (cx, cy) = transform-position(sheet-transform(child), 0, 0);
       medium-transform(medium)
         := compose-translation-into!(cx, cy, medium-transform(medium));
       handle-repaint(child, medium, untransform-region(sheet-transform(child), region));
       medium-transform(medium)
         := compose-translation-into!(-cx, -cy, medium-transform(medium));
     end,
     record, region)
end method handle-repaint;

// 'normalize-repaint-region' might have given us a mutable bounding box,
// in which case we can avoid consing intermediate regions
define method handle-repaint
    (record :: <composite-output-record-mixin>, medium :: <medium>, region :: <bounding-box>) => ()
  when (default-background(record))
    repaint-background(record, medium)
  end;
  do-children-overlapping-region
    (method (child)
       let transform = sheet-transform(child);
       let (cx, cy) = transform-position(transform, 0, 0);
       medium-transform(medium)
         := compose-translation-into!(cx, cy, medium-transform(medium));
       untransform-region!(transform, region);
       handle-repaint(child, medium, region);
       transform-region!(transform, region);
       medium-transform(medium)
         := compose-translation-into!(-cx, -cy, medium-transform(medium));
     end,
     record, region)
end method handle-repaint;


/// Geometry maintenance

// Updates the region of RECORD to be large enough to hold the new child,
// recursively propagating the region expansion up the output record tree.
define method update-region-for-new-child
    (record :: <output-record-element-mixin>, child) => ()
  let (rleft, rtop, rright, rbottom) = box-edges(record);
  let (cleft, ctop, cright, cbottom) = box-edges(child);
  // Get the new child's box into the record's coordinate system
  transform-coordinates!(sheet-transform(child),
			 cleft, ctop, cright, cbottom);
  let first-child?
    = sheet-child-count(record, fast?: #t) = 1;
  let growing?
    = ~first-child?
      & ~ltrb-contains-ltrb?(rleft, rtop, rright, rbottom,
			     cleft, ctop, cright, cbottom);
  case
    first-child? =>
      // If this is the first child, we must always set the
      // bounding rectangle of the parent
      sheet-region(record)
	:= set-box-edges(sheet-region(record),
			 cleft, ctop, cright, cbottom);
    growing? =>
      // Don't bother to change the edges if we're not growing
      sheet-region(record)
	:= set-box-edges(sheet-region(record),
			 min(rleft, cleft), min(rtop, ctop),
			 max(rright, cright), max(rbottom, cbottom))
  end;
  // Inform the parent only in the case where we actually grew or
  // added the initial child
  let parent = sheet-parent(record);
  when (parent & (first-child? | growing?))
    transform-coordinates!(sheet-transform(record),
			   rleft, rtop, rright, rbottom);
    update-region-for-changed-child(parent, record,
				    rleft, rtop, rright, rbottom)
  end
end method update-region-for-new-child;

// Updates the region of RECORD to be reflect the change in the child's size,
// growing or shrinking RECORD's region as necessary, and recursively propagating
// the region change up the output record tree.
// Child's old edges are passed in parent's coordinate system because their
// reference point may have changed.
//---*** Make sure this really works!
define method update-region-for-changed-child
    (record :: <output-record-element-mixin>, child,
     old-left, old-top, old-right, old-bottom) => ()
  let (rleft, rtop, rright, rbottom) = box-edges(record);
  let (cleft, ctop, cright, cbottom) = box-edges(child);
  transform-coordinates!(sheet-transform(child),
			 cleft, ctop, cright, cbottom);
  // We must recompute the region if the child is not completely contained
  // or if it used to "define" one of the old edges.
  when (~ltrb-contains-ltrb?(rleft, rtop, rright, rbottom,
			     cleft, ctop, cright, cbottom)
	| old-left = rleft
	| old-top  = rtop
	| old-right  = rright
	| old-bottom = rbottom)
    update-region-for-changed-child-1(record)
  end
end method update-region-for-changed-child;

//---*** Make sure this really works!
define method update-region-for-changed-child-1
    (record :: <output-record-element-mixin>) => ()
  let (old-left, old-top, old-right, old-bottom) = box-edges(record);
  let once? = #f;
  let left :: <integer> = $largest-coordinate;
  let top  :: <integer> = $largest-coordinate;
  let right  :: <integer> = $smallest-coordinate;
  let bottom :: <integer> = $smallest-coordinate;
  begin
    local method compute-child-region (child)
	    let (cleft, ctop, cright, cbottom) = box-edges(child);
	    transform-coordinates!(sheet-transform(child),
				   cleft, ctop, cright, cbottom);
	    min!(left, cleft);
	    min!(top,  ctop);
	    max!(right,  cright);
	    max!(bottom, cbottom);
	    once? := #t
	  end method;
    unless (once?)
      left := 0;
      top  := 0;
      right  := 0;
      bottom := 0
    end;
    // Compute the box that will hold all of the children
    do-sheet-children(compute-child-region, record)
  end;
  //---*** Are left/top/right/bottom in the correct coordinate system here?
  sheet-region(record)
    := set-box-edges(sheet-region(record), left, top, right, bottom);
  let parent = sheet-parent(record);
  when (parent)
    // Pass these coordinates in parent's coordinate system
    transform-coordinates!(sheet-transform(record),
			   old-left, old-top, old-right, old-bottom);
    update-region-for-changed-child(parent, record,
				    old-left, old-top, old-right, old-bottom)
  end
end method update-region-for-changed-child-1;

// This is for adjusting extents after a bunch of leaves have been moved.
// It starts by recomputing all the regions _down_ the output record tree,
// then notifying _up_ the tree if there was a change.
//---*** Make sure this really works!
define method recompute-region (record :: <output-record-element-mixin>) => ()
  let (old-left, old-top, old-right, old-bottom) = box-edges(record);
  do-recompute-region(record);
  let parent = sheet-parent(record);
  when (parent)
    transform-coordinates!(sheet-transform(record),
			   old-left, old-top, old-right, old-bottom);
    update-region-for-changed-child(parent, record,
				    old-left, old-top, old-right, old-bottom)
  end
end method recompute-region;

define method do-recompute-region
    (record :: <output-record-element-mixin>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  box-edges(record)
end method do-recompute-region;

//---*** Make sure this really works!
define method do-recompute-region
    (record :: <composite-output-record-mixin>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let once? = #f;
  let left :: <integer> = $largest-coordinate;
  let top  :: <integer> = $largest-coordinate;
  let right  :: <integer> = $smallest-coordinate;
  let bottom :: <integer> = $smallest-coordinate;
  begin
    local method recompute-child-region (child)
	    let (cleft, ctop, cright, cbottom) = do-recompute-region(child);
	    transform-coordinates!(sheet-transform(child),
				   cleft, ctop, cright, cbottom);
	    min!(left, cleft);
	    min!(top,  ctop);
	    max!(right,  cright);
	    max!(bottom, cbottom);
	    once? := #t
	  end method;
    do-sheet-children(recompute-child-region, record)
  end;
  unless (once?)
    left := 0;
    top  := 0;
    right  := 0;
    bottom := 0
  end;
  //---*** Are left/top/right/bottom in the correct coordinate system here?
  sheet-region(record)
    := set-box-edges(sheet-region(record), left, top, right, bottom);
  values(left, top, right, bottom)
end method do-recompute-region;


/// Default methods for incremental redisplay

define method copy-display-state (record :: <output-record>, old-is-ok?)
  ignore(old-is-ok?);
  #f
end method copy-display-state;

define method recompute-contents-ok (record :: <output-record>)
  #f
end method recompute-contents-ok;

define method find-child-output-record
    (record :: <output-record>, use-old-children?, record-class,
     #rest initargs, #key)
  ignore(use-old-children?, record-class, initargs);
  #f
end method find-child-output-record;


/// Basic output record classes

define open abstract primary class <basic-leaf-record>
  (<output-record-element-mixin>,
   <leaf-output-record>)
  sealed slot record-medium-state :: <medium-state>,
    required-init-keyword: medium-state:;
end class <basic-leaf-record>;

define open abstract primary class <basic-composite-record>
  (<composite-output-record-mixin>,
   <output-record-element-mixin>,
   <composite-output-record>)
end class <basic-composite-record>;


define sealed class <medium-state> (<object>)
  sealed slot medium-state-brush,
    required-init-keyword: brush:;
  sealed slot medium-state-pen,
    required-init-keyword: pen:;
  sealed slot medium-state-clipping-region,
    required-init-keyword: clipping-region:;
  sealed slot medium-state-text-style,
    required-init-keyword: text-style:;
end class <medium-state>;

define sealed domain make (singleton(<medium-state>));
define sealed domain initialize (<medium-state>);

//---*** CLIM intersects the medium clipping region with the record clipping record
define macro with-record-medium-state
  { with-record-medium-state (?state:name = ?medium:expression, ?record:expression) ?:body end }
    => { begin
	   let ?state = record-medium-state(?record);
	   dynamic-bind (medium-brush(?medium) = medium-state-brush(?state),
			 medium-pen(?medium)   = medium-state-pen(?state),
			 medium-clipping-region(?medium)
			   = medium-state-clipping-region(?state))
	     ?body
	   end
         end }
  { with-record-medium-state (?medium:expression, ?record:expression) ?:body end }
    => { begin
	   let _state = record-medium-state(?record);
	   dynamic-bind (medium-brush(?medium) = medium-state-brush(_state),
			 medium-pen(?medium)   = medium-state-pen(_state),
			 medium-clipping-region(?medium)
			   = medium-state-clipping-region(_state))
	     ?body
	   end
         end }
end macro with-record-medium-state;
