Module:       DUIM-Recording-Internals
Synopsis:     DUIM output recording
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// "Coordinate sorted sets"

// '%children' kept in "lexicographic" order based on the bottom right corner
// of each child.  '%maximum-height' is the height of the tallest child.
define open abstract class <tree-record> (<basic-composite-record>)
  sealed slot %children = #f;
  sealed slot %maximum-height :: <integer> = 0;
end class <tree-record>;

define sealed class <concrete-tree-record> (<tree-record>)
end class <concrete-tree-record>;

define sealed inline method make
    (class == <tree-record>, #rest initargs, #key, #all-keys)
 => (pane :: <concrete-tree-record>)
  apply(make, <concrete-tree-record>, initargs)
end method make;

define sealed domain make (singleton(<concrete-tree-record>));
define sealed domain initialize (<concrete-tree-record>);

define output-record-constructor <tree-record>
    (#key children, parent, sheet, region, transform)
  children: children, parent: parent, sheet: sheet,
  region: region, transform: transform
end;

define method initialize (record :: <tree-record>, #key sheet, children)
  ignore(sheet);
  next-method();
  record.%children := make(<stretchy-vector>);
  when (children)
    for (child in children)
      add-child(record, child)
    end
  end
end method initialize;

define method sheet-children
    (record :: <tree-record>) => (children :: <vector>)
  record.%children
end method sheet-children;

//--- Do we need a fast way to clear an output record?
define method sheet-children-setter
    (children :: <vector>, record :: <tree-record>) => (children :: <vector>)
  until (empty?(record.%children))
    remove-child(record, record.%children[0])
  end;
  // Reset the region before adding the new children
  sheet-region(record) := set-box-edges(sheet-region(record), 0, 0, 0, 0);
  record.%maximum-height := 0;
  for (child in children)
    add-child(record, child);
  end;
  children
end method sheet-children-setter;

define method sheet-child-count
    (record :: <tree-record>, #key fast?) => (count :: <integer>)
  ignore(fast?);
  size(record.%children)
end method sheet-child-count;

define method sheet-element
    (record :: <tree-record>, index) => (record :: <output-record>)
  record.%children[index]
end method sheet-element;

define method do-add-child
    (record :: <tree-record>, child :: <sheet>,
     #key index: preferred-index = #"end") => ()
  block (return)
    let children = record.%children;
    let fp = size(children);
    max!(record.%maximum-height, box-height(child));
    let (left, top, right, bottom) = box-edges(child);
    transform-coordinates!(sheet-transform(child), left, top, right, bottom);
    // Quick check for doing output at the bottom of the window
    if (zero?(fp)
	| begin
	    let other-child = children[fp - 1];
	    when (other-child == child)
	      return()
	    end;
	    let (oleft, otop, oright, obottom) = box-edges(other-child);
	    ignore(oleft, otop);
	    transform-coordinates!(sheet-transform(other-child), oright, obottom);
	    bottom > obottom | (bottom = obottom & right >= oright)
	  end)
      add!(children, child)
    else
      let index = index-for-position(children, right, bottom);
      unless (preferred-index == #"start")
	// Make sure that the new child comes after any child it overlaps
	// so that replaying happens in the right order.
	block (break)
	  while (#t)
	    if (index < fp
		& begin
		    let other-child = children[index];
		    let (oleft, otop, oright, obottom) = box-edges(other-child);
		    transform-coordinates!(sheet-transform(other-child),
					   oleft, otop, oright, obottom);
		    ltrb-intersects-ltrb?(left, top, right, bottom,
					  oleft, otop, oright, obottom)
		  end)
	      inc!(index)
	    else
	      break()
	    end
	  end
	end
      end;
      insert-at!(children, child, index)
    end
  end
end method do-add-child;

define method do-remove-child
    (record :: <tree-record>, child :: <sheet>) => ()
  let children = record.%children;
  let index
    = index-for-child(children, child)
      // If we couldn't find it with the binary search, try
      // again the hard way.  If these things were more
      // disciplined with respect to managing overlapping
      // records, we wouldn't have to resort to this.
      | position(children, child);
  when (index)
    remove-at!(children, index)
  end
end method do-remove-child;

define method do-replace-child
    (record :: <tree-record>, old-child :: <sheet>, new-child :: <sheet>) => ()
  remove-child(record, old-child);
  add-child(record, new-child)
end method do-replace-child;

//---*** Arg! We don't have our hands on the child at this point!
// (define-method note-transform-changed
//		(child (record <tree-record>))
//   (bind ((children (slot-value record %children))
//	  (proper-index (index-for-child children child)))
//     (unless proper-index                                        ;this guy's misfiled now
//       (with-box-edges (left top right bottom) child
//	 (ignore left top)
//	 (set! proper-index (index-for-position children right bottom)))
//       (bind ((index (find-key children (curry id? child))))
//	 (if (> index proper-index)
//	     (for ((i+1 = index then i)
//		   (i = (- index 1) then (- i 1))
//		   (until (= i+1 proper-index)))
//	       (set! (element children i+1) (element children i)))
//	     (for ((i = index then i+1)
//		   (i+1 = (+ index 1) then (+ i+1 1))
//		   (until (= i proper-index)))
//	       (set! (element children i) (element children i+1))))
//	 (set! (element children proper-index) child)))))


define method do-sheet-children
    (function :: <function>, record :: <tree-record>,
     #key z-order :: <z-order> = #f) => ()
  let children = record.%children;
  let iteration-protocol
    = if (z-order == #"top-down") top-down-iteration-protocol
      else bottom-up-iteration-protocol end;
  for (child :: <sheet> in children using iteration-protocol)
    function(child)
  end
end method do-sheet-children;

// Breadth-first recursive...
define method do-sheet-tree
    (function :: <function>, record :: <tree-record>) => ()
  dynamic-extent(function);
  function(record);
  let children = record.%children;
  without-bounds-checks
    for (i :: <integer> from 0 below size(children))
      let child = children[i];
      do-sheet-tree(function, child)
    end
  end
end method do-sheet-tree;


// X and Y are in the coordinate system of the tree record
define method do-children-containing-position
    (function :: <function>, record :: <tree-record>, x :: <real>, y :: <real>) => ()
  let children = record.%children;
  let fp = size(children);
  let bound = record.%maximum-height;
  let start = index-for-position(children, 0, y + bound + 1);
  let limit = y - bound;
  block (return)
    without-bounds-checks
      for (index :: <integer> = min(fp - 1, start) then index - 1,
	   until: index < 0)
	let child = children[index];
	// Get the position into the coordinate system of the child
	let (left, top, right, bottom) = box-edges(child);
	transform-coordinates!(sheet-transform(child), left, top, right, bottom);
	when (ltrb-contains-position?(left, top, right, bottom, x, y))
	  function(child)
	end;
	when (bottom < limit)
	  return()
	end
      end
    end
  end
end method do-children-containing-position;

// The region is in the coordinate system of the tree record
define method do-children-overlapping-region
    (function :: <function>, record :: <tree-record>, region :: <region>) => ()
  let children = record.%children;
  if (everywhere?(region))
    do(function, children)
  else
    let fp = size(children);
    let (left, top, right, bottom) = box-edges(region);
    let start = index-for-position(children, 0, top);
    let limit = bottom + record.%maximum-height;
    block (return)
      without-bounds-checks
	for (index :: <integer> = start then index + 1,
	     until: index = fp)
	  let child = children[index];
	  let (cleft, ctop, cright, cbottom) = box-edges(child);
	  transform-coordinates!(sheet-transform(child),
				 cleft, ctop, cright, cbottom);
	  when (ltrb-intersects-ltrb?(left, top, right, bottom,
				      cleft, ctop, cright, cbottom))
	    function(child)
	  end;
	  when (cbottom > limit)
	    return()
	  end
	end
      end
    end
  end
end method do-children-overlapping-region;


/// Utilities...

// Like 'find-key', but searches coordinate sorted sets for a given child
define method index-for-child
    (vector :: <vector>, record :: <output-record>) => (index :: false-or(<integer>))
  block (return)
    let (left, top, right, bottom) = box-edges(record);
    transform-coordinates!(sheet-transform(record), left, top, right, bottom);
    // Binary search to find where this one goes.
    let fp = size(vector);
    let initial-index = index-for-position(vector, right, bottom);
    // Search back over things in the same place, accounting for overlap
    when (initial-index < fp)
      without-bounds-checks
	for (index :: <integer> from initial-index to 0 by -1)
	  let child = vector[index];
	  when (child == record)
	    return(index)
	  end;
	  let (cleft, ctop, cright, cbottom) = box-edges(child);
	  transform-coordinates!(sheet-transform(child),
				 cleft, ctop, cright, cbottom);
	  unless (right = cright & bottom = cbottom)
	    unless (ltrb-intersects-ltrb?(left, top, right, bottom,
					  cleft, ctop, cright, cbottom))
	      return(#f)
	    end
	  end
	end
      end
    end;
    // Search forward too, also accounting for overlap
    without-bounds-checks
      for (index :: <integer> from (if (initial-index < fp) initial-index + 1 else 0 end) below fp)
	let child = vector[index];
	when (child == record)
	  return(index)
	end;
	let (cleft, ctop, cright, cbottom) = box-edges(child);
	transform-coordinates!(sheet-transform(child),
			       cleft, ctop, cright, cbottom);
	when (cbottom > bottom)
	  unless (ltrb-intersects-ltrb?(left, top, right, bottom,
					cleft, ctop, cright, cbottom))
	    return(#f)
	  end
	end
      end
    end
  end
end method index-for-child;

// Binary search; dictionary order Y, X.
// X and Y are in the coordinate space of the tree record
define method index-for-position
    (vector :: <vector>, right, bottom) => (index :: <integer>)
  let below :: <integer> = 0;
  let above :: <integer> = size(vector);
  block (return)
    while (#t)
      when (above = below)
        return(above)
      end;
      let index :: <integer> = ash(above + below, -1);
      let child = vector[index];
      let (cleft, ctop, cright, cbottom) = box-edges(child);
      ignore(cleft, ctop);
      transform-coordinates!(sheet-transform(child), cright, cbottom);
      case
        bottom < cbottom | (bottom = cbottom & right < cright) =>
          above := index;
        bottom > cbottom | (bottom = cbottom & right > cright) =>
          if (below = index)
            return(above)
          else
            below := index
          end;
        otherwise =>
          return(index)
      end
    end
  end
end method index-for-position;


/// Tree output history

define sealed class <tree-output-history>
    (<output-history-mixin>, <tree-record>)
end class <tree-output-history>;

define sealed domain make (singleton(<tree-output-history>));
define sealed domain initialize (<tree-output-history>);
