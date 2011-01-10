Module:       DUIM-Recording-Internals
Synopsis:     DUIM output recording
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Sequence output record

// The Z-ordering of the children is that earlier elements are lower in
// the stack.  That is, the last (most recently added) element in '%children'
// is at the top.
define open abstract class <sequence-record> (<basic-composite-record>)
  sealed slot %children = #f;
end class <sequence-record>;

define sealed class <concrete-sequence-record> (<sequence-record>)
end class <concrete-sequence-record>;

define sealed inline method make
    (class == <sequence-record>, #rest initargs, #key, #all-keys)
 => (pane :: <concrete-sequence-record>)
  apply(make, <concrete-sequence-record>, initargs)
end method make;

define sealed domain make (singleton(<concrete-sequence-record>));
define sealed domain initialize (<concrete-sequence-record>);

define output-record-constructor <sequence-record>
    (#key children, parent, sheet, region, transform)
  children: children, parent: parent, sheet: sheet,
  region: region, transform: transform
end;

define method initialize (record :: <sequence-record>, #key sheet, children)
  ignore(sheet);
  next-method();
  when (children)
    for (child in children)
      add-child(record, child)
    end
  end
end method initialize;

define method sheet-children
    (record :: <sequence-record>) => (children :: <vector>)
  let children = record.%children;
  case
    children == #f           => #[];
    output-record?(children) => vector(children);
    otherwise                => children;
  end
end method sheet-children;

//--- Do we need a fast way to clear an output record?
define method sheet-children-setter
    (children :: <vector>, record :: <sequence-record>) => (children :: <vector>)
  until (empty?(record.%children))
    remove-child(record, record.%children[0])
  end;
  // Reset the region before adding the new children
  sheet-region(record) := set-box-edges(sheet-region(record), 0, 0, 0, 0);
  for (child in children)
    add-child(record, child);
  end;
  children
end method sheet-children-setter;

define method sheet-child-count
    (record :: <sequence-record>, #key fast?) => (count :: <integer>)
  ignore(fast?);
  let children = record.%children;
  case
    children == #f           => 0;
    output-record?(children) => 1;
    otherwise                => size(children);
  end
end method sheet-child-count;

define method sheet-element
    (record :: <sequence-record>, index) => (record :: false-or(<output-record>))
  let children = record.%children;
  case
    children == #f           => #f;
    output-record?(children) => if (zero?(index)) children else #f end;
    otherwise                => children[index];
  end
end method sheet-element;

define method do-add-child
    (record :: <sequence-record>, child :: <sheet>, #key index = #"end") => ()
  let children = record.%children;
  case
    children == #f =>
      record.%children := child;
    output-record?(children) =>
      let old-child = children;
      let new-children :: <stretchy-object-vector> = make(<stretchy-vector>);
      // Upgrade the single child to a vector, getting the order
      // of the (now two) children correct
      case
        index = #"end" | index > 0 =>
          add!(new-children, old-child);
          add!(new-children, child);
        otherwise =>
	  add!(new-children, child);
	  add!(new-children, old-child);
      end;
      record.%children := new-children;
    otherwise =>
      insert-at!(children, child, index);
  end
end method do-add-child;

define method do-remove-child
    (record :: <sequence-record>, child :: <sheet>) => ()
  let children = record.%children;
  case
    children == #f =>
      #f;
    output-record?(children) =>
      when (children == child)
	record.%children := #f
      end;
    otherwise =>
      remove!(record.%children, child);
  end
end method do-remove-child;

define method do-replace-child
    (record :: <sequence-record>, old-child :: <sheet>, new-child :: <sheet>) => ()
  substitute!(record.%children, old-child, new-child)
end method do-replace-child;


define method do-sheet-children
    (function :: <function>, record :: <sequence-record>,
     #key z-order :: <z-order> = #f) => ()
  let children = record.%children;
  case
    children == #f =>
      #f;
    output-record?(children) =>
      function(children);
    otherwise =>
      let iteration-protocol
	= if (z-order == #"top-down") top-down-iteration-protocol
	  else bottom-up-iteration-protocol end;
      for (child :: <sheet> in children using iteration-protocol)
	function(child)
      end;
  end
end method do-sheet-children;

// Breadth-first recursive...
define method do-sheet-tree
    (function :: <function>, record :: <sequence-record>) => ()
  dynamic-extent(function);
  function(record);
  let children = record.%children;
  case
    children == #f =>
      #f;
    output-record?(children) =>
      do-sheet-tree(function, children);
    otherwise =>
      without-bounds-checks
	for (i :: <integer> from 0 below size(children))
	  let child = children[i];
	  do-sheet-tree(function, child)
	end
      end;
  end
end method do-sheet-tree;


// X and Y are in the coordinate system of the sequence record
define method do-children-containing-position
    (function :: <function>, record :: <sequence-record>, x :: <real>, y :: <real>) => ()
  let children = record.%children;
  case
    children == #f =>
      #f;
    output-record?(children) =>
      let child = children;
      let (left, top, right, bottom) = box-edges(child);
      transform-coordinates!(sheet-transform(child), left, top, right, bottom);
      when (ltrb-contains-position?(left, top, right, bottom, x, y))
	function(child)
      end;
    otherwise =>
      // Walk from the top to the bottom of the Z-ordering
      without-bounds-checks
	for (i :: <integer> from size(children) - 1 to 0 by -1)
	  let child = children[i];
	  let (left, top, right, bottom) = box-edges(child);
	  transform-coordinates!(sheet-transform(child), left, top, right, bottom);
	  when (ltrb-contains-position?(left, top, right, bottom, x, y))
	    function(child)
	  end
	end
      end;
  end
end method do-children-containing-position;

// The region is in the coordinate system of the sequence record
define method do-children-overlapping-region
    (function :: <function>, record :: <sequence-record>, region :: <region>) => ()
  let children = record.%children;
  case
    children == #f =>
      #f;
    output-record?(children) =>
      let child = children;
      if (everywhere?(region))
	function(child)
      else
	let (left, top, right, bottom) = box-edges(region);
	let (cleft, ctop, cright, cbottom) = box-edges(child);
	transform-coordinates!(sheet-transform(child),
			       cleft, ctop, cright, cbottom);
	when (ltrb-intersects-ltrb?(left, top, right, bottom,
				    cleft, ctop, cright, cbottom))
	  function(child)
	end
      end;
    otherwise =>
      if (everywhere?(region))
	do(function, children)
      else
	let (left, top, right, bottom) = box-edges(region);
	// Walk from the bottom to the top of the Z-ordering
	without-bounds-checks
	  for (i :: <integer> from 0 below size(children))
	    let child = children[i];
	    let (cleft, ctop, cright, cbottom) = box-edges(child);
	    transform-coordinates!(sheet-transform(child),
				   cleft, ctop, cright, cbottom);
	    when (ltrb-intersects-ltrb?(left, top, right, bottom,
					cleft, ctop, cright, cbottom))
	      function(child)
	    end
	  end
	end
      end
  end
end method do-children-overlapping-region;

define method do-raise-sheet
    (parent :: <sequence-record>, record :: <output-record>, #key activate?) => ()
  ignore(activate?);
  remove!(parent.%children, record);
  insert-at!(parent.%children, record, #"start")
end method do-raise-sheet;

define method do-lower-sheet
    (parent :: <sequence-record>, record :: <output-record>) => ()
  remove!(parent.%children, record);
  insert-at!(parent.%children, record, #"end")
end method do-lower-sheet;


/// Sequence output history

define sealed class <sequence-output-history>
  (<output-history-mixin>, <sequence-record>)
end class <sequence-output-history>;

define sealed domain make (singleton(<sequence-output-history>));
define sealed domain initialize (<sequence-output-history>);
