Module:       duim-geometry-internals
Synopsis:     DUIM geometry
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Arithmetic on simple regions and region sets

/// General region union

define sealed class <region-union> (<region-set>)
  sealed constant slot %regions :: <vector>,
    required-init-keyword: regions:;
end class <region-union>;

define sealed domain make (singleton(<region-union>));
define sealed domain initialize (<region-union>);

define inline function make-region-union
    (#rest regions) => (region :: <region-union>)
  make(<region-union>, regions: as(<simple-vector>, regions))
end function make-region-union;

define sealed method region-set-function
    (region :: <region-union>) => (function)
  union
end method region-set-function;

define sealed method region-set-regions
    (region :: <region-union>, #key normalize?) => (regions :: <vector>)
  ignore(normalize?);
  region.%regions
end method region-set-regions;

define sealed method transform-region
    (transform :: <transform>, region-set :: <region-union>)
 => (region :: <region-union>)
  let regions :: <stretchy-object-vector> = make(<stretchy-vector>);
  local method do-transform (region) => ()
	  add!(regions, transform-region(transform, region))
	end method;
  do-regions(do-transform, region-set);
  make(<region-union>, regions: regions)
end method transform-region;

define method region-union
    (region :: <region>, nowhere :: <nowhere>) => (region :: <region>)
  region
end method region-union;

define method region-union
    (nowhere :: <nowhere>, region :: <region>) => (region :: <region>)
  region
end method region-union;

define method region-union 
    (everywhere :: <everywhere>, region :: <region>) => (region :: <everywhere>)
  $everywhere
end method region-union;

define method region-union
    (region :: <region>, everywhere :: <everywhere>) => (region :: <everywhere>)
  $everywhere
end method region-union;

// Take the region of maximum dimensionality
define method region-union
    (point :: <point>, path :: <path>) => (region :: <path>)
  path
end method region-union;

define method region-union
    (path :: <path>, point :: <point>) => (region :: <path>)
  path
end method region-union;

define method region-union
    (point :: <point>, area :: <area>) => (region :: <area>)
  area
end method region-union;

define method region-union
    (area :: <area>, point :: <point>) => (region :: <area>)
  area
end method region-union;

define method region-union
    (path :: <path>, area :: <area>) => (region :: <area>)
  area
end method region-union;

define method region-union
    (area :: <area>, path :: <path>) => (region :: <area>)
  area
end method region-union;

define method region-union
    (point1 :: <point>, point2 :: <point>) => (region :: <region>)
  if (region-equal(point1, point2))
    point1
  else
    make-region-union(point1, point2)
  end
end method region-union;

define method region-union
    (path1 :: <path>, path2 :: <path>) => (region :: <region>)
  case
    region-contains-region?(path1, path2) => path1;
    region-contains-region?(path2, path1) => path2;
    otherwise => make-region-union(path1, path2)
  end
end method region-union;

define method region-union
    (area1 :: <area>, area2 :: <area>) => (region :: <region>)
  case
    region-contains-region?(area1, area2) => area1;
    region-contains-region?(area2, area1) => area2;
    otherwise => make-region-union(area1, area2)
  end
end method region-union;

define method region-union
    (region1 :: <region>, region2 :: <region>) => (region :: <region-union>)
  make-region-union(region1, region2)
end method region-union;

define method region-union
    (region :: <region>, union :: <region-union>) => (region :: <region-union>)
  apply(make-region-union, region, union.%regions)
end method region-union;

define method region-union
    (union :: <region-union>, region :: <region>) => (region :: <region-union>)
  apply(make-region-union, region, union.%regions)
end method region-union;

define method region-union
    (region1 :: <region-union>, region2 :: <region-union>)
 => (region :: <region-union>)
  apply(make-region-union, concatenate(region1.%regions, region2.%regions))
end method region-union;


/// General region intersection

define sealed class <region-intersection> (<region-set>)
  sealed constant slot %regions :: <vector>,
    required-init-keyword: regions:;
end class <region-intersection>;

define sealed domain make (singleton(<region-intersection>));
define sealed domain initialize (<region-intersection>);

define inline function make-region-intersection
    (#rest regions) => (region :: <region-intersection>)
  make(<region-intersection>, regions: as(<simple-vector>, regions))
end function make-region-intersection;

define sealed method region-set-function
    (region :: <region-intersection>) => (function)
  intersection
end method region-set-function;

define sealed method region-set-regions
    (region :: <region-intersection>, #key normalize?) => (regions :: <vector>)
  ignore(normalize?);
  region.%regions
end method region-set-regions;

define sealed method transform-region
    (transform :: <transform>, region-set :: <region-intersection>)
 => (region :: <region-intersection>)
  let regions :: <stretchy-object-vector> = make(<stretchy-vector>);
  local method do-transform (region) => ()
	  add!(regions, transform-region(transform, region))
	end method;
  do-regions(do-transform, region-set);
  make(<region-intersection>, regions: regions)
end method transform-region;

define method region-intersection
    (region :: <region>, nowhere :: <nowhere>) => (region :: <nowhere>)
  $nowhere
end method region-intersection;

define method region-intersection
    (nowhere :: <nowhere>, region :: <region>) => (region :: <nowhere>)
  $nowhere
end method region-intersection;

define method region-intersection
    (everywhere :: <everywhere>, region :: <region>) => (region :: <region>)
  region
end method region-intersection;

define method region-intersection
    (region :: <region>, everywhere :: <everywhere>) => (region :: <region>)
  region
end method region-intersection;

// Take the region of minumum dimensionality
define method region-intersection
    (point :: <point>, path :: <path>) => (region :: <region>)
  if (region-intersects-region?(point, path))
    point
  else
    $nowhere
  end
end method region-intersection;

define method region-intersection
    (path :: <path>, point :: <point>) => (region :: <region>)
  if (region-intersects-region?(point, path))
    point
  else
    $nowhere
  end
end method region-intersection;

define method region-intersection
    (point :: <point>, area :: <area>) => (region :: <region>)
  if (region-intersects-region?(point, area))
    point
  else
    $nowhere
  end
end method region-intersection;

define method region-intersection
    (area :: <area>, point :: <point>) => (region :: <region>)
  if (region-intersects-region?(point, area))
    point
  else
    $nowhere
  end
end method region-intersection;

define method region-intersection
    (path :: <path>, area :: <area>) => (region :: <region>)
  if (region-intersects-region?(path, area))
    path
  else
    $nowhere
  end
end method region-intersection;

define method region-intersection
    (area :: <area>, path :: <path>) => (region :: <region>)
  if (region-intersects-region?(path, area))
    path
  else
    $nowhere
  end
end method region-intersection;

define method region-intersection
    (point1 :: <point>, point2 :: <point>) => (region :: <region>)
  if (region-equal(point1, point2))
    point1
  else
    $nowhere
  end
end method region-intersection;

// This catches paths and areas, too
define method region-intersection
    (region1 :: <region>, region2 :: <region>) => (region :: <region>)
  if (region-intersects-region?(region1, region2))
    make-region-intersection(region1, region2)
  else
    $nowhere
  end
end method region-intersection;

define method region-intersection
    (region :: <region>, intersection :: <region-intersection>) => (region :: <region-intersection>)
  apply(make-region-intersection, region, intersection.%regions)
end method region-intersection;

define method region-intersection
    (intersection :: <region-intersection>, region :: <region>) => (region :: <region-intersection>)
  apply(make-region-intersection, region, intersection.%regions)
end method region-intersection;

define method region-intersection
    (region1 :: <region-intersection>, region2 :: <region-intersection>)
 => (region :: <region-intersection>)
  apply(make-region-intersection, concatenate(region1.%regions, region2.%regions))
end method region-intersection;


/// General region difference

define sealed class <region-difference> (<region-set>)
  sealed constant slot %region1 :: <region>,
    required-init-keyword: region1:;
  sealed constant slot %region2 :: <region>,
    required-init-keyword: region2:;
  sealed slot %regions :: false-or(<vector>) = #f;
end class <region-difference>;

define sealed domain make (singleton(<region-difference>));
define sealed domain initialize (<region-difference>);

define inline function make-region-difference
    (region1, region2) => (region :: <region-difference>)
  make(<region-difference>, region1: region1, region2: region2)
end function make-region-difference;

define sealed method region-set-function
    (region :: <region-difference>) => (function)
  difference
end method region-set-function;

define sealed method region-set-regions
    (region :: <region-difference>, #key normalize?) => (regions :: <vector>)
  ignore(normalize?);
  region.%regions
  | (region.%regions := vector(region.%region1, region.%region2))
end method region-set-regions;

define sealed method do-regions
    (function :: <function>, region :: <region-difference>, #key normalize?) => ()
  ignore(normalize?);
  function(region.%region1);
  function(region.%region2)
end method do-regions;

define sealed method transform-region
    (transform :: <transform>, region-set :: <region-difference>)
 => (region :: <region-difference>)
  make-region-difference
    (transform-region(transform, region-set.%region1),
     transform-region(transform, region-set.%region2))
end method transform-region;

define method region-difference
    (nowhere :: <nowhere>, region :: <region>) => (region :: <nowhere>)
  $nowhere
end method region-difference;

define method region-difference
    (region :: <region>, nowhere :: <nowhere>) => (region :: <region>)
  region
end method region-difference;

define method region-difference
    (region :: <region>, everywhere :: <everywhere>) => (region :: <nowhere>)
  $nowhere
end method region-difference;

// For the case where the first region has higher dimensionality, the
// first region is the result.
define method region-difference
    (path :: <path>, point :: <point>) => (region :: <path>)
  path
end method region-difference;

define method region-difference
    (area :: <area>, point :: <point>) => (region :: <area>)
  area
end method region-difference;

define method region-difference
    (area :: <area>, path :: <path>) => (region :: <area>)
  area
end method region-difference;

define method region-difference
    (region1 :: <region>, region2 :: <region>) => (region :: <region-difference>)
  make-region-difference(region1, region2)
end method region-difference;


/// Simple box (LTRB) arithmetic
/// These operate only on <integer>'s, so be careful!

//--- Should we really allow zero-sized LTRBs?
define inline function ltrb-well-formed?
    (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
 => (true? :: <boolean>)
  right >= left & bottom >= top
end function ltrb-well-formed?;

define inline function ltrb-equals-ltrb? 
    (left1 :: <integer>, top1 :: <integer>, right1 :: <integer>, bottom1 :: <integer>,
     left2 :: <integer>, top2 :: <integer>, right2 :: <integer>, bottom2 :: <integer>)
 => (true? :: <boolean>)
  left1 = left2 & top1 = top2
  & right1 = right2 & bottom1 = bottom2
end function ltrb-equals-ltrb?;

define inline function ltrb-size-equal?
    (left1 :: <integer>, top1 :: <integer>, right1 :: <integer>, bottom1 :: <integer>,
     left2 :: <integer>, top2 :: <integer>, right2 :: <integer>, bottom2 :: <integer>)
 => (true? :: <boolean>)
  right1 - left1 = right2 - left2
  & bottom1 - top1 = bottom2 - top2
end function ltrb-size-equal?;

define inline function ltrb-contains-position?
    (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>,
     x :: <integer>, y :: <integer>)
 => (true? :: <boolean>)
  left <= x & top <= y
  & right >= x & bottom >= y
end function ltrb-contains-position?;

// Returns #t iff LTRB1 wholly contains LTRB2
define inline function ltrb-contains-ltrb?
    (left1 :: <integer>, top1 :: <integer>, right1 :: <integer>, bottom1 :: <integer>,
     left2 :: <integer>, top2 :: <integer>, right2 :: <integer>, bottom2 :: <integer>)
 => (true? :: <boolean>)
  left1 <= left2 & top1 <= top2
  & right1 >= right2 & bottom1 >= bottom2
end function ltrb-contains-ltrb?;

define function ltrb-intersects-ltrb? 
    (left1 :: <integer>, top1 :: <integer>, right1 :: <integer>, bottom1 :: <integer>,
     left2 :: <integer>, top2 :: <integer>, right2 :: <integer>, bottom2 :: <integer>)
 => (valid? :: <boolean>,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let left = max(left1, left2);
  let top  = max(top1, top2);
  let right  = min(right1, right2);
  let bottom = min(bottom1, bottom2);
  if (ltrb-well-formed?(left, top, right, bottom))
    values(#t, left, top, right, bottom)
  else
    values(#f, 0, 0, 0, 0)
  end
end function ltrb-intersects-ltrb?;

// Returns a sequence of bounding boxes that represent the union
define sealed method ltrb-union
    (left1 :: <integer>, top1 :: <integer>, right1 :: <integer>, bottom1 :: <integer>,
     left2 :: <integer>, top2 :: <integer>, right2 :: <integer>, bottom2 :: <integer>,
     #key banding = #"x-banding")
 => (boxes :: <vector>)
  case
    ltrb-contains-ltrb?(left1, top1, right1, bottom1,
			left2, top2, right2, bottom2) =>
      vector(make-bounding-box(left1, top1, right1, bottom1));
    ltrb-contains-ltrb?(left2, top2, right2, bottom2,
			left1, top1, right1, bottom1) =>
      vector(make-bounding-box(left2, top2, right2, bottom2));
    ~ltrb-intersects-ltrb?(left1, top1, right1, bottom1,
			   left2, top2, right2, bottom2) =>
      vector(make-bounding-box(left1, top1, right1, bottom1),
	     make-bounding-box(left2, top2, right2, bottom2));
    otherwise =>
      select (banding)
	#"x-banding" =>
	  when (abs(left2) < abs(left1))
	    swap!(left1, left2);
	    swap!(top1,  top2);
	    swap!(right1,  right2);
	    swap!(bottom1, bottom2)
	  end;
	  let result :: <stretchy-object-vector> = make(<stretchy-vector>);
	  when (top1 < top2)
	    add!(result, make-bounding-box(left1, top1, right1, top2))
	  end;
	  when (bottom2 > bottom1)
	    add!(result, make-bounding-box(left2, bottom2, right2, bottom1))
	  end;
	  when (left1 < left2)
	    let top = max(top1, top2);
	    let bottom = min(bottom1, bottom2);
	    when (bottom > top)
	      add!(result, make-bounding-box(left1, top, right2, bottom))
	    end
	  end;
	  when (right1 > right2)
	    let top = min(bottom1, bottom2);
	    let bottom = max(top1, top2);
	    when (bottom > top)
	      add!(result, make-bounding-box(left2, top, right1, bottom))
	    end
	  end;
	  result;
	#"y-banding" =>
	  when (abs(top2) < abs(top1))
	    swap!(left1, left2);
	    swap!(top1,  top2);
	    swap!(right1,  right2);
	    swap!(bottom1, bottom2)
	  end;
	  let result :: <stretchy-object-vector> = make(<stretchy-vector>);
	  when (left1 < left2)
	    add!(result, make-bounding-box(left1, top1, left2, bottom1))
	  end;
	  when (right2 > right1)
	    add!(result, make-bounding-box(right1, top2, right2, bottom2))
	  end;
	  when (top1 < top2)
	    let left = max(left1, left2);
	    let right = min(right1, right2);
	    when (right > left)
	      add!(result, make-bounding-box(left, top1, right, bottom2))
	    end
	  end;
          when (bottom1 > bottom2)
	    let left = min(right1, right2);
	    let right = max(left1, left2);
	    when (right > left)
	      add!(result, make-bounding-box(left, top2, right, bottom1))
	    end
	  end;
	  result;
	#f =>
	  vector(make-bounding-box(left1, top1, right1, bottom1),
		 make-bounding-box(left2, top2, right2, bottom2));
      end
  end
end method ltrb-union;

// Returns a single bounding box that represents the intersection, or #f.
define sealed method ltrb-intersection
    (left1 :: <integer>, top1 :: <integer>, right1 :: <integer>, bottom1 :: <integer>,
     left2 :: <integer>, top2 :: <integer>, right2 :: <integer>, bottom2 :: <integer>)
 => (box :: false-or(<bounding-box>))
  let (valid?, left, top, right, bottom)
    = ltrb-intersects-ltrb? (left1, top1, right1, bottom1,
			     left2, top2, right2, bottom2);
  when (valid?)
    make-bounding-box(left, top, right, bottom)
  end
end method ltrb-intersection;

// Returns a sequence of bounding boxes that represent the difference, or #f.
// Diagrams of box differences:
//
//     111111111111111111
//     1aaaaaaaaaaaaaaaa1
//     1aaaaaaaaaaaaaaaa1
//     1aaaaaaaaaaaaaaaa1
//     1aaaaaaaaaaaaaaaa1
//     1cccccc222222222232222222222
//     1cccccc2         1         2
//     1cccccc2         1         2
//     1cccccc2         1         2
//     111111131111111111         2
//            2                   2
//            2                   2
//            222222222222222222222
//
//
//     111111111111111111
//     1aaaaaaaaaaaaaaaa1
//     1aaaaaaaaaaaaaaaa1
//     1aaaaaaaaaaaaaaaa1
//     1aaaaaaaaaaaaaaaa1
// 2222322222222222222dd1
// 2   1             2dd1
// 2   1             2dd1
// 2   1             2dd1
// 2   1             2dd1
// 2   1             2dd1
// 2   1             2dd1
// 2222322222222222222dd1
//     1bbbbbbbbbbbbbbbb1
//     1bbbbbbbbbbbbbbbb1
//     111111111111111111
define sealed method ltrb-difference
    (left1 :: <integer>, top1 :: <integer>, right1 :: <integer>, bottom1 :: <integer>,
     left2 :: <integer>, top2 :: <integer>, right2 :: <integer>, bottom2 :: <integer>)
 => (box :: false-or(<vector>))
  // If the second ltrb contains the first ltrb, the difference is #f
  unless (ltrb-contains-ltrb?(left2, top2, right2, bottom2,
			      left1, top1, right1, bottom1))
    if (~ltrb-intersects-ltrb?(left1, top1, right1, bottom1,
			       left2, top2, right2, bottom2))
      vector(make-bounding-box(left1, top1, right1, bottom1))
    else
      let result :: <stretchy-object-vector> = make(<stretchy-vector>);
      when (top1 < top2)
	// Area A above
	add!(result, make-bounding-box(left1, top1, right1, top2))
      end;
      when (bottom1 > bottom2)
	// Area B above
	add!(result, make-bounding-box(left1, bottom2, right1, bottom1))
      end;
      when (left1 < left2)
	// Area C above
	let top = max(top1, top2);
	let bottom = min(bottom1, bottom2);
	when (bottom > top)
	  add!(result, make-bounding-box(left1, top, left2, bottom))
	end
      end;
      when (right1 > right2)
	// Area D above
	let top = max(top1, top2);
	let bottom = min(bottom1, bottom2);
	when (bottom > top)
	  add!(result, make-bounding-box(right2, top, right1, bottom))
	end
      end;
      if (empty?(result))
	#f
      else
	result
      end
    end
  end
end method ltrb-difference;


/// Special cases for sets of boxes

define sealed class <box-set> (<region-set>)
  sealed constant slot %left :: <integer>,
    required-init-keyword: left:;
  sealed constant slot %top  :: <integer>,
    required-init-keyword: top:;
  sealed constant slot %right  :: <integer>,
    required-init-keyword: right:;
  sealed constant slot %bottom :: <integer>,
    required-init-keyword: bottom:;
  sealed slot box-set-boxes :: <vector>,
    required-init-keyword: boxes:;
  sealed slot %x-banded-boxes :: false-or(<vector>) = #f;
  sealed slot %y-banded-boxes :: false-or(<vector>) = #f;
end class <box-set>;

define sealed domain make (singleton(<box-set>));
define sealed domain initialize (<box-set>);

define method make-box-set
    (#rest boxes) => (boxes :: <box-set>)
  let left :: <integer> = $largest-coordinate;
  let top  :: <integer> = $largest-coordinate;
  let right  :: <integer> = $smallest-coordinate;
  let bottom :: <integer> = $smallest-coordinate;
  for (box in boxes)
    let (rl, rt, rr, rb) = box-edges(box);
    min!(left, rl);
    min!(top, rt);
    max!(right, rr);
    max!(bottom, rb)
  end;
  make(<box-set>,
       boxes: as(<simple-vector>, boxes),
       left: left, top: top, right: right, bottom: bottom)
end method make-box-set;

define sealed inline method box-edges
    (box :: <box-set>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  values(box.%left, box.%top, box.%right, box.%bottom)
end method box-edges;

define sealed method transform-region
    (transform :: <transform>, set :: <box-set>) => (boxes :: <box-set>)
  local method do-transform (box) => ()
	  transform-region(transform, box)
	end method;
  apply(make-box-set, map-as(<simple-vector>, do-transform, box-set-boxes(set)))
end method transform-region;

define sealed method region-set-function (region :: <box-set>) => (function)
  union
end method region-set-function;

define sealed method region-set-regions
    (region :: <box-set>, #key normalize?) => (regions :: <vector>)
  select (normalize?)
    #f => box-set-boxes(region);
    #"x-banding" => region-set-x-banded-boxes(region);
    #"y-banding" => region-set-y-banded-boxes(region)
  end
end method region-set-regions;

define sealed method region-set-x-banded-boxes
    (region :: <box-set>) => (regions :: <vector>)
  region.%x-banded-boxes
  | begin
      let boxes = normalize-box-set(region, #"x-banding");
      region.%x-banded-boxes := boxes;
      boxes
    end
end method region-set-x-banded-boxes;

define sealed method region-set-y-banded-boxes
    (region :: <box-set>) => (regions :: <vector>)
  region.%y-banded-boxes
  | begin
      let boxes = normalize-box-set(region, #"y-banding");
      region.%y-banded-boxes := boxes;
      boxes
    end
end method region-set-y-banded-boxes;

define sealed method region-union
    (box1 :: <bounding-box>, box2 :: <bounding-box>) => (region :: <region>)
  let (left1, top1, right1, bottom1) = box-edges(box1);
  let (left2, top2, right2, bottom2) = box-edges(box2);
  let new-boxes = ltrb-union(left1, top1, right1, bottom1,
			     left2, top2, right2, bottom2);
  if (size(new-boxes) = 1)
    new-boxes[0]
  else
    apply(make-box-set, new-boxes)
  end
end method region-union;

define sealed method region-union
    (box :: <bounding-box>, set :: <box-set>) => (region :: <box-set>)
  apply(make-box-set, box, box-set-boxes(set))
end method region-union;

define sealed method region-union 
    (set :: <box-set>, box :: <bounding-box>) => (region :: <box-set>)
  apply(make-box-set, box, box-set-boxes(set))
end method region-union;

define sealed method region-union
    (set1 :: <box-set>, set2 :: <box-set>) => (region :: <box-set>)
  apply(make-box-set, concatenate(box-set-boxes(set1), box-set-boxes(set2)))
end method region-union;

define sealed method region-intersection
    (box1 :: <bounding-box>, box2 :: <bounding-box>) => (region :: <region>)
  let (left1, top1, right1, bottom1) = box-edges(box1);
  let (left2, top2, right2, bottom2) = box-edges(box2);
  let box = ltrb-intersection(left1, top1, right1, bottom1,
			      left2, top2, right2, bottom2);
  box | $nowhere
end method region-intersection;

define sealed method region-intersection
    (box :: <bounding-box>, set :: <box-set>) => (region :: <region>)
  let new-boxes :: <stretchy-object-vector> = make(<stretchy-vector>);
  let (left1, top1, right1, bottom1) = box-edges(box);
  local method do-intersection (b) => ()
	  let (left2, top2, right2, bottom2) = box-edges(b);
	  let new = ltrb-intersection(left1, top1, right1, bottom1,
				      left2, top2, right2, bottom2);
	  when (new)
	    add!(new-boxes, new)
	  end
	end method;
  do-regions(do-intersection, set);
  if (empty?(new-boxes))
    $nowhere
  else
    apply(make-box-set, new-boxes)
  end
end method region-intersection;

define sealed method region-intersection
    (set :: <box-set>, box :: <bounding-box>) => (region :: <region>)
  let new-boxes :: <stretchy-object-vector> = make(<stretchy-vector>);
  let (left2, top2, right2, bottom2) = box-edges(box);
  local method do-intersection (b) => ()
	  let (left1, top1, right1, bottom1) = box-edges(b);
	  let new = ltrb-intersection(left1, top1, right1, bottom1,
				      left2, top2, right2, bottom2);
	  when (new)
	    add!(new-boxes, new)
	  end
	end method;
  do-regions(do-intersection, set);
  if (empty?(new-boxes))
    $nowhere
  else
    apply(make-box-set, new-boxes)
  end
end method region-intersection;

define sealed method region-intersection
    (set1 :: <box-set>, set2 :: <box-set>) => (region :: <region>)
  let new-boxes :: <stretchy-object-vector> = make(<stretchy-vector>);
  do-regions
    (method (box1)
       do-regions
         (method (box2)
	    let (left1, top1, right1, bottom1) = box-edges(box1);
	    let (left2, top2, right2, bottom2) = box-edges(box2);
	    let new = ltrb-intersection(left1, top1, right1, bottom1,
					left2, top2, right2, bottom2);
	    when (new)
	      add!(new-boxes, new)
	    end
	  end,
	  set2)
     end,
     set1);
  if (empty?(new-boxes))
    $nowhere
  else
    apply(make-box-set, new-boxes)
  end
end method region-intersection;

define sealed method region-difference
    (box1 :: <bounding-box>, box2 :: <bounding-box>) => (region :: <region>)
  let (left1, top1, right1, bottom1) = box-edges(box1);
  let (left2, top2, right2, bottom2) = box-edges(box2);
  let new-boxes = ltrb-difference(left1, top1, right1, bottom1,
				  left2, top2, right2, bottom2);
  if (empty?(new-boxes))
    $nowhere
  else
    if (size(new-boxes) = 1)
      new-boxes[0]
    else
      apply(make-box-set, new-boxes)
    end
  end
end method region-difference;

define sealed method region-difference
    (box :: <bounding-box>, set :: <box-set>) => (region :: <region>)
  let new-boxes :: <stretchy-object-vector> = make(<stretchy-vector>);
  let (left1, top1, right1, bottom1) = box-edges(box);
  local method do-difference (b) => ()
	  let (left2, top2, right2, bottom2) = box-edges(b);
	  let new = ltrb-difference(left1, top1, right1, bottom1,
				    left2, top2, right2, bottom2);
	  when (new)
	    add!(new-boxes, new)
	  end
	end method;
  do-regions(do-difference, set);
  if (new-boxes)
    apply(make-box-set, new-boxes)
  else
    $nowhere
  end
end method region-difference;

define sealed method region-difference
    (set :: <box-set>, box :: <bounding-box>) => (region :: <region>)
  let new-boxes :: <stretchy-object-vector> = make(<stretchy-vector>);
  let (left2, top2, right2, bottom2) = box-edges(box);
  local method do-difference (b) => ()
	  let (left1, top1, right1, bottom1) = box-edges(b);
	  let new = ltrb-difference(left1, top1, right1, bottom1,
				    left2, top2, right2, bottom2);
	  when (new)
	    add!(new-boxes, new)
	  end
	end method;
    do-regions(do-difference, set);
  if (empty?(new-boxes))
    $nowhere
  else
    apply(make-box-set, new-boxes)
  end
end method region-difference;

define sealed method region-difference
    (set1 :: <box-set>, set2 :: <box-set>) => (region :: <region>)
  let new-boxes :: <stretchy-object-vector> = make(<stretchy-vector>);
  do-regions
    (method (box1)
       do-regions
         (method (box2)
	    let (left1, top1, right1, bottom1) = box-edges(box1);
	    let (left2, top2, right2, bottom2) = box-edges(box2);
	    let new = ltrb-difference(left1, top1, right1, bottom1,
				      left2, top2, right2, bottom2);
	    when (new)
	      add!(new-boxes, new)
	    end
	  end,
	  set2)
     end,
     set1);
  if (empty?(new-boxes))
    $nowhere
  else
    apply(make-box-set, new-boxes)
  end
end method region-difference;

define sealed method region-empty?
    (set :: <box-set>) => (true? :: <boolean>)
  every?(region-empty?, box-set-boxes(set))
end method region-empty?;

define sealed method normalize-box-set
    (set :: <box-set>, banding) => (boxes :: <vector>)
  local method collect-boxes (region) => (boxes)
          select (region by instance?)
            <box-set> =>
              apply(concatenate-as, <list>, map(collect-boxes, box-set-boxes(region)));
	    <bounding-box> => list(region);
	    <everywhere> => list(region);
          end
        end method,
        method reduce-boxes (pending-boxes, processed-boxes) => (boxes)
          case
            empty?(pending-boxes) =>
              processed-boxes;
            region-empty?(head(pending-boxes)) =>
              reduce-boxes(tail(pending-boxes), processed-boxes);
            otherwise =>
              let intersecting-region
                = begin
		    local method intersects? (box) => (intersects :: <boolean>)
			    region-intersects-region?(box, head(pending-boxes))
			  end method;
                    find-value(tail(pending-boxes), intersects?)
                  end;
              if (empty?(intersecting-region))
                reduce-boxes
		  (tail(pending-boxes), pair(head(pending-boxes), processed-boxes))
              else
                reduce-boxes
                  (concatenate!
                     (reduce-box-pair(head(pending-boxes), intersecting-region),
		      remove!(tail(pending-boxes), intersecting-region)),
                   processed-boxes)
	      end;
          end
        end method,
        method reduce-box-pair (box1, box2) => (boxes)
          // Don't use 'region-union', because we are only prepared
          // to deal with bounding boxes
	  let (left1, top1, right1, bottom1) = box-edges(box1);
	  let (left2, top2, right2, bottom2) = box-edges(box2);
	  remove!(ltrb-union(left1, top1, right1, bottom1,
			     left2, top2, right2, bottom2,
			     banding: banding),
		  #f,
		  test: method (_x, _y) ignore(_y); region-empty?(_x) end)
	end method;
  as(<simple-vector>, reduce-boxes(collect-boxes(set), #()))
end method normalize-box-set;
