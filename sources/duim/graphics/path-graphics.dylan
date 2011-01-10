Module:       duim-graphics-internals
Synopsis:     DUIM graphics
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Path-based Graphics

define protocol <<path-graphics-protocol>> ()
  // API level functions
  function start-path (drawable :: <drawable>) => (record);
  function end-path (drawable :: <drawable>) => (record);
  function close-path (drawable :: <drawable>) => (record);
  function abort-path (drawable :: <drawable>) => (record);
  function stroke-path (drawable :: <drawable>, #key filled?) => (record);
  function fill-path (drawable :: <drawable>) => (record);
  function clip-from-path (drawable :: <drawable>, #key function) => (record);
  function save-clipping-region (drawable :: <drawable>) => (record);
  function restore-clipping-region (drawable :: <drawable>) => (record);
  function move-to (drawable :: <drawable>, x, y) => (record);
  function line-to (drawable :: <drawable>, x, y) => (record);
  function arc-to (drawable :: <drawable>, center-x, center-y,
		   radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
		   #key start-angle, end-angle) => (record);
  function curve-to (drawable :: <drawable>, x1, y1, x2, y2, x3, y3) => (record);
end protocol <<path-graphics-protocol>>;


/// START-PATH

define method start-path
    (sheet :: <basic-sheet>) => (record)
  with-sheet-medium (medium = sheet)
    start-path(medium)
  end
end method start-path;

define method start-path
    (sheet :: <permanent-medium-mixin>) => (record)
  start-path(sheet-medium(sheet))
end method start-path;


/// END-PATH

define method end-path
    (sheet :: <basic-sheet>) => (record)
  with-sheet-medium (medium = sheet)
    end-path(medium)
  end
end method end-path;

define method end-path
    (sheet :: <permanent-medium-mixin>) => (record)
  end-path(sheet-medium(sheet))
end method end-path;


/// ABORT-PATH

define method abort-path
    (sheet :: <basic-sheet>) => (record)
  with-sheet-medium (medium = sheet)
    abort-path(medium)
  end
end method abort-path;

define method abort-path
    (sheet :: <permanent-medium-mixin>) => (record)
  abort-path(sheet-medium(sheet))
end method abort-path;


/// CLOSE-PATH

define method close-path
    (sheet :: <basic-sheet>) => (record)
  with-sheet-medium (medium = sheet)
    close-path(medium)
  end
end method close-path;

define method close-path
    (sheet :: <permanent-medium-mixin>) => (record)
  close-path(sheet-medium(sheet))
end method close-path;


/// STROKE-PATH

define method stroke-path
    (sheet :: <basic-sheet>, #key filled?) => (record)
  with-sheet-medium (medium = sheet)
    stroke-path(medium, filled?: filled?)
  end
end method stroke-path;

define method stroke-path
    (sheet :: <permanent-medium-mixin>, #key filled?) => (record)
  stroke-path(sheet-medium(sheet), filled?: filled?)
end method stroke-path;


/// FILL-PATH

define method fill-path
    (sheet :: <basic-sheet>) => (record)
  with-sheet-medium (medium = sheet)
    fill-path(medium)
  end
end method fill-path;

define method fill-path
    (sheet :: <permanent-medium-mixin>) => (record)
  fill-path(sheet-medium(sheet))
end method fill-path;


/// CLIP-FROM-PATH

define method clip-from-path
    (sheet :: <basic-sheet>, #key function = $boole-and) => (record)
  with-sheet-medium (medium = sheet)
    clip-from-path(medium, function: function)
  end
end method clip-from-path;

define method clip-from-path
    (sheet :: <permanent-medium-mixin>, #key function = $boole-and) => (record)
  clip-from-path(sheet-medium(sheet), function: function)
end method clip-from-path;


/// SAVE-CLIPPING-REGION

define method save-clipping-region
    (sheet :: <basic-sheet>) => (record)
  with-sheet-medium (medium = sheet)
    save-clipping-region(medium)
  end
end method save-clipping-region;

define method save-clipping-region
    (sheet :: <permanent-medium-mixin>) => (record)
  save-clipping-region(sheet-medium(sheet))
end method save-clipping-region;


/// RESTORE-CLIPPING-REGION

define method restore-clipping-region
    (sheet :: <basic-sheet>) => (record)
  with-sheet-medium (medium = sheet)
    restore-clipping-region(medium)
  end
end method restore-clipping-region;

define method restore-clipping-region
    (sheet :: <permanent-medium-mixin>) => (record)
  restore-clipping-region(sheet-medium(sheet))
end method restore-clipping-region;


/// MOVE-TO

define method move-to
    (sheet :: <basic-sheet>, x, y) => (record)
  with-sheet-medium (medium = sheet)
    move-to(medium, x, y)
  end
end method move-to;

define method move-to
    (sheet :: <permanent-medium-mixin>, x, y) => (record)
  move-to(sheet-medium(sheet), x, y)
end method move-to;

define function move-to*
    (medium :: <drawable>, point :: <standard-point>) => (record)
  move-to(medium, point-x(point), point-y(point))
end function move-to*;


/// LINE-TO

define method line-to
    (sheet :: <basic-sheet>, x, y) => (record)
  with-sheet-medium (medium = sheet)
    line-to(medium, x, y)
  end
end method line-to;

define method line-to
    (sheet :: <permanent-medium-mixin>, x, y) => (record)
  line-to(sheet-medium(sheet), x, y)
end method line-to;

define function line-to*
    (medium :: <drawable>, point :: <standard-point>) => (record)
  line-to(medium, point-x(point), point-y(point))
end function line-to*;


/// ARC-TO

define method arc-to
    (sheet :: <basic-sheet>, center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #rest keys, #key start-angle, end-angle) => (record)
  dynamic-extent(keys);
  ignore(start-angle, end-angle);
  with-sheet-medium (medium = sheet)
    apply(arc-to, medium, center-x, center-y,
	  radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy, keys)
  end
end method arc-to;

define method arc-to
    (sheet :: <permanent-medium-mixin>, center-x, center-y,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #rest keys, #key start-angle, end-angle) => (record)
  dynamic-extent(keys);
  ignore(start-angle, end-angle);
  apply(arc-to, sheet-medium(sheet), center-x, center-y,
	radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy, keys)
end method arc-to;

define function arc-to*
    (medium :: <drawable>, center :: <standard-point>,
     radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy,
     #rest keys, #key start-angle, end-angle) => (record)
  dynamic-extent(keys);
  ignore(start-angle, end-angle);
  apply(arc-to, medium, point-x(center), point-y(center),
	radius-1-dx, radius-1-dy, radius-2-dx, radius-2-dy, keys)
end function arc-to*;


/// CURVE-TO

define method curve-to
    (sheet :: <basic-sheet>, x1, y1, x2, y2, x3, y3) => (record)
  with-sheet-medium (medium = sheet)
    curve-to(medium, x1, y1, x2, y2, x3, y3)
  end
end method curve-to;

define method curve-to
    (sheet :: <permanent-medium-mixin>, x1, y1, x2, y2, x3, y3) => (record)
  curve-to(sheet-medium(sheet), x1, y1, x2, y2, x3, y3)
end method curve-to;

define function curve-to* 
    (medium :: <drawable>, 
     p1 :: <standard-point>, p2 :: <standard-point>, p3 :: <standard-point>) => (record)
  curve-to(medium, point-x(p1), point-y(p1), 
                   point-x(p2), point-y(p2),
                   point-x(p3), point-y(p3))
end function curve-to*;
