Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library simple-draw
  use dylan;
  export simple-draw;
end library;

define module simple-draw
  use dylan;
  export
    
    // Geometry utlities
    <point>, point, x, y, h, v,
    <rectangle>, top, bottom, left, right, as-rect, SetRect,
    <QDrect>,
    <region>, 

    // The drawing area and its callback 
    <view>, draw, extent, invalidate-all, idle, representation,
    \with-focused-view, call-with-focused-view,

    // Color and drawing
    Forecolor, $blackColor, $cyanColor, 
    PenMode, $patBic, $patCopy,
    Line, LineTo, MoveTo, FrameRect,
 
    // Misc
    Random;
end library;

// eof
