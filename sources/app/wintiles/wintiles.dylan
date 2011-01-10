Module: wintiles
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Hack

define method wrapped-make-tiling-view (#rest args)
//  format-out("Making tiling view with args %=\n", args);
  let view = apply(make-tiling-view, args);
//  format-out("Made view %=\n", view);
  view
end method;

define method wrapped-do-draw-action (view, #rest args)
//  format-out("Doing draw action with args %=\n", args);
  draw(view, make(<region>, x: 0, y: 0, width: 500, height: 400));
//  format-out("Done draw action\n");
end method;

define method wrapped-do-menu-action (#rest args)
//  format-out("Doing menu action with args %=\n", args);
  apply(do-menu-action, args);
//  format-out("Done menu action\n");
end method;

define method wrapped-do-mouse-action (#rest args)
//  format-out("Doing mouse action with args %=\n", args);
  apply(do-mouse-action, args);
//  format-out("Done mouse action\n");
end method;

define method wrapped-do-mouse-drag-action (#rest args)
//  format-out("Doing mouse action with args %=\n", args);
  apply(do-mouse-drag-action, args);
//  format-out("Done mouse action\n");
end method;

define method wrapped-do-idle-action (view, #rest args)
//  format-out("Doing idle action with args %=\n", args);
  idle(view);
//  format-out("Done idle action\n");
end method;

define C-callable-wrapper C-make-tiling-view of wrapped-make-tiling-view
  parameter representation :: <C-unsigned-long>;
  parameter width          :: <C-int>;
  parameter height         :: <C-int>;
  result    view           :: <C-dylan-object>;
  c-name: "MakeTilingView";
end C-callable-wrapper;

define C-callable-wrapper C-do-draw-action of wrapped-do-draw-action
  parameter view :: <C-dylan-object>;
  c-name: "DoDrawAction";
end C-callable-wrapper;

define C-callable-wrapper C-do-menu-action of wrapped-do-menu-action
  parameter view  :: <C-dylan-object>;
  parameter code  :: <C-int>;
  parameter value :: <C-int>;
  c-name: "DoMenuAction";
end C-callable-wrapper;

define C-callable-wrapper C-do-mouse-action of wrapped-do-mouse-action
  parameter view :: <C-dylan-object>;
  parameter x :: <C-int>;
  parameter y :: <C-int>;
  c-name: "DoMouseAction";
end C-callable-wrapper;

define C-callable-wrapper C-do-mouse-drag-action 
    of wrapped-do-mouse-drag-action
  parameter view :: <C-dylan-object>;
  parameter x :: <C-int>;
  parameter y :: <C-int>;
  c-name: "DoMouseDragAction";
end C-callable-wrapper;

define C-callable-wrapper C-do-idle-action of wrapped-do-idle-action
  parameter view :: <C-dylan-object>;
  c-name: "DoIdleAction";
end C-callable-wrapper;

define C-function DylanMoveTo
  parameter x :: <C-int>;
  parameter y :: <C-int>;
  c-name: "DylanMoveTo";
end C-function;

define C-function DylanLineTo
  parameter x :: <C-int>;
  parameter y :: <C-int>;
  c-name: "DylanLineTo";
end C-function;

define C-function DylanForeColor
  parameter code :: <C-int>;
  c-name: "DylanForeColor";
end C-function;

define C-function DylanPenMode
  parameter code :: <C-int>;
  c-name: "DylanPenMode";
end C-function;

define C-function DylanInvalidateAll
  c-name: "DylanInvalidateAll"
end C-function;

define method code-from-pen (pen)
  select (pen)
    #"black" => 0;
    #"cyan"  => 1;
  end;
end method;

define method code-from-mode (mode)
  select (mode)
    #"copy" => 0;
    #"bic"  => 1;
  end;
end method;

// Stub drawing:

define variable *cursor-x* = 0;
define variable *cursor-y* = 0;

define method ForeColor (color)
//  format-out("ForeColor: %=\n", color);
  DylanForeColor(code-from-pen(color));
end method;

define method PenMode (mode)
//  format-out("PenMode: %=\n", mode);
  DylanPenMode(code-from-mode(mode));
end method;

define method MoveTo (x, y)
//  format-out("MoveTo: %= %=\n", x, y);
  *cursor-x* := x;
  *cursor-y* := y;
  DylanMoveTo(x, y);
end method;

define method Line (dx, dy)
//  format-out("Line: %= %=\n", dx, dy);
  DylanLineTo(*cursor-x* + dx, *cursor-y* + dy);
end method;

define method LineTo (x, y)
//  format-out("LineTo: %= %=\n", x, y);
  DylanLineTo(x, y);
end method;

define method FrameRect (r)
  DylanMoveTo(r.left, r.top);
  DylanLineTo(r.right - 1, r.top);
  DylanLineTo(r.right - 1 , r.bottom - 1);
  DylanMoveTo(r.left, r.top);
  DylanLineTo(r.left , r.bottom - 1);
  DylanLineTo(r.right - 1 , r.bottom - 1);
end method;

define method invalidate-all (v :: <view>)
//  format-out("invalidate-all: %=\n", v);
  DylanInvalidateAll();
end method;

// format-out("Dylan libraries initialized.\n");

/*
define method doit ()
  format-out("Doin' it\n");
  let tiles = make-tiling-view("rep", 100, 100);
  for (i from 0 below 10)
    idle(tiles);
  end;
end method;

doit();
*/
