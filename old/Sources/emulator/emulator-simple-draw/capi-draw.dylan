Module: emulator-simple-draw
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Shouldn't be here!!!

define macro lisp-interface-definer
  { define lisp-interface ?imports end }
    => { ?imports }
imports:
  { ?import; ... }
    => { ?import; ... }
  { }
    => { }
import:
  { functions ?import-specs }
    => { import-cl-functions(?import-specs) }
  { values ?import-specs }
    => { import-cl-values(?import-specs) }
  { classes ?import-specs }
    => { import-cl-classes(?import-specs) }
import-specs:
  { ?import-spec, ... }
    => { ?import-spec, ... }
  { }
    => { }
import-spec:
  { ?symbol:name from ?package:name ?opt-alias }
    => { ?package(?symbol) ?opt-alias }
  { ?symbol:name ?opt-alias }
    => { ?symbol ?opt-alias }
opt-alias:
  { as ?alias:name }
    => { (as: ?alias) }
  { }
    => { }
end macro;

define lisp-interface 
  functions 
    representation-force-output from capi-library 
      as representation-force-output,
    schedule-event from mp as schedule-event,
    process-allow-scheduling from mp as process-allow-scheduling,
    sleep-for-time from mp as sleep-for-time,
    contain from capi as contain,
    set-graphics-state from gp as set-graphics-state,
    get-graphics-state from gp as get-graphics-state,
    draw-line from gp as draw-line,
    representation from capi as lisp/representation,
    representation-geometry from capi-library as representation-geometry,
    do-demand-pre-loads from lw as do-demand-pre-loads;
  values
    boole-ior from lisp as $boole-ior,
    boole-clr from lisp as $boole-clr;
  classes 
    output-pane from capi as <output-pane>;
end lisp-interface;

do-demand-pre-loads(capi:);

define class <view-pane> (<output-pane>)
  slot pane-view, init-value: #f, init-keyword: pane-view:;
end class;

define method make-app-window (#rest args)
  let pane 
    = apply(make, <view-pane>,
            display-callback: draw-wrapper, 
            background:       white:,
            args);
  contain(pane);
  pane
end method;

define method realize 
    (view-class, #rest initargs, #key extent = point(50, 50))
  let pane
    = make(<view-pane>,
           display-callback: draw-wrapper,
           background:       white:,
           min-width:        extent.h,
           min-height:       extent.v);
  contain(pane);
  let view
    = apply(make, view-class,
            representation: pane,
            initargs);
  view.representation.pane-view := view;
  view
end method;

define method extent (v :: <view>)
  let (x, y, width, height) 
    = representation-geometry
        (lisp/representation(representation(v)));
  point(width, height)
end method;

define method draw-wrapper (pane, x, y, w, h)
  *drawing-pane* := pane;
  let region = make(<region>, x: x, y: y, width: w, height: h);
  *last-region* := region;
  block ()
    when (pane.pane-view)
      draw(pane.pane-view, region)
    end;
  exception (<error>)
    #f
  end;
end method;

define variable *drawing-pane* = #f;
define variable *drawing-x* = #f;
define variable *drawing-y* = #f;

define constant $blackColor = black:;
define constant $cyanColor = cyan:;

define constant $patBic = $boole-clr;  // Dodgy assignments
define constant $patCopy = $boole-ior; 

define method ForeColor (color)
  // set-graphics-state(*drawing-pane*, foreground: color);
end method;

define method MoveTo (x, y)
  *drawing-x* := x;
  *drawing-y* := y;
end method;

define method Line (dx, dy)
  draw-line(*drawing-pane*, 
            *drawing-x*, *drawing-y*,
            *drawing-x* + dx, *drawing-y* + dy);
end method;

define method LineTo (x, y)
  draw-line(*drawing-pane*, 
            *drawing-x*, *drawing-y*,
            x, y);
end method;

define method FrameRect (r)
end method;

define method PenMode (mode)
  // set-graphics-state(*drawing-pane*, operation: mode)
end method;

// eof
