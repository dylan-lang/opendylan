Module: dylan-framework
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
    random from lisp as lisp/random,
    representation-force-output from capi-library 
      as representation-force-output,
    schedule-event from mp as schedule-event,
    process-allow-scheduling from mp as process-allow-scheduling,
    sleep-for-time from mp as sleep-for-time,
    contain from capi as contain,
    set-graphics-state from gp as set-graphics-state,
    get-graphics-state from gp as get-graphics-state,
    draw-line from gp as draw-line,
    representation from capi as representation,
    representation-geometry from capi-library as representation-geometry;
  values
    boole-ior from lisp as $boole-ior,
    boole-clr from lisp as $boole-clr;
  classes 
    output-pane from capi as <output-pane>;
end lisp-interface;

define class <view> (<output-pane>) 
  slot ready?, init-value: #f;
end;

define method make 
    (c :: subclass(<view>), #rest args, #key extent = point(200, 200))
  let pane 
    = apply(next-method, c, 
            display-callback: draw-wrapper, 
            background:       white:,
            min-width:        extent.x,
            min-height:       extent.y,
            args);
  pane.ready? := #t;
  pane
end method;

define method initialize (v :: <view>, #rest args)
  next-method();
  contain(v);
  v
end method;

define method extent (v :: <view>)
  let (x, y, width, height) = representation-geometry(representation(v));
  make(<extent>, v: height, h: width)
end method;

define method draw-wrapper (pane, x, y, w, h)
  // Hack!!! Bogus, bogus, spinish-lock. Should reorder initialisation.
  until (pane.ready?) 
    sleep-for-time(1);
  end;
  *drawing-pane* := pane;
  let region = make(<region>, x: x, y: y, width: w, height: h);
  *last-region* := region;
  draw(pane, region)
end method;

define method draw (v :: <view>, r :: <region>)
end method;

define variable *drawing-pane* = #f;
define variable *drawing-x* = #f;
define variable *drawing-y* = #f;

define constant $patBic = $boole-clr;
define constant $patCopy = $boole-ior;

define method ForeColor (color)
  set-graphics-state(*drawing-pane*, foreground: color);
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

define method SetRect (r, top, bottom, left, right)
  make(<rectangle>, top: top, bottom: bottom, left: left, right: right)
end method;

define method FrameRect (r)
end method;

define method PenMode (mode)
  set-graphics-state(*drawing-pane*, operation: mode)
end method;

define constant $idler-table = make(<table>);

define method idle (v :: <view>)
end method;

define method add-idler (v :: <view>, time)
  let prodder = method () prod(v) end;
  let lw-time = time / 100;
  $idler-table[v] := pair(lw-time, prodder);
  schedule-event(5, prodder); // Hack!!!
  v
end method;

define method remove-idler (v :: <view>)
  remove-key!($idler-table, v);
end method;

define method prod (v :: <view>)
  let pair = element($idler-table, v, default: #f);
  if (pair)
    idle(v);
    schedule-event(pair.head, pair.tail);
  end;
end method;

define method random ()
  lisp/random(9999)
end method;

// eof
