Module:    mini-duim
Synopsis:  Mini-DUIM mediums
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Mediums

define open abstract class <sheet-with-medium-mixin> (<sheet>)
end class <sheet-with-medium-mixin>;

define method note-sheet-attached
    (sheet :: <sheet-with-medium-mixin>) => ()
  next-method();	// first create the mirror
  let medium = make-medium(port(sheet), sheet);
  sheet-medium(sheet) := medium;
  medium-drawable(medium) := sheet-mirror(sheet)
end method note-sheet-attached;


define open abstract class <abstract-medium> (<object>)
end class <abstract-medium>;

define open abstract class <medium> (<abstract-medium>)
end class <medium>;

define open abstract class <basic-medium> (<medium>)
  slot port = #f,
    init-keyword: port:;
  slot medium-sheet = #f,
    init-keyword: sheet:;
  slot medium-drawable = #f;
  //--- We really need for these setters to clear the drawing state cache...
  slot medium-foreground :: false-or(<ink>) = $foreground,
    init-keyword: foreground:;
  slot medium-background :: false-or(<ink>) = $background,
    init-keyword: background:;
  slot medium-pen :: <pen> = make(<pen>);
  slot medium-brush :: type-union(<brush>, <color>) = make(<brush>);
  slot medium-text-style :: <text-style> = make(<standard-text-style>);
  slot medium-clipping-region :: <region> = $everywhere;
  slot drawing-state-cached? = #f,
    setter: %drawing-state-cached?-setter;
end class <basic-medium>;

define method drawing-state-cached?-setter
    (state, medium :: <basic-medium>) => (state)
  let old-state = drawing-state-cached?(medium);
  unless (state == old-state)
    if (old-state)
      invalidate-cached-drawing-state(medium, old-state)
    end;
    medium.%drawing-state-cached? := state
  end;
  state
end method drawing-state-cached?-setter;

define method medium-merged-text-style
    (medium :: <basic-medium>) => (style :: <text-style>)
  medium-text-style(medium)
end method medium-merged-text-style;

define open generic make-medium
    (port :: <port>, sheet :: <sheet>) => (medium :: <medium>);

define method destroy-medium (medium :: <medium>) => ()
  invalidate-cached-drawing-state(medium, medium.drawing-state-cached?);
  do-destroy-medium(medium)
end method destroy-medium;

define open generic do-destroy-medium
    (medium :: <medium>) => ();

define method beep (sheet :: <basic-sheet>) => ()
  let medium = sheet-medium(sheet);
  if (medium)
    beep(medium)
  end
end method beep;


define macro with-drawing-options
  { with-drawing-options (?medium:name, #rest ?options:expression) ?:body end }
    => { begin
	   let with-drawing-options-body = method () ?body end;
	   do-with-drawing-options(?medium, with-drawing-options-body, ?options)
	 end }
end macro with-drawing-options;

define method do-with-drawing-options
    (medium :: <basic-medium>, continuation :: <function>,
     #key brush, pen, text-style) => (#rest values)
  let saved-brush = medium-brush(medium);
  let saved-pen   = medium-pen(medium);
  let saved-style = medium-text-style(medium);
  block ()
    if (brush)
      medium-brush(medium) := brush
    end;
    if (pen)
      medium-pen(medium) := pen
    end;
    if (text-style)
      medium-text-style(medium) := text-style
    end;
    drawing-state-cached?(medium) := #f;
    continuation()
  cleanup
    medium-brush(medium) := saved-brush;
    medium-pen(medium)   := saved-pen;
    medium-text-style(medium) := saved-style;
    drawing-state-cached?(medium) := #f;
  end
end method do-with-drawing-options;

define method do-with-drawing-options
    (sheet :: <basic-sheet>, continuation :: <function>, #rest options,
     #key brush, pen, text-style)
 => (#rest values)
  ignore(brush, pen, text-style);
  apply(do-with-drawing-options, sheet-medium(sheet), continuation, options)
end method do-with-drawing-options;


/// Pixmaps

define open abstract class <pixmap-medium> (<medium>) end;

define open abstract class <basic-pixmap-medium>
    (<basic-medium>, <pixmap-medium>)
  slot pixmap-medium-pixmap, required-init-keyword: pixmap:;
end class <basic-pixmap-medium>;

define open generic make-pixmap-medium
    (port :: <port>, sheet :: <sheet>, #key width, height)
 => (medium :: <pixmap-medium>);
