Module:    mini-duim
Synopsis:  Mini-DUIM sheets
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Sheets

define open abstract class <abstract-sheet> (<object>)
end class <abstract-sheet>;

define open abstract class <sheet> (<abstract-sheet>)
end class <sheet>;

define open abstract class <basic-sheet> (<sheet>)
  slot sheet-region :: <region> = make-bounding-box(0, 0, 100, 100),
    init-keyword: region:;
  slot sheet-transform :: <transform> = make-translation-transform(0, 0),
    init-keyword: transform:;
  slot sheet-parent :: false-or(<sheet>) = #f,
    init-keyword: parent:;
  slot sheet-children :: <sequence> = make(<stretchy-vector>);
  slot sheet-medium = #f;
  slot sheet-direct-mirror = #f;
  slot port = #f;
  slot %mapped? = #f;
end class <basic-sheet>;

define method make
    (class == <sheet>, #rest initargs, #key) => (sheet :: <sheet>)
  apply(make, <basic-sheet>, initargs)
end method make;

define method initialize
    (sheet :: <basic-sheet>, #key x, y, width, height, children) => ()
  next-method();
  if (x & y)
    sheet-transform(sheet) := make-translation-transform(x, y)
  end;
  if (width & height)
    sheet-region(sheet) := make-bounding-box(0, 0, width, height)
  end;
  if (children)
    for (child in children)
      add-child(sheet, child)
    end
  end
end method initialize;


define method box-edges
    (sheet :: <basic-sheet>) => (left, top, right, bottom)
  box-edges(sheet-region(sheet))
end method box-edges;

// Returns the edges of the sheet in the parent's coordinate space
define method sheet-edges
    (sheet :: <basic-sheet>) 
 => (left :: <integer>,  top :: <integer>, 
     right :: <integer>, bottom :: <integer>)
  let transform = sheet-transform(sheet);
  let (left, top, right, bottom) = box-edges(sheet);
  transform-box(transform, left, top, right, bottom)
end method sheet-edges;

define method sheet-position
    (sheet :: <basic-sheet>)
 => (x :: <integer>, y :: <integer>)
  let (left, top, right, bottom) = sheet-edges(sheet);
  ignore(right, bottom);
  values(left, top)
end method sheet-position;

define method sheet-size
    (sheet :: <basic-sheet>)
 => (width :: <integer>, height :: <integer>)
  let (left, top, right, bottom) = sheet-edges(sheet);
  values(right - left, bottom - top)
end method sheet-size;


define method set-sheet-edges
    (sheet :: <basic-sheet>, 
     left :: <integer>, top :: <integer>, 
     right :: <integer>, bottom :: <integer>)
 => ()
  //---*** Separated out to avoid Webster next-method() crash!
  %set-sheet-edges(sheet, left, top, right, bottom)
end method set-sheet-edges;

//---*** Separated out due to next-method() bug in Webster 8000
define method %set-sheet-edges
    (sheet :: <basic-sheet>, 
     left :: <integer>, top :: <integer>, 
     right :: <integer>, bottom :: <integer>)
 => ()
  let (old-x, old-y) = sheet-edges(sheet);
  let new-transform = #f;
  if (left ~= old-x | top ~= old-y)
    new-transform
      := compose-translation-with-transform
           (sheet-transform(sheet), left - old-x, top - old-y)
  end;
  let new-region = #f;
  let (old-left, old-top, old-right, old-bottom) = box-edges(sheet);
  let old-width  = old-right  - old-left;
  let old-height = old-bottom - old-top;
  let width  = right  - left;
  let height = bottom - top;
  if (width ~= old-width | height ~= old-height)
    new-region :=
      make-bounding-box(old-left, old-top, old-left + width, old-top + height)
  end;
  if (new-transform)
    sheet-transform(sheet) := new-transform
  end;
  if (new-region)
    sheet-region(sheet) := new-region
  end
end method %set-sheet-edges;

define method set-sheet-position
    (sheet :: <basic-sheet>, x :: <integer>, y :: <integer>) => ()
  let (width, height) = sheet-size(sheet);
  debug-message("set-sheet-position setting sheet edges %=: %=, %=, %=, %=\n",
		sheet, x, y, x + width, y + height);
  set-sheet-edges(sheet, x, y, x + width, y + height)
end method set-sheet-position;

define method set-sheet-size
    (sheet :: <basic-sheet>, width :: <integer>, height :: <integer>) => ()
  let (x, y) = sheet-position(sheet);
  debug-message("set-sheet-size setting sheet edges %=: %=, %=, %=, %=\n",
		sheet, x, y, x + width, y + height);
  set-sheet-edges(sheet, x, y, x + width, y + height)
end method set-sheet-size;


define method sheet-delta-transform
    (sheet :: <sheet>, ancestor :: <sheet>) => (transform :: <transform>)
  if (sheet == ancestor)
    $identity-transform
  else
    local method delta-transform (s :: <sheet>, a :: <sheet>)
            let parent = sheet-parent(s);
            case
              parent == a =>
                sheet-transform(s);
              ~parent =>
                error("The sheet %= is not an ancestor of %=", ancestor, sheet);
              otherwise =>
                compose-transforms(sheet-transform(s),
                                   sheet-delta-transform(parent, a));
            end
          end method;
    delta-transform(sheet, ancestor)
  end
end method sheet-delta-transform;


define method add-child
    (sheet :: <basic-sheet>, child :: <basic-sheet>) => ()
  add!(sheet-children(sheet), child);
  sheet-parent(child) := sheet;
  note-child-added(sheet, child)
end method add-child;

define method note-child-added (sheet :: <sheet>, child :: <sheet>) => ()
  if (sheet-grafted?(sheet))
    graft-sheet(sheet, child)
  end
end method note-child-added;


define method remove-child
    (sheet :: <basic-sheet>, child :: <basic-sheet>) => ()
  remove!(sheet-children(sheet), child);
  note-child-removed(sheet, child);
  sheet-parent(child) := #f
end method remove-child;

define method note-child-removed (sheet :: <sheet>, child :: <sheet>) => ()
  if (sheet-grafted?(sheet))
    sheet-mapped?(child) := #f;
    degraft-sheet(sheet, child)
  end
end method note-child-removed;


define method sheet-grafted?
    (sheet :: <basic-sheet>) => (mapped? :: <boolean>)
  port(sheet) & #t
end method sheet-grafted?;

define method graft-sheet
    (parent :: <basic-sheet>, sheet :: <basic-sheet>) => ()
  port(sheet) := port(parent);
  note-sheet-attached(sheet);
  for (child in sheet-children(sheet))
    graft-sheet(sheet, child)
  end
end method graft-sheet;

define method note-sheet-attached (sheet :: <basic-sheet>) => ()
  ignore(sheet);
  #f
end method note-sheet-attached;

define method degraft-sheet
    (parent :: <basic-sheet>, sheet :: <basic-sheet>) => ()
  ignore(parent);
  for (child in sheet-children(sheet))
    degraft-sheet(sheet, child)
  end;
  note-sheet-detached(sheet);
  port(sheet) := #f
end method degraft-sheet;

define method note-sheet-detached (sheet :: <basic-sheet>) => ()
  ignore(sheet);
  #f
end method note-sheet-detached;


define method sheet-mapped?
    (sheet :: <basic-sheet>) => (mapped? :: <boolean>)
  sheet.%mapped?
end method sheet-mapped?;

define method sheet-mapped?-setter
    (mapped? == #t, sheet :: <basic-sheet>) => (mapped? :: <boolean>)
  for (child in sheet-children(sheet))
    sheet-mapped?(child) := #t
  end;
  sheet.%mapped? := #t;
  let mirror = sheet-direct-mirror(sheet);
  if (mirror)
    map-mirror(port(sheet), sheet, mirror)
  end;
  #t
end method sheet-mapped?-setter;

define method sheet-mapped?-setter
    (mapped? == #f, sheet :: <basic-sheet>) => (mapped? :: <boolean>)
  sheet.%mapped? := #f;
  let mirror = sheet-direct-mirror(sheet);
  if (mirror)
    unmap-mirror(port(sheet), sheet, mirror)
  end;
  for (child in sheet-children(sheet))
    sheet-mapped?(child) := #f
  end;
  #f
end method sheet-mapped?-setter;


define method destroy-sheet (sheet :: <basic-sheet>) => ()
  for (child in sheet-children(sheet))
    destroy-sheet(child)
  end;
  if (sheet-medium(sheet))
    destroy-medium(sheet-medium(sheet))
  end;
  let mirror = sheet-direct-mirror(sheet);
  if (mirror)
    destroy-mirror(port(sheet), sheet, mirror);
    sheet-direct-mirror(sheet) := #f
  end
end method destroy-sheet;


/// Stubs needed for smooth back-end sharing

define open abstract class <standard-input-mixin> (<object>)
end class <standard-input-mixin>;

define open abstract class <standard-repainting-mixin> (<object>)
end class <standard-repainting-mixin>;

define open abstract class <permanent-medium-mixin> (<object>)
end class <permanent-medium-mixin>;

define open generic invalidate-cached-regions (sheet :: <abstract-sheet>) => ();
define open generic invalidate-cached-region (drawable) => ();
define open generic invalidate-cached-transforms (sheet :: <abstract-sheet>) => ();
define open generic invalidate-cached-transform (drawable) => ();
define open generic invalidate-cached-drawing-state (medium :: <medium>, cached-state) => ();
