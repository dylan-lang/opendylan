Module:       duim-sheets-internals
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// User-level geometry, interacts correctly with layout (later)

define protocol <<sheet-geometry-protocol>> (<<sheet-protocol>>)
  // Simple geometry changes
  function sheet-position
    (sheet :: <abstract-sheet>) => (x :: <integer>, y :: <integer>);
  function set-sheet-position
    (sheet :: <abstract-sheet>, x :: <integer>, y :: <integer>) => ();
  function sheet-size
    (sheet :: <abstract-sheet>) => (width :: <integer>, height :: <integer>);
  function set-sheet-size
    (sheet :: <abstract-sheet>, width :: <integer>, height :: <integer>) => ();
  function sheet-edges
    (sheet :: <abstract-sheet>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>);
  function set-sheet-edges
    (sheet :: <abstract-sheet>,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>) => ();
  // Geometry propagation
  function relayout-children
    (sheet :: <abstract-sheet>) => ();
  function relayout-parent
    (sheet :: <abstract-sheet>, #key width, height) => (did-layout? :: <boolean>);
end protocol <<sheet-geometry-protocol>>;


// Returns the position of the sheet in the parent's coordinate space
define sealed method sheet-position
    (sheet :: <basic-sheet>) => (x :: <integer>, y :: <integer>)
  let transform = sheet-transform(sheet);
  transform-position(transform, 0, 0)
end method sheet-position;

define sealed method set-sheet-position
    (sheet :: <basic-sheet>, x :: <integer>, y :: <integer>) => ()
  let (width, height) = sheet-size(sheet);
  set-sheet-edges(sheet, x, y, x + width, y + height)
end method set-sheet-position;

define sealed method sheet-size
    (sheet :: <basic-sheet>) => (width :: <integer>, height :: <integer>)
  box-size(sheet-region(sheet))
end method sheet-size;

define sealed method set-sheet-size
    (sheet :: <basic-sheet>, width :: <integer>, height :: <integer>) => ()
  let (x, y) = sheet-position(sheet);
  set-sheet-edges(sheet, x, y, x + width, y + height)
end method set-sheet-size;

// Returns the edges of the sheet in the parent's coordinate space
// Note that this is different from calling 'box-edges' on a sheet!
define sealed method sheet-edges
    (sheet :: <basic-sheet>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let transform = sheet-transform(sheet);
  let (left, top, right, bottom) = box-edges(sheet);
  transform-box(transform, left, top, right, bottom)
end method sheet-edges;

define sealed method set-sheet-edges
    (sheet :: <basic-sheet>,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>) => ()
  let (old-x, old-y) = sheet-position(sheet);
  let new-transform = #f;
  when (left ~= old-x | top ~= old-y)
    // We would like to use volatile transformations here, but it's
    // not really safe, since the current implementation of transformations
    // is likely to cause them to be shared
    new-transform
      := compose-translation-with-transform
           (sheet-transform(sheet), left - old-x, top - old-y)
  end;
  let width  :: <integer> = right  - left;
  let height :: <integer> = bottom - top;
  let new-region = #f;
  unless (sheet-layed-out-to-size?(sheet, width, height)) 
    validate-sheet-size(sheet, width, height);
    let (old-left, old-top) = box-position(sheet);
    // It should be safe to modify the sheet's region, since
    // each sheet gets a fresh region when it is created
    new-region				// might cons a new region...
      := set-box-edges(sheet-region(sheet),
		       old-left, old-top, old-left + width, old-top + height)
  end;
  case
    new-transform =>
      sheet-transform(sheet) := new-transform;
    //---*** Should we do this?  It's intended to move mirrors for
    //---*** sheets that have been "regrafted" even if their sheet
    //---*** position has not changed
    // sheet-direct-mirror(sheet) =>
    //   update-mirror-transform(port(sheet), sheet, sheet-direct-mirror(sheet));
    otherwise =>
      #f;
  end;
  when (new-region)
    sheet-region(sheet) := new-region;
    // We need to relayout the kids only if the region changed.
    // (Note that full-strength layout will take care of moving mirrors.)
    relayout-children(sheet)
  end;
  //--- A new transform could cause any of the mirrors to be incorrectly
  //--- positioned, so currently we update them all. There must be a more
  //--- efficient algorithm...
  when (new-transform)
    update-all-mirror-positions(sheet)
  end
end method set-sheet-edges;


define open generic sheet-layed-out-to-size?
    (sheet :: <abstract-sheet>, width :: <integer>, height :: <integer>)
 => (layed-out? :: <boolean>);

define method sheet-layed-out-to-size?
    (sheet :: <basic-sheet>, width :: <integer>, height :: <integer>)
 => (layed-out? :: <boolean>)
  let (old-width, old-height) = box-size(sheet);
  // We don't need to check 'sheet-layed-out?' here because this
  // only get called for non-composite sheets
  width = old-width & height = old-height
end method sheet-layed-out-to-size?;


// On raw sheets, this doesn't need to do anything
define method relayout-children
    (pane :: <sheet>) => ()
  #f
end method relayout-children;


define open generic validate-sheet-size
    (sheet :: <abstract-sheet>, width :: <integer>, height :: <integer>) => ();

define method validate-sheet-size
    (sheet :: <sheet>, width :: <integer>, height :: <integer>) => ()
  ignore(width, height);
  #f
end method validate-sheet-size;
