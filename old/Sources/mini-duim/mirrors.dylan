Module:    mini-duim
Synopsis:  Mini-DUIM mirrors
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Mirrors and mirrored sheets

define protocol <<mirror-protocol>> ()
  // Creating and destroying mirrors
  function make-mirror
    (port :: <port>, sheet :: <sheet>) => (mirror :: <mirror>);
  function do-make-mirror
    (port :: <port>, sheet :: <sheet>) => (mirror :: <mirror>);
  function destroy-mirror
    (port :: <port>, sheet :: <sheet>, mirror :: <mirror>) => ();
  function map-mirror
    (port :: <port>, sheet :: <sheet>, mirror :: <mirror>) => ();
  function unmap-mirror
    (port :: <port>, sheet :: <sheet>, mirror :: <mirror>) => ();
  function raise-mirror
    (port :: <port>, sheet :: <sheet>, mirror :: <mirror>) => ();
  function lower-mirror
    (port :: <port>, sheet :: <sheet>, mirror :: <mirror>) => ();
  function mirror-visible?
    (port :: <port>, sheet :: <sheet>, mirror :: <mirror>)
 => (true? :: <boolean>);
  // Mirror geometry
  // Returns the region's mirror in the mirror coordinate space
  // Returns the coordinates of sheet's mirror's bounding box in the coordinates
  // of the parent of the mirror, that is (left,top, right,bottom)
  function mirror-edges
    (port :: <port>, sheet :: <sheet>, mirror :: <mirror>)
 => (left :: <coordinate>, top :: <coordinate>, right :: <coordinate>, bottom :: <coordinate>);
  // Set the edges of the sheet mirror in the coordinate space of the parent mirror.
  //--- Should we add 'set-mirror-region' for consistency (and OpenDoc support)?
  function set-mirror-edges
    (port :: <port>, sheet :: <sheet>, mirror :: <mirror>,
     left :: <coordinate>, top :: <coordinate>, right :: <coordinate>, bottom :: <coordinate>) => ();
  // Sheet <-> mirror synchronization
end protocol <<mirror-protocol>>;

define open abstract class <mirror> (<object>)
end class <mirror>;

define open abstract class <mirrored-sheet-mixin> (<sheet>)
end class <mirrored-sheet-mixin>;

define method make-mirror
    (_port :: <port>, sheet :: <mirrored-sheet-mixin>) 
 => (mirror :: <mirror>)
  sheet-direct-mirror(sheet)
  | begin
      let mirror = do-make-mirror(_port, sheet);
      sheet-direct-mirror(sheet) := mirror;
      mirror-sheet(mirror) := sheet;
      mirror
    end
end method make-mirror;


define method note-sheet-attached
    (sheet :: <mirrored-sheet-mixin>) => ()
  make-mirror(port(sheet), sheet);
  next-method()
end method note-sheet-attached;

define method note-sheet-detached
    (sheet :: <mirrored-sheet-mixin>) => ()
  let mirror = sheet-direct-mirror(sheet);
  if (mirror)
    destroy-mirror(port(sheet), sheet, mirror);
    sheet-direct-mirror(sheet) := #f;
  end;
  next-method()
end method note-sheet-detached;

define method sheet-mirror (sheet :: <basic-sheet>) => (mirror)
  let ancestor = sheet-mirrored-ancestor(sheet, error?: #f);
  ancestor & sheet-direct-mirror(ancestor)
end method sheet-mirror;

define method sheet-mirrored-ancestor
    (sheet :: <basic-sheet>, #key error? = #t) => (sheet :: false-or(<sheet>))
  case
    sheet-direct-mirror(sheet) =>
      sheet;
    ~sheet-parent(sheet) =>
      error? & error("The sheet %= has no mirrored ancestors", sheet);
    otherwise =>
      sheet-mirrored-ancestor(sheet-parent(sheet), error?: error?);
  end
end method sheet-mirrored-ancestor;

define method sheet-device-transform
    (sheet :: <basic-sheet>) => (transform :: <transform>)
  if (sheet-parent(sheet))
    compose-transforms
      (sheet-transform(sheet), sheet-device-transform(sheet-parent(sheet)))
  else
    $identity-transform
  end    
end method sheet-device-transform;

define method sheet-device-transform
    (sheet :: <mirrored-sheet-mixin>) => (transform :: <transform>)
  if (sheet-direct-mirror(sheet))
    $identity-transform
  else
    next-method()
  end
end method sheet-device-transform;

define method sheet-device-region
    (sheet :: <basic-sheet>) => (region :: <region>)
  transform-region(sheet-device-transform(sheet), sheet-region(sheet))
end method sheet-device-region;

define method sheet-native-edges
    (sheet :: <mirrored-sheet-mixin>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  let (left, top, right, bottom) = box-edges(sheet-region(sheet));
  transform-box(sheet-transform(sheet), left, top, right, bottom)
end method sheet-native-edges;

define method set-sheet-edges
    (sheet :: <mirrored-sheet-mixin>, left, top, right, bottom) => ()
  //---*** Should be: next-method() but Webster crashes!
  %set-sheet-edges(sheet, left, top, right, bottom);
  let mirror = sheet-direct-mirror(sheet);
  if (mirror)
    set-mirror-edges(port(sheet), sheet, mirror, left, top, right, bottom)
  end
end method set-sheet-edges;

define method sheet-parent-mirror 
    (sheet :: <basic-sheet>) => (mirror :: <mirror>)
  let mirrored-parent = sheet-mirrored-ancestor(sheet);
  sheet-direct-mirror(mirrored-parent)
end method sheet-parent-mirror;
