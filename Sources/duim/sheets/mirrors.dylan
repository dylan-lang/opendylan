Module:       duim-sheets-internals
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Mirrored sheets and mirrors

define protocol <<mirrored-sheet-protocol>> ()
  // Mirroring a sheet
  getter sheet-mirror
    (sheet :: <abstract-sheet>) => (mirror);
  getter mirror-sheet
    (mirror) => (sheet :: <abstract-sheet>);
  setter mirror-sheet-setter
    (sheet :: <abstract-sheet>, mirror) => (sheet :: <abstract-sheet>);
  // Sheet native and device coordinates
  // The "ground" coordinate space for a sheet, almost always $identity-transform
  getter sheet-native-transform
    (sheet :: <abstract-sheet>) => (transform :: <transform>);
  setter sheet-native-transform-setter
    (transform :: <transform>, sheet :: <abstract-sheet>) => (transform :: <transform>);
  // The clipping region for a sheet in its "ground" coordinate space
  // usually a box of (0,0, width,height).  There's no setter for this
  // since that gets done by setting the mirror itself.
  getter sheet-native-region
    (sheet :: <abstract-sheet>) => (region :: <region>);
  // Returns the edges of the <bounding-box> for the sheet's native region
  // in its "ground" coordinate space
  getter sheet-native-edges
    (sheet :: <abstract-sheet>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>);
  // Transform from sheet's coordinate space to the coordinate space of its
  // first mirrored ancestor.  For a directly mirrored sheet, this will
  // almost always be $identity-transform.
  getter sheet-device-transform
    (sheet :: <abstract-sheet>) => (transform :: <transform>);
  // The sheet's clipping region expressed in the coordinate space of its
  // first mirrored ancestor.
  getter sheet-device-region
    (sheet :: <abstract-sheet>) => (region :: <region>);
  getter sheet-device-edges
    (sheet :: <abstract-sheet>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>);
  // Mirror-based repainting
  // In the X model, a repaint event comes in for every mirrored sheet.
  // In the Windows model, a repaint event comes in for every mirrored sheet.
  // In the Mac model, a repaint event comes in only for the top level sheet.
  // This function should return #t for any sheet that gets automagically
  // repainted by the native Window system, and should return #f for every
  // sheet for which 'repaint-sheet' should arrange to call 'handle-repaint'
  function port-handles-repaint?
    (port :: <abstract-port>, sheet :: <abstract-sheet>) => (true? :: <boolean>);
end protocol <<mirrored-sheet-protocol>>;

define protocol <<mirror-protocol>> ()
  // Creating and destroying mirrors
  function make-mirror
    (port :: <abstract-port>, sheet :: <abstract-sheet>) => (mirror :: <mirror>);
  function do-make-mirror
    (port :: <abstract-port>, sheet :: <abstract-sheet>) => (mirror :: <mirror>);
  function destroy-mirror
    (port :: <abstract-port>, sheet :: <abstract-sheet>, mirror :: <mirror>) => ();
  function map-mirror
    (port :: <abstract-port>, sheet :: <abstract-sheet>, mirror :: <mirror>) => ();
  function unmap-mirror
    (port :: <abstract-port>, sheet :: <abstract-sheet>, mirror :: <mirror>) => ();
  function raise-mirror
    (port :: <abstract-port>, sheet :: <abstract-sheet>, mirror :: <mirror>,
     #key activate?) => ();
  function lower-mirror
    (port :: <abstract-port>, sheet :: <abstract-sheet>, mirror :: <mirror>) => ();
  function mirror-visible?
    (port :: <abstract-port>, sheet :: <abstract-sheet>, mirror :: <mirror>)
 => (true? :: <boolean>);
  function mirror-origin
    (port :: <abstract-port>, sheet :: <abstract-sheet>) => (origin);
  // Mirror geometry
  // Returns the region's mirror in the mirror coordinate space
  function mirror-region
    (port :: <abstract-port>, sheet :: <abstract-sheet>, mirror :: <mirror>)
 => (region :: <region>);
  // Returns the coordinates of sheet's mirror's bounding box in the coordinates
  // of the parent of the mirror, that is (left,top, right,bottom)
  function mirror-edges
    (port :: <abstract-port>, sheet :: <abstract-sheet>, mirror :: <mirror>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>);
  // Set the edges of the sheet mirror in the coordinate space of the parent mirror.
  //--- Should we add 'set-mirror-region' for consistency (and OpenDoc support)?
  function set-mirror-edges
    (port :: <abstract-port>, sheet :: <abstract-sheet>, mirror :: <mirror>,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>) => ();
  // Sheet <-> mirror synchronization
  function note-mirror-geometry-changed
    (port :: <abstract-port>, sheet :: <abstract-sheet>, region :: <region>) => ();
  function update-mirror-region
    (port :: <abstract-port>, sheet :: <abstract-sheet>, mirror) => ();
  function update-mirror-transform
    (port :: <abstract-port>, sheet :: <abstract-sheet>, mirror) => ();
end protocol <<mirror-protocol>>;


/// Mirrored sheets

// A sealed protocol...
define generic sheet-resource-id
    (sheet :: <abstract-sheet>) => (resource-id);

define open abstract class <sheet-with-resource-mixin> (<abstract-sheet>)
  sealed constant slot sheet-resource-id = #f,
    init-keyword: resource-id:;
end class <sheet-with-resource-mixin>;

define sealed method sheet-resource-id (sheet :: <sheet>) => (resource-id)
  #f
end method sheet-resource-id;


// Another sealed protocol...
define generic sheet-direct-mirror
    (sheet :: <abstract-sheet>) => (mirror);
define generic sheet-direct-mirror-setter
    (mirror, sheet :: <abstract-sheet>) => (mirror);

// This must come after <permanent-medium-mixin> in any CPL.  See the
// comments on <permanent-medium-mixin>'s class definition for why.
define open abstract class <mirrored-sheet-mixin> (<sheet-with-resource-mixin>)
  sealed slot sheet-direct-mirror = #f;
end class <mirrored-sheet-mixin>;

define sealed method sheet-direct-mirror (sheet :: <sheet>) => (mirror)
  #f
end method sheet-direct-mirror;


define method do-destroy-sheet (sheet :: <mirrored-sheet-mixin>) => ()
  let _port  = port(sheet);
  let mirror = sheet-direct-mirror(sheet);
  when (_port & mirror)
    destroy-mirror(_port, sheet, mirror)
  end;
  next-method()		// off to <basic-sheet>
end method do-destroy-sheet;

// This property allows certain mirrored sheets to decline to have
// mirrors parented into them
define method sheet-mirror-accepts-children?
    (sheet :: <mirrored-sheet-mixin>) => (accepts-children? :: <boolean>)
  logand(sheet-flags(sheet), %mirror_accepts_children) = %mirror_accepts_children
end method sheet-mirror-accepts-children?;

define method sheet-mirror-accepts-children?-setter
    (accepts-children? :: <boolean>, sheet :: <mirrored-sheet-mixin>)
 => (accepts-children? :: <boolean>)
  sheet-flags(sheet)
    := logior(logand(sheet-flags(sheet), lognot(%mirror_accepts_children)),
	      if (accepts-children?) %mirror_accepts_children else 0 end);
  accepts-children?
end method sheet-mirror-accepts-children?-setter;


/// Native transformation, only meaningful on mirrored sheets

define method sheet-native-transform
    (sheet :: <mirrored-sheet-mixin>) => (transform :: <transform>)
  $identity-transform
end method sheet-native-transform;

define method sheet-native-transform-setter
    (transform :: <transform>, sheet :: <mirrored-sheet-mixin>)
 => (transform :: <transform>)
  // The default implementation only supports the identity transform,
  // but back-ends can override this
  assert(identity-transform?(transform),
	 "The native transform %= is not the identity transform", transform);
  transform
end method sheet-native-transform-setter;


/// Device transformation

define method sheet-device-transform
    (sheet :: <sheet>) => (transform :: <transform>)
  sheet-device-parent-transform(sheet)
end method sheet-device-transform;

define method sheet-device-transform
    (sheet :: <basic-sheet>) => (transform :: <transform>)
  sheet-cached-device-transform(sheet)
  | (sheet-cached-device-transform(sheet) := next-method())
end method sheet-device-transform;

define method sheet-device-transform
    (sheet :: <mirrored-sheet-mixin>) => (transform :: <transform>)
  // If this sheet is directly mirrored, use the mirror's native transform
  if (sheet-direct-mirror(sheet))
    sheet-native-transform(sheet)
  else
    next-method()
  end
end method sheet-device-transform;

define method sheet-device-parent-transform
    (sheet :: <sheet>) => (transform :: <transform>)
  local method parent-transform
	    (sheet :: <sheet>) => (transform :: <transform>)
	  let parent = sheet-parent(sheet);
	  // For mirrored sheets that don't accept children, we just
	  // go up the tree to find the first mirror that will
	  if (sheet-direct-mirror(parent)
	      & ~sheet-mirror-accepts-children?(parent))
	    compose-transforms(sheet-transform(parent), parent-transform(parent))
	  else
	    sheet-device-transform(parent)
	  end
	end method;
  compose-transforms(sheet-transform(sheet), parent-transform(sheet))
end method sheet-device-parent-transform;


/// Native region, only meaningful on mirrored sheets

// This conses, but you don't much ask for the native region
// of a mirrored sheet...
define method sheet-native-region
    (sheet :: <mirrored-sheet-mixin>) => (region :: <region>)
  transform-region(sheet-native-transform(sheet), sheet-region(sheet))
end method sheet-native-region;


/// Device region

define method sheet-device-region
    (sheet :: <sheet>) => (region :: <region>)
  let parent = sheet-device-parent(sheet);
  region-intersection
    (transform-region(sheet-device-transform(sheet), sheet-region(sheet)),
     sheet-device-region(parent))
end method sheet-device-region;

//--- This assumes that sheet siblings do not overlap
//--- This also assumes that sheet regions are implemented by <bounding-box>
define method sheet-device-region
    (sheet :: <basic-sheet>) => (region :: <region>)
  let region = sheet-cached-device-region(sheet);
  // We decache the device region by setting this slot to #f...
  case
    region == $nowhere		// it can happen
    | (region & ~box-invalidated?(region)) =>
      region;
    region =>
      // Be very careful not to cons.  It's worth all this hair
      // because common operations such as scrolling invalidate
      // this cache all the time.
      let (left, top, right, bottom) = box-edges(sheet);
      let (pleft, ptop, pright, pbottom)
        = begin
	    let parent = sheet-device-parent(sheet);
	    let region = sheet-device-region(parent);
	    if (region == $nowhere)
	      values(0, 0, 0, 0)
	    else
	      box-edges(region)
	    end
	  end;
      let (valid?, left, top, right, bottom)
	= begin
	    let (sleft, stop, sright, sbottom)
	      = transform-box(sheet-device-transform(sheet),
			      left, top, right, bottom);
	    ltrb-intersects-ltrb?(sleft, stop, sright, sbottom,
				  pleft, ptop, pright, pbottom)
	  end;
      if (valid?)
	sheet-cached-device-region(sheet)	// might cons a new region...
	  := set-box-edges(region, left, top, right, bottom)
      else
	sheet-cached-device-region(sheet) := $nowhere
      end;
    otherwise =>
      sheet-cached-device-region(sheet) := next-method();
  end
end method sheet-device-region;

define method sheet-device-region
    (sheet :: <mirrored-sheet-mixin>) => (region :: <region>)
  sheet-native-region(sheet)
end method sheet-device-region;


/// Native edges

// Returns the edges of the <bounding-box> for the sheet's native region
define method sheet-native-edges
    (sheet :: <mirrored-sheet-mixin>)
 => (left :: <integer>, top :: <integer>,
     right :: <integer>, bottom :: <integer>)
  let (left, top, right, bottom) = box-edges(sheet);
  transform-box(sheet-native-transform(sheet), left, top, right, bottom)
end method sheet-native-edges;


/// Mirror region stuff

//--- Note: this doesn't behave like other 'sheet-XXX-edges' functions;
//--- it returns the edges as seen from the device parent, not as seen
//--- from the sheet itself
define method sheet-device-edges
    (sheet :: <sheet>)
 => (left :: <integer>, top :: <integer>,
     right :: <integer>, bottom :: <integer>)
  let (left, top, right, bottom) = box-edges(sheet);
  transform-box(sheet-device-transform(sheet), left, top, right, bottom)
end method sheet-device-edges;

define method sheet-device-edges
    (sheet :: <mirrored-sheet-mixin>)
 => (left :: <integer>, top :: <integer>,
     right :: <integer>, bottom :: <integer>)
  let (left, top, right, bottom) = box-edges(sheet);
  transform-box(sheet-device-parent-transform(sheet), left, top, right, bottom)
end method sheet-device-edges;


// Returns the region for the sheet mirror.
// Default method makes a box from the result of (new) 'mirror-edges'
define method mirror-region
    (_port :: <basic-port>, sheet :: <sheet>, mirror :: <mirror>)
 => (region :: <region>)
  let (left, top, right, bottom) = mirror-edges(_port, sheet, mirror);
  make-bounding-box(left, top, right, bottom)
end method mirror-region;


define method mirror-origin
    (_port :: <basic-port>, sheet :: <sheet>) => (origin)
  #"north-west"
end method mirror-origin;


/// Finding and making mirrors

define method sheet-mirror 
    (sheet :: <sheet>) => (mirror :: false-or(<mirror>))
  sheet-direct-mirror(sheet)
  | begin
      let parent = sheet-device-parent(sheet, error?: #f);
      parent & sheet-direct-mirror(parent)
    end
end method sheet-mirror;

// Returns the ancestor whose mirror will be the parent for this sheet's mirror
// This is for things like Windows group boxes, where the children's mirrors are
// siblings of the group box's mirror (!)
define function sheet-device-parent
    (sheet :: <sheet>, #key error? = #t)
 => (sheet :: false-or(<mirrored-sheet-mixin>))
  let parent = sheet-parent(sheet);
  case
    ~parent =>
      error? & error("The sheet %= has no mirrored ancestors", sheet);
    sheet-mirror-accepts-children?(parent) =>
      parent;
    otherwise =>
      sheet-device-parent(parent, error?: error?)
  end
end function sheet-device-parent;


define method make-mirror
    (_port :: <port>, sheet :: <mirrored-sheet-mixin>) 
 => (mirror :: <mirror>)
  let mirror
    = sheet-direct-mirror(sheet)
      | (sheet-direct-mirror(sheet) := do-make-mirror(_port, sheet));
  // You might think we should update the mirror regions and transforms
  // here, but that will get taken care of by layout and space allocation
  mirror
end method make-mirror;


/// Mirror notifications

define method note-sheet-mapped (sheet :: <mirrored-sheet-mixin>) => ()
  next-method();		// almost certainly the method on <sheet>
  let mirror = sheet-direct-mirror(sheet);
  assert(mirror,
	 "The sheet %= must be mirrored to map it", sheet);
  map-mirror(port(sheet), sheet, mirror)
end method note-sheet-mapped;

define method note-sheet-unmapped (sheet :: <mirrored-sheet-mixin>) => ()
  next-method();		// almost certainly the method on <sheet>
  let mirror = sheet-direct-mirror(sheet);
  assert(mirror,
	 "The sheet %= must be mirrored to unmap it", sheet);
  unmap-mirror(port(sheet), sheet, mirror)
end method note-sheet-unmapped;


define method do-note-sheet-attached (sheet :: <mirrored-sheet-mixin>) => ()
  next-method();
  make-mirror(port(sheet), sheet)
end method do-note-sheet-attached;

define method do-note-sheet-detached (sheet :: <mirrored-sheet-mixin>) => ()
  next-method();
  let _port = port(sheet);
  let mirror = sheet-direct-mirror(sheet);
  when (_port & mirror)
    destroy-mirror(_port, sheet, mirror)
  end
end method do-note-sheet-detached;


// Mirrored sheets will take care of this themselves
define method repaint-within-parent
    (sheet :: <mirrored-sheet-mixin>, #key clear? = #t) => ()
  ignore(clear?);
  #f
end method repaint-within-parent;


/// Mirror geometry notifications

define method note-region-changed
    (sheet :: <mirrored-sheet-mixin>) => ()
  next-method();
  let mirror = sheet-direct-mirror(sheet);
  let _port = port(sheet);
  when (mirror & _port)		// can happen before attachment!
    update-mirror-region(_port, sheet, mirror)
  end
end method note-region-changed;

define method note-transform-changed
    (sheet :: <mirrored-sheet-mixin>) => ()
  next-method();
  let mirror = sheet-direct-mirror(sheet);
  let _port = port(sheet);
  when (mirror & _port)		// can happen before attachment!
    update-mirror-transform(_port, sheet, mirror)
  end
end method note-transform-changed;


//--- Maybe this should get done by 'note-transform-changed',
//--- but we are trying to keep that function cheap...
define method update-all-mirror-positions (sheet :: <sheet>) => ()
  do-sheet-tree(method (sheet)
		  when (sheet-direct-mirror(sheet))
		    note-transform-changed(sheet)
		  end
		end method,
		sheet)
end method update-all-mirror-positions;


// This gets called by 'note-region-changed' on mirrored sheets.
// At this point, the sheet's edges are the desired ones, and we
// have to update the mirror itself
// This default implementation ensures the native (sheet->mirror) 
// transform is the identity transform
define method update-mirror-region
    (_port :: <port>, sheet :: <mirrored-sheet-mixin>, mirror) => ()
  // Get the desired new coordinates in parent space
  let (new-left, new-top, new-right, new-bottom)
    = sheet-device-edges(sheet);
  // Get the current mirror's corrdinates
  let (current-left, current-top, current-right, current-bottom)
    = mirror-edges(port(sheet), sheet, mirror);
  when (new-left  ~= current-left
	| new-top ~= current-top
	| new-right  ~= current-right
	| new-bottom ~= current-bottom)
    set-mirror-edges(port(sheet), sheet, mirror,
		     new-left, new-top, new-right, new-bottom);
    sheet-native-transform(sheet) := $identity-transform
  end
end method update-mirror-region;

// This gets called by 'note-transform-changed' on mirrored sheets.
// At this point, the sheet's position is the desired one, and we
// have to update the mirror itself
// This default implementation ensures the native (sheet->mirror) 
// transform is the identity transform
define method update-mirror-transform
    (_port :: <port>, sheet :: <mirrored-sheet-mixin>, mirror) => ()
  // Get the desired new coordinates in parent space
  let (new-left, new-top, new-right, new-bottom)
    = sheet-device-edges(sheet);
  // Get the current mirror's coordinates
  let (current-left, current-top, current-right, current-bottom)
    = mirror-edges(port(sheet), sheet, mirror);
  ignore(current-right, current-bottom);
  when (new-left  ~= current-left
	| new-top ~= current-top)
    let width  = new-right - new-left;
    let height = new-bottom - new-top;
    set-mirror-edges(port(sheet), sheet, mirror,
		     new-left, new-top, new-left + width, new-top + height);
    sheet-native-transform(sheet) := $identity-transform
  end
end method update-mirror-transform;

// Called from the back-ends, usually via window configuration events
define method note-mirror-geometry-changed
    (_port :: <port>, sheet :: <mirrored-sheet-mixin>, region :: <region>) => ()
  let mirror = sheet-mirror(sheet);
  when (mirror)
    let (new-left :: <integer>, new-top :: <integer>,
	 new-right :: <integer>, new-bottom :: <integer>)
      = begin
	  let (left, top, right, bottom) = box-edges(region);
	  untransform-box(sheet-native-transform(sheet), left, top, right, bottom)
	end;
    let new-width  :: <integer> = new-right  - new-left;
    let new-height :: <integer> = new-bottom - new-top;
    transform-distances!(sheet-transform(sheet), new-width, new-height);
    // This might well cause 'allocate-space' to run, notably when
    // called on a top-level sheet
    set-sheet-edges(sheet, 
		    new-left, new-top,
		    new-left + new-width, new-top + new-height)
  end
end method note-mirror-geometry-changed;


/// Mirror-based repainting

// Unmirrored sheets need to have DUIM call 'handle-repaint' on them
define method port-handles-repaint?
    (port :: <port>, sheet :: <sheet>) => (true? :: <boolean>)
  #f
end method port-handles-repaint?;

// For mirrored sheets, back-ends must implement their own methods!
define method port-handles-repaint?
    (port :: <port>, sheet :: <mirrored-sheet-mixin>) => (true? :: <boolean>)
  error("The back-end must supply a method for 'port-handles-repaint?'");
end method port-handles-repaint?;
