Module:       DUIM-Recording-Internals
Synopsis:     DUIM output recording
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Output recording sheet mixin

// Note that this needs to precede any stream classes (<input-stream-mixin>,
// <output-stream-mixin>) in any class precedence list
define open abstract class <output-recording-mixin> (<abstract-sheet>)
  sealed slot sheet-drawing? :: <boolean> = #t,
    init-keyword: draw?:;
  sealed slot sheet-recording? :: <boolean> = #t,
    init-keyword: record?:;
  // The top level output record
  sealed slot sheet-output-history :: <output-record> = make(<tree-output-history>),
    init-keyword: output-history:,
    setter: %output-history-setter;
  // The currently open output record and its absolute position
  sealed slot sheet-output-record :: false-or(<output-record>) = #f;
  sealed slot sheet-output-record-position = make-point(0, 0);
  sealed slot %medium-state :: false-or(<medium-state>) = #f;
  sealed slot stream-text-output-record :: false-or(<output-record>) = #f;
  sealed slot sheet-highlighted-record :: false-or(<output-record>) = #f;
  sealed slot sheet-redisplay-record :: false-or(<output-record>) = #f;
  sealed slot sheet-redisplaying? = #f;
end class <output-recording-mixin>;

define thread-slot sheet-recording? :: <boolean>;
define thread-slot sheet-drawing?   :: <boolean>;
define thread-slot %output-history  :: <output-record>;
define thread-slot sheet-output-record       :: false-or(<output-record>);
define thread-slot stream-text-output-record :: false-or(<output-record>);
define thread-slot %medium-state :: false-or(<medium-state>);


define method initialize (sheet :: <output-recording-mixin>, #key)
  next-method();
  let (x, y) = point-position(sheet-output-record-position(sheet));
  let history = sheet-output-history(sheet);
  set-sheet-position(history, x, y);
  when (output-history?(history))
    output-history-sheet(history) := sheet
  end
end method initialize;

//--- Yetch, we need this for the 'dynamic-bind' in 'do-with-output-to-output-record'
define method %output-history
    (sheet :: <output-recording-mixin>) => (record :: <output-record>)
  sheet-output-history(sheet)
end method %output-history;

define method sheet-output-history-setter
    (record :: <output-record>, sheet :: <output-recording-mixin>) => (record :: <output-record>)
  sheet.%output-history := record
end method sheet-output-history-setter;

define method sheet-medium-state
    (sheet :: <output-recording-mixin>) => (state :: <medium-state>)
  sheet.%medium-state
  | begin
      let medium = sheet-medium(sheet);
      sheet.%medium-state
        := make(<medium-state>,
                brush: medium-brush(medium),
                pen: medium-pen(medium),
                text-style: medium-merged-text-style(medium),
                clipping-region: medium-clipping-region(medium))
    end
end method sheet-medium-state;

define method sheet-medium-state-setter
    (state :: <medium-state>, sheet :: <output-recording-mixin>) => (state :: <medium-state>)
  sheet.%medium-state := state
end method sheet-medium-state-setter;


// Repainting an output recording sheet replays its history after doing
// any other repainting tasks
define method repaint-sheet
    (sheet :: <output-recording-mixin>, region :: <region>, #key medium, force?) => ()
  next-method();
  repaint-sheet(sheet-output-history(sheet), region,
		medium: medium, force?: force?)
end method repaint-sheet;

// Text recording supplies this method
define method stream-close-text-output-record (sheet :: <output-recording-mixin>) => ()
  #f
end method stream-close-text-output-record;


// Reset the medium state cache and continue
define method do-with-drawing-options
    (sheet :: <output-recording-mixin>, continuation :: <function>, #rest options, #key)
 => (#rest values)
  ignore(continuation, options);
  dynamic-bind (sheet.%medium-state = #f)
    next-method()
  end
end method do-with-drawing-options;

// Reset the medium state cache and continue
define method do-with-text-style
    (sheet :: <output-recording-mixin>, continuation :: <function>, style :: <text-style>)
 => (#rest values)
  ignore(continuation);
  dynamic-bind (sheet.%medium-state = #f)
    next-method()
  end
end method do-with-text-style;


define method do-with-end-of-line-action
    (sheet :: <sheet>, continuation :: <function>, action)
 => (#rest values)
  ignore(action);
  continuation()
end method do-with-end-of-line-action;

define method do-with-end-of-page-action
    (sheet :: <sheet>, continuation :: <function>, action)
 => (#rest values)
  ignore(action);
  continuation()
end method do-with-end-of-page-action;


/// Output record constructors

define variable *output-record-constructors* :: <object-table> = make(<table>);

// Define a constructor function that calls 'make' directly, on the assumption
// that the compiler can optimize a static call to 'make' better than it can
// optimize a call to 'apply make' on a non-constant object.
define macro output-record-constructor-definer
  { define output-record-constructor ?class:name (?arglist:*) ?initargs:* end }
    => { define constant ?class ## "-constructor"
           = method (?arglist)
	       make(?class, ?initargs)
	     end method;
         gethash(*output-record-constructors*, ?class) := ?class ## "-constructor"
       }
end macro output-record-constructor-definer;

// It's only really useful to define output record constructors on composite
// output records -- records that get created with 'do-with-new-output-record'.
// That's because leaf output records get created directly with 'make'.
define method make-output-record (record-class, #rest initargs, #key)
  dynamic-extent(initargs);
  let constructor = gethash(*output-record-constructors*, record-class);
  if (constructor)
    apply(constructor, initargs)
  else
    apply(make, record-class, initargs)
  end
end method make-output-record;


/// Output record creation

define method do-with-output-recording-options
    (sheet :: <output-recording-mixin>, continuation :: <function>,
     #key record? = sheet-recording?(sheet), draw? = sheet-drawing?(sheet))
 => (#rest values)
  dynamic-bind (sheet-recording?(sheet) = record?,
		sheet-drawing?(sheet) = draw?)
    continuation()
  end
end method do-with-output-recording-options;

// Creates a new output record for the sheet, calls the continuation, and
// adds the new record to an appropriate parent
define method do-with-new-output-record
    (sheet :: <output-recording-mixin>, continuation :: <function>, #rest initargs,
     #key record-class = <sequence-record>, constructor, parent, #all-keys)
 => (record :: <output-record>)
  dynamic-extent(initargs);
  with-keywords-removed (initargs = initargs, #[record-class:, constructor:, parent:])
    apply(do-with-new-output-record-1,
	  sheet, continuation, record-class, constructor, parent, initargs)
  end
end method do-with-new-output-record;

// Split this out so internal functions don't re-cons the initargs
define method do-with-new-output-record-1
    (sheet :: <output-recording-mixin>, continuation :: <function>,
     record-class, constructor, parent, #rest initargs, #key, #all-keys)
 => (record :: <output-record>)
  dynamic-extent(initargs);
  let current-record = sheet-output-record(sheet);
  let redisplaying? = sheet-redisplaying?(sheet);
  let new-record
    = redisplaying?
      & current-record
      & apply(find-child-output-record, current-record, record-class, initargs);
  let parent = parent | current-record | sheet-output-history(sheet);
  //---*** Is this really the right value for RECORD-X/Y?
  let (record-x, record-y)
    = transform-position(sheet-device-transform(parent), 0, 0);
  if (new-record)
    // Start the region and transform at "zero", and fix them up later
    copy-display-state(new-record, #f)
  else
    let region = make-bounding-box(0, 0, 0, 0);
    let transform = make-translation-transform(0, 0);
    // Output records that care about the caret position will
    // record the caret position inside of 'initialize'
    new-record := if (constructor)
		    apply(constructor,
			  sheet: sheet,
			  region: region,
			  transform: transform,
			  initargs)
		  else
		    apply(make-output-record,
			  record-class,
			  sheet: sheet,
			  region: region,
			  transform: transform,
			  initargs)
		  end;
  end;
  do-with-output-record(sheet, new-record, continuation, record-x, record-y);
  when (redisplaying?)
    recompute-contents-ok(new-record)
  end;
  // We add the child to its parent after doing everything else, so that
  // calls to 'recompute-contents-ok' inside the dynamic extent of the
  // continuation won't take forever.
  when (parent)
    add-child(parent, new-record)
  end;
  new-record
end method do-with-new-output-record-1;

define method do-with-new-output-record
    (sheet :: <sheet>, continuation :: <function>, #rest initargs, #key)
 => (record :: singleton(#f))
  ignore(initargs);
  continuation(#f);		// no output record for you!
  #f
end method do-with-new-output-record;

//---*** I hope the generic function isn't overly restrictive...
define method outer-stream (sheet :: <sheet>)
  sheet
end method outer-stream;

// Given a sheet and an output record, calls the continuation with the
// appropriate output recording state set up.
define method do-with-output-record
    (sheet :: <output-recording-mixin>, record :: <output-record-element-mixin>,
     continuation :: <function>, record-x, record-y) => ()
  // Close the text record before and after
  stream-close-text-output-record(outer-stream(sheet));
  let current-position = sheet-output-record-position(sheet);
  dynamic-bind (point-x(current-position) = record-x,
		point-y(current-position) = record-y,
		sheet-output-record(sheet) = record)
    continuation(record);
    // Close the any text record here.  Records that care about
    // the caret position should also record the end caret position.
    stream-close-text-output-record(outer-stream(sheet))
  end
end method do-with-output-record;

//--- Pretty yucky that we have to do this here
define thread-slot point-x :: <real> of <standard-point>;
define thread-slot point-y :: <real> of <standard-point>;

// Creates a new output record for the sheet, calls the continuation, but does
// not add the record to the sheet
define method do-with-output-to-output-record
    (sheet :: <output-recording-mixin>, continuation :: <function>, #rest initargs, #key)
 => (record :: <output-record>)
  dynamic-extent(initargs);
  with-output-recording-options (sheet, draw?: #f, record?: #t)
    dynamic-bind (sheet.%output-history = make(<sequence-output-history>),
		  sheet-output-record(sheet) = #f,
		  stream-text-output-record(sheet) = #f)
      apply(do-with-new-output-record, sheet, continuation, initargs)
    end
  end
end method do-with-output-to-output-record;

define method do-with-output-to-output-record
    (sheet :: <sheet>, continuation :: <function>, #rest initargs, #key)
 => (record :: singleton(#f))
  ignore(initargs);
  continuation(#f);		// no output record for you!
  #f
end method do-with-output-to-output-record;


define method add-output-record (sheet :: <output-recording-mixin>, record)
  let output-record = sheet-output-record(sheet) | sheet-output-history(sheet);
  add-child(output-record, record)
end method add-output-record;

define method clear-output-history (sheet :: <output-recording-mixin>)
  when (sheet-output-history(sheet))
    sheet-children(sheet-output-history(sheet)) := #[]
  end;
  stream-text-output-record(sheet) := #f;
  sheet-highlighted-record(sheet) := #f
end method clear-output-history;


/// Top level output histories

define open abstract class <output-history-mixin> (<abstract-sheet>)
  sealed slot output-history-sheet :: false-or(<sheet>) = #f,
    init-keyword: sheet:;
end class <output-history-mixin>;

define method output-history? (record :: <output-history-mixin>) #t end;
define method output-history? (record) #f end;

// If someone sets the output history, be sure the sheet and the new
// output history stay in sync.  Leave it to a higher level to clear
// the sheet's viewport, reset the drawing/recording state, etc.
define method sheet-output-history-setter
    (record :: <output-history-mixin>, sheet :: <output-recording-mixin>)
 => (record :: <output-record>)
  next-method();
  output-history-sheet(record) := sheet;
  record
end method sheet-output-history-setter;


define method note-region-changed
    (record :: <output-history-mixin>) => ()
  next-method();
  let (rleft, rtop, rright, rbottom) = box-edges(record);
  // Top-level output records must not have their upper left corner
  // any "later" than (0,0), or else scroll bars and scrolling will
  // not do the right thing.
  // Note the implicit assumption that histories are top-level records,
  // which strikes me as pretty reasonable...
  let left = min(rleft, 0);
  let top  = min(rtop, 0);
  let right  = rright;
  let bottom = rbottom;
  let changed? = (rleft ~= left) | (rtop ~= top);
  when (changed?)
    sheet-region(record)
      := set-box-edges(sheet-region(record), left, top, right, bottom)
  end;
  let sheet = output-history-sheet(record);
  let viewport = sheet & sheet-viewport(sheet);
  when (viewport)
    let (sleft, stop, sright, sbottom) = box-edges(sheet);
    // Update the sheet's region, and then the scroll bars
    when (left  < sleft
	  | top < stop
	  | right  > sright
	  | bottom > sbottom)
      min!(left, sleft);
      min!(top,  stop);
      max!(right,  sright);
      max!(bottom, sbottom);
      sheet-region(sheet) :=		// might cons a new region...
	set-box-edges(sheet-region(sheet), left, top, right, bottom);
      note-region-changed(sheet)
    end;
    update-scroll-bars(viewport)
  end
end method note-region-changed;


/// Graphics

define function sheet-caret-position (sheet :: <sheet>) => (x, y)
  let caret = sheet-caret(sheet);
  if (caret)
    caret-position(caret)
  else
    values(0, 0)
  end
end function sheet-caret-position;

define function set-sheet-caret-position (sheet :: <sheet>, x, y) => ()
  let caret = sheet-caret(sheet);
  when (caret)
    set-caret-position(caret, x, y)
  end
end function set-sheet-caret-position;

// Establish a local +Y-downward coordinate system at the current caret position,
// and execute the body
define method do-with-local-coordinates
    (sheet :: <output-recording-mixin>, continuation :: <function>, #key x, y)
 => (#rest values)
  let medium = sheet-medium(sheet);
  let (cx, cy) = if (x & y) values(x, y) else sheet-caret-position(sheet) end;
  let (tx, ty) = transform-position(medium-transform(medium), 0, 0);
  let transform = make-translation-transform(cx - tx, cy - ty);
  with-transform (sheet, transform)
    continuation()
  end
end method do-with-local-coordinates;

// Establish a local +Y-upward coordinate system at the current caret position,
// and execute the body
define method do-with-first-quadrant-coordinates
    (sheet :: <output-recording-mixin>, continuation :: <function>, #key x, y)
 => (#rest values)
  let medium = sheet-medium(sheet);
  let (cx, cy) = if (x & y) values(x, y) else sheet-caret-position(sheet) end;
  let (tx, ty) = transform-position(medium-transform(medium), 0, 0);
  let transform = if (medium-+Y-upward?(medium))
                    $identity-transform
                  else
                    make-transform(1, 0, 0, -1, cx - tx, cy - ty)
                  end;
  with-transform (sheet, transform)
    dynamic-bind (medium-+Y-upward?(medium) = #t)
      continuation()
    end
  end
end method do-with-first-quadrant-coordinates;

define method do-with-room-for-graphics
    (sheet :: <output-recording-mixin>, continuation :: <function>,
     #key x, y, height, first-quadrant? = #t, move-caret? = #t,
          record-class = <sequence-record>)
 => (record :: <output-record>)
  let record
    = if (first-quadrant?)
        with-output-recording-options (sheet, draw?: #f, record?: #t)
          with-first-quadrant-coordinates (sheet, x: x, y: y)
            with-new-output-record (record = sheet, record-class: record-class)
              ignore(record);
              continuation()
            end
          end
        end
      else
        with-output-recording-options (sheet, draw?: #f, record?: #t)
          with-local-coordinates (sheet, x: x, y: y)
            with-new-output-record (record = sheet, record-class: record-class)
              ignore(record);
              continuation()
            end
          end
        end
      end;
  let (x, y) = sheet-position(record);
  when (height)
    inc!(y, height - box-height(record))
  end;
  set-sheet-position(record, x, y);	//---*** just mung the transform?
  recompute-region(record);		//---*** do we really need to do this?
  when (sheet-drawing?(sheet))
    repaint-sheet(record, $everywhere)
  end;
  when (move-caret?)
    move-caret-beyond-output-record(sheet, record)
  end;
  record
end method do-with-room-for-graphics;

define method do-with-room-for-graphics
    (sheet :: <sheet>, continuation :: <function>, #rest keys, #key)
 => (record :: singleton(#f))
  ignore(keys);
  continuation();
  #f
end method do-with-room-for-graphics;

define method move-caret-beyond-output-record
    (sheet :: <sheet>, record :: <output-record>) => ()
  let (left, top, right, bottom) = box-edges(record);
  ignore(left, top);
  let (x, y) = transform-position(sheet-device-transform(record), right, bottom);
  with-end-of-page-action (sheet, #"allow")
    set-sheet-caret-position(sheet, x, y - sheet-line-height(sheet))
  end
end method move-caret-beyond-output-record;


/// Erasing

define method erase-output-record
    (record :: <output-record>, sheet :: <output-recording-mixin>) => ()
  let parent = sheet-parent(record);
  let medium = sheet-medium(sheet);
  let (left, top, right, bottom) = box-edges(record);
  let transform = sheet-device-transform(record);
  transform-coordinates!(transform, left, top, right, bottom);
  with-drawing-options(medium, brush: $background)
    if (left = right | top = bottom)
      // Handle specially, since a thin line is wider than a
      // rectangle of zero width or height
      draw-line(medium, left, top, right, bottom)
    else
      draw-rectangle(medium, left, top, right, bottom, filled?: #t)
    end
  end;
  when (parent)
    remove-child(parent, record)
  end;
  // Use the output record itself as the repaint region, and repaint
  // the stuff that might have been obscured by the erased output.
  // Note that 'repaint-sheet' sets up a clipping region using the
  // bbox of the original record, so we don't end up erroneously
  // redrawing anything outside the erasure region (which could
  // clobber useful output).
  //---*** This assumes 'repaint-sheet' sets up a clipping region.
  //---*** Does it really?
  repaint-sheet(record, record)
end method erase-output-record;

// Like the above, except it erases a while set of output records in one
// go, then replays the "damaged" reason just once.  This is much faster
// than calling 'erase-output-record' multiple times on single records.
define method erase-output-record
    (records :: <sequence>, sheet :: <output-recording-mixin>)
  let bl = $largest-coordinate;
  let bt = $largest-coordinate;
  let br = $smallest-coordinate;
  let bb = $smallest-coordinate;
  let medium = sheet-medium(sheet);
  dynamic-bind (medium-transform(medium) = $identity-transform,
		medium-brush(medium) = $background)
    for (record in records)
      let parent = sheet-parent(record);
      let (left, top, right, bottom) = box-edges(record);
      let transform = sheet-device-transform(record);
      transform-coordinates!(transform, left, top, right, bottom);
      min!(bl, left);
      min!(bt, top);
      max!(br, right);
      max!(bb, bottom);
      if (left = right | top = bottom)
	draw-line(medium, left, top, right, bottom)
      else
	draw-rectangle(medium, left, top, right, bottom, filled?: #t)
      end;
      when (parent)
        remove-child(parent, record)
      end
    end
  end;
  repaint-sheet(sheet, make-bounding-box(bl, bt, br, bb))
end method erase-output-record;


/// Fit for a king...

define open abstract class <recording-pane>
    (<output-recording-mixin>, <drawing-pane>)
end class <recording-pane>;

define method clear-output-history (pane :: <recording-pane>) => ()
  next-method();
  clear-box*(pane, sheet-viewport-region(pane))
end method clear-output-history;

define sealed class <concrete-recording-pane> (<recording-pane>)
end class <concrete-recording-pane>;

define sealed inline method make
    (class == <recording-pane>, #rest initargs, #key, #all-keys)
 => (pane :: <concrete-recording-pane>)
  apply(make, <concrete-recording-pane>, initargs)
end method make;

define sealed domain make (singleton(<concrete-recording-pane>));
define sealed domain initialize (<concrete-recording-pane>);
