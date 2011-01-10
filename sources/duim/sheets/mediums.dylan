Module:       duim-sheets-internals
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Output Sheets and Mediums

define protocol <<output-sheet-protocol>> (<<sheet-protocol>>)
  // Allocation, attachment, etc.
  function make-medium
    (port :: <abstract-port>, sheet :: <abstract-sheet>)
 => (medium :: <abstract-medium>);
  function allocate-medium
    (port :: <abstract-port>, sheet :: <abstract-sheet>)
 => (medium :: <abstract-medium>);
  function deallocate-medium
    (port :: <abstract-port>, medium :: <abstract-medium>) => ();
  function attach-medium
    (sheet :: <abstract-sheet>, medium :: <abstract-medium>) => ();
  function do-attach-medium
    (sheet :: <abstract-sheet>, medium :: <abstract-medium>) => ();
  function detach-medium
    (sheet :: <abstract-sheet>, medium :: <abstract-medium>) => ();
  function do-detach-medium
    (sheet :: <abstract-sheet>, medium :: <abstract-medium>) => ();
  function destroy-medium (medium :: <abstract-medium>) => ();
  // Drawing surfaces
  getter sheet-medium
    (sheet :: <abstract-sheet>) => (medium :: false-or(<abstract-medium>));
  setter sheet-medium-setter
    (medium :: false-or(<abstract-medium>), sheet :: <abstract-sheet>)
 => (medium :: false-or(<abstract-medium>));
  function do-with-sheet-medium
    (sheet :: <abstract-sheet>, continuation :: <function>) => (#rest values);
end protocol <<output-sheet-protocol>>;

define protocol <<medium-protocol>> ()
  // Accessors
  getter medium-foreground
    (medium :: <medium>) => (foreground :: false-or(<ink>));
  setter medium-foreground-setter
    (foreground :: <ink>, medium :: <medium>)
 => (foreground :: <ink>);
  getter medium-background
    (medium :: <medium>) => (background :: false-or(<ink>));
  setter medium-background-setter
    (background :: <ink>, medium :: <medium>)
 => (background :: <ink>);
  getter medium-text-style
    (medium :: <medium>) => (text-style :: false-or(<text-style>));
  setter medium-text-style-setter
    (text-style :: <text-style>, medium :: <medium>)
 => (text-style :: <text-style>);
  getter medium-default-text-style
    (medium :: <medium>) => (text-style :: false-or(<text-style>));
  setter medium-default-text-style-setter
    (text-style :: <text-style>, medium :: <medium>)
 => (text-style :: <text-style>);
  getter medium-merged-text-style
    (medium :: <medium>) => (text-style :: <text-style>);
  getter medium-brush
    (medium :: <medium>) => (brush :: type-union(<brush>, <ink>));
  setter medium-brush-setter
    (brush :: type-union(<brush>, <ink>), medium :: <medium>)
 => (brush :: type-union(<brush>, <ink>));
  getter medium-pen
    (medium :: <medium>) => (pen :: <pen>);
  setter medium-pen-setter
    (pen :: <pen>, medium :: <medium>) => (pen :: <pen>);
  getter medium-transform
    (medium :: <medium>) => (transform :: <transform>);
  setter medium-transform-setter
    (transform :: <transform>, medium :: <medium>)
 => (transform :: <transform>);
  getter medium-device-transform
    (medium :: <abstract-medium>) => (transform :: <transform>);
  getter medium-clipping-region
    (medium :: <medium>) => (region :: <region>);
  setter medium-clipping-region-setter
    (region :: <region>, medium :: <medium>)
 => (region :: <region>);
  // Drawing surfaces
  getter medium-sheet
    (medium :: <abstract-medium>) => (sheet :: false-or(<abstract-sheet>));
  setter medium-sheet-setter
    (sheet :: false-or(<abstract-sheet>), medium :: <abstract-medium>)
 => (sheet :: false-or(<abstract-sheet>));
  getter medium-drawable
    (medium :: <abstract-medium>) => (drawable);
  setter medium-drawable-setter
    (drawable, medium :: <abstract-medium>) => (drawable);
  getter medium-pixmap
    (medium :: <abstract-medium>) => (pixmap); 
  setter medium-pixmap-setter
    (pixmap, medium :: <abstract-medium>) => (pixmap);
  // Ring the bell...
  function beep (drawable :: type-union(<drawable>, <frame>, <port>)) => ();
  // Drawing options
  function do-with-drawing-options
    (drawable :: <drawable>, function :: <function>, #rest options, #key, #all-keys)
 => (#rest values);
  function do-with-text-style
    (drawable :: <drawable>, function :: <function>, text-style)
 => (#rest values);
  function do-with-transform
    (drawable :: <drawable>, function :: <function>, transform)
 => (#rest values);
  // Like 'force-output' and 'synchronize-output' from Streams...
  // These can be called on sheets, mediums, or ports
  function force-display       (drawable :: type-union(<drawable>, <frame>, <port>)) => ();
  function synchronize-display (drawable :: type-union(<drawable>, <frame>, <port>)) => ();
end protocol <<medium-protocol>>;


/// Output sheets

// This must come before <basic-sheet> in the CPL
define open abstract class <sheet-with-medium-mixin> (<abstract-sheet>)
  sealed slot sheet-medium :: false-or(<medium>) = #f;
end class <sheet-with-medium-mixin>;

define thread-slot sheet-medium :: false-or(<abstract-medium>) of <abstract-sheet>;

// This could be called 'sheet-can-have-medium?'...
define method sheet-has-medium?
    (sheet :: <sheet>) => (true? :: <boolean>)
  #f
end method sheet-has-medium?;

define method sheet-has-medium?
    (sheet :: <sheet-with-medium-mixin>) => (true? :: <boolean>)
  #t
end method sheet-has-medium?;

define method do-destroy-sheet (sheet :: <sheet-with-medium-mixin>) => ()
  let medium = sheet-medium(sheet);
  when (medium)
    destroy-medium(medium)
  end;
  next-method()		// off to <basic-sheet> or <mirrored-sheet-mixin>
end method do-destroy-sheet;

define method invalidate-cached-region (sheet :: <sheet-with-medium-mixin>) => ()
  next-method();	// off to <basic-sheet>...
  let medium = sheet-medium(sheet);
  when (medium)
    invalidate-cached-region(medium)
  end
end method invalidate-cached-region;

define method invalidate-cached-transform (sheet :: <sheet-with-medium-mixin>) => ()
  next-method();	// off to <basic-sheet>...
  let medium = sheet-medium(sheet);
  when (medium)
    invalidate-cached-transform(medium)
  end
end method invalidate-cached-transform;


// This must come before <mirrored-sheet-mixin> in any CPL so that
// the mirror gets created before any medium gets attached.
// This is because the medium's drawable is usually the mirror itself.
define open abstract class <permanent-medium-mixin> (<sheet-with-medium-mixin>)
end class <permanent-medium-mixin>;

// NB: _not_ 'DO-note-sheet-attached' because we want to make sure
// we happen "around" any 'do-note' methods, but the attaching of the
// medium needs to happen after mirrors are created.
define method note-sheet-attached (sheet :: <permanent-medium-mixin>) => ()
  // First call 'next-method' to make sure that the mirror has been
  // realized at this point, if it's a mirrored sheet.  This is pretty
  // horrible but it makes sure that things happen in the right order.
  next-method();
  sheet-medium(sheet) := make-medium(port(sheet), sheet);
  when (sheet-attached?(sheet))
    attach-medium(sheet, sheet-medium(sheet))
  end
end method note-sheet-attached;

// Yes, this really is 'DO-note-sheet-detached'
define method do-note-sheet-detached (sheet :: <permanent-medium-mixin>) => ()
  when (sheet-medium(sheet))
    detach-medium(sheet, sheet-medium(sheet));
    sheet-medium(sheet) := #f
  end
end method do-note-sheet-detached;


// One medium is shared among several closely related sheets (such as
// home-grown scroll bars).  The foreground/background/text-style must
// be the same for all the sheets.  Ditto the transformation.  The sheets
// must even share the same drawable.
define open abstract class <shared-medium-mixin> (<sheet-with-medium-mixin>)
  sealed slot shared-medium-sheet = #f,
    init-keyword: shared-medium-sheet:;
end class <shared-medium-mixin>;

// Note that we do not set up the value of 'sheet-medium' here --
// that will get done in 'do-with-sheet-medium'.
define method do-note-sheet-attached (sheet :: <shared-medium-mixin>) => ()
  next-method();
  unless (shared-medium-sheet(sheet))
    shared-medium-sheet(sheet) := find-sheet-with-medium(sheet)
  end;
end method do-note-sheet-attached;

// If 'sheet' has a medium, return it; otherwise, walk up the ancestors
// of the sheet and return the first ancestor with a medium
define function find-sheet-with-medium
    (sheet :: <sheet>) => (sheet :: <sheet>)
  let parent = begin 
		 for (parent = sheet then sheet-device-parent(parent),
		      until: ~parent | sheet-has-medium?(parent))
		 finally parent
		 end
	       end;
  assert(parent,
	 "The sheet %= has no parent with a medium", sheet);
  parent
end function find-sheet-with-medium;


/// Basic mediums

define constant $medium-brush-cached   :: <integer> = #o01;
define constant $medium-pen-cached     :: <integer> = #o02;
define constant $medium-font-cached    :: <integer> = #o04;
define constant $medium-region-cached  :: <integer> = #o10;
define constant $medium-fully-cached   :: <integer> = #o17;	// the sum of the above

define open abstract primary class <basic-medium> (<medium>)
  sealed slot port :: false-or(<port>) = #f,
    init-keyword: port:,
    setter: %port-setter;
  // Default foreground, background, and text style to #f so that
  // port-specific resources can fill them in appropriately.  Before the
  // medium is attached, fg/bg/text style can be seen to be #f, but real
  // values are filled in when the medium is attached.  There is no
  // interface that sets these values back to #f.
  sealed slot medium-foreground :: false-or(<ink>) = #f,
    init-keyword: foreground:,
    setter: %foreground-setter;
  sealed slot medium-background :: false-or(<ink>) = #f,
    init-keyword: background:,
    setter: %background-setter;
  sealed slot medium-text-style :: false-or(<text-style>) = #f,
    setter: %text-style-setter;
  sealed slot medium-default-text-style :: false-or(<text-style>) = #f,
    setter: %default-text-style-setter;
  sealed slot medium-merged-text-style :: <text-style> = $default-text-style,
    setter: %merged-text-style-setter;
  // Drawing state
  sealed slot medium-brush :: type-union(<standard-brush>, <ink>) = $foreground,
    setter: %brush-setter;
  sealed slot medium-pen :: <standard-pen> = $solid-pen,
    setter: %pen-setter;
  sealed slot medium-clipping-region :: <region> = $everywhere,
    init-keyword: region:,
    setter: %clipping-region-setter;
  sealed slot medium-transform :: <transform> = $identity-transform,
    init-keyword: transform:,
    setter: %transform-setter;
  sealed slot medium-device-transform :: <transform> = $identity-transform;
  sealed slot medium-+Y-upward? :: <boolean> = #f;
  sealed slot medium-sheet :: false-or(<sheet>) = #f,
    init-keyword: sheet:;
  sealed slot medium-drawable = #f,
    init-keyword: drawable:;
  // A pixmap for use in "double buffering"
  sealed slot medium-pixmap = #f;
  // A word of bits telling what bit of drawing state is cached,
  // where 0 means the state is entirely decached
  sealed slot medium-drawing-state-cache :: <integer> = 0,
    setter: %medium-drawing-state-cache-setter;
end class <basic-medium>;

define thread-slot medium-sheet :: false-or(<abstract-sheet>) of <abstract-medium>;
define thread-slot medium-drawable of <abstract-medium>;
define thread-slot medium-brush :: type-union(<brush>, <ink>) of <abstract-medium>;
define thread-slot medium-pen :: <pen> of <abstract-medium>;
define thread-slot medium-text-style :: false-or(<text-style>) of <abstract-medium>;
define thread-slot medium-merged-text-style :: <text-style> of <abstract-medium>;
define thread-slot medium-transform :: <transform> of <abstract-medium>;
define thread-slot medium-device-transform :: <transform> of <abstract-medium>;
define thread-slot medium-clipping-region :: <region> of <abstract-medium>;
define thread-slot medium-+Y-upward? :: <boolean> of <abstract-medium>;

define sealed inline method make
    (class == <medium>, #key port, sheet)
 => (medium :: <medium>)
  make-medium(port, sheet)
end method make;

define sealed method medium-drawing-state-cache-setter
    (state :: <integer>, medium :: <basic-medium>) => (state :: <integer>)
  let old-state :: <integer> = medium-drawing-state-cache(medium);
  unless (state = old-state)
    when (old-state ~= 0)
      invalidate-cached-drawing-state(medium, state)
    end;
    medium.%medium-drawing-state-cache := state
  end;
  state
end method medium-drawing-state-cache-setter;

define method invalidate-cached-drawing-state
    (medium :: <medium>, new-state :: <integer>) => ()
  ignore(new-state);
  #f 
end method invalidate-cached-drawing-state;


define method medium-foreground-setter
    (fg :: <ink>, medium :: <basic-medium>) => (foreground :: <ink>)
  medium-drawing-state-cache(medium)
    := logand(medium-drawing-state-cache(medium), lognot($medium-brush-cached));
  medium.%foreground := fg
end method medium-foreground-setter;

define method medium-background-setter
    (bg :: <ink>, medium :: <basic-medium>) => (background :: <ink>)
  medium-drawing-state-cache(medium)
    := logand(medium-drawing-state-cache(medium), lognot($medium-brush-cached));
  medium.%background := bg
end method medium-background-setter;

define method medium-text-style-setter
    (text-style :: <text-style>, medium :: <basic-medium>) => (text-style :: <text-style>)
  unless (text-style == medium-text-style(medium))
    medium-merged-text-style(medium)
      := merge-text-styles(text-style, medium-default-text-style(medium));
    medium.%text-style := text-style
  end;
  text-style
end method medium-text-style-setter;

define method medium-default-text-style-setter
    (text-style :: <text-style>, medium :: <basic-medium>) => (text-style :: <text-style>)
  unless (text-style == medium-default-text-style(medium))
    when (medium-text-style(medium))
      medium-merged-text-style(medium)
	:= merge-text-styles(medium-text-style(medium), text-style)
    end;
    medium.%default-text-style := text-style
  end;
  text-style
end method medium-default-text-style-setter;

define method medium-merged-text-style-setter
    (text-style :: <text-style>, medium :: <basic-medium>) => (text-style :: <text-style>)
  medium-drawing-state-cache(medium)
    := logand(medium-drawing-state-cache(medium), lognot($medium-font-cached));
  // We don't check, but this better be a fully merged style!
  medium.%merged-text-style := text-style
end method medium-merged-text-style-setter;


define method medium-brush-setter
    (brush :: type-union(<standard-brush>, <ink>), medium :: <basic-medium>)
 => (brush :: type-union(<standard-brush>, <ink>))
  unless (brush == medium-brush(medium))
    medium-drawing-state-cache(medium)
      := logand(medium-drawing-state-cache(medium), lognot($medium-brush-cached));
    medium.%brush := brush
  end;
  brush
end method medium-brush-setter;

define method medium-pen-setter
    (pen :: <standard-pen>, medium :: <basic-medium>)
 => (pen :: <standard-pen>)
  unless (pen == medium-pen(medium))
    medium-drawing-state-cache(medium)
      := logand(medium-drawing-state-cache(medium), lognot($medium-pen-cached));
    medium.%pen := pen
  end;
  pen
end method medium-pen-setter;

define method medium-transform-setter
    (transform :: <transform>, medium :: <basic-medium>) => (transform :: <transform>)
  // The medium's device transform applies the user-level transform first,
  // followed by the sheet's device transform
  medium-device-transform(medium)
    := compose-transforms(sheet-device-transform(medium-sheet(medium)), transform);
  medium.%transform := transform
end method medium-transform-setter;

define method medium-clipping-region-setter
    (region :: <region>, medium :: <basic-medium>) => (region :: <region>)
  medium-drawing-state-cache(medium)
    := logand(medium-drawing-state-cache(medium), lognot($medium-region-cached));
  medium.%clipping-region := region
end method medium-clipping-region-setter;


define method display (medium :: <basic-medium>) => (display :: false-or(<display>))
  let sheet = medium-sheet(medium);
  sheet & display(sheet)
end method display;


define method invalidate-cached-region (medium :: <medium>) => ()
  #f
end method invalidate-cached-region;

define method invalidate-cached-transform (medium :: <medium>) => ()
  medium-device-transform(medium)
    := compose-transforms(sheet-device-transform(medium-sheet(medium)),
			  medium-transform(medium))
end method invalidate-cached-transform;


/// Ports vs. mediums

define method allocate-medium
    (_port :: <basic-port>, sheet :: <sheet>) => (medium :: <medium>)
  if (empty?(_port.%medium-cache))
    make-medium(_port, sheet)
  else
    pop(_port.%medium-cache)
  end
end method allocate-medium;

define method deallocate-medium
    (_port :: <basic-port>, medium :: <medium>) => ()
  medium-sheet(medium) := #f;
  push(_port.%medium-cache, medium)
end method deallocate-medium;

define method destroy-medium (medium :: <medium>) => ()
  #f
end method destroy-medium;


/// Attaching mediums to sheets

// The idea here is to go searching up the sheet hierarchy for a sheet
// with a usable medium.  Once found, we set up its critical state and
// then hope for the best...
define method do-with-sheet-medium
    (sheet :: <sheet>, continuation :: <function>) => (#rest values)
  dynamic-extent(continuation);
  let parent = find-sheet-with-medium(sheet);
  with-sheet-medium (medium = parent)
    let old-sheet = medium-sheet(medium);
    let old-region    = medium-clipping-region(medium);
    let old-transform = medium-device-transform(medium);
    block ()
      medium-sheet(medium) := sheet;
      // Be sure the clipping region and other medium state is decached
      medium-clipping-region(medium)  := $everywhere;
      medium-device-transform(medium) := sheet-device-transform(sheet);
      invalidate-cached-region(medium);
      continuation(medium)
    cleanup
      medium-sheet(medium) := old-sheet;
      medium-clipping-region(medium)  := old-region;
      medium-device-transform(medium) := old-transform;
    end
  end
end method do-with-sheet-medium;

define method do-with-sheet-medium
    (sheet :: <sheet-with-medium-mixin>, continuation :: <function>) => (#rest values)
  dynamic-extent(continuation);
  let medium = sheet-medium(sheet);
  if (medium)
    continuation(medium)
  else
    // Some gadgets won't have a medium while they are being created.
    // Go get one now so that foreground/background can be decoded, etc.
    with-temporary-medium (medium = sheet)
      // Note that we don't call 'detach-medium' because we don't want to
      // release any of the medium's resource (GCs, etc).  This is because
      // the medium is intended for reuse, so we'd just have to go out and
      // get the resources again anyway.
      dynamic-bind (sheet-medium(sheet) = medium)
        attach-medium(sheet, medium);
        continuation(medium)
      end
    end
  end
end method do-with-sheet-medium;

define method do-with-sheet-medium
    (sheet :: <shared-medium-mixin>, continuation :: <function>) => (#rest values)
  dynamic-extent(continuation);
  let medium = sheet-medium(sheet);
  if (medium)
    // Note that we do not go through 'attach-medium' or anything
    // like that, since the sheets are assumed to be closely enough
    // related that 'attach-medium' shouldn't do anything useful.
    // This means, for example, that we might not decache the
    // cached drawing state in the back end.
    dynamic-bind (sheet-medium(sheet) = medium,
		  medium-sheet(medium) = sheet)
      // Be sure the clipping region (and other medium state) is decached
      medium-clipping-region(medium)  := $everywhere;
      medium-device-transform(medium) := sheet-device-transform(sheet);
      continuation(medium)
    end
  else
    next-method()
  end
end method do-with-sheet-medium;


define method attach-medium (sheet :: <sheet>, medium :: <medium>) => ()
  medium-sheet(medium) := sheet;
  medium-drawing-state-cache(medium) := 0;
  do-attach-medium(sheet, medium)
end method attach-medium;

define method do-attach-medium (sheet :: <sheet>, medium :: <medium>) => ()
  #f
end method do-attach-medium;

define method detach-medium (sheet :: <sheet>, medium :: <medium>) => ()
  do-detach-medium(sheet, medium)
end method detach-medium;

define method do-detach-medium (sheet :: <sheet>, medium :: <medium>) => ()
  #f
end method do-detach-medium;


/// Medium-related sheet properties

define method attach-medium
    (sheet :: <sheet>, medium :: <basic-medium>) => ()
  medium-device-transform(medium) := sheet-device-transform(sheet);
  next-method();
  // If we don't have values for fg/bg/text-style, ask the port
  // to fill them in now.  If the user explicitly specified a
  // value, use it.
  let _port = port(sheet);
  medium.%foreground := get-default-foreground(_port, sheet);
  medium.%background := get-default-background(_port, sheet);
  medium.%text-style
    := get-default-text-style(_port, sheet, text-style: medium-text-style(medium));
  medium.%default-text-style
    := get-default-text-style(_port, sheet, text-style: medium-default-text-style(medium));
  medium-merged-text-style(medium)
    := merge-text-styles(medium-text-style(medium), medium-default-text-style(medium))
end method attach-medium;


/// Drawing options

define sealed method do-with-drawing-options
    (medium :: <basic-medium>, continuation :: <function>,
     #key brush, pen, text-style, clipping-region, transform)
 => (#rest values)
  let saved-brush  = medium-brush(medium);
  let saved-pen    = medium-pen(medium);
  let saved-region    = medium-clipping-region(medium);
  let saved-transform = medium-transform(medium);
  let saved-device-transform = medium-device-transform(medium);
  let changed :: <integer> = 0;
  block ()
    // It's a bit ugly to have to dereference the brush here, but the
    // potential performance gain in retaining a cached drawing state
    // is pretty compelling...
    let new-brush
      = if (brush == $foreground) medium-foreground(medium) else brush end;
    let old-brush
      = if (saved-brush == $foreground) medium-foreground(medium) else saved-brush end;
    when (brush & ~(new-brush == old-brush))
      changed := logior(changed, $medium-brush-cached);
      medium.%brush := brush
    end;
    when (pen & ~(pen == saved-pen))
      changed := logior(changed, $medium-pen-cached);
      medium.%pen := pen
    end;
    when (transform)
      medium-transform(medium) := compose-transforms(saved-transform, transform)
    end;
    when (clipping-region & ~(clipping-region == saved-region))
      changed := logior(changed, $medium-region-cached);
      medium.%clipping-region
        := region-intersection
             (saved-region,
              transform-region(medium-transform(medium), clipping-region))
    end;
    when (changed ~= 0)
      medium-drawing-state-cache(medium)
	:= logand(medium-drawing-state-cache(medium), lognot(changed))
    end;
    if (text-style)
      do-with-text-style(medium, continuation, text-style)
    else
      continuation()
    end
  cleanup
    medium.%transform := saved-transform;
    medium-device-transform(medium) := saved-device-transform;
    medium.%clipping-region  := saved-region;
    medium.%pen   := saved-pen;
    medium.%brush := saved-brush;
    when (changed ~= 0)
      medium-drawing-state-cache(medium)
	:= logand(medium-drawing-state-cache(medium), lognot(changed))
    end;
  end
end method do-with-drawing-options;


/// Medium text styles

define sealed method do-with-text-style
    (medium :: <basic-medium>, continuation :: <function>, style :: <text-style>)
 => (#rest values)
  if (~style | style == $null-text-style)
    continuation()
  else
    dynamic-bind (medium-text-style(medium)	// flushes cache
		    = merge-text-styles(style, medium-text-style(medium)))
      continuation()
    end
  end
end method do-with-text-style;


/// Medium transformations

define sealed method do-with-transform
    (medium :: <basic-medium>, continuation :: <function>, transform :: <transform>)
 => (#rest values)
  let saved-transform = medium-transform(medium);
  let saved-device-transform = medium-device-transform(medium);
  block ()
    // That is, apply the new transform first...
    medium-transform(medium) := compose-transforms(saved-transform, transform);
    continuation()
  cleanup
    medium.%transform := saved-transform;
    medium-device-transform(medium) := saved-device-transform;
  end
end method do-with-transform;


// Some default methods

define method beep (medium :: <medium>) => ();
  beep(port(medium))
end method beep;

define method beep (_port :: <port>) => ();
  #f
end method beep;


define method force-display (medium :: <medium>) => ()
  force-display(port(medium))
end method force-display;

define method force-display (_port :: <port>) => ()
  #f
end method force-display;


define method synchronize-display (medium :: <medium>) => ()
  synchronize-display(port(medium))
end method synchronize-display;

define method synchronize-display (_port :: <port>) => ()
  #f
end method synchronize-display;


/// Trampolines from sheets to mediums

define method do-with-drawing-options
    (sheet :: <sheet>, continuation :: <function>, #rest options, #key)
 => (#rest values)
  dynamic-extent(options);
  with-sheet-medium (medium = sheet)
    apply(do-with-drawing-options, medium, continuation, options)
  end
end method do-with-drawing-options;

define method do-with-transform
    (sheet :: <sheet>, continuation :: <function>, transform :: <transform>)
 => (#rest values)
  with-sheet-medium (medium = sheet)
    do-with-transform(medium, continuation, transform)
  end
end method do-with-transform;

define method do-with-text-style
    (sheet :: <sheet>, continuation :: <function>, style :: <text-style>)
 => (#rest values)
  with-sheet-medium (medium = sheet)
    do-with-text-style(medium, continuation, style)
  end
end method do-with-text-style;


define method beep (sheet :: <sheet>) => ();
  beep(port(sheet))
end method beep;

define method force-display (sheet :: <sheet>) => ()
  force-display(port(sheet))
end method force-display;

define method force-display (sheet :: <sheet-with-medium-mixin>) => ()
  with-sheet-medium (medium = sheet)
    force-display(medium)
  end
end method force-display;

define method synchronize-display (sheet :: <sheet>) => ()
  synchronize-display(port(sheet))
end method synchronize-display;

define method synchronize-display (sheet :: <sheet-with-medium-mixin>) => ()
  with-sheet-medium (medium = sheet)
    synchronize-display(medium)
  end
end method synchronize-display;


/// CLEAR-BOX

define method clear-box
    (sheet :: <sheet>, left, top, right, bottom) => ()
  with-sheet-medium (medium = sheet)
    clear-box(medium, left, top, right, bottom)
  end
end method clear-box;

define method clear-box
    (sheet :: <permanent-medium-mixin>, left, top, right, bottom) => ()
  clear-box(sheet-medium(sheet), left, top, right, bottom)
end method clear-box;

define method clear-box*
    (sheet :: <sheet>, region :: <region>) => ()
  when (everywhere?(region))
    region := sheet-region(sheet)
  end;
  let (left, top, right, bottom) = box-edges(region);
  clear-box(sheet, left, top, right, bottom)
end method clear-box*;

define method clear-box*
    (medium :: <basic-medium>, region :: <region>) => ()
  when (everywhere?(region))
    region := sheet-region(medium-sheet(medium))
  end;
  let (left, top, right, bottom) = box-edges(region);
  clear-box(medium, left, top, right, bottom)
end method clear-box*;
