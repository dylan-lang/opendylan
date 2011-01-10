Module:       duim-sheets-internals
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Sheets

// The basic sheet protocol
define protocol <<sheet-protocol>> ()
  // Sheet region
  getter sheet-region
    (sheet :: <abstract-sheet>) => (region :: <region>);
  setter sheet-region-setter
    (region :: <region>, sheet :: <abstract-sheet>) => (region :: <region>);
  function note-region-changed
    (sheet :: <abstract-sheet>) => ();
  // Sheet transform
  getter sheet-transform
    (sheet :: <abstract-sheet>) => (transform :: <transform>);
  setter sheet-transform-setter
    (transform :: <transform>, sheet :: <abstract-sheet>) => (transform :: <transform>);
  function note-transform-changed
    (sheet :: <abstract-sheet>) => ();
  function sheet-delta-transform
    (sheet :: <abstract-sheet>, ancestor :: <abstract-sheet>) => (transform :: <transform>);
  // Sheet mapping
  getter sheet-mapped?
    (sheet :: <abstract-sheet>) => (mapped? :: <boolean>);
  setter sheet-mapped?-setter
    (mapped? :: <boolean>, sheet :: <abstract-sheet>, #key do-repaint?, clear?)
 => (mapped? :: <boolean>);
  getter sheet-withdrawn?
    (sheet :: <abstract-sheet>) => (withdrawn? :: <boolean>);
  setter sheet-withdrawn?-setter
    (withdrawn? :: <boolean>, sheet :: <abstract-sheet>, #key do-repaint?, clear?)
 => (withdrawn? :: <boolean>);
  function note-sheet-mapped (sheet :: <abstract-sheet>) => ();
  function note-sheet-unmapped (sheet :: <abstract-sheet>) => ();
  getter sheet-layed-out?
    (sheet :: <abstract-sheet>) => (layed-out? :: <boolean>);
  setter sheet-layed-out?-setter
    (layed-out? :: <boolean>, sheet :: <abstract-sheet>)
 => (layed-out? :: <boolean>);
  // Cursors
  getter sheet-cursor
    (sheet :: <abstract-sheet>) => (cursor :: <cursor>);
  setter sheet-cursor-setter
    (cursor :: <cursor>, sheet :: <abstract-sheet>) => (cursor :: <cursor>);
  // Carets
  getter sheet-caret
    (sheet :: <abstract-sheet>) => (caret :: type-union(<caret>, one-of(#f, #t)));
  // Focus
  getter sheet-input-focus
    (sheet :: <abstract-sheet>) => (focus :: false-or(<abstract-sheet>));
  getter sheet-accepts-focus?
    (sheet :: <abstract-sheet>) => (focus? :: <boolean>);
  setter sheet-accepts-focus?-setter
    (focus? :: <boolean>, sheet :: <abstract-sheet>) => (focus? :: <boolean>);
  // Clearing
  function clear-box
    (drawable :: <drawable>, left, top, right, bottom) => ();
  function clear-box*
    (drawable :: <drawable>, region :: <region>) => ();
  // Destruction
  function destroy-sheet (sheet :: <abstract-sheet>) => ();
  function do-destroy-sheet (sheet :: <abstract-sheet>) => ();
  // Help
  getter sheet-help-context
    (sheet :: <abstract-sheet>) => (context);
  setter sheet-help-context-setter
    (context, sheet :: <abstract-sheet>) => (context);
  getter sheet-help-source
    (sheet :: <abstract-sheet>) => (locator);
  setter sheet-help-source-setter
    (locator, sheet :: <abstract-sheet>) => (locator);
end protocol <<sheet-protocol>>;

// Sheet genealogy
define protocol <<sheet-genealogy-protocol>> (<<sheet-protocol>>)
  getter sheet-parent
    (sheet :: <abstract-sheet>) => (parent :: false-or(<abstract-sheet>));
  setter sheet-parent-setter
    (parent :: false-or(<abstract-sheet>), sheet :: <abstract-sheet>)
 => (parent :: false-or(<abstract-sheet>));
  function sheet-ancestor?
    (sheet :: <abstract-sheet>, putative-ancestor :: <abstract-sheet>)
 => (true? :: <boolean>);
  getter sheet-children
    (sheet :: <abstract-sheet>) => (children :: <sequence>);
  setter sheet-children-setter
    (children :: <sequence>, sheet :: <abstract-sheet>)
 => (children :: <sequence>);
  getter sheet-child
    (sheet :: <abstract-sheet>) => (child :: false-or(<abstract-sheet>));
  setter sheet-child-setter
    (child :: false-or(<abstract-sheet>), sheet :: <abstract-sheet>)
 => (child :: false-or(<abstract-sheet>));
  // Adding a child sheet
  function add-child
    (sheet :: <abstract-sheet>, child :: <abstract-sheet>, #key index, #all-keys)
 => (sheet :: <abstract-sheet>);
  function do-add-child
    (sheet :: <abstract-sheet>, child :: <abstract-sheet>, #key index, #all-keys) => ();
  function note-child-added
    (sheet :: <abstract-sheet>, child :: <abstract-sheet>) => ();
  // Removing a child sheet
  function remove-child
    (sheet :: <abstract-sheet>, child :: <abstract-sheet>)
 => (sheet :: <abstract-sheet>);
  function do-remove-child
    (sheet :: <abstract-sheet>, child :: <abstract-sheet>) => ();
  function note-child-removed
    (sheet :: <abstract-sheet>, child :: <abstract-sheet>) => ();
  // Replacing a child
  function replace-child
    (sheet :: <abstract-sheet>, old-child :: <abstract-sheet>, new-child :: <abstract-sheet>)
 => (sheet :: <abstract-sheet>); 
  function do-replace-child
    (sheet :: <abstract-sheet>, old-child :: <abstract-sheet>, new-child :: <abstract-sheet>) => ();
  // Traversing children
  function child-containing-position
    (sheet :: <abstract-sheet>, x :: <real>, y :: <real>)
 => (sheet :: false-or(<abstract-sheet>));
  function do-children-containing-position
    (function :: <function>, sheet :: <abstract-sheet>, x :: <real>, y :: <real>) => ();
  function children-overlapping-region
    (sheet :: <abstract-sheet>, region :: <region>) => (sheets :: <sequence>);
  function do-children-overlapping-region
    (function :: <function>, sheet :: <abstract-sheet>, region :: <region>) => ();
  // Raising and lowering
  function raise-sheet
    (sheet :: <abstract-sheet>, #key activate?) => (sheet :: <abstract-sheet>);
  function do-raise-sheet
    (parent :: <abstract-sheet>, sheet :: <abstract-sheet>, #key activate? = #t) => ();
  function lower-sheet
    (sheet :: <abstract-sheet>) => (sheet :: <abstract-sheet>);
  function do-lower-sheet
    (parent :: <abstract-sheet>, sheet :: <abstract-sheet>) => ();
end protocol <<sheet-genealogy-protocol>>;


// Cached geometry
define open generic invalidate-cached-regions (sheet :: <abstract-sheet>) => ();
define open generic invalidate-cached-region  (drawable :: <drawable>) => ();

define open generic invalidate-cached-transforms (sheet :: <abstract-sheet>) => ();
define open generic invalidate-cached-transform  (drawable :: <drawable>) => ();

define open generic invalidate-cached-drawing-state
    (medium :: <medium>, new-state :: <integer>) => ();


/// General accessors

define open generic port
    (object) => (port :: false-or(<abstract-port>));
define open generic port-setter
    (port :: false-or(<abstract-port>), object)
 => (port :: false-or(<abstract-port>));
define open generic %port-setter	//--- sigh...
    (port :: false-or(<abstract-port>), object)
 => (port :: false-or(<abstract-port>));

define open generic display
    (object) => (display :: false-or(<abstract-display>));
define open generic display-setter
    (display :: false-or(<abstract-display>), object)
 => (display :: false-or(<abstract-display>));

define open generic sheet-frame
    (sheet :: <abstract-sheet>) => (frame :: false-or(<abstract-frame>));

define open generic sheet-frame-setter
    (frame :: false-or(<abstract-frame>), sheet :: <abstract-sheet>)
 => (frame :: false-or(<abstract-frame>));

define open generic frame-manager
    (object) => (framem :: false-or(<abstract-frame-manager>));
define open generic frame-manager-setter
    (framem :: false-or(<abstract-frame-manager>), object)
 => (framem :: false-or(<abstract-frame-manager>));

define open generic top-level-sheet
    (object) => (sheet :: false-or(<abstract-sheet>));
define open generic top-level-sheet-setter
    (sheet :: false-or(<abstract-sheet>), object) => (sheet :: false-or(<abstract-sheet>));

define constant <z-order> = one-of(#f, #"top-down", #"bottom-up");

define open generic do-sheet-children
    (function :: <function>, sheet :: <abstract-sheet>, #key z-order :: <z-order>) => ();
define open generic do-sheet-tree
    (function :: <function>, sheet :: <abstract-sheet>) => ();

define open generic sheet-shell
    (sheet :: <abstract-sheet>)
 => (sheet :: false-or(<abstract-sheet>));


/// Decoding the sheet flags

// Bits 0..1 are the sheet's state
define constant %sheet_mapped_mask :: <integer>	= #o03;
define constant %sheet_withdrawn   :: <integer> = #o00;
define constant %sheet_managed     :: <integer> = #o01;
define constant %sheet_mapped      :: <integer> = #o02;

define constant $sheet-states :: <simple-object-vector>
    = #[#"withdrawn", #"managed", #"mapped", #"unknown"];

// Bit 2 is the "layed out" flag
define constant %layed_out :: <integer> = #o04;

// Bits 3..5 are the event mask
define constant %sheet_event_mask    :: <integer> = #o70;
define constant %pointer_motion_mask :: <integer> = #o10;
define constant %pointer_button_mask :: <integer> = #o20;
define constant %keyboard_mask       :: <integer> = #o40;

// Bits 6..11 are the pointer cursor type
define constant %pointer_cursor_shift :: <integer> = 6;
define constant %pointer_cursor_mask  :: <integer> = #o7700;

// Bits 12..13 are reserved for box layouts (see layouts/box-pane)
// define constant %equalize_widths  :: <integer> = #o10000;
// define constant %equalize_heights :: <integer> = #o20000;

// Bit 14 is the "mirror accepts children" flag
define constant %mirror_accepts_children :: <integer> = #o40000;

// Bits 15..16 are reserved for fixed space requirements (see layouts/layout)
// define constant %fixed_width  :: <integer> = #o100000;
// define constant %fixed_height :: <integer> = #o200000;

// Bit 17 is presently unused
// define constant %presently_unused :: <integer> = #o400000;

// Bit 18 and 19 are the "accepts focus" and "keyboard navigation tab stop" flags
define constant %accepts_focus :: <integer> = #o1000000;
define constant %tab_stop      :: <integer> = #o2000000;

define constant $initial-sheet-flags :: <integer>
    = logior(%sheet_managed,
	     %pointer_motion_mask, %pointer_button_mask, %keyboard_mask,
	     %mirror_accepts_children, %accepts_focus, %tab_stop);

// Note: these are intentionally not open
define generic sheet-flags
    (sheet :: <sheet>) => (flags :: <integer>);
define generic sheet-flags-setter
    (flags :: <integer>, sheet :: <sheet>) => (flags :: <integer>);


/// Basic sheet implementation class

define open abstract primary class <basic-sheet> (<sheet>)
  sealed slot sheet-parent :: false-or(<sheet>) = #f,
    setter: %parent-setter;
  sealed slot sheet-region :: <region> = $nowhere,
    init-keyword: region:,
    setter: %region-setter;
  sealed slot sheet-transform :: <transform> = $identity-transform,
    init-keyword: transform:,
    setter: %transform-setter;
  sealed slot sheet-cached-device-region :: false-or(<region>) = #f;
  sealed slot sheet-cached-device-transform :: false-or(<transform>) = #f;
  // This is the port to which the sheet has been grafted
  //--- This should really be a %display slot (but see <basic-display>)
  sealed slot port :: false-or(<port>) = #f,
    init-keyword: port:,
    setter: %port-setter;
  // #t if the sheet (and it's mirror, if it has one) has been mapped,
  // #f otherwise (by "mapped" we mean "visible on the display" -- ignoring
  // issues of occlusion, of course)
  sealed slot sheet-flags :: <integer> = $initial-sheet-flags;
  // Default style, for when we create mediums and mirrors
  sealed slot %style-descriptor :: false-or(<style-descriptor>) = #f,
    init-keyword: style-descriptor:;
  // Help contexts and locators
  // We store these as class slots in separate tables because we expect the
  // number of UI objects that have distinct contexts and locators to be small.
  // Pretending that these are slots lets users use 'help-context: #"foo"'
  //--- Maybe just add a 'sheet-properties' slots for these and for documentation?
  virtual slot sheet-help-context,
    init-keyword: help-context:;
  class slot %help-contexts :: <object-table> = make(<table>, weak: #"key"),
    setter: #f;
  virtual slot sheet-help-source,
    init-keyword: help-source:;
  class slot %help-sources :: <object-table> = make(<table>, weak: #"key"),
    setter: #f;
end class <basic-sheet>;

define method initialize (sheet :: <sheet>, #key parent)
  next-method();
  when (parent)
    sheet-parent(sheet) := parent
  end
end method initialize;

define constant $default-sheet-size :: <integer> = 100;

// The flag initializations here need to be consonant with $initial-sheet-flags
define method initialize
    (sheet :: <basic-sheet>,
     #key x, y, width, height, region, transform,
	  withdrawn?, accepts-focus? = #t, tab-stop? = #t, cursor,
          foreground, background, text-style,
	  help-context, help-source)
  // Be forgiving of things that pass in #f by mistake...
  initialize-sheet-geometry
    (sheet,
     x | 0, y | 0, 
     width | $default-sheet-size, height | $default-sheet-size,
     region: region, transform: transform);
  when (foreground | background | text-style)
    sheet.%style-descriptor
      := make(<style-descriptor>,
	      foreground: foreground,
	      background: background,
	      text-style: text-style)
  end;
  unless (accepts-focus?)
    // If this sheet doesn't accept the focus, turn off the flag
    sheet-flags(sheet)
      := logand(sheet-flags(sheet), lognot(%accepts_focus))
  end;
  unless (tab-stop?)
    // Ditto for the "tab stop" flag
    sheet-flags(sheet)
      := logand(sheet-flags(sheet), lognot(%tab_stop))
  end;
  when (cursor)
    let index = position($pointer-cursors, cursor) | 0;
    sheet-flags(sheet)
      := logior(logand(sheet-flags(sheet), lognot(%pointer_cursor_mask)),
		ash(index, %pointer_cursor_shift))
  end;
  next-method();
  when (withdrawn?)
    sheet-withdrawn?(sheet) := #t
  end;
  when (help-context)
    gethash(sheet.%help-contexts, sheet) := help-context
  end;
  when (help-source)
    gethash(sheet.%help-sources, sheet) := help-source
  end
end method initialize;


define open generic initialize-sheet-geometry
    (sheet :: <abstract-sheet>,
     x :: <integer>, y :: <integer>, width :: <integer>, height :: <integer>,
     #key region, transform) => ();

define sealed inline method initialize-sheet-geometry
    (sheet :: <basic-sheet>,
     x :: <integer>, y :: <integer>, width :: <integer>, height :: <integer>,
     #key region, transform) => ()
  unless (region)
    sheet.%region := make-bounding-box(0, 0, width, height)
  end;
  unless (transform)
    sheet.%transform := make-translation-transform(x, y)
  end
end method initialize-sheet-geometry;


define method destroy-sheet (sheet :: <sheet>) => ()
  local method destroy (sheet :: <sheet>)
	  do(destroy, sheet-children(sheet));
	  do-destroy-sheet(sheet)
	end method;
  // First remove the sheet from its parent, then destroy the
  // hierarchy under the sheet from the bottom up.  We are
  // relying on the fact that removing the top sheet from its
  // parent will degraft all the descendent sheets.
  let parent = sheet-parent(sheet);
  when (parent)
    remove-child(parent, sheet)
  end;
  destroy(sheet)
end method destroy-sheet;

define method do-destroy-sheet (sheet :: <sheet>) => ()
  #f
end method do-destroy-sheet;

define method do-destroy-sheet (sheet :: <basic-sheet>) => ()
  remhash(sheet.%help-contexts, sheet);
  remhash(sheet.%help-sources, sheet)
end method do-destroy-sheet;


define method sheet-parent-setter
    (parent :: <sheet>, sheet :: <sheet>) => (parent :: <sheet>)
  add-child(parent, sheet);
  parent
end method sheet-parent-setter;

define method sheet-parent-setter
    (parent == #f, sheet :: <sheet>) => (parent :: singleton(#f))
  remove-child(sheet-parent(sheet), sheet);
  parent
end method sheet-parent-setter;


// By default, sheets have no children
define method sheet-children (sheet :: <sheet>) => (children :: <sequence>)
  #[]
end method sheet-children;


define method sheet-region-setter
    (region :: <region>, sheet :: <basic-sheet>) => (region :: <region>)
  sheet.%region := region;
  note-region-changed(sheet);
  region
end method sheet-region-setter;

define method sheet-transform-setter
    (transform :: <transform>, sheet :: <basic-sheet>) => (transform :: <transform>)
  sheet.%transform := transform;
  note-transform-changed(sheet);
  transform
end method sheet-transform-setter;

// Returns the edges of the sheet in the sheet's own coordinate space
// Note that this is different from calling 'sheet-edges' on a sheet!
define method box-edges
    (sheet :: <sheet>)
 => (left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>)
  box-edges(sheet-region(sheet))
end method box-edges;


define sealed method sheet-state
    (sheet :: <basic-sheet>) => (state)
  let state = logand(sheet-flags(sheet), %sheet_mapped_mask);
  $sheet-states[state]
end method sheet-state;

define sealed method sheet-state-setter
    (state, sheet :: <basic-sheet>,
     #rest keys, #key do-repaint?, clear?) => (state)
  ignore(do-repaint?, clear?);
  select (state)
    #"mapped"    => apply(sheet-mapped?-setter,    #t, sheet, keys);
    #"managed"   => apply(sheet-mapped?-setter,    #f, sheet, keys);
    #"withdrawn" => apply(sheet-withdrawn?-setter, #t, sheet, keys);
  end;
  state
end method sheet-state-setter;

// Is the sheet mapped, i.e., is both grafted and visible on the display device
// (ignoring issues of occluding windows)?
define sealed inline method sheet-mapped?
    (sheet :: <basic-sheet>) => (mapped? :: <boolean>)
  logand(sheet-flags(sheet), %sheet_mapped_mask) = %sheet_mapped
end method sheet-mapped?;

// Map sheets from the bottom up
// This can only move a sheet tree from the "managed" to the "mapped" state
define method sheet-mapped?-setter
    (mapped? == #t, sheet :: <basic-sheet>,
     #key do-repaint? = #t, clear? = do-repaint?)
 => (mapped? :: <boolean>)
  //--- We should really do something like this to protect unwary users
  /* assert(sheet-mapped?(sheet-parent(sheet)),
	    "Attempting to map %= whose parent %= is unmapped",
	    sheet, sheet-parent(sheet)); */
  let flags = sheet-flags(sheet);
  let state = logand(flags, %sheet_mapped_mask);
  unless (state = %sheet_withdrawn)	// sheet is withdrawn, stop here
    for (child :: <basic-sheet> in sheet-children(sheet))
      sheet-mapped?(child, do-repaint?: #f) := #t
    end;
    when (state = %sheet_managed)
      sheet-flags(sheet)
	:= logior(logand(flags, lognot(%sheet_mapped_mask)), %sheet_mapped);
      note-sheet-mapped(sheet);
      when (do-repaint? & sheet-handles-repaint?(sheet))
	repaint-within-parent(sheet, clear?: clear?)
      end
    end
  end;
  #t
end method sheet-mapped?-setter;

define method note-sheet-mapped (sheet :: <sheet>) => ()
  #f
end method note-sheet-mapped;

define open generic repaint-within-parent
    (sheet :: <abstract-sheet>, #key clear?) => ();

//---*** Suspicious! Should maybe use 'sheet-device-parent'
define method repaint-within-parent
    (sheet :: <sheet>, #key clear? = #t) => ()
  let parent = sheet-parent(sheet);
  when (sheet-mapped?(parent))
    let region = transform-region(sheet-transform(sheet), sheet-region(sheet));
    when (clear?)
      clear-box*(parent, region)
    end;
    repaint-sheet(parent, region)
  end
end method repaint-within-parent;


// Unmap sheets from the top down
// This can only move a sheet tree from the "mapped" to the "managed" state
define method sheet-mapped?-setter
    (mapped? == #f, sheet :: <basic-sheet>, 
     #key do-repaint? = #t, clear? = do-repaint?)
 => (mapped? :: <boolean>)
  let flags = sheet-flags(sheet);
  let state = logand(flags, %sheet_mapped_mask);
  unless (state = %sheet_withdrawn)	// sheet is withdrawn, stop here
    when (state = %sheet_mapped)
      sheet-flags(sheet)
	:= logior(logand(flags, lognot(%sheet_mapped_mask)), %sheet_managed);
      note-sheet-unmapped(sheet)
    end;
    for (child :: <basic-sheet> in sheet-children(sheet))
      sheet-mapped?(child, do-repaint?: #f) := #f
    end;
    when (do-repaint? & state = %sheet_mapped & sheet-handles-repaint?(sheet))
      repaint-within-parent(sheet, clear?: clear?)
    end
  end;
  #f
end method sheet-mapped?-setter;

define method note-sheet-unmapped (sheet :: <sheet>) => ()
  // If we're unmapping a sheet, make sure it doesn't hold the focus
  let _port     = port(sheet);
  let new-focus = #f;
  let old-focus = port & port-input-focus(_port);
  when (sheet == old-focus)
    port-input-focus(_port) := new-focus
  end
end method note-sheet-unmapped;


// Stronger than unmapped.  The sheet doesn't even participate in layout
define sealed inline method sheet-withdrawn?
    (sheet :: <basic-sheet>) => (withdrawn? :: <boolean>)
  logand(sheet-flags(sheet), %sheet_mapped_mask) = %sheet_withdrawn
end method sheet-withdrawn?;

// Moves a sheet from "mapped" or "managed" to "withdrawn"
define method sheet-withdrawn?-setter
    (withdrawn? == #t, sheet :: <basic-sheet>,
     #key do-repaint? = #t, clear? = do-repaint?)
 => (withdrawn? :: <boolean>)
  let flags = sheet-flags(sheet);
  let state = logand(flags, %sheet_mapped_mask);
  unless (state = %sheet_withdrawn)
    sheet-flags(sheet)
      := logior(logand(flags, lognot(%sheet_mapped_mask)), %sheet_withdrawn);
    // When we go into the withdrawn state, pass through unmapped first...
    when (state = %sheet_mapped)
      note-sheet-unmapped(sheet)
    end;
    // Ensure all of the kids are unmapped (not withdrawn!)
    // We do this so that layout is unaffected below the withdrawn sheet
    for (child :: <basic-sheet> in sheet-children(sheet))
      sheet-mapped?(child) := #f
    end;
    when (do-repaint? & state = %sheet_mapped)
      repaint-within-parent(sheet, clear?: clear?)
    end
  end;
  #t
end method sheet-withdrawn?-setter;

// Moves a sheet from "withdrawn" to "managed" (_not_ to "mapped")
define method sheet-withdrawn?-setter
    (withdrawn? == #f, sheet :: <basic-sheet>, 
     #key do-repaint? = #t, clear? = do-repaint?)
 => (withdrawn? :: <boolean>)
  ignore(do-repaint?, clear?);
  let flags = sheet-flags(sheet);
  let state = logand(flags, %sheet_mapped_mask);
  when (state = %sheet_withdrawn)
    sheet-flags(sheet)
      := logior(logand(flags, lognot(%sheet_mapped_mask)), %sheet_managed);
    // Ensure all of the kids are unwithdrawn, so that they can be
    // moved into managed or mapped later
    for (child :: <basic-sheet> in sheet-children(sheet))
      sheet-withdrawn?(child) := #f
    end
  end;
  #f
end method sheet-withdrawn?-setter;


// Returns #t only when 'allocate-space' has been run on the sheet
define sealed inline method sheet-layed-out?
    (sheet :: <basic-sheet>) => (layed-out? :: <boolean>)
  logand(sheet-flags(sheet), %layed_out) = %layed_out
end method sheet-layed-out?;

// Don't seal this, because layout needs to augment it
define method sheet-layed-out?-setter
    (layed-out? :: <boolean>, sheet :: <basic-sheet>)
 => (layed-out? :: <boolean>)
  sheet-flags(sheet)
    := logior(logand(sheet-flags(sheet), lognot(%layed_out)),
	      if (layed-out?) %layed_out else 0 end);
  layed-out?
end method sheet-layed-out?-setter;


define method sheet-accepts-focus?
    (sheet :: <basic-sheet>) => (accepts-focus? :: <boolean>)
  logand(sheet-flags(sheet), %accepts_focus) = %accepts_focus
end method sheet-accepts-focus?;

define method sheet-accepts-focus?-setter
    (accepts-focus? :: <boolean>, sheet :: <basic-sheet>)
 => (accepts-focus? :: <boolean>)
  sheet-flags(sheet)
    := logior(logand(sheet-flags(sheet), lognot(%accepts_focus)),
	      if (accepts-focus?) %accepts_focus else 0 end);
  accepts-focus?
end method sheet-accepts-focus?-setter;


define sealed inline method sheet-tab-stop?
    (sheet :: <basic-sheet>) => (tab-stop? :: <boolean>)
  logand(sheet-flags(sheet), %tab_stop) = %tab_stop
end method sheet-tab-stop?;


// This property allows certain mirrored sheets (e.g., Windows group boxes)
// to decline to have mirrors parented into them.  The interesting method
// is defined on <mirrored-sheet-mixin>.
// For unmirrored sheets, this must return #f.
define method sheet-mirror-accepts-children?
    (sheet :: <basic-sheet>) => (accepts-children? :: <boolean>)
  #f
end method sheet-mirror-accepts-children?;


define sealed inline method sheet-event-mask
    (sheet :: <basic-sheet>) => (mask :: <integer>)
  logand(sheet-flags(sheet), %sheet_event_mask)
end method sheet-event-mask;

define sealed method sheet-event-mask-setter
    (mask :: <integer>, sheet :: <basic-sheet>) => (mask :: <integer>)
  sheet-flags(sheet)
    := logior(logand(sheet-flags(sheet), lognot(%sheet_event_mask)), mask)
end method sheet-event-mask-setter;


define method display
    (sheet :: <sheet>) => (display :: false-or(<display>))
  let top-sheet = top-level-sheet(sheet);
  top-sheet & display(top-sheet)
end method display;

// This method just provides a uniform protocol for setting the display
//--- Note that setting the display is done only during the grafting process
define method display-setter 
    (_display :: false-or(<display>), sheet :: <sheet>)
 => (_display :: false-or(<display>))
  _display
end method display-setter;


define method sheet-frame
    (sheet :: <sheet>) => (frame :: false-or(<frame>))
  let top-sheet = top-level-sheet(sheet);
  top-sheet & sheet-frame(top-sheet)
end method sheet-frame;

define method sheet-frame-setter 
    (frame :: false-or(<frame>), sheet :: <sheet>)
 => (frame :: false-or(<frame>))
  error("Attempt to set frame of a non top-level sheet %=\n", sheet)
end method sheet-frame-setter;

define method frame-manager
    (sheet :: <sheet>) => (framem :: false-or(<frame-manager>))
  let top-sheet = top-level-sheet(sheet);
  top-sheet & frame-manager(top-sheet)
end method frame-manager;


// Some mirrored sheets might have a "shell" that acts as it's external handle
define method sheet-shell (sheet :: <sheet>) => (shell :: <sheet>)
  sheet
end method sheet-shell;

// This method allows a sheet to delegate its focus somewhere else.
// Most sheets just keep the focus themselves -- but not viewports
define method sheet-input-focus (sheet :: <sheet>) => (focus :: <sheet>)
  sheet
end method sheet-input-focus;


define sealed method default-foreground
    (sheet :: <basic-sheet>) => (fg :: false-or(<ink>))
  let style = sheet.%style-descriptor;
  style & default-foreground(style)
end method default-foreground;

define sealed method default-foreground-setter
    (fg :: false-or(<ink>), sheet :: <basic-sheet>) => (fg :: false-or(<ink>))
  let style = sheet.%style-descriptor
	      | begin
		  let style = make(<style-descriptor>);
		  sheet.%style-descriptor := style;
		  style
		end;
  default-foreground(style) := fg;
  fg
end method default-foreground-setter;

define sealed method default-background
    (sheet :: <basic-sheet>) => (fg :: false-or(<ink>))
  let style = sheet.%style-descriptor;
  style & default-background(style)
end method default-background;

define sealed method default-background-setter
    (bg :: false-or(<ink>), sheet :: <basic-sheet>) => (bg :: false-or(<ink>))
  let style = sheet.%style-descriptor
	      | begin
		  let style = make(<style-descriptor>);
		  sheet.%style-descriptor := style;
		  style
		end;
  default-background(style) := bg;
  bg
end method default-background-setter;

define sealed method default-text-style
    (sheet :: <basic-sheet>) => (ts :: false-or(<text-style>))
  let style = sheet.%style-descriptor;
  style & default-text-style(style)
end method default-text-style;

define sealed method default-text-style-setter
    (ts :: false-or(<text-style>), sheet :: <basic-sheet>) => (ts :: false-or(<text-style>))
  let style = sheet.%style-descriptor
	      | begin
		  let style = make(<style-descriptor>);
		  sheet.%style-descriptor := style;
		  style
		end;
  default-text-style(style) := ts;
  ts
end method default-text-style-setter;


/// Help

define method sheet-help-context
    (sheet :: <basic-sheet>) => (context)
  gethash(sheet.%help-contexts, sheet)
end method sheet-help-context;

define method sheet-help-context-setter
    (context, sheet :: <basic-sheet>) => (context)
  gethash(sheet.%help-contexts, sheet) := context
end method sheet-help-context-setter;

define method sheet-help-source
    (sheet :: <basic-sheet>) => (locator)
  gethash(sheet.%help-sources, sheet)
end method sheet-help-source;

define method sheet-help-source-setter
    (locator, sheet :: <basic-sheet>) => (locator)
  gethash(sheet.%help-sources, sheet) := locator
end method sheet-help-source-setter;


/// Cursors

define constant $pointer-cursors :: <simple-object-vector>
    = #[#"default",		// must be at index 0
	#"busy",
	#"vertical-scroll",
	#"horizontal-scroll",
	#"scroll-up",
	#"scroll-down",
	#"scroll-left",
	#"scroll-right",
	#"upper-left",
	#"upper-right",
	#"lower-left",
	#"lower-right",
	#"vertical-thumb",
	#"horizontal-thumb",
	#"button",
	#"prompt",
	#"move",
	#"position",
	#"i-beam",
	#"cross",
	#"starting",
	#"hand"];

define method sheet-cursor 
    (sheet :: <basic-sheet>) => (cursor :: <cursor>)
  let index = ash(logand(sheet-flags(sheet), %pointer_cursor_mask),
		  -%pointer_cursor_shift);
  $pointer-cursors[index]
end method sheet-cursor;

define method sheet-cursor-setter
    (cursor :: <cursor>, sheet :: <basic-sheet>) => (cursor :: <cursor>)
  unless (sheet-cursor(sheet) == cursor)
    do-set-sheet-cursor(port(sheet), sheet, cursor);
    let index = position($pointer-cursors, cursor) | 0;
    sheet-flags(sheet)
      := logior(logand(sheet-flags(sheet), lognot(%pointer_cursor_mask)),
		ash(index, %pointer_cursor_shift))
  end;
  cursor
end method sheet-cursor-setter;


/// Carets

define open abstract class <sheet-with-caret-mixin> (<abstract-sheet>)
  sealed slot sheet-caret :: type-union(<caret>, one-of(#f, #t)) = #f,
    init-keyword: caret:;
end class <sheet-with-caret-mixin>;

define method sheet-caret
    (sheet :: <basic-sheet>) => (caret :: singleton(#f))
  #f
end method sheet-caret;

define method note-sheet-attached (sheet :: <sheet-with-caret-mixin>) => ()
  next-method();
  when (sheet-caret(sheet) = #t)
    sheet-caret(sheet) := make-caret(port(sheet), sheet)
  end
end method note-sheet-attached;


/// Genealogy

define method sheet-child
    (sheet :: <sheet>) => (child :: false-or(<sheet>))
  let children = sheet-children(sheet);
  let n-children :: <integer> = size(children);
  assert(n-children <= 1,
         "The sheet %= has more than one child", sheet);
  if (zero?(n-children)) #f else children[0] end
end method sheet-child;


//--- Kludge so that sheets can be shared among multiple layouts
define thread variable *old-layout* :: false-or(<sheet>) = #f;

define method add-child
    (sheet :: <sheet>, child :: <sheet>, #rest keys, #key index) => (sheet :: <sheet>)
  dynamic-extent(keys);
  ignore(index);
  when (sheet-parent(child))
    if (*old-layout* & sheet-ancestor?(child, *old-layout*))
      // If this child is parented into an old layout, just remove it
      remove-child(sheet-parent(child), child)
    else
      assert(~sheet-parent(child),
	     "The sheet %= already has a parent", child)
    end
  end;
  apply(do-add-child, sheet, child, keys);
  note-child-added(sheet, child);
  sheet
end method add-child;

define method note-child-added (sheet :: <sheet>, child :: <sheet>) => ()
  sheet-layed-out?(sheet) := #f;
  when (sheet-attached?(sheet))
    // If the sheet we're adding is attached, attach the child as well
    graft-sheet(sheet, child)
  end
end method note-child-added;

define method note-child-added (sheet :: <sheet>, child :: <basic-sheet>) => ()
  child.%parent := sheet;
  next-method()
end method note-child-added;

// Attach the sheet all the way up to the display, and make sure all its
// children are attached as well.  Note that this doesn't map the sheet,
// because we don't want to willy-nilly pop windows up on the screen.
//--- Should this avoid grafting if the sheet is withdrawn?
define method graft-sheet (parent :: <sheet>, sheet :: <sheet>) => ()
  let _port = port(parent);
  let _display = display(parent);
  sheet.%port := _port;
  display(sheet) := _display;
  note-sheet-attached(sheet);
  for (child :: <sheet> in sheet-children(sheet))
    graft-sheet(sheet, child)		// graft it, but don't map it
  end
end method graft-sheet;


define method remove-child
    (sheet :: <sheet>, child :: <sheet>) => (sheet :: <sheet>)
  assert(sheet-parent(child) == sheet,
         "The sheet %= is not a child of %=", child, sheet);
  do-remove-child(sheet, child);
  note-child-removed(sheet, child);
  sheet
end method remove-child;

define method note-child-removed (sheet :: <sheet>, child :: <sheet>) => ()
  sheet-layed-out?(sheet) := #f;
  when (sheet-attached?(sheet))
    // If we're going to degraft it, unmap it first
    sheet-mapped?(child) := #f;
    // If the sheet we're removing from was attached, detach the child
    degraft-sheet(sheet, child)
  end
end method note-child-removed;

define method note-child-removed (sheet :: <sheet>, child :: <basic-sheet>) => ()
  next-method();
  child.%parent := #f
end method note-child-removed;

define method degraft-sheet (parent :: <sheet>, sheet :: <sheet>) => ()
  for (child :: <sheet> in sheet-children(sheet))
    degraft-sheet(sheet, child)
  end;
  note-sheet-detached(sheet);
  sheet-layed-out?(sheet) := #f;
  // We remove the sheet from the display, but not from the port.  The
  // reason is that we don't want to lose any closing-down events
  display(sheet) := #f
end method degraft-sheet;


define method replace-child
    (sheet :: <sheet>, old-child :: <sheet>, child :: <sheet>) => (sheet :: <sheet>)
  when (sheet-parent(child))
    if (*old-layout* & sheet-ancestor?(child, *old-layout*))
      // If this child is parented into an old layout, just remove it
      remove-child(sheet-parent(child), child)
    else
      assert(~sheet-parent(child),
	     "The sheet %= already has a parent", child)
    end
  end;
  do-replace-child(sheet, old-child, child);
  note-child-removed(sheet, old-child);
  note-child-added(sheet, child);
  sheet
end method replace-child;


define open abstract class <multiple-child-mixin> (<abstract-sheet>)
  sealed slot sheet-children :: <sequence> = make(<stretchy-vector>),
    setter: %children-setter;
end class <multiple-child-mixin>;

define method initialize (sheet :: <multiple-child-mixin>, #key children)
  next-method();
  when (children)
    sheet.%children := as(<stretchy-vector>, children);
    do(curry(note-child-added, sheet), children)
  end
end method initialize;

define method sheet-children-setter
    (children :: <sequence>, sheet :: <multiple-child-mixin>)
 => (children :: <sequence>)
  let old-children = sheet-children(sheet);
  let new-children :: <stretchy-object-vector> = as(<stretchy-vector>, children);
  sheet.%children := new-children;
  for (child :: <basic-sheet> in old-children)
    unless (member?(child, new-children))
      note-child-removed(sheet, child)
    end
  end;
  for (child :: <basic-sheet> in new-children)
    unless (member?(child, old-children))
      note-child-added(sheet, child)
    end
  end;
  children
end method sheet-children-setter;

define method do-add-child
    (sheet :: <multiple-child-mixin>, child :: <sheet>,
     #key index = #"end") => ()
  //--- This might not get the Z-ordering right if the children overlap...
  insert-at!(sheet-children(sheet), child, index)
end method do-add-child;

define method do-remove-child 
    (sheet :: <multiple-child-mixin>, child :: <sheet>) => ()
  remove!(sheet-children(sheet), child)
end method do-remove-child;

define method do-replace-child
    (sheet :: <multiple-child-mixin>, old-child :: <sheet>, new-child :: <sheet>) => ()
  substitute!(sheet-children(sheet), old-child, new-child)
end method do-replace-child;


define open abstract class <single-child-mixin> (<abstract-sheet>)
  sealed slot sheet-children :: <sequence> = #[],
    setter: %children-setter;
end class <single-child-mixin>;

define method initialize (sheet :: <single-child-mixin>, #key child)
  next-method();
  when (child)
    sheet.%children := vector(child);
    note-child-added(sheet, child)
  end
end method initialize;

define method sheet-children-setter
    (children :: <sequence>, sheet :: <single-child-mixin>) => (children :: <sequence>)
  assert(size(children) <= 1,
         "You can only add one child to the sheet %=", sheet);
  let old-children = sheet-children(sheet);
  let old-child = ~empty?(old-children) & old-children[0];
  let new-child = ~empty?(children)     & children[0];
  sheet.%children := as(<simple-vector>, children);
  unless (old-child == new-child)
    when (old-child)
      note-child-removed(sheet, old-child)
    end;
    when (new-child)
      note-child-added(sheet, new-child)
    end
  end;
  children
end method sheet-children-setter;

define method sheet-child (sheet :: <single-child-mixin>) => (child :: false-or(<sheet>))
  let children = sheet-children(sheet);
  unless (empty?(children))
    children[0]
  end
end method sheet-child;

define method sheet-child-setter 
    (child :: <sheet>, sheet :: <single-child-mixin>) => (child :: <sheet>)
  sheet-children(sheet) := vector(child);
  child
end method sheet-child-setter;

define method do-add-child
    (sheet :: <single-child-mixin>, child :: <basic-sheet>, #key index) => ()
  ignore(index);
  assert(empty?(sheet-children(sheet)),
         "The single-child sheet %= already has a child", sheet);
  sheet.%children := vector(child)
end method do-add-child;

define method do-remove-child
    (sheet :: <single-child-mixin>, child :: <basic-sheet>) => ()
  sheet.%children := #[]
end method do-remove-child;

define method do-replace-child
    (sheet :: <single-child-mixin>, old-child :: <sheet>, new-child :: <sheet>) => ()
  substitute!(sheet-children(sheet), old-child, new-child)
end method do-replace-child;


/// Traversing sheet regions

// When a sequence is used to store children, DUIM requires that the
// lowest sheets in the Z-ordering (bottom) be at the front of the sequence
define sealed inline method bottom-up-iteration-protocol
    (children :: <sequence>)
 => (initial-state :: <integer>, limit :: <integer>,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>)
  forward-iteration-protocol(children)
end method bottom-up-iteration-protocol;

// When a sequence is used to store children, DUIM requires that the
// highest sheets in the Z-ordering (top) be at the back of the sequence
define sealed inline method top-down-iteration-protocol
    (children :: <sequence>)
 => (initial-state :: <integer>, limit :: <integer>,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>)
  backward-iteration-protocol(children)
end method top-down-iteration-protocol;

// X and Y are in the coordinate system of SHEET
define sealed method child-containing-position
    (sheet :: <basic-sheet>, x :: <real>, y :: <real>)
 => (sheet :: false-or(<sheet>))
  block (return)
    for (child :: <basic-sheet> in sheet-children(sheet)
	   using top-down-iteration-protocol)
      when (sheet-mapped?(child)
            & begin
                let (x, y) = untransform-position(sheet-transform(child), x, y);
                region-contains-position?(sheet-region(child), x, y)
              end)
        return(child)
      end
    end;
    #f
  end
end method child-containing-position;

// X and Y are in the coordinate system of SHEET
define sealed method do-children-containing-position
    (function :: <function>, sheet :: <basic-sheet>, x :: <real>, y :: <real>) => ()
  dynamic-extent(function);
  for (child :: <basic-sheet> in sheet-children(sheet)
	 using top-down-iteration-protocol)
    when (sheet-mapped?(child)
          & begin
              let (x, y) = untransform-position(sheet-transform(child), x, y);
              region-contains-position?(sheet-region(child), x, y)
            end)
      function(child)
    end
  end
end method do-children-containing-position;

// REGION is in the coordinate system of SHEET
define sealed method children-overlapping-region
    (sheet :: <basic-sheet>, region :: <region>) => (children :: <sequence>)
  if (everywhere?(region))
    sheet-mapped-children(sheet)
  else
    let (left, top, right, bottom) = box-edges(region);
    let result :: <stretchy-object-vector> = make(<stretchy-vector>);
    for (child :: <basic-sheet> in sheet-children(sheet)
	   using bottom-up-iteration-protocol)
      when (sheet-mapped?(child)
	    & begin
		let (left1, top1, right1, bottom1) = box-edges(child);
		let (left2, top2, right2, bottom2)
		  = untransform-box(sheet-transform(child),
				    left, top, right, bottom);
		ltrb-intersects-ltrb?(left1, top1, right1, bottom1,
				      left2, top2, right2, bottom2)
	      end)
	add!(result, child)
      end
    end;
    result
  end
end method children-overlapping-region;

// REGION is in the coordinate system of SHEET
define sealed method do-children-overlapping-region
    (function :: <function>, sheet :: <basic-sheet>, region :: <region>) => ()
  dynamic-extent(function);
  if (everywhere?(region))
    for (child :: <basic-sheet> in sheet-children(sheet)
	   using bottom-up-iteration-protocol)
      when (sheet-mapped?(child))
        function(child)
      end
    end
  else
    let (left, top, right, bottom) = box-edges(region);
    for (child :: <basic-sheet> in sheet-children(sheet)
	   using bottom-up-iteration-protocol)
      when (sheet-mapped?(child)
	    & begin
		let (left1, top1, right1, bottom1) = box-edges(child);
		let (left2, top2, right2, bottom2)
		  = untransform-box(sheet-transform(child),
				    left, top, right, bottom);
		ltrb-intersects-ltrb?(left1, top1, right1, bottom1,
				      left2, top2, right2, bottom2)
	      end)
	function(child)
      end
    end
  end
end method do-children-overlapping-region;


/// Sheet transforms

// Returns the transform that maps SHEET's coordinate system all
// the way up to ANCESTOR
define method sheet-delta-transform
    (sheet :: <sheet>, ancestor :: <sheet>) => (transform :: <transform>)
  if (sheet == ancestor)
    $identity-transform
  else
    local method delta-transform
	      (s :: <sheet>, a :: <sheet>) => (transform :: <transform>)
            let parent = sheet-parent(s);
            case
              parent == a =>
                sheet-transform(s);
              ~parent =>
                error("The sheet %= is not an ancestor of %=", ancestor, sheet);
              otherwise =>
		//---*** Why doesn't this call the local 'delta-transform' function?
                compose-transforms(sheet-transform(s),
                                   sheet-delta-transform(parent, a));
            end
          end method;
    delta-transform(sheet, ancestor)
  end
end method sheet-delta-transform;


/// A little more genealogy

define method sheet-ancestor?
    (sheet :: <sheet>, putative-ancestor :: <sheet>) => (true? :: <boolean>)
  block (return)
    for (sheet = sheet then sheet-parent(sheet),
	 until: ~sheet)
      when (sheet == putative-ancestor)
	return(#t)
      end
    end;
    #f
  end
end method sheet-ancestor?;


/// Cached geometry

define method note-region-changed (sheet :: <sheet>) => ()
  invalidate-cached-regions(sheet)
end method note-region-changed;

// Invalidate the cached device region for this sheet and its descendents
define method invalidate-cached-regions (sheet :: <sheet>) => ()
  invalidate-cached-region(sheet);
  // Once we hit a mirrored sheet that accepts children, then we know the
  // regions below that sheet are "normalized", so we can stop smashing
  // the cached regions
  unless (sheet-direct-mirror(sheet)
	    & sheet-mirror-accepts-children?(sheet))
    do(invalidate-cached-regions, sheet-children(sheet))
  end
end method invalidate-cached-regions;

define method invalidate-cached-region (sheet :: <sheet>) => ()
  #f
end method invalidate-cached-region;

define method invalidate-cached-region (sheet :: <basic-sheet>) => ()
  let region = sheet-cached-device-region(sheet);
  when (region)
    if (region == $nowhere)	// it can happen...
      sheet-cached-device-region(sheet) := #f
    else
      invalidate-box!(sheet-cached-device-region(sheet))
    end
  end
end method invalidate-cached-region;


define method note-transform-changed (sheet :: <sheet>) => ()
  invalidate-cached-transforms(sheet)
end method note-transform-changed;

// Invalidate the cached device transform for this sheet and its descendents
define method invalidate-cached-transforms (sheet :: <sheet>) => ()
  invalidate-cached-transform(sheet);
  // Once we hit a mirrored sheet that accepts children, then we know the
  // transforms below that sheet are "normalized", so we can stop smashing
  // the cached transforms
  unless (sheet-direct-mirror(sheet)
	    & sheet-mirror-accepts-children?(sheet))
    do(invalidate-cached-transforms, sheet-children(sheet))
  end
end method invalidate-cached-transforms;

define method invalidate-cached-transform (sheet :: <sheet>) => ()
  #f
end method invalidate-cached-transform;

define method invalidate-cached-transform (sheet :: <basic-sheet>) => ()
  // Changing the transform also invalidates the region
  invalidate-cached-region(sheet);
  sheet-cached-device-transform(sheet) := #f
end method invalidate-cached-transform;


/// Functions that work on all sheets

define method do-sheet-children
    (function :: <function>, sheet :: <sheet>, #key z-order :: <z-order> = #f) => ()
  dynamic-extent(function);
  let iteration-protocol
    = if (z-order == #"top-down") top-down-iteration-protocol
      else bottom-up-iteration-protocol end;
  for (child :: <sheet> in sheet-children(sheet)
	 using iteration-protocol)
    function(child)
  end
end method do-sheet-children;

// Bottom up (lowest in Z order), breadth-first recursive...
define method do-sheet-tree
    (function :: <function>, sheet :: <sheet>) => ()
  dynamic-extent(function);
  function(sheet);
  // 'sheet-children' is defined to support the forward iteration protocol...
  for (child :: <sheet> in sheet-children(sheet))
    do-sheet-tree(function, child)
  end
end method do-sheet-tree;

// There's a "stopper" method on the <top-level-sheet> class
// that returns the top level sheet itself.
define method top-level-sheet
    (sheet :: <sheet>) => (sheet :: false-or(<sheet>))
  let parent = sheet-parent(sheet);
  parent & top-level-sheet(parent)
end method top-level-sheet;

// Find the first ancestor of 'sheet' of the given class,
// or #f if there is no such ancestor
define method find-ancestor-of-type
    (sheet :: <sheet>, type :: <type>) => (sheet :: false-or(<sheet>))
  block (return)
    for (s = sheet-parent(sheet) then sheet-parent(s))
      when (~s | instance?(s, type))
	return(s)
      end
    end;
    #f
  end
end method find-ancestor-of-type;

define method sheet-mapped-children (sheet :: <sheet>) => (children :: <sequence>)
  choose(sheet-mapped?, sheet-children(sheet))
end method sheet-mapped-children;


/// Raising and lowering

define method raise-sheet
    (sheet :: <sheet>, #key activate? = #t) => (sheet :: <sheet>)
  let parent = sheet-parent(sheet);
  when (parent)
    do-raise-sheet(parent, sheet, activate?: activate?)
  end;
  let mirror = sheet-direct-mirror(sheet);
  when (mirror)
    raise-mirror(port(sheet), sheet, mirror, activate?: activate?)
  end;
  sheet
end method raise-sheet;

define method do-raise-sheet
    (parent :: <sheet>, sheet :: <sheet>, #key activate? = #t) => ()
  ignore(activate?);
  #f
end method do-raise-sheet;

define method do-raise-sheet
    (parent :: <multiple-child-mixin>, sheet :: <sheet>, #key activate? = #t) => ()
  ignore(activate?);
  remove!(sheet-children(parent), sheet);
  insert-at!(sheet-children(parent), sheet, #"start")
end method do-raise-sheet;


define method lower-sheet (sheet :: <sheet>) => (sheet :: <sheet>)
  let parent = sheet-parent(sheet);
  when (parent)
    do-lower-sheet(parent, sheet)
  end;
  let mirror = sheet-direct-mirror(sheet);
  when (mirror)
    lower-mirror(port(sheet), sheet, mirror)
  end;
  sheet
end method lower-sheet;

define method do-lower-sheet (parent :: <sheet>, sheet :: <sheet>) => ()
  #f
end method do-lower-sheet;

define method do-lower-sheet (parent :: <multiple-child-mixin>, sheet :: <sheet>) => ()
  remove!(sheet-children(parent), sheet);
  insert-at!(sheet-children(parent), sheet, #"end")
end method do-lower-sheet;
