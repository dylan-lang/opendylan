Module:       duim-frames-internals
Synopsis:     DUIM frames
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Frames

define constant <frame-state>
    = one-of(#"detached", #"unmapped", #"mapped", #"destroyed");

define constant <frame-mode>
    = one-of(#"modeless", #"modal", #"system-modal");

define protocol <<frame-protocol>> ()
  getter frame-layout
    (frame :: <abstract-frame>) => (layout :: false-or(<abstract-sheet>));
  setter frame-layout-setter
    (layout :: false-or(<abstract-sheet>), frame :: <abstract-frame>)
 => (layout :: false-or(<abstract-sheet>));
  function layout-frame
    (frame :: <abstract-frame>, #key width, height) => ();
  getter frame-owner
    (frame :: <abstract-frame>) => (owner :: false-or(<abstract-frame>));
  getter frame-owned-frames
    (frame :: <abstract-frame>) => (owned-frames :: <sequence>);
  getter frame-owned-menus
    (frame :: <abstract-frame>) => (owned-menus :: <sequence>);
  getter frame-menu-bar
    (frame :: <abstract-frame>) => (menu-bar :: false-or(<menu-bar>));
  setter frame-menu-bar-setter
    (menu-bar :: false-or(<menu-bar>), frame :: <abstract-frame>)
 => (menu-bar :: false-or(<menu-bar>));
  getter frame-tool-bar
    (frame :: <abstract-frame>) => (tool-bar :: false-or(<tool-bar>));
  setter frame-tool-bar-setter
    (tool-bar :: false-or(<tool-bar>), frame :: <abstract-frame>)
 => (tool-bar :: false-or(<tool-bar>));
  getter frame-status-bar
    (frame :: <abstract-frame>) => (status-bar :: false-or(<status-bar>));
  setter frame-status-bar-setter
    (status-bar :: false-or(<status-bar>), frame :: <abstract-frame>)
 => (status-bar :: false-or(<status-bar>));
  getter frame-status-message
    (frame :: <abstract-frame>) => (message :: false-or(<string>));
  setter frame-status-message-setter
    (message :: false-or(<string>), frame :: <abstract-frame>)
 => (message :: false-or(<string>));
//  getter frame-input-focus		// defined in DUIM-sheets
//    (frame :: <abstract-frame>) => (sheet :: false-or(<abstract-sheet>));
//  setter frame-input-focus-setter	// defined in DUIM-sheets
//    (sheet :: false-or(<abstract-sheet>), frame :: <abstract-frame>)
// => (sheet :: false-or(<abstract-sheet>));
  getter frame-command-table
    (frame :: <abstract-frame>) => (command-table :: false-or(<command-table>));
  setter frame-command-table-setter
    (command-table :: false-or(<command-table>), frame :: <abstract-frame>)
 => (command-table :: false-or(<command-table>));
  function note-command-table-changed
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>) => ();
  function handle-id-activation
    (frame :: <abstract-frame>, id) => (handled? :: <boolean>);
  function frame-top-level-sheet-class
    (frame :: <abstract-frame>, #key, #all-keys) => (class :: <class>);
  function frame-wrapper
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>,
     sheet :: false-or(<abstract-sheet>))
 => (wrapper :: false-or(<abstract-sheet>));
  getter frame-resource-id
    (frame :: <abstract-frame>) => (resource-id);
  function start-frame
    (frame :: <abstract-frame>) => (status-code :: false-or(<integer>));
  function port-start-frame
    (port :: <abstract-port>, frame :: <abstract-frame>)
 => (status-code :: false-or(<integer>));
  function make-event-queue
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>)
 => (event-queue :: false-or(<event-queue>));
  function frame-needs-event-queue?
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>, #key mode)
 => (needs-event-queue? :: <boolean>);
  function frame-can-exit?
    (frame :: <abstract-frame>) => (can-exit? :: <boolean>);
  function frame-top-level
    (frame :: <abstract-frame>) => (#rest values);
  // Enabling and disabling a whole frame (for modal dialog parents)
  getter frame-enabled?
    (frame :: <abstract-frame>) => (enabled? :: <boolean>);
  setter frame-enabled?-setter
    (enabled? :: <boolean>, frame :: <abstract-frame>) => (enabled? :: <boolean>);
  function note-frame-enabled
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>) => ();
  function note-frame-disabled
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>) => ();
  // Iconifying, etc
  function frame-iconified?
    (frame :: <abstract-frame>) => (iconified? :: <boolean>);
  function frame-iconified?-setter
    (iconified? :: <boolean>, frame :: <abstract-frame>) => (iconified? :: <boolean>);
  function frame-maximized?
    (frame :: <abstract-frame>) => (maximized? :: <boolean>);
  function frame-maximized?-setter
    (maximized? :: <boolean>, frame :: <abstract-frame>) => (maximized? :: <boolean>);
  function raise-frame (frame :: <abstract-frame>, #key activate?) => ();
  function lower-frame (frame :: <abstract-frame>) => ();
  // For exit buttons, etc
  getter frame-default-button
    (frame :: <abstract-frame>) => (button :: false-or(<button>));
  getter frame-default-button-setter
    (button :: false-or(<button>), frame :: <abstract-frame>)
 => (button :: false-or(<button>));
  getter frame-accelerators
    (frame :: <abstract-frame>) => (accelerators :: <sequence>);
  getter frame-accelerators-setter
    (accelerators :: false-or(<sequence>), frame :: <abstract-frame>)
 => (accelerators :: false-or(<sequence>));
  function note-accelerators-changed
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>) => ();
  // User frame classes can use this to watch transition in 'frame-state',
  // 'frame-iconified?', and 'frame-maximized?'
  function note-frame-state-changed
    (frame :: <abstract-frame>, old-state, new-state) => ();
  // User frame classes can add methods to these, provided the methods
  // start with a call to 'next-method'
  function note-frame-mapped
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>) => ();
  function note-frame-unmapped
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>) => ();
  function note-frame-iconified
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>) => ();
  function note-frame-deiconified
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>) => ();
  function note-frame-maximized
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>) => ();
  function note-frame-unmaximized
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>) => ();
  // These communicate to the back-end to do some layout work
  function update-frame-layout
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>) => ();
  function update-frame-wrapper
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>) => ();
  function note-frame-title-changed
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>) => ();
  function note-frame-icon-changed
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>) => ();
  // The most basic support for documents
  getter frame-document
    (frame :: <abstract-frame>) => (document :: false-or(<object>));
  setter frame-document-setter
    (document :: false-or(<object>), frame :: <abstract-frame>)
 => (document :: false-or(<object>));
  // Is this frame covered by any other window?
  function frame-occluded?
    (frame :: <abstract-frame>) => (occluded? :: <boolean>);
  function do-frame-occluded?
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>) => (occluded? :: <boolean>);
end protocol <<frame-protocol>>;


/// Decoding the frame flags

// Bits 0..2 are the frames's state
define constant %frame_state_mask :: <integer> = #o07;
define constant %frame_detached   :: <integer> = #o00;
define constant %frame_unmapped   :: <integer> = #o01;
define constant %frame_mapped     :: <integer> = #o02;
define constant %frame_destroyed  :: <integer> = #o03;

define constant $frame-states :: <simple-object-vector>
    = #[#"detached", #"unmapped", #"mapped", #"destroyed"];

// Bits 3..5 are the frames's mode
define constant %frame_mode_shift   :: <integer> = 3;
define constant %frame_mode_mask    :: <integer> = #o70;
define constant %frame_modeless     :: <integer> = #o00;
define constant %frame_modal        :: <integer> = #o10;
define constant %frame_system_modal :: <integer> = #o20;

define constant $frame-modes :: <simple-object-vector>
    = #[#"modeless", #"modal", #"system-modal", #"unknown"];

// Bit 6 is the enabled flag
define constant %frame_enabled :: <integer> = #o100;

// Bits 7 and 8 are the iconified and maximized flags
define constant %frame_iconified :: <integer> = #o200;
define constant %frame_maximized :: <integer> = #o400;

// Bits 9 and 10 are the fixed width/height flags
define constant %frame_fixed_width  :: <integer> = #o1000;
define constant %frame_fixed_height :: <integer> = #o2000;

// Bit 11 is the save-under flag
define constant %frame_save_under   :: <integer> = #o4000;

// Bits 12 and 13 are alt=meta and allow-control-meta flags
define constant %frame_alt_is_meta       :: <integer> = #o10000;
define constant %frame_allow_control_alt :: <integer> = #o20000;

// Bit 14 is the "always on top" flag
define constant %frame_on_top :: <integer> = #o40000;

// Bits 15 and 16 are the minimize/maximize box flags
define constant %frame_minimize_box :: <integer> = #o100000;
define constant %frame_maximize_box :: <integer> = #o200000;

// Bit 17 is the "center over parent" flag
define constant %frame_centered :: <integer> = #o400000;

// Bit 18 is the "handle keyboard interrupt" flag
define constant %keyboard_interrupt :: <integer> = #o1000000;

define constant $initial-frame-flags :: <integer>
    = logior(%frame_detached, %frame_modeless, %frame_enabled,
	     %frame_minimize_box, %frame_maximize_box, %keyboard_interrupt);


define open abstract primary class <basic-frame> (<frame>)
  slot frame-title :: false-or(<string>) = #f,
    init-keyword: title:,
    setter: %title-setter;
  slot frame-icon :: false-or(<image>) = #f,
    init-keyword: icon:,
    setter: %icon-setter;
  sealed slot frame-manager :: false-or(<frame-manager>) = #f;
  sealed slot frame-owner   :: false-or(<frame>) = #f,
    init-keyword: owner:;
  sealed slot frame-flags :: <integer> = $initial-frame-flags;
  sealed constant slot frame-owned-frames :: <stretchy-object-vector>
    = make(<stretchy-vector>);
  sealed constant slot frame-owned-menus  :: <stretchy-object-vector>
    = make(<stretchy-vector>);
  sealed slot top-level-sheet :: false-or(<sheet>) = #f,
    init-keyword: top-level-sheet:;
  sealed slot frame-default-button :: false-or(<button>) = #f,
    init-keyword: default-button:,
    setter: %default-button-setter;
  sealed constant slot frame-geometry :: <simple-object-vector>
    = vector(#f, #f, #f, #f),		// x, y, width, height
    init-keyword: geometry:;
  sealed constant slot frame-properties :: <stretchy-object-vector>
    = make(<stretchy-vector>);
  sealed slot frame-thread = #f,
    init-keyword: thread:;
  sealed slot frame-event-queue :: false-or(<event-queue>) = #f,
    init-keyword: event-queue:;
  sealed slot event-handler :: false-or(<event-handler>) = #f,
    init-keyword: event-handler:;
  sealed slot %input-focus :: false-or(<sheet>) = #f,
    init-keyword: input-focus:;
  sealed slot frame-cursor-override :: false-or(<cursor>) = #f,
    setter: %cursor-override-setter,
    init-keyword: cursor-override:;
  sealed slot frame-background-operations :: <integer> = 0;
  sealed slot %style-descriptor :: false-or(<style-descriptor>) = #f,
    init-keyword: style-descriptor:;
  sealed slot %palette :: false-or(<palette>) = #f,
    init-keyword: palette:;
  sealed slot %exit-function :: false-or(<function>) = #f;
  sealed slot %accelerators  :: false-or(<sequence>) = #f;
  // Documents
  slot frame-document :: false-or(<object>) = #f,
    init-keyword: document:;
  // Resources
  // Use 'keyword resource-id: = $mumble-frame-id' to initialize this
  sealed constant slot frame-resource-id = #f,
    init-keyword: resource-id:;
  // For handing to top level sheet in case this is an embedded frame...
  sealed slot %container = #f,
    init-keyword: container:;
  sealed slot %container-region = #f,
    init-keyword: container-region:;
end class <basic-frame>;

// Users can make an instance of this class, but it's better to subclass it
define open abstract primary class <simple-frame> (<basic-frame>)
  sealed slot frame-command-queue :: <event-queue> = make(<event-queue>),
    init-keyword: command-queue:;
  sealed slot frame-pointer-documentation :: false-or(<sheet>) = #f,
    init-keyword: pointer-documentation:;
  sealed slot %command-table :: false-or(<command-table>) = #f,
    init-keyword: command-table:;
  sealed slot frame-disabled-commands :: <object-table> = make(<table>);
  // The next four slots have accessors defined for them by 'define frame'
  // for every user-defined subclass of <simple-frame>
  sealed slot %layout :: false-or(<sheet>) = #f,
    init-keyword: layout:;
  sealed slot %menu-bar :: false-or(<menu-bar>) = #f,
    init-keyword: menu-bar:;
  sealed slot %tool-bar :: false-or(<tool-bar>) = #f,
    init-keyword: tool-bar:;
  sealed slot %status-bar :: false-or(<status-bar>) = #f,
    init-keyword: status-bar:;
end class <simple-frame>;

define sealed class <concrete-simple-frame> (<simple-frame>)
end class <concrete-simple-frame>;

define sealed domain make (singleton(<concrete-simple-frame>));
define sealed domain initialize (<concrete-simple-frame>);

//--- 'sideways' because <frame> is defined in DUIM-Sheets
define sealed inline sideways method make
    (class == <frame>, #rest initargs, #key, #all-keys)
 => (pane :: <concrete-simple-frame>)
  apply(make, <concrete-simple-frame>, initargs)
end method make;

define sealed inline method make
    (class == <simple-frame>, #rest initargs, #key, #all-keys)
 => (pane :: <concrete-simple-frame>)
  apply(make, <concrete-simple-frame>, initargs)
end method make;


define sealed method port (frame :: <basic-frame>) => (port :: false-or(<port>))
  port(frame-manager(frame))
end method port;

define sealed method display (frame :: <basic-frame>) => (display :: false-or(<display>))
  let framem = frame-manager(frame);
  framem & display(framem)
end method display;


define sealed inline method frame-state
    (frame :: <basic-frame>) => (state :: <frame-state>)
  let state = logand(frame-flags(frame), %frame_state_mask);
  $frame-states[state]
end method frame-state;

define sealed method frame-state-setter
    (state :: <frame-state>, frame :: <basic-frame>)
 => (state :: <frame-state>)
  let old-state = frame-state(frame);
  unless (state == old-state)
    let new-state :: <integer>
      = select (state)
	  #"detached"  => %frame_detached;
	  #"unmapped"  => %frame_unmapped;
	  #"mapped"    => %frame_mapped;
	  #"destroyed" => %frame_destroyed;
	end;
    frame-flags(frame)
      := logior(logand(frame-flags(frame), lognot(%frame_state_mask)), new-state);
    note-frame-state-changed(frame, old-state, state)
  end;
  state
end method frame-state-setter;

define method note-frame-state-changed
    (frame :: <basic-frame>, old-state, new-state) => ()
  #f
end method note-frame-state-changed;


define sealed inline method frame-mode
    (frame :: <basic-frame>) => (mode :: <frame-mode>)
  let mode = ash(logand(frame-flags(frame), %frame_mode_mask), -%frame_mode_shift);
  $frame-modes[mode]
end method frame-mode;

define sealed method frame-mode-setter
    (mode :: <frame-mode>, frame :: <basic-frame>)
 => (mode :: <frame-mode>)
  let new-mode :: <integer>
    = select (mode)
	#"modeless"     => %frame_modeless;
	#"modal"        => %frame_modal;
	#"system-modal" => %frame_system_modal;
      end;
  frame-flags(frame)
    := logior(logand(frame-flags(frame), lognot(%frame_mode_mask)), new-mode);
  mode
end method frame-mode-setter;


define sealed inline method frame-enabled?
    (frame :: <basic-frame>) => (enabled? :: <boolean>)
  ~zero?(logand(frame-flags(frame), %frame_enabled))
end method frame-enabled?;

define sealed method frame-enabled?-setter
    (enabled? :: <boolean>, frame :: <basic-frame>)
 => (enabled? :: <boolean>)
  when (frame-enabled?(frame) ~= enabled?)
    frame-flags(frame)
      := logior(logand(frame-flags(frame), lognot(%frame_enabled)),
		if (enabled?) %frame_enabled else 0 end);
    if (enabled?)
      note-frame-enabled(frame-manager(frame), frame)
    else
      note-frame-disabled(frame-manager(frame), frame)
    end
  end;
  enabled?
end method frame-enabled?-setter;

define method note-frame-enabled
    (framem :: <frame-manager>, frame :: <frame>) => ()
  #f
end method note-frame-enabled;

define method note-frame-disabled
    (framem :: <frame-manager>, frame :: <frame>) => ()
  #f
end method note-frame-disabled;


define sealed inline method frame-iconified?
    (frame :: <basic-frame>) => (iconified? :: <boolean>)
  ~zero?(logand(frame-flags(frame), %frame_iconified))
end method frame-iconified?;

define sealed method frame-iconified?-setter
    (iconified? :: <boolean>, frame :: <basic-frame>)
 => (iconified? :: <boolean>)
  when (frame-iconified?(frame) ~= iconified?)
    frame-flags(frame)
      := logior(logand(frame-flags(frame), lognot(%frame_iconified)),
		if (iconified?) %frame_iconified else 0 end);
    if (iconified?)
      note-frame-state-changed(frame, #"deiconified", #"iconified");
      note-frame-iconified(frame-manager(frame), frame)
    else
      note-frame-state-changed(frame, #"iconified", #"deiconified");
      note-frame-deiconified(frame-manager(frame), frame)
    end
  end;
  iconified?
end method frame-iconified?-setter;

define method note-frame-iconified
    (framem :: <frame-manager>, frame :: <frame>) => ()
  #f
end method note-frame-iconified;

define method note-frame-deiconified
    (framem :: <frame-manager>, frame :: <frame>) => ()
  #f
end method note-frame-deiconified;


define sealed inline method frame-maximized?
    (frame :: <basic-frame>) => (maximized? :: <boolean>)
  ~zero?(logand(frame-flags(frame), %frame_maximized))
end method frame-maximized?;

define sealed method frame-maximized?-setter
    (maximized? :: <boolean>, frame :: <basic-frame>)
 => (maximized? :: <boolean>)
  when (frame-maximized?(frame) ~= maximized?)
    frame-flags(frame)
      := logior(logand(frame-flags(frame), lognot(%frame_maximized)),
		if (maximized?) %frame_maximized else 0 end);
    if (maximized?)
      note-frame-state-changed(frame, #"unmaximized", #"maximized");
      note-frame-maximized(frame-manager(frame), frame)
    else
      note-frame-state-changed(frame, #"maximized", #"unmaximized");
      note-frame-unmaximized(frame-manager(frame), frame)
    end
  end;
  maximized?
end method frame-maximized?-setter;

define method note-frame-maximized
    (framem :: <frame-manager>, frame :: <frame>) => ()
  #f
end method note-frame-maximized;

define method note-frame-unmaximized
    (framem :: <frame-manager>, frame :: <frame>) => ()
  #f
end method note-frame-unmaximized ;


define sealed inline method frame-fixed-width?
    (frame :: <basic-frame>) => (fixed-width? :: <boolean>)
  ~zero?(logand(frame-flags(frame), %frame_fixed_width))
end method frame-fixed-width?;

define sealed inline method frame-fixed-height?
    (frame :: <basic-frame>) => (fixed-height? :: <boolean>)
  ~zero?(logand(frame-flags(frame), %frame_fixed_height))
end method frame-fixed-height?;

define function frame-resizable?
    (frame :: <frame>) => (resizable? :: <boolean>)
  ~frame-fixed-width?(frame) | ~frame-fixed-height?(frame)
end function frame-resizable?;


define sealed inline method frame-alt-key-is-meta?
    (frame :: <basic-frame>) => (alt-is-meta? :: <boolean>)
  ~zero?(logand(frame-flags(frame), %frame_alt_is_meta))
end method frame-alt-key-is-meta?;

define sealed method frame-alt-key-is-meta?-setter
    (alt-is-meta? :: <boolean>, frame :: <basic-frame>) => (alt-is-meta? :: <boolean>)
  frame-flags(frame)
    := logior(logand(frame-flags(frame), lognot(%frame_alt_is_meta)),
	      if (alt-is-meta?) %frame_alt_is_meta else 0 end);
  alt-is-meta?
end method frame-alt-key-is-meta?-setter;


define sealed inline method frame-allow-control-alt?
    (frame :: <basic-frame>) => (alt-is-meta? :: <boolean>)
  ~zero?(logand(frame-flags(frame), %frame_alt_is_meta))
end method frame-allow-control-alt?;

define sealed method frame-allow-control-alt?-setter
    (alt-is-meta? :: <boolean>, frame :: <basic-frame>) => (alt-is-meta? :: <boolean>)
  frame-flags(frame)
    := logior(logand(frame-flags(frame), lognot(%frame_alt_is_meta)),
	      if (alt-is-meta?) %frame_alt_is_meta else 0 end);
  alt-is-meta?
end method frame-allow-control-alt?-setter;


define sealed inline method frame-save-under?
    (frame :: <basic-frame>) => (save-under? :: <boolean>)
  ~zero?(logand(frame-flags(frame), %frame_save_under))
end method frame-save-under?;

define sealed inline method frame-minimize-box?
    (frame :: <basic-frame>) => (minimize-box? :: <boolean>)
  ~zero?(logand(frame-flags(frame), %frame_minimize_box))
end method frame-minimize-box?;

define sealed inline method frame-maximize-box?
    (frame :: <basic-frame>) => (maximize-box? :: <boolean>)
  ~zero?(logand(frame-flags(frame), %frame_maximize_box))
end method frame-maximize-box?;

define sealed inline method frame-always-on-top?
    (frame :: <basic-frame>) => (always-on-top? :: <boolean>)
  ~zero?(logand(frame-flags(frame), %frame_on_top))
end method frame-always-on-top?;

define sealed inline method frame-centered?
    (frame :: <basic-frame>) => (centered? :: <boolean>)
  ~zero?(logand(frame-flags(frame), %frame_centered))
end method frame-centered?;


define sealed inline method frame-keyboard-interrupt?
    (frame :: <basic-frame>) => (keyboard-interrupt? :: <boolean>)
  ~zero?(logand(frame-flags(frame), %keyboard_interrupt))
end method frame-keyboard-interrupt?;


// Go up the 'frame-owner' path until we find a frame with a thread
define method frame-controlling-frame
    (frame :: <basic-frame>) => (frame :: false-or(<frame>))
  for (f = frame then frame-owner(f),
       until: ~f | frame-thread(f))
  finally f
  end
end method frame-controlling-frame;

define method find-color
    (name, frame :: <basic-frame>, #key error? = #t) => (color :: <color>)
  find-color(name, frame-palette(frame), error?: error?)
end method find-color;

define method frame-palette
    (frame :: <basic-frame>) => (palette :: false-or(<palette>))
  let framem = frame-manager(frame);
  frame.%palette
  | if (framem)
      frame-manager-palette(framem)
    else
      port(frame) & port-default-palette(port(frame))
    end
end method frame-palette;

define method frame-palette-setter
    (palette :: false-or(<palette>), frame :: <basic-frame>)
 => (palette :: false-or(<palette>))
  frame.%palette := palette
end method frame-palette-setter;


define sealed method default-foreground
    (frame :: <basic-frame>) => (fg :: false-or(<ink>))
  let style = frame.%style-descriptor;
  style & default-foreground(style)
end method default-foreground;

define sealed method default-foreground-setter
    (fg :: false-or(<ink>), frame :: <basic-frame>) => (fg :: false-or(<ink>))
  let style = frame.%style-descriptor
	      | begin
		  let style = make(<style-descriptor>);
		  frame.%style-descriptor := style;
		  style
		end;
  default-foreground(style) := fg;
  fg
end method default-foreground-setter;

define sealed method default-background
    (frame :: <basic-frame>) => (fg :: false-or(<ink>))
  let style = frame.%style-descriptor;
  style & default-background(style)
end method default-background;

define sealed method default-background-setter
    (bg :: false-or(<ink>), frame :: <basic-frame>) => (bg :: false-or(<ink>))
  let style = frame.%style-descriptor
	      | begin
		  let style = make(<style-descriptor>);
		  frame.%style-descriptor := style;
		  style
		end;
  default-background(style) := bg;
  bg
end method default-background-setter;

define sealed method default-text-style
    (frame :: <basic-frame>) => (ts :: false-or(<text-style>))
  let style = frame.%style-descriptor;
  style & default-text-style(style)
end method default-text-style;

define sealed method default-text-style-setter
    (ts :: false-or(<text-style>), frame :: <basic-frame>) => (ts :: false-or(<text-style>))
  let style = frame.%style-descriptor
	      | begin
		  let style = make(<style-descriptor>);
		  frame.%style-descriptor := style;
		  style
		end;
  default-text-style(style) := ts;
  ts
end method default-text-style-setter;


/// Accelerators

// The idea here is to allow the accelerators to be set explicitly,
// or computed from the all the gadgets in the frame
define method frame-accelerators
    (frame :: <basic-frame>) => (accelerators :: <sequence>)
  frame.%accelerators
  | begin
      let top-sheet = top-level-sheet(frame);
      let framem    = frame-manager(frame);
      if (top-sheet)
	local method do-accelerators (function :: <function>)
		do-sheet-tree(method (sheet)
				when (instance?(sheet, <accelerator-mixin>))
				  let accelerator
				    = defaulted-gadget-accelerator(framem, sheet);
				  when (accelerator)
				    function(sheet, accelerator)
				  end
				end
			      end method, top-sheet)
	      end method;
	let n :: <integer> = 0;
	do-accelerators(method (gadget, accelerator)
			  ignore(gadget, accelerator);
			  inc!(n)
			end method);
	let accelerators :: <simple-object-vector> = make(<vector>, size: n);
	let i :: <integer> = 0;
	do-accelerators(method (gadget, accelerator)
			  accelerators[i] := vector(gadget, accelerator);
			  inc!(i)
			end method);
	frame.%accelerators := accelerators
      else
	#[]
      end
    end
end method frame-accelerators;

define method frame-accelerators-setter
    (accelerators :: false-or(<sequence>), frame :: <basic-frame>)
 => (accelerators :: false-or(<sequence>))
  note-accelerators-changed(frame-manager(frame), frame);
  frame.%accelerators := accelerators
end method frame-accelerators-setter;

define method note-accelerators-changed
    (framem :: <frame-manager>, frame :: <basic-frame>) => ()
  #f
end method note-accelerators-changed;


/// Frame geometry

// If the frame has not been layed out yet, return the default geometry,
// otherwise return the live value from the top level sheet
define method frame-position
    (frame :: <basic-frame>)
 => (x :: false-or(<integer>), y :: false-or(<integer>))
  let top-sheet = top-level-sheet(frame);
  let container-region = frame.%container-region;
  case
    container-region =>
      box-position(container-region);
    top-sheet & sheet-layed-out?(top-sheet) =>
      sheet-position(top-sheet);
    otherwise =>
      let geometry = frame-geometry(frame);
      values(geometry[0], geometry[1]);
  end
end method frame-position;

// Always update the geometry hint.  If the frame has a top level sheet,
// update it as well.
define method set-frame-position
    (frame :: <basic-frame>, x :: <integer>, y :: <integer>,
     #key constrain? = #t) => ()
  let (x, y)
    = if (constrain?) constrained-frame-position(frame, x, y)
      else values(x, y) end;
  let top-sheet = top-level-sheet(frame);
  let geometry  = frame-geometry(frame);
  geometry[0] := x;
  geometry[1] := y;
  when (top-sheet & sheet-layed-out?(top-sheet))
    set-sheet-position(top-sheet, x, y)
  end
end method set-frame-position;

define method constrained-frame-position
    (frame :: <frame>, x :: <integer>, y :: <integer>)
 => (x :: <integer>, y :: <integer>)
  let _display = display(frame);
  if (_display)
    let (width, height)   = frame-size(frame);
    let (dright, dbottom) = box-size(_display);
    let width  :: <integer> = width  | $default-sheet-size;
    let height :: <integer> = height | $default-sheet-size;
    let left   :: <integer> = x;
    let top    :: <integer> = y;
    let right  :: <integer> = left + width;
    let bottom :: <integer> = top  + height;
    when (right > dright)
      left := dright - width
    end;
    when (bottom > dbottom)
      top := dbottom - height
    end;
    values(max(0, left), max(0, top))
  else
    values(x, y)
  end
end method constrained-frame-position;

define method frame-size 
    (frame :: <basic-frame>)
 => (width :: false-or(<integer>), height :: false-or(<integer>))
  let top-sheet = top-level-sheet(frame);
  let container-region = frame.%container-region;
  case
    container-region =>
      box-size(container-region);
    top-sheet & sheet-layed-out?(top-sheet) =>
      sheet-size(top-sheet);
    otherwise =>
      let geometry = frame-geometry(frame);
      values(geometry[2], geometry[3]);
  end
end method frame-size;

define method set-frame-size
    (frame :: <basic-frame>, width :: <integer>, height :: <integer>) => ()
  let top-sheet = top-level-sheet(frame);
  let geometry  = frame-geometry(frame);
  geometry[2] := width;
  geometry[3] := height;
  when (top-sheet & sheet-layed-out?(top-sheet))
    set-sheet-size(top-sheet, width, height)
  end
end method set-frame-size;


/// Input focus

define method frame-input-focus
    (frame :: <simple-frame>) => (sheet :: false-or(<sheet>))
  frame.%input-focus
end method frame-input-focus;

define method frame-input-focus-setter
    (new :: false-or(<sheet>), frame :: <basic-frame>)
 => (new :: false-or(<sheet>))
  let old = frame.%input-focus;
  when (new ~== old)
    let _port = port(frame);
    frame.%input-focus := new;
    if (port-input-focus(_port) == old)
      port-input-focus(_port) := new
    end;
    when (frame-mapped?(frame))
      distribute-event(_port,
		       make(<frame-input-focus-changed-event>,
			    frame: frame,
			    old-focus: old,
			    new-focus: new))
    end
  end;
  new
end method frame-input-focus-setter;


/// Pointer cursors

define method frame-cursor-override-setter
    (new :: false-or(<cursor>), frame :: <basic-frame>)
 => (new :: false-or(<cursor>))
  let old = frame-cursor-override(frame);
  when (new ~== old)
    frame.%cursor-override := new;
    let _port = port(frame);
    if (_port)
      let pointer = port-pointer(_port);
      update-pointer-cursor(pointer, frame: frame)
    end
  end;
  new
end method frame-cursor-override-setter;

define macro with-background-cursor
  { with-background-cursor (?frame:expression, ?cursor:expression) ?:body end }
    => { begin
	   let body = method () ?body end;
	   do-with-background-cursor(?frame, ?cursor, body)
         end }
  { with-background-cursor (?frame:expression) ?:body end }
    => { begin
	   let body = method () ?body end;
	   do-with-background-cursor(?frame, #"starting", body)
         end }
end macro with-background-cursor;

define method do-with-background-cursor
    (sheet :: <sheet>, cursor, continuation :: <function>) => (#rest values)
  let frame = sheet-frame(sheet);
  if (frame)
    do-with-background-cursor(frame, cursor, continuation)
  else
    continuation()		// sheet is not grafted
  end
end method do-with-background-cursor;

define method do-with-background-cursor
    (frame :: <frame>, cursor, continuation :: <function>) => (#rest values)
  block ()
    call-in-frame(frame, 
		  note-background-operation-started, frame, cursor: cursor);
    continuation()
  cleanup
    call-in-frame(frame, note-background-operation-finished, frame)
  end
end method do-with-background-cursor;

define method note-background-operation-started
    (frame :: <frame>, #key cursor = #"starting") => ()
  inc!(frame-background-operations(frame));
  frame-cursor-override(frame) := cursor
end method note-background-operation-started;

define method note-background-operation-finished
    (frame :: <frame>) => ()
  let operations = dec!(frame-background-operations(frame));
  when (zero?(operations))
    frame-cursor-override(frame) := #f
  end
end method note-background-operation-finished;


/// Command table handling

define method frame-command-table
    (frame :: <simple-frame>) => (command-table :: false-or(<command-table>))
  frame.%command-table
end method frame-command-table;

define method frame-command-table-setter
    (command-table :: false-or(<command-table>), frame :: <simple-frame>)
 => (command-table :: false-or(<command-table>))
  //---*** How does this interact with command loops?
  //---*** What if there was an explicit <menu> given as the menu bar?
  frame.%command-table := command-table;
  let framem = frame-manager(frame);
  when (framem)
    note-command-table-changed(framem, frame)
  end;
  command-table
end method frame-command-table-setter;

define method note-command-table-changed
    (framem :: <frame-manager>, frame :: <simple-frame>) => ()
  let menu-bar = frame-menu-bar(frame);
  when (menu-bar)
    let command-table = frame-command-table(frame);
    let menus = make-menus-from-command-table(command-table, frame, framem);
    let old-menus = sheet-children(menu-bar);
    sheet-children(menu-bar) := menus;
    do(destroy-sheet, old-menus)
  end
end method note-command-table-changed;

define method handle-id-activation
    (frame :: <frame>, id) => (handled? :: <boolean>)
  let command-table = frame-command-table(frame);
  when (command-table)
    block (return)
      do-command-table-menu-items
	(method (decorator, command-table)
	   ignore(command-table);
	   when (decorator-resource-id(decorator) = id)
	     let object = decorator-object(decorator);
	     let type   = decorator-type(decorator);
	     select (type)
	       <function>, <command> =>
		 distribute-command-event(frame, object);
		 return(#t);
	       otherwise =>
		 #f;
	     end
	   end
	 end,
	 command-table,
	 do-inherited?: #t)
    end
  end
end method handle-id-activation;


/// Menu bar handling

// Back-ends specialize this to install sensible mnemonics for
// the particular platform (e.g. 'x' for 'Exit' in MS Windows)
define open generic install-frame-mnemonics (frame :: <abstract-frame>) => ();

define method frame-menu-bar
    (frame :: <simple-frame>) => (menu-bar :: false-or(<menu-bar>))
  frame.%menu-bar
end method frame-menu-bar;

define method frame-menu-bar-setter
    (menu-bar :: false-or(<menu-bar>), frame :: <simple-frame>)
 => (menu-bar :: false-or(<menu-bar>))
  unless (menu-bar = frame-menu-bar(frame))
    frame.%menu-bar := menu-bar;
    let framem = frame-manager(frame);
    framem & update-frame-wrapper(framem, frame)
  end;
  menu-bar
end method frame-menu-bar-setter;


define method frame-tool-bar
    (frame :: <simple-frame>) => (tool-bar :: false-or(<tool-bar>))
  frame.%tool-bar
end method frame-tool-bar;

define method frame-tool-bar-setter
    (tool-bar :: false-or(<tool-bar>), frame :: <simple-frame>)
 => (tool-bar :: false-or(<tool-bar>))
  unless (tool-bar = frame-tool-bar(frame))
    frame.%tool-bar := tool-bar;
    let framem = frame-manager(frame);
    framem & update-frame-wrapper(framem, frame)
  end;
  tool-bar
end method frame-tool-bar-setter;


define method frame-status-bar
    (frame :: <simple-frame>) => (status-bar :: false-or(<status-bar>))
  frame.%status-bar
end method frame-status-bar;

define method frame-status-bar-setter
    (status-bar :: false-or(<status-bar>), frame :: <simple-frame>)
 => (status-bar :: false-or(<status-bar>))
  unless (status-bar = frame-status-bar(frame))
    frame.%status-bar := status-bar;
    let framem = frame-manager(frame);
    update-frame-wrapper(framem, frame)
  end;
  status-bar
end method frame-status-bar-setter;


// A little more support for OLE, which has a simplistic notion of status bars
define method frame-status-message
    (frame :: <simple-frame>) => (message :: false-or(<string>))
  let status-bar = frame-status-bar(frame);
  when (status-bar)
    gadget-label(status-bar)
  end
end method frame-status-message;

define method frame-status-message-setter
    (message :: false-or(<string>), frame :: <simple-frame>)
 => (message :: false-or(<string>))
  let status-bar = frame-status-bar(frame);
  when (status-bar)
    gadget-label(status-bar) := message
  end;
  message
end method frame-status-message-setter;


/// Popup menu handling

define method note-menu-attached
    (frame :: <basic-frame>, menu :: <menu>) => ()
  next-method();
  add!(frame-owned-menus(frame), menu)
end method note-menu-attached;

define method note-menu-detached
    (frame :: <basic-frame>, menu :: <menu>) => ()
  next-method();
  remove!(frame-owned-menus(frame), menu)
end method note-menu-detached;


/// Frame initialization

define method initialize
    (frame :: <basic-frame>,
     #key frame-manager: framem, parent = $unsupplied,
          state = #"detached", mode = #"modeless",
	  enabled? = #t, iconified? = #f, maximized? = #f,
	  fixed-width? = #f, fixed-height? = #f, resizable? = #t,
	  always-on-top? = #f,
     	  alt-key-is-meta? = #f, allow-control-alt? = $unsupplied,
	  x, y, width, height, disabled-commands = #[],
          foreground, background, text-style,
          dialog-for = $unsupplied, save-under? = #f,
	  minimize-box? = #t, maximize-box? = #t,
	  center? = #f, keyboard-interrupt? = #t)
  next-method();
  // Initialize the event handler very early
  unless (event-handler(frame))
    event-handler(frame) := frame
  end;
  let state :: <integer>
    = select (state)
	#"detached"  => %frame_detached;
	#"unmapped"  => %frame_unmapped;
	#"mapped"    => %frame_mapped;
	#"destroyed" => %frame_destroyed;
      end;
  let mode :: <integer>
    = select (mode)
	#"modeless"     => %frame_modeless;
	#"modal"        => %frame_modal;
	#"system-modal" => %frame_system_modal;
      end;
  let fixed-width?  = if (resizable?) fixed-width?  else #t end;
  let fixed-height? = if (resizable?) fixed-height? else #t end;
  let allow-control-alt?
    = if (supplied?(allow-control-alt?)) allow-control-alt? else alt-key-is-meta? end;
  frame-flags(frame)
    := logior(state, mode,
	      if (enabled?) %frame_enabled else 0 end,
	      if (iconified?) %frame_iconified else 0 end,
	      if (maximized?) %frame_maximized else 0 end,
	      if (fixed-width?) %frame_fixed_width else 0 end,
	      if (fixed-height?) %frame_fixed_height else 0 end,
	      if (always-on-top?) %frame_on_top else 0 end,
	      if (alt-key-is-meta?) %frame_alt_is_meta else 0 end,
	      if (allow-control-alt?) %frame_allow_control_alt else 0 end,
	      if (save-under?) %frame_save_under else 0 end,
	      if (minimize-box?) %frame_minimize_box else 0 end,
	      if (maximize-box?) %frame_maximize_box else 0 end,
	      if (center?) %frame_centered else 0 end,
	      if (keyboard-interrupt?) %keyboard_interrupt else 0 end);
  when (foreground | background | text-style)
    frame.%style-descriptor
      := make(<style-descriptor>,
	      foreground: foreground,
	      background: background,
	      text-style: text-style)
  end;
  // Save various other frame properties
  let properties :: <stretchy-object-vector> = frame-properties(frame);
  when (supplied?(dialog-for))
    put-property!(properties, dialog-for:, dialog-for)
  end;
  // Save the requested geometry
  let geometry = frame-geometry(frame);
  geometry[0] := x;
  geometry[1] := y;
  geometry[2] := width;
  geometry[3] := height;
  // Initially disable some commands
  let table :: <object-table> = frame-disabled-commands(frame);
  for (command in disabled-commands)
    gethash(table, command) := #t
  end;
  // The frame doesn't get attached and mapped until 'start-frame',
  // but we still want to figure out where to attach the frame for later
  let parent-supplied? = supplied?(parent);
  let parent = if (parent-supplied?) parent else framem end;
  let framem
    = select (parent by instance?)
	singleton(#f) =>
	  unless (parent-supplied?)
	    current-frame-manager() | find-frame-manager()
	  end;
	<frame-manager> => parent;
	<frame>   => frame-manager(parent);
	<port>    => find-frame-manager(port: parent);
	<display> => find-frame-manager(port: port(parent));
	<sheet>   => frame-manager(sheet-frame(parent))
      end;
  frame-manager(frame) := framem;
  // Now initialize the event queue
  unless (frame-event-queue(frame))
    let owner = frame-owner(frame);
    frame-event-queue(frame)
      := if (owner & ~frame-needs-event-queue?(framem, frame))
	   frame-event-queue(owner)
	 else
	   make-event-queue(framem, frame)
	 end
  end
end method initialize;

// Will return either <top-level-sheet> or <top-level-stream>
define method frame-top-level-sheet-class
    (frame :: <basic-frame>, #key) => (class :: <class>)
  <top-level-sheet>
end method frame-top-level-sheet-class;


/// Creating frames

// Create a frame of the specified type if one does not already exist, 
// and then run it, possibly in its own thread.  If one already exists,
// just select it.
define method find-frame
    (frame-class, #rest initargs,
     #key create? = #t, activate? = #t, own-thread? = #t,
          port: _port, frame-manager: framem, test = identity,
	  z-order :: one-of(#"top-down", #"bottom-up") = #"top-down",
     #all-keys)
 => (frame :: <frame>)
  dynamic-extent(initargs);
  let frame
    = unless (create? == #"force")
        block (return)
          do-frames(method (frame)
		      when (instance?(frame, frame-class) & test(frame))
			return(frame)
		      end
		    end method,
		    // Restrict the search if these are supplied
		    port: _port, frame-manager: framem, z-order: z-order)
        end
      end;
  when (create? & ~frame)
    with-keywords-removed
        (initargs = initargs,
	 #[create?:, activate?:, own-thread?:, port:, frame-manager:, z-order:])
      case
        framem => #f;
        _port =>
          framem := find-frame-manager(port: _port);
        otherwise =>
          framem := find-frame-manager()
      end;
      frame := apply(make, frame-class, frame-manager: framem, initargs)
    end
  end;
  when (frame & activate?)
    case
      frame-thread(frame) =>
        raise-frame(frame, activate?: #t);
      own-thread? =>
	make(<thread>,
	     function: method () start-frame(frame) end,
	     name: frame-title(frame) | "Unnamed frame");
      otherwise =>
        start-frame(frame)
    end
  end;
  frame
end method find-frame;


/// Attaching and detaching frames

// Attach the frame to a frame manager
define method attach-frame
    (framem :: <frame-manager>, frame :: <basic-frame>) => (frame :: <frame>)
  assert(frame-state(frame) ~== #"destroyed",
         "You cannot attach frame %= because it has been destroyed", frame);
  assert(frame-state(frame) ==  #"detached",
         "There is already a frame manager for %=", frame);
  frame-manager(frame) := framem;
  dynamic-bind (*current-frame* = frame)
    let panes = generate-panes(framem, frame);
    // If there's a command table, install it now
    let command-table = frame-command-table(frame);
    when (command-table)
      let menu-bar = frame-menu-bar(frame);
      if (menu-bar)
	unless (sheet-resource-id(menu-bar))
	  error("You can't supply both a menu bar and a command table for %=",
		frame)
	end
      else
	frame-menu-bar(frame) := make-command-menu-bar(framem, frame)
      end
    end;
    // Note that we always generate a frame wrapper, even when there are no
    // panes in the layout.  This means, for example, that we can get a frame
    // that has only a menu-bar.
    //--- Do we really want to do this?  Of course, 'frame-wrapper' is free
    //--- to return #f if it really wants a frame with no UI component...
    let layout = frame-wrapper(framem, frame, panes);
    when (layout)
      //---*** How do we specify a different display than the default
      //---*** one for the port?
      let top-sheet
	= attach-sheet(find-display(port: port(frame)), layout,
		       sheet-class: frame-top-level-sheet-class(frame),
		       fixed-width?:  frame-fixed-width?(frame),
		       fixed-height?: frame-fixed-height?(frame),
		       frame-manager: framem, frame: frame,
		       event-queue: frame-event-queue(frame),
		       container: frame.%container,
		       container-region: frame.%container-region,
		       foreground: default-foreground(frame),
		       background: default-background(frame),
		       text-style: default-text-style(frame),
		       resource-id: frame-resource-id(frame));
      top-level-sheet(frame) := top-sheet
    end
  end;
  frame-state(frame) := #"unmapped";
  let owner = frame-owner(frame);
  owner & add!(frame-owned-frames(owner), frame);
  add!(frame-manager-frames(framem), frame);
  // This uses 'handle-event' rather than 'distribute-event' because 
  // the user needs to see this before the event loop starts, since by
  // that time everything has already happened (mapping etc).
  handle-event(event-handler(frame),
	       make(<frame-created-event>, frame: frame));
  frame
end method attach-frame;

// Detach the frame from its frame manager
define method detach-frame
    (framem :: <frame-manager>, frame :: <basic-frame>) => (frame :: <frame>)
  unless (frame-state(frame) == #"destroyed")
    assert(frame-manager(frame) == framem,
	   "The frame %= does not belong to this frame manager", frame);
    select (frame-state(frame))
      #"mapped" =>
	frame-mapped?(frame) := #f;
      otherwise => #f
    end;
    // Detach the frame's top-level sheet from the display
    let top-sheet = top-level-sheet(frame);
    when (top-sheet)
      // Update the frame's geometry
      let geometry = frame-geometry(frame);
      let (x, y) = sheet-position(top-sheet);
      let (width, height) = sheet-size(top-sheet);
      geometry[0] := x;
      geometry[1] := y;
      geometry[2] := width;
      geometry[3] := height;
      let _display = display(top-sheet);
      when (_display)
	detach-sheet(_display, top-sheet)
      end
    end;
    top-level-sheet(frame) := #f;
    // If there are any menus still attached, detach them, too
    let owned-menus = frame-owned-menus(frame);
    for (menu in owned-menus)
      //--- Can we make 'detach-sheet' remove the owned menu?
      let _display = display(menu);
      when (_display)
	detach-sheet(_display, menu)
      end
    end;
    size(owned-menus) := 0;
    // Now detach the frame from the frame manager
    frame-state(frame) := #"detached";
    let owner = frame-owner(frame);
    owner & remove!(frame-owned-frames(owner), frame);
    remove!(frame-manager-frames(framem), frame)
  end;
  frame
end method detach-frame;

// The default wrapper does nothing -- it just returns the user-generated
// "top level" sheet...
define method frame-wrapper
    (framem :: <frame-manager>, frame :: <frame>, sheet :: false-or(<sheet>))
 => (wrapper :: false-or(<sheet>))
  sheet
end method frame-wrapper;


/// Frame-defining macrology

define macro frame-definer
  { define ?modifiers:* frame ?:name (?superclasses:*) ?slots:* end }
    => { define ?modifiers frame-class ?name (?superclasses) ?slots end;
         define frame-panes ?name (?superclasses) ?slots end;
         define frame-gadget-bars ?name (?superclasses) ?slots end; 
	 define frame-layout ?name (?superclasses) ?slots end; }
end macro frame-definer;

define macro frame-class-definer
  { define ?modifiers:* frame-class ?:name (?superclasses:*) ?slots:* end }
    => { define ?modifiers class ?name (?superclasses) ?slots end }
 slots:
  { } => { }
  { ?slot:*; ... } => { ?slot ... }
 slot:
  { ?modifiers:* pane ?:name (?frame:variable) ?:body }
    => { ?modifiers slot ?name ## "-pane" :: false-or(<abstract-sheet>) = #f; }
  { ?modifiers:* resource ?:name :: ?type:name = ?resource-id:expression }
    => { ?modifiers slot ?name ## "-pane" :: false-or(<abstract-sheet>) = #f; }
  // The next seven feel like the 'exception' clause in 'block', in that
  // they take an argument (the frame) and a body, but no 'end'
  { layout (?frame:variable) ?:body } => { }		// uses %layout slot
  { menu-bar (?frame:variable) ?:body } => { }		// uses %menu-bar slot
  { tool-bar (?frame:variable) ?:body } => { }		// uses %tool-bar slot
  { status-bar (?frame:variable) ?:body } => { }	// uses %status-bar slot
  { command-table (?frame:variable) ?:body } => { }	// uses %command-table slot
  { input-focus (?frame:variable) ?:body } => { }	// uses %input-focus slot
  { pages  (?frame:variable) ?:body } => { }		// uses %pages slot
  // Catch 'slot', 'keyword', and so forth
  { ?other:* } => { ?other; }
end macro frame-class-definer;

define macro frame-panes-definer
  { define frame-panes ?class:name (?superclasses:*) end }
    => { }
  { define frame-panes ?class:name (?superclasses:*) 
      pane ?:name (?frame:variable) ?:body; ?more-slots:*
    end }
    => { define method ?name (?frame :: ?class) => (pane :: <sheet>)
	   let _framem = frame-manager(?frame);
	   ?frame.?name ## "-pane"
	   | (?frame.?name ## "-pane"
                := with-frame-manager (_framem)
                     ?body
                   end)
	 end method ?name; 
         define frame-panes ?class (?superclasses) ?more-slots end; }
  { define frame-panes ?class:name (?superclasses:*) 
      resource ?:name :: ?type:name = ?resource-id:expression; ?more-slots:*
    end }
    => { define method ?name (_frame :: ?class) => (pane :: <sheet>)
	   let _framem = frame-manager(_frame);
	   _frame.?name ## "-pane"
	   | (_frame.?name ## "-pane"
                := with-frame-manager (_framem)
                     make(?type, resource-id: ?resource-id)
                   end)
	 end method ?name; 
         define frame-panes ?class (?superclasses) ?more-slots end; }
  { define frame-panes ?class:name (?superclasses:*) 
      ?non-pane-slot:*; ?more-slots:*
    end }
    => { define frame-panes ?class (?superclasses) ?more-slots end; }
end macro frame-panes-definer;

define macro frame-gadget-bars-definer
  { define frame-gadget-bars ?class:name (?superclasses:*) end }
    => { }
  { define frame-gadget-bars ?class:name (?superclasses:*) 
      layout (?frame:variable) ?:body; ?more-slots:*
    end }
    => { define method frame-layout
	     (?frame :: ?class) => (sheet :: false-or(<sheet>))
	   let framem = frame-manager(?frame);
	   ?frame.%layout
	   | (?frame.%layout
                := with-frame-manager (framem)
                     ?body
                   end)
	 end method frame-layout; 
         define frame-gadget-bars ?class (?superclasses) ?more-slots end; }
  { define frame-gadget-bars ?class:name (?superclasses:*) 
      menu-bar (?frame:variable) ?:body; ?more-slots:*
    end }
    => { define method frame-menu-bar
	     (?frame :: ?class) => (sheet :: false-or(<menu-bar>))
	   let framem = frame-manager(?frame);
	   ?frame.%menu-bar
	   | (?frame.%menu-bar
                := with-frame-manager (framem)
                     ?body
                   end)
         end method frame-menu-bar; 
         define frame-gadget-bars ?class (?superclasses) ?more-slots end; }
  { define frame-gadget-bars ?class:name (?superclasses:*) 
      tool-bar (?frame:variable) ?:body; ?more-slots:*
    end }
    => { define method frame-tool-bar 
	     (?frame :: ?class) => (sheet :: false-or(<tool-bar>))
	   let framem = frame-manager(?frame);
	   ?frame.%tool-bar
	   | (?frame.%tool-bar
                := with-frame-manager (framem)
                     ?body
                   end)
         end method frame-tool-bar; 
         define frame-gadget-bars ?class (?superclasses) ?more-slots end; }
  { define frame-gadget-bars ?class:name (?superclasses:*) 
      status-bar (?frame:variable) ?:body; ?more-slots:*
    end }
    => { define method frame-status-bar 
	     (?frame :: ?class) => (sheet :: false-or(<status-bar>))
	   let framem = frame-manager(?frame);
	   ?frame.%status-bar
	   | (?frame.%status-bar
                := with-frame-manager (framem)
                     ?body
                   end)
         end method frame-status-bar; 
         define frame-gadget-bars ?class (?superclasses) ?more-slots end; }
  { define frame-gadget-bars ?class:name (?superclasses:*) 
      command-table (?frame:variable) ?:body; ?more-slots:*
    end }
    => { define method frame-command-table
	     (?frame :: ?class) => (command-table :: false-or(<command-table>))
	   ?frame.%command-table
	   | (?frame.%command-table
                := begin
                     ?body
                   end)
         end method frame-command-table; 
         define frame-gadget-bars ?class (?superclasses) ?more-slots end; }
  { define frame-gadget-bars ?class:name (?superclasses:*) 
      input-focus (?frame:variable) ?:body; ?more-slots:*
    end }
    => { define method frame-input-focus
	     (?frame :: ?class) => (sheet :: false-or(<sheet>))
	   ?frame.%input-focus
	   | (?frame.%input-focus
                := begin
                     ?body
                   end)
         end method frame-input-focus; 
         define frame-gadget-bars ?class (?superclasses) ?more-slots end; }
  { define frame-gadget-bars ?class:name (?superclasses:*) 
      pages (?frame:variable) ?:body; ?more-slots:*
    end }
    => { define method dialog-pages 
	     (?frame :: ?class) => (pages :: <sequence>)
	   let framem = frame-manager(?frame);
	   ?frame.%pages
	   | (?frame.%pages
                := with-frame-manager (framem)
                     ?body
                   end)
	 end method dialog-pages; 
         define frame-gadget-bars ?class (?superclasses) ?more-slots end; }
  { define frame-gadget-bars ?class:name (?superclasses:*) 
      ?non-bar-slot:*; ?more-slots:*
    end }
    => { define frame-gadget-bars ?class (?superclasses) ?more-slots end; }
end macro frame-gadget-bars-definer;

define macro frame-layout-definer
  { define frame-layout ?class:name (?superclasses:*) end }
    => { }
  { define frame-layout ?class:name (?superclasses:*) 
      resource ?:name :: ?type:name = ?resource-id:expression; ?more-slots:*
    end }
    => { //---*** How do we now get this pane into a pinboard layout?
         define frame-layout ?class (?superclasses) ?more-slots end; }
  { define frame-layout ?class:name (?superclasses:*) 
      ?non-resource-slot:*; ?more-slots:*
    end }
    => { define frame-layout ?class (?superclasses) ?more-slots end; }
end macro frame-layout-definer;


define open generic generate-panes
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>)
 => (panes :: false-or(<sheet>));

// Note that this assumes that 'current-frame()' will return the right thing...
define method generate-panes
    (framem :: <frame-manager>, frame :: <simple-frame>) => (panes :: false-or(<sheet>))
  frame-layout(frame)
end method generate-panes;


/// Frame titles and icons

define method frame-title-setter
    (title :: false-or(<string>), frame :: <basic-frame>) => (title :: false-or(<string>))
  frame.%title := title;
  let framem = frame-manager(frame);
  when (framem)
    note-frame-title-changed(framem, frame)
  end;
  title
end method frame-title-setter;

define method note-frame-title-changed
    (framem :: <frame-manager>, frame :: <frame>) => ()
  #f
end method note-frame-title-changed;


define method frame-icon-setter
    (icon :: false-or(<image>), frame :: <basic-frame>) => (icon :: false-or(<image>))
  frame.%icon := icon;
  let framem = frame-manager(frame);
  when (framem)
    note-icon-changed(framem, frame)
  end;
  icon
end method frame-icon-setter;

define method note-icon-changed
    (framem :: <frame-manager>, frame :: <frame>) => ()
  #f
end method note-icon-changed;


/// Layout

define method layout-frame
    (frame :: <basic-frame>, #key width, height) => ()
  dynamic-bind (*current-frame* = frame)
    let top-sheet = top-level-sheet(frame);
    when (top-sheet)
      let (new-width, new-height) 
        = frame-top-level-sheet-size(frame-manager(frame), frame, width, height);
      when (width & new-width ~= width | height & new-height ~= height)
	warn("Frame %= rejected size %dX%d, using %dX%d instead",
	     frame, width, height, new-width, new-height)
      end;
      // Setting the top level sheets edges will call 'allocate-space'.
      //--- Don't bother with this if the size didn't change?
      //--- Really 'set-sheet-size' should handle this kludge
      let mirror = sheet-direct-mirror(top-sheet);
      if (mirror)
        let (x, y, right, bottom)
          = mirror-edges(port(frame), top-sheet, mirror);
        ignore(right, bottom);
        // Do this instead of just 'set-sheet-size' to ensure that the
        // frame does not get moved by a window manager to (0,0)
        set-sheet-edges(top-sheet, x, y, x + new-width, y + new-height)
      else
        set-sheet-size(top-sheet, new-width, new-height)
      end
    end
  end
end method layout-frame;


define method frame-layout (frame :: <simple-frame>) => (layout :: false-or(<sheet>))
  frame.%layout
end method frame-layout;

define method frame-layout-setter 
    (layout :: false-or(<sheet>), frame :: <simple-frame>)
 => (layout :: false-or(<sheet>))
  let old-layout = frame-layout(frame);
  unless (layout == old-layout)
    dynamic-bind (*old-layout* = old-layout)
      frame.%layout := layout;
      when (old-layout)
	sheet-mapped?(old-layout) := #f
      end;
      let framem = frame-manager(frame);
      framem & update-frame-layout(framem, frame)
    end
  end;
  layout
end method frame-layout-setter;


define method frame-top-level-sheet-size
    (framem :: <frame-manager>, frame :: <basic-frame>,
     width :: false-or(<integer>), height :: false-or(<integer>))
 => (width, height)
  let top-sheet = top-level-sheet(frame);
  let resizable = frame-resizable?(frame);
  when (top-sheet)
    invalidate-space-requirements(top-sheet);
    // Don't let the frame get bigger than the whole screen
    let (gwidth, gheight) = box-size(display(frame));
    when (width)
      min!(width, gwidth)
    end;
    when (height)
      min!(height, gheight)
    end;
    when (~width | ~height | resizable)
      let space-req = compose-space(top-sheet, width: width, height: height);
      let (w, w-, w+, h, h-, h+) = space-requirement-components(top-sheet, space-req);
      ignore(w-, w+, h-, h+);
      when (~width  | resizable) width  := w end;
      when (~height | resizable) height := h end;
    end
  end;
  values(width, height)
end method frame-top-level-sheet-size;


/// Default button handling

define method frame-default-button-setter
    (new-button :: false-or(<button>), frame :: <basic-frame>)
 => (new-button :: false-or(<button>))
  let old-button = frame-default-button(frame);
  frame.%default-button := new-button;
  when (old-button) gadget-default?(old-button) := #f end;
  when (new-button) gadget-default?(new-button) := #t end;
  new-button
end method frame-default-button-setter;


/// Mapping and unmapping

define method frame-mapped?
    (frame :: <basic-frame>) => (mapped? :: <boolean>)
  frame-state(frame) == #"mapped"
end method frame-mapped?;

define method frame-mapped?-setter
    (mapped? :: <boolean>, frame :: <basic-frame>) => (mapped? :: <boolean>)
  if (mapped?)
    map-frame(frame)
  else
    unmap-frame(frame)
  end;
  mapped?
end method frame-mapped?-setter;


define method map-frame (frame :: <basic-frame>) => ()
  assert(frame-state(frame) ~== #"destroyed",
         "You cannot map frame %= because it has been destroyed", frame);
  select (frame-state(frame))
    #"mapped" =>
      #f;
    #"detached" =>
      attach-frame(frame-manager(frame), frame);
      let top-sheet = top-level-sheet(frame);
      when (top-sheet)
	let geometry = frame-geometry(frame);
	let x      = geometry[0];
	let y      = geometry[1];
	let width  = geometry[2];
	let height = geometry[3];
	layout-frame(frame, width: width, height: height);
	// This uses 'handle-event' rather than 'distribute-event' because 
	// the user needs to see this before the event loop starts
	handle-event(event-handler(frame),
		     make(<frame-layed-out-event>, frame: frame));
	// Position the frame on the screen
	case 
	  frame-centered?(frame) =>
	    let (width, height) = frame-size(frame);
	    let (screen-width, screen-height) = sheet-size(display(frame));
	    let x = max(floor/(screen-width  - width,  2), 0);
	    let y = max(floor/(screen-height - height, 2), 0);
	    set-frame-position(frame, x, y);
	  x & y => 
	    set-frame-position(frame, x, y);
	  otherwise => #f;
	end
      end;
      map-frame(frame);
    #"unmapped" =>
      frame-state(frame) := #"mapped";
      note-frame-mapped(frame-manager(frame), frame);
  end
end method map-frame;

define method note-frame-mapped
    (framem :: <frame-manager>, frame :: <basic-frame>) => ()
  update-frame-settings(framem, frame);
  let top-sheet = top-level-sheet(frame);
  //--- Perhaps we want to resize the top level sheet if there is one
  when (top-sheet)
    sheet-mapped?(top-sheet) := #t
  end;
  distribute-event(port(frame), 
		   make(<frame-mapped-event>, frame: frame))
end method note-frame-mapped;


define method unmap-frame (frame :: <basic-frame>) => ()
  unless (frame-state(frame) == #"destroyed")
    select (frame-state(frame))
      #"detached", #"unmapped" => 
	// Nothing to do
	#f;
      #"mapped" =>
	frame-state(frame) := #"unmapped";
	note-frame-unmapped(frame-manager(frame), frame);
	// 'force-display' to ensure that the frame disappears immediately
	force-display(frame);
	let _port = port(frame);
	// Distribute an exit event from the top level sheet so that none
	// of the frame's sheets appear in the port's trace stack
	distribute-event(_port, make(<pointer-exit-event>,
				     sheet: top-level-sheet(frame),
				     x: -1, y: -1,
				     modifier-state: port-modifier-state(_port),
				     pointer: port-pointer(_port)));
	distribute-event(_port, make(<frame-unmapped-event>,
				     frame: frame))
    end
  end
end method unmap-frame;

define method note-frame-unmapped
    (framem :: <frame-manager>, frame :: <basic-frame>) => ()
  let top-sheet = top-level-sheet(frame);
  when (top-sheet)			// might be a pane-less frame
    sheet-mapped?(top-sheet) := #f
  end
end method note-frame-unmapped;


// This is "sideways" because it is a forward reference from DUIM-Sheets.
define sideways method destroy-frame (frame :: <frame>) => ()
  unless (frame-state(frame) == #"destroyed")
    when (frame-mapped?(frame))
      frame-mapped?(frame) := #f
    end;
    // NB: copy the sequence because it will get changed from under us
    for (owned-frame in copy-sequence(frame-owned-frames(frame)))
      //--- Maybe use 'exit-frame'?
      destroy-frame(owned-frame)
    end;
    let top-sheet = top-level-sheet(frame);
    when (frame-manager(frame))		// just in case
      detach-frame(frame-manager(frame), frame)
    end;
    when (top-sheet)
      destroy-sheet(top-sheet)
    end;
    // Note that we use 'handle-event' instead of 'distribute-event',
    // because the event loop and event queue may well already be gone
    handle-event(event-handler(frame),
		 make(<frame-destroyed-event>, frame: frame));
    frame-state(frame) := #"destroyed"
  end
end method destroy-frame;

define method raise-frame
    (frame :: <frame>, #key activate? = #t) => ()
  let top-sheet = top-level-sheet(frame);
  assert(top-sheet & sheet-mapped?(top-sheet),
         "Attempted to raise %=, which isn't mapped",
         frame);
  raise-sheet(top-sheet, activate?: activate?)
end method raise-frame;

define method lower-frame
    (frame :: <frame>) => ()
  let top-sheet = top-level-sheet(frame);
  assert(top-sheet & sheet-mapped?(top-sheet),
         "Attempted to lower %=, which isn't mapped",
         frame);
  lower-sheet(top-sheet)
end method lower-frame;


define method frame-occluded?
    (frame :: <frame>) => (occluded? :: <boolean>)
  let framem = frame-manager(frame);
  // If it's not attached to a frame manager, just pretend it's occluded
  ~framem | do-frame-occluded?(framem, frame)
end method frame-occluded?;

define method do-frame-occluded?
    (framem :: <frame-manager>, frame :: <frame>)
 => (occluded? :: <boolean>)
  #f
end method do-frame-occluded?;


define method update-frame-settings
    (framem :: <frame-manager>, frame :: <frame>) => ()
  ignore(frame);
  #f
end method update-frame-settings;


// For compatibility...
define sealed method iconify-frame (frame :: <frame>) => ()
  frame-iconified?(frame) := #t
end method iconify-frame;

define sealed method deiconify-frame (frame :: <frame>) => ()
  frame-iconified?(frame) := #f
end method deiconify-frame;

define sealed method maximize-frame (frame :: <frame>) => ()
  frame-maximized?(frame) := #t
end method maximize-frame;

define sealed method unmaximize-frame (frame :: <frame>) => ()
  frame-maximized?(frame) := #f
end method unmaximize-frame;


define method beep (frame :: <basic-frame>) => ();
  let _port = port(frame);
  _port & beep(_port)
end method beep;

define method force-display (frame :: <basic-frame>) => ()
  let _port = port(frame);
  _port & force-display(_port)
end method force-display;

define method synchronize-display (frame :: <basic-frame>) => ()
  let _port = port(frame);
  _port & synchronize-display(_port)
end method synchronize-display;


/// Frame event distribution and queueing

define method do-dispatch-event
    (frame :: <basic-frame>, event :: <event>) => ()
  queue-event(frame, event)
end method do-dispatch-event;

define method queue-event
    (frame :: <basic-frame>, event :: <event>) => ()
  // Push the new event onto the tail of the event queue,
  // or handle the event directly if there is no queue
  let queue = frame-event-queue(frame);
  if (queue)
    event-queue-push-last(queue, event)
  else
    handle-event(event-handler(frame), event)
  end
end method queue-event;


// Returns #t if this frame needs its very own event queue
define method frame-needs-event-queue?
    (framem :: <frame-manager>, frame :: <frame>, #key mode)
 => (needs-event-queue? :: <boolean>)
  select (mode | frame-mode(frame))
    #"modeless" =>
      // Modeless frames get their own event queue only when they have no owner
      ~(frame-owner(frame) & port-event-processor-type(port(framem)) = #"n");
    #"modal", #"system-modal" =>
      // Modal frames always get their own event queue
      #t;
  end
end method frame-needs-event-queue?;

define method make-event-queue
    (framem :: <frame-manager>, frame :: <frame>)
 => (event-queue :: <event-queue>)
  make(<event-queue>)
end method make-event-queue;


/// Thread handling

// Users can use these functions to make a function call in the frame's
// thread. If calling from a different thread, it queues a function event
// onto the frame's event loop.

define inline function in-frame-thread?
    (frame :: <frame>) => (in-frame-thread? :: <boolean>)
  current-thread() == frame-thread(frame)
end function in-frame-thread?;

define method queue-call-in-frame
    (frame :: <frame>, function :: <function>, #rest args) => ()
  let _function = method () apply(function, args) end;
  distribute-function-event(frame, _function)
end method queue-call-in-frame;

define method queue-apply-in-frame
    (frame :: <frame>, function :: <function>, arg, #rest more-args) => ()
  let _function = method () apply(apply, function, arg, more-args) end;
  distribute-function-event(frame, _function)
end method queue-apply-in-frame;

define method call-in-frame
    (frame :: <frame>, function :: <function>, #rest args) => ()
  if (in-frame-thread?(frame))
    apply(function, args)
  else
    let _function = method () apply(function, args) end;
    distribute-function-event(frame, _function)
  end
end method call-in-frame;

define method apply-in-frame
    (frame :: <frame>, function :: <function>, arg, #rest more-args) => ()
  if (in-frame-thread?(frame))
    apply(apply, function, arg, more-args)
  else
    let _function = method () apply(apply, function, arg, more-args) end;
    distribute-function-event(frame, _function)
  end
end method apply-in-frame;


/// Starting frames

// Realizes and displays the frame's UI, then starts the frame's event loop.
// Note that when 'start-frame' is called on modeless, owned frames, it
// will return immediately with a single value of #f
define method start-frame
    (frame :: <simple-frame>)
 => (status-code :: false-or(<integer>))
  assert(frame-state(frame) ~== #"destroyed",
         "You cannot start frame %= because it has been destroyed", frame);
  port-start-frame(port(frame), frame)
end method start-frame;

define method port-start-frame
    (_port :: <port>, frame :: <simple-frame>)
 => (status-code :: false-or(<integer>))
  block (return)
    let framem = frame-manager(frame);
    dynamic-bind (*current-frame* = frame)
      // Map the frame now if it's not already mapped
      unless (frame-mapped?(frame))
        frame-mapped?(frame) := #t
      end;
      // If we are starting an "owned" frame, we need to be careful with
      // multiple threads -- in an "in thread" event processor, we mustn't
      // start a new event loop since we already have one. 
      if (frame-needs-event-queue?(framem, frame, mode: frame-mode(frame)))
	block ()
	  let status-code = execute-frame-top-level(frame);
	  return(status-code);
	cleanup
	  note-frame-top-level-finished(frame)
	end
      else
	// Modeless, owned frame sharing a thread and event queue!
	// Just return #f
        return(#f)
      end
    end
  end
end method port-start-frame;

define method execute-frame-top-level
    (frame :: <simple-frame>)
 => (status-code :: false-or(<integer>))
  duim-debug-message("Starting up frame %=", frame);
  when (frame-thread(frame))
    cerror("Bludgeon ahead, assuming the risk",
	   "The thread %= is already running the top-level function for frame %=",
	   frame-thread(frame), frame)
  end;
  frame-thread(frame) := current-thread();
  block (return)
    // Set up the exit function for use by the exit event handler
    frame.%exit-function := return;
    duim-debug-message("Starting top level loop for frame %=", frame);
    with-abort-restart-loop ("Exit frame %s",
			     frame-title(frame) | "Unnamed frame")
      // This will return when a <frame-exited-event> comes in
      frame-top-level(frame)
    end;
  cleanup
    duim-debug-message("Finished top level loop for frame %=", frame);
  end
end method execute-frame-top-level;

//--- Frames really need to have a first-class "command loop" object
//---  - It implements 'read-command', 'do-execute-command', and 'redisplay'
//---  - It has notification protocols to install/deinstall menu bars, etc.
define method frame-top-level
    (frame :: <simple-frame>) => (#rest values)
  // The "read-eval-print" loop for event-driven applications...
  let top-sheet = top-level-sheet(frame);
  assert(top-sheet,
	 "The frame %= does not have a top-level sheet after it was started", frame);
  while (#t)
    // Get an event and then handle it.  Note that the user
    // program is responsible for managing its own redisplay.
    let event  = read-event(top-sheet);
    let client = event-client(event);
    unless (instance?(event, <pointer-motion-event>))
      duim-debug-message("  Handling event %= for %=", event, client)
    end;
    // Note that if a <frame-exited-event> comes in, the
    // event handler for it will exit this loop by calling the
    // frame.%exit-function thunk
    handle-event(event-handler(client), event)
  end
end method frame-top-level;

define method note-frame-top-level-finished
    (frame :: <simple-frame>) => ()
  event-queue-clear(frame-command-queue(frame));
  when (top-level-sheet(frame))		// some hackers do this themselves
    let queue = sheet-event-queue(top-level-sheet(frame));
    when (queue)
      event-queue-clear(queue)
    end
  end;
  frame-thread(frame)  := #f
end method note-frame-top-level-finished;


/// Stopping frames

define method exit-frame
    (frame :: <frame>, #key destroy? = #t) => ()
  let _port = port(frame);
  when (_port)
    distribute-event(_port, make(<frame-exit-event>,
				 frame: frame,
				 destroy-frame?: destroy?));
    let top-sheet = top-level-sheet(frame);
    when (top-sheet)
      generate-trigger-event(_port, top-sheet)
    end
  end
end method exit-frame;

define method exit-frame
    (sheet :: <sheet>, #key destroy? = #t) => ()
  let frame = sheet-frame(sheet);
  exit-frame(frame, destroy?: destroy?)
end method exit-frame;

// Intended to be specialized by users
define method frame-can-exit? 
    (frame :: <frame>) => (can-exit? :: <boolean>)
  #t
end method frame-can-exit?;

define method handle-event
    (frame :: <frame>, event :: <frame-exit-event>) => ()
  when (frame-can-exit?(frame))
    do-exit-frame(frame-manager(frame), frame, destroy?: event-destroy-frame?(event))
  end
end method handle-event;

define open generic do-exit-frame 
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>,
     #key status-code, destroy?) => ();

define method do-exit-frame
    (framem :: <frame-manager>, frame :: <frame>,
     #key status-code, destroy? = #t) => ()
  // First get rid of the windows
  if (destroy?)
    // 'destroy-frame' will arrange to handle a <frame-destroyed-event>...
    destroy-frame(frame)
  else
    frame-mapped?(frame) := #f
  end;
  // Now we can claim that the frame has exited
  // Note that we use 'handle-event' instead of 'distribute-event',
  // because the event loop and event queue may well already be gone
  handle-event(event-handler(frame),
	       make(<frame-exited-event>,
		    frame: frame,
		    status-code: status-code))
end method do-exit-frame;

define method handle-event
    (frame :: <frame>, event :: <frame-exited-event>) => ()
  let exit-function = frame.%exit-function;
  when (exit-function)
    frame.%exit-function := #f;
    duim-debug-message("Shutting down event loop for frame %=", frame);
    exit-function(event-status-code(event))
  end
end method handle-event;


/// Pointer documentation

define method display-pointer-documentation
    (frame :: <frame>, sheet :: false-or(<sheet>),
     string :: false-or(<string>)) => ()
  let framem = frame-manager(frame) | find-frame-manager();
  do-display-pointer-documentation(framem, frame, sheet, string)
end method display-pointer-documentation;

define method do-display-pointer-documentation
    (framem :: <frame-manager>, frame :: <frame>, sheet :: false-or(<sheet>),
     string :: false-or(<string>)) => ()
  let sheet = pointer-documentation-sheet(framem, frame, sheet);
  when (sheet)
    with-sheet-medium (medium = sheet)
      clear-box*(medium, sheet-viewport-region(sheet));
      when (string)
	draw-text(medium, string, 0, 0,
		  align-x: #"left", align-y: #"top")
      end
    end
  end
end method do-display-pointer-documentation;

// This method allows a frame to redirect the pointer documentation
define method pointer-documentation-sheet
    (framem :: <frame-manager>, frame :: <frame>, sheet :: false-or(<sheet>))
 => (sheet :: false-or(<sheet>))
  sheet
end method pointer-documentation-sheet;


/*
///---*** This all belongs in the presentation layer

define variable *pointer-documentation-interval*
    = max(truncate($internal-time-units-per-second * 0.10), 1);

define variable *last-pointer-documentation-time* = 0;

define variable *last-pointer-documentation-modifier-state* = 0;

define method document-highlighted-presentation
    (frame :: <frame>, sheet :: false-or(<sheet>),
     presentation, input-context, window, x, y) => ()
  let framem = frame-manager(frame) | find-frame-manager();
  display-presentation-documentation
    (framem, frame, sheet, presentation, input-context, window, x, y)
end method document-highlighted-presentation;

define method display-presentation-documentation
    (framem :: <frame-manager>, frame :: <frame>, sheet :: false-or(<sheet>),
     presentation, input-context, window, x, y) => ()
  ignore(input-context, x, y);
  block (return)
    let framem = frame-manager(frame) | find-frame-manager();
    when (pointer-documentation-sheet(framem, frame, sheet))
      // The documentation should never say anything if we're not over a presentation
      unless (presentation)
        display-pointer-documentation(frame, sheet, #f)
      end;
      // Cheap test to not do this work too often
      let old-modifier-state = *last-pointer-documentation-modifier-state*;
      let modifier-state = port-modifier-state(port(window));
      let last-time = *last-pointer-documentation-time*;
      let time = get-internal-real-time();
      *last-pointer-documentation-modifier-state* := modifier-state;
      when (time < last-time + *pointer-documentation-interval*
	    & modifier-state = old-modifier-state)
	return(#f)
      end;
      *last-pointer-documentation-time* := time;
      //---*** See frame-manager-display-pointer-documentation
      //---*** See frame-document-highlighted-presentation-1
      //---*** See find-applicable-translators-for-documentation
      do-display-presentation-documentation
        (framem, frame, sheet, presentation, input-context, window, x, y)
    end
  end
end method display-presentation-documentation;
*/
