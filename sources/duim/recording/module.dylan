Module:	      Dylan-User
Synopsis:     DUIM output recording
Author:	      Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module duim-recording
  create <abstract-output-record>,
	 <composite-output-record>,
	 <graphics-record>,
	 <leaf-output-record>,
	 <output-record>,
	 <recording-pane>,
	 <sequence-output-history>,
	 <sequence-record>,
	 <tree-output-history>,
	 <tree-record>,
	 clear-output-history,
	 composite-output-record?,
	 // \dragging-output, do-dragging-output,
	 highlight-output-record,
	 leaf-output-record?,
	 output-record?,
	 graphics-record?,
	 refined-position-test,
	 erase-output-record,
	 sheet-child-count,
	 sheet-drawing?, sheet-drawing?-setter, sheet-drawing?-dynamic-binder,
	 sheet-element,
	 sheet-highlighted-record, sheet-highlighted-record-setter,
	 sheet-output-history, sheet-output-history-setter,
	 sheet-output-record, sheet-output-record-setter, sheet-output-record-dynamic-binder,
	 sheet-recording?, sheet-recording?-setter, sheet-recording?-dynamic-binder,
	 \tracking-pointer, do-tracking-pointer,
	 \with-end-of-line-action, do-with-end-of-line-action,
	 \with-end-of-page-action, do-with-end-of-page-action,
	 \with-first-quadrant-coordinates, do-with-first-quadrant-coordinates,
	 \with-local-coordinates, do-with-local-coordinates,
	 \with-new-output-record, do-with-new-output-record,
	 // \with-output-as-gadget, do-with-output-as-gadget,
	 \with-output-recording-options, do-with-output-recording-options,
	 \with-output-to-output-record, do-with-output-to-output-record,
	 \with-room-for-graphics, do-with-room-for-graphics;
end module duim-recording;

define module duim-recording-internals
  use dylan;
  use duim-imports;
  use duim-utilities;
  use duim-geometry-internals;
  use duim-DCs-internals;
  use duim-sheets-internals;
  use duim-graphics-internals;
  use duim-layouts-internals;
  use duim-gadgets-internals;
  use duim-frames-internals;
  use duim-recording, export: all;

  export <basic-composite-record>,
	 <basic-leaf-record>,
	 <composite-output-record-mixin>,
	 <medium-state>,
	 <output-history-mixin>,
	 <output-recording-mixin>,
	 <output-record-element-mixin>,
	 add-output-record,
	 do-with-new-output-record-1,
	 medium-state-brush,
	 medium-state-clipping-region,
	 medium-state-pen,
	 medium-state-text-style,
	 move-caret-beyond-output-record,
	 note-child-added-1,
	 output-history?,
	 \output-record-constructor-definer,
	 recompute-region, do-recompute-region,
	 record-medium-state,
	 record-redisplay-state,
	 sheet-caret-position, set-sheet-caret-position,
	 sheet-medium-state, sheet-medium-state-setter,
	 sheet-output-record-position,
	 sheet-redisplay-record, sheet-redisplay-record-setter,
	 sheet-redisplaying?, sheet-redisplaying?-setter,
	 stream-text-output-record, stream-text-output-record-setter, stream-text-output-record-dynamic-binder,
	 update-region-for-changed-child,
	 update-region-for-new-child,
	 \with-record-medium-state;

  //--- Needed until macro hygiene works...
  export *output-record-constructors*;

  // Incremental redisplay stubs
  export copy-display-state,
	 find-child-output-record,
	 // match-output-records,
	 recompute-contents-ok;
end module duim-recording-internals;
