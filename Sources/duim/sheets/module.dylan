Module:       Dylan-User
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module duim-sheets
  // Sheets
  create <abstract-sheet>,
	 <sheet>,
         add-child,
         child-containing-position,
         children-overlapping-region,
         clear-box, clear-box*,
	 destroy-sheet,
	 do-sheet-children,
	 do-children-containing-position,
	 do-children-overlapping-region,
	 do-sheet-tree,
	 lower-sheet,
	 raise-sheet,
	 remove-child,
	 replace-child,
	 sheet-ancestor?,
	 sheet-child, sheet-child-setter,
	 sheet-children, sheet-children-setter,
	 sheet-cursor, sheet-cursor-setter,
	 sheet-event-mask, sheet-event-mask-setter,
         sheet-handles-events?,
	 sheet-handles-keyboard?,
	 sheet-handles-repaint?,
         sheet-help-context, sheet-help-context-setter,
         sheet-help-source,  sheet-help-source-setter,
	 sheet-mapped?, sheet-mapped?-setter,
	 sheet-parent, sheet-parent-setter,
	 sheet-region, sheet-region-setter,
         sheet-state, sheet-state-setter,
	 sheet-transform, sheet-transform-setter,
	 sheet-withdrawn?, sheet-withdrawn?-setter,
	 sheet?;

  // User-level geometry hacking
  create relayout-children,		//--- this should really be in 'duim-layout'
	 relayout-parent,		//--- this should really be in 'duim-layout'
	 sheet-edges, set-sheet-edges,
         sheet-position, set-sheet-position,
         sheet-size, set-sheet-size;

  // Mediums and output sheets
  create <abstract-medium>,
	 <medium>,
	 beep,
	 force-display,
	 medium-background, medium-background-setter,
	 medium-brush, medium-brush-setter,
	 medium-clipping-region, medium-clipping-region-setter,
	 medium-default-text-style, medium-default-text-style-setter,
	 medium-drawable, medium-drawable-setter,
	 medium-foreground, medium-foreground-setter,
	 medium-merged-text-style,
	 medium-pen, medium-pen-setter,
         medium-pixmap, medium-pixmap-setter,
	 medium-sheet,
	 medium-text-style, medium-text-style-setter,
	 medium-transform, medium-transform-setter,
	 medium?,
         sheet-medium,
         synchronize-display,
         \with-brush,
         \with-clipping-region,
         \with-drawing-options, do-with-drawing-options,
         \with-identity-transform,
         \with-pen,
         \with-rotation,
         \with-scaling,
         \with-sheet-medium, do-with-sheet-medium,
         \with-text-style, do-with-text-style,
         \with-transform, do-with-transform,
         \with-translation;

  // Font mapping
  create <undefined-text-style-mapping>,
	 fixed-width-font?,
	 font-ascent,
	 font-descent,
	 font-height,
	 font-width,
	 font-metrics,
	 text-size,
	 text-style-mapping, text-style-mapping-setter,
	 text-style-mapping-exists?;
  
  // Ports
  create <abstract-port>,
	 <port>,
	 default-port, default-port-setter,
	 destroy-port,
	 do-ports,
	 find-port,
	 get-default-background,
	 get-default-foreground,
	 get-default-text-style,
	 note-port-terminated,
	 port-modifier-state,
	 port-name,
	 port-pointer,
	 port-server-path,
	 port-type,
	 port?,
	 restart-port;

  // Displays
  create <abstract-display>,
	 <display>,
	 display-depth,
	 display-height,
	 display-mm-height,
	 display-mm-width,
	 display-orientation,
	 display-pixel-height,
	 display-pixel-width,
	 display-pixels-per-point,
	 display-units,
	 display-width,
	 display?,
	 do-displays,
	 find-display;

  // Pointers and cursors
  create <pointer>,
         <cursor>,
	 pointer-button-state,
	 pointer-cursor, pointer-cursor-setter,
	 pointer-grabbed?, pointer-grabbed?-setter,
	 pointer-position, set-pointer-position,
	 pointer-sheet,
	 pointer?,
	 \with-busy-cursor, do-with-busy-cursor,
	 \with-pointer-grabbed, do-with-pointer-grabbed;

  // Carets
  create <caret>,
	 caret-position, set-caret-position,
	 caret-sheet,
	 caret-size, set-caret-size,
	 caret-visible?, caret-visible?-setter,
	 caret?,
	 sheet-caret, sheet-caret-setter,
	 \with-caret-hidden,
	 \with-caret-position-saved;

  // Gestures
  create $alt-key,
	 $control-key,
	 $hyper-key,
	 $left-button,
	 $meta-key,
	 $middle-button,
	 $modifier-keys,
	 $option-key,
	 $pointer-buttons,
	 $right-button,
	 $shift-key,
	 $super-key,
         <gesture>,
         <keyboard-gesture>,
         <pointer-gesture>,
	 button-index,
	 button-index-name,
	 event-matches-gesture?,
         gesture?,
	 gesture-character,
	 gesture-button,
	 gesture-equal,
	 gesture-keysym,
	 gesture-modifier-state,
	 make-modifier-state,
	 modifier-key-index,
	 modifier-key-index-name,
	 standard-char->keysym;

  // Event model
  create <event>,
         <event-handler>,
	 event?,
	 event-handler?,
	 event-handler, event-handler-setter, event-handler-dynamic-binder,
	 handle-button-event,
	 handle-event,
	 handle-repaint,
	 queue-event,
	 queue-repaint,
	 repaint-sheet,
	 sheet-event-queue,
	 \with-atomic-redisplay, do-with-atomic-redisplay;
  
  // Events and input sheets
  create <button-press-event>,
	 <button-release-event>,
	 <device-event>,
	 <double-click-event>,
         <frame-event>,
         <input-focus-event>,
         <input-focus-in-event>,
         <input-focus-out-event>,
	 <key-press-event>,
	 <key-release-event>,
	 <keyboard-event>,
	 <pointer-boundary-event>,
	 <pointer-button-event>,
	 <pointer-drag-event>,
	 <pointer-enter-event>,
	 <pointer-event>,
	 <pointer-exit-event>,
	 <pointer-motion-event>,
	 <port-terminated-event>,
         <sheet-event>,
	 <timer-event>,
	 <window-configuration-event>,
	 <window-event>,
	 <window-repaint-event>,
	 boundary-event-kind,
	 event-button,
	 event-client,
	 event-character,
	 event-frame,
	 event-key-name,
	 event-modifier-state, event-matches-modifiers?,
	 event-pointer,
         event-region,
	 event-sheet,
	 event-x,
	 event-y;

  // Clipboards
  create <clipboard>,
         add-clipboard-data,
         add-clipboard-data-as,
	 clear-clipboard,
         clipboard?,
	 clipboard-data-available?,
         clipboard-owner,
         clipboard-sheet,
         close-clipboard,
	 get-clipboard-data-as,
	 open-clipboard,
         \with-clipboard;

  // Frames
  //--- These should really be in 'duim-frames'
  create <abstract-frame>,
	 <frame>,
         current-frame,
	 *current-frame*,		//--- until macro hygiene works...
         destroy-frame,
	 frame?,
	 frame-input-focus, frame-input-focus-setter,
         frame-cursor-override, frame-cursor-override-setter;

  // Frame managers
  create <abstract-frame-manager>,
	 <frame-manager>,
	 do-frames,
	 find-frame-manager,
	 frame-manager-frames,
	 frame-manager-palette, frame-manager-palette-setter,
	 frame-manager?,
	 make-pane,
	 \with-frame-manager,
         *current-frame-manager*;	//--- until macro hygiene works...

  // Standard dialogs
  create <notification-style>,
	 <notification-exit-style>,
	 choose-color,
	 choose-directory,
	 choose-file,
	 choose-from-dialog,
	 choose-from-menu,
	 choose-printer,
	 choose-text-style,
	 notify-user;

  // General accessors
  create display,
	 frame-manager,
	 port,
         sheet-frame,
	 top-level-sheet;

  // Other accessors
  create sheet-line-height,
         sheet-line-spacing;

end module duim-sheets;

define module duim-sheets-internals
  use dylan;
  use duim-imports;
  use duim-utilities;
  use duim-geometry-internals;
  use duim-DCs-internals;
  use duim-sheets, export: all;

  // Sheets
  export <basic-sheet>,
         <multiple-child-mixin>,
         <single-child-mixin>,
         <z-order>,
	 $default-sheet-size,
         bottom-up-iteration-protocol,
         do-add-child,
         do-destroy-sheet,
         do-lower-sheet,
         do-raise-sheet,
         do-remove-child,
         do-replace-child,
         do-set-sheet-cursor,
         initialize-sheet-geometry,
	 invalidate-cached-regions, invalidate-cached-region,
         invalidate-cached-transforms, invalidate-cached-transform,
         note-child-added,
         note-child-removed,
         note-region-changed,
         note-sheet-attached, do-note-sheet-attached,
         note-sheet-detached, do-note-sheet-detached,
         note-sheet-mapped,
         note-sheet-unmapped,
         note-transform-changed,
	 repaint-within-parent,
	 sheet-accepts-focus?, sheet-accepts-focus?-setter,
	 sheet-attached?,
         sheet-delta-transform,
         sheet-flags, sheet-flags-setter,
         sheet-input-focus,
	 sheet-layed-out?, sheet-layed-out?-setter,
         sheet-layed-out-to-size?,
	 sheet-mirror-accepts-children?, sheet-mirror-accepts-children?-setter,
	 sheet-shell,
         sheet-tab-stop?,
	 top-down-iteration-protocol;

  // User-level geometry hacking
  export validate-sheet-size;

  // Mediums and output sheets
  export <basic-medium>,
         <drawable>,
         <permanent-medium-mixin>,
         <sheet-with-medium-mixin>,
         <shared-medium-mixin>,
	 $medium-brush-cached,
	 $medium-font-cached,
	 $medium-fully-cached,
	 $medium-pen-cached,
	 $medium-region-cached,
         allocate-medium,
         attach-medium, do-attach-medium,
         deallocate-medium,
         destroy-medium,
         detach-medium, do-detach-medium,
         find-sheet-with-medium,
	 invalidate-cached-drawing-state,
         make-medium,
         medium-+Y-upward?, medium-+Y-upward?-setter,
	 medium-+Y-upward?-dynamic-binder,
         medium-brush-dynamic-binder,
         medium-clipping-region-dynamic-binder,
         medium-device-transform, medium-device-transform-setter,
	 medium-device-transform-dynamic-binder,
         medium-drawable-dynamic-binder,
	 medium-drawing-state-cache, medium-drawing-state-cache-setter,
	 medium-merged-text-style-setter,
	 medium-merged-text-style-dynamic-binder,
         medium-pen-dynamic-binder,
         medium-sheet-setter,
	 medium-sheet-dynamic-binder,
         medium-text-style-dynamic-binder,
         medium-transform-dynamic-binder,
         sheet-has-medium?,
	 sheet-medium-setter,
	 sheet-medium-dynamic-binder,
         \with-temporary-medium;

  // Font mapping
  export compute-text-adjustment,
         do-text-style-mapping,
         glyph-for-character,
         index-and-character-set,
         standardize-text-style,
         standardize-text-style-size;

  // Ports
  export <basic-port>,
         <focus-policy>,
         *global-lock*,
         register-port-class,
	 class-for-make-port,
         note-focus-in, note-focus-out,
	 port-default-background,
         port-default-foreground,
	 port-default-frame-manager, port-default-frame-manager-setter,
         port-default-palette, port-default-palette-setter,
         port-default-text-style,
         port-displays, port-displays-setter,
         port-display-class,
         port-event-thread,
         port-frame-managers,
	 port-focus-policy,
         port-font-mapping-cache,
         port-font-mapping-table,
         port-input-focus, port-input-focus-setter,
         port-matches-server-path?,
	 port-modifier-state-setter,
         port-event-processor-type,
	 port-pointer-setter,
         port-properties, port-properties-setter,
         port-undefined-text-style, port-undefined-text-style-setter,
         process-next-event,
         \with-port-locked;

  // Displays
  export <basic-display>,
         <standard-display>,
         attach-sheet,
         detach-sheet,
         display-matches-characteristics?,
         display-mm-height-setter,
         display-mm-width-setter,
         display-pixel-height-setter,
         display-pixel-width-setter,
         display-pixels-per-point-setter,
         initialize-display,
         \with-display-locked;

  // Mirrors
  export <mirror>,
	 <mirrored-sheet-mixin>,
	 <sheet-with-resource-mixin>,
         destroy-mirror,
         lower-mirror,
         make-mirror, do-make-mirror,
         map-mirror,
         mirror-edges, set-mirror-edges,
         mirror-origin,
         mirror-region,
         mirror-sheet, mirror-sheet-setter,
         mirror-visible?,
         mirror?,
         note-mirror-geometry-changed,
         raise-mirror,
         sheet-device-edges,
         sheet-device-parent,
         sheet-device-region,
         sheet-device-transform,
         sheet-direct-mirror, sheet-direct-mirror-setter,
         sheet-mirror,
         sheet-native-edges,
         sheet-native-region,
         sheet-native-transform, sheet-native-transform-setter,
         sheet-resource-id,
	 unmap-mirror,
         update-all-mirror-positions,
	 update-mirror-region,
         update-mirror-transform;

  // Pointers and cursors
  export <standard-pointer>,
         do-pointer-position, do-set-pointer-position,
         do-set-pointer-cursor,
         grab-pointer, ungrab-pointer,
	 pointer-button-state-setter,
         pointer-state-changed?,
         update-pointer-cursor;

  // Carets
  export <basic-caret>,
	 <sheet-with-caret-mixin>,
	 do-set-caret-position,
	 do-set-caret-size,
         do-show-caret, do-hide-caret,
	 make-caret;

  // Gestures
  export $altgr-key,
	 $bucky-keys,
	 $capslock-key,
	 modifier-state-matches-gesture?;

  // Event model
  export do-queue-repaint,
	 sheet-event-queue-setter;
  
  // Events and input sheets
  export <basic-event>,
         <function-event>,
         <delegate-input-mixin>,
         <immediate-input-mixin>,
         <immediate-repainting-mixin>,
         <null-input-mixin>,
         <null-repainting-mixin>,
	 <sheet-with-event-queue-mixin>,
         <sheet-with-repainting-mixin>,
         <standard-input-mixin>,
         <standard-repainting-mixin>,
         dispatch-event, do-dispatch-event,
         distribute-event, do-distribute-event,
         distribute-function-event,
         event-pending?,
         event-timestamp,
	 generate-trigger-event,
         peek-event,
         port-handles-repaint?,
         read-event,
         read-event-no-hang,
	 unread-event,
         wait-for-event;

  // Event queues
  export <event-queue>,
         event-queue-clear,
         event-queue-empty?,
         event-queue-pop,
         event-queue-push,
         event-queue-push-last,
         event-queue-top,
         event-queue-wait;

  // Clipboard
  export maybe-clear-clipboard;

  // Frame managers
  export <basic-frame-manager>,
         <portable-frame-manager>,
         current-frame-manager,
         class-for-make-pane,
         destroy-frame-manager,
         frame-manager-do-frames,
	 frame-manager-matches-options?,
	 make-frame-manager;

  // Standard dialogs
  export do-choose-color,
         do-choose-directory,
         do-choose-file,
         do-choose-from-dialog,
         do-choose-from-menu,
         do-choose-printer,
	 do-choose-text-style,
         do-notify-user;

  // General accessors
  export display-setter,
         find-ancestor-of-type,
	 frame-manager-setter,
	 port-setter, 
         sheet-frame-setter,
	 top-level-sheet-setter;

  //--- Kludge for 'add-child' vs. multiple layouts
  export *old-layout*;

  //--- Work around the fact that 'port-setter' is overloaded...
  export %port-setter;
end module duim-sheets-internals;
