Module:    dylan-user
Synopsis:  A miniature DUIM
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define module mini-duim
  use dylan;
  use functional-extensions;

  // For debugging
  use simple-streams;
  use simple-format;

  //--- use transcendentals;

  // Geometry
  export <coordinate>,
	 \fix-coordinates!,
         do-coordinates, do-endpoint-coordinates,
	 <region>,
	 <point>, make-point,
         point-x, point-y, point-position,
         <bounding-box>, make-bounding-box,
         box-left, box-top, box-right, box-bottom, box-edges,
         box-width, box-height, box-size,
         <transform>, make-translation-transform,
         $identity-transform,
	 compose-transforms,
         compose-translation-with-transform,
	 invert-transform,
         transform-position, untransform-position, 
         transform-distance, untransform-distance, 
         transform-box, untransform-box,
         transform-region, untransform-region,
         ltrb-contains-position?,
         ltrb-contains-ltrb?, ltrb-intersects-ltrb?;

  // DCs
  export <ink>,
         $foreground, $background,
	 <color>, make-rgb-color,
         color-rgb,
         $black, $red, $green, $blue, $cyan, $magenta, $yellow, $white,
         <palette>, <basic-palette>, make-palette,
         <pen>,
         pen-width, pen-units, pen-dashes,
         pen-joint-shape, pen-cap-shape,
         \with-pen,
         <brush>,
         brush-foreground, brush-background,
	 brush-mode,
	 brush-fill-style, brush-fill-rule,
	 brush-tile, brush-stipple, brush-ts-x, brush-ts-y,
	 brush-stretch-mode,
         \with-brush,
         <text-style>, make-text-style,
         text-style-components,
         text-style-mapping, do-text-style-mapping,
         font-width, font-height,
         font-ascent, font-descent,
         fixed-width-font?,
         \with-text-style;

  // Sheets, mediums, and ports
  export <sheet>, <basic-sheet>,
         make-pane, do-make-pane,
         class-for-make-pane,
         sheet-region, sheet-transform,
         sheet-parent, sheet-parent-setter,
         sheet-children, sheet-children-setter,
         sheet-medium,
         sheet-mirror, sheet-direct-mirror,
         sheet-frame,
         sheet-edges, set-sheet-edges,
         sheet-position, set-sheet-position,
         sheet-size, set-sheet-size,
         add-child, note-child-added,
         remove-child, note-child-removed,
         sheet-mapped?, sheet-mapped?-setter,
         destroy-sheet,
         <medium>, <basic-medium>,
         medium-sheet, 
         medium-drawable, medium-drawable-setter,
         medium-pen, medium-brush, medium-text-style,
         \with-drawing-options, do-with-drawing-options,
         beep,
         make-medium,
         destroy-medium, do-destroy-medium,
         <port>, <basic-port>,
         port, port-setter,
         find-port, class-for-make-port,
         port-pointer, port-modifier-state,
         pointer-button-state,
         pointer-position, do-pointer-position,
         set-pointer-position, do-set-pointer-position,
         <display>, <basic-display>, <standard-display>,
         <space-requirement>;

  // Mirrors
  export <mirror>,
         <win32-mirror>,
         <window-mirror>,
         <menu-mirror>,
         <top-level-mirror>,
         make-mirror, do-make-mirror,
         destroy-mirror, do-destroy-mirror,
         map-mirror, do-map-mirror,
         unmap-mirror, do-unmap-mirror,
         raise-mirror, do-raise-mirror,
         lower-mirror, do-lower-mirror,
         make-win32-menu,
         %window-handle,
         %DC, %DC-setter,
         sheet-device-region,
         sheet-device-transform,
         sheet-native-edges,
         mirror-edges, mirror-inside-edges,
         mirror-visible?;

  // Events
  export <event>,
         <key-press-event>, <key-release-event>,
         <button-press-event>, <button-release-event>, <double-click-event>,
         <pointer-motion-event>, <pointer-drag-event>,
         <window-configuration-event>, <window-repaint-event>,
         event-client, event-sheet, event-frame,
         event-modifier-state,
         event-key-name, event-character,
         event-x, event-y,
         event-button,
         event-region,
         $left-button, $middle-button, $right-button,
         $shift-key, $control-key, $meta-key,
         handle-event,
         handle-button-event,
         handle-repaint, repaint-sheet,
         port-event-loop, 
         process-messages, process-next-message;

  // Gadgets
  export <top-level-sheet>,
         <simple-pane>,
         <drawing-pane>,
	 <gadget>, <basic-gadget>,
	 gadget-id, gadget-client,
	 gadget-label, gadget-label-setter,
         gadget-editable?, gadget-editable?-setter,
	 gadget-enabled?, gadget-enabled?-setter,
         gadget-selection-mode, gadget-selection-mode-setter,
	 gadget-value, gadget-value-setter,
	 gadget-value-range, gadget-value-range-setter,
         <label>,
	 <push-button>, <radio-button>, <check-button>,
         <menu-bar>, <menu>,
         <menu-box>,
         <menu-button>,
         <push-menu-button>, <radio-menu-button>, <check-menu-button>,
         <test-field>, <password-field>,
         menu-owner;

  // Collection gadgets
  export <list-box>,
         <option-box>,
         gadget-items, gadget-items-setter,
         gadget-item-name,
         gadget-item-selected?,
         gadget-selection, gadget-selection-setter;

  // Layout
  export <space-requirement>,
         $fill,
	 space-requirement-components,
	 compose-space, do-compose-space,
         allocate-space, do-allocate-space;

  // Graphics
  export draw-point*, do-draw-point*,
         draw-points*, do-draw-points*,
	 draw-line*, do-draw-line*,
	 draw-lines*, do-draw-lines*,
	 draw-rectangle*, do-draw-rectangle*,
	 draw-rectangles*, do-draw-rectangles*,
	 draw-rounded-rectangle*, do-draw-rounded-rectangle*,
	 draw-polygon*, do-draw-polygon*,
	 draw-ellipse*, do-draw-ellipse*,
	 draw-text*, do-draw-text*,
/*
         abort-path, do-abort-path,
         clip-from-path, do-clip-from-path,
         close-path, do-close-path,
         curve-to*, do-curve-to*,
         end-path, do-end-path,
         fill-path, do-fill-path,
         line-to*, do-line-to*,
         move-to*, do-move-to*,
         restore-clipping-region, do-restore-clipping-region,
         save-clipping-region, do-save-clipping-region,
         start-path, do-start-path,
         stroke-path, do-stroke-path,
*/
	 clear-box, clear-box*,
         update-drawing-state, invalidate-cached-drawing-state,
         force-display, synchronize-display;

  // Frame managers
  export <frame-manager>, <basic-frame-manager>,
         find-frame-manager, make-frame-manager,
         attach-frame,
         frame-manager,
         frame-wrapper,
         note-title-changed,
         \with-frame-manager;

  // Frames
  export <frame>, <simple-frame>,
         \frame-definer,
         \frame-class-definer, \frame-panes-definer, \frame-gadget-bars-definer,
         frame-layout, frame-layout-setter,
         frame-mapped?, frame-mapped?-setter,
         frame-menu-bar, frame-menu-bar-setter,
         frame-tool-bar, frame-tool-bar-setter,
         frame-status-bar, frame-status-bar-setter,
         frame-title, frame-title-setter,
         generate-panes,
         start-frame,
         frame-exit,
         find-frame,
         top-level-sheet,
         destroy-frame,
         contain, make-container;

  // Debug support
  export enter-debugger,
         report-error;

  // Modularity hacks
  export %layout, %layout-setter,
         %menu-bar, %menu-bar-setter,
         %tool-bar, %tool-bar-setter,
         %status-bar, %status-bar-setter;
end module mini-duim;
