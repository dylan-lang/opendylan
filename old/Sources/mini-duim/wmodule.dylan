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
  use equal-table;	//---*** Because C pointers don't compare with ==

  // For debugging
  use simple-streams;
  use simple-format;

  use transcendentals;

  // Utilities
  export \inc!, \dec!,
         \min!, \max!,
         warn,
         enter-debugger;

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
         <foreground>, <background>,
         $foreground, $background,
	 <color>, 
         <contrasting-color>,
         make-rgb-color,
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

  // Images
  export <image>,
         <stencil>;

  // Sheets, mediums, and ports
  export <abstract-sheet>, <sheet>, <basic-sheet>,
         make-pane,
         class-for-make-pane,
         sheet-region, sheet-transform,
         sheet-parent, sheet-parent-setter,
         sheet-children, sheet-children-setter,
         sheet-container,
         sheet-medium,
         sheet-mirror, 
         sheet-direct-mirror, sheet-direct-mirror-setter,
         sheet-frame,
         sheet-edges, set-sheet-edges,
         sheet-position, set-sheet-position,
         sheet-size, set-sheet-size,
         add-child, note-child-added,
         remove-child, note-child-removed,
         sheet-mapped?, sheet-mapped?-setter,
         destroy-sheet,
         <abstract-medium>, <medium>, <basic-medium>,
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
         initialize-display,
         <space-requirement>;

  // Mirrors
  export <mirror>,
         <win32-mirror>,
         <window-mirror>,
         <menu-mirror>,
         <top-level-mirror>,
         make-mirror, do-make-mirror,
         destroy-mirror,
         map-mirror,
         unmap-mirror,
         raise-mirror,
         lower-mirror,
         make-win32-menu,
         %window-handle,
         %DC, %DC-setter,
         sheet-delta-transform,
         sheet-device-region,
         sheet-device-transform,
         sheet-native-edges,
         sheet-parent-mirror,
         mirror-edges, set-mirror-edges,
         mirror-sheet, mirror-sheet-setter,
         mirror-visible?;

  // Events
  export <event>,
         <sheet-event>, <frame-event>,
         <key-press-event>, <key-release-event>,
         <button-press-event>, <button-release-event>, <double-click-event>,
         <pointer-motion-event>, <pointer-drag-event>,
         <window-configuration-event>, <window-repaint-event>,
         event-sheet, event-frame,
         event-modifier-state,
         event-key-name, event-character,
         event-x, event-y,
         event-button,
         event-region,
         $left-button, $middle-button, $right-button,
         $shift-key, $control-key, $meta-key,
         handle-event,
         handle-button-event,
         handle-repaint, repaint-sheet;

  // Gadgets
  export <top-level-sheet>,
         <simple-pane>,
         <drawing-pane>,
	 <abstract-gadget>, <gadget>, <basic-gadget>,
         <viewport>,
         button-gadget-value,
	 gadget-id, gadget-client,
	 gadget-label, gadget-label-setter,
         gadget-label-size,
         gadget-editable?, gadget-editable?-setter,
	 gadget-enabled?, gadget-enabled?-setter,
         gadget-selection-mode, gadget-selection-mode-setter,
	 gadget-value, gadget-value-setter,
	 gadget-value-range, gadget-value-range-setter,
         note-gadget-enabled, note-gadget-disabled,
         update-gadget,
         <label>,
	 <push-button>, <radio-button>, <check-button>,
         <button-box>,
         <menu-bar>, <menu>,
         <menu-box>,
         <menu-button>,
         <push-menu-button>, <radio-menu-button>, <check-menu-button>,
         <text-field>, <password-field>, <scroll-bar>,
         button-box-spacing,
         menu-owner;

  // Collection gadgets
  export <list-box>,
         <option-box>,
         gadget-items, gadget-items-setter,
         gadget-item-name,
         gadget-item-selected?,
	 gadget-selection, gadget-selection-setter;

  // Controls
  export <list-control>,
	 <list-item>,
	 <table-control>,
	 <table-item>,
	 <tree-control>,
	 <tree-node>,
         add-column,
	 add-item, do-add-item,
	 contract-node, do-contract-node,
	 expand-node, do-expand-node,
	 find-item,
	 make-item,
	 node-children,
	 node-parents,
         remove-column,
       	 remove-item, do-remove-item,
	 tree-control-icon-function,
	 tree-control-inferior-predicate,
	 tree-control-inferior-producer,
	 tree-control-roots, tree-control-roots-setter,
	 tree-control-show-buttons?,
	 tree-control-show-edges?,
	 tree-control-show-root-edges?;

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
    /* ---
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
    --- */
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

  // Standard dialogs
  export <notification-style>,
         notify-user,
         choose-color,
         choose-directory,
         choose-file,
         choose-from-menu,
         choose-from-dialog;

  // Frames
  export <frame>, <simple-frame>,
         \frame-definer,
         \frame-class-definer, \frame-panes-definer, \frame-gadget-bars-definer,
         frame-layout, frame-layout-setter,
         frame-mapped?, frame-mapped?-setter,
         frame-menu-bar,
         frame-tool-bar,
         frame-status-bar,
         frame-title, frame-title-setter,
         generate-panes,
         update-frame-layout,
         start-frame,
         frame-exit,
         find-frame,
         top-level-sheet,
         destroy-frame,
         contain, make-container;

  // Debug support
  export enter-debugger,
         report-error;

  // Exports required by win32-duim
  export <button>,
         <collection-gadget-mixin>,
         <column-layout>,
         <focus-in-gadget-event>,
         <focus-out-gadget-event>,
         <frame-destroyed-event>,
         <leaf-pane>,
         <mirrored-sheet-mixin>,
         <multiple-child-composite-pane>,
         <permanent-medium-mixin>,
         <pixmap>,
         <pointer>,
         <basic-pixmap-medium>,
         <single-child-composite-pane>,
         <standard-input-mixin>,
         <standard-repainting-mixin>,
         <standard-repainting-mixin>,
         <standard-text-style>,
         *debug*,
         $2pi,
         $boole-clr,   $boole-set,   $boole-1,    $boole-2,
         $boole-c1,    $boole-c2,    $boole-and,  $boole-ior,
         $boole-xor,   $boole-eqv,   $boole-nand, $boole-nor,
         $boole-andc1, $boole-andc2, $boole-orc1, $boole-orc2,
         $dotted-pen, $dash-dot-pen, $dash-dot-dot-pen,
         $nowhere, $everywhere,
         \protocol-definer, \protocol-predicate-definer, \protocol-class-definer,
         compute-clip-mask,
         compute-mnemonic-from-label,
         compute-text-adjustment,
         compute-used-mnemonics,
         convert-to-device-coordinates!,
         convert-to-device-distances!,
         deallocate-medium,
         decode-pattern,
         default-button?,
         display-mm-width-setter,
         display-mm-height-setter,
         display-pixel-width-setter,
         display-pixel-height-setter,
         display-pixels-per-point-setter,
         distribute-activate-callback,
         distribute-event,
         distribute-focus-in-callback,
         distribute-focus-out-callback,
         distribute-function-event,
         distribute-value-changed-callback,
         distribute-value-changing-callback,
         do-abort-path,
         do-arc-to*,
         do-attach-medium,
         do-choose-color,
         do-choose-file,
         do-choose-directory,
         do-choose-from-menu,
         do-choose-from-dialog,
         do-clip-from-path, 
         do-close-path, 
         do-curve-to*, 
         do-destroy-pixmap,
         do-detach-medium, 
         do-draw-image*, 
         do-draw-pixmap*, 
         do-end-path, 
         do-fill-path, 
         do-line-to*,
         do-make-pixmap,
         do-move-to*,
         do-notify-user, 
         do-queue-repaint, 
         do-restore-clipping-region, 
         do-save-clipping-region, 
         do-set-pointer-cursor, 
         do-set-sheet-pointer-cursor, 
         do-start-path, 
         do-stroke-path, 
         drawing-state-cached?, drawing-state-cached?-setter,
         elliptical-arc-box,
         find-parent-of-class,
         frame-geometry,
         frame-owner,
         gadget-name-key,
         gadget-orientation,
         gadget-scrolling-horizontally?,
         gadget-scrolling-vertically?,
         gadget-slug-size, gadget-slug-size-setter,
         gethash, gethash-setter,
         handle-button-gadget-click,
         image-depth,
         image-height,
         image-width,
         invalidate-cached-region,
         invalidate-cached-regions,
         invalidate-cached-transform,
         invalidate-cached-transforms,
         make-color-for-contrasting-color,
         make-device-font,
         make-pixmap-medium,
         make-text-style-from-hfont,
         medium-clipping-region, medium-clipping-region-setter,
         medium-background, medium-background-setter,
         medium-foreground, medium-foreground-setter,
         medium-merged-text-style,
         note-gadget-label-changed,
         note-gadget-value-changed,
         pointer-button-state-setter,
         port-default-background,
         port-default-foreground,
         port-font-mapping-table,
         port-handles-repaint?,
         port-modifier-state-setter,
         port-name,
         port-type,
         process-pending-events,
         process-next-event,
         remove-all-keys!,
         scroll-to-position,
         scroll-down-line,
         scroll-up-line,
         scroll-down-page,
         scroll-up-page,
         sheet-mirrored-ancestor,
         sheet-region-setter,
         standardize-text-style-size,
         text-control-text,
         text-style-family;

  // Modularity hacks
  export %layout, %layout-setter,
         %menu-bar, %menu-bar-setter,
         %tool-bar, %tool-bar-setter,
         %status-bar, %status-bar-setter;
end module mini-duim;

define module win32-duim
  use dylan;
  use functional-extensions;
  use equal-table;	//---*** Because C pointers don't compare with ==

  // For debugging
  use simple-streams;
  use simple-format;

  use transcendentals;

  use win32-common,
    rename: { <point> => w/<point> };
  use win32-user;
  use win32-gdi;
  use win32-kernel,
    rename: { beep => w/beep };
  use win32-dialog;
  //--- For 'window-callback-function' and 'enter-debugger'...
  //---use idvm-application;

  use mini-duim;
end module win32-duim;
