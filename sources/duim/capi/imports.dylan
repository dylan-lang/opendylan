Module:       CAPI-DUIM
Synopsis:     CAPI back-end
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Performance hacks
define lisp-interface
  functions find-character-in-string from cl-user as find-character;
end;

// Graphics Ports
define lisp-interface
  classes graphics-port-mixin from gp as <graphics-port-mixin>;
  functions graphics-port-graphics-state from gp,
            graphics-state-background from gp,
            graphics-state-background-setter from gp,
            graphics-state-foreground from gp,
            graphics-state-foreground-setter from gp,
            graphics-state-fill-style from gp,
            graphics-state-fill-style-setter from gp,
            graphics-state-operation from gp,
            graphics-state-operation-setter from gp,
            graphics-state-pattern from gp,
            graphics-state-pattern-setter from gp,
            graphics-state-mask from gp,
            graphics-state-mask-setter from gp,
            graphics-state-thickness from gp,
            graphics-state-thickness-setter from gp,
            graphics-state-dashed from gp,
            graphics-state-dashed-setter from gp,
            graphics-state-dash from gp,
            graphics-state-dash-setter from gp,
            graphics-state-line-end-style from gp,
            graphics-state-line-end-style-setter from gp,
            graphics-state-line-joint-style from gp,
            graphics-state-line-joint-style-setter from gp,
            graphics-state-font from gp,
            graphics-state-font-setter from gp;
  functions draw-point from gp as gp-draw-point,
            draw-points from gp as gp-draw-points,
            draw-line from gp as gp-draw-line,
            draw-lines from gp as gp-draw-lines,
            draw-rectangle from gp as gp-draw-rectangle,
            draw-rectangles from gp as gp-draw-rectangles,
            draw-polygon from gp as gp-draw-polygon,
            draw-arc from gp as gp-draw-arc,
            draw-string from gp as gp-draw-string,
            draw-character from gp as gp-draw-char;
  functions make-pixarray from gp,
            pixarray-to-generic-image from gp,
            load-generic-image from gp as gp-load-generic-image,
            draw-generic-image from gp as gp-draw-generic-image,
            generic-image-p from gp as gp-generic-image-p,
            generic-image-depth from gp as gp-generic-image-depth,
            generic-image-width from gp as gp-generic-image-width,
            generic-image-height from gp as gp-generic-image-height;
  functions get-string-extent from gp,
            get-char-width from gp;
  functions generic-font-weight from gp,
	    generic-font-slant from gp,
	    generic-font-size from gp,
	    generic-font-size-setter from gp,
            query-fonts from gp,
            lookup-font from gp,
            gf from gp as parse-font-name,
            copy-generic-font from gp,
            get-font-width from gp,
            get-font-height from gp,
            get-font-ascent from gp,
            get-font-descent from gp,
            font-fixed-width-p from gp;
  classes pixmap-port from gp as <pixmap-port>,
          duim-pixmap from gp as <duim-pixmap>;
  functions realize-pixmap-for-port from gp,
            port-pixmap-representation from gp,
            destroy-pixmap from gp as gp-destroy-pixmap,
            port-width from gp,
            port-height from gp,
            %pixblt from gp as gp-pixblt;
end;


// Color
define lisp-interface
  functions convert-color from color,
            unconvert-color from color,
            color-red from color,
            color-blue from color,
            color-green from color,
            make-rgb from color;
end;


// Pathname handling
define lisp-interface
  functions namestring from cl as cl-namestring;
end;

// Toolkit hacks
define lisp-interface
  functions contact-state from clue,
            contact-state-setter from clue,
            contact-display from clue,
            contact-parent from clue,
            destroy from clue;
end;


// Xlib hacks
define lisp-interface
  functions make-color from xlib as make-x-color,
            window-cursor-setter from xlib,
            create-glyph-cursor from xlib,
            pointer-position from xlib as x-pointer-position,
            global-pointer-position from xlib,
            warp-pointer from xlib,
            screen-root from xlib,
            display-default-screen from xlib;
end;


// CAPI classes
define lisp-interface
  classes interface from capi as <capi-interface>,
          layout from capi as <capi-layout>,
          element from capi as <capi-element>,
          simple-pane from capi as <capi-simple-pane>,
          pinboard-layout from capi as <capi-pinboard-layout>,
          pane-with-layout from capi as <capi-pane-with-layout>;
  classes push-button from capi as <capi-push-button>,
          radio-button from capi as <capi-radio-button>,
          check-button from capi as <capi-check-button>,
          list-panel from capi as <capi-list-panel>,
          option-pane from capi as <capi-option-pane>,
          scroll-bar from capi as <capi-scroll-bar>,
          slider from capi as <capi-slider>,
          title-pane from capi as <capi-title-pane>,
          password-pane from capi as <capi-password-pane>,
          text-input-pane from capi as <capi-text-input-pane>,
          editor-pane from capi as <capi-editor-pane>,
          output-pane from capi as <capi-output-pane>;
  classes collection from capi as <capi-collection>,
          choice from capi as <capi-choice>;
  classes menu from capi as <capi-menu>,
          menu-component from capi as <capi-menu-component>,
          menu-item from capi as <capi-menu-item>;
  classes library-element from capi as <library-element>;
end;

// CAPI functions
define lisp-interface
  functions null from lisp as null-representation?;
  functions representation from capi,
            representation-setter from capi-internals;
  functions convert-to-screen from capi,
            screen-width from capi,
            screen-height from capi,
            screen-width-in-millimeters from capi,
            screen-height-in-millimeters from capi;
  functions create from capi,
            create-dialog from capi,
            exit-dialog from capi as capi-exit-dialog,
            abort-dialog from capi as capi-abort-dialog;
  functions element-parent from capi,
            set-element-parent from capi,
            remove-element from capi,
            change-geometry from capi,
            invalidate-pane-constraints from capi,
            get-constraints from capi;
  functions maybe-decoration-pane from capi;
  functions button-selected from capi,
            button-selected-setter from capi,
            item-selected from capi,
            menu-items from capi,
            menu-items-setter from capi,
            item-data from capi,
            collection-items from capi,
            collection-items-setter from capi;
  functions display-popup-menu from capi;
  functions text-input-pane-text from capi,
            text-input-pane-text-setter from capi,
            title-pane-text from capi,
            title-pane-text-setter from capi,
            editor-pane-text from capi,
            editor-pane-text-setter from capi,
            item-text from capi,
            item-text-setter from capi,
            choice-selection from capi,
            choice-selection-setter from capi;
  functions display-message from capi,
            confirm-yes-or-no from capi,
            prompt-with-list from capi,
            prompt-for-file from capi,
            prompt-for-font from capi,
            prompt-for-color from capi;
  functions scroll-bar-line-size from capi,
            scroll-bar-line-size-setter from capi,
            scroll-bar-page-size from capi,
            scroll-bar-page-size-setter from capi,
            range-start from capi,
            range-start-setter from capi,
            range-end from capi,
            range-end-setter from capi,
            range-slug-start from capi,
            range-slug-start-setter from capi,
            range-slug-end from capi,
            range-slug-end-setter from capi;
  functions beep-pane from capi;
  functions interface-title from capi,
            interface-title-setter from capi,
            interface-menu-bar-items from capi,
            interface-menu-bar-items-setter from capi,
            raise-interface from capi,
            lower-interface from capi,
            top-level-interface-best-geometry from capi;
  functions simple-pane-enabled from capi,
            simple-pane-enabled-setter from capi,
            menu-object-enabled from capi,
            menu-object-enabled-setter from capi;
  functions run-capi-post-actions from capi,
            run-capi-top-level-function from capi,
            apply-in-pane-process from capi;
end;


// CAPI Library
define lisp-interface
  functions display-representation from capi-library,
            create-representation from capi-library,
            destroy-representation from capi-library,
            show-representation from capi-library,
            withdraw-representation from capi-library,
            restore-representation from capi-library,
            representation-visible-p from capi-library as representation-visible?,
            representation-default-foreground from capi-library,
            representation-default-background from capi-library;
  functions display-dialog-representation from capi-library,
            create-dialog-representation from capi-library,
            exit-dialog-representation from capi-library,
            abort-dialog-representation from capi-library;
  functions height-menu-bar from capi-library,
            width-menu-bar from capi-library;
  functions representation-popup-menu from capi-library;
  functions representation-geometry from capi-library,
            update-representation-bounds from capi-library;
  functions representation-force-output from capi-library,
            representation-finish-output from capi-library;
  functions update-representation-foreground from capi-library,
            update-representation-background from capi-library,
            update-representation-font from capi-library,
            update-representation-title from capi-library;
end;


// CAPI Toolkit Library
define lisp-interface
  classes event-handling-pane from capi-tk-lib as <capi-event-handling-pane>;
  functions distribute-button-event from capi-tk-lib,
            distribute-motion-event from capi-tk-lib,
            distribute-entry-exit-event from capi-tk-lib,
            distribute-key-event from capi-tk-lib,
            distribute-exposure-event from capi-tk-lib,
            distribute-abled-event from capi-tk-lib,
            distribute-focus-event from capi-tk-lib;
  functions tk-resize-top-level from capi-tk-lib;
  functions capi-shell from capi-tk-lib,
            ensure-dialog-created from capi-tk-lib,
            ensure-pane-created from capi-tk-lib;
end;

// Clipboard stuff
define lisp-interface
  functions clipboard from capi as capi-get-clipboard,
            set-clipboard from capi as capi-set-clipboard;
end;
