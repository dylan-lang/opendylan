Module:    Dylan-User
Synopsis:  Environment tools
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module environment-tools
  use environment-imports;	// this gets common-dylan

  use dylan-extensions, import: { \last-handler-definer };

  use duim-internals,
    exclude: { position, string-pluralize,
	       get-property,
	       \put-property!, do-put-property!,
	       \remove-property!, do-remove-property!,
	       remove-keywords, \with-keywords-removed };

  // We need to use duim-deuce-internals to share its images
  use duim-deuce-internals,
    import: { $current-location-image,
	      $disabled-breakpoint-image,
	      $disabled-tracepoint-image,
	      $enabled-breakpoint-image,
	      $enabled-tracepoint-image,
              $potential-breakpoint-image,
	      $profile-point-image,
	      $step-breakpoint-image };

  use environment-protocols;
  use environment-manager;
  use editor-manager;
  use source-control-manager;
  use environment-reports,
    exclude: { info-name };
  use environment-framework;
  use environment-server;

  // Some useful constants
  export project-not-built-message;

  // Environment startup (shutdown is managed by the Env-Framework)
  export start-environment;

  // The useful classes
  export <environment-frame>,
         <environment-simple-frame>,
         <environment-dialog-frame>,
         <basic-environment-tool>,
         <environment-fixed-project-frame>,
         <environment-project-tool>,
         <project-browser>,
         <object-browser>;

  // General protocols for environment frames
  export environment-frame-class-name;

  // Printing functions
  export compilation-warning-count-message,
         frame-print-environment-object,
         frame-object-unique-name,
         frame-default-object-name,
         frame-qualify-names?, frame-qualify-names?-setter;

  // Percentages
  export <percentage>,
         percentage,
         percentage-label,
         percentage-value;

  // Some useful functions
  export clone-tool,
         clone-and-link-tool,
         make-clone,
         environment-action-unavailable,
         environment-choose-file,
         environment-error-message,
         environment-message,
         environment-question,
         environment-warning-message,
	 filters-for-file-types,
         frame-sort-items,
         frame-undefined-callback,
         not-yet-implemented;

  // Some useful stuff for building other tools
  export <environment-tool>,
         $default-environment-frame-width,
         $default-environment-frame-height,
         make-environment-status-bar,
         make-environment-tool-bar,
         generate-frame-title,
	 make-clone-tool-bar-buttons,
         make-environment-tool-bar-buttons,
         make-environment-popup-menu,
	 frame-popup-menu-cache,
         environment-activate-callback,
         update-environment-popup-menu,
         display-environment-popup-menu,
         do-display-environment-popup-menu,
         update-frame-commands-for-target,
	 update-frame-commands-for-browse-target,
	 frame-ensure-project-database,
         frame-note-application-starting,
         frame-note-application-starting-done,
         pane-sheet-with-selection;

  // Command tables
  export *environment-help-command-table*,
         *environment-specific-help-command-table*,
	 *recent-files-command-table*,
	 *recent-projects-command-table*,
         *browse-locations-command-table*,
         *window-show-command-table*,
         *windows-command-table*,
         *project-command-table*,
         *project-edit-command-table*,
         *project-settings-command-table*,
         *build-command-table*,
         *basic-run-command-table*,
	 *all-breakpoints-command-table*,
         *run-command-table*;

  // Exporting
  export *export-command-table*,
         frame-export-data;

  // Images
  export environment-object-icon,
         environment-object-small-icon,
         environment-object-large-icon,
         $edit-source-bitmap,
	 $build-bitmap,
	 $compile-bitmap,
	 $load-bitmap,
	 $run-bitmap,
	 $pause-bitmap,
	 $stop-bitmap,
         $debug-bitmap,
         $interact-bitmap,
	 $new-text-file-title,
	 $open-file-title,
	 $save-file-title;

  // Project handling
  export most-recent-file, most-recent-file-setter,
         most-recent-files, most-recent-files-setter,
         most-recent-project, most-recent-project-setter,
         most-recent-projects, most-recent-projects-setter,
         register-opened-file,
         ensure-frame-project,
         frame-project,
         frame-current-project,
         frame-import-file,
	 frame-note-project-changed,
         frame-insert-source-file,
         frame-edit-project-settings;

  // Module handling
  export <frame-module-mixin>,
         frame-current-library,
         frame-current-module, frame-current-module-setter,
         frame-choose-current-module;

  // Open functions
  export frame-open-object,
         frame-open-selection;

  // Browsing functions
  export display-object-properties,
         frame-document-object, frame-document-object?,
	 frame-describe-object, frame-describe-object?,
         frame-browse-object,   frame-browse-object?,
         frame-browse-object-type,
         frame-browse-object-generic-function,
         frame-browse-project-library;

  // Command targets
  export <command-target>,
         <basic-command-target>,
         make-command-target,
         target-object,
         target-pane,
         frame-command-target, frame-command-target-setter,
	 frame-command-table-for-target,
	 frame-extra-command-table-for-target,
         frame-selection-target,
         frame-sheet-target,
         frame-target-pane,
         frame-target-object,
         note-frame-command-target-updated,
	 command-table-for-target;

  // Target browsing functions
  export *popup-menu-command-table*,
         *popup-menu-edit-command-table*,
         *popup-menu-documentation-command-table*,
         *popup-menu-clipboard-command-table*,
         *popup-menu-properties-command-table*,
         default-command-for-target,
         dylan-clipboard-object,
         dylan-clipboard-object-available?,
         frame-target-object,
         frame-target-browse-object,
         frame-target-edit-object,
         frame-target-as-string,
         frame-browse-target,
         frame-browse-target-type,
         frame-edit-object,
         frame-edit-objects,
         object-has-source?,
         frame-edit-target,
         frame-target-to-browse,
	 frame-edit-target-clients,
	 frame-edit-target-used-definitions,
	 frame-edit-target-subclasses,
	 frame-edit-target-superclasses,
	 frame-edit-target-class-methods,
	 frame-edit-target-generic-methods,
         frame-target-to-edit,
         frame-debug-target,
         frame-describe-target,
	 frame-document-target,
         display-target-properties;
         
  // Primary object functions
  export *primary-object-command-table*,
	 *primary-object-browse-command-table*,
	 *primary-object-documentation-command-table*,
	 *primary-object-edit-command-table*,
	 *primary-object-properties-command-table*,
	 frame-describe-primary-object,
	 frame-browse-primary-object,
	 frame-browse-primary-object-type,
	 frame-browse-primary-object-generic-function,
         frame-edit-primary-object,
	 frame-edit-primary-object-clients,
	 frame-edit-primary-object-used-definitions,
	 frame-edit-primary-object-subclasses,
	 frame-edit-primary-object-superclasses,
	 frame-edit-primary-object-class-methods,
	 frame-edit-primary-object-generic-methods,
	 frame-display-primary-object-properties,
	 frame-document-primary-object;

  // Compilation driving functions
  export frame-parse-project,
         frame-compile-project,
         frame-clean-compile-project,
         frame-link-project,
         frame-advanced-build-dialog,
         frame-build-project,
	 frame-clean-build-project,
	 frame-clean-project,
         frame-build-release,
	 frame-note-project-rebuilt,
	 frame-note-project-warnings-updated,
	 \with-compiler-locked, $compiler-lock;

  // Application driving functions
  export frame-start-application,
         frame-restart-application,
         frame-start-or-resume-application,
         frame-attach-application,
         frame-debug-application,
         frame-interact,
         frame-browse-threads,
         frame-pause-application,
         frame-resume-application,
         frame-continue-application,
         frame-stop-application,
	 frame-note-application-state-changed,
         frame-note-application-threads-changed;

  // Sort of halfway between the compiler and the application
  export frame-note-interaction-returned,
         frame-note-interactive-compilation-warnings;

  // Finding project browsers
  export find-project-browser-showing-project,
         ensure-project-browser-showing-project;

  // Environment property panes
  export <environment-property-pane>,
         environment-property-pane-object, environment-property-pane-object-setter,
         environment-property-pane-class, environment-property-pane-class-setter,
         refresh-environment-property-pane;

  // Displayers
  export <displayer-mixin>,
         <displayer-state>,
         <list-control-displayer>,
         <table-control-displayer>,
         <tree-control-displayer>,
         <filtering-list-control-displayer>,
         <filtering-table-control-displayer>,
         <filtering-tree-control-displayer>,
         \with-displayer-transaction,
         \with-displayer-transaction-method,
         compute-displayer-items,
         displayer-default-input-focus,
         displayer-enabled?, displayer-enabled?-setter,
         displayer-object, displayer-object-setter,
         displayer-items,
         displayer-display-items,
         displayer-sorted-items,
         displayer-item-count,
         displayer-collection-gadget,
         displayer-object-items-count,
         displayer-project,
         displayer-new-state?,
         displayer-valid?, displayer-valid?-setter,
         displayer-state, displayer-state-setter,
         displayer-state-object,
         displayer-ratios, displayer-ratios-setter,
         make-displayer-state,
         note-displayer-enabled-state-changed,
         note-displayer-state-updated,
         refresh-displayer,
         refresh-displayer-state,
         tree-control-displayer-children,
         update-displayer-state;

  // Memory displayer
  export <memory-displayer>;

  // Frame property pages
  export frame-property-types,
         frame-default-property-type,
         make-frame-property-page,
         make-frame-property-page-displayer,
         invalidate-frame-property-page,
         refresh-frame-property-page;

  // Editor stuff
  export find-editor,
	 editor-open-file,
         editor-new-file,
         editor-edit-definitions,
	 //---*** Disgusting modularity hack for now
	 make-code-viewer;

  // Source control stuff
  export claim-unit,
	 check-unit-out,
	 check-unit-in,
	 abandon-unit,
	 merge-unit,
	 diff-unit,
	 report-unit,
	 add-unit,
	 remove-unit;

  // Window settings
  export <frame-window-settings-mixin>,
         frame-default-position, set-frame-default-position,
         frame-default-size, set-frame-default-size,
         frame-default-state, frame-default-state-setter,
         save-window-settings,
         restore-window-settings;

  // Environment settings
  export environment-default-save-databases,
	 environment-default-save-databases,
	 environment-default-link-mode,
	 environment-default-upgrade-warnings,
	 environment-application-confirm-stop?,
	 environment-active-on-starting?,
	 environment-active-on-opening?,
	 environment-qualify-names?,
	 environment-auto-raise-all-frames,
	 environment-auto-lower-all-frames;

  // Useful frame mixins
  export <frame-cascading-window-mixin>,
         \cascading-window-settings-definer,
         \window-settings-definer,
         cascade-frame,
         frame-cascades?,
         frame-cascade-offset,
         frame-cascade-position, set-frame-cascade-position;
 
  // Breakpoint stuff
  export <breakpoint-location>,
         <breakpointable>,
         $breakpoint-slots,
         breakpoint-slot-abbreviation,
         breakpoint-slot-getter,
	 frame-note-all-breakpoints-changed,
	 frame-note-breakpoint-state-changed,
	 record-breakpoint-source-locations,
	 make-breakpoint-table-control-displayer,
	 frame-create-breakpoint,
	 frame-clear-breakpoint,
	 frame-edit-breakpoint-options,
	 frame-toggle-breakpoint-enabled?,
	 frame-create-or-toggle-breakpoint,
	 frame-run-to-cursor,
	 frame-run-to-target,
	 frame-new-breakpoint,
	 frame-browse-all-breakpoints,
	 frame-clear-all-breakpoints,
	 frame-enable-all-breakpoints,
	 frame-disable-all-breakpoints,
         frame-trace-target,
         frame-untrace-target,
         frame-untrace-all;

  export $application-bitmap,
	 $class-bitmap,
	 $clone-bitmap,
	 $constant-bitmap,
	 $default-bitmap,
	 $definition-bitmap,
	 $dylan-file-bitmap,
	 $canonical-source-bitmap,
	 $current-source-bitmap,
	 $error-bitmap,
	 $function-bitmap,
	 $foreign-bitmap,
	 $generic-bitmap,
	 $home-bitmap,
	 $library-bitmap,
	 $macro-bitmap,
	 $method-bitmap,
	 $module-bitmap,
	 $object-bitmap,
	 $profile-bitmap,
	 $project-bitmap,
	 $restart-bitmap,
	 $serious-warning-bitmap,
	 $slot-bitmap,
	 $stack-frame-bitmap,
	 $text-file-bitmap,
	 $threads-bitmap,
	 $unbound-bitmap,
	 $variable-bitmap,
	 $warning-bitmap,
	 $clients-folder-bitmap,
	 $uses-folder-bitmap;

  export $new-text-file-bitmap,
         $new-project-file-bitmap,
         $playground-bitmap,
         $tutorial-bitmap,
         $examples-bitmap;

  //---*** hughg, 1998/11/02: This one really belongs in DUIM, but andrewa
  // agrees this'll do for now (for the playground dialog).
  export $check-bitmap;

  export $splash-screen-bitmap,
         $about-box-bitmap;

  export $splash-screen-bitmap,
         $product-large-bitmap,
         $tutorial-large-bitmap,
         $examples-large-bitmap,
         $playground-large-bitmap,
         $project-file-large-bitmap,
         $dylan-file-large-bitmap,
         $text-file-large-bitmap,
         $open-large-bitmap;

  export $main-window-small-icon,
	 $project-window-small-icon,
	 $browser-window-small-icon,
	 $editor-window-small-icon,
	 $debugger-window-small-icon,
	 $describer-window-small-icon,
         $find-window-small-icon;

  export <frame-module-gadget-mixin>,
         frame-available-modules,
         make-module-tool-bar-buttons,
         update-module-gadget;

  export <object-wrapper>,
         <object-and-location-wrapper>,
         <project-object-wrapper>,
         <source-wrapper>,
         <source-record-wrapper>,
         <source-locator-wrapper>,
         <source-project-wrapper>,
         wrapper-object,
         wrapper-source-location,
         wrapper-project;

  // Debugging
  export find-debugger,
         find-debugger-from-environment,
         frame-create-thread;

  // Profiling
  export frame-find-profiler,
         find-profiler-from-environment;

  // Command-line
  export find-command-line-window;

  // Command decorators
  export make-command-decorator,
         $describe-target-command,
         $browse-target-command,
         $browse-target-type-command;
end module environment-tools;
