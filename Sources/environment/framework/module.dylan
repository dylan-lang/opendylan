Module:	   Dylan-User
Synopsis:  Environment Framework
Author:	   Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module environment-framework
  use environment-imports;

  use duim;
  use duim-internals,
    import: { sheet-layed-out?, sheet-layed-out?-setter,
	      <collection-gadget-mixin>,
	      gadget-item-label,
	      gadget-item-value,
	      duim-debug-message,
	      \with-abort-restart,
	      <event-queue> };

  use environment-protocols;
  use environment-manager,
    exclude: { provide-results },
    rename: { %provide-results => provide-results };

  //---*** This should probably be exported by functional-extensions,
  //---*** or maybe memory-manager.
  use dylan-extensions,
    import: { <out-of-memory-condition> };

  export <label-type>;

  // Utilities
  export make-keyboard-gesture,
	 call-in-frame-synchronously,
	 apply-in-frame-synchronously;

  // Commands
  export command-available?,
         update-command-availability;

  // Frame refresh
  export <frame-refresh-mixin>,
	 refresh-frame,
	 auto-refresh-frame;

  // Sorting
  export keyed-sort, keyed-sort!,
	 frame-order-sequence;

  // Object names
  export frame-object-name,
	 find-named-object,
	 *print-depth*,
	 *print-length*;

  // History
  export <frame-history-mixin>,
	 frame-history,
	 frame-add-to-history,
	 frame-remove-from-history,
	 frame-select-previous-object,
	 frame-select-next-object,
	 frame-view-history,
	 note-frame-history-changed,
	 note-frame-last-object-closed,
	 make-history-tool-bar-buttons,
	 *history-command-table*,
	 $back-bitmap,
         $forward-bitmap;
  
  // Input focus
  export <frame-input-focus-mixin>,
	 <frame-focus-command>,
	 command-sheet,
         command-available-for-focus?,
	 execute-command-for-focus,
         note-frame-input-focus-changed;

  // Selection
  export <frame-selection-mixin>,
	 <frame-selection-command>,
	 <frame-select-all-command>,
	 <frame-deselect-all-command>,
	 frame-selection,
	 frame-selection-empty?,
	 frame-selected-text,
	 note-frame-selection-updated,
	 frame-sheet-with-selection,
	 frame-sheet-selection,
	 frame-sheet-selection-empty?,
	 frame-sheet-selected-text,
	 frame-primary-collection-gadget,
         *selection-command-table*;
  
  // Undo
  export <frame-undo-mixin>,
         <frame-undo-command>,
         <frame-redo-command>,
	 make-undo-tool-bar-buttons,
	 *undo-command-table*,
	 $undo-bitmap,
	 $redo-bitmap;

  // Clipboard
  export <frame-clipboard-mixin>,
         <frame-cut-command>,
         <frame-copy-command>,
         <frame-paste-command>,
         <frame-delete-command>,
	 cut-object, cut-object?,
	 copy-object, copy-object?,
	 paste-object, paste-object?,
	 delete-object, delete-object?,
	 make-clipboard-tool-bar-buttons,
	 *clipboard-command-table*,
	 $cut-bitmap,
	 $copy-bitmap,
	 $paste-bitmap;

  // Searching
  export <frame-search-mixin>,
	 frame-can-find?,
	 frame-can-replace?,
	 frame-edit-search-options,
	 current-search-options-frame,
	 frame-find-next,
	 frame-find-previous,
	 frame-find-in-next-target,
	 frame-find-in-previous-target,
	 frame-copy-selection-to-search,
	 frame-copy-selection-to-replace,
	 frame-find-selection-next,
	 frame-find-selection-previous,
	 frame-replace-selection,
	 frame-replace-and-find-next,
	 frame-replace-and-find-previous,
	 frame-replace-all,
	 note-frame-searching-updated,
	 <search-frame>,
	 frame-search-frame-class,
	 <search-description>,
	 search-description-domain,
	 search-description-domain-setter,
	 search-description-targets,
	 search-description-targets-setter,
	 search-description-search-string,
	 search-description-search-string-setter,
	 search-description-replace-string,
	 search-description-replace-string-setter,
	 search-description-batch?,
	 search-description-batch?-setter,
	 search-description-wrap?,
	 search-description-wrap?-setter,
	 search-description-boundaries?,
	 search-description-boundaries?-setter,
	 search-description-match-case?,
	 search-description-match-case?-setter,
	 search-description-match-word?,
	 search-description-match-word?-setter,
	 search-description-match-regexp?,
	 search-description-match-regexp?-setter,
	 current-search-description,
	 current-search-description-setter,
	 frame-search-description,
	 frame-search-description-setter,
	 previous-search-strings,
	 previous-search-strings-setter,
	 previous-replace-strings,
	 previous-replace-strings-setter;

  // Search domains
  export <search-domain>,
	 <frame-search-domain>,
	 do-search-domains,
	 register-search-domain,
	 unregister-search-domain,
	 search-domain-label,
	 search-domain-targets,
	 search-domain-target-label,
	 search-domain-target-kind-label,
	 search-domain-target-icon,
	 search-domain-target-can-find?,
	 search-domain-target-can-replace?,
	 search-domain-find,
	 search-domain-find-all,
	 search-domain-replace-selection,
	 search-domain-replace-all,
	 search-domain-reveal-search-object,
	 search-domain-search-object-label,
	 search-domain-search-object-icon;

  // Searching within a sheet/gadget
  export can-find-in-sheet?,
	 can-replace-in-sheet?,
	 find-in-sheet,
	 find-all-in-sheet,
	 replace-in-sheet,
	 replace-all-in-sheet,
	 sheet-reveal-search-object,
	 sheet-search-object-label,
	 sheet-search-object-icon;

  // Searching within a frame
  export <search-target-frame-mixin>,
	 current-search-target-frame,
	 current-search-target-frame-setter,
	 can-find-in-frame?,
	 can-replace-in-frame?,
	 find-in-frame,
	 find-all-in-frame,
	 replace-in-frame,
	 replace-all-in-frame,
	 frame-reveal-search-object,
	 frame-search-object-label,
	 frame-search-object-icon;

  // Searching command UI
  export make-search-tool-bar-buttons,
	 *searching-command-table*,
	 $find-doc,
	 $find-title,
	 $find-bitmap,
	 $find-next-doc,
	 $find-next-title,
	 $find-next-bitmap,
	 $find-previous-doc,
	 $find-previous-title,
	 $find-previous-bitmap,
	 $find-in-next-target-doc,
	 $find-in-previous-target-doc,
	 $copy-to-search-doc,
	 $copy-to-replace-doc,
	 $find-selection-next-doc,
	 $find-selection-previous-doc,
	 $replace-doc,
	 $replace-and-find-next-doc,
	 $replace-and-find-next-title,
	 $replace-and-find-next-bitmap,
	 $replace-and-find-previous-doc,
	 $replace-all-doc;
	 
  // Edit menu
  export *edit-command-table*;

  // Go menu
  export *go-command-table*;

  // File menu
  export frame-new-file,
	 frame-open-file,
	 frame-revert-file,
	 frame-close-file,
	 frame-save-file,
	 frame-save-file-as,
	 frame-save-all,
	 frame-exit-application,
	 *file-command-table*,
	 *file-input-command-table*,
	 *file-io-command-table*,
	 *file-open-command-table*,
	 *file-save-command-table*,
	 *exit-command-table*,
	 $new-bitmap,
	 $open-bitmap,
	 $save-bitmap,
	 $save-all-bitmap;

  // Views
  export *bar-options-command-table*,
	 *view-refresh-command-table*,
	 *view-options-command-table*,
	 *basic-view-command-table*,
	 frame-edit-options,
	 *view-command-table*;

  // Primary object
  export <frame-primary-object-mixin>,
	 frame-raw-primary-object, frame-raw-primary-object-setter,
	 frame-primary-object, frame-primary-object-setter,
	 frame-primary-object-class,
	 frame-primary-object-name,
	 frame-default-primary-object,
	 frame-coerce-raw-object, frame-coerce-object,
	 frame-coerce-primary-object,
	 //---*** How can we make this automatic?
	 make-frame-primary-object-selector,
	 note-primary-object-changed,
	 note-raw-primary-object-replaced,
	 frame-select-primary-object;

  // Frame reuse
  export <frame-reuse-mixin>,
	 <port-designator>,

	 <frame-reuse-message>,
	 <frame-found-message>,
	 message-frame,

	 find-environment-frame, // obsolete (use ensure-environment-frame)
	 ensure-environment-frame,

	 reuse-environment-frame,
	 reuse-frames?,
	 reuse-frames?-setter,
	 choose-environment-frame,
	 choose-current-frame,
	 choose-matching-frame,
	 find-matching-frames,
	 reuse-matching-frame?,
	 frame-reusable?,
	 frame-reusable?-setter,
	 choose-frame,
	 reinitialize-frame,

	 fork-environment-frame,
	 fork-environment-function,
	 make-environment-frame,
	 start-environment-frame,
	 make-environment-thread,
	 frame-thread-function,
	 frame-thread-name,
	 frame-class-title,
    
	 \with-current-environment-frame,
	 current-environment-frame,

	 \with-environment-frame,
	 do-environment-frame,
	 call-in-environment-frame,

	 $environment-thread-lock,
	 $final-thread-notification,
	 exit-environment,
	 wait-for-shutdown;

  // Printing
  export *print-command-table*,
	 $page-setup-bitmap,
	 $print-bitmap,
	 frame-page-setup,
	 frame-hardcopy-document,
	 frame-hardcopy-object;

  // Frame linking
  export <frame-linking-mixin>,
	 link-frames,
	 unlink-frames;

  // Help
  export <frame-help-mixin>,
	 frame-help-contents-and-index,
	 frame-help-on-keyword,
	 *help-command-table*,
	 $help-topics-title,
	 $help-bitmap;

  // Error Handling
  export
    $internal-error-bitmap,
    environment-handler;

  //---*** cpage: 1998.08.12 These are (hopefully) temporary hacks to
  //		  allow me to experiment with using Windows APIs better
  //		  without having to modify/build DUIM. Sideways methods
  //		  are defined on these in win32-environment
  export <relative-order>,
	 <frame-order>,
	 reorder-frame,
	 order-frames,
	 restore-frame;
end module environment-framework;
