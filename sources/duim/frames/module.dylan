Module:       Dylan-User
Synopsis:     DUIM frames
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module duim-frames
  //--- Try to make some DUIM-Sheets forward references less visible
  use duim-sheets,
    import: { <abstract-frame>,
	      <frame>,
	      current-frame,
	      *current-frame*,
	      destroy-frame,
	      frame?,
	      frame-input-focus, frame-input-focus-setter },
    export: all;

  // Re-export all the Commands functionality
  use commands,
    export: all;

  // Frame events
  create <application-exited-event>,
         <dialog-cancel-event>,
         <dialog-exit-event>,
	 <frame-created-event>,
         <frame-destroyed-event>,
         <frame-exit-event>,
         <frame-exited-event>,
         <frame-focus-event>,
         <frame-focus-in-event>,
         <frame-focus-out-event>,
         <frame-input-focus-changed-event>,
	 <frame-layed-out-event>,
         <frame-mapped-event>,
         <frame-unmapped-event>,
         event-destroy-frame?,
         event-new-focus,
         event-old-focus,
         event-status-code;

  // Frames
  create <simple-frame>,
	 apply-in-frame, queue-apply-in-frame,
	 call-in-frame, queue-call-in-frame,
	 deiconify-frame,
         display-pointer-documentation,
         exit-frame,
         find-frame,
         frame-accelerators, frame-accelerators-setter,
	 frame-can-exit?,
         frame-command-queue,
         frame-command-table, frame-command-table-setter,
         frame-controlling-frame,
	 frame-default-button, frame-default-button-setter,
	 frame-document, frame-document-setter,
         frame-enabled?, frame-enabled?-setter,
         frame-event-queue,
         frame-icon, frame-icon-setter,
         frame-iconified?, frame-iconified?-setter,
         frame-layout, frame-layout-setter,
         frame-mapped?, frame-mapped?-setter,
         frame-maximized?, frame-maximized?-setter,
         frame-menu-bar, frame-menu-bar-setter,
         frame-mode,
         frame-occluded?,
	 frame-owner,
         frame-palette, frame-palette-setter,
         frame-pointer-documentation, frame-pointer-documentation-setter,
         frame-position, set-frame-position,
         frame-properties,
	 frame-thread,
         frame-size, set-frame-size,
         frame-state,
         frame-status-bar, frame-status-bar-setter,
         frame-status-message, frame-status-message-setter,
         frame-title, frame-title-setter,
         frame-tool-bar, frame-tool-bar-setter,
         frame-top-level,
         generate-panes,
         handle-id-activation,
         iconify-frame,
         layout-frame,
         lower-frame,
         do-frame-commands,
         make-command-menu-bar,
         make-command-tool-bar,
         maximize-frame,
	 note-command-table-changed,
         raise-frame,
         start-frame,
         set-frame-position,
         unmaximize-frame,
         \frame-definer,
         \with-background-cursor, do-with-background-cursor;

  // Dialogs
  create <dialog-frame>,
         <property-frame>,
         <property-page>,
         <wizard-frame>,
         <wizard-page>,
         cancel-dialog,
         compute-next-page, compute-previous-page,
	 dialog-apply-button, dialog-apply-button-setter,
         dialog-apply-callback,
         dialog-back-button, dialog-back-button-setter,
         dialog-back-callback,
         dialog-cancel-button, dialog-cancel-button-setter,
         dialog-cancel-callback,
         dialog-current-page, dialog-current-page-setter,
         dialog-exit-button, dialog-exit-button-setter,
         dialog-exit-buttons-position,
         dialog-exit-callback,
         dialog-exit-enabled?, dialog-exit-enabled?-setter,
         dialog-help-button, dialog-help-button-setter,
         dialog-help-callback,
         dialog-image, dialog-image-setter,
         dialog-next-button, dialog-next-button-setter,
         dialog-next-callback,
         dialog-next-enabled?, dialog-next-enabled?-setter,
         dialog-next-page, dialog-next-page-setter,
         dialog-page-changed-callback, dialog-page-changed-callback-setter,
         dialog-page-complete?, dialog-page-complete?-setter,
         dialog-pages, dialog-pages-setter,
         dialog-previous-page, dialog-previous-page-setter,
         move-to-next-page,
         move-to-previous-page,
         note-dialog-page-changed,
	 exit-dialog,
         start-dialog;

  // Commands
  create command-enabled?, command-enabled?-setter;

  // Command decorators
  create <command-decorator>,
         decorator-accelerator,
         decorator-documentation,
         decorator-image,
         decorator-mnemonic,
         decorator-label,
         decorator-options,
         decorator-resource-id,
         decorator-type,
         decorator-object;

  // Help
  create <help-command>,
	 <help-on-contents>,
	 <help-on-context>,
	 <help-on-help>,
	 <help-on-index>,
	 <help-on-keyword>,
	 <help-on-topics>,
	 <help-on-version>,
	 <help-quit>,
	 <help-reposition>,
	 <help-run-macro>,
	 <help-source>,
	 \help-source-definer,
	 frame-help-context,
	 frame-help-keyword,
	 frame-help-source,
	 frame-help-source-locator,
	 frame-help-topic-id;

  // Command tables
  create <command-table>,
         *global-command-table*,
         *user-command-table*,
         add-command,
         add-command-line-name,
         add-command-table-menu-item,
         add-presentation-translator,
         // \command-definer,		//--- not yet
         command-table-accelerators,
         command-table-commands,
         command-table-inherit-from, command-table-inherit-from-setter,
	 command-table-menu,
         command-table-name,
         command-table?,
         \command-table-definer,
         do-command-line-names,
         do-command-table-accelerators,
         do-command-table-commands,
         do-command-table-menu-items,
         do-command-table-menu-commands,
         do-presentation-translators,
         make-menu-from-command-table-menu,
         make-menus-from-command-table,
         remove-command,
         remove-command-line-name,
         remove-command-table,
         remove-command-table-menu-item,
         remove-presentation-translator;

  // Standard commands and command tables
  create *standard-edit-command-table*,
	 *standard-file-command-table*,
	 *standard-help-command-table*,
	 *standard-view-command-table*,
	 *standard-windows-command-table*,
	 clipboard-clear,
	 clipboard-copy,
	 clipboard-cut,
	 clipboard-paste,
	 close-document,
	 command-undo, command-redo, 
	 new-document,
	 note-document-changed,
	 open-document,
	 save-all-documents,
	 save-document, save-document-as;

  // Completer
  create complete-from-generator,
	 complete-from-sequence;

  // Progress notes
  create <progress-note>,
         *progress-note*,
         clear-progress-note,
         display-progress-note,
         lower-progress-note,
         note-progress,
         note-progress-in-phases,
         \noting-progress, do-noting-progress,
         progress-note-label, progress-note-label-setter,
         raise-progress-note;

  // Convenience functions -- shouldn't be part of the run-time
  create contain,
         make-container;

  //--- Needed until macro hygiene works...
  create %command-table, %command-table-setter,
         %layout, %layout-setter,
         %menu-bar, %menu-bar-setter,
         %pages, %pages-setter,
         %status-bar, %status-bar-setter,
         %tool-bar, %tool-bar-setter,
         \frame-class-definer,
         \frame-layout-definer,
         \frame-panes-definer,
         \frame-gadget-bars-definer,
         // \command-method-definer,	//--- not yet
         // \command-parser-definer,	//--- not yet
         // install-command,		//--- not yet
         \command-table-menu-definer,
         \command-table-variable-definer;
end module duim-frames;

define module duim-frames-internals
  use dylan;
  use duim-imports;
  use duim-utilities;
  use duim-geometry-internals;
  use duim-DCs-internals;
  use duim-sheets-internals;
  use duim-graphics-internals;
  use duim-layouts-internals;
  use duim-gadgets-internals;
  use duim-frames, export: all;

  // Frames
  export <basic-frame>,
	 <frame-mode>,
	 <frame-state>,
	 attach-frame,
         detach-frame,
         do-display-pointer-documentation,
	 do-exit-frame,
	 do-frame-occluded?,
	 port-start-frame,
         frame-always-on-top?,
	 frame-centered?,
	 frame-fixed-width?,
         frame-fixed-height?,
         frame-flags,
	 frame-geometry,
         frame-keyboard-interrupt?,
         frame-minimize-box?, frame-maximize-box?,
	 frame-needs-event-queue?,
         frame-owned-frames,
         frame-owned-menus,
         frame-resizable?,
         frame-resource-id,
	 frame-save-under?,
	 frame-state-setter,
         frame-top-level-sheet-class,
         frame-top-level-sheet-size,
         frame-wrapper,
         install-frame-mnemonics,
         make-event-queue,
         note-accelerators-changed,
	 note-frame-enabled, note-frame-disabled,
	 note-frame-iconified, note-frame-deiconified,
	 note-frame-icon-changed,
         note-frame-mapped, note-frame-unmapped,
	 note-frame-maximized, note-frame-unmaximized,
	 note-frame-state-changed,
	 note-frame-title-changed,
	 pointer-documentation-sheet,
         update-frame-layout,
         update-frame-wrapper;

  // Windows hacks for Alt=Meta
  export frame-alt-key-is-meta?, frame-alt-key-is-meta?-setter,
	 frame-allow-control-alt?, frame-allow-control-alt?-setter;

  // Embedded frames
  export <basic-embedded-frame>,
	 <embedded-top-level-sheet>;

  // Dialogs
  export <multi-page-dialog-frame>,
         <property-page-pane>,
         <wizard-page-pane>,
         do-cancel-dialog,
         do-exit-dialog,
         update-dialog-buttons;

  // Commands
  export <command-event>,
         distribute-command-event,
	 note-command-disabled,
         note-command-enabled;

  // Help systems
  export <help-system>,
	 <help-system-error>,
	 <help-system-not-installed>,
	 <no-help-system>,
	 help-system-installed?,
	 help-system-name,
	 *default-help-system*,
	 frame-manager-help-system, frame-manager-help-system-setter;

  // Help
  export <help-on-subject>,
	 <help-from-source>,
	 <help-on-pane>,
	 display-help,
	 help-context,
	 help-keyword,
	 help-macro,
	 help-pane,
	 help-popup?,
	 help-secondary-window,
	 help-source,
	 help-source-context-map,
	 help-source-locator,
	 help-source-name,
	 help-topic-id,
	 help-window-region,
	 initialize-help,
	 initialize-help-pane,
	 \initialize-table;

  // Command tables
  export <standard-command-table>,
         command-accessible?,
         command-present?,
	 command-table-resource-id,
         do-command-menu-gadgets,
         do-command-table-inheritance;

  // Progress notes
  export progress-note-sheet, progress-note-sheet-setter;

  // Contain
  export *contain-uses-own-thread?*,
	 <container-frame>,
	 container-uses-own-thread?;

  // Debugging
  export print-sheet-layout;
end module duim-frames-internals;
