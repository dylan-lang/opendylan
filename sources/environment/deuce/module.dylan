Module:    Dylan-User
Synopsis:  Environment Deuce
Author:    Scott McKay, Hugh Greene, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module environment-deuce
  use environment-imports,
    exclude: { <buffer>,
               position, position-if };

  use duim,
    exclude: { \command-definer,
               <help-command> };
  use duim-internals,
    import: { string-pluralize, string-a-or-an,
	      <collection-gadget-mixin>,
              gadget-label-size,
              read-event,
	      event-status-code,
	      update-scroll-bar,
	      do-command-menu-gadgets,
	      do-destroy-sheet };

  use environment-protocols;
  use environment-framework;
  use environment-manager;
  use environment-tools;
  use environment-command-calling;

  use commands;
  use command-lines,
    exclude: { command-error, <command-error> };
  use environment-commands,
    exclude: { command-title };

  use editor-manager,
    exclude: { <editor> };		// clashes with Deuce
  use editor-manager-internals,
    import: { *current-editor* };
  use source-control-manager;

  use dfmc-shell, 
    rename: { <command>               => shell/<command>,
	      <basic-command>         => shell/<basic-command>,
	      execute-command         => shell/execute-command,
	      command-function        => shell/command-function,
	      command-function-setter => shell/command-function-setter,
	      command-description     => shell/command-description,
	      command-documentation   => shell/command-documentation,
	      command-hidden?         => shell/command-hidden?,
	      command-arguments       => shell/command-arguments },
    exclude: { display-condition,
               display-help,
	       <help-command> };

  use deuce-internals, 
    rename: { execute-command	       => deuce/execute-command,
	      execute-command-in-frame => deuce/execute-command-in-frame,
	      command-enabled?         => deuce/command-enabled?,
	      command-enabled?-setter  => deuce/command-enabled?-setter,
	      font-metrics	    => deuce/font-metrics,
	      display-message	    => deuce/display-message,
	      display-error-message => deuce/display-error-message,
	      display-buffer-name   => deuce/display-buffer-name,
	      warning-dialog	    => deuce/warning-dialog,
	      information-dialog    => deuce/information-dialog,
	      choose-from-menu      => deuce/choose-from-menu,
	      choose-from-dialog    => deuce/choose-from-dialog,
	      show-position	  => deuce/show-position,
              describe-object	  => deuce/describe-object,
              show-documentation  => deuce/show-documentation,
              browse-object	  => deuce/browse-object,
              browse-class	  => deuce/browse-class,
              browse-function	  => deuce/browse-function,
              edit-definition	  => deuce/edit-definition,
	      undo-command	  => deuce/undo-command,
	      redo-command	  => deuce/redo-command,
              line-length         => deuce/line-length,

	     // Breakpoint handing
	     <breakpoint-state>      => deuce/<breakpoint-state>,
	     line-breakpoint?        => deuce/line-breakpoint?,
	     line-breakpoint?-setter => deuce/line-breakpoint?-setter,
	     line-breakpoint-icon    => deuce/line-breakpoint-icon,
	     $breakpoint-icon-size   => deuce/$breakpoint-icon-size,
	     <dylan-breakpoint>      => deuce/<dylan-breakpoint>,

              // Project handling
              parse-project         => deuce/parse-project,
              compile-project       => deuce/compile-project,
              clean-compile-project => deuce/clean-compile-project,
              link-project          => deuce/link-project,
              build-project         => deuce/build-project,
              clean-build-project   => deuce/clean-build-project,

	      // Breakpoints
	      <breakpoint-state>  => deuce/<breakpoint-state>,

	      // Key states, etc
	      $control-key	  => deuce/$control-key,
	      $meta-key		  => deuce/$meta-key,
	      $super-key	  => deuce/$super-key,
	      $shift-key	  => deuce/$shift-key,
	      $left-button	  => deuce/$left-button,
	      $middle-button	  => deuce/$middle-button,
	      $right-button	  => deuce/$right-button,

	      // Colors
	      <color>		  => deuce/<color>,
	      $black		  => deuce/$black,
	      $white		  => deuce/$white,
	      $red		  => deuce/$red,
	      $green		  => deuce/$green,
	      $blue		  => deuce/$blue,
	      $cyan		  => deuce/$cyan,
	      $magenta		  => deuce/$magenta,
	      $yellow		  => deuce/$yellow },
    exclude: { // Random DUIM clashes
	       node-children,
	       node-children-setter,
	       node-parent,
	       <command-table>,
	       <standard-command-table>,
	       command-table-name,
	       frame-command-table, frame-command-table-setter,
	       window-size,
	       update-scroll-bar,
	       draw-string,
	       string-size,
	       draw-line,
	       draw-rectangle,
	       draw-image,
	       clear-area,
	       copy-area,
	       scroll-position, set-scroll-position,
	       cursor-position, set-cursor-position,
	       caret-position,  set-caret-position,
	       caret-size,      set-caret-size,
	       \with-busy-cursor, do-with-busy-cursor,
	       show-caret, hide-caret,
	       color-red,
	       color-green,
	       color-blue,
               read-character };
  use duim-deuce-internals;

  export <environment-editor>,
	 $environment-editor,
	 find-relevant-editor-frame,
	 <environment-deuce-pane>,
	 <environment-deuce-gadget>,
	 <environment-editor-pane>;

  export <dylanworks-mode>,
	 <dylanworks-shell-mode>;

  export find-section-for-definition,
	 find-section-for-source-location;

  export <code-viewer>,
         code-viewer-project,
	 code-viewer-definition, code-viewer-definition-setter,
	 code-viewer-current-location, code-viewer-current-location-setter;

  export <dylanworks-shell-section>,
         shell-execute-code,
         shell-parse-input;

  export <dylan-interactor>,
	 make-dylan-interactor,
         make-interactor-buffer,
         environment-empty-buffer,
         interactor-stack-frame-context,
         interactor-remote-thread,
	 interactor-receive-values,
         interactor-receive-warnings,
         interactor-show-contents,
         interactor-last-values;

  export <result-contents>,
         <result-subset>;

  export buffer-project;

  export color-dispatch-optimizations,
         reset-optimization-colors,
         edit-compiler-warnings,
         edit-next-compiler-warning;
end module environment-deuce;
