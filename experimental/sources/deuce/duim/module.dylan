Module:       Dylan-User
Synopsis:     DUIM back-end for Deuce
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module duim-deuce
  // Deuce frame classes are in Standalone-Deuce or Environment-Deuce
  create <deuce-pane>,
	 <deuce-gadget>,
	 make-deuce-status-bar,
	 deuce-frame-show-mode,
	 deuce-frame-show-modified?,
	 deuce-frame-show-read-only?,
	 make-duim-color,
	 make-duim-text-style,
	 do-save-buffers-dialog,
	 $buffer-box-width, $buffer-box-height,
	 $edit-definition-field-width,
	 $search-string-field-width;

  create initialize-standard-images,
	 $potential-breakpoint-pattern, $potential-breakpoint-image,
	 $enabled-breakpoint-pattern,   $enabled-breakpoint-image,
	 $disabled-breakpoint-pattern,  $disabled-breakpoint-image,
	 $step-breakpoint-pattern,      $step-breakpoint-image,
	 $test-breakpoint-pattern,      $test-breakpoint-image,
	 $enabled-tracepoint-pattern,   $enabled-tracepoint-image,
	 $disabled-tracepoint-pattern,  $disabled-tracepoint-image,
	 $profile-point-pattern,        $profile-point-image,
	 $current-location-pattern,     $current-location-image,
	 $prompt-pattern,               $prompt-image,
	 $values-pattern,               $values-image,
	 $warning-pattern,              $warning-image,
	 $serious-warning-pattern,      $serious-warning-image;
end module duim-deuce;

define module duim-deuce-internals
  use functional-dylan,
    exclude: { position, position-if };
  use simple-format;
  use threads;
  use plists;

  use byte-vector;
  use streams-internals,
    rename: { <buffer>       => streams/<buffer>,
	      read-character => streams/read-character };

  use duim;
  use duim-internals,
    import: { $default-text-style,
	      read-event,
	      event-status-code,
	      update-scroll-bar,
	      execute-value-changed-callback, 
	      note-gadget-text-changed,
	      note-gadget-value-changed,
	      gadget-value-type,
	      gadget-text-parser, gadget-value-printer,
	      initialize-scrolling,
	      sheet-horizontal-scroll-bar, sheet-horizontal-scroll-bar-setter,
	      sheet-vertical-scroll-bar, sheet-vertical-scroll-bar-setter,
	      sheet-scrolls-vertically?, sheet-scrolls-horizontally?,
	      standardize-text-style,
	      line-scroll-amount, page-scroll-amount,
	      note-focus-in, note-focus-out,
	      frame-alt-key-is-meta?, frame-alt-key-is-meta?-setter,
	      do-destroy-sheet };
  use duim-deuce,
    export: all;
  use deuce-internals,
    rename: { // Random DUIM clashes
	      line-length	   => deuce/line-length,
	      node-children        => deuce/node-children,
	      node-children-setter => deuce/node-children-setter,
	      node-parent	   => deuce/node-parent,
	      execute-command	   => deuce/execute-command,
	      \command-definer     => \deuce/command-definer,
	      <command-table>	   => deuce/<command-table>,
	      <standard-command-table> => deuce/<standard-command-table>,
	      command-table-name   => deuce/command-table-name,
	      undo-command	=> deuce/undo-command,
	      redo-command	=> deuce/redo-command,
	      $control-key	=> deuce/$control-key,
	      $meta-key		=> deuce/$meta-key,
	      $super-key	=> deuce/$super-key,
	      $shift-key	=> deuce/$shift-key,
	      $left-button	=> deuce/$left-button,
	      $middle-button	=> deuce/$middle-button,
	      $right-button	=> deuce/$right-button,
	      // Deuce window protocol
	      window-enabled?	=> deuce/window-enabled?,
	      window-occluded?	=> deuce/window-occluded?,
	      window-size	   => deuce/window-size,
	      window-viewport-size => deuce/window-viewport-size,
	      update-scroll-bar    => deuce/update-scroll-bar,
	      scroll-position	   => deuce/scroll-position,
	      set-scroll-position  => deuce/set-scroll-position,
	      display-message	    => deuce/display-message,
	      display-error-message => deuce/display-error-message,
	      display-buffer-name   => deuce/display-buffer-name,
	      draw-string	=> deuce/draw-string,
	      string-size	=> deuce/string-size,
	      draw-line		=> deuce/draw-line,
	      draw-rectangle	=> deuce/draw-rectangle,
	      draw-image	=> deuce/draw-image,
	      clear-area	=> deuce/clear-area,
	      copy-area		=> deuce/copy-area,
	      font-metrics	=> deuce/font-metrics,
	      choose-from-menu	  => deuce/choose-from-menu,
	      choose-from-dialog  => deuce/choose-from-dialog,
	      cursor-position	  => deuce/cursor-position,
	      set-cursor-position => deuce/set-cursor-position,
	      \with-busy-cursor	  => \deuce/with-busy-cursor,
	      do-with-busy-cursor => deuce/do-with-busy-cursor,
	      caret-position	  => deuce/caret-position,
	      set-caret-position  => deuce/set-caret-position,
	      caret-size	  => deuce/caret-size,
	      set-caret-size      => deuce/set-caret-size,
	      show-caret	=> deuce/show-caret,
	      hide-caret	=> deuce/hide-caret,
	      <color>		=> deuce/<color>,
	      color-red		=> deuce/color-red,
	      color-green	=> deuce/color-green,
	      color-blue	=> deuce/color-blue,
	      $black		=> deuce/$black,
	      $white		=> deuce/$white,
	      $red		=> deuce/$red,
	      $green		=> deuce/$green,
	      $blue		=> deuce/$blue,
	      $cyan		=> deuce/$cyan,
	      $magenta		=> deuce/$magenta,
	      $yellow		=> deuce/$yellow,
	      command-enabled?        => deuce/command-enabled?,
	      command-enabled?-setter => deuce/command-enabled?-setter };
end module duim-deuce-internals;
