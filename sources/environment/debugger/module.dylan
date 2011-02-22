Module:    Dylan-user
Author:    Bill Chiles, Jason Trenouth, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module environment-debugger
  use environment-imports,
    exclude: { <buffer>,
               string-pluralize };

  use duim;
  use duim-internals,
    import: { string-pluralize, string-a-or-an,
	      frame-owned-frames,
              gadget-box-buttons,
	      gadget-item-label,
	      port-input-focus,
              sheet-layed-out?-setter };

  use deuce,
    import: { interval-start-bp, interval-end-bp,
	      <buffer>,
	      *buffer*,
	      *editor-frame*,
	      <window>,
	      window-buffer,
	      window-point, window-mark,
	      move-point!,
	      clear-mark!,
              <bp>,
	      copy-bp,
	      bp-line, bp-index,
              move-bp!,
	      line-start, line-end,
	      buffer-start-node, buffer-end-node,
              node-next, node-previous,
              node-section,
              <section>,
	      section-end-line, section-end-line,
	      frame-window,
	      select-buffer,
              kill-buffer,
	      bp->char-index,
	      queue-redisplay,
	      redisplay-section,
	      redisplay-window,
	      $display-text,
	      $display-all };
  use deuce-commands,
    import: { cut-region,
	      delete-region,
	      paste,
	      undo-command => deuce/undo-command,
	      redo-command => deuce/redo-command };
  //--- It's a bit of a pain that we need all this
  use deuce-internals,
    import: { <basic-bp>,
	      <basic-editor-frame>,
	      <simple-editor-frame>,
	      \with-editor-state-bound,
	      execute-command-in-frame,
	      window-frame,
	      make-empty-buffer,
	      <non-file-buffer>,
	      buffer-read-only?,
	      <command-error>,
	      command-error-window,
	      command-error-format-string,
	      command-error-format-arguments,
	      display-message,
	      display-error-message,
	      <shell-input>,
              <basic-shell-buffer>,
	      <interval-stream>,
              section-output-line, section-output-line-setter };
  use duim-deuce,
    import: { $potential-breakpoint-image,
	      $current-location-image};
  use duim-deuce-internals,
    import: { <deuce-pane>,
	      <deuce-gadget>,
	      horizontal-scroll-bar, horizontal-scroll-bar-setter,
	      vertical-scroll-bar, vertical-scroll-bar-setter,
	      scroll-window-horizontally,
	      scroll-window-vertically };

  use environment-tools;
  use environment-deuce;
  use environment-framework;
  use environment-protocols;
  use environment-manager;

  // For stop reasons
  use dfmc-application;

  export $step-over-bitmap,
	 $step-into-bitmap,
	 $step-out-bitmap,
	 $top-of-stack-bitmap,
	 $bottom-of-stack-bitmap,
	 $up-stack-bitmap,
	 $down-stack-bitmap;
end module environment-debugger;
