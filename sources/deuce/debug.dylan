Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Debugging functions

define method print-display-lines
    (window :: <basic-window>) => ()
  with-editor-state-bound (buffer = window)
    local method trim-line (line :: <line>) => ()
	    if (text-line?(line))
	      let contents = line-contents(line);
	      let length   = line-length(line);
	      copy-sequence(contents, start: 0, end: min(20, length))
	    else
	      "(no contents)"
	    end
	  end method;
    let lines   = window-display-lines(window);
    let n-lines = window-n-display-lines(window);
    format-out("\nactive lines %d, total lines %d",
	       n-lines, size(lines));
    let initial-line = window-initial-line(window);
    let initial-n    = window-line-number(window);
    format-out("\ninitial line %d (computed %d), contents %=",
	       initial-n, bp->line-index(line-start(initial-line), skip-test: #f),
	       trim-line(initial-line));
    for (i :: <integer> from 0 below n-lines)
      let dline :: <display-line> = lines[i];
      let line   = display-line-line(dline);
      let line-n = bp->line-index(line-start(line), skip-test: #f);
      let dtick  = display-line-tick(dline);
      let mtick  = line-modification-tick(line);
      let x      = 0;
      let y      = display-line-y(dline);
      let width  = display-line-width(dline);
      let height = display-line-height(dline);
      format-out("\n  dline %d, line %d, dtick %d, mtick %d, (%d,%d), %dX%d, contents %=",
		 i, line-n, dtick, mtick, x, y, width, height, trim-line(line));
      let mstart = display-line-mark-start(dline);
      let mend   = display-line-mark-end(dline);
      let mx     = display-line-mark-x(dline);
      let mw     = display-line-mark-width(dline);
      //--- Print these if they're useful
    end;
    format-out("\n")
  end
end method print-display-lines;
