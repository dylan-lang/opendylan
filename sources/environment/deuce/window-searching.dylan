Module:    environment-deuce
Synopsis:  Searching Editor Windows
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Searching editor windows; search the window's buffer

define method can-find-in-window?
    (window :: <basic-window>) => (can-replace? :: <boolean>)
  window-buffer(window) & #t
end method can-find-in-window?;

define method can-replace-in-window?
    (window :: <basic-window>) => (can-replace? :: <boolean>)
  let buffer = window-buffer(window);
  (buffer & ~buffer-read-only?(buffer)) & #t
end method can-replace-in-window?;

// Utility function copied from Deuce; perhaps we should just import it from there
define inline function order-bps
    (bp1 :: <basic-bp>, bp2 :: <basic-bp>) => (sbp :: <basic-bp>, ebp :: <basic-bp>)
  if (bp1 = bp2 | bp-less?(bp1, bp2))
    values(bp1, bp2)
  else
    values(bp2, bp1)
  end
end function order-bps;

define method find-in-window
    (window        :: <basic-window>,
     search-string :: <string>,
     #key from-selection?   :: <boolean>,
          backwards?        :: <boolean>,
          wrap?             :: <boolean>,
          match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: false-or(<pair>))
  ignore(match-regexp?, progress-callback);
  //---*** Do we need to bind both *buffer* _and_ *editor-frame*?
  with-editor-state-bound (buffer = window)
    let start-bp = when (from-selection?)
		     let point-bp :: <bp>           = window-point(window);
		     let mark-bp  :: false-or(<bp>) = window-mark(window);
		     if (mark-bp ~== #f)
		       let (start-bp, end-bp) = order-bps(mark-bp, point-bp);
		       if (backwards?) start-bp else end-bp end
		     else
		       point-bp
		     end
		   end;
    let object = find-in-buffer(buffer,
				search-string,
				start-bp:    start-bp,
				backwards?:  backwards?,
				wrap?:       wrap?,
				match-case?: match-case?,
				match-word?: match-word?);
    object & pair(buffer, object)
  end
end method find-in-window;

define method replace-in-window
    (window         :: <basic-window>,
     search-string  :: <string>,
     replace-string :: <string>,
     #rest keys,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: false-or(<pair>))
  local method do-replace
	    (window         :: <basic-window>,
	     search-string  :: <string>,
	     replace-string :: <string>,
	     #key match-case?       :: <boolean>,
	          match-word?       :: <boolean>,
	          match-regexp?     :: <boolean>,
	          progress-callback :: false-or(<function>))
	 => (object :: false-or(<pair>))
	  //---*** cpage: 1998.07.29 Add support for 'match-word?'? Don't replace
	  //              the selection if it doesn't fall on "whole word" boundaries,
	  //              even if the selected text is a match (?).
	  ignore(match-word?);
	  ignore(match-regexp?, progress-callback);
	  local method compare-strings
		    (char-test :: <function>, s1 :: <byte-string>, s2 :: <byte-string>)
		 => (equal? :: <boolean>)
		  // Automatically not equal if they're not the same size.
		  let equal? = (size(s1) = size(s2));
		  for (c1 in s1, c2 in s2, until: ~equal?)
		    equal? := char-test(c1, c2)
		  end;
		  equal?
		end method;
	  //---*** Do we need to bind both *buffer* _and_ *editor-frame*?
	  with-editor-state-bound (buffer = window)
	    let test = if (match-case?) \= else char-equal? end;
	    let bp   = point();
	    // Only replace if the text is writable
	    unless (buffer-read-only?(buffer) | line-read-only?(bp-line(bp)))
	      let ebp      = window-mark(window);
	      let interval = ebp & make-interval(bp, ebp);
	      // If the selected text matches, replace it
              when (interval & compare-strings(test, as(<string>, interval), search-string))
		queue-region-redisplay(window, bp, ebp, centering: 0);
		let object
		  = if (size(replace-string) > 0)
		      with-change-recording (buffer, <replace-change-record>,
					     interval: interval, moving?: #t)
			let dbp = delete!(interval);
			let nbp = insert!(interval-start-bp(interval), replace-string);
			pair(nbp, dbp)
		      end
		    else
		      with-change-recording (buffer, <kill-change-record>, interval: interval)
			let dbp = delete!(interval);
			pair(dbp, dbp)
		      end
		    end;
		// Update the selection
		let pbp = head(object);
		let mbp = tail(object);
		move-point!(pbp, window: window);
		if (pbp = mbp)
		  clear-mark!(window: window, redisplay?: #t)
		else
		  move-mark!(mbp, window: window)
		end;
		frame-last-command-type(window-frame(window)) := #"insert";
		queue-redisplay(window, $display-point, centering: 0);
		// Update the display
		redisplay-window(window);
		// Update Undo/Redo enabling
		let section = line-section(bp-line(bp));
		let history = buffer-undo-history(buffer, section: section);
		when (history)
		  let (n-undo, n-redo) = undo-history-state(history);
		  window-note-undo/redo(window, n-undo ~= 0, n-redo ~= 0)
		end;
		object & pair(buffer, object)
	      end
	    end
	  end
        end method;
  apply-in-frame-synchronously(sheet-frame(window),
			       do-replace, window, search-string, replace-string, keys)
end method replace-in-window;

// Reveal and select matched/replaced text
//---*** cpage: 1998.08.29 Note that there is only a method on <deuce-pane>,
//              since we need both a <window> and a <sheet>. However, the other
//              methods in this file are on <basic-window>. Should we just
//              make all of them operate on <deuce-pane>?
//---*** cpage: 1998.07.29 Eventually this should be made more robust in the
//              face of user changes to text and buffers so that revealing a
//              match from a batch search fails gracefully and returns #f.
define method window-reveal-search-object
    (window :: <deuce-pane>, object :: <pair>) => (revealed? :: <boolean>)
  local method reveal (window :: <basic-window>, object :: <pair>) => ()
	  let buffer   = object.head;
	  let bps      = object.tail;
	  let point-bp = bps.head;
	  let mark-bp  = bps.tail;
	  // Reveal the buffer
	  when (buffer ~= window-buffer(window))
	    select-buffer(window, buffer);
	    queue-redisplay(window, $display-all);
	  end;
	  // Reveal the selection
	  //---*** Do we need to bind both *buffer* _and_ *editor-frame*?
	  let frame = window-frame(window);
	  dynamic-bind (*editor-frame* = frame,
			*buffer*       = buffer)
	    // Do nothing if the text is already selected
	    unless (point-bp = window-point(window)
		      & mark-bp = window-mark(window))
	      clear-mark!(window: window, redisplay?: #t);
	      move-point!(point-bp, window: window);
	      move-mark!(mark-bp, window: window);
	      frame-last-command-type(frame) := #"motion";
	      queue-redisplay(window, $display-point, centering: 0);
	    end;
	    // Update the display
	    redisplay-window(window);
	  end;
        end method;
  call-in-frame-synchronously(sheet-frame(window), reveal, window, object);
  #t
end method window-reveal-search-object;
