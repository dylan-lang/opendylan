Module:    environment-deuce
Synopsis:  Searching Editor Buffers
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Searching within a buffer

// Search a buffer for a string, starting at start-bp. If start-bp is
// false, search from the start or end of the buffer, depending on
// the direction of the search. Returns a pair of bps representing the
// start/end of the found text, or false if no match was found.
define method find-in-buffer
    (buffer :: <buffer>,
     string :: <string>,
     #key start-bp          :: false-or(<bp>),
          backwards?        :: <boolean>,
          wrap?             :: <boolean>,
          match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: false-or(<pair>))
  let object = #f;
  dynamic-bind (*buffer* = buffer)
    let bp = start-bp
               | if (backwards?)
		   interval-end-bp(buffer)
		 else
		   interval-start-bp(buffer)
		 end;
    let syntax-table = match-word? & list-syntax-table(buffer-major-mode(buffer));
    let test         = if (match-case?) \= else char-equal? end;
    //---*** cpage: 1998.07.28 We need to find an appropriate way to cache these
    //              tables somewhere.
    let (skip-table, reoccurrence-table)
      = compute-boyer-tables(string,
			     //  skip-table: editor-skip-table(editor),
			     //  reoccurrence-table: editor-reoccurrence-table(editor),
			     test: test);
    //--- cpage: 1988.07.29 This code was filched from Deuce's find-next-or-previous-string
    //           and I'm not sure why it uses 'state' to terminate searching, or why it
    //           assigns 'wrap?' to 'state'. We may want to clean this up a bit.
    let state :: <boolean> = #t;
    while (state)
      bp := search(bp,
		   string,
		   test:               test,
		   reverse?:           backwards?,
		   syntax-table:       syntax-table,
		   skip-table:         skip-table,
		   reoccurrence-table: reoccurrence-table);
      if (bp)
	let length = size(string);
	// Return #( point . mark ) with point at the start of the match
	// for reverse searches.
	object := if (backwards?)
		    pair(bp, move-over-characters(bp, length))
		  else
		    pair(move-over-characters(bp, length), bp)
		  end;
	state := #f		// force the search to end
      else
	if (wrap?)
	  bp := if (backwards?)
		  interval-end-bp(buffer);
		else
		  interval-start-bp(buffer);
		end;
	  state := wrap?;
	  wrap? := #f;	       // give up next time around
	else
	  state := #f;         // done searching
	end
      end
    end
  end;
  object
end method find-in-buffer;


/// All Buffers search domain; searches all open editor files

define class <all-buffers-search-domain> (<search-domain>)
end class <all-buffers-search-domain>;

define constant $all-buffers-search-domain = make(<all-buffers-search-domain>);
register-search-domain($all-buffers-search-domain);

// Search domain UI labels
define constant $all-buffers-search-domain-label = "All Open Text Documents";
define constant $all-buffers-search-domain-target-kind-label = "text document";

define method search-domain-label
    (domain :: <all-buffers-search-domain>) => (label :: <string>)
  $all-buffers-search-domain-label
end method search-domain-label;

define method search-domain-targets
    (domain :: <all-buffers-search-domain>) => (buffers :: <sequence>)
  // Return all non-anonymous buffers
  choose(complement(buffer-anonymous?), editor-buffers($environment-editor))
end method search-domain-targets;

define method search-domain-target-label
    (domain :: <all-buffers-search-domain>, buffer :: <buffer>) => (label :: <string>)
  buffer-title(buffer, show-path?: #f)
end method search-domain-target-label;

define method search-domain-target-kind-label
    (domain :: <all-buffers-search-domain>, buffer :: <buffer>) => (label :: <string>)
  $all-buffers-search-domain-target-kind-label
end method search-domain-target-kind-label;

//--- cpage: 1998.07.27 This is not yet called, since we have no UI that displays this.
/*
define method search-domain-target-icon
    (domain :: <all-buffers-search-domain>, buffer :: <buffer>) => (icon :: false-or(<image>))
  // TBD: Return the same icon used in the window title bar
  #f
end method search-domain-target-icon;
*/

define method search-domain-target-can-find?
    (domain :: <all-buffers-search-domain>, buffer :: <buffer>) => (can-find? :: <boolean>)
  #t
end method search-domain-target-can-find?;

define method search-domain-target-can-replace?
    (domain :: <all-buffers-search-domain>, buffer :: <buffer>) => (can-replace? :: <boolean>)
  ~buffer-read-only?(buffer)
end method search-domain-target-can-replace?;

define method search-domain-find
    (domain        :: <all-buffers-search-domain>,
     buffer        :: <buffer>,
     search-string :: <string>,
     #rest keys,
     #key from-selection?   :: <boolean>,
          backwards?        :: <boolean>,
          wrap?             :: <boolean>,
          match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: <object>)
  ignore(domain);
  // If searching from selection, get a frame for the buffer, then defer
  // searching to find-in-frame; else search in the buffer and only get
  // a frame if there's a match. This way, we don't affect a frame's
  // current buffer setting if there's no match.
  if (from-selection?)
    let frame  = find-frame-for-buffer(buffer);
    let window = frame-window(frame);
    // Make sure the buffer is the current frame buffer
    when (buffer ~= frame-buffer(frame))
      select-buffer(window, buffer);
      queue-redisplay(window, $display-all);
    end;
    let object = apply(find-in-frame, frame, search-string, keys);
    // Update the display
    call-in-frame(frame, redisplay-window, window);
    // Pair with the frame, so that we guarantee to reveal in the
    // same frame, since this frame's selection was used.
    object & pair(frame, object)
  else
    let object = find-in-buffer(buffer,
				search-string,
				backwards?:  backwards?,
				wrap?:       wrap?,
				match-case?: match-case?,
				match-word?: match-word?);
    // Pair with the buffer; we'll find a frame when we need to reveal
    object & pair(#f, pair(buffer, object))
  end
end method search-domain-find;

//---*** cpage: 1997.07.27 This is not yet used. This will be "batch" searching.
/*
define method search-domain-find-all
    (domain          :: <all-buffers-search-domain>,
     buffer          :: <buffer>,
     register-object :: <function>,
     search-string   :: <string>,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => ()
  // TBD: I think this should be handled by a method on <search-domain>, though
  //      it may be faster to do it in a custom method.
  next-method();
end method search-domain-find-all;
*/

define method search-domain-replace-selection
    (domain         :: <all-buffers-search-domain>,
     buffer         :: <buffer>,
     search-string  :: <string>,
     replace-string :: <string>,
     #rest keys,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: <object>)
  ignore(domain);
  let frame  = find-frame-for-buffer(buffer);
  let window = frame-window(frame);
  // Make sure the buffer is the current frame buffer
  when (buffer ~= frame-buffer(frame))
    select-buffer(window, buffer);
    queue-redisplay(window, $display-all);
  end;
  let replacement-object = apply(replace-in-frame,
				 frame, search-string, replace-string, keys);
  // Update the display
  call-in-frame(frame, redisplay-window, window);
  replacement-object & pair(frame, replacement-object)
end method search-domain-replace-selection;

define method search-domain-replace-all
    (domain         :: <all-buffers-search-domain>,
     buffer         :: <object>,
     search-string  :: <string>,
     replace-string :: <string>,
     #rest keys,
     #key from-selection?   :: <boolean>,
          backwards?        :: <boolean>,
          wrap?             :: <boolean>,
          match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (replace-count :: <integer>)
  ignore(domain);
  // Don't do anything unless we're replacing from the selection
  // or there is at least one match
  if (from-selection?
	| find-in-buffer(buffer,
			 search-string,
			 backwards?:        backwards?,
			 wrap?:             wrap?,
			 match-case?:       match-case?,
			 match-word?:       match-word?,
			 progress-callback: progress-callback))
    let frame  = find-frame-for-buffer(buffer);
    let window = frame-window(frame);
    // Make sure the buffer is the current frame buffer
    when (buffer ~= frame-buffer(frame))
      select-buffer(window, buffer);
      queue-redisplay(window, $display-all);
    end;
    let replace-count :: <integer> =
      apply(replace-all-in-frame, frame, search-string, replace-string, keys);
    // Update the display
    call-in-frame(frame, redisplay-window, window);
    replace-count
  else
    0
  end
end method search-domain-replace-all;

//---*** cpage: 1998.07.29 This is just about exactly what <frame-search-domain>
//              does. Perhaps we should forward some of this to $frame-search-domain.
define method search-domain-reveal-search-object
    (domain :: <all-buffers-search-domain>, object :: <object>) => (revealed? :: <boolean>)
  ignore(domain);
  // If we searched from a particular frame's selection, use that frame;
  // else search for a frame in which to display the buffer.
  let buffer-and-bps = object.tail;
  let buffer         = buffer-and-bps.head;
  let frame          = object.head | find-frame-for-buffer(buffer);
  when (frame & frame.frame-state ~= #"destroyed")
    call-in-frame(frame, method () => ()
			   deiconify-frame(frame);
			   raise-frame(frame);
			 end);
    frame-reveal-search-object(frame, buffer-and-bps)
  end
end method search-domain-reveal-search-object;

//---*** cpage: 1998.07.27 These two functions are not yet called, anyway, as
//              they are for displaying found items for "batch" searching.
/*
define method search-domain-search-object-label
    (domain :: <all-buffers-search-domain>, object :: <object>) => (label :: false-or(<string>))
  // TBD: Return the short file name, line number or range, and perhaps an excerpt
  //      of the text. Actually, we should probably break this into two functions,
  //      one for a "location" and the other for a "description".
  #f
end method search-domain-search-object-label;

define method search-domain-search-object-icon
    (domain :: <all-buffers-search-domain>, object :: <object>) => (icon :: false-or(<image>))
  // TBD: Return the editor title bar icon, probably.
  #f
end method search-domain-search-object-icon;
*/
