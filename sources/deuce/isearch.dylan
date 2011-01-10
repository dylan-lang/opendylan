Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Emacs-style incremental searching

// As you type characters, they are processed as follows:
//  - Ordinary characters are added to the search string, and then Deuce
//    looks for next occurrence of the search string in the current direction.
//    If you type this command immediately after the first control-s or control-r,
//    the character replaces the old search string rather than augmenting it.
//  - Control-s means to reuse the previous search string and search forward for
//    next occurrence.  If done after a failed search, wrap the search and try again.
//  - Control-r means to reuse the previous search string and search backward for
//    next occurrence.  If done after a failed search, wrap the search and try again.
//  - Backspace means to remove the last character from the search string and
//    restore the caret position to the previous successful position.
//  - Control-w means to grab the remainder of the word (not atom!) at the current
//    caret position, and add it to the search string.  Don't do another search yet.
//  - Control-y means means grab the top of the kill ring and add it to the search
//    ring.  Don't do another search yet.
//  - Control-q can be used to quote a bucky character to search for it.
//  - Control-g -- if done after a failed search, undo as many characters as 
//    necessary to get back to the last point and search string contents that
//    succeeded.  If done after a successful search, abort the search and return
//    the caret to its location before the search was started.
//  - Other bucky characters terminate the search and are then executed normally.

define command incremental-search-forward (frame)
    "Start an incremental search, searching forward."
  if (use-isearch?(editor-policy(frame-editor(frame))))
    start-incremental-search(frame, direction: #"forward")
  else
    find-next-string(frame)
  end
end command incremental-search-forward;

define command incremental-search-backward (frame)
    "Start an incremental search, searching backward."
  if (use-isearch?(editor-policy(frame-editor(frame))))
    start-incremental-search(frame, direction: #"backward")
  else
    find-previous-string(frame)
  end
end command incremental-search-backward;


define constant <incremental-search-state>
    = one-of(#"input", #"continue", #"fail", #"wrap", #"wrap-fail");

define sealed class <incremental-search-entry> (<object>)
  sealed constant slot %start-bp :: <basic-bp>,
    required-init-keyword: start-bp:;
  sealed constant slot %string   :: <byte-string>,
    required-init-keyword: string:;
  sealed slot %state :: <incremental-search-state> = #"input",
    init-keyword: state:;
end class <incremental-search-entry>;


define sealed method start-incremental-search
    (frame :: <editor-state-mixin>,
     #key direction :: <incremental-search-direction>) => ()
  frame-isearch-trail(frame)      := #();
  frame-isearch-direction(frame)  := direction;
  frame-isearch-move-mark?(frame) := ~mark();	// if a mark is set, don't ever move it
  display-isearch-message(frame)
end method start-incremental-search;

define sealed method finish-incremental-search
    (frame :: <editor-state-mixin>, #key keep-mark?) => ()
  let window :: <basic-window> = frame-window(frame);
  unless (keep-mark?)
    clear-mark!(window: window, redisplay?: #t)
  end;
  let trail  = frame-isearch-trail(frame);
  let entry  = if (empty?(trail)) #f else head(trail) end;
  let string = if (entry) entry.%string else "" end;
  frame-isearch-last-string(frame) := string;
  frame-isearch-trail(frame)       := #();
  frame-isearch-direction(frame)   := #f;
end method finish-incremental-search;


//---*** This won't interact properly with keyboard macro recording!
define sealed method continue-incremental-search
    (frame :: <editor-state-mixin>, #key keysym) => (finished? :: <boolean>)
  let window :: <basic-window> = frame-window(frame);
  let char  = frame-command-character(frame);
  let bits  = frame-command-modifiers(frame);
  let trail = frame-isearch-trail(frame);
  let entry = if (empty?(trail)) #f else head(trail) end;
  let (string, state)
    = if (entry) values(entry.%string, entry.%state) else values("", #f) end;
  let move-mark? = frame-isearch-move-mark?(frame);
  local method add-entry (string :: <byte-string>) => ()
	  let entry :: <incremental-search-entry>
	    = make(<incremental-search-entry>,
		   start-bp: copy-bp(window-point(window)),
		   string:   string,
		   state:    state | #"input");
	  trail := pair(entry, trail);
	  frame-isearch-trail(frame) := trail;
	end method,
	method add-to-search-string (new :: <byte-string>, #key prepend? = #f) => ()
	  let offset = size(string);
	  let string
	    = if (prepend?) concatenate-as(<byte-string>, new, string)
	      else concatenate-as(<byte-string>, string, new) end;
	  add-entry(string);
	  unless (state == #"fail" | state == #"wrap-fail")
	    incremental-search(frame, offset: offset)
	  end;
	  display-isearch-message(frame)
	end method;
  case
    char & logand(bits, logior($control-key, $meta-key, $super-key)) = 0 =>
      let new = make(<byte-string>, size: 1, fill: char);
      add-to-search-string(new);
      #f;
    keysym == #"backspace" =>
      when (entry)
	let reverse? = (frame-isearch-direction(frame) == #"backward");
	select (state)
	  #"input", #"fail", #"wrap-fail" =>
	    // If the last isearch command was some input, remove the last bit
	    // of input and back up
	    trail := tail(trail);
	    frame-isearch-trail(frame) := trail;
	    let bp = entry.%start-bp;
	    let entry  = if (empty?(trail)) #f else head(trail) end;
	    let string = entry  & entry.%string;
	    let length = string & size(string); 
	    let (pbp, mbp)
	      = case
		  ~length   => values(bp, bp);
		  reverse?  => values(move-over-characters(bp, -length), bp);
		  otherwise => values(bp, move-over-characters(bp, -length));
		end;
	    when (move-mark? & (~length | window-mark(window) ~= mbp))
	      clear-mark!(window: window, redisplay?: #t)
	    end;
	    move-point!(pbp, window: window);
	    when (move-mark?)
	      if (mbp = pbp)
		clear-mark!(window: window)
	      else
		move-mark!(mbp, window: window)
	      end
	    end;
	    queue-redisplay(window, $display-point, centering: 0);
	    redisplay-window(window);
	    display-isearch-message(frame);
	  #"continue", #"wrap" =>
	    let success?
	      = incremental-search(frame, limit: entry.%start-bp, reverse?: ~reverse?);
	    unless (success?)
	      entry.%state := #"input"
	    end;
	    display-isearch-message(frame);
	end
      end;
      #f;
    keysym == #"escape" =>
      finish-incremental-search(frame, keep-mark?: #t);
      #f;
    bits = $control-key =>
      select (char by \=)
	's' =>
	  when (~entry & frame-isearch-last-string(frame))
	    add-entry(frame-isearch-last-string(frame))
	  end;
	  if (empty?(trail))
	    display-isearch-error(frame)
	  else
	    let entry :: <incremental-search-entry> = head(trail);
	    if (frame-isearch-direction(frame) == #"forward")
	      when (state == #f | state == #"input")
		entry.%state := #"continue"
	      end
	    else
	      frame-isearch-direction(frame) := #"forward";
	      entry.%state := #"continue"
	    end;
	    incremental-search(frame);
	    display-isearch-message(frame)
	  end;
	  #f;
	'r' =>
	  when (~entry & frame-isearch-last-string(frame))
	    add-entry(frame-isearch-last-string(frame))
	  end;
	  if (empty?(trail))
	    display-isearch-error(frame)
	  else
	    let entry :: <incremental-search-entry> = head(trail);
	    if (frame-isearch-direction(frame) == #"backward")
	      when (state == #f | state == #"input")
		entry.%state := #"continue"
	      end
	    else
	      frame-isearch-direction(frame) := #"backward";
	      entry.%state := #"continue"
	    end;
	    incremental-search(frame);
	    display-isearch-message(frame)
	  end;
	  #f;
	'w' =>
	  let n   = if (frame-isearch-direction(frame) == #"backward") -1 else 1 end;
	  let bp1 = window-point(window);
	  let bp2 = (empty?(trail) & window-mark(window)) | move-over-words(bp1, n);
	  let new = as(<byte-string>, make-interval(bp1, bp2));
	  add-to-search-string(new, prepend?: n < 0);
	  #f;
	'y' =>
	  let elt = history-top(editor-kill-history(frame-editor(frame)));
	  let new = elt & as(<byte-string>, elt);
	  if (new)
	    add-to-search-string(new)
	  else
	    display-isearch-error(frame)
	  end;
	  #f;
	'q' =>
	  let char = read-character(window);
	  let new  = make(<byte-string>, size: 1, fill: char);
	  add-to-search-string(new);
	  #f;
	'g' =>
	  let entry = if (empty?(trail)) #f else last(trail) end;
	  when (entry)
	    move-point!(entry.%start-bp, window: window)
	  end;
	  // Redisplay by hand, because 'command-error' won't do it for us
	  clear-mark!(window: window);
	  queue-redisplay(window, $display-point, centering: 0);
	  redisplay-window(window);
	  finish-incremental-search(frame);
	  command-error("Search cancelled");
	  #f;
	otherwise =>
	  // Unknown control character, process it as a command
	  // Keep the mark if the user had one set before the search started,
	  // otherwise clear it
	  finish-incremental-search(frame, keep-mark?: ~move-mark?);
	  #t;
      end;
    otherwise =>
      // Unknown bucky character, process it as a command
      finish-incremental-search(frame, keep-mark?: ~move-mark?);
      #t;
  end
end method continue-incremental-search;

define sealed method incremental-search
    (frame :: <editor-state-mixin>,
     #key offset, limit,
	  reverse? = (frame-isearch-direction(frame) == #"backward"))
 => (success? :: <boolean>)
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let trail = frame-isearch-trail(frame);
  let entry :: <incremental-search-entry> = head(trail);
  let (string, state) = values(entry.%string, entry.%state);
  let move-mark? = frame-isearch-move-mark?(frame);
  local method mark-result (bp :: <basic-bp>) => ()
	  let length = size(string);
	  let (pbp, mbp)
	    = if (reverse?) values(bp, move-over-characters(bp, length))
	      else values(move-over-characters(bp, length), bp) end;
	  when (move-mark? & window-mark(window) ~= mbp)
	    clear-mark!(window: window, redisplay?: #t)
	  end;
	  move-point!(pbp, window: window);
	  when (move-mark?)
	    move-mark!(mbp, window: window)
	  end;
	  queue-redisplay(window, $display-point, centering: 0);
	  redisplay-window(window)
	end method;
  unless (state == #"wrap-fail")
    let bp
      = if (state == #"fail")
	  state := #"wrap";
	  if (reverse?) interval-end-bp(buffer)
	  else interval-start-bp(buffer) end
	else
	  let bp = window-point(window);
	  case
	    offset &  reverse? => move-over-characters(bp,  (offset + 1));
	    offset & ~reverse? => move-over-characters(bp, -(offset + 1));
	    otherwise          => bp;
	  end
	end;
    let bp
      = search(bp, string, test: char-equal?, reverse?: reverse?);
    let success?
      = if (limit)
	  if (reverse?)
	    bp & ~bp-less?(bp, limit)
	  else
	    bp &  bp-less?(bp, limit)
	  end
	else
	  bp & #t
	end;
    if (success?)
      mark-result(bp)
    else
      when (move-mark?)
	clear-mark!(window: window, redisplay?: #t)
      end;
      if (state == #"wrap") state := #"wrap-fail"
      else state := #"fail" end;
      let kbdmac = frame-keyboard-macro(frame);
      when (kbdmac & ~keyboard-macro-closed?(kbdmac))
	command-error("Search failed")
      end
    end;
    entry.%state := state;
    success?
  end
end method incremental-search;


define sealed method display-isearch-message
    (frame :: <editor-state-mixin>) => ()
  do-display-isearch-message(frame, display-message)
end method display-isearch-message;

define sealed method display-isearch-error
    (frame :: <editor-state-mixin>) => ()
  do-display-isearch-message(frame, display-error-message)
end method display-isearch-error;

define method do-display-isearch-message
    (frame :: <editor-state-mixin>, function :: <function>) => ()
  let window :: <basic-window> = frame-window(frame);
  let trail = frame-isearch-trail(frame);
  let entry = if (empty?(trail)) #f else head(trail) end;
  let (string, state)
    = if (entry) values(entry.%string, entry.%state) else values("", #f) end;
  let reverse? = (frame-isearch-direction(frame) == #"backward");
  function(window, "%s%s%s",
	   select (state)
	     #"fail"      => "Failing ";
	     #"wrap"      => "Wrapped ";
	     #"wrap-fail" => "Failing wrapped ";
	     otherwise    => "";
	     end,
	   if (reverse?) "I-search backward: "
	   else "I-search: " end, string)
end method do-display-isearch-message;
