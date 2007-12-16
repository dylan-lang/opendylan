Module:       duim-deuce-internals
Synopsis:     DUIM back-end for Deuce
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Deuce panes as DUIM text gadgets

define sealed method gadget-read-only?
    (window :: <deuce-pane>) => (read-only? :: <boolean>)
  buffer-read-only?(window-buffer(window))
end method gadget-read-only?;

define sealed method gadget-read-only?-setter
    (read-only? :: <boolean>, window :: <deuce-pane>) => (read-only? :: <boolean>)
  buffer-read-only?(window-buffer(window)) := read-only?
end method gadget-read-only?-setter;


//---*** How can we implement this?
define sealed method gadget-enabled?
    (window :: <deuce-pane>) => (enabled? :: <boolean>)
  #t
end method gadget-enabled?;

//---*** How can we implement this?
define sealed method gadget-enabled?-setter
    (enabled? :: <boolean>, window :: <deuce-pane>) => (enabled? :: <boolean>)
  enabled?
end method gadget-enabled?-setter;


define sealed method gadget-value
    (window :: <deuce-pane>) => (value)
  gadget-text-parser(gadget-value-type(window), gadget-text(window))
end method gadget-value;

// NB: this turns the buffer into single-section buffer with no source container
define sealed method gadget-value-setter
    (value, window :: <deuce-pane>, #key do-callback?)
 => (value)
  let text = gadget-value-printer(gadget-value-type(window), value);
  unless (text = gadget-text(window))
    gadget-text(window, do-callback?: do-callback?) := text
  end
end method gadget-value-setter;

define method note-gadget-value-changed
    (window :: <deuce-pane>) => ()
  #f
end method note-gadget-value-changed;


define sealed method gadget-text
    (window :: <deuce-pane>) => (value :: <byte-string>)
  with-editor-state-bound (buffer = window)
    as(<byte-string>, buffer)
  end
end method gadget-text;

define sealed method gadget-text-setter
    (value :: <byte-string>, window :: <deuce-pane>, #key do-callback?)
 => (value :: <byte-string>)
  with-editor-state-bound (buffer = window)
    // "Read" the new contents into the buffer
    let section = make(<section>,
		       container: #f,
		       start-line: #f, end-line: #f);
    let stream = make(<string-stream>, contents: value);
    read-buffer-contents-from-stream(buffer, section, stream);
    close(stream);
    // Now make sure it displays correctly
    initialize-redisplay-for-buffer(window, buffer);
    queue-redisplay(window, $display-all);
    redisplay-window(window);
    when (do-callback?)
      execute-value-changed-callback(window, gadget-client(window), gadget-id(window))
    end;
    note-gadget-text-changed(window);
    note-gadget-value-changed(window);
    value
  end
end method gadget-text-setter;

define method note-gadget-text-changed
    (window :: <deuce-pane>) => ()
  #f
end method note-gadget-text-changed;


define sealed method text-selection
    (window :: <deuce-pane>)
 => (range :: type-union(<text-range>, one-of(#f)))
  with-editor-state-bound (window)
    let bp1 = window-point(window);
    let bp2 = window-mark(window);
    when (bp2)
      let i1 = bp->char-index(bp1);
      let i2 = bp->char-index(bp2);
      when (i1 > i2) swap!(i1, i2) end;
      make(<text-range>, start: i1, end: i2)
    end
  end
end method text-selection;

define sealed method text-selection-setter
    (range :: type-union(<text-range>, one-of(#t, #f)), window :: <deuce-pane>)
 => (range :: type-union(<text-range>, one-of(#t, #f)))
  with-editor-state-bound (buffer = window)
    select (range)
      #t =>
	let bp1 = interval-end-bp(buffer);
	let bp2 = interval-start-bp(buffer);
	move-point!(bp1, window: window);
	move-mark!(bp2, window: window);
	queue-redisplay(window, $display-point, centering: -1);
      #f =>
	clear-mark!(window: window);
	queue-redisplay(window, $display-region);
      otherwise =>
	let bp1 = char-index->bp(buffer, text-range-end(range));
	let bp2 = char-index->bp(buffer, text-range-start(range));
	move-point!(bp1, window: window);
	move-mark!(bp2, window: window);
	queue-redisplay(window, $display-point, centering: 0);
    end;
    redisplay-window(window);
    range
  end
end method text-selection-setter;


define sealed method selected-text
    (window :: <deuce-pane>) => (string :: false-or(<byte-string>))
  with-editor-state-bound (window)
    let bp1 = window-point(window);
    let bp2 = window-mark(window);
    when (bp2)
      let interval = make-interval(bp1, bp2);
      as(<byte-string>, interval)
    end
  end
end method selected-text;

define sealed method selected-text-setter
    (string :: false-or(<byte-string>), window :: <deuce-pane>)
 => (string :: false-or(<byte-string>))
  with-editor-state-bound (window)
    let bp1 = window-point(window);
    let bp2 = window-mark(window);
    when (bp2)
      let interval = make-interval(bp1, bp2);
      let bp = delete!(interval);
      when (string)
	insert-moving!(bp, string)
      end;
      clear-mark!(window: window);
      move-point!(bp, window: window);
      queue-redisplay(window, $display-text, centering: 0);
      redisplay-window(window)
    end;
    string
  end
end method selected-text-setter;


define sealed method text-field-modified?
    (window :: <deuce-pane>) => (modified? :: <boolean>)
  with-editor-state-bound (buffer = window)
    buffer-modified?(buffer)
  end
end method text-field-modified?;

define sealed method text-field-modified?-setter
    (modified? :: <boolean>, window :: <deuce-pane>) => (modified? :: <boolean>)
  with-editor-state-bound (buffer = window)
    buffer-modified?(buffer) := modified?
  end
end method text-field-modified?-setter;


define sealed method text-field-size
    (window :: <deuce-pane>) => (size :: <integer>)
  with-editor-state-bound (buffer = window)
    count-characters(buffer)
  end
end method text-field-size;

define sealed method text-field-text
    (window :: <deuce-pane>, range :: <text-range>)
 => (string :: false-or(<byte-string>))
  with-editor-state-bound (window)
    //---*** Do this
  end
end method text-field-text;


define sealed method text-caret-position
    (window :: <deuce-pane>) => (index :: false-or(<integer>))
  with-editor-state-bound (window)
    let bp = window-point(window);
    bp->char-index(bp)
  end
end method text-caret-position;

define sealed method text-caret-position-setter
    (index :: false-or(<integer>), window :: <deuce-pane>)
 => (index :: false-or(<integer>))
  with-editor-state-bound (buffer = window)
    let bp = char-index->bp(buffer, index);
    when (bp)
      move-point!(bp, window: window);
      queue-redisplay(window, $display-point, centering: 0);
      when (sheet-mapped?(window))
	redisplay-window(window)
      end
    end;
    index
  end
end method text-caret-position-setter;


define sealed method character-position
    (window :: <deuce-pane>, x :: <integer>, y :: <integer>) => (index :: <integer>)
  with-editor-state-bound (window)
    let bp = position->bp(window, x, y);
    bp->char-index(bp)
  end
end method character-position;

define sealed method position-character
    (window :: <deuce-pane>, index :: <integer>) => (x, y)
  with-editor-state-bound (buffer = window)
    let bp = char-index->bp(buffer, index);
    bp->position(window, bp)
  end
end method position-character;


define sealed method current-line
    (window :: <deuce-pane>) => (line :: false-or(<integer>))
  with-editor-state-bound (window)
    let bp = window-point(window);
    bp & bp->line-index(bp)
  end
end method current-line;

define sealed method line-length
    (window :: <deuce-pane>, line :: <integer>) => (length :: false-or(<integer>))
  with-editor-state-bound (buffer = window)
    let bp = line-index->bp(buffer, line);
    bp & deuce/line-length(bp-line(bp))
  end
end method line-length;

define sealed method get-line
    (window :: <deuce-pane>, line :: <integer>) => (line :: false-or(<byte-string>))
  with-editor-state-bound (buffer = window)
    let bp = line-index->bp(buffer, line);
    bp & as(<byte-string>, bp-line(bp))
  end
end method get-line;


define sealed method index-line
    (window :: <deuce-pane>, index :: <integer>) => (line :: false-or(<integer>))
  with-editor-state-bound (buffer = window)
    let bp = char-index->bp(buffer, index);
    bp & bp->line-index(bp)
  end
end method index-line;

define sealed method line-index
    (window :: <deuce-pane>, line :: <integer>) => (index :: false-or(<integer>))
  with-editor-state-bound (buffer = window)
    let bp = line-index->bp(buffer, line);
    bp & bp->char-index(bp)
  end
end method line-index;


define sealed method text-range-protected?
    (window :: <deuce-pane>, range :: <text-range>) => (protected? :: <boolean>)
  with-editor-state-bound (buffer = window)
    let interval = text-range->interval(buffer, range);
    interval-read-only?(interval)
  end
end method text-range-protected?;

define sealed method text-range-protected?-setter
    (protected? :: <boolean>, window :: <deuce-pane>, range :: <text-range>)
 => (protected? :: <boolean>)
  with-editor-state-bound (buffer = window)
    let interval = text-range->interval(buffer, range);
    interval-read-only?(interval) := protected?
  end
end method text-range-protected?-setter;

define sealed method text-range->interval
    (buffer :: <basic-buffer>, range :: <text-range>)
 => (interval :: <basic-interval>)
  let sbp = char-index->bp(buffer, text-range-start(range));
  let ebp = char-index->bp(buffer, text-range-end(range));
  make-interval(sbp, ebp)
end method text-range->interval;


define sealed method find-text 
    (window :: <deuce-pane>, string :: <byte-string>) => (index :: false-or(<integer>))
  with-editor-state-bound (window)
    //---*** Do this
  end
end method find-text;
