Module:       duim-deuce-internals
Synopsis:     DUIM back-end for Deuce
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DUIM sheets for a Deuce window

//--- Perhaps only <deuce-gadget> should be a subclass of <text-gadget>?
define open abstract class <deuce-pane>
    (<basic-window>, <text-gadget>, <drawing-pane>)
  sealed constant slot gadget-value-type :: <type> = <string>,
    init-keyword: value-type:;
  sealed slot gadget-columns :: false-or(<integer>) = #f,
    init-keyword: columns:;
  sealed slot gadget-lines   :: false-or(<integer>) = #f,
    init-keyword: lines:;
  sealed slot %last-event = #f;
  sealed slot %horizontal-scroll-bar = #f;
  sealed slot %vertical-scroll-bar   = #f;
  // keyword foreground: = $black;	// leave this to 'port-default-foreground'...
  // keyword background: = $white;	// leave this to 'port-default-background'...
  keyword caret:  = #t;
  keyword cursor: = #"i-beam";
end class <deuce-pane>;

define sealed method gadget-id
    (window :: <deuce-pane>) => (client)
  #f
end method gadget-id;

define sealed method gadget-client
    (window :: <deuce-pane>) => (client)
  #f
end method gadget-client;

define sealed method gadget-documentation
    (window :: <deuce-pane>) => (documentation)
  #f
end method gadget-documentation;


// A <simple-deuce-pane> must be contained in a Deuce frame
define sealed class <simple-deuce-pane>
    (<deuce-pane>)
end class <simple-deuce-pane>;

define sealed inline method make
    (class == <deuce-pane>, #rest initargs, #key, #all-keys)
 => (pane :: <simple-deuce-pane>)
  apply(make, <simple-deuce-pane>, initargs)
end method make;

define sealed domain make (singleton(<simple-deuce-pane>));
define sealed domain initialize (<simple-deuce-pane>);


// A <deuce-gadget> can be contained anywhere
// Note that users of <deuce-gadget> should establish a handler for <command-error>
define open abstract class <deuce-gadget>
    (<editor-state-mixin>, <deuce-pane>)
  sealed slot gadget-id = #f,
    init-keyword: id:;
  sealed slot gadget-client = #f,
    init-keyword: client:;
  sealed slot gadget-documentation :: false-or(<string>) = #f,
    init-keyword: documentation:;
  // When a Deuce gadget gets the focus, we need to ignore the
  // accelerators of the owning frame.  Ditto for Alt=Meta.
  sealed slot %accelerators = #f;
  sealed slot %alt-is-meta? = #f;
  //---*** Record the frame, so that we can handle focus out events even
  //---*** after the gadget has been detached.
  sealed slot %frame :: false-or(<frame>) = #f;
end class <deuce-gadget>;

define variable *gadget-buffer-count* :: <integer> = 0;

define method initialize
    (window :: <deuce-gadget>,
     #key value = $unsupplied, text = $unsupplied, read-only?) => ()
  window-frame(window) := window;
  frame-window(window) := window;
  next-method();
  dynamic-bind (*editor-frame* = window-frame(window))
    inc!(*gadget-buffer-count*);
    let name   = format-to-string("Gadget %d", *gadget-buffer-count*);
    let buffer = make-empty-buffer(<non-file-buffer>,
				   name: name,
				   anonymous?: #t);
    buffer-read-only?(buffer) := read-only?;
    dynamic-bind (*buffer* = buffer)
      select-buffer(window, buffer);
      case
	supplied?(value) =>
	  gadget-value(window, do-callback?: #f) := value;
	supplied?(text) =>
	  gadget-text(window, do-callback?: #f) := text;
	otherwise =>
	  #f;
      end
    end
  end
end method initialize;


define sealed class <simple-deuce-gadget>
    (<deuce-gadget>)
end class <simple-deuce-gadget>;

define sealed inline method make
    (class == <deuce-gadget>, #rest initargs, #key, #all-keys)
 => (pane :: <simple-deuce-gadget>)
  apply(make, <simple-deuce-gadget>, initargs)
end method make;

define sealed domain make (singleton(<simple-deuce-gadget>));
define sealed domain initialize (<simple-deuce-gadget>);


define sealed method do-compose-space
    (pane :: <deuce-pane>, #key width, height)
 => (space-req :: <space-requirement>)
  let style = default-text-style(pane)
	      | make-duim-text-style(#f, window-default-font(pane));
  let (font, fw, fh, fa, fd) = font-metrics(style, port(pane));
  ignore(font, fa, fd);
  let nlines = gadget-lines(pane);
  let ncols  = gadget-columns(pane);
  when (nlines & ~height) height := nlines * fh end;
  when (ncols  & ~width)  width  := ncols  * fw end;
  local method constrain-size
	    (size :: <integer>, minimum :: <integer>, maximum :: <integer>)
	 => (size :: <integer>)
	  max(minimum, min(maximum, size))
	end method;
  let min-chars = 25;
  let min-lines =  3;
  let extra-height = 6;
  let min-width  = min-chars * fw;
  let max-width  = $fill;
  let min-height = min-lines * fh + extra-height;
  let max-height = $fill;
  let width  = constrain-size(width  | min-width,  min-width,  max-width);
  let height = constrain-size(height | min-height, min-height, max-height);
  make(<space-requirement>,
       width:  width,  min-width:  min-width,  max-width:  max-width,
       height: height, min-height: min-height, max-height: max-height)
end method do-compose-space;

define method do-destroy-sheet
    (sheet :: <deuce-pane>) => ()
  let frame = window-frame(sheet);	// ~= sheet-frame for <deuce-gadget>
  when (frame)
    // If this window is going away, stop tracking it
    let windows = editor-windows(frame-editor(frame));
    remove!(windows, sheet)
  end;
  next-method()
end method do-destroy-sheet;

define method do-destroy-sheet
    (sheet :: <deuce-gadget>) => ()
  let frame = window-frame(sheet);	// ~= sheet-frame for <deuce-gadget>
  when (frame)
    // Deuce gadgets act as Deuce frames, so we need to stop tracking
    // the gadget-as-frame, too
    let frames = editor-frames(frame-editor(frame));
    remove!(frames, sheet)
  end;
  next-method()
end method do-destroy-sheet;

define sealed method deuce/window-enabled?
    (window :: <deuce-pane>) => (enabled? :: <boolean>)
  port(window) & sheet-mapped?(window)
end method deuce/window-enabled?;

define sealed method deuce/window-occluded?
    (window :: <deuce-pane>) => (enabled? :: <boolean>)
  let frame = sheet-frame(window);
  ~frame | frame-occluded?(frame)
end method deuce/window-occluded?;

define sealed method deuce/window-size
    (window :: <deuce-pane>) => (width :: <integer>, height :: <integer>)
  box-size(sheet-region(window))
end method deuce/window-size;

define sealed method deuce/window-viewport-size
    (window :: <deuce-pane>) => (width :: <integer>, height :: <integer>)
  box-size(sheet-viewport-region(window))
end method deuce/window-viewport-size;


// Arrange for redisplay to happen in the correct thread
define method redisplay-window-within-frame
    (frame :: <editor-state-mixin>, window :: <deuce-pane>, #key move-point? = #t) => ()
  let window-frame = sheet-frame(window);
  when (window-frame)
    if (frame-thread(window-frame) == current-thread())
      do-redisplay-window(window, move-point?: move-point?)
    else
      call-in-frame(window-frame, do-redisplay-window, window, move-point?: move-point?)
    end
  end
end method redisplay-window-within-frame;


// By default, display simple messages in the status bar, if there is one.
// We do this even for embedded <deuce-gadget> panes because it's better
// than throwing away the message altogether.
define method deuce/display-message
    (window :: <deuce-pane>, format-string :: <string>, #rest format-args) => ()
  let frame = sheet-frame(window);
  when (frame)
    let status-bar = frame-status-bar(frame);
    when (status-bar)
      let string = apply(format-to-string, format-string, format-args);
      gadget-label(status-bar) := string
    end
  end
end method deuce/display-message;

// Like 'display-message', but it beeps in addition to displaying the error
define method deuce/display-error-message
    (window :: <deuce-pane>, format-string :: <string>, #rest format-args) => ()
  beep(window);
  apply(deuce/display-message, window, format-string, format-args)
end method deuce/display-error-message;


// Display the buffer name in the frame title area
define method deuce/display-buffer-name
    (window :: <deuce-pane>, buffer :: false-or(<basic-buffer>)) => ()
  let frame = sheet-frame(window);
  when (frame)
    let string = if (buffer) buffer-name(buffer) else "" end;
    frame-title(frame) := string
  end
end method deuce/display-buffer-name;

// In an embedded Deuce gadget, there's definitely no place to do this
define method deuce/display-buffer-name
    (window :: <deuce-gadget>, buffer :: false-or(<basic-buffer>)) => ()
  ignore(buffer);
  #f
end method deuce/display-buffer-name;


/// Scrolling

// Note that there is an asymmetry in the behavior of horizontal and vertical
// scrolling in Deuce.  Deuce does its own vertical scrolling, and it maintains
// the vertical scroll position in lines.  Deuce simply uses DUIM to do horizontal
// scrolling, and so the horizontal scroll position is maintained in pixels.

// We manage vertical scrolling by hand, and let DUIM take care of
// horizontal scrolling
define sealed method initialize-scrolling
    (scroller :: <scroller>, window :: <deuce-pane>) => ()
  let viewport = sheet-viewport(window);
  let hscroll  = sheet-horizontal-scroll-bar(viewport);
  let vscroll  = sheet-vertical-scroll-bar(viewport);
  window.%horizontal-scroll-bar := hscroll;
  window.%vertical-scroll-bar   := vscroll;
  gadget-value-changed-callback(vscroll)  := scroll-window-vertically;
  gadget-value-changing-callback(vscroll) := scroll-window-vertically;
  gadget-client(vscroll) := window;
  // Now make DUIM think that the scroll bar is just plain gone
  sheet-vertical-scroll-bar(viewport) := #f
end method initialize-scrolling;

define method sheet-scrolls-vertically?
    (window :: <deuce-pane>) => (true? :: <boolean>)
  window.%vertical-scroll-bar ~== #f
end method sheet-scrolls-vertically?;

define method sheet-scrolls-horizontally?
    (window :: <deuce-pane>) => (true? :: <boolean>)
  window.%horizontal-scroll-bar ~== #f
end method sheet-scrolls-horizontally?;

// NB: 'total-columns' and 'visible-columns' are in pixels
define sealed method deuce/update-scroll-bar
    (window :: <deuce-pane>, which == #"horizontal",
     total-columns :: <integer>, position :: <integer>, visible-columns :: <integer>) => ()
  let (width, height)   = box-size(sheet-region(window));
  let (vwidth, vheight) = box-size(sheet-viewport-region(window));
  ignore(vheight);
  // Make sure the window's width never gets smaller than the viewport
  let new-width  = max(vwidth, total-columns);
  let new-height = height;
  // Update the horizontal scroll bar by updating the Deuce window region
  unless (new-width = width)
    sheet-region(window) := set-box-size(sheet-region(window), new-width, new-height);
    let scroll-bar = window.%horizontal-scroll-bar;
    when (scroll-bar)
      // If no position was supplied, use the current one
      when (position < 0)
        let (x, y) = scroll-position(window);
        ignore(y);
        position := x
      end;
      update-scroll-bar(scroll-bar, 0, total-columns, position, position + visible-columns)
    end
  end
end method deuce/update-scroll-bar;

// NB: 'total-lines' and 'visible-lines' are in lines
define sealed method deuce/update-scroll-bar
    (window :: <deuce-pane>, which == #"vertical",
     total-lines :: <integer>, position :: <integer>, visible-lines :: <integer>) => ()
  // Update the vertical scroll bar directly
  let scroll-bar = window.%vertical-scroll-bar;
  when (scroll-bar)
    let contents-start = 0;
    let contents-end   = max(total-lines - 1, 0);
    let visible-start  = position;
    let visible-end    = max(position + visible-lines - 1, 0);
    update-scroll-bar(scroll-bar, contents-start, contents-end, visible-start, visible-end)
  end
end method deuce/update-scroll-bar;

define sealed method deuce/scroll-position
    (window :: <deuce-pane>) => (x :: <integer>, y :: <integer>)
  scroll-position(window)
end method deuce/scroll-position;

define sealed method deuce/set-scroll-position
    (window :: <deuce-pane>, x :: false-or(<integer>), y :: false-or(<integer>)) => ()
  let (sx, sy) = scroll-position(window);
  set-scroll-position(window, x | sx, y | sy)
end method deuce/set-scroll-position;

// This gets called only via the vertical scroll bar's value-change callback
define sealed method scroll-window-vertically
    (scroll-bar :: <scroll-bar>) => ()
  let window = gadget-client(scroll-bar);
  with-editor-state-bound (buffer = window)
    let n-lines :: <integer> = window-n-display-lines(window);
    // DUIM scroll bars might have floating values in them.
    // Furthermore, 'scroll-by-pixels' might go negative if
    // the Deuce pane isn't an integral number of lines high
    let frame = window-frame(window);
    let move-point? = scrolling-moves-point?(editor-policy(frame-editor(frame)));
    let old-line :: <integer> = window-line-number(window);
    let new-line :: <integer> = max(0, floor(gadget-value(scroll-bar)));
    let n :: <integer> = new-line - old-line;
    if (abs(n) < n-lines)
      // Scrolling less than one screenful, do it the fast way
      scroll-n-lines(window, n, move-point?: move-point?)
    else
      let line = line-index->line(buffer, new-line, skip-test: #f);
      when (line)
        when (move-point?)
	  move-point!(line-start(line), window: window)
	end;
        recenter-window(window, line, #"top");
        window-centering-fraction(window) := #f;
        queue-redisplay(window, $display-all);
        redisplay-window(window, move-point?: move-point?)
      end
    end
  end
end method scroll-window-vertically;

define sealed method line-scroll-amount
    (window :: <deuce-pane>, orientation :: <gadget-orientation>)
 => (amount :: <integer>)
  select (orientation)
    #"horizontal" =>
      // Scroll horizontally by one character width (in pixels)
      let font = window-default-font(window);
      let (font-width, font-height) = deuce/font-metrics(window, font);
      ignore(font-height);
      font-width;
    #"vertical"   =>
      // Scroll vertically by one line (in lines)
      1;
  end
end method line-scroll-amount;

define sealed method page-scroll-amount
    (window :: <deuce-pane>, orientation :: <gadget-orientation>)
 => (amount :: <integer>)
  select (orientation)
    #"horizontal" =>
      // Scroll horizontally by one page width (in pixels)
      let (width, height) = deuce/window-size(window);
      ignore(height);
      width;
    #"vertical"   =>
      // Scroll vertically by one page (in lines)
      window-n-display-lines(window) - 1;
  end
end method page-scroll-amount;


/// Clipboard

define sealed method add-to-clipboard
    (window :: <deuce-pane>, data :: <string>) => ()
  with-clipboard (clipboard = window)
    add-clipboard-data(clipboard, data)
  end
end method add-to-clipboard;

//--- It's too bad that we have to coerce intervals to strings
define sealed method add-to-clipboard
    (window :: <deuce-pane>, data :: <interval>) => ()
  with-clipboard (clipboard = window)
    add-clipboard-data(clipboard, as(<string>, data))
  end
end method add-to-clipboard;

//--- Right now we only support strings
define sealed method get-from-clipboard
    (window :: <deuce-pane>, class :: subclass(<string>))
 => (data :: false-or(<string>))
  let data = with-clipboard (clipboard = window)
	       get-clipboard-data-as(class, clipboard)
	     end;
  // If there's a Return character in the clipboard data,
  // fix newline sequences to conform to Deuce's conventions.
  when (data & position(data, '\r'))
    let length :: <integer> = size(data);
    let sv :: <stretchy-object-vector> = make(<stretchy-vector>);
    for (i :: <integer> from 0 below length)
      let ch :: <character> = data[i];
      if (ch = '\r')
	// If we found a '\r' that's not followed by a '\n',
	// convert it to a '\n'.  If it is followed by a '\n',
	// just skip over it.
	when (i = length - 1 | data[i + 1] ~= '\n')
	  add!(sv, '\n')
	end
      else
	add!(sv, ch)
      end
    end;
    data := as(<string>, sv)
  end;
  data
end method get-from-clipboard;


/// Cursors and carets

define sealed method deuce/cursor-position
    (window :: <deuce-pane>) => (x :: <integer>, y :: <integer>)
  let pointer = port-pointer(port(window));
  pointer-position(pointer, sheet: window)
end method deuce/cursor-position;

define sealed method deuce/set-cursor-position
    (window :: <deuce-pane>, x :: <integer>, y :: <integer>) => ()
  let pointer = port-pointer(port(window));
  set-pointer-position(pointer, x, y, sheet: window)
end method deuce/set-cursor-position;

define sealed method deuce/do-with-busy-cursor
    (window :: <deuce-pane>, continuation :: <function>) => (#rest values)
  let frame = sheet-frame(window);
  with-busy-cursor (frame)
    continuation()
  end
end method deuce/do-with-busy-cursor;

define sealed method deuce/caret-position
    (window :: <deuce-pane>) => (x :: <integer>, y :: <integer>)
  let caret = sheet-caret(window);
  if (caret?(caret)) caret-position(caret)
  else values(0, 0) end
end method deuce/caret-position;

define sealed method deuce/set-caret-position
    (window :: <deuce-pane>, x :: <integer>, y :: <integer>) => ()
  let caret = sheet-caret(window);
  when (caret?(caret))
    set-caret-position(caret, x, y)
  end
end method deuce/set-caret-position;

define sealed method deuce/caret-size
    (window :: <deuce-pane>) => (width :: <integer>, height :: <integer>)
  let caret = sheet-caret(window);
  if (caret?(caret)) caret-size(caret)
  else values(0, 0) end
end method deuce/caret-size;

define sealed method deuce/set-caret-size
    (window :: <deuce-pane>, width :: <integer>, height :: <integer>) => ()
  let caret = sheet-caret(window);
  when (caret?(caret))
    set-caret-size(caret, width, height)
  end
end method deuce/set-caret-size;

define sealed method deuce/show-caret
    (window :: <deuce-pane>, #key tooltip?) => ()
  let caret = sheet-caret(window);
  // The caret can be #t during sheet bootstrapping, so watch out
  when (caret?(caret) & ~caret-visible?(caret))
    caret-visible?(caret, tooltip?: tooltip?) := #t
  end
end method deuce/show-caret;

define sealed method deuce/hide-caret
    (window :: <deuce-pane>, #key tooltip?) => ()
  let caret = sheet-caret(window);
  // The caret can be #t during sheet bootstrapping, so watch out
  when (caret?(caret) & caret-visible?(caret))
    caret-visible?(caret, tooltip?: tooltip?) := #f
  end
end method deuce/hide-caret;


/// Repaint handling

define sealed method handle-repaint
    (sheet :: <deuce-pane>, medium :: <medium>, region :: <region>) => ()
  //--- It would be more efficient to obey the region...
  ignore(region);
  // Simple enough -- just redraw the existing display lines
  with-editor-state-bound (sheet)
    let frame = window-frame(sheet);
    let move-point? = scrolling-moves-point?(editor-policy(frame-editor(frame)));
    do-redisplay-window(sheet, redisplay?: #t, move-point?: move-point?)
  end
end method handle-repaint;

define sealed method handle-event
    (sheet :: <deuce-pane>, event :: <window-configuration-event>) => ()
  // Force the display lines for the window to be recomputed
  // We'll get a repaint event, so no need to actually do the redisplay yet
  with-editor-state-bound (sheet)
    queue-redisplay(sheet, $display-all)
  end
end method handle-event;


/// Deuce gadget focus handling

// Save the frame's accelerators, then turn them all off so that
// Deuce's own keyboard event handling will do all the work.
// Ditto for Alt=Meta.
define method handle-event
    (sheet :: <deuce-gadget>, event :: <input-focus-in-event>) => ()
  let frame = sheet-frame(sheet);
  debug-message("Focus in: %=, %=", sheet, frame);
  sheet.%frame := frame;
  when (frame)
    let policy = editor-policy(frame-editor(window-frame(sheet)));
    sheet.%accelerators := frame-accelerators(frame);
    sheet.%alt-is-meta? := frame-alt-key-is-meta?(frame);
    frame-accelerators(frame)     := #[];
    frame-alt-key-is-meta?(frame) := alt-key-is-meta?(policy)
  end
end method handle-event;

// Restore the frame's accelerators and Alt=Meta state
define method handle-event
    (sheet :: <deuce-gadget>, event :: <input-focus-out-event>) => ()
  let frame = sheet.%frame | sheet-frame(sheet);
  debug-message("Focus out: %=, %=", sheet, frame);
  when (frame)
    when (sheet.%accelerators)
      frame-accelerators(frame)     := sheet.%accelerators;
      frame-alt-key-is-meta?(frame) := sheet.%alt-is-meta?;
    end;
    sheet.%accelerators := #f
  end
end method handle-event;
