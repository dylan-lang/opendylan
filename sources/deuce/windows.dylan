Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Windows

define constant $default-window-border       = 0;
define constant $default-window-line-spacing = 1;

define protocol <<window>> ()
  getter window-lock
    (window :: <window>) => (lock :: <exclusive-lock>);
  getter window-buffer
    (window :: <window>) => (buffer :: false-or(<buffer>));
  setter window-buffer-setter
    (buffer :: false-or(<buffer>), window :: <window>) => (buffer :: false-or(<buffer>));
  getter window-point
    (window :: <window>) => (point :: <bp>);
  setter window-point-setter
    (point :: <bp>, window :: <window>) => (point :: <bp>);
  getter window-mark
    (window :: <window>) => (mark :: false-or(<bp>));
  setter window-mark-setter
    (mark :: false-or(<bp>), window :: <window>) => (mark :: false-or(<bp>));
  getter window-temporary-mark?
    (window :: <window>) => (temporary? :: type-union(<boolean>, <bp>));
  setter window-temporary-mark?-setter
    (temporary? :: type-union(<boolean>, <bp>), window :: <window>)
 => (temporary? :: type-union(<boolean>, <bp>));
  getter window-hide-section-separators?
    (window :: <window>) => (hide? :: <boolean>);
  getter line-visible-in-window?
    (line :: <line>, window :: <window>) => (visible? :: <boolean>);
  // Buffer selection
  function select-buffer
    (window :: <window>, buffer :: <buffer>) => ();
  function select-buffer-in-appropriate-window
    (window :: <window>, buffer :: <buffer>, #key line, index) => ();
  // Notifications
  function window-note-mode-entered
    (window :: <window>, mode :: <mode>) => ();
  function window-note-buffer-changed
    (window :: <window>, buffer :: <buffer>, modified? :: <boolean>) => ();
  function window-note-buffer-read-only
    (window :: <window>, buffer :: <buffer>, read-only? :: <boolean>) => ();
  function window-note-buffer-selected
    (window :: <window>, buffer :: false-or(<buffer>)) => ();
  function window-note-selection-changed
    (window :: <window>, mark :: false-or(<bp>)) => ();
  function window-note-search-string
    (window :: <window>, string :: false-or(<string>)) => ();
  function window-note-undo/redo
    (window :: <window>, undo? :: <boolean>, redo? :: <boolean>) => ();
  function window-note-policy-changed
    (window :: <window>,
     new-policy :: <editor-policy>, old-policy :: false-or(<editor-policy>)) => ();
end protocol <<window>>;


// A window is an abstract model of the intuitive notion of a window.
// A buffer gets displayed in a window.
// Deuce back-ends are expected to supply a concrete implementation class
// that is a subclass of <basic-window>.
define open abstract class <basic-window> (<window>)
  constant sealed slot window-lock :: <exclusive-lock> = make(<recursive-lock>);
  // The buffer being displayed in this window
  sealed slot window-buffer :: false-or(<basic-buffer>) = #f,
    init-keyword: buffer:;
  // 'point' and 'mark' delimit a contiguous selected region
  // Both of these will always be permanent BPs pointing into 'window-buffer'
  sealed slot window-point :: <basic-bp> = $null-bp,
    init-keyword: point:;
  sealed slot window-mark :: false-or(<basic-bp>) = #f,
    init-keyword: mark:;
  // keep track of when the mark was set using Shift + arrow keys
  // and drop the mark on point movements then
  sealed slot window-mark-with-shift :: <boolean> = #f;
  // This is only set by 'with-temporary-selection'...
  sealed slot window-temporary-mark? :: type-union(<boolean>, <bp>) = #f;
  sealed slot window-last-mark :: false-or(<basic-bp>) = #f;
  sealed slot window-point-pdl :: <list> = #();
  // Goal X position for 'next-line' and 'previous-line'
  sealed slot window-goal-x-position :: false-or(<integer>) = #f;
  // The set of recently selected buffers, in order
  // Each item in the deque is a <buffer-selection>
  sealed slot window-selected-buffers :: <object-deque> = make(<deque>);
  // A vector of the <display-line>s to display, and the number of active
  // display lines in the vector
  sealed slot window-display-lines :: <simple-object-vector> = #[];
  sealed slot window-n-display-lines :: <integer> = 0;
  // The width of the widest visible display line
  sealed slot window-max-line-width  :: <integer> = 0;
  // This tells us how much redisplay we have to do
  sealed slot window-redisplay-degree :: <integer> = $display-all;
  // The line at which to start doing redisplay computations
  sealed slot window-initial-line :: false-or(<basic-line>) = #f;
  // The size of the window when we last did redisplay
  sealed slot window-last-size :: <pair> = pair(0, 0);
  sealed slot %line-number :: false-or(<integer>) = #f,
    setter: window-line-number-setter;
  sealed slot %total-lines :: false-or(<integer>) = #f,
    setter: window-total-lines-setter;
  // When the redisplay degree is $display-line, these tell which line
  // (and at what index) to begin redisplay.  When the degree is $display-blt,
  // these tell what line after which to insert/delete new display lines.
  sealed slot window-redisplay-line  :: false-or(<basic-line>) = #f;
  sealed slot window-redisplay-index :: false-or(<integer>) = #f;
  // A cache for 'find-display-line'
  sealed slot window-display-line-cache :: false-or(<display-line>) = #f;
  sealed slot window-display-line-hint  :: false-or(<integer>) = #f;
  // How should the window be recentered?
  sealed slot window-centering-fraction :: false-or(<real>) = #f;
  // For bracket matching
  sealed slot window-matching-bp     :: false-or(<basic-bp>) = #f;
  sealed slot window-matching-string :: <byte-string> = make(<byte-string>, size: 2);
  // Pointer from the window back to the owning frame
  sealed slot window-frame :: <editor-state-mixin> = $null-editor-frame,
    init-keyword: frame:;
  // Inter-line spacing
  sealed slot window-line-spacing :: <integer> = $default-window-line-spacing,
    init-keyword: line-spacing:;
  // A cache for the 'line-invisible-in-window?' test
  slot %line-invisible-test :: <function> = false;
  // The window's default fonts
  sealed slot window-default-font        :: <font> = $default-font,
    init-keyword: font:;
  sealed slot window-default-bold-font   :: <font> = $default-bold-font,
    init-keyword: bold-font:;
  sealed slot window-default-italic-font :: <font> = $default-italic-font,
    init-keyword: italic-font:;
  // Caches for the window back-end
  sealed slot window-color = #f;	// <color> or foreground/background
  sealed slot window-font  = #f;	// <font>
end class <basic-window>;

define method initialize
    (window :: <basic-window>, #key) => ()
  next-method();
  let windows = editor-windows(frame-editor(window-frame(window)));
  add!(windows, window);
  window.%line-invisible-test := rcurry(line-invisible-in-window?, window)
end method initialize;

// The line number for the initial display line, for scroll bars
define method window-line-number
    (window :: <basic-window>) => (line-number :: false-or(<integer>))
  window.%line-number
  | begin
      let line = window-initial-line(window);
      when (line)
	let index = bp->line-index(line-start(line),
                                   skip-test: window.%line-invisible-test);
	window-line-number(window) := index;
	index
      end
    end
end method window-line-number;

// The total number of lines in the window's buffer, for scroll bars
define method window-total-lines
    (window :: <basic-window>) => (total-lines :: false-or(<integer>))
  window.%total-lines
  | begin
      let buffer = window-buffer(window);
      when (buffer)
	let total = count-lines(buffer,
                                skip-test: window.%line-invisible-test,
				cache-result?: #t);
	window-total-lines(window) := total;
	total
      end
    end
end method window-total-lines;

define method window-border
    (window :: <basic-window>) => (border :: <integer>)
  $default-window-border
end method window-border;

define method window-hide-section-separators?
    (window :: <basic-window>) => (hide? :: <boolean>)
  let buffer :: <basic-buffer> = window-buffer(window);
  let style  = buffer-section-separator-style(buffer);
  select (style)
    #"always"    => #f;
    #"never"     => #t;
    #"requested" =>
      let frame  = window-frame(window);
      let policy = editor-policy(frame-editor(frame));
      ~show-section-separators?(policy);
  end
end method window-hide-section-separators?;

define method line-visible-in-window?
    (line :: <basic-line>, window :: <basic-window>) => (visible? :: <boolean>)
  #t
end method line-visible-in-window?;

define inline function line-invisible-in-window?
    (line :: <line>, window :: <window>) => (visible? :: <boolean>)
  ~line-visible-in-window?(line, window)
end function line-invisible-in-window?;


/// Default notification methods

define method window-note-mode-entered
    (window :: <basic-window>, mode :: <major-mode>) => ()
  // Install the command table for this major mode
  let frame = window-frame(window);
  frame-command-set(frame)   := mode-command-set(mode);
  frame-command-state(frame) := standard-command-table(frame-command-set(frame))
end method window-note-mode-entered;

define method window-note-buffer-changed
    (window :: <basic-window>, buffer :: <basic-buffer>, modified? :: <boolean>) => ()
  #f
end method window-note-buffer-changed;

define method window-note-buffer-read-only
    (window :: <basic-window>, buffer :: <basic-buffer>, read-only? :: <boolean>) => ()
  #f
end method window-note-buffer-read-only;

define method window-note-buffer-selected
    (window :: <basic-window>, buffer :: <basic-buffer>) => ()
  display-buffer-name(window, buffer);
  window-note-buffer-changed(window, buffer, buffer-modified?(buffer));
  window-note-buffer-read-only(window, buffer, buffer-read-only?(buffer));
  let history = buffer-undo-history(buffer);
  when (history)
    let (n-undo, n-redo) = undo-history-state(history);
    window-note-undo/redo(window, n-undo ~= 0, n-redo ~= 0)
  end;
  window-note-selection-changed(window, window-mark(window))
end method window-note-buffer-selected;

define method window-note-buffer-selected
    (window :: <basic-window>, buffer == #f) => ()
  display-buffer-name(window, #f);
  window-note-undo/redo(window, #f, #f);
  window-note-selection-changed(window, window-mark(window))
end method window-note-buffer-selected;

define method window-note-selection-changed
    (window :: <basic-window>, mark :: false-or(<basic-bp>)) => ()
  #f
end method window-note-selection-changed;

define method window-note-search-string
    (window :: <basic-window>, string :: false-or(<string>)) => ()
  #f
end method window-note-search-string;

define method window-note-undo/redo
    (window :: <basic-window>, undo? :: <boolean>, redo? :: <boolean>) => ()
  #f
end method window-note-undo/redo;

define method window-note-policy-changed
    (window :: <basic-window>,
     new-policy :: <editor-policy>, old-policy :: false-or(<editor-policy>)) => ()
  // Changing the policy may affect the Copy command's enabling, depending
  // on the selection.
  let mark?      = (mark & #t);
  let copy-line? = (unselected-copy-policy(new-policy) == #"copy-line");
  command-enabled?(window, copy-region) := mark? | copy-line?;
  // Search command enabling is affected by what command set we're using...
  let editor = frame-editor(window-frame(window));
  window-note-search-string(window, editor-search-string(editor));
  // If necessary, update the window's "title-bar".
  when (~old-policy
	| show-path-in-title?(old-policy) ~== show-path-in-title?(new-policy))
    display-buffer-name(window, window-buffer(window))
  end;
  // Update the font and redisplay, if it changed.
  when (~old-policy
	| default-font(new-policy) ~= default-font(old-policy)
	| show-section-separators?(new-policy) ~= show-section-separators?(old-policy))
    set-default-font(window, default-font(new-policy));
    // Don't redisplay immediately in case previous methods are also going
    // to do things requiring redisplay; leave redisplay to the caller
    queue-redisplay(window, $display-all)
  end;
end method window-note-policy-changed;


/// Window back end protocol

define constant <goto-target-type> = one-of(#"line", #"character");

define protocol <<window-back-end>> ()
  function window-enabled?
    (window :: <window>) => (enabled? :: <boolean>);
  function window-occluded?
    (window :: <window>) => (occluded? :: <boolean>);
  // The window size is the size of the Deuce pane, which can be wider
  // than what is visible on the screen when there is scrolling.  The
  // height of the window might be smaller than the viewport height, but
  // because Deuce manages its own vertical scrolling, it's never taller.
  function window-size
    (window :: <window>) => (width :: <integer>, height :: <integer>);
  // The window viewport size is the size of the visible part of the
  // Deuce pane.  When there is no scrolling, this is the same as the
  // window size.
  function window-viewport-size
    (window :: <window>) => (width :: <integer>, height :: <integer>);
  function update-scroll-bar
    (window :: <window>, which,
     total-lines :: <integer>, position :: <integer>, visible-lines :: <integer>) => ();
  function scroll-position
    (window :: <window>) => (x :: <integer>, y :: <integer>);
  function set-scroll-position
    (window :: <window>, x :: false-or(<integer>), y :: false-or(<integer>)) => ();
  function window-line-spacing
    (window :: <window>) => (spacing :: <integer>);
  // Message display functions
  function display-message
    (window :: <window>, format-string :: <string>, #rest format-args) => ();
  function display-error-message
    (window :: <window>, format-string :: <string>, #rest format-args) => ();
  function display-buffer-name
    (window :: <window>, buffer :: false-or(<buffer>)) => ();
  // Text display functions
  function draw-string
    (window :: <window>,
     string :: <string>, x :: <integer>, y :: <integer>,
     #key start: _start, end: _end, color, font, align-x, align-y) => ();
  function string-size
    (window :: <window>, string :: <string>,
     #key start: _start, end: _end, font)
 => (width :: <integer>, height :: <integer>, baseline :: <integer>);
  // Other simple graphics
  function draw-line
    (window :: <window>,
     x1 :: <integer>, y1 :: <integer>, x2 :: <integer>, y2 :: <integer>,
     #key color, thickness) => ();
  function draw-rectangle
    (window :: <window>,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>,
     #key color, thickness, filled?) => ();
  function draw-image
    (window :: <window>,
     image, x :: <integer>, y :: <integer>) => ();
  function clear-area
    (window :: <window>,
     left :: <integer>, top :: <integer>, right :: <integer>, bottom :: <integer>) => ();
  // BITBLT
  function copy-area
    (window :: <window>,
     from-x :: <integer>, from-y :: <integer>, width :: <integer>, height :: <integer>,
     to-x :: <integer>, to-y :: <integer>) => ();
  // Cursor and caret
  function cursor-position
    (window :: <window>) => (x :: <integer>, y :: <integer>);
  function set-cursor-position
    (window :: <window>, x :: <integer>, y :: <integer>) => ();
  function do-with-busy-cursor
    (window :: <window>, continuation :: <function>) => (#rest values);
  function caret-position
    (window :: <window>) => (x :: <integer>, y :: <integer>);
  function set-caret-position
    (window :: <window>, x :: <integer>, y :: <integer>) => ();
  function caret-size
    (window :: <window>) => (width :: <integer>, height :: <integer>);
  function set-caret-size
    (window :: <window>, width :: <integer>, height :: <integer>) => ();
  function show-caret
    (window :: <window>, #key tooltip?) => ();
  function hide-caret
    (window :: <window>, #key tooltip?) => ();
  // Font metrics
  function font-metrics
    (window :: <window>, font :: false-or(<font>))
 => (width :: <integer>, height :: <integer>, ascent :: <integer>, descent :: <integer>);
  // Simple menu choose
  function choose-from-menu
    (window :: <window>, items :: <sequence>,
     #key title, value, label-key, value-key, width, height, multiple-sets?)
 => (value :: false-or(<object>), success? :: <boolean>);
  function choose-from-dialog
    (window :: <window>, items :: <sequence>,
     #key title, value, label-key, value-key, width, height, selection-mode)
 => (value :: false-or(<object>), success? :: <boolean>,
     width :: false-or(<integer>), height :: false-or(<integer>));
  // Canned dialogs
  function information-dialog
    (window :: <window>, format-string :: <string>, #rest format-args) => ();
  function warning-dialog
    (window :: <window>, format-string :: <string>, #rest format-args) => ();
  function yes-or-no-dialog
    (window :: <window>, format-string :: <string>, #rest format-args)
 => (result :: <boolean>);
  function yes-no-or-cancel-dialog
    (window :: <window>, format-string :: <string>, #rest format-args)
 => (result :: type-union(<boolean>, singleton(#"cancel")));
  function open-file-dialog
    (window :: <window>, #key default, default-type)
 => (pathname :: false-or(<pathname>));
  function new-file-dialog
    (window :: <window>, #key default, default-type)
 => (pathname :: false-or(<pathname>));
  function save-buffers-dialog
    (window :: <window>,
     #key exit-label :: false-or(<string>), reason :: false-or(<string>),
	  buffers :: false-or(<sequence>), default-buffers :: false-or(<sequence>))
 => (buffers :: type-union(<sequence>, singleton(#f), singleton(#"cancel")),
     no-buffers? :: <boolean>);
  function choose-buffer-dialog
    (window :: <window>, #key title, buffer :: false-or(<buffer>), buffers)
 => (buffer :: false-or(<buffer>));
  function choose-buffers-dialog
    (window :: <window>, #key title, buffer :: false-or(<buffer>), buffers)
 => (buffers :: false-or(<sequence>));
  function new-buffer-dialog
    (window :: <window>, #key title)
 => (buffer-name);
  function edit-definition-dialog
    (window :: <window>, name :: <string>, #key title)
 => (definition);
  function choose-string-dialog
    (window :: <window>, #key default, prompt, title)
 => (string);
  function hack-matching-lines-dialog
    (window :: <window>)
 => (string, operation);
  function goto-position-dialog
    (window :: <window>, what :: <goto-target-type>)
 => (number :: false-or(<integer>), what :: false-or(<goto-target-type>));
  // The next two are modeless, and hence return no values
  function string-search-dialog
    (window :: <window>,
     #key string,
	  reverse? :: <boolean>, case-sensitive? :: <boolean>, whole-word? :: <boolean>) => ();
  function string-replace-dialog
    (window :: <window>,
     #key string, replace,
	  reverse? :: <boolean>, case-sensitive? :: <boolean>, whole-word? :: <boolean>) => ();
  function configuration-dialog
    (window :: <window>) => (policy :: false-or(<editor-policy>));
  // Clipboard
  function add-to-clipboard
    (window :: <window>, data) => ();
  function get-from-clipboard
    (window :: <window>, class) => (data);
  // Commands
  getter command-enabled?
    (window :: <window>, command :: <function>)
 => (enabled? :: <boolean>);
  setter command-enabled?-setter
    (enabled? :: <boolean>, window :: <window>, command :: <function>)
 => (enabled? :: <boolean>);
  // Character-stream-like functions for reading from the keyboard
  function read-character
    (window :: <window>) => (character :: <character>);
  function read-gesture
    (window :: <window>) => (keysym, char, modifiers);
end protocol <<window-back-end>>;


define macro with-busy-cursor
  { with-busy-cursor (?window:expression) ?:body end }
    => { begin
	   let with-busy-cursor-body = method () ?body end;
	   do-with-busy-cursor(?window, with-busy-cursor-body)
         end }
end macro with-busy-cursor;


/// Colors

// Colors are represented as integers, and so can be compared with \=
define constant <color> = limited(<integer>, min: 0, max: 16777216);

define inline function make-color
    (r :: <integer>, g :: <integer>, b :: <integer>) => (color :: <color>)
  logior(r, ash(g, 8), ash(b, 16))
end function make-color;

define inline function color-red
    (color :: <color>) => (red :: limited(<integer>, min: 0, max: 255))
  logand(color, #xFF)
end function color-red;

define inline function color-green
    (color :: <color>) => (green :: limited(<integer>, min: 0, max: 255))
  logand(ash(color, -8), #xFF)
end function color-green;

define inline function color-blue
    (color :: <color>) => (blue :: limited(<integer>, min: 0, max: 255))
  logand(ash(color, -16), #xFF)
end function color-blue;

define variable $black   :: <color> = make-color(  0,   0,   0);
define variable $white   :: <color> = make-color(255, 255, 255);
define variable $red     :: <color> = make-color(255,   0,   0);
define variable $green   :: <color> = make-color(  0, 255,   0);
define variable $blue    :: <color> = make-color(  0,   0, 255);
define variable $cyan    :: <color> = make-color(  0, 255, 255);
define variable $magenta :: <color> = make-color(255,   0, 255);
define variable $yellow  :: <color> = make-color(255, 255,   0);

define constant $default-foreground = #"foreground";
define constant $default-background = #"background";


/// Fonts

define function update-caret-from-font (window :: <basic-window>) => ()
  let (fw, fh, fa, fd) = font-metrics(window, window-default-font(window));
  ignore(fh);
  let width  :: <integer> = if (fw < 6) 1 else 2 end;
  let height :: <integer> = fa + fd;
  set-caret-size(window, width, height);
end function update-caret-from-font;

// Sets the default font for the window
// Note that doing a full redisplay is required after doing this
define open generic set-default-font
    (window :: <window>, font) => ();

define method set-default-font
    (window :: <basic-window>, font) => ()
  // Update all of the window's fonts
  window-default-font(window)
    := make-font(font-family(font), font-name(font), font-weight(font),
		 font-slant(font), font-size(font));
  window-default-bold-font(window)
    := make-font(font-family(font), font-name(font), #"bold",
		 font-slant(font), font-size(font));
  window-default-italic-font(window)
    := make-font(font-family(font), font-name(font), font-weight(font),
		 #"italic", font-size(font));
  // Update the height of the caret
  update-caret-from-font(window);
end method set-default-font;

// Sets the default font size for the window
// Note that doing a full redisplay is required after doing this
define open generic set-default-font-size
    (window :: <window>, font-size) => ();

define method set-default-font-size
    (window :: <basic-window>, font-size) => ()
  local method merge-size (font :: <font>, size)
	  make-font(font-family(font), font-name(font), font-weight(font),
		    font-slant(font), size)
	end method;
  // Update all of the window's fonts
  window-default-font(window)
    := merge-size(window-default-font(window), font-size);
  window-default-bold-font(window)
    := merge-size(window-default-bold-font(window), font-size);
  window-default-italic-font(window)
    := merge-size(window-default-italic-font(window), font-size);
  // Update the height of the caret
  update-caret-from-font(window);
end method set-default-font-size;


/// Canned images

define open generic standard-images
    (window :: <window>, i :: <integer>) => (image);

define constant $potential-breakpoint :: <integer> =  0;
define constant $enabled-breakpoint   :: <integer> =  1;
define constant $disabled-breakpoint  :: <integer> =  2;
define constant $step-breakpoint      :: <integer> =  3;
define constant $test-breakpoint      :: <integer> =  4;
define constant $enabled-tracepoint   :: <integer> =  5;
define constant $disabled-tracepoint  :: <integer> =  6;
define constant $profile-point        :: <integer> =  7;
define constant $current-location     :: <integer> =  8;
define constant $prompt-arrow         :: <integer> =  9;
define constant $values-arrow         :: <integer> = 10;
define constant $warning	      :: <integer> = 11;
define constant $serious-warning      :: <integer> = 12;


/// Current buffer, point, mark

define constant $point-pdl-size :: <integer> = 16;

define sealed method push-point-pdl!
    (window :: <basic-window>, bp :: <basic-bp>,
     #key display-message? = #t) => ()
  let line   = bp-line(bp);
  let index  = bp-index(bp);
  let buffer = bp-buffer(bp);
  let bp = make(<bp>, line: line, index: index, buffer: buffer);
  let pdl = window-point-pdl(window);
  if (size(pdl) = $point-pdl-size)
    kill-bp!(last(pdl));
    let last-pair = begin
		      let last-pair = pdl;
		      for (i :: <integer> from 0 below size(pdl) - 3)
			last-pair := tail(last-pair)
		      end;
		      last-pair
		    end;
    push!(window-point-pdl(window), bp);
    tail(last-pair) := list(head(tail(last-pair)))
  else
    push!(window-point-pdl(window), bp);
  end;
  when (display-message?)
    display-message(window, "Cursor position saved")
  end
end method push-point-pdl!;

define sealed method pop-point-pdl!
    (window :: <basic-window>)
 => (bp :: false-or(<basic-bp>))
  unless (empty?(window-point-pdl(window)))
    pop!(window-point-pdl(window))
  end
end method pop-point-pdl!;


/// Window buffer selection

define sealed class <buffer-selection> (<object>)
  sealed constant slot selection-buffer :: <basic-buffer>,
    required-init-keyword: buffer:;
  sealed slot selection-point :: false-or(<basic-bp>) = #f;
  sealed slot selection-mark  :: false-or(<basic-bp>) = #f;
  sealed slot selection-initial-line :: false-or(<basic-line>) = #f;
  sealed slot selection-goal-x-position :: false-or(<integer>) = #f;
end class <buffer-selection>;

define sealed domain make (singleton(<buffer-selection>));
define sealed domain initialize (<buffer-selection>);

define sealed method select-buffer
    (window :: <basic-window>, buffer :: <basic-buffer>) => ()
  // Locate the new and old buffers in the buffer history
  let frame   = window-frame(window);
  let buffers = window-selected-buffers(window);
  let new-buffer = buffer;
  let new-entry  = find-value(buffers, method (s) selection-buffer(s) == new-buffer end);
  let new-mode   = buffer-major-mode(new-buffer);
  let old-buffer = window-buffer(window);
  let old-entry  = find-value(buffers, method (s) selection-buffer(s) == old-buffer end);
  let old-mode   = old-buffer & buffer-major-mode(old-buffer);
  // Update the buffer history
  unless (buffer == window-buffer(window))		// maybe avoid some work
    when (new-entry)
      remove!(buffers, new-entry)
    end;
    when (old-entry)
      let old-entry :: <buffer-selection> = old-entry;	// force tighter type...
      selection-point(old-entry) := window-point(window);
      selection-mark(old-entry)  := ~window-temporary-mark?(window) & window-mark(window);
      selection-initial-line(old-entry)    := window-initial-line(window);
      selection-goal-x-position(old-entry) := window-goal-x-position(window)
    end;
    let new-entry :: <buffer-selection>
      = new-entry | make(<buffer-selection>, buffer: new-buffer);
    initialize-redisplay-for-buffer(window, new-buffer,
				    point:  selection-point(new-entry),
				    mark:   selection-mark(new-entry),
				    line:   selection-initial-line(new-entry),
				    goal-x: selection-goal-x-position(new-entry));
    set-scroll-position(window, 0, #f);
    push(buffers, new-entry)
  end;
  // Now select the new buffer
  // We do this regardless of whether we think it changed,
  // just in case something funny is going on...
  frame-buffer(frame)   := new-buffer;
  window-buffer(window) := new-buffer;
  *buffer* := new-buffer;
  unless (new-mode == old-mode)
    window-note-mode-entered(window, new-mode)
  end;
  window-note-buffer-selected(window, new-buffer)
end method select-buffer;

// This gives client a chance to implement alternate strategies for
// mapping buffers to windows in which they are shown.  Note that this
// can involve the creation of another window in another thread, so
// doing something like calling 'move-point!' directly after calling
// 'select-buffer-in-appropriate-window' may not work, because the
// binding of *buffer* may apply to the wrong thread.  Beware!
define method select-buffer-in-appropriate-window
    (window :: <window>, buffer :: <buffer>, #key line, index = 0) => ()
  select-buffer(window, buffer);
  when (line)
    move-point!(line, index: index, window: window)
  end;
  queue-redisplay(window, $display-all)
end method select-buffer-in-appropriate-window;
