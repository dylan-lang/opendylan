Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Dylan mode

//--- We should really have <dylan-syntax-mixin> that gets used by 
//--- both <dylan-mode> and <dylan-shell-mode>
define open class <dylan-mode> (<language-mode>)
end class <dylan-mode>;

define method initialize-major-mode
    (mode :: <dylan-mode>, #key command-set = mode-command-set(mode)) => ()
  next-method();
  let command-set = install-dylan-mode-bindings(mode, command-set);
  // Install a few more things for Dylan mode only
  let nothing = 0;
  let shift   = $shift-key;
  let control = $control-key;
  let meta    = $meta-key;
  let control+shift = logior($control-key, $shift-key);
  select (command-set-name(command-set))
    #"emacs" =>
      let command-table = standard-command-table(command-set);
      add-commands!(command-table,
		    vector('C', control, evaluate-region),
		    vector('C', meta,    evaluate-buffer),
		    vector('M', control, macroexpand-region),
		    vector(#"f7",  nothing,       build-project),
		    vector(#"f7",  control,       clean-build-project),
		    vector(#"f7",  control+shift, evaluate-region));
      let command-table = control-X-command-table(command-set);
      add-commands!(command-table,
		    vector('c', control, build-project),
		    vector('c', nothing, clean-build-project));
      let command-table = escape-command-table(command-set);
      add-commands!(command-table,
		    vector('C', nothing, evaluate-buffer));
    #"windows" =>
      let command-table = standard-command-table(command-set);
      add-commands!(command-table,
		    vector(#"f7",  nothing,       build-project),
		    vector(#"f7",  control,       clean-build-project),
		    vector(#"f7",  control+shift, evaluate-region));
    otherwise =>
      #[];
  end
end method initialize-major-mode;

define method install-dylan-mode-bindings
    (mode :: <major-mode>, command-set :: <standard-command-set>)
 => (command-set :: <standard-command-set>)
  let nothing = 0;
  let shift   = $shift-key;
  let control = $control-key;
  let meta    = $meta-key;
  let control+meta  = logior($control-key, $meta-key);
  let control+shift = logior($control-key, $shift-key);
  let meta+shift    = logior($meta-key, $shift-key);
  let command-set   = copy-command-set(command-set);
  mode-command-set(mode) := command-set;
  select (command-set-name(command-set))
    #"emacs" =>
      let command-table = standard-command-table(command-set);
      add-commands!(command-table,
		    vector('f', control+meta, forward-expression),
		    vector('b', control+meta, backward-expression),
		    vector('u', control+meta, up-expression),
		    vector('d', control+meta, down-expression),
		    vector('a', control+meta, start-of-definition),
		    vector('e', control+meta, end-of-definition),
		    vector(#"right", meta,    forward-expression),
		    vector(#"left",  meta,    backward-expression),
		    vector(#"up",    meta,    up-expression),
		    vector(#"down",  meta,    down-expression), 
		    vector(#"prior", meta,    start-of-definition), 
		    vector(#"next",  meta,    end-of-definition),
		    vector(#"delete",    control+meta, delete-expression),
		    vector(#"backspace", control+meta, rubout-expression),
		    vector('k', control+meta, delete-expression),
		    vector('t', control+meta, transpose-expressions),
		    vector(';',    control,    insert-comment),
		    vector(#"tab", control,    insert-tab),
		    vector(#"tab", nothing,    indent-line),
		    vector('\\', control+meta, indent-region),
		    vector('q',  control+meta, indent-expression),
		    vector(')', meta,         dylan-insert-block-end),
		    vector('.', meta,         edit-definition),
		    vector('i', control+meta, complete-name),
		    vector('A', control,      show-arglist),
		    vector('D', control,      show-documentation),
		    vector('V', control,      describe-object),
		    vector('O', control+meta, browse-object),
		    vector('C', control+meta, browse-class),
		    //---*** Clashes with Debugger's stepping bindings
		    //---*** vector(#"f12", nothing,   complete-name),
		    vector(#"f2", nothing,    browse-object),
		    vector(#"f2", control,    browse-class),
		    vector(#"f2", shift,      edit-definition));
      let command-table = control-X-command-table(command-set);
      add-commands!(command-table,
		    vector(';', control,      comment-region));
      let command-table = escape-command-table(command-set);
      add-commands!(command-table,
		    vector('f', control,      forward-expression),
		    vector('b', control,      backward-expression),
		    vector('u', control,      up-expression),
		    vector('d', control,      down-expression),
		    vector('a', control,      start-of-definition),
		    vector('e', control,      end-of-definition),
		    vector(#"delete",    control, delete-expression),
		    vector(#"backspace", control, rubout-expression),
		    vector('k', control,      delete-expression),
		    vector('t', control,      transpose-expressions),
		    vector('\\', control,     indent-region),
		    vector('q',  control,     indent-expression),
		    vector(')', nothing,      dylan-insert-block-end),
		    vector('.', nothing,      edit-definition),
		    vector('i', control,      complete-name),
		    vector('O', control,      browse-object),
		    vector('C', control,      browse-class));
    #"windows" =>
      let command-table = standard-command-table(command-set);
      add-commands!(command-table,
		    vector(#"right", meta,    forward-expression),
		    vector(#"left",  meta,    backward-expression),
		    vector(#"up",    meta,    up-expression),
		    vector(#"down",  meta,    down-expression), 
		    vector(#"prior", meta,    start-of-definition), 
		    vector(#"next",  meta,    end-of-definition),
		    vector(#"tab",   nothing, indent-region),
		    vector('i',      control, insert-tab),
		    //---*** Clashes with Debugger's stepping bindings
		    //---*** vector(#"f12", nothing,   complete-name),
		    vector(#"f1",  meta,      show-documentation),
		    vector(#"f2", nothing,    browse-object),
		    vector(#"f2", control,    browse-class),
		    vector(#"f2", shift,      edit-definition));
    otherwise =>
      #[];
  end;
  command-set
end method install-dylan-mode-bindings;

begin
  gethash(*keyword->major-mode*,   #"dylan") := <dylan-mode>;
  gethash(*file-type->major-mode*, #"dylan") := <dylan-mode>
end;

define method mode-name
    (mode :: <dylan-mode>) => (name :: <byte-string>)
  "Dylan"
end method mode-name;

define method source-file-type
    (mode :: <dylan-mode>) => (file-type)
  #"dylan"
end method source-file-type;


/// Dylan sections

define sealed class <dylan-section> (<definition-section>)
  // The line in which the define word is contained
  sealed slot section-defining-line :: false-or(<basic-line>) = #f,
    init-keyword: defining-line:;
  sealed slot %signature :: false-or(<dylan-signature>) = #f;
end class <dylan-section>;

define sealed domain make (singleton(<dylan-section>));
define sealed domain initialize (<dylan-section>);

define sealed inline method section-definition-signature
    (section :: <dylan-section>) => (signature :: false-or(<dylan-signature>))
  section.%signature | compute-section-signature(section)
end method section-definition-signature;

define sealed method section-definition-name
    (section :: <dylan-section>) => (name :: false-or(<string>))
  let signature = section-definition-signature(section);
  signature & signature-name(signature)
end method section-definition-name;

define sealed method section-definition-type
    (section :: <dylan-section>) => (type :: false-or(<symbol>))
  let signature = section-definition-signature(section);
  signature & signature-type(signature)
end method section-definition-type;

define method do-note-line-changed
    (mode :: <dylan-mode>, line :: <basic-line>) => (line :: <basic-line>)
  let section = line-section(line);
  when (section & line == section-defining-line(section))
    section.%signature := #f;
    // Make each window redisplay the Dylan definition line for this section
    do-associated-windows (window :: <basic-window> = *editor-frame*)
      let buffer = window-buffer(window);
      when (buffer & buffer-contains-section?(buffer, section))
        let definition-line = section-start-line(section);
        when (structural-diagram-line?(definition-line))
	  line-modification-tick(definition-line) := tick();
	  queue-redisplay(window, $display-line,
			  line: definition-line, index: 0)
	end
      end
    end
  end;
  next-method()
end method do-note-line-changed;


/// Dylan definition nodes

define sealed class <dylan-definition-node> (<definition-node>)
end class <dylan-definition-node>;

define sealed domain make (singleton(<dylan-definition-node>));
define sealed domain initialize (<dylan-definition-node>);

define sealed class <dylan-header-node> (<header-node>)
  sealed slot node-module-name :: false-or(<byte-string>) = #f,
    init-keyword: module-name:;
end class <dylan-header-node>;

define sealed domain make (singleton(<dylan-header-node>));
define sealed domain initialize (<dylan-header-node>);


/// Dylan "divider lines"

// A divider line that appears at the beginning of each Dylan section,
// except for the header section
define sealed class <dylan-definition-line> (<structural-diagram-line>)
end class <dylan-definition-line>;

define sealed domain make (singleton(<dylan-definition-line>));
define sealed domain initialize (<dylan-definition-line>);

define variable $dylan-definition-line-color = make-color(150, 150, 150);

define constant <section-separator-style> = one-of(#"always", #"requested", #"never");

define open generic buffer-section-separator-style
    (buffer :: <buffer>) => (style :: <section-separator-style>);

define method buffer-section-separator-style
    (buffer :: <buffer>) => (style :: <section-separator-style>)
  #"requested"
end method buffer-section-separator-style;

define sealed method window-hide-section-separators?
    (window :: <basic-window>) => (hide? :: <boolean>)
  let buffer = window-buffer(window);
  let style  = buffer-section-separator-style(buffer);
  select (style)
    #"always"    => #f;
    #"never"     => #t;
    #"requested" =>
      let frame  = window-frame(window);
      let policy = editor-policy(frame-editor(frame));
      ~show-section-separators?(policy)
  end
end method window-hide-section-separators?;

// Note that this method is intentionally not on <dylan-mode> because
// it's possible to get <dylan-definition-line>s into non-Dylan buffers
define sealed method display-line
    (line :: <dylan-definition-line>, mode :: <major-mode>, window :: <basic-window>,
     x :: <integer>, y :: <integer>,
     #key start: _start :: <integer> = 0, end: _end, align-y = #"top") => ()
  ignore(_end);
  unless (window-hide-section-separators?(window))
    when (_start = 0)		// no icon on continuation lines
      let name  = section-definition-name(line-section(line));
      let font  = window-default-font(window);
      let color = $dylan-definition-line-color;
      // Make the definition line only as wide as the visible part of the screen
      let (width, height) = window-viewport-size(window);
      ignore(height);
      let xoff1    = 5;
      let xoff2    = 6;
      let (fw, fh, fa, fd) = font-metrics(window, font);
      ignore(fw, fd);
      let height   = fh;
      let baseline = fa;
      when (align-y == #"baseline")
	// Normalize the Y coordinate as though we are using 'align-y: #"top"'
	dec!(y, baseline)
      end;
      let line-y   = y + floor/(height, 2);
      if (name)
	// Here is the layout of the graphic we are drawing:
	//        xoff1 line-width xoff2 name-width xoff2 line-width xoff1
	//  |<-->|<--->|<-------->|<--->|<-------->|<--->|<-------->|<--->|
	//  0    x                                                        width
	let name-width = string-size(window, name, font: font);
	let line-width = max(0, floor/(width - x - name-width, 2) - xoff1 - xoff2);
	draw-line(window,
		  x + xoff1, line-y,
		  x + xoff1 + line-width, line-y,
		  thickness: 1, color: color);
	draw-line(window,
		  width - xoff1, line-y,
		  width - xoff1 - line-width, line-y,
		  thickness: 1, color: color);
	draw-string(window, name,
		    x + xoff1 + line-width + xoff2, y,
		    font: font, color: color, align-y: #"top")
      else
	draw-line(window,
		  x + xoff1, line-y,
		  x + width - xoff1, line-y,
		  thickness: 1, color: color);
      end
    end
  end
end method display-line;

// Same deal -- this method is intentionally not on <dylan-mode>
define sealed method line-size
    (line :: <dylan-definition-line>, mode :: <major-mode>, window :: <basic-window>,
     #key start: _start, end: _end)
 => (width :: <integer>, height :: <integer>, baseline :: <integer>)
  ignore(_start, _end);
  let (width, height) = window-viewport-size(window);
  if (window-hide-section-separators?(window))
    values(width, 0, 0)
  else
    // Definition lines are only as wide as the visible part of the screen
    ignore(height);
    let (fw, fh, fa, fd) = font-metrics(window, window-default-font(window));
    ignore(fw, fd);
    let height   = fh;
    let baseline = fa;
    values(width, height, baseline)
  end
end method line-size;


/// Presentations

define method do-cursor-at-position
    (mode :: <dylan-mode>, window :: <basic-window>, x :: <integer>, y :: <integer>) => (cursor)
  let over-icon? = x < $breakpoint-image-width;
  over-icon? & #"default"
end method do-cursor-at-position;

define method do-presentation-at-position
    (mode :: <dylan-mode>, window :: <basic-window>, x :: <integer>, y :: <integer>,
     #key button = $left-button, modifiers = 0, event-type = #"press")
 => (presentation :: false-or(<presentation>),
     bp :: false-or(<basic-bp>), move-point? :: <boolean>)
  let (bp, dline) = position->bp(window, x, y);
  let line = bp & bp-line(bp);
  let over-icon?  = line & x < $breakpoint-image-width;
  let dylan-line? = line & instance?(line, <dylan-definition-line>);
  let (sbp, ebp)  = bp & ~over-icon? & ~dylan-line? & atom-under-bp(bp);
  let menu? = gesture-matches?($menu-gesture, button, modifiers, event-type: event-type);
  case
    over-icon?
    & (menu?
       | (event-type == #"release"
	  & ~gesture-matches?($menu-gesture, button, modifiers))) =>
      // Mouse-right (press) on a breakpoint icon gets a presentation for a menu
      // Mouse-other (release) on a breakpoint icon also gets a presentation
      // Strictly speaking, we don't need to return #t as the third value,
      // but it makes the point-moving behavior more consistent...
      values(make(<presentation>,
		  object: line, type: <dylan-breakpoint>),
	     bp, #t);
    dylan-line?
    & (menu?
       | (event-type == #"double-click"
	  & gesture-matches?($move-gesture, button, modifiers))) =>
      // Mouse-right (press) on a dylan line gets a presentation for a menu
      values(make(<presentation>,
		  object: line, type: <dylan-definition-line>),
	     bp, #f);
    ~(sbp & ebp)
    | gesture-matches?($move-gesture, button, modifiers, event-type: event-type)
    | gesture-matches?($copy-gesture, button, modifiers, event-type: event-type) =>
      // Copy and move are handled at a lower level...
      values(#f, bp, #t);
    menu? & dline & x > display-line-width(dline) + 10 =>
      // Mouse-right (press) on blank area gets a presentation for a menu
      values(make(<presentation>,
		  object: #f, type: <blank-area>),
	     bp, #f);
    event-type == #"press" | event-type == #"release" & sbp & ebp =>
      // If it's a button press or release that is not a copy or move,
      // give it a presentation as well
      values(make(<presentation>,
		  object: make-interval(sbp, ebp), type: <dylan-atom>),
	     bp, ~menu?);	// don't move point if a menu is requested
    otherwise =>
      // Everything else just gets a raw BP
      values(#f, bp, #t);
  end
end method do-presentation-at-position;


define sealed class <dylan-breakpoint> (<object>)
end class <dylan-breakpoint>;

define sealed domain make (singleton(<dylan-breakpoint>));
define sealed domain initialize (<dylan-breakpoint>);

define variable $cycle-break-gesture :: <gesture>
    = make(<gesture>,
	   button: $left-button,  modifiers: 0, event-type: #"release");
define variable $cycle-trace-gesture :: <gesture>
    = make(<gesture>,
	   button: $left-button,  modifiers: $shift-key, event-type: #"release");
define variable $step-gesture :: <gesture>
    = make(<gesture>,
	   button: $left-button,  modifiers: $control-key, event-type: #"release");

define method do-handle-presentation-event
    (mode :: <dylan-mode>, window :: <basic-window>,
     line :: <basic-line>, type == <dylan-breakpoint>,
     #key bp, x, y, button = $left-button, modifiers = 0, event-type = #"press",
	  menu-function = dylan-breakpoint-menu) => ()
  let old-state = line-breakpoint?(mode, line);
  let menu? = gesture-matches?($menu-gesture, button, modifiers, event-type: event-type);
  when (old-state)
    case
      menu? =>
	// Pop up a menu of operations for the breakpoint
	menu-function(window, mode, line, bp: bp, x: x, y: y);
      gesture-matches?($cycle-break-gesture, button, modifiers, event-type: event-type) =>
	// Cycle between breakpoint states
	// Note that we don't ever cycle back to #"none", because
	// that could easily lose carefully constructed state
	let new-state = select (old-state)
			  #"none"           => #"enabled-break";
			  #"enabled-break"  => #"disabled-break";
			  #"disabled-break" => #"enabled-break";
			  otherwise   => #f;
			end;
	when (new-state)
	  line-breakpoint?(mode, line) := new-state
	end;
      gesture-matches?($cycle-trace-gesture, button, modifiers, event-type: event-type) =>
	let new-state = #"enabled-trace";
	line-breakpoint?(mode, line) := new-state;
      gesture-matches?($step-gesture, button, modifiers, event-type: event-type) =>
	let new-state = #"step";
	line-breakpoint?(mode, line) := new-state;
    end;
    queue-redisplay(window, $display-line, line: line, index: 0, centering: #f);
    redisplay-window(window)
  end
end method do-handle-presentation-event;

define method dylan-breakpoint-menu
    (window :: <basic-window>, mode :: <dylan-mode>, line :: <basic-line>,
     #key bp, x, y) => ()
  ignore(bp, x, y);
  // This is where DylanWorks mode calls out to the environment
  let new-state
    = choose-from-menu(window,
		       #[#["Clear",          #"none"],
			 #["Set Breakpoint", #"enabled-break"],
			 #["Step to here",   #"step"],
			 #["Set Tracepoint", #"enabled-trace"],
			 #["Set Profile",    #"profile"]],
		       label-key: first, value-key: second);
  when (new-state)
    line-breakpoint?(mode, line) := new-state
  end
end method dylan-breakpoint-menu;


define sealed class <dylan-atom> (<object>)
end class <dylan-atom>;

define sealed domain make (singleton(<dylan-atom>));
define sealed domain initialize (<dylan-atom>);

define method do-handle-presentation-event
    (mode :: <dylan-mode>, window :: <basic-window>,
     atom :: <basic-interval>, type == <dylan-atom>,
     #key bp, x, y, button = $left-button, modifiers = 0, event-type = #"press",
	  menu-function = dylan-atom-menu) => ()
  let menu? = gesture-matches?($menu-gesture, button, modifiers, event-type: event-type);
  case
    gesture-matches?($edit-gesture, button, modifiers, event-type: event-type) =>
      // Move the point so that the right definition is edited
      move-point!(bp, window: window);
      // This is where DylanWorks mode calls out to the environment
      #f;
    menu? =>
      // Pop up a menu of operations for the atom under the mouse
      with-temporary-selection (window, bp)
	menu-function(window, mode, atom, bp: bp, x: x, y: y)
      selecter
	select-atom-under-bp(window, bp);
      end;
    otherwise => #f;
  end
end method do-handle-presentation-event;

define method dylan-atom-menu
    (window :: <basic-window>, mode :: <dylan-mode>, atom :: <basic-interval>,
     #key bp, x, y) => ()
  ignore(atom, bp, x, y);
  let buffer = window-buffer(window);
  let module = instance?(buffer-start-node(buffer), <dylan-header-node>)
	       & node-module-name(buffer-start-node(buffer));
  // This is where DylanWorks mode calls out to the environment
  choose-from-menu(window,
		   #[#["Edit",         #"edit"],
		     #["Browse",       #"browse"],
		     #["Browse Class", #"browse-class"],
		     #["Properties",   #"properties"]],
		   label-key: first, value-key: second)
end method dylan-atom-menu;


define method do-handle-presentation-event
    (mode :: <dylan-mode>, window :: <basic-window>,
     line :: <dylan-definition-line>, type == <dylan-definition-line>,
     #key bp, x, y, button = $left-button, modifiers = 0, event-type = #"press",
	  menu-function = dylan-definition-line-menu) => ()
  let menu? = gesture-matches?($menu-gesture, button, modifiers, event-type: event-type);
  case
    menu? =>
      // Pop up a menu of operations for the atom
      menu-function(window, mode, line, bp: bp, x: x, y: y);
    otherwise => #f;
  end
end method do-handle-presentation-event;

define method dylan-definition-line-menu
    (window :: <basic-window>, mode :: <dylan-mode>, line :: <dylan-definition-line>,
     #key bp, x, y) => ()
  ignore(line, bp, x, y);
  // This is where DylanWorks mode calls out to the environment
  choose-from-menu(window,
		   #[#["Evaluate Definition", #"compile"],
		     #["Browse Definition",   #"browse"],
		     #["Properties",          #"properties"]],
		   label-key: first, value-key: second)
end method dylan-definition-line-menu;


/// Dylan sectionizer

/*
//---*** Remove when debugging is complete
define constant $debug-sectionizer? :: <boolean> = #f;
define constant $sectionizer-debug-message :: <function>
    = if ($debug-sectionizer?) debug-message else ignore end;
*/

// First break the source container into sections, then fill the
// the buffer with section nodes containing those sections
//--- Note that we just ignore the issue of long comments (/* ... */)
//---*** 'sectionize-buffer' methods should maintain identity of sections so
//---*** that other client nodes don't lose when the old sections disappear
define method do-sectionize-buffer
    (mode :: <dylan-mode>, buffer :: <basic-buffer>)
 => (sectionized? :: <boolean>)
  // First split the buffer's lines up into sections
  let container = buffer-source-container(buffer);
  let (sections, header-section, module-line)
    = split-lines-into-sections(mode, buffer, container);
  insert-at!(sections, header-section, 0);
  container-sections(container) := sections;
  // Then fill the buffer with section nodes
  buffer-start-node(buffer) := #f;
  buffer-end-node(buffer)   := #f;
  add-dylan-section-nodes!(buffer, sections,
			   header-section: header-section, module-line: module-line);
  /*
  when ($debug-sectionizer?)
    let start-bp   = interval-start-bp(buffer);
    let start-line = start-bp & bp-line(start-bp);
    let end-bp   = interval-end-bp(buffer);
    let end-line = end-bp & bp-line(end-bp);
    // If the following fails, you can add ", verbose?: #t" to get
    // some useful(?) diagnostics.
    dynamic-bind (*buffer* = buffer)
      assert(last-line-from(start-line, in-buffer?: #t, verbose?: #t) == end-line,
	     "Buffer line link structure incorrect after sectionize-buffer");
      // If the following fails, you can add ", verbose?: #t" to get
      // some useful(?) diagnostics.
      assert(check-buffer-bps(buffer, verbose?: #t),
	     "Buffer BPs incorrect after sectionize-buffer")
    end
  end;
  */
  #t
end method do-sectionize-buffer;

//---*** Implement this, make 'do-sectionize-buffer' use it
define method do-sectionize-container
    (mode :: <dylan-mode>, container :: <basic-source-container>)
 => (sectionized? :: <boolean>)
end method do-sectionize-container;

define method do-resectionize-section
    (mode :: <dylan-mode>, section :: <dylan-section>)
 => (resectionized? :: <boolean>)
  // The idea here is to examine the current section to see if it needs
  // to be split into two.  When we split, the existing section gets reused
  // for whichever part that has the same definition name.  The initial
  // part of the section might also need to be merged with the previous
  // section, but we never need to merge with the following section.
  //---*** This will completely fall over for containerless buffers
  let container    = section-container(section);
  let old-sections = container-sections(container);
  let old-index :: <integer> = position(old-sections, section);
  //---*** For debugging only...
  let section-buffers = map(node-buffer, section-nodes(section));
  let (sections, header-section, module-line)
    = split-lines-into-sections(mode, section, container);
  // We keep the header section separate for now, to make the following
  // merging easier.  The "screw case" is when we're resectionizing the
  // header of a buffer, and there are "extra" lines between it and the
  // first definition.
  let n-sections :: <integer>
    = size(sections) + (if (header-section) 1 else 0 end);
  assert(n-sections ~== 0,
	 "Resectionizing the section resulted in no sections");
  let resectionized? :: <boolean> = (n-sections > 1);
  local method assign-section
	    (first-line :: <basic-line>, section :: <basic-section>)
	  for (line = first-line then line-next(line),
	       until: ~line)
	    line-section(line) := section
	  end
	end method;
  // The 'sections' we've got back (not including the one we passed in) are
  // in a temporary container, so now we must move them into 'container'.
  // If the first new section, after any header, has no defining-line,
  // merge it into:
  //  (a) the section preceding 'section', unless that section is a
  //      header section (or 'header-section' is non-#f); otherwise...
  //  (b) the following section (the next in 'sections', or the section
  //      following 'section'), if there is one; otherwise...
  //  (c) uh, don't bother merging it, just exit this loop.
  // Loop until the new first section after merging has a defining-line.
  //--- Can we go around this loop more than once?
  let first-section :: false-or(<basic-section>)
    = element(sections, 0, default: #f);
  block (return)
    while (first-section & ~section-defining-line(first-section))
      local method merge-sections!
		(section1 :: <basic-section>, section2 :: <basic-section>,
		 into :: <basic-section>) => ()
	      let (start1, end1, start2, end2)
		= values(section-start-line(section1), section-end-line(section1),
			 section-start-line(section2), section-end-line(section2));
	      let from :: <basic-section>
		= if (into == section1) section2 else section1 end;
	      assert(start1 & end1 & start2 & end2,
		     "There's an empty section while merging sections");
	      // Move all the lines in 'from' to 'into'
	      assign-section(section-start-line(from), into);
	      line-next(end1)       := start2;
	      line-previous(start2) := end1;
	      section-start-line(into) := start1;
	      section-end-line(into)   := end2;
	      // Update the start/end BPs of any nodes using these sections
	      for (node :: <basic-node> in section-nodes(section1))
		move-bp!(interval-end-bp(node), end2, line-length(end2));
		node-section(node) := into
	      end;
	      for (node :: <basic-node> in section-nodes(section2))
		move-bp!(interval-start-bp(node), start1, line-length(start1));
		node-section(node) := into
	      end;
	      into.%n-lines := #f;
	    end method;
      if (old-index ~== 1 & ~header-section)
	// Case (a): merge into the preceding section
	// 'old-index ~== 1' ensures we don't merge into the header section
	let into :: <basic-section> = old-sections[old-index - 1];
	merge-sections!(into, first-section, into);
	remove!(sections, first-section)
      else
	let next-section
	  = element(sections, 1, default: #f)
	    | element(old-sections, old-index + 1, default: #f);
	if (next-section)
	  // Case (b): merge into the following section
	  let next-section-start-line = section-start-line(next-section);
	  let dylan-definition-line
	    = if (instance?(next-section-start-line, <dylan-definition-line>))
		remove-line!(next-section, next-section-start-line);
		//---*** Following line is temporary until 'remove-line!' does it
		line-section(next-section-start-line) := #f;
		next-section-start-line
	      else
		#f
	      end;
	  merge-sections!(first-section, next-section, next-section);
	  when (dylan-definition-line)
	    add-line!(next-section, dylan-definition-line, after: #"start")
	  end;
	  remove!(sections, first-section)
	else
	  // Case (c): there's no possible following section, so don't merge
	  return()
	end
      end;
      first-section := element(sections, 0, default: #f)
    end while
  end block;
  // Now insert the header with the rest of the sections.
  when (header-section)
    insert-at!(sections, header-section, 0)
  end;
  let first-section = element(sections, 0, default: #f);
  if (first-section)
    // Assign all the lines in the first new section (NB: which new
    // section is first may just have changed) to the original 'section',
    // effectively "overwriting" the lines which were there before.
    let start-line :: <basic-line> = section-start-line(first-section);
    let end-line   :: <basic-line> = section-end-line(first-section);
    section-start-line(section) := start-line;
    section-end-line(section)   := end-line;
    // 'first-section' was newly created by 'split-lines-into-sections', so
    // it doesn't yet have any section nodes for us to worry about.
    // Update the start and end BPs of any nodes using the existing section
    for (node :: <basic-node> in section-nodes(section))
      move-bp!(interval-start-bp(node), start-line, 0);
      move-bp!(interval-end-bp(node),   end-line,   line-length(end-line))
    end;
    //---*** What about any moving BPs?
    assign-section(section-start-line(first-section), section);
    section-defining-line(section) := section-defining-line(first-section);
    section.%signature := first-section.%signature;
    section.%n-lines   := #f;
    //---*** Fix up 'window-initial-line' like 'remove-line!' does?
    remove!(sections, first-section);
    // Remove the remaining sections from their temporary container
    // and add them into the existing container, after 'section'.  Also
    // add nodes to every buffer where the original section was visible.
    let first-section = element(sections, 0, default: #f);
    when (first-section)
      let after-section :: <basic-section> = section;
      for (section :: <basic-section> in sections)
	section-container(section) := #f;	// this is how we remove it...
	add-section!(container, section, after: after-section);
	after-section := section
      end;
      for (node :: <basic-node> in section-nodes(section))
	let buffer = node-buffer(node);
	when (buffer)
	  add-dylan-section-nodes!(buffer, sections,
				   after: section-node(section, buffer: buffer))
	end
      end
    end
  else
    // The contents of the original 'section' have all been merged into
    // other sections, so we must dispose of 'section'.
    remove-section!(section-container(section), section);
    for (node :: <basic-node> in section-nodes(section))
      let buffer = node-buffer(node);
      when (buffer)
	remove-node!(buffer, node)
      end
    end
  end;
  /*
  when ($debug-sectionizer?)
    for (buffer :: false-or(<basic-buffer>) in section-buffers)
      when (buffer)
	let start-bp   = interval-start-bp(buffer);
	let start-line = start-bp & bp-line(start-bp);
	let end-bp     = interval-end-bp(buffer);
	let end-line   = end-bp & bp-line(end-bp);
	// If the following fails, you can add ", verbose?: #t" to get
	// some useful(?) diagnostics.
	dynamic-bind (*buffer* = buffer)
	  assert(last-line-from(start-line, in-buffer?: #t, verbose?: #t) == end-line,
		 "Buffer line link structure incorrect after resectionize-section");
	  // If the following fails, you can add ", verbose?: #t" to get
	  // some useful(?) diagnostics.
	  assert(check-buffer-bps(buffer, verbose?: #t),
		 "Buffer BPs incorrect after sectionize-section")
        end
      end
    end
  end;
  */
  resectionized?
end method do-resectionize-section;


define variable *show-dylan-definition-lines?* :: <boolean> = #t;

define method split-lines-into-sections
    (mode :: <dylan-mode>, line-source :: type-union(<basic-buffer>, <basic-section>),
     container :: <basic-source-container>)
 => (sections :: <stretchy-object-vector>,
     header-section :: false-or(<basic-section>), module-line :: false-or(<basic-line>))
  let sectionizing-buffer? :: <boolean>
    = instance?(line-source, <buffer>);
  let starts-in-header? :: <boolean>
    = sectionizing-buffer? | (line-source == first(container-sections(container)));
  /*
  $sectionizer-debug-message
    ("sectionizing-buffer? == %=; starts-in-header? == %=",
     sectionizing-buffer?, starts-in-header?);
  */
  unless (sectionizing-buffer?)
    assert(section-container(line-source) == container,
	   "The section %= is not in the correct container", line-source)
  end;
  // We assume that the lines in the interval are all in the same section,
  // in which case the container provided should contain that section, and
  // we will create our new sections in a temporary container, and move them
  // back to the existing container afterwards.  Or, we are to ignore which
  // section the lines are in, in which case we put them in new sections
  // which have the given container (without needing to be moved).
  unless (sectionizing-buffer?)
    container := make(<flat-file-source-container>,
		      pathname: "*temporary dylan container*")
  end;
  // First pick out the header and make a header section, collecting
  // the remaining lines for use downstream.
  let sections     :: <stretchy-object-vector> = make(<stretchy-vector>);
  let lines        :: <stretchy-object-vector> = make(<stretchy-vector>);
  let header-lines :: <stretchy-object-vector> = make(<stretchy-vector>);
  let header-section :: false-or(<basic-section>) = #f;
  let module-line    :: false-or(<basic-line>) = #f;
  // 'collect' will be bound to 'collect-header-line' then later 'collect-line'
  let collect :: <function> = identity;
  local method collect-line (line :: <line>)
	  add!(lines, line)
	end method,
        method collect-header-line (line :: <line>)
	  add!(header-lines, line);
	  when (text-line?(line))	// no funny stuff in the header, please
	    when (line-length(line) = 0)
	      collect := collect-line;
	      /*
	      $sectionizer-debug-message
		("Header lines:\n%=",
		 if (empty?(header-lines)) "_None_"
		 else reduce1(concatenate, map(curry(as, <string>), header-lines))
		 end);
	      */
	    end
	  end
	end method;
  collect := if (starts-in-header?) collect-header-line else collect-line end;
  // We have to make sure that we catch and safely dispose of any
  // <dylan-definition-line>s, because node start/end BPs may point to them
  let old-definition-lines :: <stretchy-object-vector> = make(<stretchy-vector>);
  do-lines(method (line, si, ei, last?)
	     ignore(si, ei, last?);
	     if (instance?(line, <dylan-definition-line>))
	       // Must remove these _later_, or we may confuse 'do-lines'
	       add!(old-definition-lines, line)
	     else
	       // 'collect' is either 'collect-header-line' or 'collect-line'
	       collect(line)
	     end
	   end method, line-source,
	   skip-test: #f);
  for (line :: <dylan-definition-line> in old-definition-lines)
    remove-line!(line-section(line), line);
    /*
    $sectionizer-debug-message("Discarded definition-line")
    */
  end;
  /*
  $sectionizer-debug-message
    ("Lines:\n%=",
     if (empty?(lines)) "_None_"
     else reduce1(concatenate, map(curry(as, <string>), lines)) end);
  */
  local method assign-section 
	    (lines :: <stretchy-object-vector>,
	     start-index :: <integer>, end-index :: <integer>,
	     section :: <basic-section>)
	  let prev-line :: false-or(<basic-line>) = #f;
	  for (i :: <integer> from start-index to end-index)
	    let line :: <basic-line> = lines[i];
	    when (prev-line)
	      line-next(prev-line) := line
	    end;
	    line-previous(line) := prev-line;
	    line-section(line)  := section;
            prev-line := line
	  end;
	  line-next(lines[end-index]) := #f
	end method;
  unless (empty?(header-lines))
    let si :: <integer> = 0;
    let ei :: <integer> = size(header-lines) - 1;
    let start-line :: <basic-line> = header-lines[si];
    let end-line   :: <basic-line> = header-lines[ei];
    module-line := find-module-line(header-lines);
    let dylan-section :: <basic-section>
      = make(<dylan-section>,
	     container:  container,
	     start-line: start-line,
	     end-line:   end-line,
	     defining-line: first(header-lines));
    header-section := dylan-section;
    assign-section(header-lines, si, ei, header-section);
    /*
    $sectionizer-debug-message
      ("Header section:\n%=\n", as(<string>, header-section))
    */
  end;
  unless (empty?(lines))
    // Now sectionize the rest of the lines in the file as follows:
    //  - Set _this-start_ to the first 'define' form
    //  - Loop doing
    //    - Set _next-start_ to the next 'define' form
    //    - Back up from _next-start_ to the last non-blank,
    //      non-comment line, setting _this-end_
    //    - The section runs from _prev-end_ to _this-end_,
    //      with the defining line at _this-start_
    //    - Set _this-start_ to _next-start_, and _prev-end_
    //      to _this-end_, and loop
    local method collect-section
	      (si :: <integer>, ei :: <integer>, defining-line :: false-or(<basic-line>))
	   => (section :: <basic-section>)
	    let start-line :: <basic-line> = lines[si];
	    let end-line   :: <basic-line> = lines[ei];
	    let section = make(<dylan-section>,
			       container:  container,
			       start-line: start-line,
			       end-line:   end-line,
			       defining-line: defining-line);
	    assign-section(lines, si, ei, section);
	    add!(sections, section);
	    section
	  end method;
    block (break)
      let prev-end   = -1;
      let this-start = find-defining-line(lines, 0);
      let final = size(lines) - 1;
      if (~this-start)
	// Only one section in the rest of the interval
	let section = collect-section(0, final, #f);
	/*
	$sectionizer-debug-message
	  ("Only one, non-definition section:\n%=\n",
	   as(<string>, section));
	*/
	break()
      else
	when (~sectionizing-buffer? & this-start >= 1)
	  // Grab any initial "extra" lines by looking backwards from
	  // this-start with 'find-non-blank-line'.  They get their own
	  // section with no defining-line, but they'll be moved later,
	  // in 'do-resectionize-section'.
	  let this-end = find-non-blank-line(lines, this-start - 1);
	  when (this-end)
	    let section = collect-section(0, this-end, #f);
	    /*
	    $sectionizer-debug-message
	      ("'Extra' section:\n%=\n", as(<string>, section))
	    */
	  end;
	  prev-end := this-end | -1
	end;
	while (#t)
	  let next-start = find-defining-line(lines, this-start + 1);
	  let this-end   = if (next-start) find-non-blank-line(lines, next-start - 1)
			   else final end;
	  when (this-end = next-start)
	    dec!(this-end)		// two definitions jammed back-to-back
	  end;
	  let section = collect-section(prev-end + 1, this-end, lines[this-start]);
	  // Add section header line if requested
	  when (*show-dylan-definition-lines?*)
	    let diagram = make(<dylan-definition-line>);
	    add-line!(section, diagram, after: #"start")
	  end;
	  /*
	  $sectionizer-debug-message
	    ("Next section:\n%=\n", as(<string>, section));
	  */
	  if (next-start)
	    this-start := next-start;
	    prev-end   := this-end
	  else
	    break()
	  end
	end
      end
    end
  end;
  // Note that 'sections' does not contain 'header-section'
  values(sections, header-section, module-line)
end method split-lines-into-sections;

define method add-dylan-section-nodes!
    (buffer :: <basic-buffer>, sections :: <stretchy-object-vector>,
     #key header-section :: false-or(<section>), module-line :: false-or(<line>),
	  after :: false-or(<node>)) => ()
  for (section :: <basic-section> in sections)
    let start-bp
      = make(<bp>,
	     line: section-start-line(section), index: 0,
	     buffer: buffer);
    let end-bp
      = make(<bp>,
	     line: section-end-line(section), index: line-length(section-end-line(section)),
	     buffer: buffer,
	     moving?: #t);
    let node
      = if (section == header-section)
	  let module-name
	    = module-line
	      & trim-whitespace(copy-sequence(line-contents(module-line), start: 8));
	  make(<dylan-header-node>,
	       start-bp: start-bp, end-bp: end-bp,
	       section:  header-section,
	       module-name: module-name & as-lowercase(module-name))
	else
	  make(<dylan-definition-node>,
	       start-bp: start-bp, end-bp: end-bp,
	       section:  section)
	end;
    // Our caller may be adding this section to more than one buffer by
    // calling us repeatedly, so we must preserve existing section-nodes
    push!(section-nodes(section), node);
    if (after)
      add-node!(buffer, node, after: after);
      after := node
    else
      add-node!(buffer, node)
    end
  end
end method add-dylan-section-nodes!;


/*
//---*** Remove when debugging is complete
define function last-line-from
    (line :: <line>,
     #key in-buffer? :: <boolean> = #f, verbose? :: <boolean> = #f)
 => (last-line :: false-or(<line>))
  // Follow the line-next chain from line, until we hit #f,
  // returning the last line we saw.  Also check that
  // previous(next(line)) == line (when next(line) ~== #f), i.e. that
  // next and previous pointers match up.
  let (next, previous)
    = if (in-buffer?)
        values(rcurry(line-next-in-buffer, *buffer*),
               rcurry(line-previous-in-buffer, *buffer*))
      else
        values(line-next, line-previous)
      end;
  block(return)
    for (this = line then next(this),
	 until: ~next(this))
      when (verbose?)
	$sectionizer-debug-message("this = %=", line-contents(this));
      end;
      unless(previous(next(this)) == this)
        when (verbose?)
          $sectionizer-debug-message
            ("this.next = %=, this.next.previous = %=",
             line-contents(this.next), line-contents(this.next.previous));
        end when;
        return(#f);
      end;
    finally this
    end for
  end
end function;

//---*** Remove when debugging is complete
define function check-buffer-bps
    (buffer :: <basic-buffer>, #key verbose? :: <boolean> = #f)
 => (okay? :: <boolean>)
  block(return)
    // Check that the node-{start,end}-bps are indeed at the start/end
    // of lines which have no line-{previous,next}.
    for (node = buffer-start-node(buffer) then node-next(node),
         until: ~node)
      when (verbose?)
        $sectionizer-debug-message("node: %=", node.node-definition-name);
      end when;
      let start-bp = interval-start-bp(node);
      let start-line = bp-line(start-bp);
      let start-index = bp-index(start-bp);
      let end-bp = interval-end-bp(node);
      let end-line = bp-line(end-bp);
      let end-index = bp-index(end-bp);
      unless
          ( ~line-previous(start-line) & start-index == 0
          & ~line-next(end-line) & end-index == line-length(end-line)
          & ~moving-bp?(start-bp) & moving-bp?(end-bp)
          & ~simple-bp?(start-bp) & ~simple-bp?(end-bp))
        when (verbose?)
          ~line-previous(start-line)
            | $sectionizer-debug-message("start line has a previous line");
          start-index == 0
            | $sectionizer-debug-message("start index not == 0");
          ~line-next(end-line)
            | $sectionizer-debug-message("end line has a next line");
          end-index == line-length(end-line)
            | $sectionizer-debug-message
                ("end index %= not == line length %=", end-index,
                 line-length(end-line));
          ~moving-bp?(start-bp)
            | $sectionizer-debug-message("start BP is a <moving-bp>");
          moving-bp?(end-bp)
            | $sectionizer-debug-message("end BP is not a <moving-bp>");
          ~simple-bp?(start-bp)
            | $sectionizer-debug-message("start BP is not a <permanent-bp>");
          ~simple-bp?(end-bp)
            | $sectionizer-debug-message("end BP is not a <permanent-bp>");
        end when;
        return(#f);
      end unless;
    end for;

    // Check that all line-bps are moving-bps, that they're on the
    // right line and that their indices are in range for the line
    // they're on.
    do-lines
      (method (line, si, ei, last?)
         ignore(si, ei, last?);
         for (bp :: <basic-bp> in line-bps(line))
           let _line = bp-line(bp);
           let _index = bp-index(bp);
           unless (moving-bp?(bp) & _line == line
                     & 0 <= _index & _index <= line-length(line))
             when (verbose?)
               moving-bp?(bp)
                 | $sectionizer-debug-message("line BP is not a <moving-bp>");
               _line == line
                 | $sectionizer-debug-message("line BP is on wrong line");
               0 <= _index
                 | $sectionizer-debug-message("line BP has index < 0");
               _index <= line-length(line)
                 | $sectionizer-debug-message("line BP has index > line length");
             end when;
             return(#f);
           end unless;
         end for;
       end method, buffer);

    // If we got this far, everything's okay.
    #t
  end;
end function;
*/

// Search forward for the "Module:" line
define method find-module-line
    (header-lines :: <vector>) => (module-line :: false-or(<basic-line>))
  block (return)
    for (line in header-lines)
      when (text-line?(line))
	let length   = line-length(line);
	let contents = line-contents(line);
	when (length > 7  & string-equal?(contents, "Module:", end1: 7))
	  return(line)
	end
      end
    end;
    values(#f, #f)
  end
end method find-module-line;

// Search forward for a line starting with 'define' or 'begin',
// returning the index of that line
define method find-defining-line
    (lines :: <vector>, index :: <integer>) => (line :: false-or(<integer>))
  block (return)
    let n-lines :: <integer> = size(lines);
    for (i :: <integer> from index below n-lines)
      let line = lines[i];
      when (text-line?(line))
	let length   = line-length(line);
	let contents = line-contents(line);
	/*
	$sectionizer-debug-message
	  ("Seeking define line at line %=/%=: %=", i, n-lines, contents);
	*/
	when (  (length > 6 & string-equal?(contents, "define", end1: 6))
		// '>=' because 'begin' might stand alone on this line
	      | (length >= 5 & string-equal?(contents, "begin", end1: 5)))
	  /*
	  $sectionizer-debug-message
	    ("Found define line at line %=/%=", i, n-lines);
	  */
	  return(i)
	end
      end
    end;
    #f
  end
end method find-defining-line;

// Search backward for a non-blank, non-comment line,
// returning the index of the following line
define method find-non-blank-line
    (lines :: <vector>, index :: <integer>) => (line :: false-or(<integer>))
  block (return)
    while (index >= 0)
      let line = lines[index];
      when (text-line?(line))
	let length   = line-length(line);
	let contents = line-contents(line);
	let bp = find-key(contents,
			  method (ch) ~whitespace-char?(ch) end, failure: length);
	when (length - bp > 2
	      & ~string-equal?(contents, "//", start1: bp, end1: bp + 2))
	  return(index + 1)
        end
      end;
      index := index - 1
    end;
    #f
  end
end method find-non-blank-line;


/// Dylan parsing

define sealed method do-relevant-function-interval
    (mode :: <dylan-mode>, bp :: <basic-bp>)
 => (interval :: false-or(<basic-interval>))
  let node = bp-node(bp) | bp-buffer(bp);
  // First try to find the atom right under the point
  let sbp = if (atom-syntax(bp-character-before(bp)) == $atom-delimiter)
              forward-over(bp, #[' ', '\t', '\f'], interval: node)
            else
	      move-over-atoms(bp, -1, interval: node)
            end;
  let ebp = move-over-atoms(sbp, 1, interval: node);
  if (bp-character-before(sbp) = '.')
    make-interval(sbp, ebp, in-order?: #t)
  else
    let lbp = move-over-lists(move-over-lists(ebp, 1, interval: node), -1, interval: node);
    unless (bp-character(lbp) = '(')
      // If that atom doesn't start a function call, move backwards until
      // we find what might be a function call, and pick _that_ atom
      sbp := decrement-bp!(backward-until(sbp, #['('], interval: node));
      sbp := move-over-atoms(sbp, -1, interval: node);
      ebp := move-over-atoms(sbp, 1, interval: node)
    end;
    make-interval(sbp, ebp, in-order?: #t)
  end
end method do-relevant-function-interval;


define method do-atom-under-bp
    (mode :: <dylan-mode>, bp :: <basic-bp>)
 => (sbp :: <basic-bp>, ebp :: <basic-bp>)
  let node = bp-node(bp) | bp-buffer(bp);
  let sbp  = if (atom-syntax(bp-character-before(bp)) == $atom-delimiter)
	       forward-over(bp, #[' ', '\t', '\f'], interval: node)
	     else
	       move-over-atoms(bp, -1, interval: node)
	     end;
  let ebp  = move-over-atoms(sbp, 1, interval: node);
  // Some characters are magic at the beginning of an atom
  // Skip them, unless the user is right on top of one
  when (bp ~= sbp
	& member?(bp-character(sbp), #['+', '-', '~', '?', '/']))
    increment-bp!(sbp)
  end;
  values(sbp, ebp)
end method do-atom-under-bp;

define sealed method do-move-over-expressions
    (mode :: <dylan-mode>, bp :: <basic-bp>, n :: <integer>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  //---*** Do this for real
  move-over-lists(bp, n, fixup?: fixup?, interval: interval)
end method do-move-over-expressions;

define sealed method do-move-up-or-down-expressions
    (mode :: <dylan-mode>, bp :: <basic-bp>, n :: <integer>,
     #key fixup? = #t, interval = bp-buffer(bp))
 => (bp :: false-or(<basic-bp>))
  //---*** Do this for real
  move-up-or-down-lists(bp, n, fixup?: fixup?, interval: interval)
end method do-move-up-or-down-expressions;


/// Dylan signatures

define sealed method compute-file-signatures
    (pathname :: <pathname>, #key editor = frame-editor(*editor-frame*))
 => (buffer :: false-or(<basic-buffer>))
  // Read the file into a buffer (sectionizing it in the process),
  // then compute the signatures for the file buffer
  let buffer
    = find-buffer-from-pathname(editor, pathname)
      | do-find-file(editor, pathname, direction: #"input");
  buffer & compute-buffer-signatures(buffer);
  buffer
end method compute-file-signatures;

define sealed method compute-buffer-signatures
    (buffer :: <basic-buffer>) => (buffer :: <basic-buffer>)
  let container = buffer-source-container(buffer);
  when (container)
    do(compute-section-signature, container-sections(container))
  end;
  buffer
end method compute-buffer-signatures;

define sealed method compute-section-signature
    (section :: <basic-section>) => (signature :: false-or(<dylan-signature>))
  #f
end method compute-section-signature;

define sealed method compute-section-signature
    (section :: <dylan-section>) => (signature :: false-or(<dylan-signature>))
  let start-line = section-defining-line(section);
  let end-line   = section-end-line(section);
  let signature
    = when (start-line)
	let contents = line-contents(start-line);
	let length   = line-length(start-line);
	case
	  string-equal?(contents, "define", start1: 0, end1: min(length, 6)) =>
	    let interval
	      = make-interval(line-start(start-line), line-end(end-line), in-order?: #t);
	    let stream :: <interval-stream>
	      = make(<interval-stream>, interval: interval, direction: #"input");
	    block ()
	      parse-dylan-signature(stream)
	    cleanup
	      close(stream)
	    end;
	  string-equal?(contents, "begin", start1: 0, end1: min(length, 5)) =>
	    make(<dylan-signature>,
		 type: #"top-level-form", name: "begin ... end");
	  otherwise =>
	    #f;
	end;
      end;
  section.%signature := signature;
  signature
end method compute-section-signature;


define sealed class <dylan-signature> (<object>)
  sealed constant slot signature-type :: <symbol>,
    required-init-keyword: type:;
  sealed constant slot signature-name :: <string>,
    required-init-keyword: name:;
  sealed constant slot signature-parameters :: <vector> = #[],
    init-keyword: parameters:;
end class <dylan-signature>;

define sealed domain make (singleton(<dylan-signature>));
define sealed domain initialize (<dylan-signature>);

// Note that all definitions whose type is #"top-level-form" will compare as
// equal, meaning that such forms will need to be disambiguated at a higher
// level, e.g., by comparing the text of sections
define sealed method \=
    (s1 :: <dylan-signature>, s2 :: <dylan-signature>) => (equal? :: <boolean>)
  signature-type(s1)   = signature-type(s2)
  & signature-name(s1) = signature-name(s2)
  & signature-parameters(s1) = signature-parameters(s2)
end method \=;

define sealed inline method token-terminator?
    (ch :: <byte-character>) => (true? :: <boolean>)
  any-whitespace-char?(ch)
  | ch == ',' | ch == ';' | ch == '.'
  | ch == '(' | ch == ')' | ch == '[' | ch == ']' | ch == '{' | ch == '}'
end method token-terminator?;

define sealed method parse-dylan-signature
    (stream :: <stream>) => (signature :: false-or(<dylan-signature>))
  let buffer :: <stretchy-object-vector> = make(<stretchy-vector>);
  let params :: <stretchy-object-vector> = make(<stretchy-vector>);
  local method skip-whitespace () => ()
	  while (any-whitespace-char?(peek(stream, on-end-of-stream: '\0')))
	    read-element(stream)
	  end
	end method,
	method next-name () => (name :: <string>)
	  skip-whitespace();
	  buffer.size := 0;
	  block (return)
	    for (ch = read-element(stream, on-end-of-stream: #f)
		   then read-element(stream, on-end-of-stream: #f))
	      case
		~ch =>
		  return(as(<string>, buffer));
		ch == '\\' =>
		  add!(buffer, read-element(stream, on-end-of-stream: '\0'));
		token-terminator?(ch) =>
		  if (buffer.size = 0)
		    add!(buffer, ch)
		  else
		    unread-element(stream, ch)
		  end;
		  return(as(<string>, buffer));
		otherwise =>
		  add!(buffer, ch);
	      end
	    end
	  end
	end method,
	method next-token () => (token :: <symbol>)
	  as(<symbol>, next-name())
	end method,
	// The idea is to parse simple type expressions such as '<integer>',
	// 'false-or(<integer>)', 'one-of(#t, #f)', etc.
	//---*** We need to do a better job than this...
	next-expression (format-string :: <string>) => (expression :: <string>)
	  let name = next-name();
	  let more = "";
	  skip-whitespace();
	  when (peek(stream, on-end-of-stream: #f) == '(')
	    // If this token starts a function call, pick up the arguments
	    let bp1 = stream-position(stream);
	    let bp2 = move-over-lists(bp1, 1, fixup?: #f);
	    when (bp2)
	      let interval = make-interval(bp1, bp2, in-order?: #t);
	      more := as(<string>, interval);
	      stream-position(stream) := increment-bp!(bp2)
	    end
	  end;
	  format-to-string(format-string, concatenate(name, more))
	end method;
  local method parse-function-parameters () => (parameters :: <vector>)
	  if (next-token() ~== #"(")
	    #[]
	  else
	    params.size := 0;
	    block (break)
	      while (#t)
		// The next token should be either a parameter name or something like #key
		let token = next-token();
		when (member?(token, #[#"#rest", #"#key", #"#all-keys", #"#next"]))
		  break()
		end;
		// We got a name, get the next token
		let next = next-token();
		case
		  next == #"," | next == #")" =>
		    add!(params, "<object>");
		  next == #"::" =>
		    add!(params, next-expression("%s"));
		    next := next-token();
		  next == #"==" =>
		    add!(params, next-expression("singleton(%s)"));
		    next := next-token();
		end;
		unless (next == #",")
		  break()
		end
	      end
	    end;
	    as(<simple-object-vector>, params)
	  end
	end method,
	method parse-domain-parameters () => (parameters :: <vector>)
	  if (next-token() ~== #"(")
	    #[]
	  else
	    params.size := 0;
	    block (break)
	      while (#t)
		add!(params, next-expression("%s"));
		let next = next-token();
		unless (next == #",")
		  break()
		end
	      end
	    end;
	    as(<simple-object-vector>, params)
	  end
	end method;
  // We should be looking at a defining form, but if we're not,
  // just give up and don't return a signature
  when (next-token() == #"define")
    block (return)
      while (#t)
	let token = next-token();
	select (token)
	  #"library", #"module", #"interface", #"protocol", #"macro" =>
	    let name = next-name();
	    return(make(<dylan-signature>,
			type: token, name: name));
	  #"class", #"frame" =>
	    let name = next-name();
	    return(make(<dylan-signature>,
			type: token, name: name));
	  #"generic" =>
	    let name = next-name();
	    return(make(<dylan-signature>,
			type: token, name: name,
			parameters: parse-function-parameters()));
	  #"function", #"method" =>
	    let name = next-name();
	    return(make(<dylan-signature>,
			type: token, name: name,
			parameters: parse-function-parameters()));
	  #"domain" =>
	    let name = next-name();
	    return(make(<dylan-signature>,
			type: token, name: name,
			parameters: parse-domain-parameters()));
	  #"sealed", #"open", #"primary", #"free", #"abstract", #"concrete",
	  #"sideways", #"thread", #"atomic",
	  #"inline", #"inline-only", #"may-inline", #"default-inline", #"not-inline" =>
	    #f;
	  otherwise =>
	    let name = next-name();
	    return(make(<dylan-signature>,
			type: token, name: name));
	end
      end
    end
  end
end method parse-dylan-signature;


/// Dylan comments and indentation

define method do-insert-comment
    (mode :: <dylan-mode>, line :: <basic-line>, #key column)
 => (bp :: false-or(<basic-bp>))
  if (column)
    //--- This should attempt to insert the comment at the given column
    let bp = line-end(line);
    insert-moving!(bp, "\t// ")
  else
    let bp = line-start(line);
    insert-moving!(bp, "// ")
  end
end method do-insert-comment;

define method do-comment-region
    (mode :: <dylan-mode>, region :: <basic-interval>, #key comment?) => ()
  block (return)
    local method comment (line :: <basic-line>, si, ei, last?)
	    ignore(last?);
	    // Comment out the line only if the start index is zero and
	    // the end index is not zero
	    when (si = 0 & ei ~= 0)
	      insert-into-line(line, 0, "// ")
	    end
	  end method,
	  method uncomment (line :: <basic-line>, si, ei, last?)
	    ignore(last?);
	    when (si = 0 & ei ~= 0)
	      let length   = line-length(line);
	      let contents = line-contents(line);
	      if (length >= 3 & string-equal?(contents, "// ", end1: 3))
		delete-within-line(line, 0, 3)
	      else
		return()
	      end
	    end
	  end method;
    do-lines(if (comment?) comment else uncomment end, region)
  end
end method do-comment-region;


define method do-indent-line
    (mode :: <dylan-mode>, line :: <basic-line>)
 => (bp :: false-or(<basic-bp>), dx :: <integer>, nchars :: <integer>)
  indent-dylan-line(mode, line)
end method do-indent-line;

define sealed method indent-dylan-line
    (mode :: <major-mode>, line :: <basic-line>, #key definition-type)
 => (bp :: false-or(<basic-bp>), dx :: <integer>, nchars :: <integer>)
  let bp1 = line-start(line);
  let bp2 = forward-over(bp1, #[' ', '\t']);
  local method insert-indentation!
	    (line :: <basic-line>, indentation :: <integer>)
	 => (bp :: <basic-bp>, nchars :: <integer>)
	  if (text-line?(line))
	    let interval = make-interval(bp1, bp2, in-order?: #t);
	    let old-n    = count-characters(interval);
	    let new-n    = max(indentation, 0);
	    delete!(interval);
	    let spaces = make(<byte-string>, size: new-n, fill: ' ');
	    values(insert-moving!(bp1, spaces), new-n - old-n)
	  else
	    values(bp1, 0)
	  end
	end method;
  let (old-indentation, new-indentation)
    = dylan-line-indentation(mode, line, definition-type: definition-type);
  let dx = new-indentation - old-indentation;
  if (dx = 0)
    values(bp2, 0, 0)
  else
    let (bp, nchars) = insert-indentation!(line, new-indentation);
    values(bp, nchars, dx)
  end
end method indent-dylan-line;

//---*** We could sure use a better indenter than this!
define sealed method dylan-line-indentation
    (mode :: <major-mode>, line :: <basic-line>, #key definition-type)
 => (old-indentation :: <integer>, new-indentation :: <integer>)
  let window :: <basic-window> = frame-window(*editor-frame*);
  let margin = line-margin(line, mode, window);
  let space-width = string-size(window, " ");
  local method line-indentation
	    (sbp :: <basic-bp>) => (indentation :: <integer>)
	  if (text-line?(bp-line(sbp)))
	    let indentation
	      = index->position(bp-line(sbp), mode, window, bp-index(sbp)) - margin;
	    floor/(indentation, space-width)
	  else
	    0
	  end
	end method,
	method looking-at?
	    (contents :: <byte-string>, _start :: <integer>, _end :: <integer>,
	     string :: <byte-string>)
	 => (true? :: <boolean>)
	  let _end = min(_start + size(string), _end);
	  string-equal?(contents, string, start1: _start, end1: _end)
	end method,
	method looking-at-atom?
	    (contents :: <byte-string>, _start :: <integer>, _end :: <integer>,
	     string :: <byte-string>)
	 => (true? :: <boolean>)
	  let _end = min(_start + size(string), _end);
	  string-equal?(contents, string, start1: _start, end1: _end)
	  /* & (_end >= size(contents)
	        | atom-syntax(contents[_end]) ~= $atom-alphabetic) */
	end method,
	method bp-looking-at-call?
	    (bp :: <basic-bp>, string :: <byte-string>)
	 => (true? :: <boolean>)
	  bp-looking-at-atom?(bp, string)
	  & begin
	      let bp = forward-over!(move-over-atoms(bp, 1), #[' ', '\t']);
              bp-character(bp) == '('
            end
	end method;
  let section  = line-section(line);
  let type     = definition-type | section-definition-type(section);
  let this-bp  = line-start(line);
  let this-sbp = forward-over(this-bp, #[' ', '\t']);
  let this-indentation :: <integer> = line-indentation(this-sbp);
  let prev-line = line-previous(line);		// stay within this node
  let prev-bp   = prev-line & line-start(prev-line);
  let prev-sbp  = prev-bp   & forward-over(prev-bp, #[' ', '\t']);
  let prev-indentation :: <integer> = if (prev-sbp) line-indentation(prev-sbp) else 0 end;
  let indentation :: <integer> = prev-indentation;
  block (break)
    when (text-line?(line))
      let line  = bp-line(this-sbp);
      let index = bp-index(this-sbp);
      let length   = line-length(line);
      let contents = line-contents(line);
      local method maybe-hack-function-indentation ()
	      dec!(indentation, 2);
	      when (prev-line)
		let l = line-length(prev-line);
		let c = line-contents(prev-line);
		when (l >= 2
		      & ((looking-at?(c, l - 2, l, ");") & dec!(l))
                         | looking-at?(c, l - 1, l, ")")))
		  let bp = move-over-lists(make-bp(prev-line, l), -2);
                  let bp = move-bp!(bp, bp-line(bp), 0);
                  let bp = forward-over!(bp, #[' ', '\t']);
		  unless (bp-looking-at-call?(bp, "for")
			  | bp-looking-at-call?(bp, "while")
			  | bp-looking-at-call?(bp, "until")
			  | bp-looking-at-call?(bp, "if")
			  | bp-looking-at-call?(bp, "when")
			  | bp-looking-at-call?(bp, "unless"))
		    indentation := line-indentation(bp) - 2;
		    break()
		  end
                end
              end
            end method;
      when (type == #"function" | type == #"method")
      end;
      case
	looking-at-atom?(contents, index, length, "end") =>
	maybe-hack-function-indentation();
	looking-at-atom?(contents, index, length, "else")
	| looking-at-atom?(contents, index, length, "elseif") =>
	  maybe-hack-function-indentation();
	looking-at-atom?(contents, index, length, "exception")
	| looking-at-atom?(contents, index, length, "cleanup")
	| looking-at-atom?(contents, index, length, "finally") =>
	  maybe-hack-function-indentation();
	looking-at?(contents, index, length, "= ")
	| looking-at?(contents, index, length, ":= ") =>
	  inc!(indentation, 2);
	  break();
	looking-at-atom?(contents, index, length, "define") =>
	  // 'define' forms always start at the beginning of the line
	  indentation := 0;
	  break();
	type == #"class"
        & (looking-at-atom?(contents, index, length, "slot")
	   | looking-at-atom?(contents, index, length, "sealed slot")
	   | looking-at-atom?(contents, index, length, "constant slot")
	   | looking-at-atom?(contents, index, length, "sealed constant slot")
	   | looking-at-atom?(contents, index, length, "open slot")
	   | looking-at-atom?(contents, index, length, "virtual slot")
	   | looking-at-atom?(contents, index, length, "keyword")) =>
	  indentation := 2;
	  break();
	looking-at?(contents, index, length, "=>") =>
	  if (bp-line(move-over-lists(this-bp, -2)) == section-defining-line(section))
	    // Assume '=>' at the beginning of the line is for return values
	    indentation := 1;
	    break()
	  else
	    inc!(indentation, 2)
	  end;
	(type == #"function" | type == #"method")
	& length >= 2
	& looking-at?(contents, length - 2, length, "=>") =>
	  dec!(indentation, 2);
      end
    end;
    when (prev-line & text-line?(prev-line))
      let line  = bp-line(prev-sbp);
      let index = bp-index(prev-sbp);
      let length   = line-length(line);
      let contents = line-contents(line);
      when (type == #"function" | type == #"method")
        when (length >= 1
	      & looking-at?(contents, length - 1, length, ",")
              & ~bp-looking-at?(forward-over(prev-bp, #[' ', '\t']), "//"))
	  // Comma at the end of a line is probably inside a parameter list,
	  // so indent to one past the enclosing parenthesis
	  let pbp = move-up-or-down-lists(this-bp, -1);
	  indentation := line-indentation(pbp) + 1;
	  break()
	end;
	let l = length;
        when (l >= 2
              & ((looking-at?(contents, l - 2, l, ");") & dec!(l))
                  | looking-at?(contents, l - 1, l, ")")))
	  // We're probably looking at the end of a function call,
	  // so indent to the same level as the function call
	  let bp = move-over-lists(make-bp(line, l), -2);
	  let bp = move-bp!(bp, bp-line(bp), 0);
	  let bp = forward-over!(bp, #[' ', '\t']);
          if (bp-looking-at-call?(bp, "for")
              | bp-looking-at-call?(bp, "while")
              | bp-looking-at-call?(bp, "until")
              | bp-looking-at-call?(bp, "if")
              | bp-looking-at-call?(bp, "when")
              | bp-looking-at-call?(bp, "unless")
              | bp-looking-at-call?(bp, "block")
              | bp-looking-at-call?(bp, "exception")
              | bp-looking-at-call?(bp, "select")
              | bp-looking-at-call?(bp, "dynamic-bind")
              | bp-looking-at-call?(bp, "with-")
              | bp-looking-at-call?(bp, "without-")
              | bp-looking-at-call?(bp, "iterate")
              | bp-looking-at-call?(bp, "collecting"))
            indentation := line-indentation(bp) + 2
          elseif (bp-looking-at?(bp, "local method "))
            indentation := line-indentation(bp) + 8
          else
	    indentation := line-indentation(bp);
	    when (bp-looking-at?(bp, "= ")
                  | bp-looking-at?(bp, ":= "))
	      dec!(indentation, 2)
	    end
	  end;
	  break()
	end;
        when (length >= 2
	      & looking-at?(contents, length - 2, length, "=>"))
	  // "=>" at the end of a line is probably for 'case' or 'select'
	  inc!(indentation, 2)
	end
      end;
      when (looking-at?(contents, index, length, "= "))
        inc!(index, 2);
        inc!(indentation, 2)
      end;
      case
	looking-at-atom?(contents, index, length, "define") =>
	  inc!(indentation, 4);
	looking-at-atom?(contents, index, length, "begin") =>
	  inc!(indentation, 2);
	looking-at-atom?(contents, index, length, "block")
	| looking-at-atom?(contents, index, length, "exception")
	| looking-at-atom?(contents, index, length, "cleanup")
	| looking-at-atom?(contents, index, length, "finally") =>
	  inc!(indentation, 2);
	looking-at-atom?(contents, index, length, "when")
	| looking-at-atom?(contents, index, length, "unless") =>
	  inc!(indentation, 2);
	looking-at-atom?(contents, index, length, "if")
	| looking-at-atom?(contents, index, length, "else")
	| looking-at-atom?(contents, index, length, "elseif") =>
	  inc!(indentation, 2);
	looking-at-atom?(contents, index, length, "while")
	| looking-at-atom?(contents, index, length, "until") =>
	  inc!(indentation, 2);
	looking-at-atom?(contents, index, length, "for") =>
	  inc!(indentation, 2);
	looking-at-atom?(contents, index, length, "case") =>
	  inc!(indentation, 2);
	looking-at-atom?(contents, index, length, "select") =>
	  inc!(indentation, 2);
	looking-at-atom?(contents, index, length, "local method") =>
	  inc!(indentation, 8);
	looking-at-atom?(contents, index, length, "slot")
	| looking-at-atom?(contents, index, length, "sealed slot")
	| looking-at-atom?(contents, index, length, "open slot")
	| looking-at-atom?(contents, index, length, "virtual slot") =>
	  inc!(indentation, 2);
	// Special hack for 'with-XXX' macros, etc.
	looking-at?(contents, index, length, "with-")
	| looking-at?(contents, index, length, "without-")
	| looking-at-atom?(contents, index, length, "dynamic-bind") =>
	  inc!(indentation, 2);
      end
    end
  end;
  values(this-indentation, indentation)
end method dylan-line-indentation;


define command dylan-insert-block-end (frame)
    "Insert an 'end' to close the current block."
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let mode    = buffer-major-mode(buffer);
  let bp1     = point();
  let line1   = bp-line(bp1);
  let new?    = ~line-empty?(line1);
  let section = line-section(bp-line(bp1));
  with-change-recording (buffer, <paste-change-record>, start-bp: bp1)
    let (name, type)
      = values(section-definition-name(section), section-definition-type(section));
    let bp2
      = if (new?) insert-moving!(line-end(line1), '\n') else line-start(line1) end;
    let line2 = bp-line(bp2);
    insert-moving!(bp2, "end");
    indent-dylan-line(mode, line2);
    move-bp!(bp2, line2, line-length(line2));
    case
      type == #"function" | type == #"method" =>
	if (string-equal?(line-contents(line2), "end", end1: 3))
	  // If we're in a function body and there's no indentation,
	  // insert the "end of definition" frob
	  insert-moving!(bp2, format-to-string(" %s %s;", as-lowercase(as(<string>, type)), name))
	else
	  // Take a reasonable guess as to whether to insert a ";"
	  let next = line-next(line2);
	  let nbp  = next & forward-over!(line-start(next), #[' ', '\t']);
	  when (nbp & ~(bp-looking-at?(nbp, "end")
			| bp-looking-at?(nbp, "else")
			| bp-looking-at?(nbp, "elseif")))
	    insert-moving!(bp2, ";")
	  end
	end;
      type == #"top-level-form" =>
	insert-moving!(bp2, ";");
      otherwise =>
	// Not within a function body, always insert the "end of definition" frob
	insert-moving!(bp2, format-to-string(" %s %s;", as-lowercase(as(<string>, type)), name));
    end;
    move-point!(bp2);
    if (new?)
      // The following bit is based on 'do-insert-newline'
      let last? = (line2 == bp-line(interval-end-bp(buffer)));
      if (last?)
	queue-redisplay(window, $display-text, centering: 1)
      else
	queue-redisplay(window, $display-blt, line: line1, index: 1, centering: 1)
      end
    else
      queue-redisplay(window, $display-line, line: line1, index: 0, centering: 1)
    end
  end;
  frame-last-command-type(frame) := #"insert"
end command dylan-insert-block-end;


/// Breakpoints

define method line-breakpoint?
    (mode :: <dylan-mode>, line :: <basic-line>)
 => (state :: <breakpoint-state>)
  let type = section-definition-type(line-section(line));
  when (type == #"function" | type == #"method" | type == #"command")
    if (line-empty?(line)
	| bp-looking-at?(line-start(line), "//"))
      #f
    else
      //--- This is where DylanWorks mode calls out to the environment
      get-property(line-properties(line), #"breakpoint", default: #"none")
    end
  end
end method line-breakpoint?;

define method line-breakpoint?-setter
    (state :: <breakpoint-state>, mode :: <dylan-mode>, line :: <basic-line>)
 => (state :: <breakpoint-state>)
  //--- This is where DylanWorks mode calls out to the environment
  put-property!(line-properties(line), #"breakpoint", state);
  state
end method line-breakpoint?-setter;


/// Dylan shells

define open class <dylan-shell-mode> (<shell-mode>)
end class <dylan-shell-mode>;

define method initialize-major-mode
    (mode :: <dylan-shell-mode>, #key command-set = mode-command-set(mode)) => ()
  let command-set = install-dylan-mode-bindings(mode, command-set);
  next-method(mode, command-set: command-set)
end method initialize-major-mode;

begin
  gethash(*keyword->major-mode*, #"dylan-shell") := <dylan-shell-mode>
end;

define method mode-name
    (mode :: <dylan-shell-mode>) => (name :: <byte-string>)
  "Dylan shell"
end method mode-name;

define method do-indent-line
    (mode :: <dylan-shell-mode>, line :: <basic-line>)
 => (bp :: false-or(<basic-bp>), dx :: <integer>, nchars :: <integer>)
  indent-dylan-line(mode, line)
end method do-indent-line;

define method shell-input-complete?
    (mode :: <dylan-shell-mode>,
     buffer :: <basic-shell-buffer>, section :: <basic-shell-section>)
 => (complete? :: <boolean>, message :: false-or(<string>))
  //--- This is where DylanWorks mode decides if there's a complete form
  values(#t, #f)
end method shell-input-complete?;

define method do-process-shell-input
    (mode :: <dylan-shell-mode>,
     buffer :: <basic-shell-buffer>, section :: <basic-shell-section>,
     #key window = frame-window(*editor-frame*)) => ()
  ignore(window);
  let bp = line-end(section-end-line(section));
  queue-redisplay(window, $display-text);
  //--- This is where DylanWorks mode evaluates the form and prints the results
  insert-moving!(bp, "You wish this worked!\n");
  move-point!(bp, window: window)
end method do-process-shell-input;

define variable *dylan-shell-count* :: <integer> = 0;

define method make-dylan-shell
    (#key name, anonymous? = #f,
	  buffer-class  = <simple-shell-buffer>,
	  major-mode    = find-mode(<dylan-shell-mode>),
	  section-class = <simple-shell-section>,
	  editor        = frame-editor(*editor-frame*))
 => (buffer :: <basic-shell-buffer>)
  unless (name)
    inc!(*dylan-shell-count*);
    name := format-to-string("Dylan shell %d", *dylan-shell-count*)
  end;
  let buffer = make-empty-buffer(buffer-class,
				 name:       name,
				 major-mode: major-mode,
				 anonymous?: anonymous?,
				 section-class: section-class,
				 editor: editor);
  shell-buffer-section-class(buffer) := section-class;
  buffer
end method make-dylan-shell;


/// Display within shells

//--- Need a more modular way to do this
define constant $prompt-image-width  :: <integer> = 16;
define constant $prompt-image-height :: <integer> = 16;
define constant $prompt-image-offset :: <integer> =  2;

define sealed method display-line
    (line :: <text-line>, mode :: <dylan-shell-mode>, window :: <basic-window>,
     x :: <integer>, y :: <integer>,
     #key start: _start = 0, end: _end = line-length(line), align-y = #"top") => ()
  let section = line-section(line);
  let image = case
		~shell-section?(section) =>
		  #f;
		line == section-start-line(section) =>
		  $prompt-arrow;
		line == section-output-line(section) =>
		  $values-arrow;
		otherwise =>
		  #f;
	      end;
  when (image & _start = 0)	// no icon on continuation lines
    let image-y = if (align-y == #"top") y else y - $prompt-image-height + 2 end;
    draw-image(window, standard-images(window, image), x, image-y + $prompt-image-offset)
  end;
  next-method(line, mode, window, x + $prompt-image-width, y,
	      start: _start, end: _end, align-y: align-y)
end method display-line;

define sealed method line-size
    (line :: <text-line>, mode :: <dylan-shell-mode>, window :: <basic-window>,
     #key start: _start, end: _end)
 => (width :: <integer>, height :: <integer>, baseline :: <integer>)
  ignore(_start, _end);
  let (width, height, baseline) = next-method();
  values(width + $prompt-image-width, height, baseline)
end method line-size;

define sealed method position->index
    (line :: <text-line>, mode :: <dylan-shell-mode>, window :: <basic-window>,
     x :: <integer>)
 => (index :: <integer>)
  let x = x - $prompt-image-width;
  if (x < 0) 0 else next-method(line, mode, window, x) end
end method position->index;

define sealed method index->position
    (line :: <text-line>, mode :: <dylan-shell-mode>, window :: <basic-window>,
     index :: <integer>)
 => (x :: <integer>)
  next-method(line, mode, window, index)
end method index->position;

define sealed method line-margin
    (line :: <text-line>, mode :: <dylan-shell-mode>, window :: <basic-window>)
 => (margin :: <integer>)
  $prompt-image-width
end method line-margin;
