Module:       CAPI-DUIM
Synopsis:     CAPI back-end
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// capi port

define sealed class <capi-port> (<basic-port>)
  sealed slot %screen = #f;
  sealed slot %window = #f;
  sealed slot %cursor-cache :: <object-table> = make(<table>);
  sealed slot %cursor-font = #f;
  keyword focus-policy: = #"sheet-under-pointer";
end class <capi-port>;

define method initialize (_port :: <capi-port>, #key)
  next-method();
  _port.%screen := convert-to-screen();
  _port.%window := representation(_port.%screen);
  install-font-mappings(_port);
end method initialize;

define constant $capi-port-name = #"capi";

define method port-type (_port :: <capi-port>) => (type :: <symbol>)
  $capi-port-name
end method port-type;

define method port-name (_port :: <capi-port>) => (name :: false-or(<string>))
  #f  
end method port-name;

define sideways method class-for-make-port
    (type == $capi-port-name, #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  values(<capi-port>, concatenate(initargs, #(event-processor-type:, #"n")));
end method class-for-make-port;

define sideways method class-for-make-port
    (type == #"local", #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  apply(class-for-make-port, #"capi", initargs)
end method class-for-make-port;

define method find-capi-port ()
  find-port(server-path: list($capi-port-name))
end method find-capi-port;


/// Font mapping

define constant $capi-font-families :: <list>
    = #(#(#"fix", "courier"),
	#(#"serif", "times", "charter", "new century schoolbook"),
	#(#"sans-serif", "helvetica"));

define constant $capi-logical-sizes :: <simple-object-vector>
    = #[#[#"normal",     12],	// put most common one first for efficiency
	#[#"small",      10],
	#[#"large",      14],
	#[#"very-small",  8],
	#[#"very-large", 18],
	#[#"tiny",        6],
	#[#"huge",       24]];

define function font->text-style (font, family)
  let weight = select (generic-font-weight(font))
		 bold: => #"bold";		//--- damned emulator
		 otherwise => #"normal";
	       end;
  let slant = select (generic-font-slant(font))
		italic:, oblique: => #"italic";//--- damned emulator
		otherwise => #"roman";
	      end;
  let size = generic-font-size(font);
  make-text-style(family, weight, slant, size)
end function font->text-style;

define method install-font-mapping (_port :: <capi-port>, family, font)
  let text-style = font->text-style(font, family);
  // Don't override if we've already defined a mapping
  unless (text-style-mapping-exists?(_port, text-style, exact-size?: #t))
    text-style-mapping(_port, text-style) := font
  end;
  font
end method install-font-mapping;

define method install-font-mappings (_port :: <capi-port>)
  // Establish the basic mappings, but don't read the fonts
  for (family-data in $capi-font-families)
    let family = head(family-data);
    for (font-name in tail(family-data))
      for (font in query-fonts(_port.%window, parse-font-name(#(), font-name)))
	if (generic-font-size(font))
	  install-font-mapping(_port, family, font)
	else
	  for (size-data in $capi-logical-sizes)
	    let size = size-data[1];
	    let new-font = copy-generic-font(font);
	    generic-font-size(new-font) := size;
	    install-font-mapping(_port, family, new-font)
	  end
	end
      end
    end
  end;
  // Establish some default mappings, too
  let default-style
    = block (return) 
	for (family-data in $capi-font-families)
	  let family = head(family-data);
	  let text-style = make-text-style(family, #"normal", #"roman", 10);
	  when (text-style-mapping-exists?(_port, text-style))
	    return(text-style)
	  end
	end
      end;
  if (default-style)
    port-undefined-text-style(_port) := default-style
  else
    let font = lookup-font(_port.%window, default-port-font:);
    port-undefined-text-style(_port) := font->text-style(font, #"fix");
    text-style-mapping(_port, port-undefined-text-style(_port)) := font
  end;
  _port.%cursor-font := lookup-font(_port.%window, "cursor");
end method install-font-mappings;

define method text-style-mapping
    (_port :: <capi-port>, style :: <text-style>, #key character-set)
  let font = next-method();
  when (instance?(font, <string>) | instance?(font, <symbol>))
    font := lookup-font(_port.%window, as(<string>, font));
    text-style-mapping(_port, style) := font
  end;
  font
end method text-style-mapping;

define method standardize-text-style
    (_port :: <capi-port>, style :: <text-style>, #key character-set)
    => (style :: <text-style>)
  ignore(character-set);
  standardize-text-style-size(_port, style, $capi-logical-sizes)
end method standardize-text-style;


/// Beeping, etc

define method force-display (_port :: <capi-port>)
  representation-force-output(_port.%window)
end method force-display;

define method synchronize-display (_port :: <capi-port>)
  representation-finish-output(_port.%window)
end method synchronize-display;

define sealed method beep (_port :: <capi-port>)
  beep-pane()
end method beep;


/// Pointer handling

define method do-pointer-position
    (_port :: <capi-port>, pointer :: <pointer>, sheet :: <sheet>)
 => (x :: <integer>, y :: <integer>)
  let mirror = sheet-mirror(sheet);
  let rep = representation(mirror);
  x-pointer-position(rep)
end method do-pointer-position;

define method do-pointer-position
    (_port :: <capi-port>, pointer :: <pointer>, sheet :: <display>)
 => (x :: <integer>, y :: <integer>)
  let display = contact-display(_port.%screen);
  global-pointer-position(display)
end method do-pointer-position;


define method do-set-pointer-position
    (_port :: <capi-port>, pointer :: <pointer>, sheet :: <sheet>,
     x :: <integer>, y :: <integer>) => ()
  let mirror = sheet-mirror(sheet);
  let rep = representation(mirror);
  warp-pointer(rep, x, y)
end method do-set-pointer-position;

define method do-set-pointer-position
    (_port :: <capi-port>, pointer :: <pointer>, sheet :: <display>,
     x :: <integer>, y :: <integer>) => ()
  let display = contact-display(_port.%screen);
  warp-pointer(screen-root(display-default-screen(display)), x, y)
end method do-set-pointer-position;


//--- make-event-mask(:pointer-motion, :button-motion, :button-press, :button-release)
define constant $pointer-grab-mask = 0;

define method grab-pointer
    (_port :: <capi-port>, pointer :: <pointer>, sheet :: <sheet>)
 => (success? :: <boolean>)
  let rep = representation(sheet-mirror(sheet));
  //---*** Now what?
  #t
end method grab-pointer;

define method ungrab-pointer
    (_port :: <capi-port>, pointer :: <pointer>)
 => (success? :: <boolean>)
  //---*** Now what?
  #t
end method ungrab-pointer;


/// Pointer cursors

define constant $capi-cursor-types :: <simple-object-vector>
    = #[#[#"default",           132],
	#[#"busy",              150],
	#[#"vertical-scroll",   116],
	#[#"horizontal-scroll", 108],
	#[#"scroll-up",         114],
	#[#"scroll-down",       106],
	#[#"scroll-left",       110],
	#[#"scroll-right",      112],
	#[#"upper-left",        134],
	#[#"upper-right",       136],
	#[#"lower-left",         12],
	#[#"lower-right",        14],
	#[#"vertical-thumb",    116],
	#[#"horizontal-thumb",  108],
	#[#"button",            132],
	#[#"prompt",             92],
	#[#"move",               52],
	#[#"position",           34],
	#[#"i-beam",            114],
	#[#"cross",              52],
	#[#"starting",          150],
	#[#"hand",              132]];

define method do-set-pointer-cursor
    (_port :: <capi-port>, pointer :: <pointer>, cursor :: <cursor>)
  let sheet = pointer-sheet(pointer);
  when (sheet)
    let mirror = sheet-mirror(sheet);
    let rep = mirror & representation(mirror);
    when (rep & ~lisp-false?(rep))	// damned emulator
      window-cursor(rep) := make-capi-cursor(_port, cursor);
      force-display(_port)
    end
  end
end method do-set-pointer-cursor;

define method do-set-sheet-cursor
    (_port :: <capi-port>, sheet :: <sheet>, cursor :: <cursor>) => ()
  let mirror = sheet-mirror(sheet);
  let rep = mirror & representation(mirror);
  when (rep & ~lisp-false?(rep))	// damned emulator
    window-cursor(rep) := make-capi-cursor(_port, cursor);
    force-display(_port)
  end
end method do-set-sheet-cursor;

define method make-capi-cursor (_port :: <capi-port>, cursor)
  gethash(_port.%cursor-cache, cursor)
  | begin
      let entry = find-pair($capi-cursor-types, cursor);
      let cursor = if (entry) entry[1] else 132 end;
      let x-cursor
	= create-glyph-cursor(source-font: _port.%cursor-font,
			      source-char: cursor,
			      mask-font: _port.%cursor-font,
			      mask-char: cursor + 1,
			      //--- Should query for fg and bg colors
			      foreground: make-x-color(red: 0.0, green: 0.0, blue: 0.0),
			      background: make-x-color(red: 1.0, green: 1.0, blue: 1.0));
      gethash(_port.%cursor-cache, cursor) := x-cursor
    end
end method make-capi-cursor;


/// Focus handling

define sealed method note-focus-in
    (_port :: <win32-port>, sheet :: <sheet>) => ()
  let caret = sheet-caret(sheet);
  // Show the new sheet's caret if it supposed to be visible
  when (caret?(caret) & caret-visible?(caret))	// could be #t or #f...
    do-show-caret(caret)
  end
end method note-focus-in;

define sealed method note-focus-out
    (_port :: <win32-port>, sheet :: <sheet>) => ()
  let caret = sheet-caret(sheet);
  // Hide the old sheet's caret if it is currently visible
  when (caret?(caret) & caret-visible?(caret))	// could be #t or #f...
    do-hide-caret(caret)
  end
end method note-focus-out;


/// Palettes

define sealed class <capi-palette> (<basic-palette>)
  sealed slot port :: false-or(<port>),
    required-init-keyword: port:,
    setter: %port-setter;
end class <capi-palette>;
