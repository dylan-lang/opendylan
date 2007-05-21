Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Lines

define protocol <<line>> ()
  getter line-next
    (line :: <line>) => (next :: false-or(<line>));
  setter line-next-setter
    (next :: false-or(<line>), line :: <line>) => (next :: false-or(<line>));
  getter line-previous
    (line :: <line>) => (previous :: false-or(<line>));
  setter line-previous-setter
    (previous :: false-or(<line>), line :: <line>) => (previous :: false-or(<line>));
  getter line-section
    (line :: <line>) => (section :: false-or(<section>));
  setter line-section-setter
    (section :: false-or(<section>), line :: <line>) => (section :: false-or(<section>));
  getter line-modification-tick
    (line :: <line>) => (tick :: <integer>);
  setter line-modification-tick-setter
    (tick :: <integer>, line :: <line>) => (tick :: <integer>);
  getter line-bps
    (line :: <line>) => (bps :: <sequence>);
  setter line-bps-setter
    (bps :: <sequence>, line :: <line>) => (bps :: <sequence>);
  getter line-properties
    (line :: <line>) => (properties :: <sequence>);
  setter line-properties-setter
    (properties :: <sequence>, line :: <line>) => (properties :: <sequence>);
  getter line-contents-properties
    (line :: <line>) => (properties :: <sequence>);
  setter line-contents-properties-setter
    (properties :: <sequence>, line :: <line>) => (properties :: <sequence>);
  function line-empty?
    (line :: <line>, #key index) => (empty? :: <boolean>);
  function copy-line
    (line :: <line>, #key start: _start, end: _end) => (new-line :: <line>);
  function note-line-changed
    (line :: <line>) => (line :: <line>);
  getter line-read-only?
    (line :: <line>) => (read-only? :: <boolean>);
  setter line-read-only?-setter
    (read-only? :: <boolean>, line :: <line>) => (read-only? :: <boolean>);
  getter line-for-display-only?
    (line :: <line>) => (display-only? :: <boolean>);
  // Text lines
  getter line-length
    (line :: <line>) => (length :: <integer>);
  setter line-length-setter
    (length :: <integer>, line :: <line>) => (length :: <integer>);
  getter line-contents
    (line :: <line>) => (contents :: <string>);
  setter line-contents-setter
    (contents :: <string>, line :: <line>) => (contents :: <string>);
  getter line-style-changes
    (line :: <line>) => (changes :: <vector>);
  setter line-style-changes-setter
    (changes :: <vector>, line :: <line>) => (changes :: <vector>);
  // Saving lines to files
  function dump-line
    (line :: <line>, stream :: <stream>) => ();
  // Conserving memory
  function gc-line
    (line :: <line>) => ();
end protocol <<line>>;

// Lines are the primary storage unit for data, and are maintained as
// a doubly-linked list.  Lines are held in sections.  Note that lines
// can be what you normally think of as textual lines, but we also
// support the notion of a diagram line that contains graphical data.
define open abstract primary class <basic-line> (<line>)
  sealed slot line-next :: false-or(<basic-line>) = #f,
    init-keyword: next:;
  sealed slot line-previous :: false-or(<basic-line>) = #f,
    init-keyword: previous:;
  sealed slot line-section :: false-or(<basic-section>) = #f,
    init-keyword: section:;
  // The line modification tick is used to help drive redisplay
  sealed slot line-modification-tick :: <integer> = *tick*;
  // A set of any moving BPs that point to this line
  sealed slot line-bps :: <list> = #();
  // The (relatively) permanent properties of the line
  sealed slot line-properties :: <list> = #(),
    init-keyword: properties:;
  // The properties of the line based on its contents
  sealed slot line-contents-properties :: <list> = #();
end class <basic-line>;

//--- Zwei has "copy on write" lines in 'mung-line' -- do we need this?
define method note-line-changed
    (line :: <basic-line>) => (line :: <basic-line>)
  // Bump the modification tick and flush contents properties
  line-modification-tick(line) := tick();
  line-contents-properties(line) := #();
  // Let the major mode know the line changed
  let node   = line-node(line);
  let buffer = node & node-buffer(node);
  when (buffer)
    line := do-note-line-changed(buffer-major-mode(buffer), line)
  end;
  // Notify the section that one of its lines changed
  let section = line-section(line);
  when (section)
    note-section-changed(section)
  end;
  line
end method note-line-changed;


define sealed inline method line-read-only?
    (line :: <basic-line>) => (read-only? :: <boolean>)
  // We overload the modification tick to indicate a read-only line
  // This has the advantage that 'display-line-tick' of a display line
  // will always be greater than the tick of a read-only line
  line-modification-tick(line) == -1
end method line-read-only?;

define sealed method line-read-only?-setter
    (read-only? :: <boolean>, line :: <basic-line>) => (read-only? :: <boolean>)
  line-modification-tick(line) := if (read-only?) -1 else *tick* end;
  read-only?
end method line-read-only?-setter;


define sealed method line-for-display-only?
    (line :: <basic-line>) => (display-only? :: <boolean>)
  #f
end method line-for-display-only?;


/// Text lines

// A line containing text
define open abstract class <text-line> (<basic-line>)
  // We keep the length separately from since strings aren't stretchy.
  // That is, we allocate more contents than we need, and use 'line-length'
  // to indicate the active part of the string.
  sealed slot line-length :: <integer>,
    setter: %length-setter,
    required-init-keyword: length:;
  sealed slot line-contents :: <byte-string>,
    setter: %contents-setter,
    required-init-keyword: contents:;
end class <text-line>;

define sealed inline method text-line?
    (line :: <line>) => (text-line? :: <boolean>)
  #f
end method text-line?;

define sealed inline method text-line?
    (line :: <text-line>) => (text-line? :: <boolean>)
  #t
end method text-line?;


// For deciding how much to "pre-emptively" grow line contents
define constant $line-expansion-factor :: <single-float> = 1.2;

// For deciding when to shrink lines during "garbage collection"
//--- Make this be the platform's smallest memory allocation unit?
define constant $line-gc-size-margin :: <integer> = 8;


define sealed method line-length-setter
    (length :: <integer>, line :: <text-line>) => (length :: <integer>)
  let contents = line-contents(line);
  let contents-length = size(contents);
  when (length > contents-length)
    // Ensure there's enough room in the contents for the new length
    let new-contents
      = make(object-class(contents),
	     size: max(length,
		       floor(contents-length * $line-expansion-factor)));
    copy-bytes(new-contents, 0, contents, 0, line-length(line));
    line.%contents := new-contents
  end;
  line.%length := length;
  note-line-changed(line);	// note the change!
  length
end method line-length-setter;

define sealed method line-contents-setter
    (contents :: <byte-string>, line :: <text-line>)
 => (contents :: <byte-string>)
  line.%length   := size(contents);
  line.%contents := contents;
  note-line-changed(line);	// note the change!
  contents
end method line-contents-setter;

// This copies neither the line's section nor its properties.  That can be
// done at a higher level if necessary.
define sealed method copy-line
    (line :: <text-line>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = line-length(line))
 => (new-line :: <text-line>)
  make(object-class(line),
       next: #f, previous: #f, section: #f,
       length:   _end - _start,
       contents: copy-sequence(line-contents(line), start: _start, end: _end))
end method copy-line;

// Text lines are considered empty if they contain only whitespace
define sealed method line-empty?
    (line :: <text-line>, #key index :: <integer> = 0) => (empty? :: <boolean>)
  line-length(line) = 0
  | ~position-if(line-contents(line),
		 method (ch :: <byte-character>) ~any-whitespace-char?(ch) end,
		 // Whitespace tends to be at the beginning of a line, so
		 // searching backwards for non-whitespace is likely faster
		 start: index, end: line-length(line), from-end?: #t)
end method line-empty?;

define sealed method gc-line
    (line :: <text-line>) => ()
  let length   = line-length(line);
  let contents = line-contents(line);
  when (size(contents) > length + $line-gc-size-margin)
    line-contents(line) := copy-sequence(contents, end: length)
  end
end method gc-line;

define sealed inline method make
    (class == <text-line>, #rest initargs, #key contents, length, #all-keys)
 => (line :: <fixed-text-line>)
  apply(make, <fixed-text-line>,
	contents: contents,
	length:   length | size(contents),
	initargs)
end method make;


// A line containing only fixed-width text of the default style
define sealed class <fixed-text-line> (<text-line>)
end class <fixed-text-line>;

define sealed domain make (singleton(<fixed-text-line>));
define sealed domain initialize (<fixed-text-line>);

define sealed method line-style-changes
    (line :: <fixed-text-line>) => (changes :: <vector>)
  #[]
end method line-style-changes;

define sealed method line-style-changes-setter
    (changes :: <vector>, line :: <fixed-text-line>) => (changes :: <vector>)
  error("You can't set the style attributes of a fixed text line this way")
end method line-style-changes-setter;

define sealed method dump-line
    (line :: <fixed-text-line>, stream :: <stream>) => ()
  write-line(stream, line-contents(line), start: 0, end: line-length(line))
end method dump-line;


// A line containing arbitrary ("rich") text and images
define sealed class <rich-text-line> (<text-line>)
  sealed slot line-style-changes :: <vector> = #[],
    init-keyword: style-changes:;
end class <rich-text-line>;

define sealed domain make (singleton(<rich-text-line>));
define sealed domain initialize (<rich-text-line>);

// Lines start out with the window's default font and color $default-foreground,
// and all shifts happen explicitly (which is why there's no end index)
define sealed class <style-change> (<object>)
  // The index at which the font/color shift happens
  sealed slot style-change-index :: <integer>,
    required-init-keyword: index:;
  // The font, given as a <font>
  // Images are represented having this be an image instead of a font
  sealed slot style-change-font  = #f,		// use 'window-default-font'
    init-keyword: font:;
  // The color, given as a <color> or foreground/background
  sealed slot style-change-color = $default-foreground,
    init-keyword: color:;
end class <style-change>;

define sealed domain make (singleton(<style-change>));
define sealed domain initialize (<style-change>);

define sealed method copy-line
    (line :: <rich-text-line>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = line-length(line))
 => (new-line :: <rich-text-line>)
  let new-line = next-method();
  line-style-changes(new-line) := copy-sequence(line-style-changes(line));
  new-line
end method copy-line;

define sealed method dump-line
    (line :: <rich-text-line>, stream :: <stream>) => ()
  //---*** Dump 'line-style-changes' as well as the text
end method dump-line;


// Calls the function with three arguments: the character, the line, and the
// index of the character within the line
define sealed method do-characters
    (function :: <function>, line :: <text-line>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = line-length(line),
	  from-end? = #f, skip-test) => ()
  ignore(skip-test);
  let contents :: <byte-string> = line-contents(line);
  range-check(contents, line-length(line), _start, _end);
  let (_start :: <integer>, _end :: <integer>, _step :: <integer>)
    = if (from-end?) values(_end - 1, _start - 1, -1)
      else values(_start, _end, 1) end;
  without-bounds-checks
    for (i :: <integer> = _start then i + _step,
	 until: i = _end)
      let char = contents[i];
      function(char, line, i)
    end
  end
end method do-characters;

// Note that this does not include a trailing '\n' character!
define method as
    (class :: subclass(<string>), line :: <text-line>)
 => (string :: <byte-string>)
  let length   = line-length(line);
  let contents = line-contents(line);
  let string   = make(<byte-string>, size: length);
  // Use the fastest method available to copy the line contents
  copy-bytes(string, 0, contents, 0, length);
  string
end method as;


/// Diagram lines

// A line containing arbitrary text and/or diagrams.  Note that Deuce
// doesn't support editing of diagram lines, just their display.
define open abstract class <diagram-line> (<basic-line>)
end class <diagram-line>;

define sealed inline method diagram-line?
    (line :: <line>) => (diagram? :: <boolean>)
  #f
end method diagram-line?;

define sealed inline method diagram-line?
    (line :: <diagram-line>) => (diagram? :: <boolean>)
  #t
end method diagram-line?;

// The text length of a diagram line is zero
define method line-length
    (line :: <diagram-line>) => (length :: <integer>)
  0
end method line-length;

// The text contents of a diagram line is the null string
define method line-contents
    (line :: <diagram-line>) => (contents :: <byte-string>)
  ""
end method line-contents;

define method line-contents-setter
    (contents :: <byte-string>, line :: <diagram-line>)
 => (contents :: <byte-string>)
  error("There is no 'line-contents-setter' method for the diagram line %=", line)
end method line-contents-setter;

define method as
    (class :: subclass(<string>), line :: <diagram-line>)
 => (string :: <byte-string>)
  line-contents(line)
end method as;

// Diagram lines usually act as though they're empty
define method line-empty?
    (line :: <diagram-line>, #key index) => (empty? :: <boolean>)
  ignore(index);
  #t
end method line-empty?;


define method do-characters
    (function :: <function>, line :: <diagram-line>,
     #key start: _start, end: _end, from-end?, skip-test) => ()
  ignore(_start, _end, from-end?, skip-test);
  error("There is no 'do-characters' method for the diagram line %=", line)
end method do-characters;

define sealed method dump-line
    (line :: <diagram-line>, stream :: <stream>) => ()
  error("There is no 'dump-line' method for the diagram line %=", line)
end method dump-line;

define method gc-line
    (line :: <diagram-line>) => ()
  // Nothing to GC for the basic <diagram-line> class
  #f
end method gc-line;


// A diagram line used for display only.
// Note that client-level navigation functions (such as 'line-next-in-buffer')
// generally skip over structural diagram lines.
define open abstract class <structural-diagram-line> (<diagram-line>)
end class <structural-diagram-line>;

define sealed inline method structural-diagram-line?
    (line :: <line>) => (sdl? :: <boolean>)
  #f
end method structural-diagram-line?;

define sealed inline method structural-diagram-line?
    (line :: <structural-diagram-line>) => (sdl? :: <boolean>)
  #t
end method structural-diagram-line?;

define sealed method do-characters
    (function :: <function>, line :: <structural-diagram-line>,
     #key start: _start, end: _end, from-end?, skip-test) => ()
  ignore(_start, _end, from-end?, skip-test);
  #f
end method do-characters;

// By default, structural diagram lines don't get saved.
define sealed method dump-line
    (line :: <structural-diagram-line>, stream :: <stream>) => ()
  #f
end method dump-line;

define sealed method line-for-display-only?
    (line :: <structural-diagram-line>) => (display-only? :: <boolean>)
  #t
end method line-for-display-only?;
