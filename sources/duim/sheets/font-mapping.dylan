Module:       duim-sheets-internals
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Font mapping

define protocol <<font-mapping-protocol>> (<<text-style-protocol>>)
  // Mapping text styles to fonts
  function text-style-mapping
    (port :: <abstract-port>, style :: <text-style>, #key character-set)
 => (font);
  function do-text-style-mapping 
    (port :: <abstract-port>, style :: <text-style>, character-set) => (font);
  function text-style-mapping-setter
    (mapping, port :: <abstract-port>, style :: <text-style>, #key character-set)
 => (mapping);
  function text-style-mapping-exists?
    (port :: <abstract-port>, style :: <text-style>, #key character-set, exact-size?)
 => (true? :: <boolean>);
  function standardize-text-style
    (port :: <abstract-port>, style :: <text-style>, #key character-set)
 => (style :: <text-style>);
  function standardize-text-style-size
    (port :: <abstract-port>, style :: <text-style>, size-alist, #key character-set)
 => (style :: <text-style>);
end protocol <<font-mapping-protocol>>;

define protocol <<font-protocol>> (<<text-style-protocol>>)
  // Font metrics, which get done in the back end
  function font-metrics
    (text-style :: <text-style>, port :: <abstract-port>, #key character-set)
 => (font, width :: <real>, height :: <real>, ascent :: <real>, descent :: <real>);
  function font-width
    (text-style :: <text-style>, port :: <abstract-port>, #key character-set)
 => (width :: <real>);
  function font-height
    (text-style :: <text-style>, port :: <abstract-port>, #key character-set)
 => (height :: <real>);
  function font-ascent
    (text-style :: <text-style>, port :: <abstract-port>, #key character-set)
 => (ascent :: <real>);
  function font-descent
    (text-style :: <text-style>, port :: <abstract-port>, #key character-set)
 => (descent :: <real>);
  function fixed-width-font?
    (text-style :: <text-style>, port :: <abstract-port>, #key character-set)
 => (true? :: <boolean>);
  // Text measurement
  function text-size
    (drawable :: type-union(<abstract-port>, <drawable>), string-or-char,
     #key text-style, start: _start, end: _end, do-newlines?, do-tabs?)
 => (largest-x :: <real>, largest-y :: <real>,
     cursor-x :: <real>, cursor-y :: <real>, baseline :: <real>);
  function glyph-for-character
    (port :: <abstract-port>, char :: <character>, text-style :: <text-style>, #key font)
 => (index :: <integer>, font,
     escapement-x :: <real>, escapement-y :: <real>,
     origin-x :: <real>, origin-y :: <real>, bb-x :: <real>, bb-y :: <real>);
end protocol <<font-protocol>>;


/// Text style mapping

// Return a key into the mapping tables that ignores the text style size
define inline function make-text-style-key (style) => (key)
  let (family, name, weight, slant, size, underline?, strikeout?)
    = text-style-components(style);
  ignore(size);
  make-text-style(family, name, weight, slant, #f,
		  underline?: underline?, strikeout?: strikeout?)
end function make-text-style-key;

define method text-style-mapping-setter
    (font, _port :: <basic-port>, style :: <standard-text-style>,
     #key character-set = $standard-character-set) => (font)
  style := standardize-text-style(_port, style, character-set: character-set);
  let table :: <object-table> = port-font-mapping-table(_port);
  let cache :: <pair> = port-font-mapping-cache(_port);
  head(cache) := #f;		// invalidate the key first...
  tail(cache) := #f;		// ...so nothing else will see a valid cache
  case
    _port.%text-style-size-mapping == #"exact" =>
      gethash(table, style) := font;
    _port.%text-style-size-mapping == #"loose" =>
      let key = make-text-style-key(style);
      // Each entry in the hash table has a stretchy vector of
      // (text-style, font) pairs sorted by their numeric size
      let fonts :: <stretchy-object-vector> = gethash(table, key) | make(<stretchy-vector>);
      let old-entry = find-pair(fonts, style);
      case
	old-entry =>
	  old-entry[1] := font;
	otherwise =>
	  add!(fonts, list(style, font));
	  fonts
	    := sort!(fonts,
		     test: method (e1, e2)
			     text-style-size(e1[0]) < text-style-size(e2[0])
			   end);
	  gethash(table, key) := fonts;
      end;
    _port.%text-style-size-mapping == #"scalable" =>
      // If it's scalable, we only want to cache based on the family,
      // weight, and slant
      let key = make-text-style-key(style);
      gethash(table, key) := font
  end;
  font
end method text-style-mapping-setter;

define sealed class <undefined-text-style-mapping> (<error>)
  sealed constant slot %the-port,
    required-init-keyword: port:;
  sealed constant slot %text-style,
    required-init-keyword: text-style:;
end class <undefined-text-style-mapping>;

define method condition-to-string
    (condition :: <undefined-text-style-mapping>) => (string :: <string>)
  format-to-string("The text style %= has no mapping for the port type %=",
		   condition.%text-style, port-type(condition.%the-port))
end method condition-to-string;

define method text-style-mapping
    (_port :: <basic-port>, style :: <text-style>,
     #key character-set = $standard-character-set) => (font)
  let cache = port-font-mapping-cache(_port);
  if (style == head(cache))
    tail(cache)			// one-element cache hit
  else
    let font = do-text-style-mapping(_port, style, character-set);
    unless (font)
      error(make(<undefined-text-style-mapping>,
		 port: _port, text-style: style))
    end;
    head(cache) := #f;		// ensure nobody sees a valid cache...
    tail(cache) := font;
    head(cache) := style;
    font
  end
end method text-style-mapping;

define method text-style-mapping
    (_port :: <basic-port>, style :: <device-font>,
     #key character-set = $standard-character-set) => (font)
  ignore(character-set);
  //--- 'type-equal'?  This is too restrictive as it stands...
  assert(_port == device-font-port(style),
	 "The device font %= was defined for port %= "
	 "but is being used on port %=",
	 style, device-font-port(style), _port);
  device-font-font(style)
end method text-style-mapping;

define method do-text-style-mapping
    (_port :: <basic-port>, style :: <standard-text-style>, character-set) => (font)
  style := standardize-text-style(_port, style, character-set: character-set);
  let mapping-type = _port.%text-style-size-mapping;
  let table :: <object-table> = port-font-mapping-table(_port);
  let result
    = select (mapping-type)
        #"exact" =>
          gethash(table, style);
        #"loose" =>
          find-closest-font(style, table);
        #"scalable" =>
	  let key = make-text-style-key(style);
	  gethash(table, key);
	otherwise => #f;
      end
      | select (mapping-type)
          #"exact" =>
            gethash(table, port-undefined-text-style(_port));
          #"loose" =>
            find-closest-font(port-undefined-text-style(_port), table);
          #"scalable" =>
	    let key = make-text-style-key(style);
	    gethash(table, key);
	  otherwise => #f;
        end;
  when (text-style?(result))	// logical translations
    result := do-text-style-mapping(_port, result, character-set)
  end;
  result
end method do-text-style-mapping;

define method text-style-mapping-exists?
    (_port :: <basic-port>, style :: <text-style>,
     #key character-set = $standard-character-set, exact-size?) => (true? :: <boolean>)
  style := standardize-text-style(_port, style, character-set: character-set);
  let mapping-type = _port.%text-style-size-mapping;
  let table = port-font-mapping-table(_port);
  let result
    = select (mapping-type)
        #"exact" =>
          gethash(table, style);
        #"loose" =>
          find-closest-font(style, table, exact-size?: exact-size?);
        #"scalable" =>
	  let key = make-text-style-key(style);
	  gethash(table, key);
	otherwise => #f;
      end;
  case
    ~result => #f;
    text-style?(result) =>	// logical translations
      text-style-mapping-exists?(_port, style, character-set: character-set);
    otherwise => #t;
  end
end method text-style-mapping-exists?;

define method find-closest-font
    (style :: <text-style>, mapping-table, #key exact-size?) => (font)
  let key = make-text-style-key(style);
  let size = text-style-size(style);
  let entries = gethash(mapping-table, key);
  let last-entry = #f;
  let last-size = #f;
  when (entries)
    block (return)
      for (entry in entries)
        let font-size = text-style-size(entry[0]);
        if (exact-size?)
          when (size = font-size)
            return(entry[1])
          end
        else
          when (size <= font-size)
            case
              ~last-entry =>
                return(entry[1]);
              abs(size - font-size) < abs(size - last-size) =>
                return(entry[1]);
	      otherwise =>
		return(last-entry[1]);
            end
          end
        end;
        last-entry := entry;
        last-size := font-size;
      finally
        when (~empty?(entries) & ~exact-size?)
          last(entries)[1]
        end
      end
    end
  end
end method find-closest-font;

// This method allows the device to convert logical sizes into point
// sizes, etc.  The default method doesn't do much of anything.
define method standardize-text-style
    (_port :: <basic-port>, style :: <text-style>, #key character-set)
 => (style :: <text-style>)
  ignore(character-set);
  unless (instance?(text-style-size(style), <number>))
    standardize-text-style-error(style)
  end;
  style
end method standardize-text-style;

// For use by more specific 'standardize-text-style' methods
define method standardize-text-style-size
    (_port :: <basic-port>, style :: <text-style>, size-alist,
     #key character-set) => (style :: <text-style>)
  ignore(character-set);
  let size = text-style-size(style);
  if (instance?(size, <number>))
    style
  else
    let new-size = find-pair(size-alist, size);
    case
      new-size =>
	make-text-style(text-style-family(style),
			text-style-name(style),
			text-style-weight(style),
			text-style-slant(style),
			second(new-size));
      otherwise =>
	standardize-text-style-error(style);
    end
  end
end method standardize-text-style-size;

define method standardize-text-style-error
    (text-style :: <text-style>) => (text-style :: <text-style>)
  if (fully-merged-text-style?(text-style))
    cerror("Use the undefined text style stand-in instead",
	   "The size component of %= is not numeric.  This port does not know "
	   "how to map logical text style sizes", text-style)
  else
    cerror("Use the undefined text style stand-in instead",
	   "The text style %= must be a fully merged text style", text-style)
  end;
  $undefined-text-style
end method standardize-text-style-error;


/// Font metrics and text measurement

// For now, only standard character set characters are understood...
define inline function index-and-character-set
    (character :: <character>) => (index :: <integer>, character-set)
  values(as(<integer>, character), $standard-character-set)
end function index-and-character-set;

// Return the glyph, font, and all measurements for a character
// Default method
define method glyph-for-character
    (_port :: <port>, char :: <character>, text-style :: <text-style>, #key font)
 => (index :: <integer>, font,
     escapement-x :: <real>, escapement-y :: <real>,
     origin-x :: <real>, origin-y :: <real>, bb-x :: <real>, bb-y :: <real>);
  let (index, char-set) = index-and-character-set(char);
  let (_font, width, height, ascent, descent)
    = font-metrics(text-style, _port, character-set: char-set);
  ignore(descent);
  let font = font | _font;
  let escapement-x = width;
  let escapement-y = 0;
  let origin-x = 0;
  let origin-y = ascent;
  let bb-x = escapement-x;	// not available yet
  let bb-y = height;
  values(index, font, escapement-x, escapement-y, origin-x, origin-y, bb-x, bb-y)
end method glyph-for-character;


// First two values are the width and height of the box surrounding the text.
// The next two values are the final X and Y position of the "cursor".
// The last value is the baseline.
define method text-size
    (medium :: <basic-medium>, char :: <character>,
     #rest keys,
     #key text-style :: <text-style> = medium-merged-text-style(medium),
          start: _start, end: _end, do-newlines? = #f, do-tabs? = #f)
 => (largest-x :: <real>, largest-y :: <real>,
     cursor-x :: <real>, cursor-y :: <real>, baseline :: <real>)
  ignore(_start, _end, do-newlines?, do-tabs?);
  // Ask the port to do the real work...
  apply(text-size, port(medium), char, text-style: text-style, keys)
end method text-size;

define method text-size
    (medium :: <basic-medium>, string :: <string>,
     #rest keys,
     #key text-style :: <text-style> = medium-merged-text-style(medium),
          start: _start, end: _end, do-newlines? = #f, do-tabs? = #f)
 => (largest-x :: <real>, largest-y :: <real>,
     cursor-x :: <real>, cursor-y :: <real>, baseline :: <real>)
  ignore(_start, _end, do-newlines?, do-tabs?);
  // Ask the port to do the real work...
  apply(text-size, port(medium), string, text-style: text-style, keys)
end method text-size;

// Provide a default method for 'text-size' in case back-ends don't have a
// primitive for measuring a whole string
define method text-size
    (_port :: <basic-port>, char :: <character>,
     #key text-style :: <text-style> = $default-text-style,
          start: _start, end: _end, do-newlines? = #f, do-tabs? = #f)
 => (largest-x :: <real>, largest-y :: <real>,
     cursor-x :: <real>, cursor-y :: <real>, baseline :: <real>)
  ignore(_start, _end, do-newlines?);
  if (do-tabs? & char == '\t')
    let (index, font, escapement-x, escapement-y, origin-x, origin-y, bb-x, bb-y)
      = glyph-for-character(_port, ' ', text-style);
    ignore(index, font, origin-x);
    values(bb-x * 8, bb-y, escapement-x * 8, escapement-y, origin-y)
  else
    let (index, font, escapement-x, escapement-y, origin-x, origin-y, bb-x, bb-y)
      = glyph-for-character(_port, char, text-style);
    ignore(index, font, origin-x);
    values(bb-x, bb-y, escapement-x, escapement-y, origin-y)
  end
end method text-size;

// Provide a default method for 'text-size' in case back-ends don't have a
// primitive for measuring a whole string
define method text-size
    (_port :: <basic-port>, string :: <string>,
     #key text-style :: <text-style> = $default-text-style,
          start: _start, end: _end, do-newlines? = #f, do-tabs? = #f)
 => (largest-x :: <real>, largest-y :: <real>,
     cursor-x :: <real>, cursor-y :: <real>, baseline :: <real>)
  let length :: <integer> = size(string);
  let _start :: <integer> = _start | 0;
  let _end   :: <integer> = _end   | length;
  let largest-x = 0;
  let largest-y = 0;
  let last-x = 0;
  let last-y = 0;
  let (font, width, height, ascent, descent) = font-metrics(text-style, _port);
  ignore(font, ascent);
  let baseline = height - descent;
  let tab-width = width * 8;
  range-check(string, length, _start, _end);
  without-bounds-checks
    for (i :: <integer> from _start below _end)
      let char = string[i];
      case
	// By default, treat Newline and Return the same
	do-newlines? & (char == '\n' | char == '\r') =>
	  inc!(largest-y, height);
	  inc!(last-y, height);
	  last-x := 0;
	do-tabs? & (char == '\t') =>
	  last-x := floor/(last-x + tab-width, tab-width) * tab-width;
	  max!(largest-x, last-x);
	  max!(largest-y, height);
	otherwise =>
	  let (index, font, escapement-x, escapement-y, origin-x, origin-y, bb-x, bb-y)
	    = glyph-for-character(_port, char, text-style);
	  ignore(index, font, escapement-y, origin-x, origin-y, bb-x);
	  inc!(last-x, escapement-x);
	  max!(largest-x, last-x);
	  max!(largest-y, bb-y);
      end
    end
  end;
  values(largest-x, largest-y, last-x, last-y, baseline)
end method text-size;


define method compute-text-adjustment
    (medium :: <basic-medium>, text, text-style :: <text-style>,
     align-x, align-y, #rest keys, #key, #all-keys)
 => (x-adjust :: <integer>, y-adjust :: <integer>)
  dynamic-extent(keys);
  let x-adjust
    = select (align-x)
	#"left"  => 0;
	#"right" => -apply(text-size, medium, text, text-style: text-style, keys);
	#"center", #"centre" =>
	  -floor/(apply(text-size, medium, text, text-style: text-style, keys), 2);
      end;
  let (font, width, height, ascent, descent)
    = font-metrics(text-style, port(medium));
  ignore(font, width);
  let y-adjust
    = select (align-y)
	#"baseline" => 0;
	#"top"      => ascent;
	#"bottom"   => -descent;
	#"center", #"centre" => ascent - floor/(height, 2)
      end;
  values(floor(x-adjust), floor(y-adjust))
end method compute-text-adjustment;


/// Trampolines from sheets to mediums

define method sheet-line-height
    (sheet :: <sheet>) => (height :: <integer>)
  with-sheet-medium (medium = sheet)
    let text-style = medium-merged-text-style(medium);
    floor(font-ascent(text-style, port(medium)))
  end
end method sheet-line-height;

define method sheet-line-spacing
    (sheet :: <sheet>) => (spacing :: <integer>)
  with-sheet-medium (medium = sheet)
    let text-style = medium-merged-text-style(medium);
    floor(font-descent(text-style, port(medium)))
  end
end method sheet-line-spacing;
