Module:    motif-duim
Synopsis:  Motif font mapping implementation
Author:    Scott McKay, Stuart Croy
	   Based on work by John Aspinall and Richard Billington
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Motif font management

define sealed class <motif-font> (<object>)
  sealed slot %font-name :: <string>,
    required-init-keyword: name:;
  sealed slot %font-id :: <integer> = 0;
  sealed slot %font-struct = #f;
end class <motif-font>;

define sealed domain make (singleton(<motif-font>));
define sealed domain initialize (<motif-font>);


define abstract class <font-error> (<error>)
end class <font-error>;

define abstract class <font-name-parse-error> (<font-error>)
  sealed constant slot %font-name, required-init-keyword: name:;
end class <font-name-parse-error>;

define sealed class <font-name-private-font-registry> (<font-name-parse-error>)
end class <font-name-private-font-registry>;

define method condition-to-string
    (condition :: <font-name-private-font-registry>) => (string :: <string>)
  format-to-string("Font name %s is from a private registry",
		   condition.%font-name)
end method condition-to-string;

define sealed class <font-name-numeric-field-non-numeric> (<font-name-parse-error>)
  sealed constant slot %start, required-init-keyword: start:;
  sealed constant slot %end,   required-init-keyword: end:;
  sealed constant slot %token, required-init-keyword: token:;
end class <font-name-numeric-field-non-numeric>;

define method condition-to-string
    (condition :: <font-name-numeric-field-non-numeric>) => (string :: <string>)
  format-to-string("Font name %s should have had an integer in %d..%d while looking for %s)",
		   condition.%font-name, condition.%start, condition.%end, condition.%token)
end method condition-to-string;


/// Decoding X font names

define sealed method disassemble-x-font-name
    (font-name :: <byte-string>) =>
 => (registry, foundry, family, weight, slant, set-width, add-style,
     pixel-size, point-size, horiz-dpi, vert-dpi,
     spacing, avg-width, char-set)
  let prev-pos :: <integer> = 1;
  let dash-pos :: <integer> = 0;
  local method one-token (token :: <byte-string>) => (token :: false-or(<byte-string>))
	  ignore(token);
	  dash-pos := position(font-name, '-', start: prev-pos);
	  let result = if (prev-pos = dash-pos) #f
		       else copy-sequence(font-name, start: prev-pos, end: dash-pos) end;
	  prev-pos := dash-pos + 1;
	  result
	end method,
        method one-integer (token :: <byte-string>) => (integer :: false-or(<integer>))
	  dash-pos := position(font-name, '-', start: prev-pos);
	  let string = copy-sequence(font-name, start: prev-pos, end: dash-pos);
	  let result = string-to-integer(string);
	  if (result)
	    prev-pos := dash-pos + 1;
	    result
	  else
	    error(make(<font-name-numeric-field-non-numeric>,
		       name: font-name,
		       start: prev-pos, end: dash-pos,
		       token: token))
	  end
	end method,
        method last-token (token :: <byte-string>) => (token :: <byte-string>)
	  ignore(token);
	  copy-sequence(font-name, start: prev-pos)
	end method;
  let registry
    = begin
	let char0 = font-name[0];
	case
	  char0 = '-' =>
	    prev-pos := 1;
	    #t;
	  char0 = '+' =>
	    one-token("registry");
	  otherwise =>
	    error(make(<font-name-private-font-registry>,
		       name: font-name))
	end
      end;
  let foundry    = one-token("foundry");
  let family     = one-token("family");
  let weight     = one-token("weight");
  let slant      = one-token("slant");
  let set-width  = one-token("set-width");
  let add-style  = one-token("add-style");
  let pixel-size = one-integer("pixel-size");
  let point-size = one-integer("point-size");
  let horiz-dpi  = one-integer("horizontal-dpi");
  let vert-dpi   = one-integer("vertical-dpi");
  let spacing    = one-token("spacing");
  let avg-width  = one-integer("average-width");
  let char-set   = last-token("char-set");
  values(registry, foundry, family, weight, slant, set-width, add-style,
	 pixel-size, point-size, horiz-dpi, vert-dpi,
	 spacing, avg-width, char-set)
end method disassemble-x-font-name;

define sealed method disassemble-x-font-name
    (registry, foundry, family, weight, slant, set-width, add-style,
     pixel-size, point-size, horiz-dpi, vert-dpi,
     spacing, avg-width, char-set)
 => (font-name :: <byte-string>)
  //---*** WHAT DOES ~@[~A~] DO???
  format-to-string("-%s-%s-%s-%s-%s-~@[~A~]-%d-%d-%d-%d-%s-%d-%s",
		   foundry, family, weight, slant, set-width, add-style,
		   pixel-size, point-size, horiz-dpi, vert-dpi,
		   spacing, average-width, char-set)
end method disassemble-x-font-name;


/// Font mapping

define constant $motif-font-families :: <list>
  = #(#(#"fix",        "courier"),
      #(#"sans-serif", "helvetica"),
      #(#"serif",      "times", "charter", "new century schoolbook"),
      #(#"symbol",     "symbol"));

//--- We should compute the numbers based on either device characteristics
//--- or some user option
define constant $motif-logical-sizes :: <simple-object-vector>
    = #[#[#"normal",     10],	// put most common one first for efficiency
	#[#"small",       8],
	#[#"large",      12],
	#[#"very-small",  6],
	#[#"very-large", 14],
	#[#"tiny",        5],
	#[#"huge",       18]];

define method install-default-text-style-mappings
    (_port :: <motif-port>) => ()
  for (entry in $motif-font-families)
    let duim-family = head(entry);
    let x-families  = tail(entry);
    block (break)
      when (size(x-families) > 1)
	for (x-family in x-families)
	  when (can-install-entire-family?(_port, duim-family, x-family))
	    break(install-font-family(_port, duim-family, x-family))
	  end
	end
      end;
      for (x-family in x-families)
	install-font-family(_port, duim-family, x-family)
      end
    end
  end
end method install-default-text-style-mappings;
      
define method can-install-entire-family?
    (_port :: <motif-port>, duim-family, x-family :: <byte-string>)
 => (can-install? :: <boolean>)
  let x-display    = %port.%display;
  let font-matches = x/XListFonts(x-display, format-to-string("-*-%s-*", x-family));
  let nsizes       = length($motif-logical-sizes);
  let fixed-match  = make(<array>, dimensions: vector(4, nsizes));
  let scaled-match = make(<vector>, size: 4);
  let horiz-dpi    = #f;
  let vert-dpi     = #f;
  for (font-name in font-matches)
    let (registry, foundry, family, weight, slant, set-width, add-style,
	 pixel-size, point-size, h-dpi, v-dpi, spacing, avg-width, char-set)
      = disassemble-x-font-name(font-name);
    ignore(registry, foundry, family, set-width, add-style, spacing, char-set);
    when (~horiz-dpi & h-dpi > 0 & ~vert-dpi & v-dpi > 0)
      horiz-dpi := h-dpi;
      vert-dpi  := v-dpi
    end;
    let face-code :: <integer>
      = logior(if (string-equal(weight, "bold")) 1 else 0 end,
	       if (string-equal(slant, "i") | string-equal(slant, "o")) 2 else 0 end);
    let scalable? = (pixel-size = 0 & avg-width = 0);
    if (scalable?)
      scaled-match[face-code] := font-name
    else
      let index = position($motif-logical-sizes, round/(point-size, 10),
			   test: method (entry, s) second(entry) = s end);
      when (index)
	fixed-match[face-code, index] := font-name
      end
    end
  end;
  block (return)
    when (horiz-dpi & vert-dpi)
      for (face-code :: <integer> from 0 below 4)
	unless (scaled-match[face-code]
		| for (size :: <integer> from 0 below n-sizes)
		    unless (fixed-match[face-code, size])
		      return(#f)
		    end
		  end)
	  return(#f)
	end
      end;
      #t
    end
  end
end method can-install-entire-family?;

define method install-font-family
    (_port :: <motif-port>, duim-family, x-family :: <byte-string>) => ()
  let x-display    = %port.%display;
  let font-matches = x/XListFonts(x-display, format-to-string("-*-%s-*", x-family));
  let scaled-match = #();
  let horiz-dpi    = #f;
  let vert-dpi     = #f;
  for (font-name in font-matches)
    let (registry, foundry, family, weight, slant, set-width, add-style,
	 pixel-size, point-size, h-dpi, v-dpi, spacing, avg-width, char-set)
      = disassemble-x-font-name(font-name);
    ignore(registry, foundry, family, set-width, add-style, spacing, char-set);
    when (~horiz-dpi & h-dpi > 0 & ~vert-dpi & v-dpi > 0)
      horiz-dpi := h-dpi;
      vert-dpi  := v-dpi
    end;
    let weight = if (string-equal(weight, "bold")) #"bold" else #"normal" end;
    let slant  = if (string-equal(slant, "i") | string-equal(slant, "o")) #"italic" else #"roman" end;
    let scalable? = (pixel-size = 0 & avg-width = 0);
    if (scalable?)
      push!(scaled-match, vector(weight, slant, font-name))
    else
      let index = position($motif-logical-sizes, round/(point-size, 10),
			   test: method (entry, s) second(entry) = s end);
      let size  = index & first($motif-logical-sizes[index]);
      when (size)
	let text-style = make-text-style(duim-family, weight, slant, size);
	unless (text-style-mapping-exists?(_port, text-style, exact-size?: #t))
	  text-style-mapping(_port, text-style) := font-name
	end
      end
    end
  end;
  // Now add any scaleable font mappings
  for (entry in scaled-match)
    let weight    = entry[0];
    let slant     = entry[1];
    let font-name = entry[2];
    for (entry in $motif-logical-sizes)
      let size   = entry[0];
      let points = entry[1];
      let text-style = make-text-style(duim-family, weight, slant, size);
      unless (text-style-mapping-exists?(_port, text-style, exact-size?: #t))
	text-style-mapping(_port, text-style)
	  := scaleable-font-name-at-size(font-name, points, horiz-dpi, vert-dpi)
      end
    end
  end
end method install-font-family;

define method scaleable-font-name-at-size
    (font-name :: <byte-string>, point-size :: <integer>,
     horiz-dpi :: <integer>, vertical-dpi :: <integer>)
 => (font-name :: <integer>)
  let (registry, foundry, family, weight, slant, set-width, add-style,
       pixel-size, pt-size, h-dpi, v-dpi, spacing, avg-width, char-set)
  = disassemble-x-font-name(font-name);
  ignore(registry, pt-size, h-dpi, v-dpi);
  assemble-font-name(foundry, family, weight, slant, set-width, add-style,
		     pixel-size, (* point-size 10), horiz-dpi, vert-dpi,
		     spacing, average-width, char-set)
end method scaleable-font-name-at-size;


define sealed method do-text-style-mapping
    (_port :: <motif-port>, text-style :: <standard-text-style>, character-set)
 => (font :: <motif-font>)
  ignore(character-set);
  let text-style
    = standardize-text-style(_port, text-style,
			     character-set: character-set);
  let table :: <object-table> = port-font-mapping-table(_port);
  let font = gethash(table, text-style);
  font
    | begin
	//---*** DO THE REST OF THIS
	let font = make(<motif-font>, ...);
	gethash(table, text-style) := font;
	font;
      end
end method do-text-style-mapping;

//--- This approach seems unnecessarily clumsy; we might as well just have 
//--- 'do-text-style-mapping' do the table lookup directly itself.  We shouldn't
//--- need to cons up a whole new text-style object just to map the size.
define sealed method standardize-text-style
    (_port :: <motif-port>, text-style :: <standard-text-style>,
     #rest keys, #key character-set)
 => (text-style :: <text-style>)
  apply(standardize-text-style-size,
	_port, text-style, $motif-logical-sizes, keys)
end method standardize-text-style;


/// Font metrics

define sealed inline method font-width
    (text-style :: <text-style>, _port :: <motif-port>,
     #rest keys, #key character-set)
 => (width :: <integer>)
  let (font, width, height, ascent, descent)
    = apply(font-metrics, text-style, _port, keys);
  ignore(font, height, ascent, descent);
  width
end method font-width;

define sealed inline method font-height
    (text-style :: <text-style>, _port :: <motif-port>,
     #rest keys, #key character-set)
 => (height :: <integer>)
  let (font, width, height, ascent, descent)
    = apply(font-metrics, text-style, _port, keys);
  ignore(font, width, ascent, descent);
  height
end method font-height;

define sealed inline method font-ascent
    (text-style :: <text-style>, _port :: <motif-port>,
     #rest keys, #key character-set)
 => (ascent :: <integer>)
  let (font, width, height, ascent, descent)
    = apply(font-metrics, text-style, _port, keys);
  ignore(font, width, height, descent);
  ascent
end method font-ascent;

define sealed inline method font-descent
    (text-style :: <text-style>, _port :: <motif-port>,
     #rest keys, #key character-set)
 => (descent :: <integer>)
  let (font, width, height, ascent, descent)
    = apply(font-metrics, text-style, _port, keys);
  ignore(font, width, height, ascent);
  descent
end method font-descent;

define sealed inline method fixed-width-font?
    (text-style :: <text-style>, _port :: <motif-port>, #key character-set)
 => (fixed? :: <boolean>)
  let font   = font-metrics(text-style, _port);
  let struct = font.%font-struct;
  empty?(struct.x/per-char-value)
end method fixed-width-font?;

define sealed method font-metrics
    (text-style :: <text-style>, _port :: <motif-port>,
     #rest keys, #key character-set)
 => (font,
     width :: <integer>, height :: <integer>, ascent :: <integer>, descent :: <integer>)
  let font :: <motif-font>
    = apply(text-style-mapping, _port, text-style, keys);
  motif-font-metrics(font, _port)
end method font-metrics;

define sealed method motif-font-metrics
    (font :: <motif-font>, _port :: <motif-port>)
 => (font :: <motif-font>,
     width :: <integer>, height :: <integer>, ascent :: <integer>, descent :: <integer>)
  unless (font.%font-struct)
    let x-display = port.%display;
    font.%font-id     := x/XLoadFont(x-display, font.%font-name);
    font.%font-struct := x/XQueryFont(x-display, font.%font-id)
  end;
  let struct = font.%font-struct;
  values(font,
	 struct.x/max-bounds-value.x/width-value,
	 struct.x/ascent-value + struct.x/descent-value,
	 struct.x/ascent-value,
	 struct.x/descent-value)
end method motif-font-metrics;


/// Text measurement

define sealed method text-size
    (_port :: <motif-port>, char :: <character>,
     #key text-style :: <text-style> = $default-text-style,
          start: _start, end: _end, do-newlines? = #f, do-tabs? = #f)
 => (largest-x :: <integer>, largest-y :: <integer>,
     cursor-x :: <integer>, cursor-y :: <integer>, baseline :: <integer>)
  ignore(_start, _end, do-newlines?, do-tabs?);
  let string = make(<string>, size: 1, fill: char);
  text-size(_port, string, text-style: text-style)
end method text-size;

//---*** What do we do about Unicode strings?
define sealed method text-size
    (_port :: <motif-port>, string :: <string>,
     #key text-style :: <text-style> = $default-text-style,
          start: _start, end: _end, do-newlines? = #f, do-tabs? = #f)
 => (largest-x :: <integer>, largest-y :: <integer>,
     cursor-x :: <integer>, cursor-y :: <integer>, baseline :: <integer>)
  let length :: <integer> = size(string);
  let _start :: <integer> = _start | 0;
  let _end   :: <integer> = _end   | length;
  let (font :: <win32-font>, width, height, ascent, descent)
    = font-metrics(text-style, _port);
  ignore(width, height);
  local method measure-string
	    (font :: <win32-font>, string :: <string>, _start :: <integer>, _end :: <integer>)
	 => (x1 :: <integer>, y1 :: <integer>, x2 :: <integer>, y2 :: <integer>)
	  let substring
	    = if (_start = 0 & _end = size(string)) string
	      else copy-sequence(string, start: _start, end: _end) end;
	  with-c-string (c-string = substring)
	    with-stack-structure (overall :: x/<XCharStruct>)
	      let (direction, max-ascent, max-descent)
		= x/XTextExtents(font.%font-struct, substring, overall);
	      ignore(direction, max-ascent, max-descent);
	      //---*** IS THIS REALLY RIGHT?
	      values(0, overall.x/width-value,
		     0, overall.x/ascent-value + overall.x/descent-value)
	    end
	  end
	end method;
  case
    do-tabs? & do-newlines? =>
      next-method();		// the slow case...
    do-tabs? =>
      let tab-width :: <integer> = width * 8;
      let last-x    :: <integer> = 0;
      let last-y    :: <integer> = 0;
      let s         :: <integer> = _start;
      block (return)
	while (#t)
	  let e = position(string, '\t', start: s, end: _end);
	  let (x1, y1, x2, y2) = measure-string(font, string, s, e);
	  ignore(x1);
	  if (e = _end)
	    last-x := last-x + x2
	  else
	    last-x := floor/(last-x + x2 + tab-width, tab-width) * tab-width;
	  end;
	  max!(last-y, y2 - y1);
	  s := min(e + 1, _end);
	  when (e = _end)
	    return(last-x, last-y, last-x, last-y, ascent)
	  end
	end
      end;
    do-newlines? =>
      let largest-x :: <integer> = 0;
      let largest-y :: <integer> = 0;
      let last-x    :: <integer> = 0;
      let last-y    :: <integer> = 0;
      let s         :: <integer> = _start;
      block (return)
	while (#t)
	  let e = position(string, '\n', start: s, end: _end);
	  let (x1, y1, x2, y2) = measure-string(font, string, s, e);
	  ignore(x1);
	  max!(largest-x, x2);
	  last-x := x2;
	  inc!(largest-y, y2 - y1);
	  last-y := y2;
	  s := min(e + 1, _end);
	  when (e = _end)
	    return(largest-x, largest-y, last-x, last-y, ascent)
	  end
	end
      end;
    otherwise =>
      let (x1, y1, x2, y2) = measure-string(font, string, _start, _end);
      ignore(x1);
      values(x2, y2 - y1, x2, y2 - y1, ascent);
  end
end method text-size;
