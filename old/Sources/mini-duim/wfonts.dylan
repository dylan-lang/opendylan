Module:    win32-duim
Synopsis:  Win32 font mapping implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Win32 font management

define sealed class <win32-font> (<object>)
  slot %font-handle :: <HFONT>,
    required-init-keyword: handle:;
  slot %font-text-style :: false-or(<standard-text-style>) = #f,
    init-keyword: text-style:;
  slot %font-height    :: <integer> = 0;
  slot %font-width     :: <integer> = 0;
  slot %font-max-width :: <integer> = 0;
  slot %font-ascent    :: <integer> = 0;
  slot %font-descent   :: <integer> = 0;
end class <win32-font>;

define method do-text-style-mapping
    (_port :: <win32-port>, text-style :: <standard-text-style>, character-set)
 => (font :: <win32-font>)
  ignore(character-set);
  text-style := standardize-text-style(_port, text-style,
				       character-set: character-set);
  let table :: <table> = port-font-mapping-table(_port);
  let font = gethash(table, text-style);
  if (font)
    font
  else
    let (family, weight, slant, size, underline?, strikeout?)
      = text-style-components(text-style);
    let charset =
      select (character-set)
	#f => $DEFAULT-CHARSET;
	//--- Are these reasonable values for the character set?
	#"ANSI", #"ISO-Latin-1", #"Windows", #"ASCII" => $ANSI-CHARSET;
	#"OEM", #"PC" =>  $OEM-CHARSET;
	#"symbol" => $SYMBOL-CHARSET;
	//--- Later, add support for non-Latin character sets
	otherwise => $DEFAULT-CHARSET;
      end;
    let weight
      = select (weight)
	  // Standard values:
	  #"normal" 	=> 400;
	  #"bold"   	=> 700;
	  // Other values, following Windows terminology:
	  #"thin"   	=> 100;
	  #"extra-light"=> 200;
	  #"light" 	=> 300;
	  #"medium" 	=> 500;
	  #"demibold" 	=> 600;
	  #"extra-bold" => 800;
	  #"black"  	=> 900;
	  otherwise	=> 400;
	end;
    let pitch-and-family
      = select (family)
	  #"fix"        => logior($FIXED-PITCH,    $FF-MODERN);
	  #"serif"      => logior($VARIABLE-PITCH, $FF-ROMAN);
	  #"sans-serif" => logior($DEFAULT-PITCH,  $FF-SWISS);
	  #"symbol"	=> charset := $SYMBOL-CHARSET;
	                   logior($DEFAULT-PITCH,  $FF-DONTCARE);
	  #"script"     => logior($DEFAULT-PITCH,  $FF-SCRIPT);
	  #"decorative" => logior($DEFAULT-PITCH,  $FF-DECORATIVE);
	  otherwise	=> logior($DEFAULT-PITCH,  $FF-DONTCARE);
	end;
    let italic
      = select (slant)
	  #"roman"	=> 0;
	  #"italic"	=> 1;
	  #"oblique"	=> 1;
	  otherwise	=> 0;
	end;
    let height = round/(size * win32-pixels-per-inch(_port), 72);
    let hFont :: <HFONT> =
      CreateFont(- height,		// height
		 0, 			// width not specified
		 0, 			// escapement (1/10 degree)
		 0, 			// orientation (1/10 degree)
		 weight,		// weight,
		 italic,		// italic?
		 if (underline?) 1 else 0 end,
		 if (strikeout?) 1 else 0 end,
		 charset,		// character set
		 $OUT-TT-PRECIS,	// criteria - use TrueType
		 $CLIP-DEFAULT-PRECIS,	// clip precision
		 $DEFAULT-QUALITY,	// quality
		 pitch-and-family,	// pitch and family
		 $NULL-string);		// typeface name
    if (null-handle?(hFont))
      report-error("CreateFont");
      hFont := pointer-cast(<HFONT>, GetStockObject($ANSI-FIXED-FONT))
    end;
    let font = make(<win32-font>,
		    handle: hFont, text-style: text-style);
    gethash(table, text-style) := font;
    font
  end
end method do-text-style-mapping;

// Table of font sizes
//--- We should compute the numbers based on either device characteristics
//--- or some user option
define variable *win32-logical-sizes*
  = #[#[#"normal",     12],	// put most common one first for efficiency
      #[#"small",      10],
      #[#"large",      14],
      #[#"very-small",  8],
      #[#"very-large", 18],
      #[#"tiny",        6],
      #[#"huge",       24]];

//--- This approach seems unnecessarily clumsy; we might as well just have 
//--- 'do-text-style-mapping' do the table lookup directly itself.  We shouldn't
//--- need to cons up a whole new text-style object just to map the size.
define method standardize-text-style
    (_port :: <win32-port>, text-style :: <standard-text-style>,
     #rest keys, #key character-set)
 => (text-style :: <text-style>)
  apply(standardize-text-style-size,
	_port, text-style, *win32-logical-sizes*, keys)
end method standardize-text-style;


/// Font metrics

define method font-metrics
    (text-style :: <text-style>, medium :: <win32-medium>,
     #rest keys, #key character-set)
 => (font,
     width :: <integer>, height :: <integer>, ascent :: <integer>, descent :: <integer>)
  let font :: <win32-font>
    = apply(text-style-mapping, port(medium), text-style, keys);
  windows-font-metrics(font, medium)
end method font-metrics;

define method font-width
    (text-style :: <text-style>, medium :: <win32-medium>,
     #rest keys, #key character-set)
 => (width :: <integer>)
  let (font, width, height, ascent, descent)
    = apply(font-metrics, text-style, medium, keys);
  ignore(font, height, ascent, descent);
  width
end method font-width;

define method font-height
    (text-style :: <text-style>, medium :: <win32-medium>,
     #rest keys, #key character-set)
 => (height :: <integer>)
  let (font, width, height, ascent, descent)
    = apply(font-metrics, text-style, medium, keys);
  ignore(font, width, ascent, descent);
  height
end method font-height;

define method font-ascent
    (text-style :: <text-style>, medium :: <win32-medium>,
     #rest keys, #key character-set)
 => (ascent :: <integer>)
  let (font, width, height, ascent, descent)
    = apply(font-metrics, text-style, medium, keys);
  ignore(font, width, height, descent);
  ascent
end method font-ascent;

define method font-descent
    (text-style :: <text-style>, medium :: <win32-medium>,
     #rest keys, #key character-set)
 => (descent :: <integer>)
  let (font, width, height, ascent, descent)
    = apply(font-metrics, text-style, medium, keys);
  ignore(font, width, height, ascent);
  descent
end method font-descent;

define method windows-font-metrics
    (font :: <win32-font>, medium :: <win32-medium>)
 => (font :: <win32-font>,
     width :: <integer>, height :: <integer>, ascent :: <integer>, descent :: <integer>)
  if (zero?(font.%font-height))
    let hFont :: <HFONT> = font.%font-handle;
    let hDC :: <HDC> = get-DC(medium);
    let old-hFont = 
      if (hFont == medium.%hFont)
	hFont
      else
	check-result("SelectObject font", SelectObject(hDC, hFont))
      end;
    with-stack-structure (ptm :: <LPTEXTMETRIC>)
      check-result("GetTextMetrics", GetTextMetrics(hDC, ptm));
      font.%font-height    := ptm.tmHeight-value;
      font.%font-width     := ptm.tmAveCharWidth-value;
      font.%font-max-width := ptm.tmMaxCharWidth-value;
      font.%font-ascent    := ptm.tmAscent-value;
      font.%font-descent   := ptm.tmDescent-value;
    end;
    unless (old-hFont.pointer-address = hFont.pointer-address)
      SelectObject(hDC, old-hFont)
    end
  end;
  values(font,
	 font.%font-width, font.%font-height, font.%font-ascent, font.%font-descent)
end method win-font-metrics;

define method fixed-width-font?
    (text-style :: <text-style>, medium :: <win32-medium>, #key character-set)
 => (fixed? :: <boolean>)
  let (font, width)
    = font-metrics(text-style, medium, character-set: character-set);
  let maximum-width = font.%font-max-width;
  width = maximum-width
end method fixed-width-font?;

define method fixed-width-font?
    (text-style :: <standard-text-style>, medium :: <win32-medium>,
     #key character-set)
 => (fixed? :: <boolean>)
  select (text-style-family(text-style))
    #"fix" => #t;
    #"serif" => #f;
    otherwise => next-method();
  end
end method fixed-width-font?;

// This is just for platform-dependent user code:
define method make-text-style-from-hfont
    (window, font :: <HFONT>) => (text-style :: <text-style>)
  make-device-font(port(window),
		   make(<win32-font>, handle: font))
end method make-text-style-from-hfont;


/// Text measurement

define method text-size
    (medium :: <win32-medium>, char :: <character>,
     #key text-style :: <text-style> = medium-merged-text-style(medium),
          start: _start, end: _end, do-newlines?)
 => (largest-x :: <real>, largest-y :: <real>,
     cursor-x :: <real>, cursor-y :: <real>, baseline :: <real>)
  ignore(_start, _end, do-newlines?);
  //--- Should be using <C-unicode-string> when supported by following method.
  //--- Note: with-stack-structure doesn't yet support multiple values
  /*
  with-stack-structure (ptr :: <C-string>, size: 2, fill: char)
    text-size(medium, ptr, end: 1, text-style: text-style)
  end;
  */
  let ptr :: <C-string> = make(<C-string>, size: 2, fill: char);
  let (largest-x, total-height, last-x, last-y, baseline)
    = text-size(medium, ptr, end: 1, text-style: text-style);
  destroy(ptr);
  values(largest-x, total-height, last-x, last-y, baseline)
end method text-size;

define method text-size
    (medium :: <win32-medium>, string :: <string>,
     #key text-style :: <text-style> = medium-merged-text-style(medium),
          start: _start = 0, end: _end = size(string), do-newlines?)
 => (largest-x :: <real>, largest-y :: <real>,
     cursor-x :: <real>, cursor-y :: <real>, baseline :: <real>)
  //---*** Still need to handle multi-line text.
  //---*** What do we do about Unicode strings?
  let (font :: <win32-font>, width, height, ascent, descent)
    = font-metrics(text-style, medium);
  ignore(width, height);
  let hFont :: <HFONT> = font.%font-handle;
  let hDC :: <HDC> = get-DC(medium);
  let old-hFont
    = if (hFont == medium.%hFont)
	hFont
      else
	check-result("SelectObject font",
		     SelectObject(hDC, hFont))
      end;
  let width  :: <integer> = 0;
  let height :: <integer> = 0;
  with-c-string (c-string = string, start: _start, end: _end)
    with-stack-structure (lpsize :: <LPSIZE>)
      check-result("GetTextExtentPoint32",
		   GetTextExtentPoint32(hDC, c-string, _end - _start, lpsize));
      width  := lpsize.cx-value;
      height := lpsize.cy-value;
    end
  end;
  let baseline = height - descent;
  unless (old-hFont.pointer-address = hFont.pointer-address)
    SelectObject(hDC, old-hFont)
  end;
  values(width, height, width, height, baseline)
end method text-size;
