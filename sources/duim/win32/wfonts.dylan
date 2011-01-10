Module:    win32-duim
Synopsis:  Win32 font mapping implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Win32 font management

define sealed class <win32-font> (<object>)
  sealed slot %font-handle :: <HFONT>,
    required-init-keyword: handle:;
  sealed slot %font-text-style :: false-or(<standard-text-style>) = #f,
    init-keyword: text-style:;
  sealed slot %font-height    :: <integer> = 0;
  sealed slot %font-width     :: <integer> = 0;
  sealed slot %font-max-width :: <integer> = 0;
  sealed slot %font-ascent    :: <integer> = 0;
  sealed slot %font-descent   :: <integer> = 0;
end class <win32-font>;

define sealed domain make (singleton(<win32-font>));
define sealed domain initialize (<win32-font>);

// ---*** hughg, 1999/08/02: The element-type of the face-name return value
// is meant to be <TCHAR>, which is "A WCHAR if UNICODE is defined, a CHAR
// otherwise.", but I'm not sure how to handle that.  The size must be less
// than $LF-FACESIZE, to allow the C string size, including NULL terminator,
// to be no more than $LF-FACESIZE.
define sealed method font-components-from-text-style
    (_port :: <win32-port>, text-style :: <standard-text-style>, character-set)
 => (height :: <integer>,
     width :: <integer>,
     escapement :: <integer>,
     orientation :: <integer>,
     weight :: <integer>,
     italic :: <integer>,
     underline :: <integer>,
     strikeout :: <integer>,
     charset :: <integer>,
     output-precision :: <integer>,
     clip-precision :: <integer>,
     quality :: <integer>,
     pitch-and-family :: <integer>,
     face-name :: <string>)
  let (family, name, weight, slant, _size, underline?, strikeout?)
    = text-style-components(text-style);
  if (name)
    when (size(name) > $LF-FACESIZE - 1)
      name := copy-sequence(name, end: $LF-FACESIZE - 1);
    end;
  else
    name := "";
  end;
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
        #"normal"       => 400;
        #"bold"         => 700;
        // Other values, following Windows terminology:
        #"thin"         => 100;
        #"extra-light"=> 200;
        #"light"        => 300;
        #"medium"       => 500;
        #"demibold"     => 600;
        #"extra-bold" => 800;
        #"black"        => 900;
        otherwise       => 400;
      end;
  let pitch-and-family
    = select (family)
        #"fix"        => logior($FIXED-PITCH,    $FF-MODERN);
        #"serif"      => logior($VARIABLE-PITCH, $FF-ROMAN);
        #"sans-serif" => logior($DEFAULT-PITCH,  $FF-SWISS);
        #"symbol"       => charset := $SYMBOL-CHARSET;
                         logior($DEFAULT-PITCH,  $FF-DONTCARE);
        #"script"     => logior($DEFAULT-PITCH,  $FF-SCRIPT);
        #"decorative" => logior($DEFAULT-PITCH,  $FF-DECORATIVE);
        otherwise       => logior($DEFAULT-PITCH,  $FF-DONTCARE);
      end;
  let italic
    = select (slant)
        #"roman"        => 0;
        #"italic"       => 1;
        #"oblique"      => 1;
        otherwise       => 0;
      end;
  let height = -round/(_size * win32-pixels-per-inch(_port), 72);
  values(height,                // height
         0,                     // width not specified
         0,                     // escapement (1/10 degree)
         0,                     // orientation (1/10 degree)
         weight,                // weight,
         italic,                // italic?
         if (underline?) 1 else 0 end,
         if (strikeout?) 1 else 0 end,
         charset,               // character set
         $OUT-TT-PRECIS,        // criteria - use TrueType
         $CLIP-DEFAULT-PRECIS,  // clip precision
         $DEFAULT-QUALITY,      // quality
         pitch-and-family,      // pitch and family
         name)                  // typeface name
end method font-components-from-text-style;

define sealed method do-text-style-mapping
    (_port :: <win32-port>, text-style :: <standard-text-style>, character-set)
 => (font :: <win32-font>)
  let text-style
    = standardize-text-style(_port, text-style,
                             character-set: character-set);
  let table :: <object-table> = port-font-mapping-table(_port);
  let font = gethash(table, text-style);
  case
    font =>
      font;
    text-style == $win32-default-gadget-text-style =>
      let hFont :: <HFONT> = $gui-hfont;
      let font = make(<win32-font>,
                      handle: hFont, text-style: text-style);
      gethash(table, text-style) := font;
      font;
    otherwise =>
      let (#rest font-components)
        = font-components-from-text-style(_port, text-style, character-set);
      let hFont :: <HFONT> = apply(CreateFont, font-components);
      when (null-handle?(hFont))
        report-error("CreateFont");
        hFont := pointer-cast(<HFONT>, GetStockObject($ANSI-FIXED-FONT))
      end;
      let font = make(<win32-font>,
                      handle: hFont, text-style: text-style);
      gethash(table, text-style) := font;
      font;
  end
end method do-text-style-mapping;

// Table of font sizes
//--- We should compute the numbers based on either device characteristics
//--- or some user option
define constant $win32-logical-sizes :: <simple-object-vector>
    = #[#[#"normal",      8],   // put most common one first for efficiency
        #[#"small",       7],
        #[#"large",      10],
        #[#"very-small",  6],
        #[#"very-large", 12],
        #[#"tiny",        5],
        #[#"huge",       16]];

//--- This approach seems unnecessarily clumsy; we might as well just have 
//--- 'do-text-style-mapping' do the table lookup directly itself.  We shouldn't
//--- need to cons up a whole new text-style object just to map the size.
define sealed method standardize-text-style
    (_port :: <win32-port>, text-style :: <standard-text-style>,
     #rest keys, #key character-set)
 => (text-style :: <text-style>)
  if (text-style == $win32-default-gadget-text-style)
    text-style
  else
    apply(standardize-text-style-size,
          _port, text-style, $win32-logical-sizes, keys)
  end
end method standardize-text-style;


/// Font metrics

define sealed inline method font-width
    (text-style :: <text-style>, _port :: <win32-port>,
     #rest keys, #key character-set)
 => (width :: <integer>)
  let (font, width, height, ascent, descent)
    = apply(font-metrics, text-style, _port, keys);
  ignore(font, height, ascent, descent);
  width
end method font-width;

define sealed inline method font-height
    (text-style :: <text-style>, _port :: <win32-port>,
     #rest keys, #key character-set)
 => (height :: <integer>)
  let (font, width, height, ascent, descent)
    = apply(font-metrics, text-style, _port, keys);
  ignore(font, width, ascent, descent);
  height
end method font-height;

define sealed inline method font-ascent
    (text-style :: <text-style>, _port :: <win32-port>,
     #rest keys, #key character-set)
 => (ascent :: <integer>)
  let (font, width, height, ascent, descent)
    = apply(font-metrics, text-style, _port, keys);
  ignore(font, width, height, descent);
  ascent
end method font-ascent;

define sealed inline method font-descent
    (text-style :: <text-style>, _port :: <win32-port>,
     #rest keys, #key character-set)
 => (descent :: <integer>)
  let (font, width, height, ascent, descent)
    = apply(font-metrics, text-style, _port, keys);
  ignore(font, width, height, ascent);
  descent
end method font-descent;

define sealed inline method fixed-width-font?
    (text-style :: <text-style>, _port :: <win32-port>, #key character-set)
 => (fixed? :: <boolean>)
  let (font, width)
    = font-metrics(text-style, _port, character-set: character-set);
  let maximum-width = font.%font-max-width;
  width = maximum-width
end method fixed-width-font?;

define sealed inline method fixed-width-font?
    (text-style :: <standard-text-style>, _port :: <win32-port>, #key character-set)
 => (fixed? :: <boolean>)
  select (text-style-family(text-style))
    #"fix" => #t;
    #"serif" => #f;
    otherwise => next-method();
  end
end method fixed-width-font?;

define sealed method font-metrics
    (text-style :: <text-style>, _port :: <win32-port>,
     #rest keys, #key character-set)
 => (font,
     width :: <integer>, height :: <integer>, ascent :: <integer>, descent :: <integer>)
  let font :: <win32-font>
    = apply(text-style-mapping, _port, text-style, keys);
  windows-font-metrics(font, _port)
end method font-metrics;

define function windows-font-metrics
    (font :: <win32-font>, _port :: <win32-port>)
 => (font :: <win32-font>,
     width :: <integer>, height :: <integer>, ascent :: <integer>, descent :: <integer>)
  when (zero?(font.%font-height))
    let hDC   :: <hDC>   = _port.%memory-hDC;
    let hFont :: <HFONT> = font.%font-handle;
    check-result("SelectObject font", SelectObject(hDC, hFont));
    with-stack-structure (ptm :: <LPTEXTMETRIC>)
      check-result("GetTextMetrics", GetTextMetrics(hDC, ptm));
      font.%font-height    := ptm.tmHeight-value;
      font.%font-width     := ptm.tmAveCharWidth-value;
      font.%font-max-width := ptm.tmMaxCharWidth-value;
      font.%font-ascent    := ptm.tmAscent-value;
      font.%font-descent   := ptm.tmDescent-value;
    end
  end;
  values(font,
         font.%font-width, font.%font-height, font.%font-ascent, font.%font-descent)
end function windows-font-metrics;


/// Fonts -> text styles

define method make-text-style-from-font
    (_port :: <win32-port>, logfont :: <c-pointer>)
 => (text-style :: <text-style>)
  let logfont :: <LPLOGFONT> = pointer-cast(<LPLOGFONT>, logfont);
  let lfFamily    :: <integer> = logand(as(<integer>, logfont.lfPitchAndFamily-value), #x00F0);
  let lfWeight    :: <integer> = logfont.lfWeight-value;
  let lfItalic    :: <integer> = as(<integer>, logfont.lfItalic-value);
  let lfHeight    :: <integer> = logfont.lfHeight-value;
  let lfUnderline :: <integer> = as(<integer>, logfont.lfUnderline-value);
  let lfStrikeout :: <integer> = as(<integer>, logfont.lfStrikeout-value);
  let lfFaceName :: <string>
    = for (null-position from 0 below ($LF-FACESIZE - 1),
           until: lfFaceName-array(logfont, null-position) = '\0')
        // Just iterating, looking for the NULL terminator.
        // If there isn't one (which would be an error), we'll stop at the
        // $FL-FACESIZE limit anyway, thereby silently "fixing" the error.
      finally
        if (null-position == 0)
          ""
        else
          let lfFaceName :: <string>
            = make(<string>, size: null-position, fill: ' ');
          for (i from 0 below null-position)
            lfFaceName[i] := pointer-value(logfont.lfFaceName-value, index: i);
          end;
          lfFaceName
        end
      end;
  make-text-style-from-font-components
    (_port, lfFamily, lfFaceName, lfWeight, lfItalic, lfHeight, lfUnderline,
     lfStrikeout, logfont: logfont)
end method make-text-style-from-font;

define method make-text-style-from-font
    (_port :: <win32-port>, hFont :: <HFONT>)
 => (text-style :: <text-style>)
  // Since Windows provides no mapping from an HFONT to a LOGFONT,
  // we have to go through GetTextMetrics instead
  let (lfFamily :: <integer>, lfWeight :: <integer>, lfItalic :: <integer>,
       lfHeight :: <integer>, lfUnderline :: <integer>, lfStrikeout :: <integer>)
    = values(0, 0, 0, 0, 0, 0);
  let hDC :: <hDC> = _port.%memory-hDC;
  check-result("SelectObject font", SelectObject(hDC, hFont));
  with-stack-structure (ptm :: <LPTEXTMETRIC>)
    check-result("GetTextMetrics", GetTextMetrics(hDC, ptm));
    lfFamily    := logand(as(<integer>, ptm.tmPitchAndFamily-value), #x00F0);
    lfWeight    := ptm.tmWeight-value;
    lfItalic    := as(<integer>, ptm.tmItalic-value);
    lfHeight    := -(ptm.tmHeight-value - ptm.tmInternalLeading-value);
    lfUnderline := as(<integer>, ptm.tmUnderlined-value);
    lfStrikeout := as(<integer>, ptm.tmStruckOut-value);
  end;
  let lfFaceName :: <string>
    = with-stack-structure (buffer :: <LPTSTR>, element-count: $LF-FACESIZE)
        check-result("GetTextFace", GetTextFace(hDC, $LF-FACESIZE, buffer));
        map-as(<string>, identity, buffer)
      end;
  make-text-style-from-font-components
    (_port, lfFamily, lfFaceName, lfWeight, lfItalic, lfHeight, lfUnderline,
     lfStrikeout, hFont: hFont)
end method make-text-style-from-font;

define method make-text-style-from-font-components
    (_port :: <win32-port>, 
     lfFamily :: <integer>, lfFaceName :: <string>, lfWeight :: <integer>,
     lfItalic :: <integer>, lfHeight :: <integer>, lfUnderline :: <integer>,
     lfStrikeout :: <integer>,
     #key hFont, logfont)
 => (text-style :: <text-style>)
  block (return)
    let family
      = select (lfFamily)
          $FF-MODERN     => #"fix";
          $FF-ROMAN      => #"serif";
          $FF-SWISS      => #"sans-serif";
          $FF-SCRIPT     => #"script";
          $FF-DECORATIVE => #"decorative";
          otherwise => #"fix";
/*
            let handle
              = case
                  hFont   => hFont;
                  logfont => null-handle(<HFONT>);      //---*** how to get a useful handle?
                end;
            // No family match, so return a device font
            return(make-device-font(_port, make(<win32-font>, handle: handle)));
*/
        end;
    let face-name = if (empty?(lfFaceName)) #f else lfFaceName end;
    let weight
      = select (lfWeight)
          400 => #"normal";
          700 => #"bold";
          100 => #"thin";
          200 => #"extra-light";
          300 => #"light";
          500 => #"medium";
          600 => #"demibold";
          800 => #"extra-bold";
          900 => #"black";
          otherwise => #"normal";
        end;
    let slant
      = if (zero?(lfItalic)) #"roman" else #"italic" end;
    // lfHeight = -round/(point-size * GetDeviceCaps(LOGPIXELSY), 72)
    // You can figure out how to get point-size from that!
    let point-size
      = round/(-lfHeight * 72, win32-pixels-per-inch(_port));
    let logsize
      = begin
          let i = find-key($win32-logical-sizes, method (x) x[1] == point-size end);
          i & $win32-logical-sizes[i][0]
        end;
    let underline? = (~zero?(lfUnderline));
    let strikeout? = (~zero?(lfStrikeout));
    make-text-style(family, face-name, weight, slant, logsize | point-size,
                    underline?: underline?, strikeout?: strikeout?)
  end
end method make-text-style-from-font-components;

define method make-text-style-from-name-and-size
    (_port :: <win32-port>, font-name :: <string>, font-size :: <integer>)
 => (text-style :: false-or(<text-style>))
  #f
end method make-text-style-from-name-and-size;


/// Text measurement

define sealed method text-size
    (_port :: <win32-port>, char :: <character>,
     #key text-style :: <text-style> = $default-text-style,
          start: _start, end: _end, do-newlines? = #f, do-tabs? = #f)
 => (largest-x :: <integer>, largest-y :: <integer>,
     cursor-x :: <integer>, cursor-y :: <integer>, baseline :: <integer>)
  ignore(_start, _end, do-newlines?);
  //---*** Should be using <C-unicode-string> when supported by following method
  with-stack-structure (c-string :: <C-string>, size: 2, fill: char)
    text-size(_port, c-string, end: 1, text-style: text-style)
  end
end method text-size;

//---*** What do we do about Unicode strings?
define sealed method text-size
    (_port :: <win32-port>, string :: <string>,
     #key text-style :: <text-style> = $default-text-style,
          start: _start, end: _end, do-newlines? = #f, do-tabs? = #f)
 => (largest-x :: <integer>, largest-y :: <integer>,
     cursor-x :: <integer>, cursor-y :: <integer>, baseline :: <integer>)
  let length :: <integer> = size(string);
  let _start :: <integer> = _start | 0;
  let _end   :: <integer> = _end   | length;
  case
    do-tabs? =>
      let (font :: <win32-font>, width, height, ascent, descent)
        = font-metrics(text-style, _port);
      ignore(width, height);
      let hDC   :: <hDC>   = _port.%memory-hDC;
      let hFont :: <HFONT> = font.%font-handle;
      check-result("SelectObject font", SelectObject(hDC, hFont));
      let substring
        = if (_start = 0 & _end = length) string
          else copy-sequence(string, start: _start, end: _end) end;
      let width  :: <integer> = 0;
      let height :: <integer> = 0;
      with-c-string (c-string = substring)
        let dword /* :: <unsigned-int> */       //--- no useful FFI type for <DWORD>
          = GetTabbedTextExtent(hDC, c-string, _end - _start,
                                0, null-pointer(<LPINT>));
        width  := LOWORD(dword);
        height := HIWORD(dword)
      end;
      let baseline = height - descent;
      values(width, height, width, height, baseline);
    do-newlines? =>
      next-method();
    otherwise =>
      let (font :: <win32-font>, width, height, ascent, descent)
        = font-metrics(text-style, _port);
      ignore(width, height);
      let hDC   :: <hDC>   = _port.%memory-hDC;
      let hFont :: <HFONT> = font.%font-handle;
      check-result("SelectObject font", SelectObject(hDC, hFont));
      let width  :: <integer> = 0;
      let height :: <integer> = 0;
      //---*** It would be great if 'with-c-string' took start & end!
      let substring
        = if (_start = 0 & _end = length) string
          else copy-sequence(string, start: _start, end: _end) end;
      with-c-string (c-string = substring)
        with-stack-structure (lpsize :: <LPSIZE>)
          check-result("GetTextExtentPoint32",
                       GetTextExtentPoint32(hDC, c-string, _end - _start, lpsize));
          width  := lpsize.cx-value;
          height := lpsize.cy-value
        end
      end;
      let baseline = height - descent;
      values(width, height, width, height, baseline);
  end
end method text-size;
