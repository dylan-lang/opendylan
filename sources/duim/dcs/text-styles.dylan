Module:       duim-dcs-internals
Synopsis:     DUIM display device contexts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Text style protocol

define constant <text-style-weight>
    = one-of(#f, #"normal", #"condensed", #"thin", #"extra-light", #"light",
	     #"medium", #"demibold", #"bold", #"extra-bold", #"black");

define constant <text-style-slant>
    = one-of(#f, #"roman", #"italic", #"oblique");

define protocol <<text-style-protocol>> ()
  // Components
  function text-style-components
    (style :: <text-style>)
 => (family, name :: false-or(<string>), weight :: <text-style-weight>,
     slant :: <text-style-slant>, size, underline? :: <boolean>,
     strikeout? :: <boolean>);
  getter text-style-family
    (style :: <text-style>) => (family);
  getter text-style-name
    (style :: <text-style>) => (name :: false-or(<string>));
  getter text-style-weight
    (style :: <text-style>) => (weight :: <text-style-weight>);
  getter text-style-slant
    (style :: <text-style>) => (slant :: <text-style-slant>);
  getter text-style-size
    (style :: <text-style>) => (size);
  getter text-style-underline?
    (style :: <text-style>) => (underline? :: <boolean>);
  getter text-style-strikeout?
    (style :: <text-style>) => (strikeout? :: <boolean>);
  // Merging
  function merge-text-styles
    (style :: <text-style>, default :: <text-style>)
 => (style :: <text-style>);
  function fully-merged-text-style?
    (style :: <text-style>) => (true? :: <boolean>);
end protocol <<text-style-protocol>>;


/// Text styles

define sealed class <standard-text-style> (<text-style>)
  sealed constant slot text-style-family = #f,
    init-keyword: family:;
  sealed constant slot text-style-name :: false-or(<string>) = #f,
    init-keyword: name:;
  // Encodes weight, slant, underline, and strikeout
  sealed slot text-style-face-code :: <integer> = 0,
    init-keyword: face-code:;
  sealed constant slot text-style-size = #f,
    init-keyword: size:;
end class <standard-text-style>;

define sealed domain make (singleton(<standard-text-style>));
define sealed domain initialize (<standard-text-style>);

// Bits 0..5 are the weight
define constant %weight_shift :: <integer> = 0;
define constant %weight_mask  :: <integer> = #o077;
// define constant %weight_false       = #o000;		// must be zero!
// define constant %weight_normal      = #o001;
// define constant %weight_condensed   = #o002;
// define constant %weight_thin        = #o003;
// define constant %weight_extra_light = #o004;
// define constant %weight_light       = #o005;
// define constant %weight_medium      = #o006;
// define constant %weight_demibold    = #o007;
// define constant %weight_bold        = #o010;
// define constant %weight_extra_bold  = #o011;
// define constant %weight_black       = #o012;

define constant $text-style-weights :: <simple-object-vector>
    = #[#f, #"normal", #"condensed", #"thin", #"extra-light", #"light",
        #"medium", #"demibold", #"bold", #"extra-bold", #"black"];

assert($text-style-weights[0] == #f,
       "Zero'th text style weight must be #f");

// Bits 6..8 are the slant
define constant %slant_shift :: <integer> = 6;
define constant %slant_mask  :: <integer> = #o700;
// define constant %slant_false   = #o000;		// must be zero!
// define constant %slant_roman   = #o100;
// define constant %slant_italic  = #o200;
// define constant %slant_oblique = #o300;

define constant $text-style-slants :: <simple-object-vector>
    = #[#f, #"roman", #"italic", #"oblique"];

assert($text-style-slants[0] == #f,
       "Zero'th text style slant must be #f");

// Bits 9 and 10 are the underline and strikeout flags
define constant %underline_style :: <integer> = #o1000;
define constant %strikeout_style :: <integer> = #o2000;

define inline function compute-face-code
    (weight, slant, underline?, strikeout?) => (face-code :: <integer>)
  let weight-index = position($text-style-weights, weight) | 0;
  let slant-index  = position($text-style-slants,  slant)  | 0;
  ash(weight-index, %weight_shift)
  + ash(slant-index, %slant_shift)
  + if (underline?) %underline_style else 0 end
  + if (strikeout?) %strikeout_style else 0 end
end function compute-face-code;

define sealed method initialize
    (style :: <standard-text-style>,
     #key weight = #"normal", slant = #"roman", underline? = #f, strikeout? = #f)
  next-method();
  text-style-face-code(style)
    := compute-face-code(weight, slant, underline?, strikeout?)
end method initialize;    

define sealed method \=
    (style1 :: <standard-text-style>, style2 :: <standard-text-style>) => (true? :: <boolean>)
  // Note that text-style-name is compared for equality (\=) rather than
  // identity (\==), since it can be a <string>
  style1 == style2
  | (text-style-family(style1) == text-style-family(style2)
     & text-style-name(style1) = text-style-name(style2)
     & text-style-face-code(style1) == text-style-face-code(style2)
     & text-style-size(style1) == text-style-size(style2))
end method \=;

define sealed inline method text-style-weight
    (style :: <standard-text-style>) => (weight :: <text-style-weight>)
  let index = ash(logand(text-style-face-code(style), %weight_mask),
		  -%weight_shift);
  $text-style-weights[index]
end method text-style-weight;

define sealed inline method text-style-slant
    (style :: <standard-text-style>) => (slant :: <text-style-slant>)
  let index = ash(logand(text-style-face-code(style), %slant_mask),
		  -%slant_shift);
  $text-style-slants[index]
end method text-style-slant;

define sealed inline method text-style-underline?
    (style :: <standard-text-style>) => (underline? :: <boolean>)
  logand(text-style-face-code(style), %underline_style) = %underline_style
end method text-style-underline?;

define sealed inline method text-style-strikeout?
    (style :: <standard-text-style>) => (strikeout? :: <boolean>)
  logand(text-style-face-code(style), %strikeout_style) = %strikeout_style
end method text-style-strikeout?;

define sealed method text-style-components 
    (style :: <standard-text-style>)
 => (family, name :: false-or(<string>), weight :: <text-style-weight>,
     slant :: <text-style-slant>, size, underline? :: <boolean>,
     strikeout? :: <boolean>)
  values(text-style-family(style),
         text-style-name(style),
         text-style-weight(style),
         text-style-slant(style),
         text-style-size(style),
	 text-style-underline?(style),
	 text-style-strikeout?(style))
end method text-style-components;

define variable $text-family-table :: <object-table> = make(<table>);

define sealed inline method make
    (class == <text-style>,
     #key family, name, weight, slant, size, underline? = #f, strikeout? = #f)
 => (text-style :: <standard-text-style>)
  make-text-style(family, name, weight, slant, size,
		  underline?: underline?, strikeout?: strikeout?)
end method make;

define sealed method make-text-style
    (family, name, weight, slant, size, #key underline? = #f, strikeout? = #f)
 => (text-style :: <standard-text-style>)
  let family-table = $text-family-table;
  let name-table = gethash(family-table, family);
  unless (name-table)
    name-table := make(<string-or-object-table>);
    gethash(family-table, family) := name-table
  end;
  let face-table = gethash(name-table, name);
  let face = compute-face-code(weight, slant, underline?, strikeout?);
  unless (face-table)
    face-table := make(<table>);
    gethash(name-table, name) := face-table
  end;
  let size-table = gethash(face-table, face);
  unless (size-table)
    size-table := make(<table>);
    gethash(face-table, face) := size-table
  end;
  let text-style = gethash(size-table, size);
  unless (text-style)
    text-style
      := make(<standard-text-style>,
              family: family, name: name, weight: weight, slant: slant, size: size,
	      underline?: underline?, strikeout?: strikeout?);
    gethash(size-table, size) := text-style
  end;
  text-style
end method make-text-style;


/// Text style merging

define sealed method fully-merged-text-style?
    (style :: <standard-text-style>) => (true? :: <boolean>)
  text-style-family(style)
  & text-style-name(style)
  & begin
      let code = text-style-face-code(style);
      ~zero?(logand(code, %weight_mask)) & ~zero?(logand(code, %slant_mask))
    end
  & begin
      let size = text-style-size(style);
      size & size ~== #"larger" & size ~== #"smaller"
    end
  & #t
end method fully-merged-text-style?;

define sealed method merge-text-styles
    (style :: <standard-text-style>, default :: <standard-text-style>)
 => (text-style :: <standard-text-style>)
  let (family1, name1, weight1, slant1, size1, underline?, strikeout?)
    = text-style-components(style);
  if (family1 & name1 & weight1 & slant1
      & (size1 & size1 ~== #"larger" & size1 ~== #"smaller"))
    style
  else
    let (family2, name2, weight2, slant2, size2)
      = text-style-components(default);
    make-text-style
      (family1 | family2,
       name1   | name2,
       weight1 | weight2,
       slant1  | slant2,
       merge-text-style-sizes(size1, size2),
       underline?: underline?, strikeout?: strikeout?)
  end
end method merge-text-styles;

define constant $text-style-sizes :: <simple-object-vector>
    = #[#"tiny", #"very-small", #"small", #"normal", #"large", #"very-large", #"huge"];

define function merge-text-style-sizes
    (size1, size2) => (new-size)
  let max-larger-size = 24;		// limits for #"larger" and #"smaller"
  let min-smaller-size = 4;
  select (size1)
    #"larger" =>
      case
        instance?(size2, <number>) => min(size2 + 2, max-larger-size);
        size2 == #"smaller" => #f;	// let a higher level decide...
        otherwise =>
          let index = position($text-style-sizes, size2);
          if (index)
            $text-style-sizes[index + 1] | #"huge"
          else
            size1
	  end;
      end;
    #"smaller" =>
      case
        instance?(size2, <number>) => max(size2 - 2, min-smaller-size);
        size2 == #"larger" => #f;	// let a higher level decide...
        otherwise =>
          let index = position($text-style-sizes, size2);
          if (index)
            if (zero?(index))
              #"tiny"
            else
              $text-style-sizes[index - 1]
            end
          else
            size1
	  end;
      end;
    otherwise =>
      size1 | size2;
  end
end function merge-text-style-sizes;


/// Device fonts

define sealed class <device-font> (<text-style>)
  sealed constant slot device-font-port,
    required-init-keyword: port:;
  sealed constant slot device-font-font,
    required-init-keyword: font:;
end class <device-font>;

define sealed domain make (singleton(<device-font>));
define sealed domain initialize (<device-font>);

define method make-device-font
    (port, font) => (device-font :: <device-font>)
  make(<device-font>, port: port, font: font)
end method make-device-font;

// Device fonts can't be merged against anything.
define sealed method merge-text-styles
    (style :: <device-font>, default :: <standard-text-style>) => (style :: <device-font>)
  ignore(default);
  style
end method merge-text-styles;

define sealed method merge-text-styles
    (style :: <standard-text-style>, default :: <device-font>) => (style :: <device-font>)
  ignore(style);
  default
end method merge-text-styles;

define sealed method fully-merged-text-style? (style :: <device-font>) => (true? :: <boolean>)
  #t
end method fully-merged-text-style?;


/// Initializations

// Not really used yet...
define variable $standard-character-set = #f;

define variable $null-text-style :: <standard-text-style>
    = make(<text-style>,
	   family: #f, name: #f, weight: #f, slant: #f, size: #f);

define variable $undefined-text-style :: <standard-text-style>
    = make(<text-style>,
	   family: #"undefined", name: #f, weight: #"normal", slant: #"roman",
	   size: #"normal");

// The default default, if no other can be found anywhere
define variable $default-text-style :: <standard-text-style>
    = make(<text-style>,
	   family: #"fix", name: #f, weight: #"normal", slant: #"roman",
	   size: #"normal");
