Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Fonts

define constant $fonts :: <stretchy-object-vector> = make(<stretchy-vector>);

define sealed class <font> (<object>)
  // #"fix", #"serif", or #"sans-serif", e.g.
  sealed constant slot font-family = #"fix",
    init-keyword: family:;
  // "Courier", "Times New Roman", or #f, e.g.
  sealed constant slot font-name :: false-or(<string>) = #f,
    init-keyword: name:;
  // #"normal" or #"bold", e.g.
  sealed constant slot font-weight = #"normal",
    init-keyword: weight:;
  // #"roman" or #"italic", e.g.
  sealed constant slot font-slant  = #"roman",
    init-keyword: slant:;
  // A "logical" size or a point size
  sealed constant slot font-size   = #"normal",
    init-keyword: size:;
end class <font>;

define sealed domain make (singleton(<font>));
define sealed domain initialize (<font>);

define sealed method \=
    (font1 :: <font>, font2 :: <font>) => (true? :: <boolean>)
  // 'font-name' is compared for equality rather than identity since it can be a string
  font1 == font2
  | (  font-family(font1) == font-family(font2)
     & font-name(font1)   =  font-name(font2)
     & font-weight(font1) == font-weight(font2)
     & font-slant(font1)  == font-slant(font2)
     & font-size(font1)   == font-size(font2))
end method \=;

define function make-font
    (family, name, weight, slant, point-size) => (font :: <font>)
  block (return)
    let fonts  :: <stretchy-object-vector> = $fonts;
    let nfonts :: <integer> = size(fonts);
    without-bounds-checks
      for (i :: <integer> from 0 below nfonts)
	let font :: <font> = fonts[i];
	// 'font-name' is compared for equality rather than identity since it can be a string
	when (  font-family(font) == family
	      & font-name(font)   =  name
	      & font-weight(font) == weight
	      & font-slant(font)  == slant
	      & font-size(font)   == point-size)
	  return(font)
	end
      end
    end;
    let font = make(<font>,
		    family: family, name: name, weight: weight, slant: slant,
		    size: point-size);
    add!(fonts, font);
    font
  end
end function make-font;

define inline function font->index
    (font :: <font>) => (index :: <integer>)
  position($fonts, font, test: \=)
  | error("The font %= has no index", font)
end function font->index;

define inline function index->font
    (index :: <integer>) => (font :: <font>)
  $fonts[index]
end function index->font;

// Default fonts
define variable $default-font :: <font>
    = make-font(#"fix", #f, #"normal", #"roman", #"normal");
define variable $default-bold-font :: <font>
    = make-font(#"fix", #f, #"bold", #"roman", #"normal");
define variable $default-italic-font :: <font>
    = make-font(#"fix", #f, #"normal", #"italic", #"normal");

// ---*** hughg, 1999/08/12: This is currently being kept in sync with
// $win32-logical-sizes in win32-duim, but that's fragile and non-portable.
// A good solution would seem to be, to make the logical<->literal size
// mapping a part of the front-end of Deuce's and DUIM's protocols, potentially
// implemented differently in each Deuce/DUIM backend (and possibly
// even changeable on-the-fly by users of DUIM, though that could lead to
// all sorts of complications, e.g., cache cleaning, conflicts with existing
// <text-style> intstances etc.).  The duim-deuce backend would delegate to
// the DUIM frontend.  Having something like "font-point-size" (and maybe
// "font-logical-size" as well) in the Deuce (and DUIM?) frontend would be
// good, too.
define constant $deuce-font-sizes :: <simple-object-vector>
    = #[#[5, #"tiny"],
	#[6, #"very-small"],
	#[7, #"small"],
	#[8, #"normal"],
	#[10, #"large"],
	#[12, #"very-large"],
	#[16, #"huge"]];

define function font-point-size
    (font :: <font>)
 => (integer-size :: false-or(<integer>))
  let f-size = font.font-size;
  select (f-size by instance?)
    <integer> => f-size;
    <symbol> =>
      any?(method (literal-logical)
	     when (second(literal-logical) == f-size)
	       first(literal-logical)
	     end
	   end,
	   $deuce-font-sizes);
    otherwise => #f;
  end
end function font-point-size;
