Module:    mini-duim
Synopsis:  Mini-DUIM drawing context objects
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Inks

define open abstract class <ink> (<object>)
end class <ink>;


define sealed class <foreground> (<ink>)
end class <foreground>;

define constant $foreground :: <foreground> = make(<foreground>);


define sealed class <background> (<ink>)
end class <background>;

define constant $background :: <background> = make(<background>);


/// Colors

define sealed class <color> (<ink>)
  slot %red   :: <single-float>,
    required-init-keyword: red:;
  slot %green :: <single-float>,
    required-init-keyword: green:;
  slot %blue  :: <single-float>,
    required-init-keyword: blue:;
end class <color>;

define method initialize (color :: <color>, #key red, green, blue) => ()
  assert((0 <= red & 0 <= 1),
	 "The red value %= is not a number between 0 and 1", red);
  assert((0 <= green & 0 <= 1),
         "The green value %= is not a number between 0 and 1", green);
  assert((0 <= blue & 0 <= 1),
         "The blue value %= is not a number between 0 and 1", blue);
  next-method();
end method initialize;
  
define function make-rgb-color
    (red :: <real>, green :: <real>, blue :: <real>)
 => (color :: <color>)
  make(<color>,
       red:   as(<single-float>, red),
       green: as(<single-float>, green),
       blue:  as(<single-float>, blue))
end function make-rgb-color;

define method color-rgb
    (color :: <color>)
 => (red :: <single-float>, green :: <single-float>, blue :: <single-float>)
  values(color.%red, color.%green, color.%blue)
end method color-rgb;

define method \=
    (c1 :: <color>, c2 :: <color>) => (true? :: <boolean>)
  c1 == c2
  | begin
      let (r1, g1, b1) = color-rgb(c1);
      let (r2, g2, b2) = color-rgb(c2);
      r1 = r2 & g1 = g2 & b1 = b2
    end
end method \=;


define constant $black   :: <color> = make-rgb-color(0, 0, 0);
define constant $red     :: <color> = make-rgb-color(1, 0, 0);
define constant $green   :: <color> = make-rgb-color(0, 1, 0);
define constant $blue    :: <color> = make-rgb-color(0, 0, 1);
define constant $cyan    :: <color> = make-rgb-color(0, 1, 1);
define constant $magenta :: <color> = make-rgb-color(1, 0, 1);
define constant $yellow  :: <color> = make-rgb-color(1, 1, 0);
define constant $white   :: <color> = make-rgb-color(1, 1, 1);


define sealed class <contrasting-color> (<color>)
end class <contrasting-color>;

// Stub...
define method make-color-for-contrasting-color
    (ink :: <contrasting-color>) => (color :: <color>)
  $black
end method make-color-for-contrasting-color;


/// Images

define open abstract class <image> (<ink>)
end class <image>;

define open generic image-width  (image :: <image>) => (width  :: <integer>);
define open generic image-height (image :: <image>) => (height :: <integer>);


define sealed class <stencil> (<image>)
end class <stencil>;

define sealed class <pattern> (<stencil>)
end class <pattern>;

// Stub...
define method decode-pattern (stencil :: <stencil>) => (array, colors, transform)
  values(make(<array>, dimensions: #(0,0)), #[], #f)
end method decode-pattern;


define open abstract class <pixmap> (<image>)
end class <pixmap>;


/// Palettes

define open abstract class <palette> (<object>)
end class <palette>;

define open abstract class <basic-palette> (<palette>)
  slot color-palette? :: <boolean> = #f,
    init-keyword: color?:;
  slot dynamic-palette? :: <boolean> = #f,
    init-keyword: dynamic?:;
end class <basic-palette>;

define open generic make-palette
    (port, #rest keys, #key, #all-keys) => (palette :: <palette>);


/// Pens

define sealed class <pen> (<object>)
  slot pen-width :: <integer> = 1,
    init-keyword: width:;
  slot pen-units = #"normal",
    init-keyword: units:;
  slot pen-dashes :: type-union(<boolean>, <sequence>) = #f,
    init-keyword: dashes:;
  slot pen-joint-shape = #"miter",
    init-keyword: joint-shape:;
  slot pen-cap-shape = #"butt",
    init-keyword: cap-shape:;
end class <pen>;

define method \=
    (pen1 :: <pen>, pen2 :: <pen>) => (true? :: <boolean>)
  pen1 == pen2
  | begin
      pen-width(pen1) = pen-width(pen2)
      & pen-units(pen1) = pen-units(pen2)
      & pen-dashes(pen1) = pen-dashes(pen2)
      & pen-joint-shape(pen1) = pen-joint-shape(pen2)
      & pen-cap-shape(pen1) = pen-cap-shape(pen2)
    end
end method \=;

// Windows-like "stock" pens
define constant $solid-pen  :: <pen> = make(<pen>, dashes: #f);
define constant $dashed-pen :: <pen> = make(<pen>, dashes: #t);
define constant $dotted-pen :: <pen> = make(<pen>, dashes: #[1, 1]);
define constant $dash-dot-pen :: <pen> = make(<pen>, dashes: #[4, 1, 1, 1]);
define constant $dash-dot-dot-pen :: <pen> = make(<pen>, dashes: #[4, 1, 1, 1, 1, 1]);

define macro with-pen
  { with-pen (?medium:name, #rest ?options:expression) ?:body end }
    => { begin
	   let with-pen-body = method () ?body end;
	   let _pen = make(<pen>, ?options);
	   do-with-drawing-options(?medium, with-pen-body, pen: _pen)
	 end }
end macro with-pen;


/// Brushes

define constant $boole-clr   :: <integer> = 0;
define constant $boole-set   :: <integer> = 1;
define constant $boole-1     :: <integer> = 2;
define constant $boole-2     :: <integer> = 3;
define constant $boole-c1    :: <integer> = 4;
define constant $boole-c2    :: <integer> = 5;
define constant $boole-and   :: <integer> = 6;
define constant $boole-ior   :: <integer> = 7;
define constant $boole-xor   :: <integer> = 8;
define constant $boole-eqv   :: <integer> = 9;
define constant $boole-nand  :: <integer> = 10;
define constant $boole-nor   :: <integer> = 11;
define constant $boole-andc1 :: <integer> = 12;
define constant $boole-andc2 :: <integer> = 13;
define constant $boole-orc1  :: <integer> = 14;
define constant $boole-orc2  :: <integer> = 15;

define sealed class <brush> (<object>)
  slot brush-foreground :: <ink> = $black,
    init-keyword: foreground:;
  slot brush-background :: <ink> = $white,
    init-keyword: background:;
  slot brush-mode :: <integer> = $boole-1,
    init-keyword: mode:;
  slot brush-fill-style = #f,
    init-keyword: fill-style:;
  slot brush-fill-rule = #f,
    init-keyword: fill-rule:;
  slot brush-tile = #f,
    init-keyword: tile:;
  slot brush-stipple = #f,
    init-keyword: stipple:;
  slot brush-ts-x :: false-or(<integer>) = #f,
    init-keyword: ts-x:;
  slot brush-ts-y :: false-or(<integer>) = #f,
    init-keyword: ts-y:;
  slot brush-stretch-mode = #f,
    init-keyword: stretch-mode:;
end class <brush>;

define method \=
    (brush1 :: <brush>, brush2 :: <brush>) => (true? :: <boolean>)
  brush1 == brush2
  | begin
      brush-foreground(brush1) = brush-foreground(brush2)
      & brush-background(brush1) = brush-background(brush2)
      & brush-mode(brush1) = brush-mode(brush2)
      & brush-fill-style(brush1) = brush-fill-style(brush2)
      & brush-fill-rule(brush1) = brush-fill-rule(brush2)
      & brush-tile(brush1) = brush-tile(brush2)
      & brush-stipple(brush1) = brush-stipple(brush2)
      & brush-ts-x(brush1) = brush-ts-x(brush2)
      & brush-ts-y(brush1) = brush-ts-y(brush2)
      & brush-stretch-mode(brush1) = brush-stretch-mode(brush2)
    end
end method \=;

define macro with-brush
  { with-brush (?medium:name, #rest ?options:expression) ?:body end }
    => { begin
	   let with-brush-body = method () ?body end;
	   let _brush = make(<brush>, ?options);
	   do-with-drawing-options(?medium, with-brush-body, brush: _brush)
	 end }
end macro with-brush;


/// Text styles

define open abstract class <text-style> (<object>)
end class <text-style>;

define sealed class <standard-text-style> (<text-style>)
  slot text-style-family = #f;
  slot text-style-weight = #f;
  slot text-style-slant  = #f;
  slot text-style-size   = #f;
  slot text-style-strikeout? = #f,
    init-keyword: strikeout?:;
  slot text-style-underline? = #f,
    init-keyword: underline?:;
end class <standard-text-style>;

define method initialize
    (text-style :: <standard-text-style>, #key family, weight, slant, size) => ()
  next-method();
  text-style-family(text-style) := family | #"fix";
  text-style-weight(text-style) := weight | #"normal";
  text-style-slant(text-style)  := slant  | #"roman";
  text-style-size(text-style)   := size   | 12;
end method initialize;

define method make-text-style
    (family, weight, slant, size, #key underline? = #f, strikeout? = #f)
 => (text-style :: <text-style>)
  make(<standard-text-style>,
       family: family, weight: weight, slant: slant, size: size,
       underline?: underline?, strikeout?: strikeout?)
end method make-text-style;

define method text-style-components 
    (style :: <standard-text-style>)
 => (family, weight, slant, size, strikeout, underline)
  values(text-style-family(style),
         text-style-weight(style),
         text-style-slant(style),
         text-style-size(style),
	 text-style-strikeout?(style),
	 text-style-underline?(style))
end method text-style-components;

define method \=
    (style1 :: <standard-text-style>, style2 :: <standard-text-style>)
 => (true? :: <boolean>)
  style1 == style2
  | begin
      let (f1, w1, s1, z1, t1, u1) = text-style-components(style1);
      let (f2, w2, s2, z2, t2, u2) = text-style-components(style2);
      f1 = f2 & w1 = w2 & s1 = s2 & z1 = z2
      & t1 = t2 & u1 = u2
    end
end method \=;


define sealed class <device-font> (<text-style>)
  slot device-font-port, required-init-keyword: port:;
  slot device-font-font, required-init-keyword: font:;
end class <device-font>;

define method make-device-font (port, font) => (device-font :: <device-font>)
  make(<device-font>, port: port, font: font)
end method make-device-font;


define macro with-text-style
  { with-text-style (?medium:name, #rest ?options:expression) ?:body end }
    => { begin
	   let with-text-style-body = method () ?body end;
	   let _text-style = make(<standard-text-style>, ?options);
	   do-with-drawing-options(?medium, with-text-style-body, text-style: _text-style)
	 end }
end macro with-text-style;


// Stub...
define method standardize-text-style-size
    (_port, style :: <text-style>, size-alist,
     #key character-set) => (style :: <text-style>)
  ignore(character-set);
  style
end method standardize-text-style-size;
