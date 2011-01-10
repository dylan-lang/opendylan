Module:       duim-dcs-internals
Synopsis:     DUIM display device contexts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Colors

define protocol <<color-protocol>> ()
  function color-rgb
    (color :: <color>)
 => (red :: <real>, green :: <real>, blue :: <real>, opacity :: <real>);
  function color-ihs
    (color :: <color>)
 => (intensity :: <real>, hue :: <real>, saturation :: <real>, opacity :: <real>);
  getter color-luminosity (color :: <color>)
 => (value :: <real>);
end protocol <<color-protocol>>;


/// RGB <-> IHS conversions

define constant $ihs-rgb-c1 :: <single-float> = 0.4082483;	// sqrt(1/6)
define constant $ihs-rgb-c2 :: <single-float> = 0.7071068;	// sqrt(1/2)
define constant $ihs-rgb-c3 :: <single-float> = 0.5773503;	// sqrt(1/3)

define constant $sqrt3      :: <single-float> = 1.732051;	// sqrt(3)

define function ihs->rgb
    (intensity :: <single-float>, hue :: <single-float>, saturation :: <single-float>)
 => (red :: <single-float>, green :: <single-float>, blue :: <single-float>)
  let hh = modulo(hue - 0.5, 1.0) * $2pi - $pi;
  let c3 = cos(saturation);
  let s3 = sin(saturation);
  let cos-hh = cos(hh);
  let sin-hh = sin(hh);
  let x = $ihs-rgb-c1 * s3 * cos-hh * intensity;
  let y = $ihs-rgb-c2 * s3 * sin-hh * intensity;
  let z = $ihs-rgb-c3 * c3 * intensity;
  values(max(0.0, min(1.0, x + x + z)),
	 max(0.0, min(1.0, y + z + -x)),
	 max(0.0, min(1.0, (z - x) - y)))
end function ihs->rgb;

define function rgb->ihs
    (red :: <single-float>, green :: <single-float>, blue :: <single-float>)
 => (intensity :: <single-float>, hue :: <single-float>, saturation :: <single-float>)
  let x = $ihs-rgb-c1 * (((red + red) - blue) - green);
  let y = $ihs-rgb-c2 * (green - blue);
  let z = $ihs-rgb-c3 * (red + green + blue);
  let q = x * x + y * y;
  let intensity = sqrt(q + z * z);	// == sqrt(r^2 + g^2 + b^2) !
  if (zero?(q))
    // A totally unsaturated color
    values(intensity, 0.0, 0.0)
  else
    let hue = modulo(atan2(y, x) / $2pi, 1.0);
    let f1 = z / intensity;
    let f2 = sqrt(1.0 - f1 * f1);
    let saturation = atan2(f2, f1);
    values(intensity, hue, saturation)
  end
end function rgb->ihs;

// From Foley and Van Dam, page 613 (discussion of YIQ color model)...
define inline function rgb->luminosity
    (r :: <single-float>, g :: <single-float>, b :: <single-float>)
 => (luminosity :: <single-float>)
  0.299 * r + 0.587 * g + 0.114 * b
end function rgb->luminosity;


/// RGB Colors

define sealed class <rgb-color> (<color>)
  sealed constant slot %red :: <single-float>,
    required-init-keyword: red:;
  sealed constant slot %green :: <single-float>,
    required-init-keyword: green:;
  sealed constant slot %blue :: <single-float>,
    required-init-keyword: blue:;
  sealed constant slot color-opacity :: <single-float> = 1.0,
    init-keyword: opacity:;
end class <rgb-color>;

define method \=
    (color1 :: <color>, color2 :: <color>) => (true? :: <boolean>)
  color1 == color2
  | begin
      let (r1, g1, b1, o1) = color-rgb(color1);
      let (r2, g2, b2, o2) = color-rgb(color2);
      r1 = r2 & g1 = g2 & b1 = b2 & o1 = o2
    end
end method \=;

// A faster method for two rgb colors...
define method \=
    (color1 :: <rgb-color>, color2 :: <rgb-color>) => (true? :: <boolean>)
  color1 == color2
  | (color1.%red = color2.%red
     & color1.%green = color2.%green
     & color1.%blue = color2.%blue
     & color-opacity(color1) = color-opacity(color2))
end method \=;


define sealed method make
    (class == <color>,
     #key red, green, blue, intensity, hue, saturation, opacity = 1.0)
 => (color :: <rgb-color>)
  if (red | green | blue)
    make-rgb-color(red, green, blue, opacity: opacity)
  else
    make-ihs-color(intensity, hue, saturation, opacity: opacity)
  end
end method make;

define sealed domain make (singleton(<rgb-color>));
define sealed domain initialize (<rgb-color>);

define function make-rgb-color
    (red :: <real>, green :: <real>, blue :: <real>, #key opacity = 1.0)
 => (color :: <rgb-color>)
  assert((0 <= red & 0 <= 1),
         "The red value %= is not a number between 0 and 1", red);
  assert((0 <= green & 0 <= 1),
         "The green value %= is not a number between 0 and 1", green);
  assert((0 <= blue & 0 <= 1),
         "The blue value %= is not a number between 0 and 1", blue);
  make(<rgb-color>,
       red:     as(<single-float>, red),
       green:   as(<single-float>, green),
       blue:    as(<single-float>, blue),
       opacity: as(<single-float>, opacity))
end function make-rgb-color;

define function make-ihs-color
    (intensity :: <real>, hue :: <real>, saturation :: <real>,
     #key opacity = 1.0)
 => (color :: <rgb-color>)
  assert((0 <= intensity & 0 <= $sqrt3),
         "The intensity value %= is not a number between 0 and sqrt(3)", intensity);
  assert((0 <= hue & 0 <= 1),
         "The hue value %= is not a number between 0 and 1", hue);
  assert((0 <= saturation & 0 <= 1),
         "The saturation value %= is not a number between 0 and 1", saturation);
  case
    intensity = 0 =>
      $black;
    otherwise =>
      let (red, green, blue)
	= ihs->rgb(as(<single-float>, intensity),
		   as(<single-float>, hue),
		   as(<single-float>, saturation));
      make(<rgb-color>,
	   red:     red,
	   green:   green,
	   blue:    blue,
	   opacity: as(<single-float>, opacity))
  end
end function make-ihs-color;

define function make-gray-color
    (luminosity :: <real>, #key opacity = 1.0)
 => (color :: <rgb-color>)
  assert((0 <= luminosity & 0 <= 1),
         "The luminosity %= is not a number between 0 and 1", luminosity);
  case
    luminosity = 0 & opacity = 1.0 =>
      $black;
    luminosity = 1 & opacity = 1.0 =>
      $white;
    otherwise =>
      let luminosity = as(<single-float>, luminosity);
      make(<rgb-color>,
	   red:     luminosity,
	   green:   luminosity,
	   blue:    luminosity,
	   opacity: as(<single-float>, opacity))
  end
end function make-gray-color;


// The primary colors, constant across all platforms
define constant $black   :: <rgb-color> = make-rgb-color(0, 0, 0);
define constant $red     :: <rgb-color> = make-rgb-color(1, 0, 0);
define constant $green   :: <rgb-color> = make-rgb-color(0, 1, 0);
define constant $blue    :: <rgb-color> = make-rgb-color(0, 0, 1);
define constant $cyan    :: <rgb-color> = make-rgb-color(0, 1, 1);
define constant $magenta :: <rgb-color> = make-rgb-color(1, 0, 1);
define constant $yellow  :: <rgb-color> = make-rgb-color(1, 1, 0);
define constant $white   :: <rgb-color> = make-rgb-color(1, 1, 1);


define sealed inline method color-rgb
    (color :: <rgb-color>)
 => (red :: <single-float>, green :: <single-float>, blue :: <single-float>,
     opacity :: <single-float>);
  values(color.%red, color.%green, color.%blue, color-opacity(color))
end method color-rgb;

define sealed method color-ihs
    (color :: <rgb-color>)
 => (intensity :: <single-float>, hue :: <single-float>, saturation :: <single-float>,
     opacity :: <single-float>);
  let (intensity, hue, saturation)
    = rgb->ihs(color.%red, color.%green, color.%blue);
  values(intensity, hue, saturation, color-opacity(color))
end method color-ihs;

define sealed method color-luminosity
    (color :: <rgb-color>) => (luminosity :: <single-float>)
  rgb->luminosity(color.%red, color.%green, color.%blue)
end method color-luminosity;


/// Foreground and background (indirect) inks

define sealed class <foreground> (<ink>)
end class <foreground>;

define sealed domain make (singleton(<foreground>));
define sealed domain initialize (<foreground>);

define constant $foreground :: <foreground> = make(<foreground>);

// The default defaults, if no others can be found anywhere
define constant $default-foreground :: <rgb-color> = $black;


define sealed class <background> (<ink>)
end class <background>;

define sealed domain make (singleton(<background>));
define sealed domain initialize (<background>);

define constant $background :: <background> = make(<background>);

define constant $default-background :: <rgb-color> = $white;


/// Contrasting colors

define sealed class <contrasting-color> (<color>)
  sealed constant slot %how-many  :: <integer>,
    required-init-keyword: how-many:;
  sealed constant slot %which-one :: <integer>,
    required-init-keyword: which-one:;
end class <contrasting-color>;

define sealed domain make (singleton(<contrasting-color>));
define sealed domain initialize (<contrasting-color>);

//--- 8 is pretty small
define constant $contrasting-colors-count :: <integer> = 8;

define sealed method make-contrasting-colors
    (n :: <integer>, #key k = $unsupplied) => (colors)
  check-type(n, limited(<integer>, min: 2, max: $contrasting-colors-count));
  if (unsupplied?(k))
    let result :: <simple-object-vector> = make(<simple-vector>, size: n);
    without-bounds-checks
      for (k :: <integer> from 0 below n)
	result[k] := make(<contrasting-color>, which-one: k, how-many: n)
      end
    end;
    result
  else
    assert(k < n,
	   "The index %d must be smaller than the count %d", k, n);
    make(<contrasting-color>, which-one: k, how-many: n)
  end
end method make-contrasting-colors;

define method contrasting-colors-limit
    (port) => (limit :: <integer>)
  $contrasting-colors-count
end method contrasting-colors-limit;

define sealed inline method contrasting-color-index
    (color :: <contrasting-color>) => (which :: <integer>, how-many :: <integer>)
  values(color.%which-one, color.%how-many)
end method contrasting-color-index;

define constant $contrasting-colors :: <simple-object-vector>
    = vector($red, $blue, $green, $yellow, $cyan, $magenta, $black, $white);

define sealed inline method contrasting-color->color
    (color :: <contrasting-color>) => (color :: <rgb-color>)
  $contrasting-colors[color.%which-one]
end method contrasting-color->color;


define sealed method color-rgb
    (color :: <contrasting-color>)
 => (red :: <single-float>, green :: <single-float>, blue :: <single-float>,
     opacity :: <single-float>);
  color-rgb(contrasting-color->color(color))
end method color-rgb;

define sealed method color-ihs
    (color :: <contrasting-color>)
 => (intensity :: <single-float>, hue :: <single-float>, saturation :: <single-float>,
     opacity :: <single-float>);
  color-ihs(contrasting-color->color(color))
end method color-ihs;

define sealed method color-luminosity
    (color :: <contrasting-color>) => (luminosity :: <single-float>)
  color-luminosity(contrasting-color->color(color))
end method color-luminosity;
