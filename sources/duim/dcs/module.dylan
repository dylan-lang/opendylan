Module:       Dylan-User
Synopsis:     DUIM display device contexts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module duim-DCs
  // Colors
  create $background,
	 $black,
	 $blue,
	 $cyan,
	 $foreground,
	 $green,
	 $magenta,
	 $red,
	 $white,
	 $yellow,
	 <color>,
	 <contrasting-color>,
	 color-ihs,
	 color-luminosity,
	 color-rgb,
	 color?,
	 contrasting-colors-limit,
	 make-contrasting-colors,
	 make-gray-color,
	 make-ihs-color,
	 make-rgb-color;

  // Palettes
  create <color-not-found>,
	 <palette-full>,
	 <palette>,
	 add-colors,
	 color-palette?,
	 dynamic-palette?,
	 find-color,
	 make-palette,
	 palette?,
	 remove-colors;

  // Dynamic and layered colors
  create <dynamic-color>,
	 dynamic-color-color, dynamic-color-color-setter,
	 make-dynamic-color;

  // Images, patterns, and stencils
  create <image>,
	 <ink>,
	 <pattern>,
	 <stencil>,
	 convert-image,
	 image-convertible?,
	 image-depth,
	 image-height,
	 image-width,
	 image?,
	 ink?,
	 make-pattern,
	 make-stencil,
	 pattern?,
         \pattern-definer,
	 read-image, read-image-as,
	 stencil?,
         \stencil-definer,
         transform-image,
	 write-image;

  // Pens
  create <pen>,
	 $solid-pen,
	 $dashed-pen,
	 $dotted-pen,
	 $dash-dot-pen,
	 $dash-dot-dot-pen,
	 contrasting-dash-patterns-limit,
	 make-contrasting-dash-patterns,
	 pen-cap-shape,
	 pen-dashes,
	 pen-joint-shape,
	 pen-units,
	 pen-width,
	 pen?;

  // Brushes
  create <brush>,
	 $boole-clr,   $boole-set,   $boole-1,    $boole-2,
	 $boole-c1,    $boole-c2,    $boole-and,  $boole-ior,
	 $boole-xor,   $boole-eqv,   $boole-nand, $boole-nor,
	 $boole-andc1, $boole-andc2, $boole-orc1, $boole-orc2,
	 $horizontal-hatch, $vertical-hatch, $cross-hatch,
	 $diagonal-hatch-down, $diagonal-hatch-up,
	 $bricks-stipple, $tiles-stipple,
	 $parquet-stipple, $hearts-stipple,
	 $xor-brush,
	 brush-background,
	 brush-fill-rule,
	 brush-fill-style,
	 brush-foreground,
	 brush-mode,
	 brush-stipple,
	 brush-stretch-mode,
	 brush-tile,
	 brush-ts-x,
	 brush-ts-y,
	 brush?;

  // Text styles
  create <text-style>,
	 <device-font>,
	 fully-merged-text-style?,
	 make-device-font,
	 make-text-style,
	 merge-text-styles,
	 text-style-components,
	 text-style-family,
	 text-style-name,
	 text-style-size,
	 text-style-slant,
	 text-style-strikeout?,
	 text-style-underline?,
	 text-style-weight,
	 text-style?;

  // Style descriptors
  create <style-descriptor>,
         default-background, default-background-setter,
         default-foreground, default-foreground-setter,
         default-text-style, default-text-style-setter;

end module duim-DCs;

define module duim-DCs-internals
  use dylan;
  use duim-imports;
  use duim-utilities;
  use duim-geometry-internals;
  use duim-DCs, export: all;

  // Colors
  export <rgb-color>,
	 $default-foreground,
         $default-background,
         contrasting-color-index,
	 contrasting-color->color;

  // Palettes
  export <basic-palette>,
	 do-add-colors,
         allocate-color,
         deallocate-color,
         do-remove-colors,
         update-palette-entry,
         update-palette-entries;

  // Dynamic and layered colors
  create dynamic-color-palettes, dynamic-color-palettes-setter,
	 layered-color,
	 layered-color-color-setter,
	 make-layered-color-set;

  // Pens
  export <standard-pen>;

  // Brushes
  export <standard-brush>,
         make-stipple,
         \stipple-definer;

  // Images, patterns, and stencils
  export <background>,
	 <foreground>,
	 decode-pattern;

  // Text styles
  export <standard-text-style>,
	 $default-text-style,
	 $null-text-style,
	 $standard-character-set,
	 $undefined-text-style,
         device-font-font,
         device-font-port,
         text-style-face-code;
end module duim-DCs-internals;
