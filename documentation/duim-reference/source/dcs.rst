****************
DUIM-DCs Library
****************

.. current-library:: duim-dcs
.. current-module:: duim-dcs

Overview
========

The DUIM-DCs library provides color support to the DUIM library. The
library contains a single module, *duim-dcs*, from which all the
interfaces described in this chapter are exposed. `DUIM-DCs
Module`_ contains complete reference entries for each
exposed interface.

Throughout this chapter, a *drawing context* consists of the combination
of ink, color, brush, pen, palette, and shapes that make up patterns and
images.

The class hierarchy for DUIM-DCs
================================

A number of base classes are exposed in the DUIM-DCs library, each of
which is a subclass of :class:`<object>`. They are shown in the following table

+----------+--------------+------------------------------+
| <object> |              |                              |
+----------+--------------+------------------------------+
|          | <pen>        |                              |
+----------+--------------+------------------------------+
|          | <brush>      |                              |
+----------+--------------+------------------------------+
|          | <palette>    |                              |
+----------+--------------+------------------------------+
|          | <ink>        | See `Subclasses of \<ink\>`_ |
+----------+--------------+------------------------------+
|          | <text-style> |                              |
+----------+--------------+------------------------------+
|          |              | <device-font>                |
+----------+--------------+------------------------------+

-  :class:`<pen>` This is protocol class for pens. A pen is used to draw 1
   dimensional graphics such as lines or outline, using a specific color
   or pattern.
-  :class:`<brush>` The protocol class for brushes. Brushes are used to fill in
   2 dimensional areas with a specific color or pattern.
-  :class:`<palette>` The protocol class for palettes. A palette provides a set
   of colors which can be made available to an application.
-  :class:`<ink>` This class can be thought of as anything that can be drawn.
   As the name implies, an ink describes the color and opacity features
   used by a given pen or brush. That is, the pen and brush define the
   drawing style (outlining or filling, respectively), and an ink is
   used to describe the color or pattern that is drawn. This class has a
   number of subclasses, described in `Subclasses of \<ink\>`_.
-  :class:`<text-style>` The protocol class for text styles. A text style is a
   portable way of describing the appearance of a piece of text on
   screen (its font family, size, weight, and so on) in an abstract
   fashion. Because the fonts available on a particular computer may not
   necessarily match the fonts available on the computer of the
   programmer, DUIM provides a portable model which allows the most
   suitable font on the user’s machine to be chosen at run-time.
-  :class:`<device-font>` The protocol class for device-specific fonts, that
   is, fonts that are resident on a particular device. This is a direct
   subclass of :class:`<text-style>`.

Subclasses of <ink>
-------------------

A number of subclasses of <ink> are exposed by the DUIM-DCs library, as
follows:

-  :class:`<color>` The class of all colors available on the system. This is a
   direct subclass of :class:`<ink>`.
-  :class:`<image>` The class of all images, such as icons and bitmap images.
   Images may often be acquired from an outside source, such as a file
   on disk. This is a direct subclass of :class:`<ink>`.
-  :class:`<stencil>` A stencil is a special kind of pattern that contains only
   opacities, that is, it provides a layer of transparency. This can be
   useful, for instance, when overlaying a color onto an image, so as to
   provide the impression of shading. This is a direct subclass of
   :class:`<image>`.
-  :class:`<pattern>` A pattern is a bounded rectangular arrangement of color,
   like a checkerboard. Drawing a pattern draws a different design in
   each rectangular cell of the pattern. This is a direct subclass of
   :class:`<stencil>`.

Error classes provided by DUIM-DCs
----------------------------------

Two error classes are provided by the DUIM-DCs library, both of which
are immediate subclasses of :class:`<error>`.

-  :class:`<color-not-found>` This class of error is signalled when a color is
   requested but is not available on the user’s system.
-  :class:`<palette-full>` This class of error is signalled when an attempt is
   made to add a color to a palette, and the palette cannot accept any
   more colors. The number of colors in a palette depends on the color
   depth of the connected monitor.

DUIM-DCs Module
===============

This section contains a complete reference of all the interfaces that
are exported from the *duim-dcs* module.

.. generic-function:: \=

   Returns ``#t`` if two objects are equal.

   :signature: = *color1 color2* => *boolean*
   :signature: = *pen1* *pen2* => *boolean*
   :signature: = *brush1* *brush2* => *boolean*
   :signature: = *text-style1* *text-style2* => *value*

   :parameter color1: An instance of type :class:`<color>`.
   :parameter color2: An instance of type :class:`<color>`.
   :parameter pen1: An instance of type :class:`<pen>`.
   :parameter pen2: An instance of type :class:`<pen>`.
   :parameter brush1: An instance of type :class:`<brush>`.
   :parameter brush2: An instance of type :class:`<brush>`.
   :parameter text-style1: An instance of type :class:`<text-style>`.
   :parameter text-style2: An instance of type :class:`<text-style>`.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if two objects are equal.

.. generic-function:: add-colors

   Adds one or more colors to a palette and returns the updated palette.

   :signature: add-colors *palette* *#rest* *colors* => *palette*

   :parameter palette: An instance of type :class:`<palette>`.
   :parameter colors: Instances of type :class:`<color>`.

   :value palette: An instance of type :class:`<palette>`.

   :description:

     Adds *colors* to *palette* and returns the updated palette.

.. constant:: $background

   An indirect ink that uses the medium's background design.

   :type: :class:`<ink>`

   :description:

     An indirect ink that uses the medium's background design.

   See also

   - :class:`<palette>`
   - :gf:`image-height`

.. constant:: $black

   The usual definition of black.

   :type: :class:`<color>`

   :description:

     The usual definition black, the absence of all colors. In the *rgb*
     color model, its value is *000*.

   See also

   - :class:`<color>`

.. constant:: $blue

   The usual definition of the color blue.

   :type: :class:`<color>`

   :description:

     The usual definition of the color blue.

   See also

   - :class:`<color>`

.. constant:: $boole-clr

   The logical operator that is always 0.

   :type: :class:`<integer>`

   :description:

     The logical operator that is always 0. It is a suitable first argument
     to the *boole* function.

.. constant:: $boole-set

   The logical operator that is always 1.

   :type: :class:`<integer>`

   :description:

     The logical operator that is always 1. It is a suitable first argument
     to the *boole* function.

.. constant:: $boole-1

   The logical operator that is always the same as the first integer
   argument to the *boole* function.

   :type: :class:`<integer>`

   :description:

     The logical operator that is always the same as the first integer
     argument to the *boole* function. It is a suitable first argument to the
     *boole* function.

.. constant:: $boole-2

   The logical operator that is always the same as the second integer
   argument to the *boole* function.

   :type: :class:`<integer>`

   :description:

     The logical operator that is always the same as the second integer
     argument to the *boole* function. It is a suitable first argument to the
     *boole* function.

.. constant:: $boole-c1

   The logical operator that is always the same as the complement of the
   first integer argument to the *boole* function.

   :type: :class:`<integer>`

   :description:

     The logical operator that is always the same as the complement of the
     first integer argument to the *boole* function. It is a suitable first
     argument to the *boole* function.

.. constant:: $boole-c2

   The logical operator that is always the same as the complement of the
   second integer argument to the *boole* function.

   :type: :class:`<integer>`

   :description:

     The logical operator that is always the same as the complement of the
     second integer argument to the *boole* function. It is a suitable first
     argument to the *boole* function.

.. constant:: $boole-and

   The logical operator *and*.

   :type: :class:`<integer>`

   :description:

     The logical operator *and*. It is a suitable first argument to the
     *boole* function.

.. constant:: $boole-ior

   The logical operator *inclusive* *or*.

   :type: :class:`<integer>`

   :description:

     The logical operator *inclusive* *or*. It is a suitable first argument
     to the *boole* function.

.. constant:: $boole-xor

   The logical operator *exclusive* *or*.

   :type: :class:`<integer>`

   :description:

     The logical operator *exclusive* *or*. It is a suitable first argument
     to the *boole* function.

.. constant:: $boole-eqv

   The logical operator *equivalence* (*exclusive* *nor*).

   :type: :class:`<integer>`

   :description:

     The logical operator *equivalence* (*exclusive* *nor*). It is a
     suitable first argument to the *boole* function.

.. constant:: $boole-nand

   The logical operator *not-and*.

   :type: :class:`<integer>`

   :description:

     The logical operator *not-and*. It is a suitable first argument to the
     *boole* function.

.. constant:: $boole-nor

   The logical operator *not-or*.

   :type: :class:`<integer>`

   :description:

     The logical operator *not-or*. It is a suitable first argument to the
     *boole* function.

.. constant:: $boole-andc1

   The logical operator that is the *and* of the complement of the first
   integer argument to the *boole* function with the second.

   :type: :class:`<integer>`

   :description:

     The logical operator that is the *and* of the complement of the first
     integer argument to the *boole* function with the second. It is a
     suitable first argument to the *boole* function.

.. constant:: $boole-andc2

   The logical operator that is the *and* of the first integer argument to
   the *boole* function with the second with the complement of the second.

   :type: :class:`<integer>`

   :description:

     The logical operator that is *and* of the first integer argument to the
     *boole* function with the complement of the second. It is a suitable
     first argument to the boole function.

.. constant:: $boole-orc1

   The logical operator that is the *or* of the complement of the first
   integer argument to the *boole* function with the second.

   :type: :class:`<integer>`

   :description:

     The logical operator that is the *or* of the complement of the first
     integer argument to the *boole* function with the second. It is a
     suitable first argument to the *boole* function.

.. constant:: $boole-orc2

   The logical operator that is the *or* of the first integer argument to
   the *boole* function with the second with the complement of the second.

   :type: :class:`<integer>`

   :description:

     The logical operator that is *or* of the first integer argument to the
     *boole* function with the complement of the second. It is a suitable
     first argument to the *boole* function.

.. constant:: $bricks-stipple

   A stipple pattern for use in creating a patterned brush with horizontal
   and vertical lines in the pattern of the mortar in a brick wall.

   :type: :class:`<array>`

   :description:

     A stipple pattern for use in creating a patterned brush with horizontal
     and vertical lines in the pattern of the mortar in a brick wall.
  
   See also

   - :gf:`brush-stipple`

.. class:: <brush>
   :abstract:
   :instantiable:

   The protocol class for brushes.

   :superclasses: :class:`<object>`

   :keyword foreground: An instance of type :class:`<ink>`.
   :keyword background: An instance of type :class:`<ink>`.
   :keyword mode: An instance of type ``<integer>``.
   :keyword fill-style: An instance of type *false-or(<integer>)*. Default value: ``#f``.
   :keyword fill-rule: An instance of type *false-or(<integer>)*. Default value: ``#f``.
   :keyword tile: An instance of type *false-or(<integer>)*. Default value: ``#f``.
   :keyword stipple: An instance of type *false-or(<integer>)*. Default value: ``#f``.
   :keyword ts-x: An instance of *false-or(<integer>).* Default value: ``#f``.
   :keyword ts-y: An instance of *false-or(<integer>).* Default value: ``#f``.

   :description:

     The protocol class for brushes.

   :operations:

     The following operations are exported from the *DUIM-DCs* module.

     - :gf:`=`
     - :gf:`brush?`
     - :gf:`brush-background`
     - :gf:`brush-fill-rule`
     - :gf:`brush-fill-style`
     - :gf:`brush-foreground`
     - :gf:`brush-mode`
     - :gf:`brush-stipple`
     - :gf:`brush-stretch-mode`
     - :gf:`brush-tile`
     - :gf:`brush-ts-x`
     - :gf:`brush-ts-y`

   See also

   - :gf:`make`

.. generic-function:: brush?

   Returns ``#t`` if its argument is a brush.

   :signature: brush? *object* => *boolean*

   :parameter object: An instance of type ``<object>``.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if its argument is a brush.

.. generic-function:: brush-background

   Returns the ink that is the background color of a brush.

   :signature: brush-background *brush* => *ink*

   :parameter brush: An instance of type :class:`<brush>`.

   :value ink: An instance of type :class:`<ink>`.

   :description:

     Returns the *ink* that is the background color of *brush*.

   See also

   - :gf:`brush-fill-rule`

.. generic-function:: brush-fill-rule

   Returns the fill rule of the brush.

   :signature: brush-fill-rule *brush* => *fill-rule*

   :parameter brush: An instance of type :class:`<brush>`.

   :value fill-rule: An instance of type ``fill-rule`` or ``<boolean>``.

   :description:

     Returns the fill rule for *brush*, or ``#f`` if *brush* does not have a
     fill rule.

   See also

   - :gf:`brush-fill-style`

.. generic-function:: brush-fill-style

   Returns the fill style of the brush.

   :signature: brush-fill-style *brush* => *fill-style*

   :parameter brush: An instance of type :class:`<brush>`.

   :value fill-style: An instance of ``fill-style`` or ``<boolean>``.

   :description:

     Returns the fill style of *brush*, or ``#f``, if *brush* does not have a
     fill style.

   See also

   - :gf:`brush-fill-rule`

.. generic-function:: brush-foreground

   Returns the ink that is the foreground color of a brush.

   :signature: brush-foreground *brush* => *ink*

   :parameter brush: An instance of type :class:`<brush>`.

   :value ink: An instance of type :class:`<ink>`.

   :description:

     Returns the *ink* that is the foreground color of *brush*.

   See also

   - :gf:`brush-stipple`

.. generic-function:: brush-mode

   Returns an integer representing the drawing mode of a brush.

   :signature: brush-mode *brush* => *integer*

   :parameter brush: An instance of type :class:`<brush>`.

   :value integer: An instance of type ``<integer>``. Default value: *$boole-1*.

   :description:

     Returns an integer representing the drawing mode of *brush*.

   See also

   - :const:`$boole-1`

.. generic-function:: brush-stipple

   Returns the stipple pattern of a brush.

   :signature: brush-stipple *brush* => *stipple*

   :parameter brush: An instance of type :class:`<brush>`.

   :value stipple: A *(stipple)* or ``#f``.

   :description:

     Returns the stipple pattern of *brush*.

   See also

   - :gf:`brush-tile`
   - :gf:`brush-fill-rule`
   - :gf:`brush-fill-style`

.. generic-function:: brush-stretch-mode

   Returns the stretch mode of the brush.

   :signature: brush-stretch-mode *brush* => *stretch-mode*

   :parameter brush: An instance of type :class:`<brush>`.

   :value stretch-mode: An instance of *stretch-mode* or ``<boolean>``.

   :description:

     Returns the stretch mode of the brush.

.. generic-function:: brush-tile

   Returns the tile pattern of a brush.

   :signature: brush-tile *brush* => *image*

   :parameter brush: An instance of type :class:`<brush>`.

   :value image: An instance of type :class:`<image>`.

   :description:

     Returns the tile pattern of *brush*.

   See also

   - :gf:`brush-stipple`
   - :gf:`brush-ts-x`
   - :gf:`brush-ts-y`

.. generic-function:: brush-ts-x

   Returns the value of the *x* coordinate that is used to align the
   brush’s tile or stipple pattern.

   :signature: brush-ts-x *brush* => *value*

   :parameter brush: An instance of type :class:`<brush>`.

   :value value: An instance of type *false-or(<integer>)*.

   :description:

     Returns the value of the *x* coordinate that is used to align the tile
     or stipple pattern of *brush*. If *brush* has no tile or stipple
     pattern, *brush-ts-x* returns ``#f``.

   See also

   - :gf:`brush-ts-y`

.. generic-function:: brush-ts-y

   Returns the value of the *y* coordinate that is used to align the
   brush’s tile or stipple pattern.

   :signature: brush-ts-y *brush* => *value*

   :parameter brush: An instance of type :class:`<brush>`.

   :value value: An instance of type *false-or(<integer>)*.

   :description:

     Returns the value of the *y* coordinate that is used to align the tile
     or stipple pattern of *brush*. If *brush* has no tile or stipple
     pattern, *brush-ts-y* returns *#f.*

   See also

   - :gf:`brush-ts-x`

.. class:: <color>
   :abstract:
   :instantiable:

   The protocol class for colors.

   :superclasses: :class:`<ink>`

   :keyword red: An instance of type ``<real>``.
   :keyword green: An instance of type ``<real>``.
   :keyword blue: An instance of type ``<real>``.
   :keyword intensity: An instance of type *limited(<real>, min: 0, max: sqrt(3()*.
   :keyword hue: An instance of type *limited(<real>, min: 0, max: 1)*.
   :keyword saturation: An instance of type *limited(<real>, min: 0, max: 1)*.
   :keyword opacity: An instance of type *limited(<real>, min: 0, max: 1)*.

   :description:

     The :class:`<color>` class is the protocol class for a color, and is a subclass
     of :class:`<ink>`. A member of the class :class:`<color>` is an
     ink that represents the intuitive definition of color: white, black,
     red, pale yellow, and so forth. The visual appearance of a single point
     is completely described by its color. Drawing a color sets the color of
     every point in the drawing plane to that color, and sets the opacity to
     1.

     The *red:*, *green:*, and *blue:* init-keywords represent the red,
     green, and blue components of the color. For an 8-bit color scheme,
     these can take any real number in the range 0 to 255.

     The intensity describes the brightness of the color. An intensity of 0
     is black.

     The hue of a color is the characteristic that is represented by a name
     such as red, green, blue and so forth. This is the main attribute of a
     color that distinguishes it from other colors.

     The saturation describes the amount of white in the color. This is what
     distinguishes pink from red.

     Opacity controls how new color output covers previous color output (that
     is, the final appearance when one color is painted on top of another).
     Opacity can vary from totally opaque (a new color completely obliterates
     the old color) to totally transparent (a new color has no effect
     whatsoever; the old color remains unchanged). Intermediate opacity
     values result in color blending so that the earlier color shows through
     what is drawn on top of it.

     All of the standard instantiable color classes provided by DUIM are
     immutable.

     A color can be specified by four real numbers between 0 and 1
     (inclusive), giving the amounts of red, green, blue, and opacity
     (*alpha*). Three 0's for the RGB components mean black; three 1's mean
     white. The intensity-hue-saturation color model is also supported, but
     the red-green-blue color model is the primary model we will use in the
     specification.

     An opacity may be specified by a real number between 0 and 1
     (inclusive). 0 is completely transparent, 1 is completely opaque,
     fractions are translucent. The opacity of a color is the degree to which
     it hides the previous contents of the drawing plane when it is drawn.

   :operations:

     The following operations are exported from the *DUIM-DCs* module.

     - :gf:`=`
     - :gf:`color?`
     - :gf:`color-rgb`
     - :gf:`color-ihs`
     - :gf:`color-luminosity`

   See also

   - :gf:`color?`
   - :gf:`color-ihs`
   - :gf:`color-luminosity`
   - :class:`<color-not-found>`
   - :gf:`color-palette?`
   - :gf:`color-rgb`
   - :class:`<ink>`

.. generic-function:: color?

   Returns ``#t`` if object is a color.

   :signature: color? *object* => *boolean*

   :parameter object: An instance of type ``<object>``.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if object is a color, otherwise returns ``#f``.

   See also

   - :class:`<color>`
   - :gf:`color-ihs`
   - :gf:`color-luminosity`
   - :class:`<color-not-found>`
   - :gf:`color-palette?`
   - :gf:`color-rgb`

.. generic-function:: color-ihs

   Returns four values, the intensity, hue, saturation, and opacity
   components of a color.

   :signature: color-ihs *color* => *intensity* *hue* *saturation* *opacity*

   :parameter color: An instance of type :class:`<color>`.

   :value intensity: An instance of type *limited(<real>, min: 0, max: sqrt(3()*.
   :value hue: An instance of type *limited(<real>, min: 0, max: 1)*.
   :value saturation: An instance of type *limited(<real>, min: 0, max: 1)*.
   :value opacity: An instance of type *limited(<real>, min: 0, max: 1)*.

   :description:

     Returns four values, the *intensity*, *hue,* *saturation*, and
     *opacity* components of the color *color*. The first value is a real
     number between *0* and *sqrt{3* } (inclusive). The second and third
     values are real numbers between *0* and *1* (inclusive).

   See also

   - :class:`<color>`
   - :gf:`color?`
   - :gf:`color-luminosity`
   - :gf:`color-palette?`
   - :gf:`color-rgb`

.. generic-function:: color-luminosity

   Returns the brightness of a color.

   :signature: color-luminosity* *color* => *luminosity*

   :parameter color: An instance of type :class:`<color>`.

   :value luminosity: An instance of type *limited(<real>, min: 0, max: 1)*.

   :description:

     Returns the brightness of color *color* as real number between *0* and
     *1*. The value is the solution of a function that describes the
     perception of the color by the human retina.

   See also

   - :class:`<color>`
   - :gf:`color?`
   - :gf:`color-ihs`
   - :gf:`color-palette?`
   - :gf:`color-rgb`

.. class:: <color-not-found>
   :sealed:
   :concrete:

   The class of the error that is signalled when a color that is not
   available is requested.

   :superclasses: :class:`<error>`

   :keyword color: An instance of type :class:`<color>`.

   :description:

     The class of the error that is signalled when a color that is not
     available is requested. The *color:* init-keyword is used to specify the
     color that was requested but was not available.

   :operations:

     - None.

   See also

   - :class:`<color>`
   - :gf:`find-color`
   - :gf:`remove-colors`
   - :gf:`find-color`

.. generic-function:: color-palette?

   Returns ``#t`` if the stream or medium supports color.

   :signature: color-palette? *palette* => *boolean*

   :parameter palette: An instance of type :class:`<palette>`.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if the stream or medium supports color.

   See also

   - :class:`<color>`
   - :gf:`color?`
   - :gf:`color-ihs`
   - :gf:`color-luminosity`
   - :gf:`color-rgb`

.. generic-function:: color-rgb

   Returns four values, the red, green, blue, and opacity components of a
   color.

   :signature: color-rgb *color* => *ref* *green* *blue* *opacity*

   :parameter color: An instance of type :class:`<color>`.

   :value red: An instance of type *limited(<real>, min: 0, max: 1)*
   :value gree: n An instance of type *limited(<real>, min: 0, max: 1)*
   :value blue: An instance of type *limited(<real>, min: 0, max: 1)*
   :value opacity: An instance of type *limited(<real>, min: 0, max: 1)*.

   :description:

     Returns four values, the *red*, *green*, *blue*, and *opacity*
     components of the color *color.* The values are real numbers between 0
     and 1 (inclusive).

   See also

   - :class:`<color>`
   - :gf:`color?`
   - :gf:`color-ihs`
   - :gf:`color-luminosity`
   - :gf:`color-palette?`

.. generic-function:: contrasting-colors-limit

   Returns the number of contrasting colors that can be rendered on the
   current platform.

   :signature: contrasting-colors-limit *port* => *integer*

   :parameter port: An instance of type :class:`<silica>`

   :value integer: An instance of type ``<integer>``.

   :description:

     Returns the number of contrasting colors (or stipple patterns if port is
     monochrome or grayscale) that can be rendered on any medium on the port
     *port*. Implementations are encouraged to make this as large as
     possible, but it must be at least 8. All classes that obey the medium
     protocol must implement a method for this generic function.
  
   See also

   - :gf:`contrasting-dash-patterns-limit`
   - :gf:`make-contrasting-colors`

.. generic-function:: contrasting-dash-patterns-limit

   Returns the number of contrasting dash patterns that the specified port
   can generate.

   :signature: contrasting-dash-patterns-limit *port* => *no-of-patterns*

   :parameter port: An instance of type :class:`<silica>`.

   :value no-of-patterns: An instance of type ``<integer>``.

   :description:

     Returns the number of contrasting dash patterns that the specified port
     can generate.

   See also

   - :gf:`contrasting-colors-limit`
   - :gf:`make-contrasting-dash-patterns`

.. constant:: $cross-hatch

   A stipple pattern for use in creating a patterned brush with alternating
   solid and dashed lines.

   :type: :class:`<array>`

   :description:

     A stipple pattern for use in creating a patterned brush with alternating
     solid and dashed lines.

   See also

   - :class:`<color>`.

.. constant:: $cyan

   The usual definition for the color cyan.

   :type: :class:`<color>`

   :description:

     The usual definition for the color cyan.

   See also

   - :class:`<color>`.

.. constant:: $dash-dot-dot-pen

   A pen that draws a line with two dots between each dash.

   :type: :class:`<pen>`

   :description:

     A pen that draws a line with two dots between each dash. The line width
     is *1* and *dashes:* is *#[4, 1, 1, 1, 1, 1]*.

   See also

   - :class:`<pen>`
   - :const:`$solid-pen`
   - :const:`$magenta`
   - :const:`$dash-dot-pen`
   - :const:`$dotted-pen`

.. constant:: $dash-dot-pen

   A pen that draws a dashed and dotted line.

   :type: :class:`<pen>`

   :description:

     A pen that draws a dashed and dotted line. The line width is *1* and
     *dashes:* is *#[4, 1, 1, 1]*.

   See also

   - :class:`<pen>`
   - :const:`$solid-pen`
   - :const:`$magenta`
   - :const:`$dash-dot-pen`
   - :const:`$dotted-pen`

.. constant:: $dashed-pen

   A pen that draws a dashed line.

   :type: :class:`<pen>`

   :description:

     A pen that draws a dashed line. The line width is *1* and *dashes:* is
     ``#t``.

   See also

   - :class:`<pen>`
   - :const:`$solid-pen`
   - :const:`$magenta`
   - :const:`$dash-dot-pen`
   - :const:`$dotted-pen`

.. generic-function:: default-background

   Returns the ink that is the default background of its argument.

   :signature: default-foreground *object* => *background*

   :parameter object: An instance of type ``<object>``.

   :value background: An instance of type :class:`<ink>`.

   :description:

     Returns the ink that is the default background of its argument.

   See also

   - :gf:`brush-fill-style`
   - :gf:`default-background-setter`
   - :gf:`default-foreground`

.. generic-function:: default-background-setter

   Sets the default background.

   :signature: default-foreground-setter *background* *object* => *background*

   :parameter background: An instance of type :class:`<ink>`.
   :parameter object: An instance of type ``<object>``.

   :value background: An instance of type :class:`<ink>`.

   :description:

     Sets the default background for *object*.

   See also

   - :gf:`brush-fill-style`
   - :gf:`default-background`
   - :gf:`default-foreground-setter`

.. generic-function:: default-foreground

   Returns the ink that is the default foreground of its argument.

   :signature: default-foreground *object* => *foreground*

   :parameter object: An instance of type ``<object>``.

   :value foreground: An instance of type :class:`<ink>`.

   :description:

     Returns the ink that is the default foreground of its argument.

   See also

   - :gf:`brush-fill-rule`
   - :gf:`default-background`
   - :gf:`default-foreground-setter`

.. generic-function:: default-foreground-setter

   Sets the default foreground.

   :signature: default-foreground-setter *foreground* *object* => *foreground*

   :parameter foreground: An instance of type :class:`<ink>`.
   :parameter object: An instance of type ``<object>``.

   :value foreground: An instance of type :class:`<ink>`.

   :description:

     Sets the default foreground for *object*.

   See also

   - :gf:`brush-fill-rule`
   - :gf:`default-background-setter`
   - :gf:`default-foreground`

.. generic-function:: default-text-style

   Returns the default text style for its argument.

   :signature: default-text-style *object* => *text-style*

   :parameter object: An instance of type ``<object>``.

   :value text-style: An instance of type :class:`<text-style>`.

   :description:

     Returns the default text style for its argument.This function is used to
     merge against if the text style is not fully specified, or if no text
     style is specified.

   See also

   - :gf:`default-text-style-setter`

.. generic-function:: default-text-style-setter

   Sets the default text style.

   :signature: default-text-style-setter *text-style* *object* => *text-style*

   :parameter text-style: An instance of type :class:`<text-style>`.
   :parameter object: An instance of type ``<object>``.

   :value text-style: An instance of type :class:`<text-style>`.

   :description:

     Sets the default text style.

   See also

   - :gf:`default-text-style`

.. class:: <device-font>
   :sealed:
   :concrete:

   The protocol class for device-specific fonts.

   :superclasses: :class:`<text-style>`

   :keyword port:
   :keyword font-name:

   :description:

     The protocol class for device-specific fonts.

   :operations:

    - None.

   See also

   - :gf:`<text-style>`

.. constant:: $diagonal-hatch-down

   A stipple pattern for use in creating a patterned brush with alternating
   dashes and spaces.

   :type: :class:`<array>`

   :description:

     A stipple pattern for use in creating a patterned brush with alternating
     dashes and spaces, the first line starting with a dash, followed by a
     space, and the second line starting with a space followed by a dash.

   See also

   - :gf:`brush-stipple`

.. constant:: $diagonal-hatch-up

   A stipple pattern for use in creating a patterned brush with alternating
   dashes and spaces.

   :type: :class:`<array>`

   :description:

     A stipple pattern for use in creating a patterned brush with alternating
     dashes and spaces, the first line starting with a space, followed by a
     dash, and the second line starting with a dash followed by a space.

   See also

   - :gf:`brush-stipple`

.. constant:: $dotted-pen

   A pen that draws a dotted line.

   :type: :class:`<pen>`

   :description:

     A pen that draws a dotted line. The line width is *1* and *dashes:* is
     ``#[1, 1]``.

   See also

   - :class:`<pen>`
   - :const:`$solid-pen`
   - :const:`$dash-dot-pen`

.. generic-function:: find-color

   Looks up and returns a color by name.

   :signature: find-color *name* *palette* #key *error?* => *color*

   :parameter name: An instance of type :class:`<string>`.
   :parameter palette: An instance of type :class:`<palette>`.
   :parameter error?: An instance of type ``<boolean>``. Default value: ``#f``.

   :value color: An instance of type :class:`<color>`.

   :description:

     Looks up and returns a color by name. This is a list of the commonly
     provided color names that can be looked up with *find-color*:

     - alice-blue
     - antique-white
     - aquamarine
     - azure
     - beige
     - bisque
     - black
     - blanched-almond
     - blue
     - blue-violet
     - brown
     - burlywood
     - cadet-blue
     - chartreuse
     - chocolate
     - coral
     - cornflower-blue
     - cornsilk
     - cyan
     - dark-goldenrod
     - dark-green
     - dark-khaki
     - dark-olive-green
     - dark-orange
     - dark-orchid
     - dark-salmon
     - dark-sea-green
     - dark-slate-blue
     - dark-slate-gray
     - dark-turquoise
     - dark-violet
     - deep-pink
     - deep-sky-blue
     - dim-gray
     - dodger-blue
     - firebrick
     - floral-white
     - forest-green
     - gainsboro
     - ghost-white
     - gold
     - goldenrod
     - gray
     - green
     - green-yellow
     - honeydew
     - hot-pink
     - indian-red
     - ivory
     - khaki
     - lavender
     - lavender-blush
     - lawn-green
     - lemon-chiffon
     - light-blue
     - light-coral
     - light-cyan
     - light-goldenrod
     - light-goldenrod-yellow
     - light-gray
     - light-pink
     - light-salmon
     - light-sea-green
     - light-sky-blue
     - light-slate-blue
     - light-slate-gray
     - light-steel-blue
     - light-yellow
     - lime-green
     - linen
     - magenta
     - maroon
     - medium-aquamarine
     - medium-blue
     - medium-orchid
     - medium-purple
     - medium-sea-green
     - medium-slate-blue
     - medium-spring-green
     - medium-turquoise
     - medium-violet-red
     - midnight-blue
     - mint-cream
     - misty-rose
     - moccasin
     - navajo-white
     - navy-blue
     - old-lace
     - olive-drab
     - orange
     - orange-red
     - orchid
     - pale-goldenrod
     - pale-green
     - pale-turquoise
     - pale-violet-red
     - papaya-whip
     - peach-puff
     - peru
     - pink
     - plum
     - powder-blue
     - purple
     - red
     - rosy-brown
     - royal-blue
     - saddle-brown
     - salmon
     - sandy-brown
     - sea-green
     - seashell
     - sienna
     - sky-blue
     - slate-blue
     - slate-gray
     - snow
     - spring-green
     - steel-blue
     - tan
     - thistle
     - tomato
     - turquoise
     - violet
     - violet-red
     - wheat
     - white
     - white-smoke
     - yellow
     - yellow-green

     Application programs can define other colors; these are provided because
     they are commonly used in the X Windows community, not because there is
     anything special about these particular colors.

   See also

   - :gf:`stencil?`
   - :gf:`contrasting-dash-patterns-limit`
   - :const:`$black`
   - :const:`$red`
   - :const:`$yellow`
   - :const:`$green`
   - :const:`$blue`
   - :const:`$magenta`

.. constant:: $foreground

   An indirect ink that uses the medium's foreground design.

   :type: :class:`<ink>`

   :description:

     An indirect ink that uses the medium's foreground design.

   See also

   - :class:`<ink>`
   - :class:`<palette>`

.. generic-function:: fully-merged-text-style?

   Returns ``#t`` if the specified text style is completely specified.

   :signature: fully-merged-text-style? *text-style* => *boolean*

   :parameter text-style: An instance of type :class:`<text-style>`.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if the specified text style is completely specified.

   See also

   - :gf:`merge-text-styles`

.. constant:: $green

   The usual definition of the color green.

   :type: :class:`<color>`

   :description:

     The usual definition of the color green.

   See also

   - :class:`<color>`

.. constant:: $hearts-stipple

   A stipple pattern for use in creating a patterned brush that draws a
   heart shape.

   :type: :class:`<array>`

   :description:

     A stipple pattern for use in creating a patterned brush that draws a
     heart shape.

   See also

   - :gf:`brush-stipple`

.. constant:: $horizontal-hatch

   A stipple pattern for use in creating a patterned brush with alternating
   horizontal rows of lines and spaces.

   :type: :class:`<array>`

   :description:

     A stipple pattern for use in creating a patterned brush with alternating
     horizontal rows of lines and spaces.

   See also

   - :gf:`brush-stipple`

.. class:: <image>
   :abstract:

   The class for objects that are images.

   :superclasses: :class:`<ink>`

   :description:

     The class for objects that are images.

   :operations:

     The following operation is exported from the *DUIM-DCs* module.

     - :gf:`image?`

     The following operation is exported from the *DUIM-Graphics* module.

     - :class:`<graphics>`

   See also

   - :gf:`image?`
   - :gf:`image-depth`
   - :gf:`image-height`
   - :gf:`image-width`
   - :class:`<ink>`

.. generic-function:: image?

   Returns ``#t`` if its argument is an image.

   :signature: image? *object* => *boolean*

   :parameter object: An instance of type ``<object>``.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if its argument is an image.

   See also

   - :class:`<image>`
   - :gf:`image-depth`
   - :gf:`image-height`
   - :gf:`image-width`

.. generic-function:: image-depth

   Returns the depth of an image.

   :signature: image-depth *image* => *depth*

   :parameter image: An instance of type :class:`<image>`.

   :value depth: An instance of type ``<real>``.

   :description:

     Returns the depth of the image *image*.

   See also

   - :class:`<image>`
   - :gf:`image?`
   - :gf:`image-height`
   - :gf:`image-width`

.. generic-function:: image-height

   Returns the height of an image.

   :signature: image-height *image* => *height*

   :parameter image: An instance of type :class:`<image>`.

   :value height: An instance of type ``<real>``.

   :description:

     Returns the height of the image *image*.

   See also

   - :class:`<image>`
   - :gf:`image?`
   - :gf:`image-depth`
   - :gf:`image-width`

.. generic-function:: image-width

   Returns the width of an image.

   :signature: image-width *image* => *width*

   :parameter image: An instance of type :class:`<image>`.

   :value width: An instance of type ``<real>``.

   :description:

     Returns the width of the image *image*.

   See also

   - :class:`<image>`
   - :gf:`image?`
   - :gf:`image-depth`
   - :gf:`image-height`

.. class:: <ink>
   :abstract:

   The class of objects that represent a way of arranging colors and
   opacities in the drawing plane.

   :superclasses: :class:`<object>`

   :description:

     The class of objects that represent a way of arranging colors and
     opacities in the drawing plane. Intuitively, it is anything that can be
     drawn with. An ink is anything that can be used in medium-foreground,
     medium-background, medium-ink, or the foreground or background of a
     brush.

   :operations:

     The following operation is exported from the *DUIM-DCs* module.

     - :gf:`ink?`

   See also

   - :gf:`ink?`

.. generic-function:: ink?

   Returns ``#t`` if its argument is an ink.

   :signature: ink? *object* => *boolean*

   :parameter object: An instance of type ``<object>``.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if *object* is an ink, otherwise returns ``#f``.

   See also

   - :class:`<ink>`

.. constant:: $magenta

   The usual definition of the color magenta.

   :type: :class:`<color>`

   :description:

     The usual definition of the color magenta.

   See also

   - :class:`<color>`

.. generic-function:: make

   Returns an object that is of the same type as the class given as its
   argument.

   :signature: make *(class* *==* *<pen>)* *#key* *width* *units* *dashes* *joint-shape* *cap-shape* => *pen*
   :signature: *make* *(class* *==* *<brush>)* *#key* *foreground* *background* *mode* *fill-style* *fill-rule* *tile* *stipple* *ts-x* *ts-y* => *brush*

   :parameter (class==<pen>): The class :class:`<pen>`.
   :parameter width: An instance of type :class:`<pen-width>`. Default value: *1*.
   :parameter units: An instance of type :class:`<pen-units>`. Default value: *#"normal"*.
   :parameter dashes: An instance of type :class:`<pen-dashes>`. Default value: ``#f``.
   :parameter joint-shape: An instance of type :class:`<pen-joint-shape>`. Default value: *#"miter"*.
   :parameter cap-shape: An instance of type :class:`<pen-cap-shape>`. Default value: *#"butt"*.
   :parameter (class==<brush>): The class :class:`<brush>`.
   :parameter foreground: An instance of type :class:`<ink>`. Default value: *$foreground*.
   :parameter background: An instance of type :class:`<ink>`. Default value: *$background*.
   :parameter mode: An instance of type ``<integer>``. Default value: *$boole-1*.
   :parameter fill-style: A *(fill-style)* or ``#f``. Default value: ``#f``.
   :parameter fill-rule: A *(fill-rule)* or ``#f``. Default value: ``#f``.
   :parameter tile: An *(image)* or ``#f``. Default value: ``#f``.
   :parameter stipple: A *(stipple)* or ``#f``. Default value: ``#f``.
   :parameter ts-x: An instance of *false-or(<integer>).* Default value: ``#f``.
   :parameter ts-y: An instance of *false-or(<integer>).* Default value: ``#f``.

   :value pen: An instance of type :class:`<pen>`.
   :value brush: An instance of type :class:`<brush>`.

   :description:

     Returns an object that is of the same type as the class given as its
     argument. Default values for the keywords that specify object are
     provided, or the keywords can be given explicitly to override the
     defaults.

   See also

   - :class:`<brush>`
   - :class:`<pen>`

.. generic-function:: make-color-for-contrasting-color

   Returns a color that is recognizably different from the main color.

   :signature: make-color-for-contrasting-color *ink* => *color*

   :parameter ink: An instance of type :class:`<ink>`.

   :value color: An instance of type :class:`<color>`.

   :description:

     Returns a color that is recognizably different from the main color.

   See also

   - :func:`make-contrasting-colors`

.. function:: make-contrasting-colors

   Returns a vector of colors with recognizably different appearance.

   :signature: make-contrasting-colors *n* #key *k* => *colors*

   :parameter n: An instance of type ``<integer>``.
   :parameter k: An instance of type ``<integer>``.

   :parameter colors: An instance of type limited(``<sequence>``, of: :class:`<color>`).

   :description:

     Returns a vector of n colors with recognizably different appearance.
     Elements of the vector are guaranteed to be acceptable values for the
     *brush:* argument to the drawing functions, and do not include
     *$foreground*, *$background*, or *nil*. Their class is otherwise
     unspecified. The vector is a fresh object that may be modified.

     If *k* is supplied, it must be an integer between *0* and *n* - *1*
     (inclusive), in which case *make-contrasting-colors* returns the *k* th
     color in the vector rather than the whole vector.

     If the implementation does not have *n* different contrasting colors,
     *make-contrasting-colors* signals an error. This does not happen unless
     *n* is greater than eight.
  
     The rendering of the color is a true color or a stippled pattern,
     depending on whether the output medium supports color.

   See also

   - :gf:`contrasting-colors-limit`
   - :const:`$green`
   - :gf:`make-color-for-contrasting-color`
   - :gf:`make-contrasting-dash-patterns`

.. function:: make-contrasting-dash-patterns

   Returns a vector of dash patterns with recognizably different
   appearances.

   :signature: make-contrasting-dash-patterns *n* *#key* *k* => *dashes*

   :parameter n: An instance of type ``<integer>``.
   :parameter k: An instance of type ``<integer>``.

   :value dashes: An instance of type :class:`<vector>`.

   :description:

     Returns a vector of *n* dash patterns with recognizably different
     appearances. If the keyword *k* is supplied,
     *make-contrasting-dash-patterns* returns the *k* th pattern. If there
     are not n different dash patterns, an error is signalled.

     The argument *n* represents the number of dash patterns.

     The argument *k* represents the index in the vector of dash patterns
     indicating the pattern to use.

   See also

   - :gf:`contrasting-dash-patterns-limit`
   - :gf:`make-contrasting-colors`

.. function:: make-device-font

   Returns a device-specific font.

   :signature: make-device-font *port* *font* => *device-font*

   :parameter port: An instance of type ``<silica>``.
   :parameter font: An instance of type ``<object>``.

   :value device-font: A font object or the name of a font.

   :description:

     Returns a device-specific font. Text styles are mapped to fonts for a
     port, a character set, and a text style. All ports must implement
     methods for the generic functions, for all classes of text style.

     The objects used to represent a font mapping are unspecified and are
     likely to vary from port to port. For instance, a mapping might be some
     sort of font object on one type of port, or might simply be the name of
     a font on another.

     Part of initializing a port is to define the mappings between text
     styles and font names for the port's host window system.

.. function:: make-gray-color

   Returns a member of class :class:`<color>`.

   :signature: make-gray-color *luminosity* #key *opacity* => *color*

   :parameter luminosity: An instance of type *limited(<real>, min: 0, max: 1)*.
   :parameter opacity: An instance of type *limited(<real>, min: 0, max: 1)*. Default value: *1.0*.

   :value color: An instance of type :class:`<color>`.

   :description:

     Returns a member of class :class:`<color>`. The *luminance* is a real number
     between *0* and *1* (inclusive). On a black-on-white display device, *0*
     means black, *1* means white, and the values in between are shades of
     gray. On a white-on-black display device, *0* means white, *1* means
     black, and the values in between are shades of gray.

   See also

   - :gf:`make-ihs-color`
   - :gf:`make-rgb-color`

.. function:: make-ihs-color

   Returns a member of the class :class:`<color>`.

   :signature: make-ihs-color *intensity* *hue* *saturation* #key *opacity* => *color*
   :parameter intensity: An instance of type *limited(<real>, min: 0, max: sqrt(3))*.
   :parameter hue: An instance of type *limited(<real>, min: 0, max: 1)*.
   :parameter saturation: An instance of type *limited(<real>, min: 0, max: 1)*.
   :parameter opacity: An instance of type *limited(<real>, min: 0, max: 1)*. Default value: *1.0*.

   :value color: An instance of type :class:`<color>`.

   :description:

     Returns a member of class :class:`<color>`. The *intensity* argument is a real
     number between *0* and sqrt(*3*) (inclusive). The *hue* and
     *saturation* arguments are real numbers between 0 and 1 (inclusive).

   See also

   - :gf:`make-gray-color`
   - :gf:`make-rgb-color`

.. generic-function:: make-palette

   Returns a member of the class :class:`<palette>`.

   :signature: make-palette *port* *#key* => *palette*

   :parameter port: An instance of type :class:`<silica>`.

   :value palette: An instance of type :class:`<palette>`.

   :description:

     Returns a member of the class :class:`<palette>`.

.. function:: make-pattern

   Returns a pattern generated from a two-dimensional array.

   :signature: make-pattern *array* *colors* => *pattern*

   :parameter array: An instance of type :class:`<array>`.
   :parameter colors: An instance of type limited(<sequence>, of:* :class:`<color>`).

   :value pattern: An instance of type :class:`<pattern>`.

   :description:

     Returns a pattern design that has *(array-dimension* *array* *0)* cells
     in the vertical direction and *(array-dimension* *array* *1}* cells in
     the horizontal direction. *array* must be a two-dimensional array of
     non-negative integers less than the length of *designs*. *designs* must
     be a sequence of designs. The design in cell*i,j* of the resulting
     pattern is the *n* th element of *designs*, if *n* is the value of
     *(aref* *array* *i j* *)*. For example, *array* can be a bit-array and
     *designs* can be a list of two designs, the design drawn for 0 and the
     one drawn for 1. Each cell of a pattern can be regarded as a hole that
     allows the design in it to show through. Each cell might have a
     different design in it. The portion of the design that shows through a
     hole is the portion on the part of the drawing plane where the hole is
     located. In other words, incorporating a design into a pattern does not
     change its alignment to the drawing plane, and does not apply a
     coordinate transformation to the design. Drawing a pattern collects the
     pieces of designs that show through all the holes and draws the pieces
     where the holes lie on the drawing plane. The pattern is completely
     transparent outside the area defined by the array.

     Each cell of a pattern occupies a 1 by 1 square. You can use
     :gf:`transform-region` to scale the pattern to a different cell size and
     shape, or to rotate the pattern so that the rectangular cells become
     diamond-shaped. Applying a coordinate transformation to a pattern does not
     affect the designs that make up the pattern. It only changes the position,
     size, and shape of the cells' holes, allowing different portions of the
     designs in the cells to show through.  Consequently, applying
     *make-rectangular-tile* to a pattern of nonuniform designs can produce
     a different appearance in each tile. The pattern cells' holes are tiled, but
     the designs in the cells are not tiled and a different portion of each of
     those designs shows through in each tile.

.. function:: make-rgb-color

   Returns a member of class :class:`<color>`.

   :signature: make-rgb-color *red* *green* *blue* #key *opacity* => *color*

   :parameter red: An instance of type *limited(<real>, min: 0, max: 1)*.
   :parameter green: An instance of type *limited(<real>, min: 0, max: 1)*.
   :parameter blue: An instance of type *limited(<real>, min: 0, max: 1)*.
   :parameter opacity: An instance of type *limited(<real>, min: 0, max: 1)*. Default value: *1.0*.

   :value color: An instance of type :class:`<color>`.

   :description:

     Returns a member of class :class:`<color>`. The *red*, *green*, and*blue*
     arguments are real numbers between 0 and 1 (inclusive) that specify the
     values of the corresponding color components.

     When all three color components are 1, the resulting color is white.
     When all three color components are 0, the resulting color is black.

   See also

   - :gf:`make-gray-color`
   - :gf:`make-ihs-color`

.. function:: make-stencil

   Returns a pattern design generated from a two-dimensional array.

   :signature: make-stencil *array* => *stencil*

   :parameter array: An instance of type :class:`<array>`.

   :value stencil: An instance of type :class:`<stencil>`.

   :description:

     Returns a pattern design that has (*array-dimension* *array* *0*) cells
     in the vertical direction and (*array-dimension* *array* *1*) cells in
     the horizontal direction. *array* must be a two-dimensional array of
     real numbers between 0 and 1 (inclusive) that represent opacities. The
     design in cell *i,j* of the resulting pattern is the value of
     *(make-opacity (aref* *array* *i j))*.

.. function:: make-text-style

   Returns an instance of :class:`<text-style>`.

   :signature: make-text-style *family* *weight* *slant* *size* #key *underline?* *strikeout?* => *text-style*

   :parameter family: An instance of type *one-of(#"fix", #"serif", #"sans-serif", #f)*.
   :parameter weight: An instance of type *one-of(#"normal", #"condensed", #"thin", #"extra-light", #"light", #"medium", #"demibold", #"bold", #"extra-bold", #"black", #f)*.
   :parameter slant: An instance of type *one-of(#"roman", #"italic", #"oblique", #f)*.
   :parameter size: An instance of :class:`<integer>`, or an instance of type *one-of(#"normal", #"tiny", #"very-small", #"small", #"large", #"very-large:", #"huge", #"smaller", #"larger", #f)*.
   :parameter underline?: An instance of type ``<boolean>``.
   :parameter strikeout?: An instance of type ``<boolean>``.

   :value text-style: An instance of type :class:`<text-style>`.

   :description:

     Returns an instance of :class:`<text-style>`.

     Text style objects have components for family, face, and size. Not all
     of these attributes need be supplied for a given text style object. Text
     styles can be merged in much the same way as pathnames are merged;
     unspecified components in the style object (that is, components that
     have ``#f`` in them) may be filled in by the components of a default style
     object. A text style object is called *fully specified* if none of its
     components is ``#f``, and the size component is not a relative size (that
     is, neither *#"smaller"* nor *#"larger"*).

     If *size* is an integer, it represents the size of the font in printer’s
     points.

     Implementations are permitted to extend legal values for family, face,
     and size.

   See also

   - :const:`$solid-pen`

.. generic-function:: merge-text-styles

   Merges two text styles and returns a new text style that is the same as
   the first, except that unspecified components in are filled in from the
   second.

   :signature: merge-text-styles *text-style* *default-style* => *text-style*

   :parameter text-style: An instance of type :class:`<text-style>`.
   :parameter default-style: An instance of type :class:`<text-style>`.

   :value text-style: An instance of type :class:`<text-style>`.

   :description:

     Merges the text styles *text-style* with *default-style*, that is,
     returns a new text style that is the same as *text-style,* except that
     unspecified components in style1 are filled in from *default-style*.
     For convenience, the two arguments may be also be style specs. Note that
     default-style must be a *fully specified* text style.

     When merging the sizes of two text styles, if the size from the first
     style is a relative size, the resulting size is either the next smaller
     or next larger size than is specified by *default-style*. The ordering
     of sizes, from smallest to largest, is *#"tiny"*, *#"very-small"*,
     *#"small"*, *#"normal"*, *#"large"*,*#"very-large"*, and *#"huge"*.

   See also

   - :gf:`default-background-setter`

.. class:: <palette>
   :abstract:
   :instantiable:

   The protocol class for color palettes.

   :superclasses: :class:`<object>`

   :description:

     The protocol class for color palettes.

   :operations:

    - :gf:`add-colors`
    - :gf:`do-add-colors`
    - :gf:`remove-colors`
    - :gf:`do-remove-colors`
    - :gf:`color-palette?`
    - :gf:`dynamic-palette?`

   See also

   - :gf:`palette?`

.. generic-function:: palette?

   Returns ``#t`` if an object is a palette.

   :signature: palette? *object* => *boolean*

   :parameter object: An instance of type ``<object>``.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if the object *object* is a palette. A palette is a color
     map that maps 16 bit colors into a, for example, 8 bit display.

   See also

   - :class:`<palette>`

.. class:: <palette-full>
   :sealed:
   :concrete:

   The class for errors that are signalled when a color palette is full.

   :superclasses: :class:`<error>`
   :keyword palette:

   :description:

     The class for errors that are signalled when a color palette is full.

   See also

   - :class:`<palette>`

.. constant:: $parquet-stipple

   A stipple pattern for use in creating a patterned brush that looks like
   a parquet floor.

   :type: :class:`<array>`

   :description:

     A stipple pattern for use in creating a patterned brush that looks like
     a parquet floor.

   See also

   - :gf:`brush-stipple`

.. class:: <pattern>
   :sealed:
   :concrete:

   The class for patterns.

   :superclasses: :class:`<stencil>`

   :keyword colors: An instance of type limited(``<sequence>`` of: :class:`<color>`).

   :description:

     The class for patterns. A pattern is a bounded rectangular arrangement
     of color, like a checkerboard. Drawing a pattern draws a different
     design in each rectangular cell of the pattern.

   :operations:

     The following operation is exported from the *DUIM-DCs* module.

     - :gf:`pattern?`

   See also

   - :class:`<stencil>`
   - :gf:`make-pattern`

.. generic-function:: pattern?

   Returns ``#t`` if its argument is a pattern.

   :signature: pattern? *object* => *boolean*

   :parameter object: An instance of type ``<object>``.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if *object* is a pattern.

   See also

   - :gf:`make-pattern`

.. class:: <pen>
   :abstract:
   :instantiable:

   The protocol class for pens.

   :superclasses: :class:`<object>`

   :keyword width: An instance of type ``<integer>``. Default value: *1*.
   :keyword units: An instance of type *one-of(#"normal", #"point", #"device")* . Default value: *#"normal"*.
   :keyword dashes: An instance of type *union(<boolean>, <sequence>)*. Default value: ``#f``.
   :keyword joint-shape: An instance of type *one-of(#"miter", #"bevel", #"round", #"none")*. Default value: *#"miter"*.
   :keyword cap-shape: An instance of type *one-of(#"butt", #"square", #"round", #"no-end-point")*. Default value: *#"butt"*.

   :description:

     The protocol class for pens. A pen imparts ink to a medium.

   :operations:

     The following operations are exported from the *DUIM-DCs* module.

     - :gf:`=`
     - :gf:`pen?`
     - :gf:`pen-cap-shape`
     - :gf:`pen-dashes`
     - :gf:`pen-joint-shape`
     - :gf:`pen-units`
     - :gf:`pen-width`

   See also

   - :class:`<ink>`
   - :gf:`make`
   - :gf:`pen?`
   - :gf:`pen-cap-shape`
   - :gf:`pen-dashes`
   - :gf:`pen-joint-shape`
   - :gf:`pen-units`
   - :gf:`pen-width`

.. generic-function:: pen?

   Returns ``#t`` if its argument is a pen.

   :signature: pen? *object* => *boolean*

   :parameter object: An instance of type ``<object>``.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if *object* is a pen, otherwise returns ``#f``.

   See also

   - :class:`<pen>`
   - :gf:`pen-cap-shape`
   - :gf:`pen-dashes`
   - :gf:`pen-joint-shape`
   - :gf:`pen-units`
   - :gf:`pen-width`

.. generic-function:: pen-cap-shape

   Returns the shape of the end of a line or an arc drawn by the pen.

   :signature: pen-cap-shape *pen* => *value*

   :parameter pen: An instance of type :class:`<pen>`.

   :value value: An instance of type *one-of(#"butt", #"square", #"round", #"no-end-point")*.

   :description:

     Returns the shape of the end of a line or an arc drawn by *pen*.

   See also

   - :gf:`make-contrasting-dash-patterns`
   - :class:`<pen>`
   - :gf:`pen?`
   - :gf:`pen-dashes`
   - :gf:`pen-joint-shape`
   - :gf:`pen-units`
   - :gf:`pen-width`

.. generic-function:: pen-dashes

   Returns ``#t`` if the lines drawn by a pen are dashed.

   :signature: pen-dashes *pen* => *value*

   :parameter pen: An instance of type :class:`<pen>`.

   :value value: An instance of type *type-union(<boolean>, <sequence>)*.

   :description:

     Returns ``#t`` if the lines drawn by *pen* are dashed. The sequence is a
     vector of integers indicating the pattern of dashes. There must be an
     even number of integers. The odd elements in the list indicate the
     length of the inked dashes and the even elements indicate the length of
     the gaps between dashes.

   See also

   - :class:`<pen>`
   - :gf:`pen?`
   - :gf:`pen-cap-shape`
   - :gf:`pen-joint-shape`
   - :gf:`pen-units`
   - :gf:`pen-width`

.. generic-function:: pen-joint-shape

   Returns the shape of the joints between line segments of a closed,
   unfilled figure.

   :signature: pen-joint-shape *pen* => *value*

   :parameter pen: An instance of type :class:`<pen>`.

   :parameter value: An instance of type *one-of(#"miter", #"bevel", #"round", #"none")*.

   :description:

     Returns the shape of the joints between line segments of a closed,
     unfilled figure drawn by *pen*.

   See also

   - :gf:`make-contrasting-dash-patterns`
   - :class:`<pen>`
   - :gf:`pen?`
   - :gf:`pen-cap-shape`
   - :gf:`pen-dashes`
   - :gf:`pen-units`
   - :gf:`pen-width`

.. generic-function:: pen-units

   Returns the units in which the pen width is specified.

   :signature: pen-units *pen* => *value*

   :parameter pen: An instance of type :class:`<pen>`.

   :value value: An instance of type *one-of(#"normal", #"point", #"device")*.

   :description:

     Returns the units in which the pen width is specified. They may be
     normal, points, or device-dependent. A width of *#"normal"* is a
     comfortably visible thin line.

   See also

   - :gf:`make-contrasting-dash-patterns`
   - :class:`<pen>`
   - :gf:`pen?`
   - :gf:`pen-cap-shape`
   - :gf:`pen-dashes`
   - :gf:`pen-joint-shape`
   - :gf:`pen-width`

.. generic-function:: pen-width

   Returns the pen-width, that is how wide a stroke the pen draws, of its
   argument.

   :signature: pen-width *pen* => *width*

   :parameter pen: An instance of type :class:`<pen>`.

   :value width: An instance of type :class:`<pen-width>`. The units that specify the width of the pen may be *#"normal"*, *#"points"*, or *#"device"*.

   :description:

     Returns the pen width, that is how wide a stroke the pen draws, of *pen*
     . A width of *#"normal"* is a comfortably visible thin line.

   See also

   - :gf:`make-contrasting-dash-patterns`
   - :class:`<pen>`
   - :gf:`pen?`
   - :gf:`pen-cap-shape`
   - :gf:`pen-dashes`
   - :gf:`pen-joint-shape`
   - :gf:`pen-units`

.. generic-function:: read-image

   Reads an image.

   :signature: read-image *resource-id* *#key image-type:* *image-type* *#all-keys* => *image*

   :parameter locator: An instance of type *type-union(<string>, <locator>)*.
   :parameter image-type: On Windows, an instance of type *one-of(#"bitmap", #"icon")*.

   :value image: An instance of type :class:`<image>`.

   :description:

     Reads an image from the location *resource-id*. This function calls
     *read-image-as*.

   See also

   - :gf:`read-image-as`

.. generic-function:: read-image-as

   Reads an image.

   :signature: read-image-as *class* *locator* *image-type* #key #all-keys => *image*

   :parameter class: An instance of type ``<object>``.
   :parameter locator: An instance of type :class:`<string>`.
   :parameter image-type: On Windows, *#"bitmap"* or *#"icon"*.

   :value image: An instance of type :class:`<image>`.

   :description:

     Reads the image in the location pointed to be *locator*, as an instance
     of a particular class*.* This function is called by *read-image.*

     The *class* represents the class that the image is read as an instance
     of.

   See also

   - :gf:`read-image`

.. constant:: $red

   The usual definition of the color red.

   :type: :class:`<color>`

   :description:

     The usual definition of the color red.

   See also

   - :const:`$blue`

.. generic-function:: remove-colors

   Removes one or more colors from a palette and returns the updated
   palette.

   :signature: remove-colors *palette* *#rest* *colors* => *palette*

   :parameter palette: An instance of type :class:`<palette>`.
   :parameter colors: Instances of type :class:`<color>`.

   :value palette:

   :description:

     Removes *colors* from *palette* and returns the updated palette.

.. constant:: $solid-pen

   A pen that draws a solid line.

   :type: :class:`<pen>`

   :description:

     A pen that draws a solid line. The width of the line is *1*, and
     *dashes:* is *#f.*

   See also

   - :class:`<pen>`
   - :func:`make`
   - :const:`$dash-dot-pen`
   - :const:`$dotted-pen`

.. class:: <stencil>
   :concrete:
   :sealed:

   The class for stencils.

   :superclasses: :class:`<image>`

   :keyword array: An instance of type :class:`<array>`. Required.
   :keyword transform: An instance of type :class:`<transform>`. Default value: ``#f``.

   :description:

     The class for stencils. A *stencil* is a special kind of pattern that
     contains only opacities.

   :operations:

     The following operations are exported from the *DUIM-DCs* module.

     - :gf:`image-height`
     - :gf:`image-width`
     - :gf:`stencil?`

     The following operation is exported from the *DUIM-Geometry* module.

     - :gf:`box-edges`

   See also

   - :class:`<image>`
   - :gf:`make-pattern`
   - :gf:`stencil?`

.. generic-function:: stencil?

   Returns ``#t`` if its argument is a stencil.

   :signature: stencil? *object* => *boolean*

   :parameter object: An instance of type ``<object>``.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if its argument is a stencil.

   See also

   - :func:`make-pattern`
   - :class:`<stencil>`

.. class:: <text-style>
   :abstract:
   :instantiable:

   The protocol class for text styles.

   :superclasses: :class:`<object>`

   :keyword family: An instance of type *one-of(#"fix", #"serif", #"sans-serif", #f)*. Default value: ``#f``.
   :keyword weight: An instance of type *one-of(#"normal", #"condensed", #"thin", #"extra-light", #"light", #"medium", #"demibold", #"bold", #"extra-bold", #"black", #f)*.
   :keyword slant: An instance of type *one-of(#"roman", #"italic", #"oblique", #f)*.
   :keyword size: An instance of :class:`<integer>`, or an instance of type *one-of(#"normal", #"tiny", #"very-small", #"small", #"large", #"very-large:", #"huge", #"smaller", #"larger", #f)*. Default value: ``#f``.
   :keyword underline?: An instance of type ``<boolean>``. Default value: ``#f``.
   :keyword strikeout?: An instance of type ``<boolean>``. Default value: ``#f``.

   :description:

     The protocol class for text styles. When specifying a particular
     appearance for rendered characters, there is a tension between
     portability and access to specific font for a display device. DUIM
     provides a portable mechanism for describing the desired *text style* in
     abstract terms. Each port defines a mapping between these abstract style
     specifications and particular device-specific fonts. In this way, an
     application programmer can specify the desired text style in abstract
     terms secure in the knowledge that an appropriate device font will be
     selected at run time. However, some applications may require direct
     access to particular device fonts. The text style mechanism supports
     specifying device fonts by name, allowing the programmer to sacrifice
     portability for control.

     If *size:* is specified as an integer, then it represents the font size
     in printer’s points.

   :operations:

     The following operations are exported from the *DUIM-DCs* module.

     - :gf:`=`
     - :gf:`fully-merged-text-style?`
     - :gf:`merge-text-styles`
     - :gf:`text-style?`
     - :gf:`text-style-components`
     - :gf:`text-style-family`
     - :gf:`text-style-size`
     - :gf:`text-style-slant`
     - :gf:`text-style-strikeout?`
     - :gf:`text-style-underline?`
     - :gf:`text-style-weight`

     The following operations are exported from the *DUIM-Sheets* module.

     - :gf:`medium-default-text-style`
     - :gf:`medium-default-text-style-setter`
     - :gf:`medium-merged-text-style`
     - :gf:`medium-text-style`
     - :gf:`medium-text-style-setter`

   See also

   - :gf:`text-style?`
   - :gf:`text-style-components`
   - :gf:`text-style-family`
   - :gf:`text-style-size`
   - :gf:`text-style-slant`
   - :gf:`text-style-strikeout?`
   - :gf:`text-style-underline?`
   - :gf:`text-style-weight`

.. generic-function:: text-style?

   Returns ``#t`` if its argument is a text-style.

   :signature: text-style? *object* => *text-style?*

   :parameter object: An instance of type ``<object>``.

   :value text-style?: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if its argument is a text-style.

   See also

   - :gf:`<text-style>`
   - :gf:`text-style-components`
   - :gf:`text-style-family`
   - :gf:`text-style-size`
   - :gf:`text-style-slant`
   - :gf:`text-style-strikeout?`
   - :gf:`text-style-underline?`
   - :gf:`text-style-weight`

.. generic-function:: text-style-components

   Returns the components of a text style as the values family, face,
   slant, size, underline and strikeout.

   :signature: text-style-components *text-style* => *family* *weight* *slant* *size* *underline?* *strikeout?*

   :parameter text-style: An instance of type :class:`<text-style>`.

   :value family: An instance of type *one-of(#"fix", #"serif", #"sans-serif", #f)*.
   :value weight: An instance of type *one-of(#"normal", #"condensed", #"thin", #"extra-light", #"light", #"medium", #"demibold", #"bold", #"extra-bold", #"black", #f)*.
   :parameter slant: An instance of type *one-of(#"roman", #"italic", #"oblique", #f)*.
   :value size: An instance of :class:`<integer>`, or an instance of type *one-of(#"normal", #"tiny", #"very-small", #"small", #"large", #"very-large:", #"huge", #"smaller", #"larger", #f)*. Default value: ``#f``.
   :value underline?: An instance of type ``<boolean>``.
   :value strikeout?: An instance of type ``<boolean>``.

   :description:

     Returns the components of the text style *text-style* as the values
     family, face, slant, size, underline and strikeout.

   See also

   - :class:`<text-style>`
   - :gf:`text-style?`
   - :gf:`text-style-family`
   - :gf:`text-style-size`
   - :gf:`text-style-slant`
   - :gf:`text-style-strikeout?`
   - :gf:`text-style-underline?`
   - :gf:`text-style-weight`

.. generic-function:: text-style-family

   Returns the family component of the specified text style.

   :signature: text-style-family *text-style* => *family*

   :parameter text-style: An instance of type :class:`<text-style>`.

   :value family: An instance of type *one-of(#"fix", #"serif", #"sans-serif", #f)*.

   :description:

     Returns the family component of the specified text style.

   See also

   - :class:`<text-style>`
   - :gf:`text-style?`
   - :gf:`text-style-components`
   - :gf:`text-style-size`
   - :gf:`text-style-slant`
   - :gf:`text-style-strikeout?`
   - :gf:`text-style-underline?`
   - :gf:`text-style-weight`

.. generic-function:: text-style-size

   Returns the style component of the specified text style.

   :signature: text-style-size *text-style* => *size*

   :parameter text-style: An instance of type :class:`<text-style>`.

   :value size: An instance of :class:`<integer>`, or an instance of type *one-of(#"normal", #"tiny", #"very-small", #"small", #"large", #"very-large:", #"huge", #"smaller", #"larger", #f)*. Default value: ``#f``.

   :description:

     Returns the style component of the specified text style.

   See also

   - :class:`<text-style>`
   - :gf:`text-style?`
   - :gf:`text-style-components`
   - :gf:`text-style-family`
   - :gf:`text-style-slant`
   - :gf:`text-style-strikeout?`
   - :gf:`text-style-underline?`
   - :gf:`text-style-weight`

.. generic-function:: text-style-slant

   Returns the slant component of the specified text style.

   :signature: text-style-slant *text-style* => *slant*

   :parameter text-style: An instance of type :class:`<text-style>`.

   :value slant: An instance of type *one-of(#"roman", #"italic", #"oblique", #f)*.

   :description:

     Returns the slant component of the specified text style.

   See also

   - :class:`<text-style>`
   - :gf:`text-style?`
   - :gf:`text-style-components`
   - :gf:`text-style-family`
   - :gf:`text-style-size`
   - :gf:`text-style-strikeout?`
   - :gf:`text-style-underline?`
   - :gf:`text-style-weight`

.. generic-function:: text-style-strikeout?

   Returns ``#t`` if the text style includes a line through it, striking it
   out.

   :signature: text-style-strikeout? *text-style* => *strikeout?*

   :parameter text-style: An instance of type :class:`<text-style>`.

   :value strikeout?: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if the text style includes a line through it, striking it
     out.

   See also

   - :class:`<text-style>`
   - :gf:`text-style?`
   - :gf:`text-style-components`
   - :gf:`text-style-family`
   - :gf:`text-style-size`
   - :gf:`text-style-slant`
   - :gf:`text-style-underline?`
   - :gf:`text-style-weight`

.. generic-function:: text-style-underline?

   Returns ``#t`` if the text style is underlined.

   :signature: text-style-underline? *text-style* => *underline?*

   :parameter text-style: An instance of type :class:`<text-style>`.

   :value underline?: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if the text style is underlined.

   See also

   - :class:`<text-style>`
   - :gf:`text-style?`
   - :gf:`text-style-components`
   - :gf:`text-style-family`
   - :gf:`text-style-size`
   - :gf:`text-style-slant`
   - :gf:`text-style-strikeout?`
   - :gf:`text-style-weight`

.. generic-function:: text-style-weight

   Returns the weight component of the specified text style.

   :signature: text-style-weight *text-style* => *weight*

   :parameter text-style: An instance of type :class:`<text-style>`.

   :value weight: An instance of type *one-of(#"normal", #"condensed", #"thin", #"extra-light", #"light", #"medium", #"demibold", #"bold", #"extra-bold", #"black", #f)*.

   :description:

     Returns the weight component of the text style.

   See also

   - :class:`<text-style>`
   - :gf:`text-style?`
   - :gf:`text-style-components`
   - :gf:`text-style-family`
   - :gf:`text-style-size`
   - :gf:`text-style-slant`
   - :gf:`text-style-strikeout?`
   - :gf:`text-style-underline?`

.. constant:: $tiles-stipple

   A stipple pattern for use in creating a patterned brush with lines and
   spaces suggesting tiles

   :type: :class:`<array>`

   :description:

     A stipple pattern for use in creating a patterned brush with lines and
     spaces suggesting tiles

   See also

   - :gf:`brush-stipple`

.. constant:: $vertical-hatch

      A stipple pattern for use in creating a patterned brush with alternating
      vertical columns of lines and spaces.

   :type: :class:`<array>`

   :description:

     A stipple pattern for use in creating a patterned brush with alternating
     vertical columns of lines and spaces.

   See also

   - :gf:`brush-stipple`

.. constant:: $white

   The usual definition of white.

   :type: :class:`<color>`

   :description:

     The usual definition of white. In the *rgb* color model, its value is
     *111*.

   See also

   - :class:`<color>`

.. generic-function:: write-image

   Writes out a copy of an image to disk (or other designated medium).

   :signature: write-image *image* *locator* => ()

   :parameter image: An instance of type :class:`<image>`.
   :parameter locator: An instance of type :class:`<string>`.

   :description:

     Writes out a copy of *image* to the designated medium *locator*.

.. constant:: $xor-brush

   A standard brush with the drawing property of *$boole-xor*.

   :type: :class:`<brush>`

   :description:

     A standard brush with the drawing property of *$boole-xor*.

.. constant:: $yellow

   The usual definition of the color yellow.

   :type: :class:`<color>`

   :description:

     The usual definition of the color yellow.

   See also

   - :class:`<color>`
