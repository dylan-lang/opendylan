****************
DUIM-DCs Library
****************

Overview
========

The DUIM-DCs library provides color support to the DUIM library. The
library contains a single module, *duim-dcs*, from which all the
interfaces described in this chapter are exposed. `See DUIM-DCs
Module <dcs.htm#71301>`_ contains complete reference entries for each
exposed interface.

Throughout this chapter, a *drawing context* consists of the combination
of ink, color, brush, pen, palette, and shapes that make up patterns and
images.

The class hierarchy for DUIM-DCs
================================

A number of base classes are exposed in the DUIM-DCs library, each of
which is a subclass of *<object>*. They are shown in Table `See Overall
class hierarchy for the DUIM-DCs library <dcs.htm#80940>`_.

Overall class hierarchy for the DUIM-DCs library
                                                

.. figure:: dcs-2.gif
   :align: center
   :alt: 

<object>

<pen>

<brush>

<palette>

<ink>

See `See Subclasses of <ink> <dcs.htm#98762>`_

<text-style>

<device-font>

-  *<pen>* This is protocol class for pens. A pen is used to draw 1
   dimensional graphics such as lines or outline, using a specific color
   or pattern.
-  *<brush>* The protocol class for brushes. Brushes are used to fill in
   2 dimensional areas with a specific color or pattern.
-  *<palette>* The protocol class for palettes. A palette provides a set
   of colors which can be made available to an application.
-  *<ink>* This class can be thought of as anything that can be drawn.
   As the name implies, an ink describes the color and opacity features
   used by a given pen or brush. That is, the pen and brush define the
   drawing style (outlining or filling, respectively), and an ink is
   used to describe the color or pattern that is drawn. This class has a
   number of subclasses, described in `See Subclasses of
   <ink> <dcs.htm#98762>`_.
-  *<text-style>* The protocol class for text styles. A text style is a
   portable way of describing the appearance of a piece of text on
   screen (its font family, size, weight, and so on) in an abstract
   fashion. Because the fonts available on a particular computer may not
   necessarily match the fonts available on the computer of the
   programmer, DUIM provides a portable model which allows the most
   suitable font on the user’s machine to be chosen at run-time.
-  *<device-font>* The protocol class for device-specific fonts, that
   is, fonts that are resident on a particular device. This is a direct
   subclass of *<text-style>*.

Subclasses of <ink>
-------------------

A number of subclasses of <ink> are exposed by the DUIM-DCs library, as
follows:

-  *<color>* The class of all colors available on the system. This is a
   direct subclass of *<ink>*.
-  *<image>* The class of all images, such as icons and bitmap images.
   Images may often be acquired from an outside source, such as a file
   on disk. This is a direct subclass of *<ink>*.
-  *<stencil>* A stencil is a special kind of pattern that contains only
   opacities, that is, it provides a layer of transparency. This can be
   useful, for instance, when overlaying a color onto an image, so as to
   provide the impression of shading. This is a direct subclass of
   *<image>*.
-  *<pattern>* A pattern is a bounded rectangular arrangement of color,
   like a checkerboard. Drawing a pattern draws a different design in
   each rectangular cell of the pattern. This is a direct subclass of
   *<stencil>*.

Error classes provided by DUIM-DCs
----------------------------------

Two error classes are provided by the DUIM-DCs library, both of which
are immediate subclasses of *<error>*.

*<color-not-found>*
                   

This class of error is signalled when a color is requested but is not
available on the user’s system.
                                                                                                     

-  *<palette-full>* This class of error is signalled when an attempt is
   made to add a color to a palette, and the palette cannot accept any
   more colors. The number of colors in a palette depends on the color
   depth of the connected monitor.

DUIM-DCs Module
===============

This section contains a complete reference of all the interfaces that
are exported from the *duim-dcs* module.

\=
--

G.f. method

Summary

Returns *#t* if two objects are equal.

Signature

.. code-block:: dylan

    = *color1 color2* => *boolean*
    = *pen1* *pen2* => *boolean*
    = *brush1* *brush2* => *boolean*
    = *text-style1* *text-style2* => *value*

Arguments

-  *color1* An instance of type `See <color> <dcs.htm#55341>`_.
-  *color2* An instance of type `See <color> <dcs.htm#55341>`_.
-  *pen1* An instance of type `See <pen> <dcs.htm#41757>`_.
-  *pen2* An instance of type `See <pen> <dcs.htm#41757>`_.
-  *brush1* An instance of type `See <brush> <dcs.htm#29492>`_.
-  *brush2* An instance of type `See <brush> <dcs.htm#29492>`_.
-  *text-style1* An instance of type `See
   <text-style> <dcs.htm#85385>`_.
-  *text-style2* An instance of type `See
   <text-style> <dcs.htm#85385>`_.

Values

-  *boolean* An instance of type *<boolean>*.

Description

Returns *#t* if two objects are equal.

add-colors
----------

Generic function
''''''''''''''''

Summary
       

Adds one or more colors to a palette and returns the updated palette.

Signature
         

*add-colors* *palette* *#rest* *colors* => *palette*
                                                    

Arguments
         

-  *palette* An instance of type `See <palette> <dcs.htm#11494>`_.
-  *colors* Instances of type `See <color> <dcs.htm#55341>`_.

Values
      

-  *palette* An instance of type `See <palette> <dcs.htm#11494>`_.

Description
           

Adds *colors* to *palette* and returns the updated palette.

$background
-----------

Constant
''''''''

Summary
       

An indirect ink that uses the medium's background design.

Type
    

`See <ink> <dcs.htm#15007>`_
                            

Description
           

An indirect ink that uses the medium's background design.

See also
        

`See <palette> <dcs.htm#22591>`_

`See image-height <dcs.htm#44679>`_

$black
------

Constant
''''''''

Summary
       

The usual definition of black.

Type
    

`See <color> <dcs.htm#55341>`_
                              

Description
           

The usual definition black, the absence of all colors. In the *rgb*
color model, its value is *000*.

See also
        

`See <color> <dcs.htm#55341>`_

$blue
-----

Constant
''''''''

Summary
       

The usual definition of the color blue.

Type
    

`See <color> <dcs.htm#55341>`_
                              

Description
           

The usual definition of the color blue.

See also
        

`See <color> <dcs.htm#55341>`_

$boole-clr
----------

Constant
''''''''

Summary
       

The logical operator that is always 0.

Type
    

*<integer>*
           

Description
           

The logical operator that is always 0. It is a suitable first argument
to the *boole* function.

$boole-set
----------

Constant
''''''''

Summary
       

The logical operator that is always 1.

Type
    

*<integer>*
           

Description
           

The logical operator that is always 1. It is a suitable first argument
to the *boole* function.

$boole-1
--------

Constant
''''''''

Summary
       

The logical operator that is always he same as the first integer
argument to the *boole* function.

Type
    

*<integer>*
           

Description
           

The logical operator that is always the same as the first integer
argument to the *boole* function. It is a suitable first argument to the
*boole* function.

$boole-2
--------

Constant
''''''''

Summary
       

The logical operator that is always he same as the second integer
argument to the *boole* function.

Type
    

*<integer>*
           

Description
           

The logical operator that is always the same as the second integer
argument to the *boole* function. It is a suitable first argument to the
*boole* function.

$boole-c1
---------

Constant
''''''''

Summary
       

The logical operator that is always he same as the complement of the
first integer argument to the *boole* function.

Type
    

*<integer>*
           

Description
           

The logical operator that is always the same as the complement of the
first integer argument to the *boole* function. It is a suitable first
argument to the *boole* function.

$boole-c2
---------

Constant
''''''''

Summary
       

The logical operator that is always he same as the complement of the
second integer argument to the *boole* function.

Type
    

*<integer>*
           

Description
           

The logical operator that is always the same as the complement of the
second integer argument to the *boole* function. It is a suitable first
argument to the *boole* function.

$boole-and
----------

Constant
''''''''

Summary
       

The logical operator *and*.

Type
    

*<integer>*
           

Description
           

The logical operator *and*. It is a suitable first argument to the
*boole* function.

$boole-ior
----------

Constant
''''''''

Summary
       

The logical operator *inclusive* *or*.

Type
    

*<integer>*
           

Description
           

The logical operator *inclusive* *or*. It is a suitable first argument
to the *boole* function.

$boole-xor
----------

Constant
''''''''

Summary
       

The logical operator *exclusive* *or*.

Type
    

*<integer>*
           

Description
           

The logical operator *exclusive* *or*. It is a suitable first argument
to the *boole* function.

$boole-eqv
----------

Constant
''''''''

Summary
       

The logical operator *equivalence* (*exclusive* *nor*).

Type
    

*<integer>*
           

Description
           

The logical operator *equivalence* (*exclusive* *nor*). It is a
suitable first argument to the *boole* function.

$boole-nand
-----------

Constant
''''''''

Summary
       

The logical operator *not-and*.

Type
    

*<integer>*
           

Description
           

The logical operator *not-and*. It is a suitable first argument to the
*boole* function.

$boole-nor
----------

Constant
''''''''

Summary
       

The logical operator *not-or*.

Type
    

*<integer>*
           

Description
           

The logical operator *not-or*. It is a suitable first argument to the
*boole* function.

$boole-andc1
------------

Constant
''''''''

Summary
       

The logical operator that is the *and* of the complement of the first
integer argument to the *boole* function with the second.

Type
    

*<integer>*
           

Description
           

The logical operator that is the *and* of the complement of the first
integer argument to the *boole* function with the second. It is a
suitable first argument to the *boole* function.

$boole-andc2
------------

Constant
''''''''

Summary
       

The logical operator that is the *and* of the first integer argument to
the *boole* function with the second with the complement of the second.

Type
    

*<integer>*
           

Description
           

The logical operator that is *and* of the first integer argument to the
*boole* function with the complement of the second. It is a suitable
first argument to the boole function.

$boole-orc1
-----------

Constant
''''''''

Summary
       

The logical operator that is the *or* of the complement of the first
integer argument to the *boole* function with the second.

Type
    

*<integer>*
           

Description
           

The logical operator that is the *or* of the complement of the first
integer argument to the *boole* function with the second. It is a
suitable first argument to the *boole* function.

$boole-orc2
-----------

Constant
''''''''

Summary
       

The logical operator that is the *or* of the first integer argument to
the *boole* function with the second with the complement of the second.

Type
    

*<integer>*
           

Description
           

The logical operator that is *or* of the first integer argument to the
*boole* function with the complement of the second. It is a suitable
first argument to the *boole* function.

$bricks-stipple
---------------

Constant
''''''''

Summary
       

A stipple pattern for use in creating a patterned brush with horizontal
and vertical lines in the pattern of the mortar in a brick wall.

Type
    

*<array>*
         

Description
           

A stipple pattern for use in creating a patterned brush with horizontal
and vertical lines in the pattern of the mortar in a brick wall.

See also
        

`See brush-stipple <dcs.htm#15280>`_

<brush>
-------

Abstract instantiable class
'''''''''''''''''''''''''''

Summary
       

The protocol class for brushes.

Superclasses
            

*<object>*
          

Init-keywords
             

-  *foreground:* An instance of type `See <ink> <dcs.htm#15007>`_.
-  *background:* An instance of type `See <ink> <dcs.htm#15007>`_.
-  *mode:* An instance of type *<integer>*.
-  *fill-style:* An instance of type *false-or(<integer>)*. Default
   value: *#f*.
-  *fill-rule:* An instance of type *false-or(<integer>)*.** Default
   value: *#f*.
-  *tile:* An instance of type *false-or(<integer>)*.** Default value:
   *#f*.
-  *stipple:* An instance of type *false-or(<integer>)*.** Default
   value: *#f*.
-  *ts-x:* An instance of *false-or(<integer>).* Default value: *#f*.
-  *ts-y:* An instance of *false-or(<integer>).* Default value: *#f*.

Description
           

The protocol class for brushes.

Operations
          

The following operations are exported from the *DUIM-DCs* module.

`See = <dcs.htm#33460>`_ `See brush? <dcs.htm#22718>`_ `See
brush-background <dcs.htm#14082>`_ `See
brush-fill-rule <dcs.htm#18072>`_ `See
brush-fill-style <dcs.htm#50315>`_ `See
brush-foreground <dcs.htm#65828>`_ `See brush-mode <dcs.htm#64704>`_
`See brush-stipple <dcs.htm#15280>`_ `See
brush-stretch-mode <dcs.htm#45680>`_ `See brush-tile <dcs.htm#85529>`_
`See brush-ts-x <dcs.htm#99193>`_ `See brush-ts-y <dcs.htm#12940>`_

See also
        

`See make <dcs.htm#81960>`_

brush?
------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if its argument is a brush.

Signature
         

*brush?* *object* => *boolean*
                              

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if its argument is a brush.

brush-background
----------------

Generic function
''''''''''''''''

Summary
       

Returns the ink that is the background color of a brush.

Signature
         

*brush-background* *brush* => *ink*
                                   

Arguments
         

-  *brush* An instance of type `See <brush> <dcs.htm#29492>`_.

Values
      

-  *ink* An instance of type `See <ink> <dcs.htm#15007>`_.

Description
           

Returns the *ink* that is the background color of *brush*.

See also
        

`See brush-fill-rule <dcs.htm#18072>`_

brush-fill-rule
---------------

Generic function
''''''''''''''''

Summary
       

Returns the fill rule of the brush.

Signature
         

*brush-fill-rule* *brush* => *fill-rule*
                                        

Arguments
         

-  *brush* An instance of type `See <brush> <dcs.htm#29492>`_.

Values
      

-  *fill-rule* An instance of type *fill-rule* or *<boolean>*.

Description
           

Returns the fill rule for *brush*, or *#f* if *brush* does not have a
fill rule.

See also
        

`See brush-fill-style <dcs.htm#50315>`_

brush-fill-style
----------------

Generic function
''''''''''''''''

Summary
       

Returns the fill style of the brush.

Signature
         

*brush-fill-style* *brush* => *fill-style*
                                          

Arguments
         

-  *brush* An instance of type `See <brush> <dcs.htm#29492>`_.

Values
      

-  *fill-style* An instance of *fill-style* or *<boolean>*.

Description
           

Returns the fill style of *brush*, or *#f*, if *brush* does not have a
fill style.

See also
        

`See brush-fill-rule <dcs.htm#18072>`_.

brush-foreground
----------------

Generic function
''''''''''''''''

Summary
       

Returns the ink that is the foreground color of a brush.

Signature
         

*brush-foreground* *brush* => *ink*
                                   

Arguments
         

-  *brush* An instance of type `See <brush> <dcs.htm#29492>`_.

Values
      

-  *ink* An instance of type `See <ink> <dcs.htm#15007>`_.

Description
           

Returns the *ink* that is the foreground color of *brush*.

See also
        

`See brush-stipple <dcs.htm#55088>`_.

brush-mode
----------

Generic function
''''''''''''''''

Summary
       

Returns an integer representing the drawing mode of a brush.

Signature
         

*brush-mode* *brush* => *integer*
                                 

Arguments
         

-  *brush* An instance of type `See <brush> <dcs.htm#29492>`_.

Values
      

-  *integer* An instance of type *<integer>*. Default value: *$boole-1*
   .

Description
           

Returns an integer representing the drawing mode of *brush*.

See also
        

`See $boole-1 <dcs.htm#36174>`_.

brush-stipple
-------------

Generic function
''''''''''''''''

Summary
       

Returns the stipple pattern of a brush.

Signature
         

*brush-stipple* *brush* => *stipple*
                                    

Arguments
         

-  *brush* An instance of type `See <brush> <dcs.htm#29492>`_.

Values
      

-  *stipple* A *(stipple)* or *#f*.

Description
           

Returns the stipple pattern of *brush*.

See also
        

`See brush-tile <dcs.htm#85529>`_

`See brush-fill-rule <dcs.htm#81564>`_

`See brush-fill-style <dcs.htm#50315>`_

brush-stretch-mode
------------------

Generic function
''''''''''''''''

Summary
       

Returns the stretch mode of the brush.

Signature
         

*brush-stretch-mode* *brush* => *stretch-mode*
                                              

Arguments
         

-  *brush* An instance of type `See <brush> <dcs.htm#29492>`_.

Values
      

-  *stretch-mode* An instance of *stretch-mode* or *<boolean>*.

Description
           

Returns the stretch mode of the brush.

brush-tile
----------

Generic function
''''''''''''''''

Summary
       

Returns the tile pattern of a brush.

Signature
         

*brush-tile* *brush* => *image*
                               

Arguments
         

-  *brush* An instance of type `See <brush> <dcs.htm#29492>`_.

Values
      

-  *image* An instance of type *<image>*.

Description
           

Returns the tile pattern of *brush*.

See also
        

`See brush-stipple <dcs.htm#15280>`_.

`See brush-ts-x <dcs.htm#99193>`_ and `See brush-ts-y <dcs.htm#12940>`_.

brush-ts-x
----------

Generic function
''''''''''''''''

Summary
       

Returns the value of the *x* coordinate that is used to align the
brush’s tile or stipple pattern.

Signature
         

*brush-ts-x* *brush* => *value*
                               

Arguments
         

-  *brush* An instance of type `See <brush> <dcs.htm#29492>`_.

Values
      

-  *value* An instance of type *false-or(<integer>)*.

Description
           

Returns the value of the *x* coordinate that is used to align the tile
or stipple pattern of *brush*. If *brush* has no tile or stipple
pattern, *brush-ts-x* returns *#f.*

See also
        

`See brush-ts-y <dcs.htm#12940>`_.

brush-ts-y
----------

Generic function
''''''''''''''''

Summary
       

Returns the value of the *y* coordinate that is used to align the
brush’s tile or stipple pattern.

Signature
         

*brush-ts-y* *brush* => *value*
                               

Arguments
         

-  *brush* An instance of type `See <brush> <dcs.htm#29492>`_.

Values
      

-  *value* An instance of type *false-or(<integer>)*.

Description
           

Returns the value of the *y* coordinate that is used to align the tile
or stipple pattern of *brush*. If *brush* has no tile or stipple
pattern, *brush-ts-y* returns *#f.*

See also
        

`See brush-ts-x <dcs.htm#99193>`_.

<color>
-------

Abstract instantiable class
'''''''''''''''''''''''''''

Summary
       

The protocol class for colors.

Superclasses
            

`See <ink> <dcs.htm#15007>`_
                            

Init-keywords
             

-  *red:* An instance of type *<real>*.
-  *green:* An instance of type *<real>*.
-  *blue:* An instance of type *<real>*.
-  *intensity:* An instance of type *limited(<real>, min: 0, max:
   sqrt(3()*.
-  *hue:* An instance of type *limited(<real>, min: 0, max: 1)*.
-  *saturation:* An instance of type *limited(<real>, min: 0, max: 1)*.
-  *opacity:* An instance of type *limited(<real>, min: 0, max: 1)*.

Description
           

The *<color>* class is the protocol class for a color, and is a subclass
of `See <ink> <dcs.htm#15007>`_. A member of the class *<color>* is an
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

Operations
          

The following operations are exported from the *DUIM-DCs* module.

`See = <dcs.htm#33460>`_ `See color? <dcs.htm#29100>`_ `See
color-rgb <dcs.htm#57760>`_ `See color-ihs <dcs.htm#59844>`_ `See
color-luminosity <dcs.htm#89130>`_

See also
        

`See color? <dcs.htm#29100>`_

`See color-ihs <dcs.htm#59844>`_

`See color-luminosity <dcs.htm#89130>`_

`See <color-not-found> <dcs.htm#38318>`_

`See color-palette? <dcs.htm#60680>`_

`See color-rgb <dcs.htm#57760>`_

`See <ink> <dcs.htm#15007>`_

color?
------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if object is a color.

Signature
         

*color?* *object* => *boolean*
                              

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if object is a color, otherwise returns *#f*.

See also
        

`See <color> <dcs.htm#55341>`_

`See color-ihs <dcs.htm#59844>`_

`See color-luminosity <dcs.htm#89130>`_

`See <color-not-found> <dcs.htm#38318>`_

`See color-palette? <dcs.htm#60680>`_

`See color-rgb <dcs.htm#57760>`_

color-ihs
---------

Generic function
''''''''''''''''

Summary
       

Returns four values, the intensity, hue, saturation, and opacity
components of a color.

Signature
         

*color-ihs* *color* => *intensity* *hue* *saturation* *opacity*
                                                               

Arguments
         

-  *color* An instance of type `See <color> <dcs.htm#55341>`_.

Values
      

-  *intensity* An instance of type *limited(<real>, min: 0, max:
   sqrt(3()*.
-  *hue* An instance of type *limited(<real>, min: 0, max: 1)*.
-  *saturation* An instance of type *limited(<real>, min: 0, max: 1)*.
-  *opacity* An instance of type *limited(<real>, min: 0, max: 1)*.

Description
           

Returns four values, the *intensity*, *hue,* *saturation*, and
*opacity* components of the color *color*. The first value is a real
number between *0* and *sqrt{3* } (inclusive). The second and third
values are real numbers between *0* and *1* (inclusive).

See also
        

`See <color> <dcs.htm#55341>`_

`See color? <dcs.htm#29100>`_

`See color-luminosity <dcs.htm#89130>`_

`See color-palette? <dcs.htm#60680>`_

`See color-rgb <dcs.htm#57760>`_

color-luminosity
----------------

Generic function
''''''''''''''''

Summary
       

Returns the brightness of a color.

Signature
         

*color-luminosity* *color* => *luminosity*
                                          

Arguments
         

-  *color* An instance of type `See <color> <dcs.htm#55341>`_.

Values
      

-  *luminosity* An instance of type *limited(<real>, min: 0, max: 1)*.

Description
           

Returns the brightness of color *color* as real number between *0* and
*1*. The value is the solution of a function that describes the
perception of the color by the human retina.

See also
        

`See <color> <dcs.htm#55341>`_

`See color? <dcs.htm#29100>`_

`See color-ihs <dcs.htm#59844>`_

`See color-palette? <dcs.htm#60680>`_

`See color-rgb <dcs.htm#57760>`_

<color-not-found>
-----------------

Sealed concrete class
'''''''''''''''''''''

Summary
       

The class of the error that is signalled when a color that is not
available is requested.

Superclasses
            

<error>
       

Superclasses
            

*<error>*
         

Init-keywords
             

-  *color:* An instance of type `See <color> <dcs.htm#55341>`_.

Description
           

The class of the error that is signalled when a color that is not
available is requested. The *color:* init-keyword is used to specify the
color that was requested but was not available.

Operations
          

-  None.

See also
        

`See <color> <dcs.htm#55341>`_

`See find-color <dcs.htm#33969>`_

`See remove-colors <dcs.htm#25018>`_`See find-color <dcs.htm#33969>`_

color-palette?
--------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if the stream or medium supports color.

Signature
         

*color-palette?* *palette* => *boolean*
                                       

Arguments
         

-  *palette* An instance of type `See <palette> <dcs.htm#11494>`_.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if the stream or medium supports color.

See also
        

`See <color> <dcs.htm#55341>`_

`See color? <dcs.htm#29100>`_

`See color-ihs <dcs.htm#59844>`_

`See color-luminosity <dcs.htm#89130>`_

`See color-rgb <dcs.htm#57760>`_

color-rgb
---------

Generic function
''''''''''''''''

Summary
       

Returns four values, the red, green, blue, and opacity components of a
color.

Signature
         

*color-rgb* *color* => *ref* *green* *blue* *opacity*
                                                     

Arguments
         

-  *color* An instance of type `See <color> <dcs.htm#55341>`_.

Values
      

-  *red* An instance of type *limited(<real>, min: 0, max: 1)*
-  *gree* n An instance of type *limited(<real>, min: 0, max: 1)*
-  *blue* An instance of type *limited(<real>, min: 0, max: 1)*
-  *opacity* An instance of type *limited(<real>, min: 0, max: 1)*.

Description
           

Returns four values, the *red*, *green*, *blue*, and *opacity*
components of the color *color.* The values are real numbers between 0
and 1 (inclusive).

See also
        

`See <color> <dcs.htm#55341>`_

`See color? <dcs.htm#29100>`_

`See color-ihs <dcs.htm#59844>`_

`See color-luminosity <dcs.htm#89130>`_

`See color-palette? <dcs.htm#60680>`_

contrasting-colors-limit
------------------------

Generic function
''''''''''''''''

Summary
       

Returns the number of contrasting colors that can be rendered on the
current platform.

Signature
         

*contrasting-colors-limit* *port* => *integer*
                                              

Arguments
         

-  *port* An instance of type ` <silica.htm#11606>`_.

Values
      

-  *integer* An instance of type *<integer>*.

Description
           

Returns the number of contrasting colors (or stipple patterns if port is
monochrome or grayscale) that can be rendered on any medium on the port
*port*. Implementations are encouraged to make this as large as
possible, but it must be at least 8. All classes that obey the medium
protocol must implement a method for this generic function.

See also
        

`See contrasting-dash-patterns-limit <dcs.htm#18054>`_

`See make-contrasting-colors <dcs.htm#31494>`_

contrasting-dash-patterns-limit
-------------------------------

Generic function
''''''''''''''''

Summary
       

Returns the number of contrasting dash patterns that the specified port
can generate.

Signature
         

*contrasting-dash-patterns-limit* *port* => *no-of-patterns*
                                                            

Arguments
         

-  *port* An instance of type ` <silica.htm#11606>`_.

Values
      

-  *no-of-patterns* An instance of type *<integer>*.

Description
           

Returns the number of contrasting dash patterns that the specified port
can generate.

See also
        

`See contrasting-colors-limit <dcs.htm#43277>`_

`See make-contrasting-dash-patterns <dcs.htm#23504>`_

$cross-hatch
------------

Constant
''''''''

Summary
       

A stipple pattern for use in creating a patterned brush with alternating
solid and dashed lines.

Type
    

*<array>*
         

Description
           

A stipple pattern for use in creating a patterned brush with alternating
solid and dashed lines.

See also
        

`See <color> <dcs.htm#55341>`_.

$cyan
-----

Constant
''''''''

Summary
       

The usual definition for the color cyan.

Type
    

`See <color> <dcs.htm#55341>`_
                              

Description
           

The usual definition for the color cyan.

See also
        

`See <color> <dcs.htm#55341>`_.

$dash-dot-dot-pen
-----------------

Constant
''''''''

Summary
       

A pen that draws a line with two dots between each dash.

Type
    

`See <pen> <dcs.htm#41757>`_
                            

Description
           

A pen that draws a line with two dots between each dash. The line width
is *1* and *dashes:* is *#[4, 1, 1, 1, 1, 1]*.

See also
        

`See <pen> <dcs.htm#41757>`_

`See $solid-pen <dcs.htm#80297>`_

`See $magenta <dcs.htm#41161>`_

`See $dash-dot-pen <dcs.htm#55453>`_

`See $dotted-pen <dcs.htm#76677>`_

$dash-dot-pen
-------------

Constant
''''''''

Summary
       

A pen that draws a dashed and dotted line.

Type
    

`See <pen> <dcs.htm#41757>`_
                            

Description
           

A pen that draws a dashed and dotted line. The line width is *1* and
*dashes:* is *#[4, 1, 1, 1]*.

See also
        

`See <pen> <dcs.htm#41757>`_

`See $solid-pen <dcs.htm#80297>`_

`See $magenta <dcs.htm#41161>`_

`See $dash-dot-pen <dcs.htm#55453>`_

`See $dotted-pen <dcs.htm#76677>`_

$dashed-pen
-----------

Constant
''''''''

Summary
       

A pen that draws a dashed line.

Type
    

`See <pen> <dcs.htm#41757>`_
                            

Description
           

A pen that draws a dashed line. The line width is *1* and *dashes:* is
*#t*.

See also
        

`See <pen> <dcs.htm#41757>`_

`See $solid-pen <dcs.htm#57039>`_

`See $magenta <dcs.htm#41161>`_

`See $dash-dot-pen <dcs.htm#55453>`_

`See $dotted-pen <dcs.htm#76677>`_

default-background
------------------

Generic function
''''''''''''''''

Summary
       

Returns the ink that is the default background of its argument.

Signature
         

*default-foreground* *object* => *background*
                                             

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *background* An instance of type `See <ink> <dcs.htm#15007>`_.

Description
           

Returns the ink that is the default background of its argument.

See also
        

`See brush-fill-style <dcs.htm#50315>`_.

`See default-background-setter <dcs.htm#32743>`_.

`See default-foreground <dcs.htm#40602>`_

default-background-setter
-------------------------

Generic function
''''''''''''''''

Summary
       

Sets the default background.

Signature
         

*default-foreground-setter* *background* *object* => *background*
                                                                 

Arguments
         

-  *background* An instance of type `See <ink> <dcs.htm#15007>`_.
-  *object* An instance of type *<object>*.

Values
      

-  *background* An instance of type `See <ink> <dcs.htm#15007>`_.

Description
           

Sets the default background for *object*.

See also
        

`See brush-fill-style <dcs.htm#50315>`_.

`See default-background <dcs.htm#19900>`_.

`See default-foreground-setter <dcs.htm#48231>`_

default-foreground
------------------

Generic function
''''''''''''''''

Summary
       

Returns the ink that is the default foreground of its argument.

Signature
         

*default-foreground* *object* => *foreground*
                                             

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *foreground* An instance of type `See <ink> <dcs.htm#15007>`_.

Description
           

Returns the ink that is the default foreground of its argument.

See also
        

`See brush-fill-rule <dcs.htm#18072>`_.

`See default-background <dcs.htm#19900>`_

`See default-foreground-setter <dcs.htm#48231>`_

default-foreground-setter
-------------------------

Generic function
''''''''''''''''

Summary
       

Sets the default foreground.

Signature
         

*default-foreground-setter* *foreground* *object* => *foreground*
                                                                 

Arguments
         

-  *foreground* An instance of type `See <ink> <dcs.htm#15007>`_.
-  *object* An instance of type *<object>*.

Values
      

-  *foreground* An instance of type `See <ink> <dcs.htm#15007>`_.

Description
           

Sets the default foreground for *object*.

See also
        

`See brush-fill-rule <dcs.htm#18072>`_.

`See default-background-setter <dcs.htm#32743>`_

`See default-foreground <dcs.htm#40602>`_

default-text-style
------------------

Generic function
''''''''''''''''

Summary
       

Returns the default text style for its argument.

Signature
         

*default-text-style* *object* => *text-style*
                                             

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *text-style* An instance of type *<text-style>*.

Description
           

Returns the default text style for its argument.This function is used to
merge against if the text style is not fully specified, or if no text
style is specified.

See also
        

`See default-text-style-setter <dcs.htm#34503>`_.

default-text-style-setter
-------------------------

Generic function
''''''''''''''''

Summary
       

Sets the default text style.

Signature
         

*default-text-style-setter* *text-style* *object* => *text-style*
                                                                 

Arguments
         

-  *text-style* An instance of type *<text-style>*.
-  *object* An instance of type *<object>*.

Values
      

-  *text-style* An instance of type *<text-style>*.

Description
           

Sets the default text style.

See also
        

`See default-text-style <dcs.htm#95321>`_

<device-font>
-------------

Sealed concrete class
'''''''''''''''''''''

Summary
       

The protocol class for device-specific fonts.

Superclasses
            

<*text-style>*
              

Init-keywords
             

-  *port:*
-  *font-name:*

Description
           

The protocol class for device-specific fonts.

Operations
          

-  None.

See also
        

`See <text-style> <dcs.htm#85385>`_.

$diagonal-hatch-down
--------------------

Constant
''''''''

Summary
       

A stipple pattern for use in creating a patterned brush with alternating
dashes and spaces.

Type
    

*<array>*
         

Description
           

A stipple pattern for use in creating a patterned brush with alternating
dashes and spaces, the first line starting with a dash, followed by a
space, and the second line starting with a space followed by a dash.

See also
        

`See brush-stipple <dcs.htm#15280>`_.

$diagonal-hatch-up
------------------

Constant
''''''''

Summary
       

A stipple pattern for use in creating a patterned brush with alternating
dashes and spaces.

Type
    

*<array>*
         

Description
           

A stipple pattern for use in creating a patterned brush with alternating
dashes and spaces, the first line starting with a space, followed by a
dash, and the second line starting with a dash followed by a space.

See also
        

`See brush-stipple <dcs.htm#15280>`_.

$dotted-pen
-----------

Constant
''''''''

Summary
       

A pen that draws a dotted line.

Type
    

`See <pen> <dcs.htm#41757>`_
                            

Description
           

A pen that draws a dotted line. The line width is *1* and *dashes:* is
*#[1, 1]*.

See also
        

`See <pen> <dcs.htm#41757>`_

`See $solid-pen <dcs.htm#57039>`_

`See $dash-dot-pen <dcs.htm#55453>`_

find-color
----------

Generic function
''''''''''''''''

Summary
       

Looks up and returns a color by name.

Signature
         

*find-color* *name* *palette* #key *error?* => *color*
                                                      

Arguments
         

-  *name* An instance of type *<string>*.
-  *palette* An instance of type `See <palette> <dcs.htm#11494>`_.
-  *error?* An instance of type *<boolean>*. Default value: *#f*.

Values
      

-  *color* An instance of type `See <color> <dcs.htm#55341>`_.

Description
           

Looks up and returns a color by name. `See Common color
names <dcs.htm#29308>`_ lists the commonly provided color names that can
be looked up with *find-color*.

Common color names
                  

.. figure:: dcs-2.gif
   :align: center
   :alt: 
alice-blue

antique-white

aquamarine

azure

beige

bisque

black

blanched-almond

blue

blue-violet

brown

burlywood

cadet-blue

chartreuse

chocolate

coral

cornflower-blue

cornsilk

cyan

dark-goldenrod

dark-green

dark-khaki

dark-olive-green

dark-orange

dark-orchid

dark-salmon

dark-sea-green

dark-slate-blue

dark-slate-gray

dark-turquoise

dark-violet

deep-pink

deep-sky-blue

dim-gray

dodger-blue

firebrick

floral-white

forest-green

gainsboro

ghost-white

gold

goldenrod

gray

green

green-yellow

honeydew

hot-pink

indian-red

ivory

khaki

lavender

lavender-blush

lawn-green

lemon-chiffon

light-blue

light-coral

light-cyan

light-goldenrod

light-goldenrod-yellow

light-gray

light-pink

light-salmon

light-sea-green

light-sky-blue

light-slate-blue

light-slate-gray

light-steel-blue

light-yellow

lime-green

linen

magenta

maroon

medium-aquamarine

medium-blue

medium-orchid

medium-purple

medium-sea-green

medium-slate-blue

medium-spring-green

medium-turquoise

medium-violet-red

midnight-blue

mint-cream

misty-rose

moccasin

navajo-white

navy-blue

old-lace

olive-drab

orange

orange-red

orchid

pale-goldenrod

pale-green

pale-turquoise

pale-violet-red

papaya-whip

peach-puff

peru

pink

plum

powder-blue

purple

red

rosy-brown

royal-blue

saddle-brown

salmon

sandy-brown

sea-green

seashell

sienna

sky-blue

slate-blue

slate-gray

snow

spring-green

steel-blue

tan

thistle

tomato

turquoise

violet

violet-red

wheat

white

white-smoke

yellow

yellow-green

Application programs can define other colors; these are provided because
they are commonly used in the X Windows community, not because there is
anything special about these particular colors.

See also
        

`See $black <dcs.htm#37036>`_

`See stencil? <dcs.htm#21344>`_

`See $red <dcs.htm#17333>`_

`See $yellow <dcs.htm#11828>`_

`See $green <dcs.htm#23573>`_

`See $blue <dcs.htm#10318>`_

`See $magenta <dcs.htm#41161>`_

`See contrasting-dash-patterns-limit <dcs.htm#18054>`_

$foreground
-----------

Constant
''''''''

Summary
       

An indirect ink that uses the medium's foreground design.

Type
    

`See <ink> <dcs.htm#15007>`_
                            

Description
           

An indirect ink that uses the medium's foreground design.

See also
        

`See <ink> <dcs.htm#15007>`_

`See <palette> <dcs.htm#11494>`_

fully-merged-text-style?
------------------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if the specified text style is completely specified.

Signature
         

fully-merged-text-style? *text-style* => *boolean*
                                                  

Arguments
         

-  *text-style* An instance of type *<text-style>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if the specified text style is completely specified.

See also
        

`See merge-text-styles <dcs.htm#14653>`_

$green
------

Constant
''''''''

Summary
       

The usual definition of the color green.

Type
    

`See <color> <dcs.htm#55341>`_
                              

Description
           

The usual definition of the color green.

See also
        

`See <color> <dcs.htm#55341>`_

$hearts-stipple
---------------

Constant
''''''''

Summary
       

A stipple pattern for use in creating a patterned brush that draws a
heart shape.

Type
    

*<array>*
         

Description
           

A stipple pattern for use in creating a patterned brush that draws a
heart shape.

See also
        

`See brush-stipple <dcs.htm#15280>`_

$horizontal-hatch
-----------------

Constant
''''''''

Summary
       

A stipple pattern for use in creating a patterned brush with alternating
horizontal rows of lines and spaces.

Type
    

*<array>*
         

Description
           

A stipple pattern for use in creating a patterned brush with alternating
horizontal rows of lines and spaces.

See also
        

`See brush-stipple <dcs.htm#15280>`_.

<image>
-------

Abstract class
''''''''''''''

Summary
       

The class for objects that are images.

Superclasses
            

`See <ink> <dcs.htm#15007>`_
                            

Init-keywords
             

None.

Description
           

The class for objects that are images.

Operations
          

The following operation is exported from the *DUIM-DCs* module.

`See image? <dcs.htm#81196>`_

The following operation is exported from the *DUIM-Graphics* module.

` <graphics.htm#64653>`_

See also
        

`See image? <dcs.htm#81196>`_

`See image-depth <dcs.htm#12153>`_

`See image-height <dcs.htm#44679>`_

`See image-width <dcs.htm#32781>`_

`See <ink> <dcs.htm#15007>`_

image?
------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if its argument is an image.

Signature
         

*image?* *object* => *boolean*
                              

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if its argument is an image.

See also
        

`See <image> <dcs.htm#51234>`_

`See image-depth <dcs.htm#12153>`_

`See image-height <dcs.htm#44679>`_

`See image-width <dcs.htm#32781>`_

image-depth
-----------

Generic function
''''''''''''''''

Summary
       

Returns the depth of an image.

Signature
         

*image-depth* *image* => *depth*
                                

Arguments
         

-  *image* An instance of type *<image>*.

Values
      

-  *depth* An instance of type *<real>*.

Description
           

Returns the depth of the image *image*.

See also
        

`See <image> <dcs.htm#51234>`_

`See image? <dcs.htm#81196>`_

`See image-height <dcs.htm#44679>`_

`See image-width <dcs.htm#32781>`_

image-height
------------

Generic function
''''''''''''''''

Summary
       

Returns the height of an image.

Signature
         

*image-height* *image* => *height*
                                  

Arguments
         

-  *image* An instance of type *<image>*.

Values
      

-  *height* An instance of type *<real>*.

Description
           

Returns the height of the image *image*.

See also
        

`See <image> <dcs.htm#51234>`_

`See image? <dcs.htm#81196>`_

`See image-depth <dcs.htm#12153>`_

`See image-width <dcs.htm#32781>`_

image-width
-----------

Generic function
''''''''''''''''

Summary
       

Returns the width of an image.

Signature
         

*image-width* *image* => *width*
                                

Arguments
         

-  *image* An instance of type *<image>*.

Values
      

-  *width* An instance of type *<real>*.

Description
           

Returns the width of the image *image*.

See also
        

`See <image> <dcs.htm#51234>`_

`See image? <dcs.htm#81196>`_

`See image-depth <dcs.htm#12153>`_

`See image-height <dcs.htm#44679>`_

<ink>
-----

Abstract class
''''''''''''''

Summary
       

The class of objects that represent a way of arranging colors and
opacities in the drawing plane.

Superclasses
            

*<object>*
          

Init-keywords
             

None.

Description
           

The class of objects that represent a way of arranging colors and
opacities in the drawing plane. Intuitively, it is anything that can be
drawn with. An ink is anything that can be used in medium-foreground,
medium-background, medium-ink, or the foreground or background of a
brush.

Operations
          

The following operation is exported from the *DUIM-DCs* module.

`See ink? <dcs.htm#59035>`_

See also
        

`See ink? <dcs.htm#59035>`_

ink?
----

Generic function
''''''''''''''''

Summary
       

Returns *#t* if its argument is an ink.

Signature
         

*ink?* *object* => *boolean*
                            

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if *object* is an ink, otherwise returns *#f*.

See also
        

`See <ink> <dcs.htm#15007>`_

$magenta
--------

Constant
''''''''

Summary
       

The usual definition of the color magenta.

Type
    

`See <color> <dcs.htm#55341>`_
                              

Description
           

The usual definition of the color magenta.

See also
        

`See <color> <dcs.htm#55341>`_

make
----

G.f. method
'''''''''''

Summary
       

Returns an object that is of the same type as the class given as its
argument.

Signature
         

*make* *(class* *==* *<pen>)* *#key* *width* *units* *dashes*
*joint-shape* *cap-shape* => *pen*
                                                                                                

*make* *(class* *==* *<brush>)* *#key* *foreground* *background* *mode*
*fill-style* *fill-rule* *tile* *stipple* *ts-x* *ts-y* => *brush*
                                                                                                                                          

Arguments
         

-  *(class==<pen>)* The class `See <pen> <dcs.htm#41757>`_.
-  *width* An instance of type *<pen-width>*. Default value: *1*.
-  *units* An instance of type *<pen-units>*. Default value:
   *#"normal"*.
-  *dashes* An instance of type *<pen-dashes>*. Default value: *#f*.
-  *joint-shape* An instance of type *<pen-joint-shape>*. Default
   value: *#"miter"*.
-  *cap-shape* An instance of type *<pen-cap-shape>*. Default value:
   *#"butt"*.
-  *(class==<brush>)* The class `See <brush> <dcs.htm#29492>`_.
-  *foreground* An instance of type `See <ink> <dcs.htm#15007>`_.
   Default value: *$foreground*.
-  *background* An instance of type `See <ink> <dcs.htm#15007>`_.
   Default value: *$background*.
-  *mode* An instance of type *<integer>*. Default value: *$boole-1*.
-  *fill-style* A *(fill-style)* or *#f*.** Default value: *#f*.
-  *fill-rule* A *(fill-rule)* or *#f*.** Default value: *#f*.
-  *tile* An *(image)* or *#f*.** Default value: *#f*.
-  *stipple* A *(stipple)* or *#f*.** Default value: *#f*.
-  *ts-x* An instance of *false-or(<integer>).* Default value: *#f*.
-  *ts-y* An instance of *false-or(<integer>).* Default value: *#f*.

Values
      

-  *pen* An instance of type `See <pen> <dcs.htm#41757>`_.
-  *brush* An instance of type `See <brush> <dcs.htm#29492>`_.

Description
           

Returns an object that is of the same type as the class given as its
argument. Default values for the keywords that specify object are
provided, or the keywords can be given explicitly to override the
defaults.

See also
        

`See <brush> <dcs.htm#29492>`_

`See <pen> <dcs.htm#41757>`_

make-color-for-contrasting-color
--------------------------------

Generic function
''''''''''''''''

Summary
       

Returns a color that is recognizably different from the main color.

Signature
         

*make-color-for-contrasting-color* *ink* => *color*
                                                   

Arguments
         

-  *ink* An instance of type `See <ink> <dcs.htm#15007>`_.

Values
      

-  *color* An instance of type `See <color> <dcs.htm#55341>`_.

Description
           

Returns a color that is recognizably different from the main color.

See also
        

`See make-contrasting-colors <dcs.htm#31494>`_

make-contrasting-colors
-----------------------

Function
''''''''

Summary
       

Returns a vector of colors with recognizably different appearance.

Signature
         

*make-contrasting-colors* *n* #key *k* => *colors*
                                                  

Arguments
         

-  *n* An instance of type *<integer>*.
-  *k* An instance of type *<integer>*.

Values
      

-  *colors* An instance of type *limited(<sequence>, of:* `See
   <color> <dcs.htm#55341>`_*)*.

Description
           

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
        

`See contrasting-colors-limit <dcs.htm#43277>`_

`See $green <dcs.htm#23573>`_

`See make-color-for-contrasting-color <dcs.htm#44647>`_

`See make-contrasting-dash-patterns <dcs.htm#23504>`_

make-contrasting-dash-patterns
------------------------------

Function
''''''''

Summary
       

Returns a vector of dash patterns with recognizably different
appearances.

Signature
         

*make-contrasting-dash-patterns* *n* *#key* *k* => *dashes*
                                                           

Arguments
         

-  *n* An instance of type *<integer>*.
-  *k* An instance of type *<integer>*.

Values
      

-  *dashes* An instance of type *<vector>*.

Description
           

Returns a vector of *n* dash patterns with recognizably different
appearances. If the keyword *k* is supplied,
*make-contrasting-dash-patterns* returns the *k* th pattern. If there
are not n different dash patterns, an error is signalled.

The argument *n* represents the number of dash patterns.

The argument *k* represents the index in the vector of dash patterns
indicating the pattern to use.

See also
        

`See contrasting-dash-patterns-limit <dcs.htm#18054>`_

`See make-contrasting-colors <dcs.htm#31494>`_

make-device-font
----------------

Function
''''''''

Summary
       

Returns a device-specific font.

Signature
         

*make-device-font* *port* *font* => *device-font*
                                                 

Arguments
         

-  *port* An instance of type ` <silica.htm#11606>`_.
-  *font* An instance of type *<object>*.

Values
      

-  *device-font* A font object or the name of a font.

Description
           

Returns a device-specific font. Text styles are mapped to fonts for a
port, a character set, and a text style. All ports must implement
methods for the generic functions, for all classes of text style.

The objects used to represent a font mapping are unspecified and are
likely to vary from port to port. For instance, a mapping might be some
sort of font object on one type of port, or might simply be the name of
a font on another.

Part of initializing a port is to define the mappings between text
styles and font names for the port's host window system.

make-gray-color
---------------

Function
''''''''

Summary
       

Returns a member of class *<color>*.

Signature
         

*make-gray-color* *luminosity* #key *opacity* => *color*
                                                        

Arguments
         

-  *luminosity* An instance of type *limited(<real>, min: 0, max: 1)*.
-  *opacity* An instance of type *limited(<real>, min: 0, max: 1)*.
   Default value: *1.0*.

Values
      

-  *color* An instance of type `See <color> <dcs.htm#55341>`_.

Description
           

Returns a member of class *<color>*. The *luminance* is a real number
between *0* and *1* (inclusive). On a black-on-white display device, *0*
means black, *1* means white, and the values in between are shades of
gray. On a white-on-black display device, *0* means white, *1* means
black, and the values in between are shades of gray.

See also
        

`See make-ihs-color <dcs.htm#15839>`_

`See make-rgb-color <dcs.htm#73122>`_

make-ihs-color
--------------

Function
''''''''

Summary
       

Returns a member of the class *<color>*.

Signature
         

*make-ihs-color* *intensity* *hue* *saturation* #key *opacity* =>
*color*
                                                                         

Arguments
         

-  *intensity* An instance of type *limited(<real>, min: 0, max:
   sqrt(3))*.
-  *hue* An instance of type *limited(<real>, min: 0, max: 1)*.
-  *saturation* An instance of type *limited(<real>, min: 0, max: 1)*.
-  *opacity* An instance of type *limited(<real>, min: 0, max: 1)*.
   Default value: *1.0*.

Values
      

-  *color* An instance of type `See <color> <dcs.htm#55341>`_.

Description
           

Returns a member of class *<color>*. The *intensity* argument is a real
number between *0* and sqrt(*3*) (inclusive). The *hue* and
*saturation* arguments are real numbers between 0 and 1 (inclusive).

See also
        

`See make-gray-color <dcs.htm#14505>`_

`See make-rgb-color <dcs.htm#73122>`_

make-palette
------------

Generic function
''''''''''''''''

Summary
       

Returns a member of the class *<palette>*.

Signature
         

*make-palette* *port* *#key* => *palette*
                                         

Arguments
         

-  *port* An instance of type ` <silica.htm#11606>`_.

Values
      

-  *palette* An instance of type `See <palette> <dcs.htm#11494>`_.

Description
           

Returns a member of the class `See <palette> <dcs.htm#11494>`_.

make-pattern
------------

Function
''''''''

Summary
       

Returns a pattern generated from a two-dimensional array.

Signature
         

*make-pattern* *array* *colors* => *pattern*
                                            

Arguments
         

-  *array* An instance of type *<array>*.
-  *colors* An instance of type *limited(<sequence>, of:* `See
   <color> <dcs.htm#55341>`_*)*.

Values
      

-  *pattern* An instance of type *<pattern>*.

Description
           

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

Each cell of a pattern occupies a 1 by 1 square. You can use `See
transform-region <geom.htm#33126>`_ to scale the pattern to a different
cell size and shape, or to rotate the pattern so that the rectangular
cells become diamond-shaped. Applying a coordinate transformation to a
pattern does not affect the designs that make up the pattern. It only
changes the position, size, and shape of the cells' holes, allowing
different portions of the designs in the cells to show through.
Consequently, applying *make-rectangular-tile* to a pattern of
nonuniform designs can produce a different appearance in each tile. The
pattern cells' holes are tiled, but the designs in the cells are not
tiled and a different portion of each of those designs shows through in
each tile.

make-rgb-color
--------------

Function
''''''''

Summary
       

Returns a member of class *<color>*.

Signature
         

*make-rgb-color* *red* *green* *blue* #key *opacity* => *color*
                                                               

Arguments
         

-  *red* An instance of type *limited(<real>, min: 0, max: 1)*.
-  *green* An instance of type *limited(<real>, min: 0, max: 1)*.
-  *blue* An instance of type *limited(<real>, min: 0, max: 1)*.
-  *opacity* An instance of type *limited(<real>, min: 0, max: 1)*.
   Default value: *1.0*.

Values
      

-  *color* An instance of type `See <color> <dcs.htm#55341>`_.

Description
           

Returns a member of class *<color>*. The *red*, *green*, and*blue*
arguments are real numbers between 0 and 1 (inclusive) that specify the
values of the corresponding color components.

When all three color components are 1, the resulting color is white.
When all three color components are 0, the resulting color is black.

See also
        

`See make-gray-color <dcs.htm#14505>`_

`See make-ihs-color <dcs.htm#15839>`_

make-stencil
------------

Function
''''''''

Summary
       

Returns a pattern design generated from a two-dimensional array.

Signature
         

*make-stencil* *array* => *stencil*
                                   

Arguments
         

-  *array* An instance of type *<array>*.

Values
      

-  *stencil* An instance of type *<stencil>*.

Description
           

Returns a pattern design that has (*array-dimension* *array* *0*) cells
in the vertical direction and (*array-dimension* *array* *1*) cells in
the horizontal direction. *array* must be a two-dimensional array of
real numbers between 0 and 1 (inclusive) that represent opacities. The
design in cell *i,j* of the resulting pattern is the value of
*(make-opacity (aref* *array* *i j))*.

make-text-style
---------------

Function
''''''''

Summary
       

Returns an instance of *<text-style>*.

Signature
         

*make-text-style* *family* *weight* *slant* *size* #key *underline?*
*strikeout?* => *text-style*
                                                                                                 

Arguments
         

-  *family* An instance of type *one-of(#"fix", #"serif", #"sans-serif",
   #f)*.
-  *weight* An instance of type *one-of(#"normal", #"condensed",
   #"thin", #"extra-light", #"light", #"medium", #"demibold", #"bold",
   #"extra-bold", #"black", #f)*.
-  *slant* An instance of type *one-of(#"roman", #"italic", #"oblique",
   #f)*.
-  *size* An instance of *<integer>*, or an instance of type
   *one-of(#"normal", #"tiny", #"very-small", #"small", #"large",
   #"very-large:", #"huge", #"smaller", #"larger", #f)*.
-  *underline?* An instance of type *<boolean>*.
-  *strikeout?* An instance of type *<boolean>*.

Values
      

-  *text-style* An instance of type *<text-style>*.

Description
           

Returns an instance of *<text-style>*.

Text style objects have components for family, face, and size. Not all
of these attributes need be supplied for a given text style object. Text
styles can be merged in much the same way as pathnames are merged;
unspecified components in the style object (that is, components that
have *#f* in them) may be filled in by the components of a default style
object. A text style object is called *fully specified* if none of its
components is *#f*, and the size component is not a relative size (that
is, neither *#"smaller"* nor *#"larger"*).

If *size* is an integer, it represents the size of the font in printer’s
points.

Implementations are permitted to extend legal values for family, face,
and size.

See also
        

`See $solid-pen <dcs.htm#57039>`_.

merge-text-styles
-----------------

Generic function
''''''''''''''''

Summary
       

Merges two text styles and returns a new text style that is the same as
the first, except that unspecified components in are filled in from the
second.

Signature
         

*merge-text-styles* *text-style* *default-style* => *text-style*
                                                                

Arguments
         

-  *text-style* An instance of type *<text-style>*.
-  *default-style* An instance of type *<text-style>*.

Values
      

-  *text-style* An instance of type *<text-style>*.

Description
           

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
        

`See default-background-setter <dcs.htm#32743>`_.

<palette>
---------

Abstract instantiable class
'''''''''''''''''''''''''''

Summary
       

The protocol class for color palettes.

Superclasses
            

*<object>*
          

Init-keywords
             

None.

Description
           

The protocol class for color palettes.

Operations
          

` <silica.htm#25428>`_ ` <silica.htm#39992>`_ ` <frames.htm#29202>`_
` <frames.htm#56600>`_ ` <silica.htm#84661>`_

See also
        

`See palette? <dcs.htm#12981>`_

palette?
--------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if an object is a palette.

Signature
         

*palette?* *object* => *boolean*
                                

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if the object *object* is a palette. A palette is a color
map that maps 16 bit colors into a, for example, 8 bit display.

See also
        

`See <palette> <dcs.htm#11494>`_

<palette-full>
--------------

Sealed concrete class
'''''''''''''''''''''

Summary
       

The class for errors that are signalled when a color palette is full.

Superclasses
            

*<error>*
         

Init-keywords
             

-  *palette:*

Description
           

The class for errors that are signalled when a color palette is full.

See also
        

`See <palette> <dcs.htm#11494>`_

$parquet-stipple
----------------

Constant
''''''''

Summary
       

A stipple pattern for use in creating a patterned brush that looks like
a parquet floor.

Type
    

*<array>*
         

Description
           

A stipple pattern for use in creating a patterned brush that looks like
a parquet floor.

See also
        

`See brush-stipple <dcs.htm#15280>`_.

<pattern>
---------

Sealed concrete class
'''''''''''''''''''''

Summary
       

The class for patterns.

Superclasses
            

*<stencil>*
           

Init-keywords
             

-  *colors:* An instance of type *limited(<sequence> of: `See
   <color> <dcs.htm#55341>`_)*.

Description
           

The class for patterns. A pattern is a bounded rectangular arrangement
of color, like a checkerboard. Drawing a pattern draws a different
design in each rectangular cell of the pattern.

Operations
          

The following operation is exported from the *DUIM-DCs* module.

-  `See pattern? <dcs.htm#58212>`_

See also
        

`See <stencil> <dcs.htm#33266>`_

`See make-pattern <dcs.htm#40408>`_

pattern?
--------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if its argument is a pattern.

Signature
         

*pattern?* *object* => *boolean*
                                

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if *object* is a pattern.

See also
        

`See make-pattern <dcs.htm#40408>`_

<pen>
-----

Abstract instantiable class
'''''''''''''''''''''''''''

Summary
       

The protocol class for pens.

Superclasses
            

*<object>*
          

Init-keywords
             

-  *width:* An instance of type *<integer>*. Default value: *1*.
-  *units:* An instance of type *one-of(#"normal", #"point", #"device")*
   . Default value: *#"normal"*.
-  *dashes:* An instance of type *union(<boolean>, <sequence>)*.
   Default value: *#f*.
-  *joint-shape:* An instance of type *one-of(#"miter", #"bevel",
   #"round", #"none")*. Default value: *#"miter"*.
-  *cap-shape:* An instance of type *one-of(#"butt", #"square",
   #"round", #"no-end-point")*. Default value: *#"butt"*.

Description
           

The protocol class for pens. A pen imparts ink to a medium.

Operations
          

The following operations are exported from the *DUIM-DCs* module.

`See = <dcs.htm#33460>`_ `See pen? <dcs.htm#89472>`_ `See
pen-cap-shape <dcs.htm#61017>`_ `See pen-dashes <dcs.htm#37198>`_ `See
pen-joint-shape <dcs.htm#78125>`_ `See pen-units <dcs.htm#10169>`_ `See
pen-width <dcs.htm#16848>`_

See also
        

`See <ink> <dcs.htm#15007>`_

`See make <dcs.htm#81960>`_

`See pen? <dcs.htm#89472>`_

`See pen-cap-shape <dcs.htm#61017>`_

`See pen-dashes <dcs.htm#37198>`_

`See pen-joint-shape <dcs.htm#78125>`_

`See pen-units <dcs.htm#10169>`_

`See pen-width <dcs.htm#16848>`_

pen?
----

Generic function
''''''''''''''''

Summary
       

Returns *#t* if its argument is a pen.

Signature
         

*pen?* *object* => *boolean*
                            

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if *object* is a pen, otherwise returns *#f*.

See also
        

`See <pen> <dcs.htm#41757>`_

`See pen-cap-shape <dcs.htm#61017>`_

`See pen-dashes <dcs.htm#37198>`_

`See pen-joint-shape <dcs.htm#78125>`_

`See pen-units <dcs.htm#10169>`_

`See pen-width <dcs.htm#16848>`_

pen-cap-shape
-------------

Generic function
''''''''''''''''

Summary
       

Returns the shape of the end of a line or an arc drawn by the pen.

Signature
         

*pen-cap-shape* *pen* => *value*
                                

Arguments
         

-  *pen* An instance of type `See <pen> <dcs.htm#41757>`_.

Values
      

-  *value* An instance of type *one-of(#"butt", #"square", #"round",
   #"no-end-point")*.

Description
           

Returns the shape of the end of a line or an arc drawn by *pen*.

See also
        

`See make-contrasting-dash-patterns <dcs.htm#23504>`_

`See <pen> <dcs.htm#41757>`_

`See pen? <dcs.htm#89472>`_

`See pen-dashes <dcs.htm#37198>`_

`See pen-joint-shape <dcs.htm#78125>`_

`See pen-units <dcs.htm#10169>`_

`See pen-width <dcs.htm#16848>`_

pen-dashes
----------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if the lines drawn by a pen are dashed.

Signature
         

*pen-dashes* *pen* => *value*
                             

Arguments
         

-  *pen* An instance of type `See <pen> <dcs.htm#41757>`_.

Values
      

-  *value* An instance of type *type-union(<boolean>, <sequence>)*.

Description
           

Returns *#t* if the lines drawn by *pen* are dashed. The sequence is a
vector of integers indicating the pattern of dashes. There must be an
even number of integers. The odd elements in the list indicate the
length of the inked dashes and the even elements indicate the length of
the gaps between dashes.

See also
        

`See <pen> <dcs.htm#41757>`_

`See pen? <dcs.htm#89472>`_

`See pen-cap-shape <dcs.htm#61017>`_

`See pen-joint-shape <dcs.htm#78125>`_

`See pen-units <dcs.htm#10169>`_

`See pen-width <dcs.htm#16848>`_

pen-joint-shape
---------------

Generic function
''''''''''''''''

Summary
       

Returns the shape of the joints between line segments of a closed,
unfilled figure.

Signature
         

*pen-joint-shape* *pen* => *value*
                                  

Arguments
         

-  *pen* An instance of type `See <pen> <dcs.htm#41757>`_.

Values
      

-  *value* An instance of type *one-of(#"miter", #"bevel", #"round",
   #"none")*.

Description
           

Returns the shape of the joints between line segments of a closed,
unfilled figure drawn by *pen*.

See also
        

`See make-contrasting-dash-patterns <dcs.htm#23504>`_

`See <pen> <dcs.htm#41757>`_

`See pen? <dcs.htm#89472>`_

`See pen-cap-shape <dcs.htm#61017>`_

`See pen-dashes <dcs.htm#37198>`_

`See pen-units <dcs.htm#10169>`_

`See pen-width <dcs.htm#16848>`_

pen-units
---------

Generic function
''''''''''''''''

Summary
       

Returns the units in which the pen width is specified.

Signature
         

*pen-units* *pen* => *value*
                            

Arguments
         

-  *pen* An instance of type `See <pen> <dcs.htm#41757>`_.

Values
      

-  *value* An instance of type *one-of(#"normal", #"point", #"device")*
   .

Description
           

Returns the units in which the pen width is specified. They may be
normal, points, or device-dependent. A width of *#"normal"* is a
comfortably visible thin line.

See also
        

`See make-contrasting-dash-patterns <dcs.htm#84915>`_

`See <pen> <dcs.htm#41757>`_

`See pen? <dcs.htm#89472>`_

`See pen-cap-shape <dcs.htm#61017>`_

`See pen-dashes <dcs.htm#37198>`_

`See pen-joint-shape <dcs.htm#78125>`_

`See pen-width <dcs.htm#16848>`_

pen-width
---------

Generic function
''''''''''''''''

Summary
       

Returns the pen-width, that is how wide a stroke the pen draws, of its
argument.

Signature
         

*pen-width* *pen* => *width*
                            

Arguments
         

-  *pen* An instance of type `See <pen> <dcs.htm#41757>`_.

Values
      

-  *width* An instance of type *<pen-width>*. The units that specify
   the width of the pen may be *#"normal"*, *#"points"*, or
   *#"device"*.

Description
           

Returns the pen width, that is how wide a stroke the pen draws, of *pen*
. A width of *#"normal"* is a comfortably visible thin line.

See also
        

`See make-contrasting-dash-patterns <dcs.htm#84915>`_

`See <pen> <dcs.htm#41757>`_

`See pen? <dcs.htm#89472>`_

`See pen-cap-shape <dcs.htm#61017>`_

`See pen-dashes <dcs.htm#37198>`_

`See pen-joint-shape <dcs.htm#78125>`_

`See pen-units <dcs.htm#10169>`_

read-image
----------

Generic function
''''''''''''''''

Summary
       

Reads an image.

Signature
         

*read-image* *resource-id* *#key image-type:* *image-type* *#all-keys*
=> *image*
                                                                                 

Arguments
         

-  *locator* An instance of type *type-union(<string>, <locator>)*.
-  *image-type* On Windows, an instance of type *one-of(#"bitmap",
   #"icon")*.

Values
      

-  *image* An instance of type *<image>*.

Description
           

Reads an image from the location *resource-id*. This function calls
*read-image-as*.

See also
        

`See read-image-as <dcs.htm#12647>`_.

read-image-as
-------------

Generic function
''''''''''''''''

Summary
       

Reads an image.

Signature
         

*read-image-as* *class* *locator* *image-type* #key #all-keys => *image*
                                                                        

Arguments
         

-  *class* An instance of type *<object>*.
-  *locator* An instance of type *<string>*.
-  *image-type* On Windows, *#"bitmap"* or *#"icon"*.

Values
      

-  *image* An instance of type *<image>*.

Description
           

Reads the image in the location pointed to be *locator*, as an instance
of a particular class*.* This function is called by *read-image.*

The *class* represents the class that the image is read as an instance
of.

See also
        

`See read-image <dcs.htm#65313>`_

$red
----

Constant
''''''''

Summary
       

The usual definition of the color red.

Type
    

`See <color> <dcs.htm#55341>`_
                              

Description
           

The usual definition of the color red.

See also
        

See the class `See $blue <dcs.htm#10318>`_.

remove-colors
-------------

Generic function
''''''''''''''''

Summary
       

Removes one or more colors from a palette and returns the updated
palette.

Signature
         

*remove-colors* *palette* *#rest* *colors* => *palette*
                                                       

Arguments
         

-  *palette* An instance of type `See <palette> <dcs.htm#11494>`_.
-  *colors* Instances of type `See <color> <dcs.htm#55341>`_.

Values
      

*palette*

Description
           

Removes *colors* from *palette* and returns the updated palette.

$solid-pen
----------

Constant
''''''''

Summary
       

A pen that draws a solid line.

Type
    

`See <pen> <dcs.htm#41757>`_
                            

Description
           

A pen that draws a solid line. The width of the line is *1*, and
*dashes:* is *#f.*

See also
        

See the class `See <pen> <dcs.htm#41757>`_ and the constants `See
make <dcs.htm#81960>`_, `See $dash-dot-pen <dcs.htm#55453>`_, and `See
$dotted-pen <dcs.htm#76677>`_.

<stencil>
---------

Sealed concrete class
'''''''''''''''''''''

Summary
       

The class for stencils.

Superclasses
            

*<image>*
         

Init-keywords
             

-  *array:* An instance of type *<array>*. Required.
-  *transform:* An instance of type `See <transform> <geom.htm#33417>`_.
   Default value: *#f*.

Description
           

The class for stencils. A *stencil* is a special kind of pattern that
contains only opacities.

Operations
          

The following operations are exported from the *DUIM-DCs* module.

`See image-height <dcs.htm#44679>`_ `See image-width <dcs.htm#32781>`_
`See stencil? <dcs.htm#21344>`_

The following operation is exported from the *DUIM-Geometry* module.

`See box-edges <geom.htm#52858>`_

See also
        

`See <image> <dcs.htm#51234>`_

`See make-pattern <dcs.htm#32293>`_

`See stencil? <dcs.htm#21344>`_

stencil?
--------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if its argument is a stencil.

Signature
         

stencil? *object* => *boolean*
                              

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if its argument is a stencil.

See also
        

`See make-pattern <dcs.htm#32293>`_.

`See <stencil> <dcs.htm#33266>`_

<text-style>
------------

Abstract instantiable class
'''''''''''''''''''''''''''

Summary
       

The protocol class for text styles.

Superclasses
            

*<object>*
          

Init-keywords
             

-  *family:* An instance of type *one-of(#"fix", #"serif",
   #"sans-serif", #f)*. Default value: *#f*.
-  *weight:* An instance of type *one-of(#"normal", #"condensed",
   #"thin", #"extra-light", #"light", #"medium", #"demibold", #"bold",
   #"extra-bold", #"black", #f)*.
-  *slant:* An instance of type *one-of(#"roman", #"italic", #"oblique",
   #f)*.
-  *size:* An instance of *<integer>*, or an instance of type
   *one-of(#"normal", #"tiny", #"very-small", #"small", #"large",
   #"very-large:", #"huge", #"smaller", #"larger", #f)*. Default value:
   *#f*.
-  *underline?:* An instance of type *<boolean>*. Default value: *#f*.
-  *strikeout?:* An instance of type *<boolean>*. Default value: *#f*.

Description
           

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

Operations
          

The following operations are exported from the *DUIM-DCs* module.

`See = <dcs.htm#33460>`_ `See fully-merged-text-style? <dcs.htm#67988>`_
`See merge-text-styles <dcs.htm#14653>`_ `See
text-style? <dcs.htm#15159>`_ `See
text-style-components <dcs.htm#90000>`_ `See
text-style-family <dcs.htm#33560>`_ `See
text-style-size <dcs.htm#84983>`_ `See
text-style-slant <dcs.htm#93572>`_ `See
text-style-strikeout? <dcs.htm#10272>`_ `See
text-style-underline? <dcs.htm#34228>`_ `See
text-style-weight <dcs.htm#41015>`_

The following operations are exported from the *DUIM-Sheets* module.

` <silica.htm#32535>`_ ` <silica.htm#47453>`_ ` <silica.htm#26061>`_
` <silica.htm#19634>`_ ` <silica.htm#96248>`_ ` <silica.htm#20511>`_
` <silica.htm#39545>`_ ` <silica.htm#66055>`_ ` <silica.htm#79518>`_

See also
        

`See text-style? <dcs.htm#15159>`_

`See text-style-components <dcs.htm#90000>`_

`See text-style-family <dcs.htm#88936>`_

`See text-style-size <dcs.htm#84983>`_

`See text-style-slant <dcs.htm#93572>`_

`See text-style-strikeout? <dcs.htm#10272>`_

`See text-style-underline? <dcs.htm#34228>`_

`See text-style-weight <dcs.htm#41015>`_

text-style?
-----------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if its argument is a text-style.

Signature
         

*text-style?* *object* => *text-style?*
                                       

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *text-style?* An instance of type *<boolean>*.

Description
           

Returns *#t* if its argument is a text-style.

See also
        

`See <text-style> <dcs.htm#85385>`_

`See text-style-components <dcs.htm#90000>`_

`See text-style-family <dcs.htm#88936>`_

`See text-style-size <dcs.htm#84983>`_

`See text-style-slant <dcs.htm#93572>`_

`See text-style-strikeout? <dcs.htm#10272>`_

`See text-style-underline? <dcs.htm#34228>`_

`See text-style-weight <dcs.htm#41015>`_

text-style-components
---------------------

Generic function
''''''''''''''''

Summary
       

Returns the components of a text style as the values family, face,
slant, size, underline and strikeout.

Signature
         

*text-style-components* *text-style* => *family* *weight* *slant* *size*
*underline?* *strikeout?*
                                                                                                  

Arguments
         

-  *text-style* An instance of type *<text-style>*.

Values
      

-  *family* An instance of type *one-of(#"fix", #"serif", #"sans-serif",
   #f)*.
-  *weight* An instance of type *one-of(#"normal", #"condensed",
   #"thin", #"extra-light", #"light", #"medium", #"demibold", #"bold",
   #"extra-bold", #"black", #f)*.
-  *slant* An instance of type *one-of(#"roman", #"italic", #"oblique",
   #f)*.
-  *size* An instance of *<integer>*, or an instance of type
   *one-of(#"normal", #"tiny", #"very-small", #"small", #"large",
   #"very-large:", #"huge", #"smaller", #"larger", #f)*. Default value:
   *#f*.
-  *underline?* An instance of type *<boolean>*.
-  *strikeout?* An instance of type *<boolean>*.

Description
           

Returns the components of the text style *text-style* as the values
family, face, slant, size, underline and strikeout.

See also
        

`See <text-style> <dcs.htm#85385>`_

`See text-style? <dcs.htm#15159>`_

`See text-style-family <dcs.htm#88936>`_

`See text-style-size <dcs.htm#84983>`_

`See text-style-slant <dcs.htm#93572>`_

`See text-style-strikeout? <dcs.htm#10272>`_

`See text-style-underline? <dcs.htm#34228>`_

`See text-style-weight <dcs.htm#41015>`_

text-style-family
-----------------

Generic function
''''''''''''''''

Summary
       

Returns the family component of the specified text style.

Signature
         

*text-style-family* *text-style* => *family*
                                            

Arguments
         

-  *text-style* An instance of type *<text-style>*.

Values
      

-  *family* An instance of type *one-of(#"fix", #"serif", #"sans-serif",
   #f)*.

Description
           

Returns the family component of the specified text style.

See also
        

`See <text-style> <dcs.htm#85385>`_

`See text-style? <dcs.htm#15159>`_

`See text-style-components <dcs.htm#90000>`_

`See text-style-size <dcs.htm#84983>`_

`See text-style-slant <dcs.htm#93572>`_

`See text-style-strikeout? <dcs.htm#10272>`_

`See text-style-underline? <dcs.htm#34228>`_

`See text-style-weight <dcs.htm#41015>`_

text-style-size
---------------

Generic function
''''''''''''''''

Summary
       

Returns the style component of the specified text style.

Signature
         

*text-style-size* *text-style* => *size*
                                        

Arguments
         

-  *text-style* An instance of type *<text-style>*.

Values
      

-  *size* An instance of *<integer>*, or an instance of type
   *one-of(#"normal", #"tiny", #"very-small", #"small", #"large",
   #"very-large:", #"huge", #"smaller", #"larger", #f)*. Default value:
   *#f*.

Description
           

Returns the style component of the specified text style.

See also
        

`See <text-style> <dcs.htm#85385>`_

`See text-style? <dcs.htm#15159>`_

`See text-style-components <dcs.htm#90000>`_

`See text-style-family <dcs.htm#88936>`_

`See text-style-slant <dcs.htm#93572>`_

`See text-style-strikeout? <dcs.htm#10272>`_

`See text-style-underline? <dcs.htm#34228>`_

`See text-style-weight <dcs.htm#41015>`_

text-style-slant
----------------

Generic function
''''''''''''''''

Summary
       

Returns the slant component of the specified text style.

Signature
         

*text-style-slant* *text-style* => *slant*
                                          

Arguments
         

-  *text-style* An instance of type *<text-style>*.

Values
      

-  *slant* An instance of type *one-of(#"roman", #"italic", #"oblique",
   #f)*.

Description
           

Returns the slant component of the specified text style.

See also
        

`See <text-style> <dcs.htm#85385>`_

`See text-style? <dcs.htm#15159>`_

`See text-style-components <dcs.htm#90000>`_

`See text-style-family <dcs.htm#88936>`_

`See text-style-size <dcs.htm#84983>`_

`See text-style-strikeout? <dcs.htm#10272>`_

`See text-style-underline? <dcs.htm#34228>`_

`See text-style-weight <dcs.htm#41015>`_

text-style-strikeout?
---------------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if the text style includes a line through it, striking it
out.

Signature
         

*text-style-strikeout?* *text-style* => *strikeout?*
                                                    

Arguments
         

-  *text-style* An instance of type *<text-style>*.

Values
      

-  *strikeout?* An instance of type *<boolean>*.

Description
           

Returns *#t* if the text style includes a line through it, striking it
out.

See also
        

`See <text-style> <dcs.htm#85385>`_

`See text-style? <dcs.htm#15159>`_

`See text-style-components <dcs.htm#90000>`_

`See text-style-family <dcs.htm#88936>`_

`See text-style-size <dcs.htm#84983>`_

`See text-style-slant <dcs.htm#93572>`_

`See text-style-underline? <dcs.htm#34228>`_

`See text-style-weight <dcs.htm#41015>`_

text-style-underline?
---------------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if the text style is underlined.

Signature
         

*text-style-underline?* *text-style* => *underline?*
                                                    

Arguments
         

-  *text-style* An instance of type *<text-style>*.

Values
      

-  *underline?* An instance of type *<boolean>*.

Description
           

Returns *#t* if the text style is underlined.

See also
        

`See <text-style> <dcs.htm#85385>`_

`See text-style? <dcs.htm#15159>`_

`See text-style-components <dcs.htm#90000>`_

`See text-style-family <dcs.htm#88936>`_

`See text-style-size <dcs.htm#84983>`_

`See text-style-slant <dcs.htm#93572>`_

`See text-style-strikeout? <dcs.htm#10272>`_

`See text-style-weight <dcs.htm#41015>`_

text-style-weight
-----------------

Generic function
''''''''''''''''

Summary
       

Returns the weight component of the specified text style.

Signature
         

*text-style-weight* *text-style* => *weight*
                                            

Arguments
         

-  *text-style* An instance of type *<text-style>*.

Values
      

-  *weight* An instance of type *one-of(#"normal", #"condensed",
   #"thin", #"extra-light", #"light", #"medium", #"demibold", #"bold",
   #"extra-bold", #"black", #f)*.

Description
           

Returns the weight component of the text style.

See also
        

`See <text-style> <dcs.htm#85385>`_

`See text-style? <dcs.htm#15159>`_

`See text-style-components <dcs.htm#90000>`_

`See text-style-family <dcs.htm#88936>`_

`See text-style-size <dcs.htm#84983>`_

`See text-style-slant <dcs.htm#93572>`_

`See text-style-strikeout? <dcs.htm#10272>`_

`See text-style-underline? <dcs.htm#34228>`_

$tiles-stipple
--------------

Constant
''''''''

Summary
       

A stipple pattern for use in creating a patterned brush with lines and
spaces suggesting tiles

Type
    

*<array>*
         

Description
           

A stipple pattern for use in creating a patterned brush with lines and
spaces suggesting tiles

See also
        

`See brush-stipple <dcs.htm#15280>`_.

$vertical-hatch
---------------

Constant
''''''''

Summary
       

A stipple pattern for use in creating a patterned brush with alternating
vertical columns of lines and spaces.

Type
    

*<array>*
         

Description
           

A stipple pattern for use in creating a patterned brush with alternating
vertical columns of lines and spaces.

See also
        

`See brush-stipple <dcs.htm#15280>`_.

$white
------

Constant
''''''''

Summary
       

The usual definition of white.

Type
    

`See <color> <dcs.htm#55341>`_
                              

Description
           

The usual definition of white. In the *rgb* color model, its value is
*111*.

See also
        

`See <color> <dcs.htm#55341>`_

write-image
-----------

Generic function
''''''''''''''''

Summary
       

Writes out a copy of an image to disk (or other designated medium).

Signature
         

*write-image* *image* *locator* => ()
                                     

Arguments
         

-  *image* An instance of type *<image>*.
-  *locator* An instance of type *<string>*.

Values
      

None

Description
           

Writes out a copy of *image* to the designated medium *locator*.

$xor-brush
----------

Constant
''''''''

Summary
       

A standard brush with the drawing property of *$boole-xor*.

Type
    

`See <brush> <dcs.htm#29492>`_
                              

Description
           

A standard brush with the drawing property of *$boole-xor*.

$yellow
-------

Constant
''''''''

Summary
       

The usual definition of the color yellow.

Type
    

`See <color> <dcs.htm#55341>`_
                              

Description
           

The usual definition of the color yellow.

See also
        

`See <color> <dcs.htm#55341>`_


