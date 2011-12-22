*********************
DUIM-Geometry Library
*********************

Overview
========

The DUIM-Geometry library provides basic support for coordinate
geometry. This allows the position of elements in a window object to be
determined correctly. The library contains a single module,
*duim-geometry*, from which all the interfaces described in this
chapter are exposed. `DUIM-Geometry Module`_
contains complete reference entries for each exposed interface.

The class hierarchy for DUIM-Geometry
=====================================

The base classes for classes in the DUIM-Geometry library are ``<region>``
and ``<transform>``, both of which are subclasses of ``<object>``. While
the ``<region>`` class has a number of subclasses, ``<transform>`` has no
direct subclasses.

-  ``<transform>`` The superclass of all transforms. A transform describes
   the mapping of one set of points onto another. There are one or more
   subclasses of `<transform>`_ that implement
   transforms. These subclasses have implementation-dependent names
   which are explicitly unspecified. All of the instantiable
   transformation classes provided by DUIM are immutable.

In addition, there are a number of error classes which may be signalled.
These are all subclasses of ``<error>``.

The <region> class and its subclasses
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The DUIM-Geometry library exposes the <region> class and its subclasses
as shown in Table `The <region> class and its
subclasses`_. None of these subclasses have any further
subclasses exposed in the DUIM-Geometry library, although the
DUIM-Extended-Geometry library exposes some subclasses of *<area>* and
*<path>*.

The <region> class and its subclasses
                                     

.. figure:: images/geom-2.png
   :align: center
   :alt: 
<region>

<region-set>

<point>

<path>

<area>

<bounding-box>

-  *<region>* This class is used to represent any set of points.
   The*<region>* class includes both bounded regions (that is, regions
   whose edges are known) and unbounded regions (that is, regions with
   no known edges).
-  *<region-set>* This class represents a region set, that is, a set of
   regions.
-  *<point>* This class is used to represent mathematical points (that
   is, regions with dimensionality 0).
-  *<path>* The class *<path>* denotes bounded regions with a length,
   but no area (that is, they have dimensionality 1).
-  *<area>* This class denotes bounded regions that have an area (that
   is, they have dimensionality 2).
-  *<bounding-box>* A bounding box is an axis aligned rectangle that
   contains some region.

Error classes provided by DUIM-Geometry
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The DUIM-Geometry library exposes a number of errors that can be
signalled in certain circumstances. They are shown in Table `The
<transform-error> class and its subclasses`_. All the
errors shown are subclasses of the *<error>* class. Note that the
subclasses of *<transform-error>* are all specific to particular errors.

The <transform-error> class and its subclasses
                                              

.. figure:: images/geom-2.png
   :align: center
   :alt: 
<transform-error>

<transform-underspecified>

<reflection-<underspecified>

<singular-transform>

*<transform-error>*
                   

The superclass of all error conditions signalled when there is an error
with a transform.
                                                                                         

*<transform-underspecified>*
                            

The error that is signalled when *make-3-point-transform* is given three
colinear image points.
                                                                                               

*<reflection-<underspecified>*
                              

The error that is signalled when *make-reflection-transform* is given
two coincident points.
                                                                                            

*<singular-transform>*
                      

The error that is signalled when *invert-transform* is called on a
singular transform, that is, a transform that has no inverse.
                                                                                                                                

DUIM-Geometry Module
====================

This section contains a complete reference of all the interfaces that
are exported from the *duim-geometry* module.

=
~

G.f. method
'''''''''''

Summary
       

Tests if its arguments are equal*.*

Signature
         

= *region1 region2* => *boolean*

= *transform1 transform2* => *boolean*

Arguments
         

-  *region1* An instance of type *<region>*.
-  *region2* An instance of type *<region>*.
-  *transform1* An instance of type `<transform>`_.
-  *transform2* An instance of type `<transform>`_.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Tests if its arguments are equal*.* Returns *#t* if the two regions or
transforms are the same, otherwise returns *#f*. Two regions are
considered equal if they contain exactly the same set of points. Two
transforms are considered equal if they transform every region the same
way.

<area>
------

Open abstract class
'''''''''''''''''''

Summary
       

The class *<area>* denotes bounded regions that have dimensionality 2
(that is, have area).

Superclasses
            

*<region>*

Init-keywords
             

None.

Description
           

The class *<area>* denotes bounded regions that have dimensionality 2
(that is, have area).*<area>* is a subclass of *<region>*.

Note that constructing an area object with no area (such as calling
*make-rectangle* with two coincident points, for example) may
canonicalize it to *$nowhere*.

Operations
          

The following operation is exported from the *DUIM-Geometry* module.

-  `area?`_

See also
        

`area?`_

area?
-----

Generic function
''''''''''''''''

Summary
       

Returns *#t* if its argument** is an area, otherwise returns *#f*.

Signature
         

*area?* *object* => *boolean*

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if *object* is an area, otherwise returns *#f*

See also
        

`<area>`_

<bounding-box>
--------------

Open abstract instantiable class
''''''''''''''''''''''''''''''''

Summary
       

The class that represents a bounding box.

Superclasses
            

*<region>*

Init-keywords
             

-  *left:* An instance of type *<integer>*.
-  *top:* An instance of type *<integer>*.
-  *right:* An instance of type *<integer>*.
-  *bottom:* An instance of type *<integer>*.

Description
           

A bounding box is an axis aligned rectangle that contains some region.
The representation of bounding boxes in DUIM is chosen to be efficient.
This representation is not sufficient to represent the result of
arbitrary transformations (such as rotations) of bounding boxes. The
most general class of transformations that is guaranteed to transform a
box into another box is the class of transformations that satisfy
*rectilinear-transformation?*.

Bounding boxes are immutable, but since they reflect the live state of
such mutable objects as sheets, bounding boxes are volatile. Therefore,
programmers must not depend on the bounding box associated with a
mutable object remaining constant.

Operations
          

The following operations are exported from the *DUIM-Geometry* module.

`bounding-box?`_ `box-edges`_
`region-contains-position?`_ `See
region-contains-region?`_ `See
region-difference`_ `See
region-empty?`_ `See
region-intersection`_ `See
region-intersects-region?`_ `See
region-union`_ `set-box-edges`_
`set-box-position`_ `See
set-box-size`_ `transform-region`_
`untransform-region`_

See also
        

`bounding-box?`_

`bounding-box`_

`box-edges`_

bounding-box?
-------------

Generic function
''''''''''''''''

Summary
       

Returns true** if its argument is a bounding box*.*

Signature
         

*bounding-box?* *object* => *boolean*

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if *object* is a bounding box (that is, supports the
bounding box protocol), otherwise returns *#f.*

See also
        

`<bounding-box>`_

`bounding-box`_

`box-edges`_

bounding-box
------------

Generic function
''''''''''''''''

Summary
       

Returns the bounding box of a region.

Signature
         

*bounding-box* *region* *#key* *into* => *box*

Arguments
         

-  *region* An instance of type *<region>*.
-  *into* An instance of type *false-or(`See
   <bounding-box>`_)*.

Values
      

-  *box* An instance of type *`<bounding-box>`_*.

Description
           

The argument *region* must be either a bounded region (such as a line or
an ellipse) or some other object that obeys the bounding box protocol,
such as a sheet.

This function often returns an existing object, so you should not modify
the returned result.

If *into* is supplied, it is a bounding box that might be destructively
modified to contain the result.

See also
        

`<bounding-box>`_

`bounding-box?`_

`box-edges`_

box-bottom
----------

Function
''''''''

Summary
       

Returns the *y* coordinate of the bottom right corner of the bounding
box of a region.

Signature
         

*box-bottom* *region* => *bottom*

Arguments
         

-  *region* An instance of type *<region>*.

Values
      

-  *bottom* An instance of type *<integer>*.

Description
           

Returns the *y* coordinate of the bottom right corner of the bounding
box of *region*. The argument *region* must be either a bounded region
or some other object that obeys the bounding box protocol.

See also
        

`box-left`_

`box-right`_

`box-top`_

box-edges
---------

Generic function
''''''''''''''''

Summary
       

Returns the bounding box of a region.

Signature
         

*box-edges* *region* => *left* *top* *right* *bottom*

Arguments
         

-  *region* An instance of type *<region>*.

Values
      

-  *left* An instance of type *<integer>*.
-  *top* An instance of type *<integer>*.
-  *right* An instance of type *<integer>*.
-  *bottom* An instance of type *<integer>*.

Description
           

Returns the bounding box of *region* as four integers specifying the *x*
and *y* coordinates of the top left point and the *x* and *y*
coordinates of the bottom right point of the box

The argument *region* must be either a bounded region (such as a line or
an ellipse) or some other object that obeys the bounding box protocol,
such as a sheet.

The four returned values *left, top, right*, and *bottom* will satisfy
the inequalities

*left* <= *right*
                 

*top* <= *bottom*
                 

See also
        

`<bounding-box>`_

`bounding-box?`_

`bounding-box`_

box-height
----------

Function
''''''''

Summary
       

Returns the height of the bounding box of a region.

Signature
         

*box-height* *region* => *height*

Arguments
         

-  *region* An instance of type *<region>*.

Values
      

-  *height* An instance of type *<integer>*.

Description
           

Returns the height of the bounding box *region*. The height of a
bounding box is the difference between the maximum *y* coordinate and
its minimum *y* coordinate. The argument *region* must be either a
bounded region or some other object that obeys the bounding box
protocol.

See also
        

`box-position`_

`box-size`_

`box-width`_

box-left
--------

Function
''''''''

Summary
       

Returns the *x* coordinate of the upper left corner of the bounding box
of a region.

Signature
         

*box-left* *region* => *left*

Arguments
         

-  *region* An instance of type *<region>*.

Values
      

-  *left* An instance of type *<integer>*.

Description
           

Returns the *x* coordinate of the upper left corner of the bounding box
*region*. The argument *region* must be either a bounded region or some
other object that obeys the bounding box protocol, such as a sheet.

See also
        

`box-bottom`_

`box-right`_

`box-top`_

box-position
------------

Generic function
''''''''''''''''

Summary
       

Returns the position of the bounding box of a region as two values.

Signature
         

*box-position* *region* => *x* *y*

Arguments
         

-  *region* An instance of type *<region>*.

Values
      

-  *x* An instance of type <integer>.
-  *y*

Description
           

Returns the position of the bounding box of *region* as two values. The
position of a bounding box is specified by its top left point.

See also
        

`box-height`_

`box-size`_

`box-width`_

box-right
---------

Function
''''''''

Summary
       

Returns the *x* coordinate of the bottom right corner of the bounding
box of a region.

Signature
         

*box-right* *region* => *right*

Arguments
         

-  *region* An instance of type *<region>*.

Values
      

-  *right* An instance of type *<integer>*.

Description
           

Returns the *x* coordinate of the bottom right corner of the bounding
box *region*. The argument *region* must be either a bounded region or
some other object that obeys the bounding box protocol, such as a sheet.

See also
        

`box-bottom`_

`box-left`_

`box-top`_

box-size
--------

Generic function
''''''''''''''''

Summary
       

Returns the width and height of the bounding box of a region as two
values

Signature
         

*box-size* *region* => *width* *height*

Arguments
         

-  *region* An instance of type *<region>*.

Values
      

-  *width* An instance of type *<integer>*.
-  *height* An instance of type *<integer>*.

Description
           

Returns the width and height of the bounding box of *region* as two
values The argument *region* must be either a bounded region or some
other object that obeys the bounding box protocol, such as a sheet.

See also
        

`box-height`_

`box-position`_

`box-width`_

box-top
-------

Function
''''''''

Summary
       

Returns the *y* coordinate of the upper left corner of the bounding box
of a region.

Signature
         

*box-top* *region* => *top*

Arguments
         

-  *region* An instance of type *<region>*.

Values
      

-  *top* An instance of type *<integer>*.

Description
           

Returns the *y* coordinate of the upper left corner of the bounding box
*region*. The argument *region* must be either a bounded region or some
other object that obeys the bounding box protocol.

See also
        

`box-bottom`_

`box-left`_

`box-right`_

box-width
---------

Function
''''''''

Summary
       

Returns the width of the bounding box of a region.

Signature
         

box-width *region* => *width*

Arguments
         

-  *region* An instance of type *<region>*.

Values
      

-  *width* An instance of type *<integer>*.

Description
           

Returns the width of the bounding box *region*. The width of a bounding
box is the difference between its maximum *x* coordinate (right) and its
minimum *x* coordinate (left).The argument *region* must be either a
bounded region or some other object that obeys the bounding box
protocol, such as a sheet.

See also
        

`box-height`_

`box-position`_

`box-size`_

compose-rotation-with-transform
-------------------------------

Generic function
''''''''''''''''

Summary
       

Creates a new transform by composing a transform with the given rotation

Signature
         

*compose-rotation-with-transform* *transform* *angle* *#key* *origin* =>
*transform*

Arguments
         

-  *transform* An instance of type `<transform>`_.
-  *angle* An instance of type *<real>*.
-  *origin* An instance of type `<point>`_. Default
   value: (0, 0).

Values
      

-  *transform* An instance of type `<transform>`_.

Description
           

Creates a new transform by composing the transform *transform* with the
given rotation The order of composition is that the rotation transform
is applied first, followed by the argument *transform*.

Note that this function could be implemented by using
*make-rotation-transform* and *compose-transforms*. It is provided
because it is common to build up a transform as a series of simple
transforms.

See also
        

`make-rotation-transform`_

compose-scaling-with-transform
------------------------------

Generic function
''''''''''''''''

Summary
       

Creates a new transform by composing a transform with the given scaling.

Signature
         

*compose-scaling-with-transform* *transform* *scale-x* *scale-y* #key
*origin* => *transform*

Arguments
         

-  *transform* An instance of type `<transform>`_.
-  *scale-x* An instance of type *<real>*.
-  *scale-y* An instance of type *<real>*.
-  *origin* An instance of type `<point>`_. Default
   value: (0, 0).

Values
      

-  *transform* An instance of type `<transform>`_.

Description
           

Creates a new transform by composing the transform *transform* with the
given scaling. The order of composition is that the scaling transform is
applied first, followed by the argument *transform*.

The argument *scale-x* represents the scaling factor for the *x*
direction.

The argument *scale-y* represents the scaling factor for the *y*
direction.

The argument *origin* represents the point around which scaling is
performed. The default is to scale around the origin.

Note that this function could be implemented by using
*make-scaling-transform* and *compose-transforms*. It is provided
because it is common to build up a transform as a series of simple
transforms.

See also
        

`make-scaling-transform`_

compose-transforms
------------------

Generic function
''''''''''''''''

Summary
       

Returns a transform that is the mathematical composition of its
arguments.

Signature
         

*compose-transforms* *transform1* *transform2* => *transform*

Arguments
         

-  *transform1* An instance of type `<transform>`_.
-  *transform2* An instance of type `<transform>`_.

Values
      

-  *transform* An instance of type `<transform>`_.

Description
           

Returns a transform that is the mathematical composition of its
arguments. Composition is in right-to-left order, that is, the resulting
transform represents the effects of applying the transform *transform2*
followed by the transform *transform1*.

See also
        

`compose-transform-with-rotation`_

compose-transform-with-rotation
-------------------------------

Generic function
''''''''''''''''

Summary
       

Creates a new transform by composing a given rotation with a transform.

Signature
         

*compose-transform-with-rotation* *transform* *angle* #key *origin* =>
*transform*

Arguments
         

-  *transform* An instance of type `<transform>`_.
-  *angle* An instance of type *<real>*.
-  *origin* An instance of type `<point>`_. Default
   value: (0,0).

Values
      

-  *transform* An instance of type `<transform>`_.

Description
           

Creates a new transform by composing a given rotation with the transform
*transform.* The order of composition is *transform* first, followed by
the rotation transform.

The argument *angle* represents the angle by which to rotate, in
radians.

The argument *origin* represents the point about which to rotate. The
default is to rotate around (0,0).

Note that this function could be implemented by using
*make-rotation-transform* and *compose-transforms*. It is provided
because it is common to build up a transform as a series of simple
transforms.

See also
        

`compose-transforms`_

`make-rotation-transform`_

compose-transform-with-scaling
------------------------------

Generic function
''''''''''''''''

Summary
       

Creates a new transform by composing a given scaling with a transform.

Signature
         

*compose-transform-with-scaling* *transform* *scale-x* *scale-y* *#key*
*origin* => *transform*

Arguments
         

-  *transform* An instance of type `<transform>`_.
-  *scale-x* An instance of type *<real>*.
-  *scale-y* An instance of type *<real>*.
-  o*rigin* An instance of type `<point>`_. Default
   value: (0,0).

Values
      

-  *transform* An instance of type `<transform>`_.

Description
           

Creates a new transform by composing a given scaling with the transform
*transform.* The order of composition is *transform* first, followed by
the scaling transform.

The argument *scale-x* represents the scaling factor for the *x*
direction.

The argument *scale-y* represents the scaling factor for the *y*
direction.

The argument *origin* represents the point around which scaling is
performed. The default is to scale around the origin.

Note that this function could be implemented by using
*make-scaling-transform* and *compose-transforms*. It is provided
because it is common to build up a transform as a series of simple
transforms.

See also
        

`compose-transforms`_.

`make-scaling-transform`_

compose-transform-with-translation
----------------------------------

Generic function
''''''''''''''''

Summary
       

Creates a new transform by composing a given translation with a
transform*.*

Signature
         

*compose-transform-with-translation* *transform* *dx* *dy* =>
*transform*

Arguments
         

-  *transform* An instance of type `<transform>`_.
-  *dx* An instance of type *<real>*.
-  *dy* An instance of type *<real>*.

Values
      

-  *transform* An instance of type `<transform>`_.

Description
           

Creates a new transform by composing a given translation with the
transform *transform*. The order of composition is *transform* first,
followed by the translation transform.

The argument *dx* represents the *delta* by which to translate the *x*
coordinate.

The argument *dy* represents the *delta* by which to translate the *y*
coordinate.

Note that this function could be implemented by using
*make-translation-transform* and *compose-transforms*. It is provided
because it is common to build up a transform as a series of simple
transforms.

See also
        

See the functions `make-translation-transform`_ and
`compose-transforms`_.

compose-translation-with-transform
----------------------------------

Generic function
''''''''''''''''

Summary
       

Creates a new transform by composing a transform with the given
translation.

Signature
         

*compose-translation-with-transform* *transform* *dx* *dy* =>
*transform*

Arguments
         

-  *transform* An instance of type `<transform>`_.
-  *dx* An instance of type *<real>*.
-  *dy* An instance of type *<real>*.

Values
      

-  *transform* An instance of type `<transform>`_.

Description
           

Creates a new transform by composing the transform *transform* with the
given translation. The order of composition is that the translation
transform is applied first, followed by the argument *transform*.

The argument *dx* represents the *delta* by which to translate the *x*
coordinate.

The argument *dy* represents the *delta* by which to translate the *y*
coordinate.

Note that this function could be implemented by using
*make-translation-transform* and *compose-transforms*. It is provided,
because it is common to build up a transform as a series of simple
transforms.

See also
        

See the functions `make-translation-transform`_ and
`compose-transforms`_.

do-coordinates
--------------

Function
''''''''

Summary
       

Applies a function to each coordinate pair in its argument list.

Signature
         

*do-coordinates* *function* *coordinates* => ()

Arguments
         

-  *function* An instance of type *<function>*.
-  *coordinates* An instance of type *limited(<sequence>, of: <real>)*.

Values
      

None

Description
           

Applies *function* to each coordinate pair in *coordinates*. The length
of *coordinates* must be a multiple of 2. *Function* takes two
arguments, the *x* and *y* value of each coordinate pair.

do-endpoint-coordinates
-----------------------

Function
''''''''

Summary
       

Applies a function to each coordinate pair in its argument list.

Signature
         

*do-endpoint-coordinates* *function* *coordinates* => ()

Arguments
         

-  *function* An instance of type *<function>*.
-  *coordinates* An instance of type *limited(<sequence>, of: <real>)*.

Values
      

None

Description
           

Applies *function* to each pair of coordinate pairs in *coordinates*.
The arguments *coordinates* represents a set of line segments rather
than a set of points: The length of this sequence must therefore be a
multiple of 4. Function takes 4 arguments, (*x1*, *y1*, *x2*, *y2*).

do-regions
----------

Generic function
''''''''''''''''

Summary
       

Calls a function on each region in a set of regions.

Signature
         

*do-regions* *function* *region* *#key* *normalize?* => ()

Arguments
         

-  *function* An instance of type *<function>*.
-  *region* An instance of type *<region>*.
-  *normalize?* An instance of type *<boolean>*. Default value: *#f*.

Values
      

None

Description
           

Calls *function* on each region in the region set *region.* This is
often more efficient than calling *region-set-regions*. *function* is a
function of one argument, a region. *Region* can be either a region set
or a simple region, in which case *function* is called once on *region*
itself. If *normalize* is supplied, it must be either *#"x-banding"* or
*#"y-banding"*. If it is *#"x-banding"* and all the regions in *region*
are axis-aligned rectangles, the result is normalized by merging
adjacent rectangles with banding done in the *x* direction. If it is
*#"y-banding"* and all the regions in *region* are rectangles, the
result is normalized with banding done in the *y* direction. Normalizing
a region set that is not composed entirely of axis-aligned rectangles
using x- or y-banding causes DUIM to signal the
*<region-set-not-rectangular>* error.

even-scaling-transform?
-----------------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if the transform *transform* multiplies all *x* lengths and
*y* lengths by the same magnitude, otherwise returns *#f*.

Signature
         

*even-scaling-transform?* *transform* => *boolean*

Arguments
         

-  *transform* An instance of type `<transform>`_.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if the transform *transform* multiplies all *x* lengths and
*y* lengths by the same magnitude, otherwise returns *#f*.
*even-scaling-transform?* includes pure reflections through vertical and
horizontal lines.

$everywhere
-----------

Constant
''''''''

Summary
       

The region that includes all the points on the two-dimensional infinite
drawing plane.

Type
    

*<region>*

Description
           

The region that includes all the points on the two-dimensional infinite
drawing plane.

See also
        

`$nowhere`_

fix-coordinate
--------------

Function
''''''''

Summary
       

Coerces the given coordinate into an *<integer>*.

Signature
         

*fix-coordinate* *coordinate* => *integer*

Arguments
         

-  *coordinate* An instance of type *<real>*.

Values
      

-  *integer* An instance of type *<integer>*.

Description
           

Coerces the given coordinate into an *<integer>*.

$identity-transform
-------------------

Constant
''''''''

Summary
       

An instance of a transform that is guaranteed to be an identity
transform, that is, the transform that does nothing.

Type
    

`<transform>`_

Description
           

An instance of a transform that is guaranteed to be an identity
transform, that is, the transform that does nothing.

See also
        

`identity-transform?`_

identity-transform?
-------------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if a transform is equal (in the sense of *transform-equal*
) to the identity transform.

Signature
         

*identity-transform?* *transform* => *boolean*

Arguments
         

-  *transform* An instance of type `<transform>`_.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if the transform *transform* is equal (in the sense of
*transform-equal*) to the identity transform, otherwise returns *#f*.

See also
        

`$identity-transform`_

invert-transform
----------------

Generic function
''''''''''''''''

Summary
       

Returns a transform that is the inverse of the given transform.

Signature
         

*invert-transform* *transform* => *transform*

Arguments
         

-  *transform* An instance of type `<transform>`_.

Values
      

-  *transform* An instance of type `<transform>`_.

Exceptions
          

If *transform* is singular, *invert-transform* signals the *`See
<singular-transform>`_* error.

*Note:* With finite-precision arithmetic there are several low-level
conditions that might occur during the attempt to invert a singular or
*almost* singular transform. (These include computation of a zero
determinant, floating-point underflow during computation of the
determinant, or floating-point overflow during subsequent
multiplication.) *invert-transform* signals the *<singular-transform>*
error for all of these cases.

Description
           

Returns a transform that is the inverse of the transform *transform*.
The result of composing a transform with its inverse is equal to the
identity transform.

See also
        

`invertible-transform?`_

invertible-transform?
---------------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if the given transform has an inverse.

Signature
         

*invertible-transform?* *transform* => *boolean*

Arguments
         

-  *transform* An instance of type `<transform>`_.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if the transform *transform* has an inverse, otherwise
returns *#f*.

See also
        

`invert-transform`_

$largest-coordinate
-------------------

Constant
''''''''

Summary
       

The largest valid coordinate.

Type
    

*<integer>*

Description
           

The largest valid coordinate.

See also
        

`$smallest-coordinate`_

make-3-point-transform
----------------------

Function
''''''''

Summary
       

Returns a transform that takes points *point-1* into *point-1-image*,
*point-2* into *point-2-image* and *point-3* into *point-3-image*.

Signature
         

*make-3-point-transform* *x1* *y1* *x2* *y2* *x3* *y3* *x1-image*
*y1-image* *x2-image* *y2-image* *x3-image* *y3-image* => *transform*

make-3-point-transform\* *point-1 point-2 point-3 point-1-image
point-2-image point-3-image* => *transform*

Arguments
         

The following arguments are specific to *make-3-point-transform*.

-  *x1* An instance of type *<real>*.
-  *y1* An instance of type *<real>*.
-  *x2* An instance of type *<real>*.
-  *y2* An instance of type *<real>*.
-  *x3* An instance of type *<real>*.
-  *y3* An instance of type *<real>*.
-  *x1-image* An instance of type *<real>*.
-  *y1-image* An instance of type *<real>*.
-  *x2-image* An instance of type *<real>*.
-  *y2-image* An instance of type *<real>*.
-  *x3-image* An instance of type *<real>*.
-  *y3-image* An instance of type *<real>*.

The following arguments are specific to *make-3-point-transform\**.

-  *point-1* An instance of type `<point>`_.
-  *point-2* An instance of type `<point>`_.
-  *point-3* An instance of type `<point>`_.
-  *point-1-image* An instance of type `<point>`_.
-  *point-2-image* An instance of type `<point>`_.
-  *point-3-image* An instance of type `<point>`_.

Values
      

-  *transform* An instance of type `<transform>`_.

Exceptions
          

If *point-1*, *point-2* and *point-3* are colinear, the*`See
<transform-underspecified>`_* error is signalled. If
*point-1-image*,*point-2-image* and *point-3-image* are colinear, the
resulting transform will be singular (that is, will have no inverse) but
this is not an error.

Description
           

Returns a transform that takes points *point-1* into *point-1-image*,
*point-2* into *point-2-image* and *point-3* into *point-3-image*.
Three non-colinear points and their images under the transform are
enough to specify any affine transformation.

The function *make-3-point-transform\** is identical to
*make-3-point-transform*, except that it passes composite objects,
rather than separate coordinates, in its arguments. You should be aware
that using this function may lead to a loss of performance.

make-bounding-box
-----------------

Function
''''''''

Summary
       

Returns an object of the class *<bounding-box>.*

Signature
         

*make-bounding-box* *x1* *y1* *x2* *y2* => *box*

Arguments
         

-  *x1* An instance of type *<real>*.
-  *y1* An instance of type *<real>*.
-  *x2* An instance of type *<real>*.
-  *y2* An instance of type *<real>*.

Values
      

-  *box* An instance of type *`<bounding-box>`_*.

Description
           

Returns an object of the class *`<bounding-box>`_*
with the edges specified by *x1, y1, x2*, and *y2. x1, y1, x2*, and
*y2* are canonicalized in the following way. The min point of the box
has an *x* coordinate that is the smaller of *x1* and *x2* and a *y*
coordinate that is the smaller of *y1* and *y2*. The max point of the
box has an *x* coordinate that is the larger of *x1* and *x2* and a *y*
coordinate that is the larger of *y1* and *y2*. (Therefore, in a
right-handed coordinate system the canonicalized values of *x1, y1, x2,*
and *y2* correspond to the left, top, right, and bottom edges of the
box, respectively.)

This is a convenient shorthand function for *make(`See
<bounding-box>`_, left: top: right: bottom:)*

make-point
----------

Function
''''''''

Summary
       

Returns an object of class *<point>*.

Signature
         

*make-point* *x* *y* => *point*

Arguments
         

-  *x* An instance of *<real>*.
-  *y* An instance of *<real>*

Values
      

-  *point* An instance of type `<point>`_.

Description
           

Returns an object of class *<point>* whose coordinates are *x* and *y*.

make-reflection-transform
-------------------------

Function
''''''''

Summary
       

Returns a transform that reflects every point through the line passing
through the positions *x1,y1* and *x2,y2* or through the points *point1*
and *point2*.

Signature
         

*make-reflection-transform* *x1* *y1* *x2* *y2* => *transform*

make-reflection-transform\* *point-1* *point-2* => *transform*

Arguments
         

The following arguments are specific to *make-reflection-transform*.

-  *x1* An instance of type *<real>*.
-  *y1* An instance of type *<real>*.
-  *x2* An instance of type *<real>*.
-  *y2* An instance of type *<real>*.

The following arguments are specific to *make-reflection-transform\**.

-  *point1* An instance of type `<point>`_. The
   first point.
-  *point2* An instance of type `<point>`_. The
   second point.

Values
      

-  *transform* An instance of type `<transform>`_.
   The resultant transformation.

Description
           

Returns a transform that reflects every point through the line passing
through the positions *x1,y1* and *x2,y2* or through the points *point1*
and *point2*.

The arguments *x1* and *y1* represent the coordinates of the first point
of reflection. The arguments *x2* and *y2* represent the coordinates of
the second point of reflection.

A reflection is a transform that preserves lengths and magnitudes of
angles, but changes the sign (or handedness) of angles. If you think of
the drawing plane on a transparent sheet of paper, a reflection is a
transformation that turns the paper over.

The function *make-reflection-transform\** is identical to
*make-reflection-transform*, except that it passes composite objects,
rather than separate coordinates, in its arguments. You should be aware
that using this function may lead to a loss of performance.

See also
        

`make-rotation-transform`_

`make-scaling-transform`_

`make-transform`_

`make-translation-transform`_

`<reflection-underspecified>`_

make-rotation-transform
-----------------------

Function
''''''''

Summary
       

Returns a transform that rotates all points by *angle* around the point
specified by coordinates *origin-x* and *origin-y* or the point object
*origin*.

Signature
         

*make-rotation-transform* *angle* *#key* *origin-x* *origin-y* =>
*transform*

make-rotation-transform\* *angle* #key *origin* => *transform*

Arguments
         

-  *angle* An instance of type *<real>*.

The following arguments are specific to *make-rotation-transform*.

-  *origin-x* An instance of type *<real>*. Default value: *0*.
-  *origin-y* An instance of type *<real>*. Default value: *0*.

The following argument is specific to *make-reflection-transform\**.

-  *origin* An instance of type `<point>`_. Default
   value: (0, 0).

Values
      

-  *transform* An instance of type `<transform>`_.

Description
           

Returns a transform that rotates all points by *angle* around the point
specified by coordinates *origin-x* and *origin-y* or the point object
*origin*. The angle must be expressed in radians.

A rotation is a transform that preserves length and angles of all
geometric entities. Rotations also preserve one point (the origin) and
the distance of all entities from that point.

The function *make-rotation-transform\** is identical to
*make-rotation-transform*, except that it passes composite objects,
rather than separate coordinates, in its arguments. You should be aware
that using this function may lead to a loss of performance.

See also
        

`make-reflection-transform`_

`make-scaling-transform`_

`make-transform`_

`make-translation-transform`_

make-scaling-transform
----------------------

Function
''''''''

Summary
       

Returns a transform that multiplies the *x* -coordinate distance of
every point from *origin* by *scale-x* and the *y* -coordinate distance
of every point from *origin* by *scale-y*.

Signature
         

*make-scaling-transform* *scale-x* *scale-y* #key *origin-x* *origin-y*
=> *transform*

make-scaling-transform\* *scale-x* *scale-y* #key *origin* =>
*transform*

Arguments
         

-  *scale-x* An instance of type *<real>*.
-  *scale-y* An instance of type *<real>*.

The following arguments are specific to *make-scaling-transform*.

-  *origin-x* An instance of type *<real>*. Default value: 0.
-  *origin-y* An instance of type *<real>*. Default value: 0.

The following argument is specific to *make-scaling-transform\**.

-  *origin* An instance of type `<point>`_.

Values
      

-  *transform* An instance of type `<transform>`_.
   The resultant transformation.

Description
           

Returns a transform that multiplies the *x* -coordinate distance of
every point from *origin* by *scale-x* and the *y* -coordinate distance
of every point from *origin* by *scale-y*.

The argument *scale-x* represents the scaling factor for the *x*
direction.

The argument *scale-y* represents the scaling factor for the *y*
direction.

The arguments *origin-x* and *origin-y* represent the point around which
scaling is performed. The default is to scale around the origin.

There is no single definition of a scaling transformation. Transforms
that preserve all angles and multiply all lengths by the same factor
(preserving the *shape* of all entities) are certainly scaling
transformations. However, scaling is also used to refer to transforms
that scale distances in the *x* direction by one amount and distances in
the *y* direction by another amount.

The function *make-scaling-transform\** is identical to
*make-scaling-transform*, except that it passes composite objects,
rather than separate coordinates, in its arguments. You should be aware
that using this function may lead to a loss of performance.

See also
        

`make-reflection-transform`_

`make-rotation-transform`_

`make-transform`_

`make-translation-transform`_

make-transform
--------------

Function
''''''''

Summary
       

Returns a general affine transform.

Signature
         

*make-transform* *mxx* *mxy* *myx* *myy* *tx* *ty* => *transform*

Arguments
         

-  *mxx* An instance of type *<real>*.
-  *mxy* An instance of type *<real>*.
-  *myx* An instance of type *<real>*.
-  *myy* An instance of type *<real>*.
-  *tx* An instance of type *<real>*.
-  *ty* An instance of type *<real>*.

Values
      

-  *transform* An instance of type `<transform>`_.

Description
           

Returns a general transform whose effect is:

x'= m*xx* x + m*xy* y + t*x*
                            

y'= m*yx* x + m*yy* y + t*y*
                            

where *x* and *y* are the coordinates of a point before the transform
and *x'* and *y'* are the coordinates of the corresponding point after.

All of the arguments to *make-transform* must be real numbers.

This is a convenient shorthand for *make(* `See
<transform>`_*...).*

See also
        

`make-reflection-transform`_

`make-rotation-transform`_

`make-scaling-transform`_

`make-translation-transform`_

make-translation-transform
--------------------------

Function
''''''''

Summary
       

Returns a transform that translates all points by *dx* in the *x*
direction and *dy* in the *y* direction.

Signature
         

*make-translation-transform* *dx* *dy* => *transform*

Arguments
         

-  *dx* An instance of type *<real>*.
-  *dy* An instance of type *<real>*.

Values
      

-  *transform* An instance of type `<transform>`_.

Description
           

Returns a transform that translates all points by *dx* in the *x*
direction and *dy* in the *y* direction.

The argument *dx* represents the *delta* by which to translate the *x*
coordinate.

The argument *dy* represents the *delta* by which to translate the *y*
coordinate.

A translation is a transform that preserves length, angle, and
orientation of all geometric entities.

See also
        

`make-reflection-transform`_

`make-rotation-transform`_

`make-scaling-transform`_

`make-transform`_

$nowhere
--------

Constant
''''''''

Summary
       

The empty region, the opposite of `$everywhere`_.

Type
    

*<region>*

Description
           

The empty region, the opposite of `$everywhere`_.

See also
        

`$everywhere`_

<path>
------

Open abstract class
'''''''''''''''''''

Summary
       

The class *<path>* denotes bounded regions that have dimensionality 1
(that is, have length).

Superclasses
            

*<region>*

Init-keywords
             

None.

Description
           

The class *<path>* denotes bounded regions that have dimensionality 1
(that is, have length).

*<path>* is a subclass of *<region>*.

Constructing a *<path>* object with no length (via *make-line\**, for
example) may canonicalize it to *$nowhere*.

Operations
          

The following operation is exported from the *DUIM-Geometry* module.

-  `path?`_

See also
        

`path?`_

path?
-----

Generic function
''''''''''''''''

Summary
       

Returns *#t* if its argument is a path.

Signature
         

*path?* *object* => *boolean*

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if *object* is a path, otherwise returns *#f*.

See also
        

`<path>`_

<point>
-------

Open abstract instantiable class
''''''''''''''''''''''''''''''''

Summary
       

The class that corresponds to a mathematical point.

Superclasses
            

*<region>*

Init-keywords
             

-  *x:* An instance of type *<integer>*.
-  *y:* An instance of type *<integer>*.

Description
           

The class that corresponds to a mathematical point.*<point>* is a
subclass of *<region>*. The *x:* and *y:* init-keywords correspond to
the x and y coordinates, respectively.

Operations
          

The following operations are exported from the *DUIM-Geometry* module.

`=`_ `box-edges`_ `See
point?`_ `point-position`_ `See
point-x`_ `point-y`_ `See
region-contains-position?`_ `See
region-contains-region?`_ `See
region-intersection`_ `See
region-intersects-region?`_
 `transform-region`_

point?
------

Generic function
''''''''''''''''

Summary
       

Returns true if *object* is a point*.*

Signature
         

*point?* *object* => *boolean*

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if *object* is a point*.*

point-position
--------------

Generic function
''''''''''''''''

Summary
       

Returns both the *x* and *y* coordinates of a point.

Signature
         

*point-position* *point* => *x* *y*

Arguments
         

-  *point* An instance of type `<point>`_.

Values
      

-  *x* An instance of type *<real>*.
-  *y* An instance of type *<real>*.

Description
           

Returns both the *x* and *y* coordinates of the point *point* as two
values.

See also
        

`point-x`_

`point-y`_

point-x
-------

Generic function
''''''''''''''''

Summary
       

Returns the *x* coordinate of a point.

Signature
         

*point-x* *point* => *x*

Arguments
         

-  *point* An instance of type `<point>`_.

Values
      

-  *x* An instance of type *<real>*.

Description
           

Returns the *x* coordinate of *point*.

See also
        

`point-position`_

`point-y`_

point-y
-------

Generic function
''''''''''''''''

Summary
       

Returns the *y* coordinate of a point.

Signature
         

*point-y* *point* => *y*

Arguments
         

-  *point* An instance of type `<point>`_.

Values
      

-  *y* An instance of type *<real>*

Description
           

Returns the *y* coordinate of *point*.

See also
        

`point-position`_

`point-x`_

rectilinear-transform?
----------------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if a transform always transforms any axis-aligned rectangle
into another axis-aligned rectangle.

Signature
         

*rectilinear-transform?* *transform* => *boolean*

Arguments
         

-  *transform* An instance of type `<transform>`_.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if the transform *transform* always transforms any
axis-aligned rectangle into another axis-aligned rectangle, otherwise
returns *#f*.

This category includes scalings as a subset, and also includes 90 degree
rotations.

Rectilinear transforms are the most general category of transforms for
which the bounding rectangle of a transformed object can be found by
transforming the bounding rectangle of the original object.

reflection-transform?
---------------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if the transform inverts the *handedness* of the coordinate
system*.*

Signature
         

*reflection-transform?* *transform* => *boolean*

Arguments
         

-  *transform* An instance of type `<transform>`_.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if the transform *transform* inverts the *handedness* of
the coordinate system, otherwise returns *#f.*

Note that this is a very inclusive category — transforms are considered
reflections even if they distort, scale, or skew the coordinate system,
as long as they invert the handedness.

<reflection-underspecified>
---------------------------

Sealed concrete class
'''''''''''''''''''''

Summary
       

The error that is signalled when *make-reflection-transform* is given
two coincident points.

Superclasses
            

*<transform-underspecified>*

Init-keywords
             

-  *points:* Instances of type `<point>`_.

Description
           

The error that is signalled when *make-reflection-transform* is given
two coincident points. This condition handles the *points:* initarg,
which is used to supply the points that are in error.

Operations
          

-  None.

See also
        

`make-reflection-transform`_

<region>
--------

Open abstract class
'''''''''''''''''''

Summary
       

The class that corresponds to a set of points.

Superclasses
            

*<object>*

Init-keywords
             

None.

Description
           

The class that corresponds to a set of points. The*<region>* class
includes both bounded and unbounded regions.

There is no *make* method for *<region>* because of the impossibility of
a uniform way to specify the arguments to such a function.

Operations
          

The following operations are exported from the *DUIM-Geometry* module.

`=`_ `do-regions`_ `See
region?`_ `See
region-contains-position?`_ `See
region-contains-region?`_ `See
region-difference`_ `See
region-empty?`_ `region-equal`_
`region-intersection`_ `See
region-intersects-region?`_ `See
region-set-function`_ `See
region-set-regions`_ `See
region-union`_

See also
        

`region?`_

region?
-------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if its argument is a region.

Signature
         

*region?* *object* => *boolean*

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if *object* is a region, otherwise returns*#f*.

See also
        

`<region>`_

region-contains-position?
-------------------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if the point at *x,y* is contained in the region.

Signature
         

*region-contains-position?* *region* *x* *y* => *boolean*

Arguments
         

-  *region* An instance of type *<region>*.
-  *x* An instance of type *<real>*.
-  *y* An instance of type *<real>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if the point at *x,y* is contained in the region *region*,
otherwise returns *#f*. Since regions in DUIM are closed, this returns
*#t* if the point at *x,y* is on the region's boundary.

See also
        

`region-contains-region?`_

region-contains-region?
-----------------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if all points in the second region are members of the first
region.

Signature
         

*region-contains-region?* *region1* *region2* => *boolean*

Arguments
         

-  *region1* An instance of type *<region>*.
-  *region2* An instance of type *<region>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if all points in the region *region2* are members of the
region *region1*, otherwise returns *#f*.*region-contains-position?*
is a special case of *region-contains-region?* in which the region is
the point *x,y*.

See also
        

`region-contains-position?`_.

region-difference
-----------------

Generic function
''''''''''''''''

Summary
       

Returns a region that contains all points in the region *region1* that
are not in the region *region2* (possibly plus additional boundary
points to make the result closed).

Signature
         

*region-difference* *region1* *region2* => *region*

Arguments
         

-  *region1* An instance of type *<region>*.
-  *region2* An instance of type *<region>*.

Values
      

-  *region* An instance of type *<region>*.

Description
           

Returns a region that contains all points in the region *region1* that
are not in the region *region2* (possibly plus additional boundary
points to make the result closed).

The result of *region-difference* has the same dimensionality as
*region1*, or is *$nowhere*. For example, the difference of an area
and a path produces the same area; the difference of a path and an area
produces the path clipped to stay outside of the area.

*Note:* *region-difference* may return either a simple region or a
region set.

region-empty?
-------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if the region is empty.

Signature
         

*region-empty?* *region* => *boolean*

Arguments
         

-  *region* An instance of type *<region>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if the region is empty, otherwise returns *#f*.

region-equal
------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if the two regions *region1* and *region2* contain exactly
the same set of points.

Signature
         

*region-equal* *region1* *region2* => *boolean*

Arguments
         

-  *region1* An instance of type *<region>*.
-  *region2* An instance of type *<region>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if the two regions *region1* and *region2* contain exactly
the same set of points, otherwise returns *#f*. There is a method on
*=* on *<region>* and *<region>* that calls *region-equal*.

region-intersection
-------------------

Generic function
''''''''''''''''

Summary
       

Returns the intersection of two regions, as a region.

Signature
         

*region-intersection* *region1* *region2* => *region*

Arguments
         

-  *region1* An instance of type *<region>*.
-  *region2* An instance of type *<region>*.

Values
      

-  *region* An instance of type *<region>*.

Description
           

Returns a region that contains all points that are in both of the
regions *region1* and *region2* (possibly with some points removed in
order to satisfy the dimensionality rule).

The result of *region-intersection* has dimensionality that is the
minimum dimensionality of *region1* and *region2*, or is *$nowhere*.
For example, the intersection of two areas is either another area or
*$nowhere* ; the intersection of two paths is either another path or
*$nowhere* ; the intersection of a path and an area produces the path
clipped to stay inside of the area.

*Note:* *region-intersection* may return either a simple region or a
region set.

See also
        

`region-union`_

region-intersects-region?
-------------------------

Generic function
''''''''''''''''

Summary
       

Returns *#f* if two regions do not intersect*.*

Signature
         

*region-intersects-region?* *region1* *region2* => *boolean*

Arguments
         

-  *region1* An instance of type *<region>*.
-  *region2* An instance of type *<region>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#f* if *region-intersection* of the two regions *region1* and
*region2* would be *$nowhere* (that is, they do not intersect),
otherwise returns *#t.*

<region-set>
------------

Open abstract class
'''''''''''''''''''

Summary
       

The class that represents a region set.

Superclasses
            

*<region>*

Init-keywords
             

None.

Description
           

The class that represents a region set; a subclass of *<region>*.

Operations
          

The following operations are exported from the *DUIM-Geometry* module.

`box-edges`_ `do-regions`_
`region-contains-position?`_ `See
region-contains-region?`_ `See
region-difference`_ `See
region-empty?`_ `See
region-intersection`_ `See
region-set-function`_ `See
region-set-regions`_ `See
region-union`_ `transform-region`_

See also
        

`region-set?`_

region-set?
-----------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if its argument is a region set.

Signature
         

*region-set?* *object* => *boolean*

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if *object* is a region set, otherwise returns *#f*.

See also
        

`<region-set>`_

region-set-function
-------------------

Generic function
''''''''''''''''

Summary
       

Returns the function that composed the region.

Signature
         

*region-set-function* *region* => *function*

Arguments
         

-  *region* An instance of type *<region>*.

Values
      

-  *function* An instance of type *<function>*.

Description
           

Returns the function that composed the region, *intersection*, *union*
, or *difference*.

region-set-regions
------------------

Generic function
''''''''''''''''

Summary
       

Returns a sequence of the regions in the region set.

Signature
         

*region-set-regions* *region* #key *normalize?* => *regions*

Arguments
         

-  *region* An instance of type *<region>*.
-  *normalize?* *one-of(#f, #"x-banding", "y-banding")*. Default value:
   *#f*.

Values
      

-  *regions* An instance of type *limited(<sequence>, of: <region>)*.

Exceptions
          

Normalizing a region set that is not composed entirely of axis-aligned
rectangles using x- or y-banding causes DUIM to signal the
*<region-set-not-rectangular>* error.

Description
           

Returns a sequence of the regions in the region set *region*.*region*
can be either a region set or a simple region, in which case the result
is simply a sequence of one element: region.

For the case of region sets that are unions of axis-aligned rectangles,
the rectangles returned by *region-set-regions* are guaranteed not to
overlap. If *normalize* is supplied, it must be either *#"x-banding"* or
*#"y-banding"*. If it is *#"x-banding"* and all the regions in *region*
are axis-aligned rectangles, the result is normalized by merging
adjacent rectangles with banding done in the *x* direction. If it is
*#"y-banding"* and all the regions in *region* are rectangles, the
result is normalized with banding done in the *y* direction.

region-union
------------

Generic function
''''''''''''''''

Summary
       

Returns the union of two regions, as a region.

Signature
         

*region-union* *region1* *region2* => *region*

Arguments
         

-  *region1* An instance of type *<region>*.
-  *region2* An instance of type *<region>*.

Values
      

-  *region* An instance of type *<region>*.

Description
           

Returns a region that contains all points that are in either of the
regions *region1* or *region2* (possibly with some points removed in
order to satisfy the dimensionality rule)

The result of *region-union* always has dimensionality that is the
maximum dimensionality of *region1* and *region2*. For example, the
union of a path and an area produces an area; the union of two paths is
a path.

*Note*: *region-union* may return either a simple region or a region
set.

See also
        

`region-intersection`_

rigid-transform?
----------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if the *transform* transforms the coordinate system as a
rigid object.

Signature
         

*rigid-transform?* *transform* => *boolean*

Arguments
         

-  *transform* An instance of type `<transform>`_.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if the *transform* transforms the coordinate system as a
rigid object, that is, as a combination of translations, rotations, and
pure reflections. Otherwise, it returns *#f*.

Rigid transforms are the most general category of transforms that
preserve magnitudes of all lengths and angles.

scaling-transform?
------------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if the transform *transform* multiplies all *x* lengths by
one magnitude and all *y* lengths by another magnitude, otherwise
returns *#f*.

Signature
         

*scaling-transform?* *transform* => *boolean*

Arguments
         

-  *transform* An instance of type `<transform>`_.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if the transform *transform* multiplies all *x* lengths by
one magnitude and all *y* lengths by another magnitude, otherwise
returns *#f*. This category includes even scalings as a subset.

set-box-edges
-------------

Generic function
''''''''''''''''

Summary
       

Sets the edges of a box and returns the bounding box.

Signature
         

*set-box-edges* *box* *left* *top* *right* *bottom* => *box*

Arguments
         

-  *box* An instance of type *`<bounding-box>`_*.
-  *left* An instance of type *<integer>*.
-  *top* An instance of type *<integer>*.
-  *right* An instance of type *<integer>*.
-  *bottom* An instance of type *<integer>*.

Values
      

-  *box* An instance of type *`<bounding-box>`_*.

Description
           

Sets the edges of a box and returns the bounding box *box*. This might
destructively modify *box* or it might not, depending on what class
*box* is.

set-box-position
----------------

Generic function
''''''''''''''''

Summary
       

Sets the position of the bounding box and returns a (possibly new) box.

Signature
         

*set-box-position* *box* *x* *y* => *box*

Arguments
         

-  *box* An instance of type *`<bounding-box>`_*.
-  *x* An instance of type *<real>*.
-  *y* An instance of type *<real>*.

Values
      

-  *box* An instance of type *`<bounding-box>`_*.

Description
           

Sets the position of the bounding box *box* and might or might not
modify the box.

set-box-size
------------

Generic function
''''''''''''''''

Summary
       

Sets the size (width and height) of the bounding box *box*.

Signature
         

*set-box-size* *box* *width* *height* => *box*

Arguments
         

-  *box* An instance of type *`<bounding-box>`_*.
-  *width* An instance of type <*integer* >.
-  *height* An instance of type <*integer* >

Values
      

-  *box* An instance of type *`<bounding-box>`_*.

Description
           

Sets the size (width and height) of the bounding box *box*.

<singular-transform>
--------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The error that is signalled when *invert-transform* is called on a
singular transform, that is, a transform that has no inverse.

Superclasses
            

*<transform-error>*

Init-keywords
             

-  *transform:* Used to supply the transform that is singular.

Description
           

The error that is signalled when *invert-transform* is called on a
singular transform, that is, a transform that has no inverse.

This condition handles the *transform:* initarg, which is used to supply
the transform that is singular.

Operations
          

-  None.

See also
        

`invert-transform`_

$smallest-coordinate
--------------------

Constant
''''''''

Summary
       

The smallest valid coordinate.

Type
    

*<integer>*

Description
           

The smallest valid coordinate. Coordinates must be instances of type
*<integer>*.

See also
        

`$largest-coordinate`_

<transform>
-----------

Open abstract instantiable class
''''''''''''''''''''''''''''''''

Summary
       

The superclass of all transforms.

Superclasses
            

*<object>*

Init-keywords
             

-  *mxx:* An instance of type *<real>*.
-  *mxy:* An instance of type *<real>*.
-  *myx:* An instance of type *<real>*.
-  *myy:* An instance of type *<real>*.
-  *tx:* An instance of type *<real>*.
-  *ty:* An instance of type *<real>*.

Description
           

The superclass of all transforms. There are one or more subclasses of
`<transform>`_ with implementation-dependent names
that implement transforms. The exact names of these classes is
explicitly unspecified.

All of the instantiable transformation classes provided by DUIM are
immutable.

Operations
          

The following operations are exported from the *DUIM-Geometry* module.

`=`_ `See
compose-rotation-with-transform`_ `See
compose-scaling-with-transform`_ `See
compose-transforms`_ `See
compose-transform-with-translation`_ `See
compose-translation-with-transform`_ `See
even-scaling-transform?`_ `See
identity-transform?`_ `See
invert-transform`_ `See
invertible-transform?`_
 `rectilinear-transform?`_ `See
reflection-transform?`_ `See
rigid-transform?`_ `See
scaling-transform?`_ `transform?`_
`transform-angles`_ `See
transform-box`_ `See
transform-distance`_ `See
transform-position`_ `See
transform-region`_ `See
translation-transform?`_ `See
untransform-angles`_ `See
untransform-box`_
 `untransform-distance`_ `See
untransform-position`_
 `untransform-region`_

See also
        

`transform?`_

transform?
----------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if its argument is a transform.

Signature
         

*transform?* *object* => *boolean*

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if *object* is a transform, otherwise returns *#f*.

See also
        

`<transform>`_

transform-angles
----------------

Generic function
''''''''''''''''

Summary
       

Applies the transform to the start and end angles of an object, and
returns the transformed angles.

Signature
         

*transform-angles* *transform* *start-angle* *end-angle* => *new-start*
*new-end*

Arguments
         

-  *transform* An instance of type `<transform>`_.
-  *start-angle* An instance of type *<real>*.
-  *end-angle* An instance of type *<real>*.

Values
      

-  *new-start* An instance of type *<real>*.
-  *new-end* An instance of type *<real>*.

Description
           

Applies the transform *transform* to the angles *start-angle* and
*end-angle* of an object, and returns the transformed angles.

transform-box
-------------

Generic function
''''''''''''''''

Summary
       

Applies the transform to the rectangle specified by the four coordinate
arguments.

Signature
         

*transform-box* *transform* *x1* *y1* *x2* *y2* => *left top* *right
bottom*

Arguments
         

-  *transform* An instance of type `<transform>`_.
-  *x1* An instance of type *<real>*.
-  *y1* An instance of type *<real>*.
-  *x2* An instance of type *<real>*.
-  *y2* An instance of type *<real>*.

Values
      

-  *left* An instance of type *<real>*.
-  *top* An instance of type *<real>*.
-  *right* An instance of type *<real>*.
-  *bottom* An instance of type *<real>*.

Description
           

Applies the transform *transform* to the rectangle specified by the four
coordinate arguments. *transform-box* is the spread version of `See
transform-region`_ in the case where the transform is
rectilinear and the region is a rectangle.

The arguments *x1*, *y1*, *x2*, and *y2* are canonicalized and the
four return values specify the minimum and maximum points of the
transformed rectangle in the order *left*, *top*, *right*, and
*bottom*.

An error is signalled if *transform* does not satisfy `See
rectilinear-transform?`_.

transform-distance
------------------

Generic function
''''''''''''''''

Summary
       

Applies a transform to a distance represented by the coordinate
arguments and returns the transformed coordinates.

Signature
         

*transform-distance* *transform* *dx* *dy* => *dx* *dy*

Arguments
         

-  *transform* An instance of type `<transform>`_.
-  *dx* An instance of type *<real>*.
-  *dy* An instance of type *<real>*.

Values
      

-  *dx* An instance of type *<real>*.
-  *dy* An instance of type *<real>*.

Description
           

Applies the transform *transform* to the distance represented by *dx*
and *dy*, and returns the transformed *dx* and *dy*. A distance
represents the difference between two points. It does not transform like
a point.

<transform-error>
-----------------

Sealed class
''''''''''''

Summary
       

The superclass of all error conditions distributed when there is an
error with a transform.

Superclasses
            

*<error>*

Init-keywords
             

None.

Description
           

The class that is the superclass of three error conditions, *`See
<transform-underspecified>`_*,
 `<reflection-underspecified>`_*,* and
 `<singular-transform>`_.

Operations
          

None.

transform-position
------------------

Generic function
''''''''''''''''

Summary
       

Applies a transform to the point whose coordinates are *x* and *y*.

Signature
         

*transform-position* *transform* *x* *y* => new-*x* new-*y*

Arguments
         

-  *transform* An instance of type `<transform>`_.
-  *x* An instance of type *<real>*
-  *y* An instance of type *<real>*

Values
      

-  *new-x* An instance of type *<real>*
-  *new-y* An instance of type *<real>*

Description
           

Applies the transform *transform* } to the point whose coordinates are
*x* and *y*.*transform-position* is the *spread* version of `See
transform-region`_ in the case where the region is a
point.

transform-region
----------------

Generic function
''''''''''''''''

Summary
       

Applies a transform to a region, and returns the transformed region.

Signature
         

*transform-region* *transform* *region* => *region*

Arguments
         

-  *transform* An instance of type `<transform>`_.
-  *region* An instance of type *<region>*.

Values
      

-  *region* An instance of type *<region>*.

Description
           

Applies *transform* to the region *region*, and returns the transformed
region.

<transform-underspecified>
--------------------------

Sealed concrete class
'''''''''''''''''''''

Summary
       

The error that is signalled when *make-3-point-transform* is given three
colinear image points.

Superclasses
            

*<transform-error>*

Init-keywords
             

-  *points:* The points that are in error.

Description
           

The error that is signalled when *make-3-point-transform* is given three
colinear image points. This condition handles the *points:* initarg,
which is used to supply the points that are in error.

Operations
          

-  None.

See also
        

`make-3-point-transform`_

translation-transform?
----------------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if a transform is a pure translation, that is, a transform
such that there are two distance components transform *dx* and *dy* and
every point *(x,y)* is moved to *(x+dx,y+dy)*.

Signature
         

*translation-transform?* *transform* => *boolean*

Arguments
         

-  *transform* An instance of type `<transform>`_.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if the transform *transform* is a pure translation, that
is, a transform such that there are two distance components transform
*dx* and *dy* and every point *(x,y)* is moved to *(x+dx,y+dy)*.
Otherwise, *translation-transform?* returns *#f*.

untransform-angles
------------------

Generic function
''''''''''''''''

Summary
       

Undoes a transform and** returns the original start and end angles of
the object.

Signature
         

*untransform-angles* *transform* *start-angle* *end-angle* =>
*orig-start* *orig-end*

Arguments
         

-  *transform* An instance of type `<transform>`_.
-  *start-angle* An instance of type *<real>*.
-  *end-angle* An instance of type *<real>*.

Values
      

-  *orig-start* An instance of type *<real>*.
-  *orig-end* An instance of type *<real>*.

Exceptions
          

*<singular-transform>* cannot be inverted.

Description
           

Undoes the transform *transform* to the angles *new-start* and*new-end,*
returning the original *orig-start* and *orig-end.* This is exactly
equivalent to:

transform-angles(invert-transform(*transform*))
                                                

untransform-box
---------------

Generic function
''''''''''''''''

Summary
       

Undoes the previous transformation on the rectangle *left, top* and
*right, bottom,* returning the original box.

Signature
         

*untransform-box* *transform x1 y1 x2 y2* => *left top right bottom*

Arguments
         

-  *transform* An instance of type `<transform>`_.
-  *x1* An instance of type *<real>*.
-  *y1* An instance of type *<real>*.
-  *x2* An instance of type *<real>*.
-  *y2* An instance of type *<real>*.

Values
      

-  *left* An instance of type *<real>*.
-  *top* An instance of type *<real>*.
-  *right* An instance of type *<real>*.
-  *bottom* An instance of type *<real>*.

Exceptions
          

*<singular-transform>* cannot be inverted.

Description
           

Undoes the previous transformation on the rectangle *top-left-s,
top-left-y* and *bottom-right-x, bottom-right-y,* returning the original
box. This is exactly equivalent to:

transform-box(invert-transform(*transform*))
                                             

untransform-distance
--------------------

Generic function
''''''''''''''''

Summary
       

Undoes the previous transformation on the distance *dx,dy*, returning
the original *dx,dy*.

Signature
         

*untransform-distance* *transform* *dx* *dy* => *dx* *dy*

Arguments
         

-  *transform* An instance of type `<transform>`_.
-  *dx* An instance of type *<real>*.
-  *dy* An instance of type *<real>*.

Values
      

-  *dx* An instance of type *<real>*.
-  *dy* An instance of type *<real>*.

Exceptions
          

*<singular-transform>* cannot be inverted.

Description
           

Undoes the previous transformation on the distance *dx,dy*, returning
the original *dx,dy*. This is exactly equivalent to:

transform-position(invert-transform(*transform*))
                                                  

untransform-position
--------------------

Generic function
''''''''''''''''

Summary
       

Undoes the previous transformation on the point *x,y*, returning the
original point.

Signature
         

*untransform-position* *transform* *x* *y* => *x* *y*

Arguments
         

-  *transform* An instance of type `<transform>`_.
-  *x* An instance of type *<real>*.
-  *y* An instance of type *<real>*.

Values
      

-  *x* An instance of type *<real>*.
-  *y* An instance of type *<real>*.

Exceptions
          

*<singular-transform>* cannot be inverted.

Description
           

Undoes the previous transformation on the point *x,y*, returning the
original point. This is exactly equivalent to:

transform-position(invert-transform(*transform*))
                                                  

untransform-region
------------------

Generic function
''''''''''''''''

Summary
       

Undoes the previous transformation on a region, returning the original
region.

Signature
         

*untransform-region* *transform* *region2* => *region1*

Arguments
         

-  *transform* An instance of type `<transform>`_.
-  *region2* An instance of type *<region>*. The region to untransform.

Values
      

-  *region1* An instance of type *<region>*. The original region.

Exceptions
          

*<singular-transform>* cannot be inverted.

Description
           

Undoes the previous transformation on the region *region*, returning
the original region. This is exactly equivalent to

transform-region(invert-transform(*transform region*))
                                                       


