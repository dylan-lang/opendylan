*********************
DUIM-Geometry Library
*********************

.. current-library:: duim-geometry
.. current-module:: duim-geometry

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
and ``<transform>``, both of which are subclasses of :drm:`<object>`. While
the ``<region>`` class has a number of subclasses, ``<transform>`` has no
direct subclasses.

-  ``<transform>`` The superclass of all transforms. A transform describes
   the mapping of one set of points onto another. There are one or more
   subclasses of :class:`<transform>` that implement
   transforms. These subclasses have implementation-dependent names
   which are explicitly unspecified. All of the instantiable
   transformation classes provided by DUIM are immutable.

In addition, there are a number of error classes which may be signalled.
These are all subclasses of :drm:`<error>`.

The <region> class and its subclasses
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The DUIM-Geometry library exposes the <region> class and its subclasses as
shown in the following table . None of these subclasses have any further
subclasses exposed in the DUIM-Geometry library, although the
DUIM-Extended-Geometry library exposes some subclasses of *<area>* and
*<path>*.

+----------+----------------+
| <region> |                |
+----------+----------------+
|          | <region-set>   |
+----------+----------------+
|          | <point>        |
+----------+----------------+
|          | <path>         |
+----------+----------------+
|          | <area>         |
+----------+----------------+
|          | <bounding-box> |
+----------+----------------+

-  :class:`<region>` This class is used to represent any set of points.
   The:class:`<region>` class includes both bounded regions (that is, regions
   whose edges are known) and unbounded regions (that is, regions with
   no known edges).
-  :class:`<region-set>` This class represents a region set, that is, a set of
   regions.
-  :class:`<point>` This class is used to represent mathematical points (that
   is, regions with dimensionality 0).
-  :class:`<path>` The class *<path>* denotes bounded regions with a length,
   but no area (that is, they have dimensionality 1).
-  :class:`<area>` This class denotes bounded regions that have an area (that
   is, they have dimensionality 2).
-  :class:`<bounding-box>` A bounding box is an axis aligned rectangle that
   contains some region.

Error classes provided by DUIM-Geometry
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The DUIM-Geometry library exposes a number of errors that can be signalled in
certain circumstances. They are shown in the following table . All the errors
shown are subclasses of the *<error>* class. Note that the subclasses of
*<transform-error>* are all specific to particular errors.

+-------------------+----------------------------+------------------------------+
| <transform-error> |                            |                              |
+-------------------+----------------------------+------------------------------+
|                   | <transform-underspecified> |                              |
+-------------------+----------------------------+------------------------------+
|                   |                            | <reflection-<underspecified> |
+-------------------+----------------------------+------------------------------+
|                   | <singular-transform>       |                              |
+-------------------+----------------------------+------------------------------+

-  :class:`<transform-error>` The superclass of all error conditions signalled
   when there is an error with a transform.
-  :class:`<transform-underspecified>` The error that is signalled when
   :func:`make-3-point-transform` is given three colinear image points.
-  :class:`<reflection-underspecified>` The error that is signalled when
   :func:`make-reflection-transform` is given two coincident points.
-  :class:`<singular-transform>` The error that is signalled when
   :func:`invert-transform` is called on a singular transform, that is,
   a transform that has no inverse.

DUIM-Geometry Module
====================

This section contains a complete reference of all the interfaces that
are exported from the *duim-geometry* module.

.. method:: =
   :specializer: <region>

   Tests if its arguments are equal.

   :signature: = *region1 region2* => *boolean*

   :parameter region1: An instance of type :class:`<region>`.
   :parameter region2: An instance of type :class:`<region>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Tests if its arguments are equal. Returns ``#t`` if the two regions
     are the same, otherwise returns ``#f``. Two regions are considered
     equal if they contain exactly the same set of points.

.. method:: =
   :specializer: <transform>

   Tests if its arguments are equal.

   :signature: = *transform1 transform2* => *boolean*

   :parameter transform1: An instance of type :class:`<transform>`.
   :parameter transform2: An instance of type :class:`<transform>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Tests if its arguments are equal. Returns ``#t`` if the two
     transforms are the same, otherwise returns ``#f``. Two transforms
     are considered equal if they transform every region the same way.

.. class:: <area>
   :open:
   :abstract:

   The class ``<area>`` denotes bounded regions that have dimensionality
   2 (that is, have area).

   :superclasses: :class:`<region>`

   :description:

     The class ``<area>`` denotes bounded regions that have
     dimensionality 2 (that is, have area). ``<area>`` is a subclass of
     :class:`<region>`.

     Note that constructing an area object with no area (such as calling
     :func:`make-rectangle` with two coincident points, for example) may
     canonicalize it to :const:`$nowhere`.

   :operations:

     The following operation is exported from the *DUIM-Geometry* module.

     - :gf:`area?`

   See also

   - :gf:`area?`

.. generic-function:: area?

   Returns ``#t`` if its argument is an area, otherwise returns ``#f``.

   :signature: area? *object* => *boolean*

   :parameter object: An instance of type :drm:`<object>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if *object* is an area, otherwise returns ``#f``.

   See also

   - :class:`<area>`

.. class:: <bounding-box>
   :open:
   :abstract:
   :instantiable:

   The class that represents a bounding box.

   :superclasses: :class:`<region>`

   :keyword left: An instance of type ``<integer>``.
   :keyword top: An instance of type ``<integer>``.
   :keyword right: An instance of type ``<integer>``.
   :keyword bottom: An instance of type ``<integer>``.

   :description:

     A bounding box is an axis aligned rectangle that contains some
     region. The representation of bounding boxes in DUIM is chosen to
     be efficient. This representation is not sufficient to represent
     the result of arbitrary transformations (such as rotations) of
     bounding boxes. The most general class of transformations that is
     guaranteed to transform a box into another box is the class of
     transformations that satisfy :gf:`rectilinear-transformation?`.

     Bounding boxes are immutable, but since they reflect the live state
     of such mutable objects as sheets, bounding boxes are volatile.
     Therefore, programmers must not depend on the bounding box
     associated with a mutable object remaining constant.

   :operations:

     The following operations are exported from the *DUIM-Geometry* module.

     - :gf:`bounding-box?`
     - :gf:`box-edges`
     - :gf:`region-contains-position?`
     - :gf:`region-contains-region?`
     - :gf:`region-difference`
     - :gf:`region-empty?`
     - :gf:`region-intersection`
     - :gf:`region-intersects-region?`
     - :gf:`region-union`
     - :gf:`set-box-edges`
     - :gf:`set-box-position`
     - :gf:`set-box-size`
     - :gf:`transform-region`
     - :gf:`untransform-region`

   See also

   - :gf:`bounding-box?`
   - :gf:`bounding-box`
   - :gf:`box-edges`

.. generic-function:: bounding-box?

   Returns true if its argument is a bounding box.

   :signature: bounding-box? *object* => *boolean*

   :parameter object: An instance of type :drm:`<object>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if *object* is a bounding box (that is, supports the
     bounding box protocol), otherwise returns ``#f``.

   See also

   - :class:`<bounding-box>`
   - :gf:`bounding-box`
   - :gf:`box-edges`

.. generic-function:: bounding-box

   Returns the bounding box of a region.

   :signature: bounding-box *region* *#key* *into* => *box*

   :parameter region: An instance of type :class:`<region>`.
   :parameter into: An instance of type ``false-or(<bounding-box>)``.
   :value box: An instance of type :class:`<bounding-box>`.

   :description:

     The argument *region* must be either a bounded region (such as a
     line or an ellipse) or some other object that obeys the bounding
     box protocol, such as a sheet.

     This function often returns an existing object, so you should not
     modify the returned result.

     If *into* is supplied, it is a bounding box that might be
     destructively modified to contain the result.

   See also

   - :class:`<bounding-box>`
   - :gf:`bounding-box?`
   - :gf:`box-edges`

.. function:: box-bottom

   Returns the *y* coordinate of the bottom right corner of the bounding
   box of a region.

   :signature: box-bottom *region* => *bottom*

   :parameter region: An instance of type :class:`<region>`.
   :value bottom: An instance of type ``<integer>``.

   :description:

     Returns the *y* coordinate of the bottom right corner of the
     bounding box of *region*. The argument *region* must be either a
     bounded region or some other object that obeys the bounding box
     protocol.

   See also

   - :func:`box-left`
   - :func:`box-right`
   - :func:`box-top`

.. generic-function:: box-edges

   Returns the bounding box of a region.

   :signature: box-edges *region* => *left* *top* *right* *bottom*

   :parameter region: An instance of type :class:`<region>`.
   :value left: An instance of type ``<integer>``.
   :value top: An instance of type ``<integer>``.
   :value right: An instance of type ``<integer>``.
   :value bottom: An instance of type ``<integer>``.

   :description:

     Returns the bounding box of *region* as four integers specifying
     the *x* and *y* coordinates of the top left point and the *x* and
     *y* coordinates of the bottom right point of the box

     The argument *region* must be either a bounded region (such as a
     line or an ellipse) or some other object that obeys the bounding
     box protocol, such as a sheet.

     The four returned values *left, top, right*, and *bottom* will
     satisfy the inequalities::

       *left* <= *right*
       *top* <= *bottom*

   See also

   - :class:`<bounding-box>`
   - :gf:`bounding-box?`
   - :gf:`bounding-box`

.. function:: box-height

   Returns the height of the bounding box of a region.

   :signature: box-height *region* => *height*

   :parameter region: An instance of type :class:`<region>`.
   :value height: An instance of type ``<integer>``.

   :description:

     Returns the height of the bounding box *region*. The height of a
     bounding box is the difference between the maximum *y* coordinate
     and its minimum *y* coordinate. The argument *region* must be
     either a bounded region or some other object that obeys the
     bounding box protocol.

   See also

   - :gf:`box-position`
   - :gf:`box-size`
   - :func:`box-width`

.. function:: box-left

   Returns the *x* coordinate of the upper left corner of the bounding
   box of a region.

   :signature: box-left *region* => *left*

   :parameter region: An instance of type :class:`<region>`.
   :value left: An instance of type ``<integer>``.

   :description:

     Returns the *x* coordinate of the upper left corner of the bounding
     box *region*. The argument *region* must be either a bounded region
     or some other object that obeys the bounding box protocol, such as
     a sheet.

   See also

   - :func:`box-bottom`
   - :func:`box-right`
   - :func:`box-top`

.. generic-function:: box-position

   Returns the position of the bounding box of a region as two values.

   :signature: box-position *region* => *x* *y*

   :parameter region: An instance of type :class:`<region>`.
   :value x: An instance of type <integer>.
   :value y: An instance of type <integer>.

   :description:

     Returns the position of the bounding box of *region* as two values.
     The position of a bounding box is specified by its top left point.

   See also

   - :func:`box-height`
   - :gf:`box-size`
   - :func:`box-width`

.. function:: box-right

   Returns the *x* coordinate of the bottom right corner of the bounding
   box of a region.

   :signature: box-right *region* => *right*

   :parameter region: An instance of type :class:`<region>`.
   :value right: An instance of type ``<integer>``.

   :description:

     Returns the *x* coordinate of the bottom right corner of the
     bounding box *region*. The argument *region* must be either a
     bounded region or some other object that obeys the bounding box
     protocol, such as a sheet.

   See also

   - :func:`box-bottom`
   - :func:`box-left`
   - :func:`box-top`

.. generic-function:: box-size

   Returns the width and height of the bounding box of a region as two
   values

   :signature: box-size *region* => *width* *height*

   :parameter region: An instance of type :class:`<region>`.
   :value width: An instance of type ``<integer>``.
   :value height: An instance of type ``<integer>``.

   :description:

     Returns the width and height of the bounding box of *region* as two
     values The argument *region* must be either a bounded region or
     some other object that obeys the bounding box protocol, such as a
     sheet.

   See also

   - :func:`box-height`
   - :gf:`box-position`
   - :func:`box-width`

.. function:: box-top

   Returns the *y* coordinate of the upper left corner of the bounding
   box of a region.

   :signature: box-top *region* => *top*

   :parameter region: An instance of type :class:`<region>`.
   :value top: An instance of type ``<integer>``.

   :description:

     Returns the *y* coordinate of the upper left corner of the bounding
     box *region*. The argument *region* must be either a bounded region
     or some other object that obeys the bounding box protocol.

   See also

   - :func:`box-bottom`
   - :func:`box-left`
   - :func:`box-right`

.. function:: box-width

   Returns the width of the bounding box of a region.

   :signature: box-width *region* => *width*

   :parameter region: An instance of type :class:`<region>`.
   :value width: An instance of type ``<integer>``.

   :description:

     Returns the width of the bounding box *region*. The width of a
     bounding box is the difference between its maximum *x* coordinate
     (right) and its minimum *x* coordinate (left).The argument *region*
     must be either a bounded region or some other object that obeys the
     bounding box protocol, such as a sheet.

   See also

   - :gf:`box-height`
   - :gf:`box-position`
   - :gf:`box-size`

.. generic-function:: compose-rotation-with-transform

   Creates a new transform by composing a transform with the given rotation

   :signature: compose-rotation-with-transform *transform* *angle* *#key* *origin* => *transform*

   :parameter transform: An instance of type :class:`<transform>`.
   :parameter angle: An instance of type ``<real>``.
   :parameter #key origin: An instance of type :class:`<point>`. Default
     value: (0, 0).
   :value transform: An instance of type :class:`<transform>`.

   :description:

     Creates a new transform by composing the transform *transform* with
     the given rotation The order of composition is that the rotation
     transform is applied first, followed by the argument *transform*.

     Note that this function could be implemented by using
     :func:`make-rotation-transform` and :gf:`compose-transforms`. It is
     provided because it is common to build up a transform as a series
     of simple transforms.

   See also

   - :func:`make-rotation-transform`

.. generic-function:: compose-scaling-with-transform

   Creates a new transform by composing a transform with the given scaling.

   :signature: compose-scaling-with-transform *transform* *scale-x* *scale-y* #key *origin* => *transform*

   :parameter transform: An instance of type :class:`<transform>`.
   :parameter scale-x: An instance of type ``<real>``.
   :parameter scale-y: An instance of type ``<real>``.
   :parameter #key origin: An instance of type :class:`<point>`. Default
     value: (0, 0).
   :value transform: An instance of type :class:`<transform>`.

   :description:

     Creates a new transform by composing the transform *transform* with
     the given scaling. The order of composition is that the scaling
     transform is applied first, followed by the argument *transform*.

     The argument *scale-x* represents the scaling factor for the *x*
     direction.

     The argument *scale-y* represents the scaling factor for the *y*
     direction.

     The argument *origin* represents the point around which scaling is
     performed. The default is to scale around the origin.

     Note that this function could be implemented by using
     :func:`make-scaling-transform` and :gf:`compose-transforms`. It is
     provided because it is common to build up a transform as a series
     of simple transforms.

   See also

   - :func:`make-scaling-transform`

.. generic-function:: compose-transforms

   Returns a transform that is the mathematical composition of its
   arguments.

   :signature: compose-transforms *transform1* *transform2* => *transform*

   :parameter transform1: An instance of type :class:`<transform>`.
   :parameter transform2: An instance of type :class:`<transform>`.
   :value transform: An instance of type :class:`<transform>`.

   :description:

     Returns a transform that is the mathematical composition of its
     arguments. Composition is in right-to-left order, that is, the
     resulting transform represents the effects of applying the
     transform *transform2* followed by the transform *transform1*.

   See also

   - :gf:`compose-transform-with-rotation`

.. generic-function:: compose-transform-with-rotation

   Creates a new transform by composing a given rotation with a transform.

   :signature: compose-transform-with-rotation *transform* *angle* #key *origin* => *transform*

   :parameter transform: An instance of type :class:`<transform>`.
   :parameter angle: An instance of type ``<real>``.
   :parameter #key origin: An instance of type :class:`<point>`. Default
     value: (0,0).
   :value transform: An instance of type :class:`<transform>`.

   :description:

     Creates a new transform by composing a given rotation with the
     transform *transform.* The order of composition is *transform*
     first, followed by the rotation transform.

     The argument *angle* represents the angle by which to rotate, in
     radians.

     The argument *origin* represents the point about which to rotate.
     The default is to rotate around (0,0).

     Note that this function could be implemented by using
     :func:`make-rotation-transform` and :gf:`compose-transforms`. It is
     provided because it is common to build up a transform as a series
     of simple transforms.

   See also

   - :gf:`compose-transforms`
   - :func:`make-rotation-transform`

.. generic-function:: compose-transform-with-scaling

   Creates a new transform by composing a given scaling with a transform.

   :signature: compose-transform-with-scaling *transform* *scale-x* *scale-y* *#key* *origin* => *transform*

   :parameter transform: An instance of type :class:`<transform>`.
   :parameter scale-x: An instance of type ``<real>``.
   :parameter scale-y: An instance of type ``<real>``.
   :parameter #key origin: An instance of type :class:`<point>`. Default
     value: (0,0).
   :value transform: An instance of type :class:`<transform>`.

   :description:

     Creates a new transform by composing a given scaling with the
     transform *transform.* The order of composition is *transform*
     first, followed by the scaling transform.

     The argument *scale-x* represents the scaling factor for the *x*
     direction.

     The argument *scale-y* represents the scaling factor for the *y*
     direction.

     The argument *origin* represents the point around which scaling is
     performed. The default is to scale around the origin.

     Note that this function could be implemented by using
     :func:`make-scaling-transform` and :gf:`compose-transforms`. It is
     provided because it is common to build up a transform as a series
     of simple transforms.

   See also

   - :gf:`compose-transforms`
   - :func:`make-scaling-transform`

.. generic-function:: compose-transform-with-translation

   Creates a new transform by composing a given translation with a
   transform.

   :signature: compose-transform-with-translation *transform* *dx* *dy* => *transform*

   :parameter transform: An instance of type :class:`<transform>`.
   :parameter dx: An instance of type ``<real>``.
   :parameter dy: An instance of type ``<real>``.
   :value transform: An instance of type :class:`<transform>`.

   :description:

     Creates a new transform by composing a given translation with the
     transform *transform*. The order of composition is *transform*
     first, followed by the translation transform.

     The argument *dx* represents the *delta* by which to translate the
     *x* coordinate.

     The argument *dy* represents the *delta* by which to translate the
     *y* coordinate.

     Note that this function could be implemented by using
     :func:`make-translation-transform` and :gf:`compose-transforms`. It
     is provided because it is common to build up a transform as a
     series of simple transforms.

   See also

   - :func:`make-translation-transform`
   - :gf:`compose-transforms`

.. generic-function:: compose-translation-with-transform

   Creates a new transform by composing a transform with the given
   translation.

   :signature: compose-translation-with-transform *transform* *dx* *dy* => *transform*

   :parameter transform: An instance of type :class:`<transform>`.
   :parameter dx: An instance of type ``<real>``.
   :parameter dy: An instance of type ``<real>``.
   :value transform: An instance of type :class:`<transform>`.

   :description:

     Creates a new transform by composing the transform *transform* with
     the given translation. The order of composition is that the
     translation transform is applied first, followed by the argument
     *transform*.

     The argument *dx* represents the *delta* by which to translate the
     *x* coordinate.

     The argument *dy* represents the *delta* by which to translate the
     *y* coordinate.

     Note that this function could be implemented by using
     :func:`make-translation-transform` and :gf:`compose-transforms`. It
     is provided, because it is common to build up a transform as a
     series of simple transforms.

   See also

   - :func:`make-translation-transform`
   - :gf:`compose-transforms`

.. function:: do-coordinates

   Applies a function to each coordinate pair in its argument list.

   :signature: do-coordinates *function* *coordinates* => ()

   :parameter function: An instance of type ``<function>``.
   :parameter coordinates: An instance of type ``limited(<sequence>, of: <real>)``.

   :description:

     Applies *function* to each coordinate pair in *coordinates*. The
     length of *coordinates* must be a multiple of 2. *Function* takes
     two arguments, the *x* and *y* value of each coordinate pair.

.. function:: do-endpoint-coordinates

   Applies a function to each coordinate pair in its argument list.

   :signature: do-endpoint-coordinates *function* *coordinates* => ()

   :parameter function: An instance of type ``<function>``.
   :parameter coordinates: An instance of type ``limited(<sequence>, of: <real>)``.

   :description:

     Applies *function* to each pair of coordinate pairs in
     *coordinates*. The arguments *coordinates* represents a set of line
     segments rather than a set of points: The length of this sequence
     must therefore be a multiple of 4. Function takes 4 arguments,
     (*x1*, *y1*, *x2*, *y2*).

.. generic-function:: do-regions

   Calls a function on each region in a set of regions.

   :signature: do-regions *function* *region* *#key* *normalize?* => ()

   :parameter function: An instance of type ``<function>``.
   :parameter region: An instance of type :class:`<region>`.
   :parameter #key normalize?: An instance of type ``<boolean>``. Default value: ``#f``.

   :description:

     Calls *function* on each region in the region set *region.* This is
     often more efficient than calling *region-set-regions*. *function*
     is a function of one argument, a region. *Region* can be either a
     region set or a simple region, in which case *function* is called
     once on *region* itself. If *normalize* is supplied, it must be
     either ``#"x-banding"`` or ``#"y-banding"``. If it is
     ``#"x-banding"`` and all the regions in *region* are axis-aligned
     rectangles, the result is normalized by merging adjacent rectangles
     with banding done in the *x* direction. If it is ``#"y-banding"``
     and all the regions in *region* are rectangles, the result is
     normalized with banding done in the *y* direction. Normalizing a
     region set that is not composed entirely of axis-aligned rectangles
     using x- or y-banding causes DUIM to signal the
     :class:`<region-set-not-rectangular>` error.

.. generic-function:: even-scaling-transform?

   Returns ``#t`` if the transform *transform* multiplies all *x*
   lengths and *y* lengths by the same magnitude, otherwise returns
   ``#f``.

   :signature: even-scaling-transform? *transform* => *boolean*

   :parameter transform: An instance of type :class:`<transform>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if the transform *transform* multiplies all *x*
     lengths and *y* lengths by the same magnitude, otherwise returns
     ``#f``. ``even-scaling-transform?`` includes pure reflections
     through vertical and horizontal lines.

.. constant:: $everywhere

   The region that includes all the points on the two-dimensional
   infinite drawing plane.

   :type: :class:`<region>`

   :description:

     The region that includes all the points on the two-dimensional
     infinite drawing plane.

   See also

   - :const:`$nowhere`

.. function:: fix-coordinate

   Coerces the given coordinate into an *<integer>*.

   :signature: fix-coordinate *coordinate* => *integer*

   :parameter coordinate: An instance of type ``<real>``.
   :value integer: An instance of type ``<integer>``.

   :description:

     Coerces the given coordinate into an ``<integer>``.

.. constant:: $identity-transform

   An instance of a transform that is guaranteed to be an identity
   transform, that is, the transform that does nothing.

   :type: :class:`<transform>`

   :description:

     An instance of a transform that is guaranteed to be an identity
     transform, that is, the transform that does nothing.

   See also

   - :gf:`identity-transform?`

.. generic-function:: identity-transform?

   Returns ``#t`` if a transform is equal (in the sense of *transform-equal*) to the identity transform.

   :signature: identity-transform? *transform* => *boolean*

   :parameter transform: An instance of type :class:`<transform>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if the transform *transform* is equal (in the sense
     of *transform-equal*) to the identity transform, otherwise returns
     ``#f``.

   See also

   - :const:`$identity-transform`

.. generic-function:: invert-transform

   Returns a transform that is the inverse of the given transform.

   :signature: invert-transform *transform* => *transform*

   :parameter transform: An instance of type :class:`<transform>`.
   :value transform: An instance of type :class:`<transform>`.

   :conditions:

     If *transform* is singular, ``invert-transform`` signals the
     :class:`<singular-transform>` error.

     .. note:: With finite-precision arithmetic there are several
       low-level conditions that might occur during the attempt to invert
       a singular or *almost* singular transform. (These include
       computation of a zero determinant, floating-point underflow during
       computation of the determinant, or floating-point overflow during
       subsequent multiplication.) ``invert-transform`` signals the
       :class:`<singular-transform>` error for all of these cases.

   :description:

     Returns a transform that is the inverse of the transform
     *transform*. The result of composing a transform with its inverse
     is equal to the identity transform.

   See also

   - :gf:`invertible-transform?`

.. generic-function:: invertible-transform?

   Returns ``#t`` if the given transform has an inverse.

   :signature: invertible-transform? *transform* => *boolean*

   :parameter transform: An instance of type :class:`<transform>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if the transform *transform* has an inverse,
     otherwise returns ``#f``.

   See also

   - :gf:`invert-transform`

.. constant:: $largest-coordinate

   The largest valid coordinate.

   :type: <integer>

   :description:

     The largest valid coordinate.

   See also

   - :const:`$smallest-coordinate`

.. function:: make-3-point-transform

   Returns a transform that takes points *point-1* into *point-1-image*,
   *point-2* into *point-2-image* and *point-3* into *point-3-image*.

   :signature: make-3-point-transform *x1* *y1* *x2* *y2* *x3* *y3* *x1-image* *y1-image* *x2-image* *y2-image* *x3-image* *y3-image* => *transform*

   :signature: make-3-point-transform\* *point-1 point-2 point-3 point-1-image point-2-image point-3-image* => *transform*

   The following arguments are specific to ``make-3-point-transform``.

   :parameter x1: An instance of type ``<real>``.
   :parameter y1: An instance of type ``<real>``.
   :parameter x2: An instance of type ``<real>``.
   :parameter y2: An instance of type ``<real>``.
   :parameter x3: An instance of type ``<real>``.
   :parameter y3: An instance of type ``<real>``.
   :parameter x1-image: An instance of type ``<real>``.
   :parameter y1-image: An instance of type ``<real>``.
   :parameter x2-image: An instance of type ``<real>``.
   :parameter y2-image: An instance of type ``<real>``.
   :parameter x3-image: An instance of type ``<real>``.
   :parameter y3-image: An instance of type ``<real>``.

   The following arguments are specific to ``make-3-point-transform*``.

   :parameter point-1: An instance of type :class:`<point>`.
   :parameter point-2: An instance of type :class:`<point>`.
   :parameter point-3: An instance of type :class:`<point>`.
   :parameter point-1-image: An instance of type :class:`<point>`.
   :parameter point-2-image: An instance of type :class:`<point>`.
   :parameter point-3-image: An instance of type :class:`<point>`.

   :value transform: An instance of type :class:`<transform>`.

   :conditions:

     If *point-1*, *point-2* and *point-3* are colinear, the
     :class:`<transform-underspecified>` error is signalled. If
     *point-1-image*,*point-2-image* and *point-3-image* are colinear, the
     resulting transform will be singular (that is, will have no inverse) but
     this is not an error.

   :description:

     Returns a transform that takes points *point-1* into *point-1-image*,
     *point-2* into *point-2-image* and *point-3* into *point-3-image*.
     Three non-colinear points and their images under the transform are
     enough to specify any affine transformation.

     The function ``make-3-point-transform*`` is identical to
     ``make-3-point-transform``, except that it passes composite objects,
     rather than separate coordinates, in its arguments. You should be aware
     that using this function may lead to a loss of performance.

.. function:: make-bounding-box

   Returns an object of the class :class:`<bounding-box>`.

   :signature: make-bounding-box *x1* *y1* *x2* *y2* => *box*

   :parameter x1: An instance of type ``<real>``.
   :parameter y1: An instance of type ``<real>``.
   :parameter x2: An instance of type ``<real>``.
   :parameter y2: An instance of type ``<real>``.
   :value box: An instance of type :class:`<bounding-box>`.

   :description:

     Returns an object of the class :class:`<bounding-box>` with the
     edges specified by *x1, y1, x2*, and *y2. x1, y1, x2*, and *y2* are
     canonicalized in the following way. The min point of the box has an
     *x* coordinate that is the smaller of *x1* and *x2* and a *y*
     coordinate that is the smaller of *y1* and *y2*. The max point of
     the box has an *x* coordinate that is the larger of *x1* and *x2*
     and a *y* coordinate that is the larger of *y1* and *y2*.
     (Therefore, in a right-handed coordinate system the canonicalized
     values of *x1, y1, x2,* and *y2* correspond to the left, top,
     right, and bottom edges of the box, respectively.)

     This is a convenient shorthand function for ``make(<bounding-box>,
     left: top: right: bottom:)``.

.. function:: make-point

   Returns an object of class :class:`<point>`.

   :signature: make-point *x* *y* => *point*

   :parameter x: An instance of ``<real>``.
   :parameter y: An instance of ``<real>``.
   :value point: An instance of type :class:`<point>`.

   :description:

     Returns an object of class :class:`<point>` whose coordinates are
     *x* and *y*.

.. function:: make-reflection-transform

   Returns a transform that reflects every point through the line
   passing through the positions *x1,y1* and *x2,y2*.

   :signature: make-reflection-transform *x1* *y1* *x2* *y2* => *transform*

   :parameter x1: An instance of type ``<real>``.
   :parameter y1: An instance of type ``<real>``.
   :parameter x2: An instance of type ``<real>``.
   :parameter y2: An instance of type ``<real>``.
   :value transform: An instance of type :class:`<transform>`. The
     resultant transformation.

   :description:

     Returns a transform that reflects every point through the line
     passing through the positions *x1,y1* and *x2,y2*.

     The arguments *x1* and *y1* represent the coordinates of the first
     point of reflection. The arguments *x2* and *y2* represent the
     coordinates of the second point of reflection.

     A reflection is a transform that preserves lengths and magnitudes
     of angles, but changes the sign (or handedness) of angles. If you
     think of the drawing plane on a transparent sheet of paper, a
     reflection is a transformation that turns the paper over.

   See also

   - :func:`make-rotation-transform`
   - :func:`make-scaling-transform`
   - :func:`make-transform`
   - :func:`make-translation-transform`
   - :class:`<reflection-underspecified>`

.. function:: make-reflection-transform

   Returns a transform that reflects every point through the line
   passing through the positions *x1,y1* and *x2,y2* or through the
   points *point1* and *point2*.

   :signature: make-reflection-transform\* *point-1* *point-2* => *transform*

   :parameter point1: An instance of type :class:`<point>`. The
     first point.
   :parameter point2: An instance of type :class:`<point>`. The
     second point.
   :value transform: An instance of type :class:`<transform>`.
     The resultant transformation.

   :description:

     Returns a transform that reflects every point through the line
     passing through the points *point1* and *point2*.

     A reflection is a transform that preserves lengths and magnitudes
     of angles, but changes the sign (or handedness) of angles. If you
     think of the drawing plane on a transparent sheet of paper, a
     reflection is a transformation that turns the paper over.

     The function ``make-reflection-transform*`` is identical to
     :func:make-reflection-transform, except that it passes composite
     objects, rather than separate coordinates, in its arguments. You
     should be aware that using this function may lead to a loss of
     performance.

   See also

   - :func:`make-rotation-transform`
   - :func:`make-scaling-transform`
   - :func:`make-transform`
   - :func:`make-translation-transform`
   - :class:`<reflection-underspecified>`

.. function:: make-rotation-transform

   Returns a transform that rotates all points by *angle* around the point
   specified by coordinates *origin-x* and *origin-y* or the point object
   *origin*.

   :signature: make-rotation-transform *angle* *#key* *origin-x* *origin-y* => *transform*

   :signature: make-rotation-transform\* *angle* #key *origin* => *transform*

   :parameter angle: An instance of type ``<real>``.

   The following arguments are specific to ``make-rotation-transform``.

   :parameter origin-x: An instance of type ``<real>``. Default value: 0.
   :parameter origin-y: An instance of type ``<real>``. Default value: 0.

   The following argument is specific to ``make-reflection-transform*``.

   :parameter origin: An instance of type :class:`<point>`. Default value: (0, 0).

   :value transform: An instance of type :class:`<transform>`.

   :description:

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

   - :func:`make-reflection-transform`
   - :func:`make-scaling-transform`
   - :func:`make-transform`
   - :func:`make-translation-transform`

.. function:: make-scaling-transform

   Returns a transform that multiplies the *x* -coordinate distance of
   every point from *origin* by *scale-x* and the *y* -coordinate distance
   of every point from *origin* by *scale-y*.

   :signature: make-scaling-transform *scale-x* *scale-y* #key *origin-x* *origin-y* => *transform*

   :signature: make-scaling-transform\* *scale-x* *scale-y* #key *origin* => *transform*

   :parameter scale-x: An instance of type ``<real>``.
   :parameter scale-y: An instance of type ``<real>``.

   The following arguments are specific to ``make-scaling-transform``.

   :parameter origin-x: An instance of type ``<real>``. Default value: 0.
   :parameter origin-y: An instance of type ``<real>``. Default value: 0.

   The following argument is specific to ``make-scaling-transform*``.

   :parameter origin: An instance of type :class:`<point>`.

   :value transform: An instance of type :class:`<transform>`. The resultant transformation.

   :description:

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

   - :func:`make-reflection-transform`
   - :func:`make-rotation-transform`
   - :func:`make-transform`
   - :func:`make-translation-transform`

.. function:: make-transform

   Returns a general affine transform.

   :signature: make-transform *mxx* *mxy* *myx* *myy* *tx* *ty* => *transform*

   :parameter mxx: An instance of type ``<real>``.
   :parameter mxy: An instance of type ``<real>``.
   :parameter myx: An instance of type ``<real>``.
   :parameter myy: An instance of type ``<real>``.
   :parameter tx: An instance of type ``<real>``.
   :parameter ty: An instance of type ``<real>``.
   :value transform: An instance of type :class:`<transform>`.

   :description:

     Returns a general transform whose effect is::

       x'= *mxx* x + *mxy* y + *tx*
       y'= *myx* x + *myy* y + *ty*

     where *x* and *y* are the coordinates of a point before the transform
     and *x'* and *y'* are the coordinates of the corresponding point after.

     All of the arguments to ``make-transform`` must be real numbers.

     This is a convenient shorthand for ``make(<transform>, ...)``.

   See also

   - :func:`make-reflection-transform`
   - :func:`make-rotation-transform`
   - :func:`make-scaling-transform`
   - :func:`make-translation-transform`

.. function:: make-translation-transform

   Returns a transform that translates all points by *dx* in the *x*
   direction and *dy* in the *y* direction.

   :signature: make-translation-transform *dx* *dy* => *transform*

   :parameter dx: An instance of type ``<real>``.
   :parameter dy: An instance of type ``<real>``.
   :value transform: An instance of type :class:`<transform>`.

   :description:

     Returns a transform that translates all points by *dx* in the *x*
     direction and *dy* in the *y* direction.

     The argument *dx* represents the *delta* by which to translate the
     *x* coordinate.

     The argument *dy* represents the *delta* by which to translate the
     *y* coordinate.

     A translation is a transform that preserves length, angle, and
     orientation of all geometric entities.

   See also

   - :func:`make-reflection-transform`
   - :func:`make-rotation-transform`
   - :func:`make-scaling-transform`
   - :func:`make-transform`

.. constant:: $nowhere

   The empty region, the opposite of :const:`$everywhere`.

   :type: :class:`<region>`

   :description:

     The empty region, the opposite of :const:`$everywhere`.

   See also

   - :const:`$everywhere`

.. class:: <path>
   :open:
   :abstract:

   The class ``<path>`` denotes bounded regions that have dimensionality
   1 (that is, have length).

   :superclasses: :class:`<region>`

   :description:

     The class ``<path>`` denotes bounded regions that have
     dimensionality 1 (that is, have length).

     ``<path>`` is a subclass of :class:`<region>`.

     Constructing a ``<path>`` object with no length (via
     :func:`make-line*`, for example) may canonicalize it to
     :const:`$nowhere`.

   :operations:

     The following operation is exported from the *DUIM-Geometry* module.

     - :gf:`path?`

   See also

   - :gf:`path?`

.. generic-function:: path?

   Returns ``#t`` if its argument is a path.

   :signature: path? *object* => *boolean*

   :parameter object: An instance of type :drm:`<object>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if *object* is a path, otherwise returns ``#f``.

   See also

   - :class:`<path>`

.. class:: <point>
   :open:
   :abstract:
   :instantiable:

   The class that corresponds to a mathematical point.

   :superclasses: :class:`<region>`

   :keyword x: An instance of type ``<integer>``.
   :keyword y: An instance of type ``<integer>``.

   :description:

     The class that corresponds to a mathematical point. ``<point>`` is
     a subclass of :class:`<region>`. The ``x:`` and ``y:``
     init-keywords correspond to the x and y coordinates, respectively.

   :operations:

     The following operations are exported from the *DUIM-Geometry* module.

     - :gf:`=`
     - :gf:`box-edges`
     - :gf:`point?`
     - :gf:`point-position`
     - :gf:`point-x`
     - :gf:`point-y`
     - :gf:`region-contains-position?`
     - :gf:`region-contains-region?`
     - :gf:`region-intersection`
     - :gf:`region-intersects-region?`
     - :gf:`transform-region`

.. generic-function:: point?

   Returns true if *object* is a point.

   :signature: point? *object* => *boolean*

   :parameter object: An instance of type :drm:`<object>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if *object* is a point.

.. generic-function:: point-position

   Returns both the *x* and *y* coordinates of a point.

   :signature: point-position *point* => *x* *y*

   :parameter point: An instance of type :class:`<point>`.
   :value x: An instance of type ``<real>``.
   :value y: An instance of type ``<real>``.

   :description:

     Returns both the *x* and *y* coordinates of the point *point* as
     two values.

   See also

   - :gf:`point-x`
   - :gf:`point-y`

.. generic-function:: point-x

   Returns the *x* coordinate of a point.

   :signature: point-x *point* => *x*

   :parameter point: An instance of type :class:`<point>`.
   :value x: An instance of type ``<real>``.

   :description:

     Returns the *x* coordinate of *point*.

   See also

   - :gf:`point-position`
   - :gf:`point-y`

.. generic-function:: point-y

   Returns the *y* coordinate of a point.

   :signature: point-y *point* => *y*

   :parameter point: An instance of type :class:`<point>`.
   :value y: An instance of type ``<real>``

   :description:

     Returns the *y* coordinate of *point*.

   See also

   - :gf:`point-position`
   - :gf:`point-x`

.. generic-function:: rectilinear-transform?

   Returns ``#t`` if a transform always transforms any axis-aligned
   rectangle into another axis-aligned rectangle.

   :signature: rectilinear-transform? *transform* => *boolean*

   :parameter transform: An instance of type :class:`<transform>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if the transform *transform* always transforms any
     axis-aligned rectangle into another axis-aligned rectangle,
     otherwise returns ``#f``.

     This category includes scalings as a subset, and also includes 90
     degree rotations.

     Rectilinear transforms are the most general category of transforms
     for which the bounding rectangle of a transformed object can be
     found by transforming the bounding rectangle of the original
     object.

.. generic-function:: reflection-transform?

   Returns ``#t`` if the transform inverts the *handedness* of the
   coordinate system.

   :signature: reflection-transform? *transform* => *boolean*

   :parameter transform: An instance of type :class:`<transform>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if the transform *transform* inverts the
     *handedness* of the coordinate system, otherwise returns *#f.*

     Note that this is a very inclusive category — transforms are
     considered reflections even if they distort, scale, or skew the
     coordinate system, as long as they invert the handedness.

.. class:: <reflection-underspecified>
   :sealed:
   :concrete:

   The error that is signalled when :func:`make-reflection-transform` is
   given two coincident points.

   :superclasses: <transform-underspecified>

   :keyword points: Instances of type :class:`<point>`.

   :description:

     The error that is signalled when :func:`make-reflection-transform`
     is given two coincident points. This condition handles the
     ``points:`` initarg, which is used to supply the points that are in
     error.

   See also

   - :func:`make-reflection-transform`

.. class:: <region>
   :open:
   :abstract:

   The class that corresponds to a set of points.

   :superclasses: <object>

   :description:

     The class that corresponds to a set of points. The:class:`<region>`
     class includes both bounded and unbounded regions.

     There is no :drm:`make` method for :class:`<region>` because of the
     impossibility of a uniform way to specify the arguments to such a
     function.

   :operations:

     The following operations are exported from the *DUIM-Geometry* module.

     - :gf:`=`
     - :gf:`do-regions`
     - :gf:`region?`
     - :gf:`region-contains-position?`
     - :gf:`region-contains-region?`
     - :gf:`region-difference`
     - :gf:`region-empty?`
     - :gf:`region-equal`
     - :gf:`region-intersection`
     - :gf:`region-intersects-region?`
     - :gf:`region-set-function`
     - :gf:`region-set-regions`
     - :gf:`region-union`

   See also

   - :gf:`region?`

.. generic-function:: region?

   Returns ``#t`` if its argument is a region.

   :signature: region? *object* => *boolean*

   :parameter object: An instance of type :drm:`<object>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if *object* is a region, otherwise returns``#f``.

   See also

   - :class:`<region>`

.. generic-function:: region-contains-position?

   Returns ``#t`` if the point at *x,y* is contained in the region.

   :signature: region-contains-position? *region* *x* *y* => *boolean*

   :parameter region: An instance of type :class:`<region>`.
   :parameter x: An instance of type ``<real>``.
   :parameter y: An instance of type ``<real>``.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if the point at *x,y* is contained in the region
     *region*, otherwise returns ``#f``. Since regions in DUIM are
     closed, this returns ``#t`` if the point at *x,y* is on the
     region's boundary.

   See also

   - :gf:`region-contains-region?`

.. generic-function:: region-contains-region?

   Returns ``#t`` if all points in the second region are members of the
   first region.

   :signature: region-contains-region? *region1* *region2* => *boolean*

   :parameter region1: An instance of type :class:`<region>`.
   :parameter region2: An instance of type :class:`<region>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if all points in the region *region2* are members of
     the region *region1*, otherwise returns ``#f``.
     :gf:`region-contains-position?` is a special case of
     :gf:`region-contains-region?` in which the region is the point
     *x,y*.

   See also

   - :gf:`region-contains-position?`

.. generic-function:: region-difference

   Returns a region that contains all points in the region *region1*
   that are not in the region *region2* (possibly plus additional
   boundary points to make the result closed).

   :signature: region-difference *region1* *region2* => *region*

   :parameter region1: An instance of type :class:`<region>`.
   :parameter region2: An instance of type :class:`<region>`.
   :value region: An instance of type :class:`<region>`.

   :description:

     Returns a region that contains all points in the region *region1*
     that are not in the region *region2* (possibly plus additional
     boundary points to make the result closed).

     The result of ``region-difference`` has the same dimensionality as
     *region1*, or is :const:`$nowhere`. For example, the difference of
     an area and a path produces the same area; the difference of a path
     and an area produces the path clipped to stay outside of the area.

     .. note:: ``region-difference`` may return either a simple region
        or a region set.

.. generic-function:: region-empty?

   Returns ``#t`` if the region is empty.

   :signature: region-empty? *region* => *boolean*

   :parameter region: An instance of type :class:`<region>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if the region is empty, otherwise returns ``#f``.

.. generic-function:: region-equal

   Returns ``#t`` if the two regions *region1* and *region2* contain
   exactly the same set of points.

   :signature: region-equal *region1* *region2* => *boolean*

   :parameter region1: An instance of type :class:`<region>`.
   :parameter region2: An instance of type :class:`<region>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if the two regions *region1* and *region2* contain
     exactly the same set of points, otherwise returns ``#f``. There is
     a method on :gf:`=` on :class:`<region>` and :class:`<region>` that
     calls ``region-equal``.

.. generic-function:: region-intersection

   Returns the intersection of two regions, as a region.

   :signature: region-intersection *region1* *region2* => *region*

   :parameter region1: An instance of type :class:`<region>`.
   :parameter region2: An instance of type :class:`<region>`.
   :value region: An instance of type :class:`<region>`.

   :description:

     Returns a region that contains all points that are in both of the
     regions *region1* and *region2* (possibly with some points removed
     in order to satisfy the dimensionality rule).

     The result of ``region-intersection`` has dimensionality that is
     the minimum dimensionality of *region1* and *region2*, or is
     :const:`$nowhere`. For example, the intersection of two areas is
     either another area or :const:`$nowhere`; the intersection of two
     paths is either another path or :const:`$nowhere`; the intersection
     of a path and an area produces the path clipped to stay inside of
     the area.

     .. note:: ``region-intersection`` may return either a simple region
        or a region set.

   See also

   - :gf:`region-union`

.. generic-function:: region-intersects-region?

   Returns ``#f`` if two regions do not intersect*.*

   :signature: region-intersects-region? *region1* *region2* => *boolean*

   :parameter region1: An instance of type :class:`<region>`.
   :parameter region2: An instance of type :class:`<region>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#f`` if :gf:`region-intersection` of the two regions
     *region1* and *region2* would be :const:`$nowhere` (that is, they
     do not intersect), otherwise returns *#t.*

.. class:: <region-set>
   :open:
   :abstract:

   The class that represents a region set.

   :superclasses: :class:`<region>`

   :description:

     The class that represents a region set; a subclass of
     :class:`<region>`.

   :operations:

     The following operations are exported from the *DUIM-Geometry* module.

     - :gf:`box-edges`
     - :gf:`do-regions`
     - :gf:`region-contains-position?`
     - :gf:`region-contains-region?`
     - :gf:`region-difference`
     - :gf:`region-empty?`
     - :gf:`region-intersection`
     - :gf:`region-set-function`
     - :gf:`region-set-regions`
     - :gf:`region-union`
     - :gf:`transform-region`

   See also

   - :gf:`region-set?`

.. generic-function:: region-set?

   Returns ``#t`` if its argument is a region set.

   :signature: region-set? *object* => *boolean*

   :parameter object: An instance of type :drm:`<object>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if *object* is a region set, otherwise returns
     ``#f``.

   See also

   - :class:`<region-set>`

.. generic-function:: region-set-function

   Returns the function that composed the region.

   :signature: region-set-function *region* => *function*

   :parameter region: An instance of type :class:`<region>`.
   :value function: An instance of type ``<function>``.

   :description:

     Returns the function that composed the region,
     :gf:`region-intersection`, :gf:`region-union`, or
     :gf:`region-difference`.

.. generic-function:: region-set-regions

   Returns a sequence of the regions in the region set.

   :signature: region-set-regions *region* #key *normalize?* => *regions*

   :parameter region: An instance of type :class:`<region>`.
   :parameter normalize?: ``one-of(#f, #"x-banding", #"y-banding")``.
     Default value: ``#f``.
   :value regions: An instance of type ``limited(<sequence>, of: <region>)``.

   :conditions:

     Normalizing a region set that is not composed entirely of axis-aligned
     rectangles using x- or y-banding causes DUIM to signal the
     :class:`<region-set-not-rectangular>` error.

   :description:

     Returns a sequence of the regions in the region set *region*.
     *region* can be either a region set or a simple region, in which
     case the result is simply a sequence of one element: region.

     For the case of region sets that are unions of axis-aligned
     rectangles, the rectangles returned by ``region-set-regions`` are
     guaranteed not to overlap. If *normalize?* is supplied, it must be
     either ``#"x-banding"`` or ``#"y-banding"``. If it is
     ``#"x-banding"`` and all the regions in *region* are axis-aligned
     rectangles, the result is normalized by merging adjacent rectangles
     with banding done in the *x* direction. If it is ``#"y-banding"``
     and all the regions in *region* are rectangles, the result is
     normalized with banding done in the *y* direction.

.. generic-function:: region-union

   Returns the union of two regions, as a region.

   :signature: region-union *region1* *region2* => *region*

   :parameter region1: An instance of type :class:`<region>`.
   :parameter region2: An instance of type :class:`<region>`.
   :value region: An instance of type :class:`<region>`.

   :description:

     Returns a region that contains all points that are in either of the
     regions *region1* or *region2* (possibly with some points removed
     in order to satisfy the dimensionality rule)

     The result of ``region-union`` always has dimensionality that is
     the maximum dimensionality of *region1* and *region2*. For example,
     the union of a path and an area produces an area; the union of two
     paths is a path.

     .. note:: *region-union* may return either a simple region
       or a region set.

   See also

   - :gf:`region-intersection`

.. generic-function:: rigid-transform?

   Returns ``#t`` if the *transform* transforms the coordinate system as
   a rigid object.

   :signature: rigid-transform? *transform* => *boolean*

   :parameter transform: An instance of type :class:`<transform>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if the *transform* transforms the coordinate system
     as a rigid object, that is, as a combination of translations,
     rotations, and pure reflections. Otherwise, it returns ``#f``.

     Rigid transforms are the most general category of transforms that
     preserve magnitudes of all lengths and angles.

.. generic-function:: scaling-transform?

   Returns ``#t`` if the transform *transform* multiplies all *x*
   lengths by one magnitude and all *y* lengths by another magnitude,
   otherwise returns ``#f``.

   :signature: scaling-transform? *transform* => *boolean*

   :parameter transform: An instance of type :class:`<transform>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if the transform *transform* multiplies all *x*
     lengths by one magnitude and all *y* lengths by another magnitude,
     otherwise returns ``#f``. This category includes even scalings as a
     subset.

.. generic-function:: set-box-edges

   Sets the edges of a box and returns the bounding box.

   :signature: set-box-edges *box* *left* *top* *right* *bottom* => *box*

   :parameter box: An instance of type :class:`<bounding-box>`.
   :parameter left: An instance of type ``<integer>``.
   :parameter top: An instance of type ``<integer>``.
   :parameter right: An instance of type ``<integer>``.
   :parameter bottom: An instance of type ``<integer>``.
   :value box: An instance of type :class:`<bounding-box>`.

   :description:

     Sets the edges of a box and returns the bounding box *box*. This
     might destructively modify *box* or it might not, depending on what
     class *box* is.

.. generic-function:: set-box-position

   Sets the position of the bounding box and returns a (possibly new)
   box.

   :signature: set-box-position *box* *x* *y* => *box*

   :parameter box: An instance of type :class:`<bounding-box>`.
   :parameter x: An instance of type ``<real>``.
   :parameter y: An instance of type ``<real>``.
   :value box: An instance of type :class:`<bounding-box>`.

   :description:

     Sets the position of the bounding box *box* and might or might not
     modify the box.

.. generic-function:: set-box-size

   Sets the size (width and height) of the bounding box *box*.

   :signature: set-box-size *box* *width* *height* => *box*

   :parameter box: An instance of type :class:`<bounding-box>`.
   :parameter width: An instance of type ``<integer>``.
   :parameter height: An instance of type ``<integer>``
   :value box: An instance of type :class:`<bounding-box>`.

   :description:

     Sets the size (width and height) of the bounding box *box*.

.. class:: <singular-transform>
   :sealed:
   :instantiable:

   The error that is signalled when :gf:`invert-transform` is called on
   a singular transform, that is, a transform that has no inverse.

   :superclasses: <transform-error>

   :keyword transform: Used to supply the transform that is singular.

   :description:

     The error that is signalled when :gf:`invert-transform` is called
     on a singular transform, that is, a transform that has no inverse.

     This condition handles the ``transform:`` initarg, which is used to
     supply the transform that is singular.

   See also

   - :gf:`invert-transform`

.. constant:: $smallest-coordinate

   The smallest valid coordinate.

   :type: <integer>

   :description:

     The smallest valid coordinate. Coordinates must be instances of type
     ``<integer>``.

   See also

   - :const:`$largest-coordinate`

.. class:: <transform>
   :open:
   :abstract:
   :instantiable:

   The superclass of all transforms.

   :superclasses: <object>

   :keyword mxx: An instance of type ``<real>``.
   :keyword mxy: An instance of type ``<real>``.
   :keyword myx: An instance of type ``<real>``.
   :keyword myy: An instance of type ``<real>``.
   :keyword tx: An instance of type ``<real>``.
   :keyword ty: An instance of type ``<real>``.

   :description:

     The superclass of all transforms. There are one or more subclasses
     of :class:`<transform>` with implementation-dependent names that
     implement transforms. The exact names of these classes is
     explicitly unspecified.
     All of the instantiable transformation classes provided by DUIM
     are immutable.

   :operations:

     The following operations are exported from the *DUIM-Geometry* module.

     - :gf:`=`
     - :gf:`compose-rotation-with-transform`
     - :gf:`compose-scaling-with-transform`
     - :gf:`compose-transforms`
     - :gf:`compose-transform-with-translation`
     - :gf:`compose-translation-with-transform`
     - :gf:`even-scaling-transform?`
     - :gf:`identity-transform?`
     - :gf:`invert-transform`
     - :gf:`invertible-transform?`
     - :gf:`rectilinear-transform?`
     - :gf:`reflection-transform?`
     - :gf:`rigid-transform?`
     - :gf:`scaling-transform?`
     - :gf:`transform-angles`
     - :gf:`transform-box`
     - :gf:`transform-distance`
     - :gf:`transform-position`
     - :gf:`transform-region`
     - :gf:`translation-transform?`
     - :gf:`untransform-angles`
     - :gf:`untransform-box`
     - :gf:`untransform-distance`
     - :gf:`untransform-position`
     - :gf:`untransform-region`

   See also

   - :gf:`transform?`

.. generic-function:: transform?

   Returns ``#t`` if its argument is a transform.

   :signature: transform? *object* => *boolean*

   :parameter object: An instance of type :drm:`<object>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if *object* is a transform, otherwise returns ``#f``.

   See also

   - :class:`<transform>`

.. generic-function:: transform-angles

   Applies the transform to the start and end angles of an object, and
   returns the transformed angles.

   :signature: transform-angles *transform* *start-angle* *end-angle* => *new-start* *new-end*

   :parameter transform: An instance of type :class:`<transform>`.
   :parameter start-angle: An instance of type ``<real>``.
   :parameter end-angle: An instance of type ``<real>``.
   :value new-start: An instance of type ``<real>``.
   :value new-end: An instance of type ``<real>``.

   :description:

     Applies the transform *transform* to the angles *start-angle* and
     *end-angle* of an object, and returns the transformed angles.

.. generic-function:: transform-box

   Applies the transform to the rectangle specified by the four
   coordinate arguments.

   :signature: transform-box *transform* *x1* *y1* *x2* *y2* => *left top* *right bottom*

   :parameter transform: An instance of type :class:`<transform>`.
   :parameter x1: An instance of type ``<real>``.
   :parameter y1: An instance of type ``<real>``.
   :parameter x2: An instance of type ``<real>``.
   :parameter y2: An instance of type ``<real>``.
   :value left: An instance of type ``<real>``.
   :value top: An instance of type ``<real>``.
   :value right: An instance of type ``<real>``.
   :value bottom: An instance of type ``<real>``.

   :description:

     Applies the transform *transform* to the rectangle specified by the
     four coordinate arguments. *transform-box* is the spread version of
     :gf:`transform-region` in the case where the transform is
     rectilinear and the region is a rectangle.

     The arguments *x1*, *y1*, *x2*, and *y2* are canonicalized and the
     four return values specify the minimum and maximum points of the
     transformed rectangle in the order *left*, *top*, *right*, and
     *bottom*.

     An error is signalled if *transform* does not satisfy
     :gf:`rectilinear-transform?`.

.. generic-function:: transform-distance

   Applies a transform to a distance represented by the coordinate
   arguments and returns the transformed coordinates.

   :signature: transform-distance *transform* *dx* *dy* => *dx* *dy*


   :parameter transform: An instance of type :class:`<transform>`.
   :parameter dx: An instance of type ``<real>``.
   :parameter dy: An instance of type ``<real>``.
   :value dx: An instance of type ``<real>``.
   :value dy: An instance of type ``<real>``.

   :description:

     Applies the transform *transform* to the distance represented by
     *dx* and *dy*, and returns the transformed *dx* and *dy*. A
     distance represents the difference between two points. It does not
     transform like a point.

.. class:: <transform-error>
   :sealed:

   The superclass of all error conditions distributed when there is an
   error with a transform.

   :superclasses: <error>

   :description:

     The class that is the superclass of three error conditions,
     :class:`<transform-underspecified>`,
     :class:`<reflection-underspecified>`, and
     :class:`<singular-transform>`.

.. generic-function:: transform-position

   Applies a transform to the point whose coordinates are *x* and *y*.

   :signature: transform-position *transform* *x* *y* => new-*x* new-*y*

   :parameter transform: An instance of type :class:`<transform>`.
   :parameter x: An instance of type ``<real>``
   :parameter y: An instance of type ``<real>``
   :value new-x: An instance of type ``<real>``
   :value new-y: An instance of type ``<real>``

   :description:

     Applies the transform *transform* to the point whose coordinates
     are *x* and *y*. ``transform-position`` is the *spread* version of
     :gf:`transform-region` in the case where the region is a point.

.. generic-function:: transform-region

   Applies a transform to a region, and returns the transformed region.

   :signature: transform-region *transform* *region* => *region*

   :parameter transform: An instance of type :class:`<transform>`.
   :parameter region: An instance of type :class:`<region>`.
   :value region: An instance of type :class:`<region>`.

   :description:

     Applies *transform* to the region *region*, and returns the transformed
     region.

.. class:: <transform-underspecified>
   :sealed:
   :concrete:

   The error that is signalled when :func:`make-3-point-transform` is
   given three colinear image points.

   :superclasses: <transform-error>

   :keyword points: The points that are in error.

   :description:

     The error that is signalled when :func:`make-3-point-transform` is
     given three colinear image points. This condition handles the
     ``points:`` initarg, which is used to supply the points that are in
     error.

   See also

   - :func:`make-3-point-transform`

.. generic-function:: translation-transform?

   Returns ``#t`` if a transform is a pure translation, that is, a transform
   such that there are two distance components transform *dx* and *dy* and
   every point *(x,y)* is moved to *(x+dx,y+dy)*.

   :signature: translation-transform? *transform* => *boolean*

   :parameter transform: An instance of type :class:`<transform>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if the transform *transform* is a pure translation, that
     is, a transform such that there are two distance components transform
     *dx* and *dy* and every point *(x,y)* is moved to *(x+dx,y+dy)*.
     Otherwise, *translation-transform?* returns ``#f``.

.. generic-function:: untransform-angles

   Undoes a transform and returns the original start and end angles of
   the object.

   :signature: untransform-angles *transform* *start-angle* *end-angle* => *orig-start* *orig-end*

   :parameter transform: An instance of type :class:`<transform>`.
   :parameter start-angle: An instance of type ``<real>``.
   :parameter end-angle: An instance of type ``<real>``.
   :value orig-start: An instance of type ``<real>``.
   :value orig-end: An instance of type ``<real>``.

   :conditions:

     - :class:`<singular-transform>` cannot be inverted.

   :description:

     Undoes the transform *transform* to the angles *new-start* and*new-end,*
     returning the original *orig-start* and *orig-end.* This is exactly
     equivalent to:

     .. code-block:: dylan

       transform-angles(invert-transform(*transform*))

.. generic-function:: untransform-box

   Undoes the previous transformation on the rectangle *left, top* and
   *right, bottom,* returning the original box.

   :signature: untransform-box *transform x1 y1 x2 y2* => *left top right bottom*

   :parameter transform: An instance of type :class:`<transform>`.
   :parameter x1: An instance of type ``<real>``.
   :parameter y1: An instance of type ``<real>``.
   :parameter x2: An instance of type ``<real>``.
   :parameter y2: An instance of type ``<real>``.
   :value left: An instance of type ``<real>``.
   :value top: An instance of type ``<real>``.
   :value right: An instance of type ``<real>``.
   :value bottom: An instance of type ``<real>``.

   :conditions:
     - :class:`<singular-transform>` cannot be inverted.

   :description:

     Undoes the previous transformation on the rectangle *top-left-s,
     top-left-y* and *bottom-right-x, bottom-right-y,* returning the original
     box. This is exactly equivalent to:

     .. code-block:: dylan

       transform-box(invert-transform(*transform*))

.. generic-function:: untransform-distance

   Undoes the previous transformation on the distance *dx,dy*, returning
   the original *dx,dy*.

   :signature: untransform-distance *transform* *dx* *dy* => *dx* *dy*

   :parameter transform: An instance of type :class:`<transform>`.
   :parameter dx: An instance of type ``<real>``.
   :parameter dy: An instance of type ``<real>``.
   :value dx: An instance of type ``<real>``.
   :value dy: An instance of type ``<real>``.

   :conditions:

     - :class:`<singular-transform>` cannot be inverted.

   :description:

     Undoes the previous transformation on the distance *dx,dy*, returning
     the original *dx,dy*. This is exactly equivalent to:

     .. code-block:: dylan

       transform-position(invert-transform(*transform*))

.. generic-function:: untransform-position

   Undoes the previous transformation on the point *x,y*, returning the
   original point.

   :signature: untransform-position *transform* *x* *y* => *x* *y*

   :parameter transform* An instance of type :class:`<transform>`.
   :parameter x: An instance of type ``<real>``.
   :parameter y: An instance of type ``<real>``.
   :value x: An instance of type ``<real>``.
   :value y: An instance of type ``<real>``.

   :conditions:
     - :class:`<singular-transform>` cannot be inverted.

   :description:

     Undoes the previous transformation on the point *x,y*, returning
     the original point. This is exactly equivalent to:

     .. code-block:: dylan

       transform-position(invert-transform(*transform*))

.. generic-function:: untransform-region

   Undoes the previous transformation on a region, returning the
   original region.

   :signature: untransform-region *transform* *region2* => *region1*

   :parameter transform: An instance of type :class:`<transform>`.
   :parameter region2: An instance of type :class:`<region>`. The region to untransform.
   :value region1: An instance of type :class:`<region>`. The original region.

   :conditions:

   - :class:`<singular-transform>` cannot be inverted.

   :description:

     Undoes the previous transformation on the region *region*,
     returning the original region. This is exactly equivalent to

     .. code-block:: dylan

       transform-region(invert-transform(*transform region*))
