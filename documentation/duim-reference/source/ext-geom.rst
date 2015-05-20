******************************
DUIM-Extended-Geometry Library
******************************

.. current-library:: duim-extended-geometry
.. current-module:: duim-extended-geometry

Overview
========

The DUIM-Extended-Geometry library builds on the features provided by
the DUIM-Geometry library, and provides more extensive support for
coordinate geometry that is only required for more specialist uses. The
library contains a single module, *duim-extended-geometry*, from which
all the interfaces described in this chapter are exposed. `See
DUIM-Extended-Geometry Module`_ contains complete
reference entries for each exposed interface.

The class hierarchy for DUIM-Extended-Geometry
==============================================

The DUIM-Extended-Geometry library defines no base classes itself, but
instead subclasses two classes exposed in the DUIM-Geometry library:
:class:`<area>` and :class:`<path>`. In each case, these subclasses provide more
specialized geometrical tools.

Subclasses of <area>
^^^^^^^^^^^^^^^^^^^^

Three subclasses of :class:`<area>` are exposed in the DUIM-Extended-Geometry
library, each of which provides the ability to create instances of
particular shapes. Their usage is relatively obvious.

-  :class:`<rectangle>` This class is used to create rectangular shapes on a
   drawable object.
-  :class:`<ellipse>` This class is used to create elliptical shapes on a
   drawable object.
-  :class:`<polygon>` This class is used to create more general polygon shapes
   on a drawable object.

Subclass of <path>
^^^^^^^^^^^^^^^^^^

Three subclasses of :class:`<path>` are exposed in the DUIM-Extended-Geometry
library, each of which provides the ability to create instances of
particular types of line. Their usage is relatively obvious.

-  :class:`<line>` This class is used to create straight lines between two
   points on a drawable object.
-  :class:`<elliptical-arc>` This class is used to create elliptical arcs
   (portions of an ellipse) on a drawable object.
-  :class:`<polyline>` This class is used to create lines that pass through an
   arbitrary set of coordinates. It produces a jagged line with vertices
   at each coordinate.

DUIM-Extended-Geometry Module
=============================

This section contains a complete reference of all the interfaces that
are exported from the *duim-extended-geometry* module.

.. generic-function:: do-polygon-coordinates

   Applies a function to all of the coordinates of the vertices of a
   polygon.

   :signature: do-polygon-coordinates *function* *polygon* => ()

   :parameter function: An instance of type ``<function>``.
   :parameter polygon: An instance of type :class:`<polygon>`.


   :description:

     Applies *function* to all of the coordinates of the vertices of
     *polygon*. *function* is a function of two arguments, the *x* and *y*
     coordinates of the vertex. *do-polygon-coordinates* returns ``#f``.

   :seealso:

     - :gf:`do-polygon-segments`

.. generic-function:: do-polygon-segments

   Applies a function to the segments that compose a polygon.

   :signature: do-polygon-segments *function* *polygon* => ()

   :parameter function: An instance of type ``<function>``.
   :parameter polygon: An instance of type :class:`<polygon>`.

   :description:

     Applies *function* to the segments that compose *polygon*. *function*
     is a function of four arguments, the *x* and *y* coordinates of the
     start of the segment, and the *x* and *y* coordinates of the end of the
     segment. When *do-polygon-segments* is called on a closed polyline, it
     calls *function* on the segment that connects the last point back to the
     first point.

     The function *do-polygon-segments* returns ``#f``.

   :seealso:

     - :gf:`do-polygon-coordinates`

.. generic-function:: draw-design

   Draws a design on a drawing surface.

   :signature: draw-design *drawable* *design* => ()

   :parameter drawable: An instance of type *type-union(<sheet>, <medium>)*.
   :parameter design: A :class:`<region>` to draw.

   :description:

     Draws *design* on the sheet medium *drawable*.

.. class:: <ellipse>
   :abstract:
   :instantiable:

   The class that corresponds to an ellipse.

   :superclasses: :class:`<area>`

   :keyword center-x: An instance of type ``<real>``.
   :keyword center-y: An instance of type ``<real>``.
   :keyword center-point: An instance of type :class:`<point>`.
   :keyword radius-1-dx: An instance of type ``<real>``
   :keyword radius-1-dy: An instance of type ``<real>``
   :keyword radius-2-dx: An instance of type ``<real>``
   :keyword radius-2-dy: An instance of type ``<real>``
   :keyword start-angle: An instance of ``false-or(<real>)``.
   :keyword end-angle: An instance of ``false-or(<real>)``.

   :description:

     An *ellipse* is an area that is the outline and interior of an ellipse.
     Circles are special cases of ellipses.

     The *center-x:* init-keyword specifies the *x* coordinate of the center
     of the ellipse.

     The *center-y:* init-keyword specifies the *y* coordinate of the center
     of the ellipse.

     The *center-point:* init-keyword specifies the center of the ellipse as
     a point.

     An ellipse is specified in a manner that is easy to transform, and
     treats all ellipses on an equal basis. An ellipse is specified by its
     center point and two vectors that describe a bounding parallelogram of
     the ellipse.   y*c* -dx*1* + dx*2*

     Note that several different parallelograms specify the same ellipse. One
     parallelogram is bound to be a rectangle — the vectors will be
     perpendicular and correspond to the semi-axes of the ellipse.

   :operations:

     The following operations are exported from the *DUIM-Extended-Geometry*
     module.

     - :gf:`draw-design`
     - :gf:`ellipse?`
     - :gf:`ellipse-center-point`
     - :gf:`ellipse-center-position`
     - :gf:`ellipse-end-angle`
     - :gf:`ellipse-radii`
     - :gf:`ellipse-start-angle`

     The following operations are exported from the *DUIM-Geometry* module.

     - :gf:`box-edges`
     - :gf:`transform-region`

   :seealso:

     - :class:`<area>`
     - :gf:`make-ellipse`

.. generic-function:: ellipse?

   Returns ``#t`` if an object is an ellipse.

   :signature: ellipse? *object* => *boolean*

   :parameter object: An instance of type :drm:`<object>`.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if *object* is an ellipse, otherwise returns *#f.*

   See Also

     - :class:`<ellipse>`

.. generic-function:: ellipse-center-point

   Returns the center point of an ellipse or an elliptical arc.

   :signature: ellipse-center-point *elliptical-object* => *point*

   :parameter elliptical-object: An instance of type type-union(:class:`<ellipse>`, :class:`<elliptical-arc>`).

   :value point: An instance of type :class:`<point>`.

   :description:

     Returns the center point of *ellipse-object* as a :class:`<point>` object.

   :seealso:

     - :gf:`make-ellipse`

.. generic-function:: ellipse-center-position

   Returns the coordinates of the center point of an ellipse or an
   elliptical arc.

   :signature: ellipse-center-position* *elliptical-object* => *x* *y*

   :parameter elliptical-object: An instance of type type-union(:class:`<ellipse>`, :class:`<elliptical-arc>`).

   :value x: An instance of type ``<real>``.
   :value y: An instance of type ``<real>``.

   :description:

     Returns the coordinates of the center point of *elliptical-object*.

     The arguments *x* and *y* represent the x and y coordinates of the
     center of the elliptical object, respectively.

   :seealso:

     - :gf:`make-ellipse`

.. generic-function:: ellipse-end-angle

   Returns the end angle of an ellipse or an elliptical-object.

   :signature: ellipse-end-angle *elliptical-object* => *angle*

   :parameter elliptical-object: An instance of type ``type-union(<ellipse>, <elliptical-arc>)``.
   :value angle: An instance of type ``false-or(<real>)``.

   :description:

     Returns the end angle of *elliptical-object*. If *elliptical-object* is
     a full ellipse or closed path then *ellipse-end-angle* returns ``#f`` ;
     otherwise the value is a number greater than zero, and less than or
     equal to *2p*.

   :seealso:

     - :gf:`make-ellipse`

.. generic-function:: ellipse-radii

   Returns four values corresponding to the two radius vectors of an
   elliptical arc.

   :signature: ellipse-radii *elliptical-object* => *r1-dx* *r1-dy* *r2-dx* *d2-dy*

   :parameter elliptical-object: An instance of type ``type-union(<ellipse>, <elliptical-arc>)``.
   :value r1-dx: An instance of type ``<real>``.
   :value r1-dy: An instance of type ``<real>``.
   :value r2-dx: An instance of type ``<real>``.
   :value d2-dy: An instance of type ``<real>``.

   :description:

     Returns four values corresponding to the two radius vectors of
     *elliptical-object*. These values may be canonicalized in some way, and
     so may not be the same as the values passed to the constructor function.

   :seealso:

     - :gf:`make-ellipse`

.. generic-function:: ellipse-start-angle

   Returns the start angle of an elliptical arc.

   :signature: ellipse-start-angle *elliptical-object* => *angle*

   :parameter elliptical-object: An instance of type ``type-union(<ellipse>, <elliptical-arc>)``.
   :value angle: An instance of type ``false-or(<real>)``.

   :description:

     Returns the start angle of *elliptical-object*. If *elliptical-object* is
     a full ellipse or closed path then *ellipse-start-angle* returns ``#f``;
     otherwise the value will be a number greater than or equal to zero, and
     less than *2p*.

   :seealso:

     - :gf:`make-ellipse`

.. class:: <elliptical-arc>
   :abstract:
   :instantiable:

   An *elliptical arc* is a path consisting of all or a portion of the
   outline of an ellipse.

   :superclasses: :class:`<path>`

   :keyword center-x: An instance of type ``<real>``.
   :keyword center-y: An instance of ``<real>``.
   :keyword center-point: An instance of type :class:`<point>`.
   :keyword radius-1-dx: An instance of ``<real>``.
   :keyword radius-1-dy: An instance of ``<real>``.
   :keyword radius-2-dx: An instance of ``<real>``.
   :keyword radius-2-dy: An instance of ``<real>``.
   :keyword start-angle: An instance of ``false-or(<real>)``.
   :keyword end-angle: An instance of ``false-or(<real>)``.

   :description:

     An *elliptical arc* is a path consisting of all or a portion of the
     outline of an ellipse. Circular arcs are special cases of elliptical
     arcs.

   :operations:

     The following operations are exported from the *DUIM-Extended-Geometry*
     module.

     - :gf:`draw-design`
     - :gf:`ellipse-center-point`
     - :gf:`ellipse-center-position`
     - :gf:`ellipse-end-angle`
     - :gf:`ellipse-radii`
     - :gf:`ellipse-start-angle`
     - :gf:`elliptical-arc?`

     The following operations are exported from the *DUIM-Geometry* module.

     - :gf:`box-edges`
     - :gf:`transform-region`

   :seealso:

     - :gf:`elliptical-arc?`
     - :gf:`make-elliptical-arc`

.. generic-function:: elliptical-arc?

   Returns ``#t`` if an object is an elliptical arc,

   :signature: elliptical-arc? *object* => *boolean*

   :parameter object: An instance of type :drm:`<object>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if *object* is an elliptical arc, otherwise returns ``#f``.

   :seealso:

     - :class:`<elliptical-arc>`

.. class:: <line>
   :abstract:
   :instantiable:

   The class that corresponds to a line.

   :superclasses: :class:`<path>`

   :keyword start-x: An instance of ``<real>``.
   :keyword start-y: An instance of ``<real>``.
   :keyword end-x: An instance of ``<real>``.
   :keyword end-y: An instance of ``<real>``.
   :keyword points: Instances of :class:`<point>`.

   :description:

     The class that corresponds to a line. This is a subclass of :class:`<path>`.

     This is the instantiable class that implements a line segment.
     :gf:`make-line` instantiates an object of type ``<line>``.

   :operations:

     The following operations are exported from the *DUIM-Extended-Geometry*
     module.

     - :gf:`do-polygon-coordinates`
     - :gf:`do-polygon-segments`
     - :gf:`draw-design`
     - :gf:`line?`
     - :gf:`line-end-point`
     - :gf:`line-end-position`
     - :gf:`line-start-point`
     - :gf:`line-start-position`
     - :gf:`polygon-coordinates`
     - :gf:`polygon-points`
     - :gf:`polyline-closed?`

     The following operations are exported from the *DUIM-Geometry* module.

     - :gf:`box-edges`
     - :gf:`transform-region`

   :seealso:

     - :class:`<path>`
     - :gf:`make-line`

.. generic-function:: line?

   Returns ``#t`` if an object is a line.

   :signature: line? *object* => *boolean*

   :parameter object: An instance of type :drm:`<object>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if *object* is a line, otherwise returns *#f.*

.. generic-function:: line-end-point

   Returns the ending point of a line.

   :signature: line-end-point *line* => *point*

   :parameter line: An instance of type :class:`<line>`.
   :value point: An instance of type :class:`<point>`.

   :description:

     Returns the ending point of *line* as a :class:`<point>` object.

   :seealso:

     - :gf:`line-start-point`

.. generic-function:: line-end-position

   Returns the ending point of a line.

   :signature: line-end-position *line* => *x y*

   :parameter line: An instance of type :class:`<line>`.
   :value x: An instance of type ``<real>``.
   :value y: An instance of type ``<real>``.

   :description:

     Returns two real numbers representing the *x* and *y* coordinates of the
     ending point of *line*.

     The arguments *x* and *y* represent the x and y coordinates of the end
     of the line, respectively.

   :seealso:

     - :gf:`line-start-position`

.. generic-function:: line-start-point

   Returns the starting point of a line.

   :signature: line-start-point *line* => *point*

   :parameter line: An instance of type :class:`<line>`.
   :value point: An instance of type :class:`<point>`.

   :description:

     Returns the starting point of *line* as a *<point>* object.

   :seealso:

     - :gf:`line-end-point`

.. generic-function:: line-start-position

   Returns the starting point of a line.

   :signature: line-start-position *line* => *x* *y*

   :parameter line: An instance of type :class:`<line>`.

   :value x: An instance of type ``<real>``.
   :value y: An instance of type ``<real>``.

   :description:

     Returns two real numbers representing the *x* and *y* coordinates of the
     starting point of *line*.

     The arguments *x* and *y* represent the x and y coordinates of the start
     of the line, respectively.

   :seealso:

     - :gf:`line-end-position`

.. function:: make-ellipse

   Returns an object of class :class:`<ellipse>`.

   :signature: make-ellipse *center-x* *center-y* *radius-1-dx* *radius-1-dy* *radius-2-dx* *radius-2-dy* #key *start-angle* *end-angle* => *ellipse*
   :signature: make-ellipse\* *center-point* *radius-1-dx* *radius-1-dy* *radius-2-dx* *radius-2-dy* #key *start-angle* *end-angle* => *ellipse*

   :parameter radius-1-dx: An instance of type ``<real>``.
   :parameter radius-1-dy: An instance of type ``<real>``.
   :parameter radius-2-dx: An instance of type ``<real>``.
   :parameter radius-2-dy: An instance of type ``<real>``.
   :parameter start-angle: An instance of type ``false-or(<real>)``.
   :parameter end-angle: An instance of type ``false-or(<real>)``.
    
   The following arguments are specific to *make-ellipse*.

   :parameter center-x: An instance of type ``<real>``.
   :parameter center-y: An instance of type ``<real>``.

   The following argument is specific to *make-ellipse*.

   :parameter center-point: An instance of type :class:`<point>`.

   :value ellipse: An instance of type :class:`<ellipse>`.

   :description:

     Returns an object of class *<ellipse>*. The center of the ellipse is at
     the position *center-x*,*center-y* or the point *center-point.*

     Two vectors, (*radius-1-dx,radius-1-dy*) and (*radius-2-dx,radius-2-dy*
     ) specify the bounding parallelogram of the ellipse. All of the radii
     are real numbers. If the two vectors are colinear, the ellipse is not
     well-defined and the *ellipse-not-well-defined* error is signalled. The
     special case of an ellipse with its axes aligned with the coordinate
     axes can be obtained by setting both *radius-1-dy* and *radius-2-dx* to
     0.

     If *start-angle* or *end-angle* are supplied, the ellipse is the *pie
     slice* area swept out by a line from the center of the ellipse to a
     point on the boundary as the boundary point moves from the angle
     *start-angle* to *end-angle*. Angles are measured counter-clockwise
     with respect to the positive *x* axis. If *end-angle* is supplied, the
     default for *start-angle* is *0* ; if *start-angle* is supplied, the
     default for *end-angle* is *2p* ; if neither is supplied then the region
     is a full ellipse and the angles are meaningless.

     The function *make-ellipse\** is identical to *make-ellipse*, except
     that it passes composite objects, rather than separate coordinates, in
     its arguments. You should be aware that using this function may lead to
     a loss of performance.

   :seealso:

     - :class:`<ellipse>`

.. function:: make-elliptical-arc

   Returns an object of class *<elliptical-arc>*.

   :signature: make-elliptical-arc *center-x* *center-y* *radius-1-dx* *radius-1-dy* *radius-2-dx* *radius-2-dy* #key *start-angle* *end-angle* => *arc*
   :signature: make-elliptical-arc\* *center-point* *radius-1-dx* *radius-1-dy* *radius-2-dx* *radius-2-dy* #key *start-angle* *end-angle* => *arc*

   :parameter radius-1-dx: An instance of type ``<real>``.
   :parameter radius-1-dy: An instance of type ``<real>``.
   :parameter radius-2-dx: An instance of type ``<real>``.
   :parameter radius-2-dy: An instance of type ``<real>``.
   :parameter start-angle: An instance of type ``false-or(<real>)``.
   :parameter end-angle: An instance of type ``false-or(<real>)``.

   The following arguments are specific to *make-elliptical-arc*.

   :parameter center-x: An instance of type ``<real>``.
   :parameter center-y: An instance of type ``<real>``.

   The following argument is specific to *make-elliptical-arc\**.

   :parameter center-point: An instance of type :class:`<point>`.

   :value arc: An instance of type :class:`<elliptical-arc>`.

   :description:

     Returns an object of class *<elliptical-arc>*. The center of the
     ellipse is at the position *center-x,center-y* or the point
     *center-point*

     Two vectors, (*radius-1-dx,radius-1-dy*) and (*radius-2-dx,radius-2-dy*
     ), specify the bounding parallelogram of the ellipse. All of the radii
     are real numbers. If the two vectors are colinear, the ellipse is not
     well-defined and the *ellipse-not-well-defined* error will be signalled.
     The special case of an elliptical arc with its axes aligned with the
     coordinate axes can be obtained by setting both *radius-1-dy* and
     *radius-2-dx* to *0*.

     If *start-angle* and *end-angle* are supplied, the arc is swept from
     *start-angle* to *end-angle*. Angles are measured counter-clockwise
     with respect to the positive *x* axis. If *end-angle* is supplied, the
     default for *start-angle* is *0* ; if *start-angle* is supplied, the
     default for *end-angle* is *2p* ; if neither is supplied then the region
     is a closed elliptical path and the angles are meaningless.

     The function *make-elliptical-arc\** is identical to
     *make-elliptical-arc*, except that it passes composite objects, rather
     than separate coordinates, in its arguments. You should be aware that
     using this function may lead to a loss of performance.

   :seealso:

     - :class:`<elliptical-arc>`

.. function:: make-line

   Returns an object of class :class:`<line>`.t

   :signature: make-line *start-x* *start-y* *end-x* *end-y* => *line*
   :signature: make-line\* *start-point* *end-point* => *line*

   :parameter start-x: An instance of type ``<real>``.
   :parameter start-y: An instance of type ``<real>``.
   :parameter end-x: An instance of type ``<real>``.
   :parameter end-y: An instance of type ``<real>``.
   :parameter start-point: An instance of type :class:`<point>`.
   :parameter end-point: An instance of type :class:`<point>`.

   :value line: An instance of type :class:`<line>`.

   :description:

     Returns an object of class :class:`<line>` that connects the two positions
     (*start-x,start-y*) and (e*nd-x,end-y*) or the two points
     *start-point* and *end-point*.

.. function:: make-polygon

   Returns an object of class :class:`<polygon>`.

   :signature: make-polygon *coord-seq* => *polygon*
   :signature: make-polygon\* *point-seq* => *polygon*

   The following argument is specific to ``make-polygon``.

   :parameter coord-seq: An instance of type *limited(<sequence>, of: <real>)*.

   The following argument is specific to ``make-polygon\*``.

   :parameter point-seq: An instance of type limited(``<sequence>``, of: :class:`<point>`).

   :value polygon: An instance of type :class:`<polygon>`.

   :description:

     Returns an object of class *<polygon>* consisting of the area contained
     in the boundary that is specified by the segments connecting each of the
     points in *point-seq* or the points represented by the coordinate pairs
     in *coord-seq*. *point-seq* is a sequence of points; *coord-seq* is a
     sequence of coordinate pairs, which are real numbers. It is an error if
     *coord-seq* does not contain an even number of elements.

     The function *make-polygon\** is identical to *make-polygon*, except
     that it passes composite objects, rather than separate coordinates, in
     its arguments. You should be aware that using this function may lead to
     a loss of performance.

.. function:: make-polyline

   Returns an object of class :class:`<polyline>`.

   :signature: make-polyline *coord-seq* #key *closed?* => *polyline*
   :signature: make-polyline\* *point-seq* #key *closed?* => *polyline*

   :parameter closed?: An instance of type ``<boolean>``. Default value: ``#f``.

   The following argument is specific to ``make-polyline``.

   :parameter coord-seq: An instance of type ``limited(<sequence>, of: <real>)``.

   The following argument is specific to ``make-polyline\*``.

   :parameter point-seq: An instance of type ``limited(<sequence>, of: <point>)``.

   :value polyline: An instance of type :class:`<polyline>`

   :description:

     Returns an object of class ``<polyline>`` consisting of the segments
     connecting each of the points in *point-seq* or the points represented
     by the coordinate pairs in *coord-seq*. *point-seq* is a sequence of
     points; *coord-seq* is a sequence of coordinate pairs, which are real
     numbers. It is an error if *coord-seq* does not contain an even number
     of elements.

     If *closed?* is ``#t``, then the segment connecting the first point and
     the last point is included in the polyline. The default for *closed?*
     is** ``#f``.

     The function ``make-polyline\*`` is identical to ``make-polyline``, except
     that it passes composite objects, rather than separate coordinates, in
     its arguments. You should be aware that using this function may lead to
     a loss of performance.

.. function:: make-rectangle

   Returns an object of class :class:`<rectangle>`.

   :signature: make-rectangle *x1* *y1* *x2* *y2* => *rectangle*
   :signature: make-rectangle\* *min-point* *max-point* => *rectangle*

   The following arguments are specific to ``make-rectangle``.

   :parameter x1: An instance of type ``<real>``. The *x* coordinate of the left top of the rectangle.
   :parameter y1: An instance of type ``<real>``. The *y* coordinate of the left top of the rectangle
   :parameter x2: An instance of type ``<real>``. The *x* coordinate of the bottom right of the rectangle.
   :parameter y2: An instance of type ``<real>``. The *y* coordinate of the bottom right of the rectangle.

   The following arguments are specific to ``make-rectangle\*``.

   :parameter min-point: The minimum point (left top) of the rectangle.
   :parameter max-point: The maximum point (bottom right) of the rectangle.

   :value rectangle: An instance of type :class:`<rectangle>`.

   :description:

     Returns an object of class :class:`<rectangle>` whose edges are parallel to the
     coordinate axes. One corner is at the point *point1* or the
     position*x1,y1* and the opposite corner is at the point *point2* or the
     position *x2,y2*. There are no ordering constraints among *point1* and
     *point2* or *x1* and *x2*, and *y1* and *y2*.

     The function *make-rectangle\** is identical to *make-rectangle*,
     except that it passes composite objects, rather than separate
     coordinates, in its arguments. You should be aware that using this
     function may lead to a loss of performance.

.. class:: <polygon>
   :abstract:
   :instantiable:

   The class that corresponds to a polygon.

   :superclasses: :class:`<area>`

   :keyword coordinates: An instance of type ``limited(<sequence>, of: <real>)``.
   :keyword points: An instance of type ``limited(<sequence>, of: <real>)``.

   :description:

     The class that corresponds to a polygon. This is a subclass of :class:`<area>`.

     A polygon can be described either in terms of the individual x and y
     coordinates that constitute its vertices, or by using composite points.
     If the former is used, then they can be specified at the time of
     creation using the *coordinates:* init-keyword, which is a sequence of
     real numbers, with x and y coordinates alternating within the sequence.

     To describe a polygon in terms of composite point objects, use the
     *points:* init-keyword, which is a sequence of instances of :class:`<point>`.
     You should be aware that using composite points may lead to a loss of
     performance.

     Exactly one of *coordinates:* and *points:* is required.

   :operations:

     The following operations are exported from the *DUIM-Extended-Geometry*
     module.

     - :gf:`do-polygon-coordinates`
     - :gf:`do-polygon-segments`
     - :gf:`draw-design`
     - :gf:`polygon?`
     - :gf:`polygon-coordinates`
     - :gf:`polygon-points`

     The following operations are exported from the *DUIM-Geometry* module.

     - :gf:`box-edges`
     - :gf:`transform-region`

   :seealso:

     - :class:`<area>`
     - :func:`make-polygon`
     - :gf:`polygon?`
     - :gf:`polygon-coordinates`
     - :gf:`polygon-points`

.. generic-function:: polygon?

   Returns ``#t`` if its argument is a polygon.

   :signature: polygon? *object* => *boolean*

   :parameter object: An instance of type :drm:`<object>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if *object* is a polygon, otherwise returns ``#f``.

   :seealso:

     - :class:`<polygon>`
     - :gf:`polygon-coordinates`
     - :gf:`polygon-points`

.. generic-function:: polygon-coordinates

   Returns a sequence of coordinate pairs that specify the segments in a
   polygon or a polyline.

   :signature: polygon-coordinates *polygon-or-polyline* => *coordinates*

   :parameter polygon-or-polyline: An instance of type ``type-union(<polygon>, <polyline>)``.
   :value coordinates: An instance of type ``limited(<sequence>, of: <real>)``.

   :description:

     Returns a sequence of coordinate pairs that specify the segments in
     *polygon-or-polyline*.

   :seealso:

     - :class:`<polygon>`
     - :gf:`polygon?`
     - :gf:`polygon-points`

.. generic-function:: polygon-points

   Returns a sequence of points that specify the segments in a polygon or a
   polyline.

   :signature: polygon-points *polygon-or-polyline* => *points*

   :parameter polygon-or-polyline: An instance of type ``type-union(<polygon>, <polyline>)``.
   :value points: An instance of type ``limited(<sequence>, of: <point>)``

   :description:

     Returns a sequence of points that specify the segments in
     *polygon-or-polyline*.

   :seealso:

     - :class:`<polygon>`
     - :gf:`polygon?`
     - :gf:`polygon-coordinates`

.. class:: <polyline>
   :abstract:
   :instantiable:

   The protocol class that corresponds to a polyline.

   :superclasses: :class:`<path>`

   :keyword coordinates: An instance of type ``limited(<sequence>, of: <real>)``. Required.
   :keyword points: An instance of type ``limited(<sequence>, of: <real>)``. Required.

   :description:

     The protocol class that corresponds to a polyline.

     A polyline can be described either in terms of the individual x and y
     coordinates that constitute its vertices, or by using composite points.
     If the former is used, then they can be specified at the time of
     creation using the *coordinates:* init-keyword, which is a sequence of
     real numbers, with x and y coordinates alternating within the sequence.

     To describe a polyline in terms of composite point objects, use the
     *points:* init-keyword, which is a sequence of instances of :class:`<point>`.
     You should be aware that using composite points may lead to a loss of
     performance.

     Exactly one of *coordinates:* and *points:* is required.

   :operations:

     The following operations are exported from the *DUIM-Extended-Geometry*
     module.

     - :gf:`do-polygon-coordinates`
     - :gf:`do-polygon-segments`
     - :gf:`draw-design`
     - :gf:`polygon-coordinates`
     - :gf:`polygon-points`
     - :gf:`polyline?`
     - :gf:`polyline-closed?`

     The following operations are exported from the *DUIM-Geometry* module.

     - :gf:`box-edges`
     - :gf:`transform-region`

   :seealso:

     - :class:`<path>`
     - :class:`<point>`
     - :func:`make-polyline`
     - :gf:`polyline?`
     - :gf:`polyline-closed?`

.. generic-function:: polyline?

   Returns ``#t`` if an object is a polyline.

   :signature: polyline? *object* => *boolean*

   :parameter object: An instance of type :drm:`<object>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if *object* is a polyline, otherwise returns ``#f``.

   :seealso:

     - :class:`<polyline>`
     - :gf:`polyline-closed?`

.. generic-function:: polyline-closed?

   Returns ``#t`` if the polyline is closed.

   :signature: polyline-closed? *polyline* => *boolean*

   :parameter polyline: An instance of type :class:`<polyline>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if the polyline *polyline* is closed, otherwise returns
     ``#f``. This function need be implemented only for polylines, not for
     polygons.

   :seealso:

     - :class:`<polyline>`
     - :gf:`polyline?`

.. class:: <rectangle>
   :abstract:
   :instantiable:

   The protocol class that corresponds to a rectangle.

   :superclasses: :class:`<area>`

   :keyword min-x: An instance of type ``<real>``.
   :keyword min-y: An instance of type ``<real>``.
   :keyword max-x: An instance of type ``<real>``.
   :keyword max-y: An instance of type ``<real>``.
   :keyword points: An instance of type limited(``<sequence>``, of: :class:`<point>`)

   :description:

     The protocol class that corresponds to a rectangle. This is a subclass
     of :class:`<polygon>`.

     Rectangles whose edges are parallel to the coordinate axes are a special
     case of polygon that can be specified completely by four real numbers
     *x1,y1,x2,y2*). They are *not* closed under general affine
     transformations (although they are closed under rectilinear
     transformations).

   :operations:

     The following operations are exported from the *DUIM-Extended-Geometry*
     module.

     - :gf:`do-polygon-coordinates`
     - :gf:`do-polygon-segments`
     - :gf:`draw-design`
     - :gf:`polygon-coordinates`
     - :gf:`polygon-points`
     - :gf:`rectangle?`
     - :gf:`rectangle-edges`
     - :gf:`rectangle-height`
     - :gf:`rectangle-max-point`
     - :gf:`rectangle-max-position`
     - :gf:`rectangle-min-point`
     - :gf:`rectangle-min-position`
     - :gf:`rectangle-size`
     - :gf:`rectangle-width`

     The following operations are exported from the *DUIM-Geometry* module.

     - :gf:`box-edges`
     - :gf:`transform-region`

   :seealso:

     - :class:`<polygon>`
     - :gf:`make-rectangle`
     - :gf:`rectangle?`
     - :gf:`rectangle-edges`
     - :gf:`rectangle-height`
     - :gf:`rectangle-max-point`
     - :gf:`rectangle-max-position`
     - :gf:`rectangle-min-point`
     - :gf:`rectangle-min-position`
     - :gf:`rectangle-size`
     - :gf:`rectangle-width`

.. generic-function:: rectangle?

   Returns ``#t`` if the object is a rectangle.

   :signature: rectangle? *object* => *boolean*

   :parameter object: An instance of type :drm:`<object>`.
   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns ``#t`` if *object* is a rectangle, otherwise returns ``#f``.

   :seealso:

     - :class:`<rectangle>`
     - :gf:`rectangle-edges`
     - :gf:`rectangle-height`
     - :gf:`rectangle-max-point`
     - :gf:`rectangle-max-position`
     - :gf:`rectangle-min-point`
     - :gf:`rectangle-min-position`
     - :gf:`rectangle-size`
     - :gf:`rectangle-width`

.. generic-function:: rectangle-edges

   Returns the coordinates of the minimum and maximum of the rectangle.

   :signature: rectangle-edges *rectangle* => *x1* *y1* *x2* *y2*
   :parameter rectangle: An instance of type :class:`<rectangle>`.

   :value min-x: An instance of type ``<real>``.
   :value min-y: An instance of type ``<real>``.
   :value max-x: An instance of type ``<real>``.
   :value max-y: An instance of type ``<real>``.

   :description:

     Returns the coordinates of the minimum *x* and *y* and maximum *x* and
     *y* of the rectangle *rectangle* as four values, *min-x, min-y, max-x,*
     and *max-y*.

     The argument *min-x* represents the *x* coordinate of the top left of
     the rectangle.

     The argument *min-y* represents the *y* coordinate of the top left of
     the rectangle.

     The argument *max-x* represents the *x* coordinate of the bottom right
     of the rectangle.

     The argument *max-y* represents the *y* coordinate of the bottom right
     of the rectangle.

   :seealso:

     - :class:`<rectangle>`
     - :gf:`rectangle?`
     - :gf:`rectangle-height`
     - :gf:`rectangle-max-point`
     - :gf:`rectangle-max-position`
     - :gf:`rectangle-min-point`
     - :gf:`rectangle-min-position`
     - :gf:`rectangle-size`
     - :gf:`rectangle-width`

.. generic-function:: rectangle-height

   Returns height of the rectangle.

   :signature: rectangle-height *rectangle* => *height*

   :parameter rectangle: An instance of type :class:`<rectangle>`.
   :value height: An instance of type ``<real>``.

   :description:

     Returns the height of the rectangle, which is the difference between the
     maximum *y* and its minimum *y*.

   :seealso:

     - :class:`<rectangle>`
     - :gf:`rectangle?`
     - :gf:`rectangle-edges`
     - :gf:`rectangle-max-point`
     - :gf:`rectangle-max-position`
     - :gf:`rectangle-min-point`
     - :gf:`rectangle-min-position`
     - :gf:`rectangle-size`
     - :gf:`rectangle-width`

.. generic-function:: rectangle-max-point

   Returns the bottom right point of the rectangle.

   :signature: rectangle-max-point *rectangle* => *point*

   :parameter rectangle: An instance of type :class:`<rectangle>`.
   :value point: An instance of type :class:`<point>`.

   :description:

     Returns the bottom right point of the rectangle.

   :seealso:

     - :class:`<rectangle>`
     - :gf:`rectangle?`
     - :gf:`rectangle-edges`
     - :gf:`rectangle-height`
     - :gf:`rectangle-max-position`
     - :gf:`rectangle-min-point`
     - :gf:`rectangle-min-position`
     - :gf:`rectangle-size`
     - :gf:`rectangle-width`

.. generic-function:: rectangle-max-position

   Returns the *x* and *y* coordinates of the bottom right of the
   rectangle.

   :signature: rectangle-max-position *rectangle* => *x2* *y2*

   :parameter rectangle: An instance of type :class:`<rectangle>`.
   :value x2: An instance of type ``<real>``.
   :value y2: An instance of type ``<real>``.

   :description:

     Returns the *x* and *y* coordinates of the bottom right of the
     rectangle.

   :seealso:

     - :class:`<rectangle>`
     - :gf:`rectangle?`
     - :gf:`rectangle-edges`
     - :gf:`rectangle-height`
     - :gf:`rectangle-max-point`
     - :gf:`rectangle-min-point`
     - :gf:`rectangle-min-position`
     - :gf:`rectangle-size`
     - :gf:`rectangle-width`

.. generic-function:: rectangle-min-point

   Returns the left top point of the rectangle.

   :signature: rectangle-min-point *rectangle* => *point*

   :parameter rectangle: An instance of type :class:`<rectangle>`.
   :value point: An instance of type :class:`<point>`.

   :description:

     Returns the left top point of the rectangle.

   :seealso:

     - :class:`<rectangle>`
     - :gf:`rectangle?`
     - :gf:`rectangle-edges`
     - :gf:`rectangle-height`
     - :gf:`rectangle-max-point`
     - :gf:`rectangle-max-position`
     - :gf:`rectangle-min-position`
     - :gf:`rectangle-size`
     - :gf:`rectangle-width`

.. generic-function:: rectangle-min-position

   Returns the *x* and *y* coordinates of the left top of the rectangle.

   :signature: rectangle-min-position *rectangle* => *x1* *y1*

   :parameter rectangle: An instance of type :class:`<rectangle>`.
   :value x1: An instance of type ``<real>``.
   :value y1: An instance of type ``<real>``.

   :description:

     Returns the *x* and *y* coordinates of the left top of the rectangle.

   :seealso:

     - :class:`<rectangle>`
     - :gf:`rectangle?`
     - :gf:`rectangle-edges`
     - :gf:`rectangle-height`
     - :gf:`rectangle-max-point`
     - :gf:`rectangle-max-position`
     - :gf:`rectangle-min-point`
     - :gf:`rectangle-size`
     - :gf:`rectangle-width`

.. generic-function:: rectangle-size

   Returns the width and the height of the rectangle.

   :signature: rectangle-size *rectangle* => *width* *height*

   :parameter rectangle: An instance of type :class:`<rectangle>`.
   :value width: An instance of type ``<real>``.
   :value height: An instance of type ``<real>``.

   :description:

     Returns two values, the width and the height.

   :seealso:

     - :class:`<rectangle>`
     - :gf:`rectangle?`
     - :gf:`rectangle-edges`
     - :gf:`rectangle-height`
     - :gf:`rectangle-max-point`
     - :gf:`rectangle-max-position`
     - :gf:`rectangle-min-point`
     - :gf:`rectangle-min-position`
     - :gf:`rectangle-width`

.. generic-function:: rectangle-width

   Returns the width of the rectangle.

   :signature: rectangle-width *rectangle* => *width*

   :parameter rectangle: An instance of type :class:`<rectangle>`.
   :value width: An instance of type ``<real>``.

   :description:

     Returns the width of the rectangle *rectangle*, which is the difference
     between the maximum *x* and its minimum *x*.

   :seealso:

     - :class:`<rectangle>`
     - :gf:`rectangle?`
     - :gf:`rectangle-edges`
     - :gf:`rectangle-height`
     - :gf:`rectangle-max-point`
     - :gf:`rectangle-max-position`
     - :gf:`rectangle-min-point`
     - :gf:`rectangle-min-position`
     - :gf:`rectangle-size`
