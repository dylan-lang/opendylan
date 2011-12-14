******************************
DUIM-Extended-Geometry Library
******************************

Overview
========

The DUIM-Extended-Geometry library builds on the features provided by
the DUIM-Geometry library, and provides more extensive support for
coordinate geometry that is only required for more specialist uses. The
library contains a single module, *duim-extended-geometry*, from which
all the interfaces described in this chapter are exposed. `See
DUIM-Extended-Geometry Module <ext-geom.htm#32467>`_ contains complete
reference entries for each exposed interface.

The class hierarchy for DUIM-Extended-Geometry
==============================================

The DUIM-Extended-Geometry library defines no base classes itself, but
instead subclasses two classes exposed in the DUIM-Geometry library:
*<area>* and *<path>*. In each case, these subclasses provide more
specialized geometrical tools.

Subclasses of <area>
^^^^^^^^^^^^^^^^^^^^

Three subclasses of *<area>* are exposed in the DUIM-Extended-Geometry
library, each of which provides the ability to create instances of
particular shapes. Their usage is relatively obvious.

-  *<rectangle>* This class is used to create rectangular shapes on a
   drawable object.
-  *<ellipse>* This class is used to create elliptical shapes on a
   drawable object.
-  *<polygon>* This class is used to create more general polygon shapes
   on a drawable object.

Subclass of <path>
^^^^^^^^^^^^^^^^^^

Three subclasses of *<path>* are exposed in the DUIM-Extended-Geometry
library, each of which provides the ability to create instances of
particular types of line. Their usage is relatively obvious.

-  *<line>* This class is used to create straight lines between two
   points on a drawable object.

*<elliptical-arc>*
                  

This class is used to create elliptical arcs (portions of an ellipse) on
a drawable object.
                                                                                           

-  *<polyline>* This class is used to create lines that pass through an
   arbitrary set of coordinates. It produces a jagged line with vertices
   at each coordinate.

DUIM-Extended-Geometry Module
=============================

This section contains a complete reference of all the interfaces that
are exported from the *duim-extended-geometry* module.

do-polygon-coordinates
----------------------

Generic function
''''''''''''''''

Summary
       

Applies a function to all of the coordinates of the vertices of a
polygon.

Signature
         

*do-polygon-coordinates* *function* *polygon* => ()

Arguments
         

-  *function* An instance of type *<function>*.
-  *polygon* An instance of type `See <polygon> <ext-geom.htm#24799>`_.

Values
      

None.

Description
           

Applies *function* to all of the coordinates of the vertices of
*polygon*. *function* is a function of two arguments, the *x* and *y*
coordinates of the vertex. *do-polygon-coordinates* returns *#f*.

See also
        

`See do-polygon-segments <ext-geom.htm#21726>`_

do-polygon-segments
-------------------

Generic function
''''''''''''''''

Summary
       

Applies a function to the segments that compose a polygon.

Signature
         

*do-polygon-segments* *function* *polygon* => ()

Arguments
         

-  *function* An instance of type *<function>*.
-  *polygon* An instance of type `See <polygon> <ext-geom.htm#24799>`_.

Values
      

None.

Description
           

Applies *function* to the segments that compose *polygon*. *function*
is a function of four arguments, the *x* and *y* coordinates of the
start of the segment, and the *x* and *y* coordinates of the end of the
segment. When *do-polygon-segments* is called on a closed polyline, it
calls *function* on the segment that connects the last point back to the
first point.

The function *do-polygon-segments* returns *#f*.

See also
        

`See do-polygon-coordinates <ext-geom.htm#77912>`_

draw-design
-----------

Generic function
''''''''''''''''

Summary
       

Draws a design on a drawing surface.

Signature
         

*draw-design* *drawable* *design* => ()

Arguments
         

-  *drawable* An instance of type *type-union(<sheet>, <medium>)*.
-  *design* A *<region>* to draw.

Values
      

-  None.

Description
           

Draws *design* on the sheet medium *drawable*.

<ellipse>
---------

Abstract instantiable class
'''''''''''''''''''''''''''

Summary
       

The class that corresponds to an ellipse.

Superclasses
            

*<area>*

Init-keywords
             

-  *center-x:* An instance of type *<real>*.
-  *center-y:* An instance of type *<real>*.
-  *center-point:* An instance of type `See <point> <geom.htm#15734>`_.
-  *radius-1-dx:* An instance of type *<real>*
-  *radius-1-dy:* An instance of type *<real>*
-  *radius-2-dx:* An instance of type *<real>*
-  *radius-2-dy:* An instance of type *<real>*
-  *start-angle:* An instance of *false-or(<real>)*.
-  *end-angle:* An instance of *false-or(<real>)*.

Description
           

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
the ellipse. The bounding parallelogram is made by adding and
subtracting the vectors from the center point in the following manner:

.. figure:: ext-geom-2.gif
   :align: center
   :alt: 

*x* coordinate

*y* coordinate

Center of ellipse

x*c*

y*c*

Vectors

dx*1*

dx*2*

dy*1*

dy*2*

Corners of parallelogram

x*c* + dx*1* + dx*2*

x*c* + dx*1* - dx*2*

x*c* - dx*1* - dx*2*

x*c* - dx*1* + dx*2*

y*c* + dx*1* +dx*2*

y*c* + dx*1* - dx*2*

y*c* - dx*1* - dx*2*

y*c* -dx*1* + dx*2*

Note that several different parallelograms specify the same ellipse. One
parallelogram is bound to be a rectangle — the vectors will be
perpendicular and correspond to the semi-axes of the ellipse.

Operations
          

The following operations are exported from the *DUIM-Extended-Geometry*
module.

`See draw-design <ext-geom.htm#88093>`_ `See
ellipse? <ext-geom.htm#97289>`_ `See
ellipse-center-point <ext-geom.htm#80133>`_ `See
ellipse-center-position <ext-geom.htm#46139>`_ `See
ellipse-end-angle <ext-geom.htm#34063>`_ `See
ellipse-radii <ext-geom.htm#35464>`_ `See
ellipse-start-angle <ext-geom.htm#61946>`_

The following operations are exported from the *DUIM-Geometry* module.

`See box-edges <geom.htm#52858>`_ `See
transform-region <geom.htm#33126>`_

See also
        

`See <area> <geom.htm#53450>`_

`See make-ellipse <ext-geom.htm#64594>`_

ellipse?
--------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if an object is an ellipse.

Signature
         

*ellipse?* *object* => *boolean*

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if *object* is an ellipse, otherwise returns *#f.*

See Also
        

`See <ellipse> <ext-geom.htm#18828>`_

ellipse-center-point
--------------------

Generic function
''''''''''''''''

Summary
       

Returns the center point of an ellipse or an elliptical arc.

Signature
         

*ellipse-center-poin* t *elliptical-object* => *point*

Arguments
         

-  *elliptical-object* An instance of type *type-union(`See
   <ellipse> <ext-geom.htm#18828>`_, `See
   <elliptical-arc> <ext-geom.htm#65659>`_)*.

Values
      

-  *point* An instance of type `See <point> <geom.htm#15734>`_.

Description
           

Returns the center point of *ellipse-object* as** a *<point>* object.

See also
        

`See make-ellipse <ext-geom.htm#64594>`_

ellipse-center-position
-----------------------

Generic function
''''''''''''''''

Summary
       

Returns the coordinates of the center point of an ellipse or an
elliptical arc.

Signature
         

*ellipse-center-position* *elliptical-object* => *x* *y*

Arguments
         

-  *elliptical-object* An instance of type *type-union(`See
   <ellipse> <ext-geom.htm#18828>`_, `See
   <elliptical-arc> <ext-geom.htm#65659>`_)*.

Values
      

-  *x* An instance of type *<real>*.
-  *y* An instance of type *<real>*.

Description
           

Returns the coordinates of the center point of *elliptical-object*.

The arguments *x* and *y* represent the x and y coordinates of the
center of the elliptical object, respectively.

See also
        

`See make-ellipse <ext-geom.htm#64594>`_

ellipse-end-angle
-----------------

Generic function
''''''''''''''''

Summary
       

Returns the end angle of an ellipse or an elliptical-object.

Signature
         

*ellipse-end-angle* *elliptical-object* => *angle*

Arguments
         

-  *elliptical-object* An instance of type *type-union(`See
   <ellipse> <ext-geom.htm#18828>`_, `See
   <elliptical-arc> <ext-geom.htm#65659>`_)*.

Values
      

-  *angle* An instance of type *false-or(<real>)*.

Description
           

Returns the end angle of *elliptical-object*. If *elliptical-object* is
a full ellipse or closed path then *ellipse-end-angle* returns *#f* ;
otherwise the value is a number greater than zero, and less than or
equal to *2p*.

See also
        

`See make-ellipse <ext-geom.htm#64594>`_

ellipse-radii
-------------

Generic function
''''''''''''''''

Summary
       

Returns four values corresponding to the two radius vectors of an
elliptical arc.

Signature
         

*ellipse-radii* *elliptical-object* => *r1-dx* *r1-dy* *r2-dx* *d2-dy*

Arguments
         

-  *elliptical-object* An instance of type *type-union(`See
   <ellipse> <ext-geom.htm#18828>`_, `See
   <elliptical-arc> <ext-geom.htm#65659>`_)*.

Values
      

-  *r1-dx* An instance of type *<real>*.
-  *r1-dy* An instance of type *<real>*.
-  *r2-dx* An instance of type *<real>*.
-  *d2-dy* An instance of type *<real>*.

Description
           

Returns four values corresponding to the two radius vectors of
*elliptical-object*. These values may be canonicalized in some way, and
so may not be the same as the values passed to the constructor function.

See also
        

`See make-ellipse <ext-geom.htm#64594>`_

ellipse-start-angle
-------------------

Generic function
''''''''''''''''

Summary
       

Returns the start angle of an elliptical arc.

Signature
         

*ellipse-start-angle* *elliptical-object* => *angle*

Arguments
         

-  *elliptical-object* An instance of type *type-union(`See
   <ellipse> <ext-geom.htm#18828>`_, `See
   <elliptical-arc> <ext-geom.htm#65659>`_)*.

Values
      

-  *angle* An instance of type *false-or(<real>)*.

Description
           

Returns the start angle of *elliptical-object*. If *elliptical-object*
is a full ellipse or closed path then *ellipse-start-angle* returns *#f*
; otherwise the value will be a number greater than or equal to zero,
and less than *2p*.

See also
        

`See make-ellipse <ext-geom.htm#64594>`_

<elliptical-arc>
----------------

Abstract instantiable class
'''''''''''''''''''''''''''

Summary
       

An *elliptical arc* is a path consisting of all or a portion of the
outline of an ellipse.

Superclasses
            

*<path>*

Init-keywords
             

-  *center-x:* An instance of type *<real>*.
-  *center-y:* An instance of *<real>*.
-  *center-point:* An instance of type `See <point> <geom.htm#15734>`_.
-  *radius-1-dx:* An instance of *<real>*.
-  *radius-1-dy:* An instance of *<real>*.
-  *radius-2-dx:* An instance of *<real>*.
-  *radius-2-dy:* An instance of *<real>*.
-  *start-angle:* An instance of *false-or(<real>)*.
-  *end-angle:* An instance of *false-or(<real>)*.

Description
           

An *elliptical arc* is a path consisting of all or a portion of the
outline of an ellipse. Circular arcs are special cases of elliptical
arcs.

Operations
          

The following operations are exported from the *DUIM-Extended-Geometry*
module.

`See draw-design <ext-geom.htm#88093>`_ `See
ellipse-center-point <ext-geom.htm#80133>`_ `See
ellipse-center-position <ext-geom.htm#46139>`_ `See
ellipse-end-angle <ext-geom.htm#34063>`_ `See
ellipse-radii <ext-geom.htm#35464>`_ `See
ellipse-start-angle <ext-geom.htm#61946>`_ `See
elliptical-arc? <ext-geom.htm#39895>`_

The following operations are exported from the *DUIM-Geometry* module.

`See box-edges <geom.htm#52858>`_ `See
transform-region <geom.htm#33126>`_

See also
        

`See elliptical-arc? <ext-geom.htm#39895>`_

`See make-elliptical-arc <ext-geom.htm#12460>`_

elliptical-arc?
---------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if an object is an elliptical arc,

Signature
         

*elliptical-arc?* *object* => *boolean*

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if *object* is an elliptical arc, otherwise returns *#f*.

See also
        

`See <elliptical-arc> <ext-geom.htm#65659>`_

<line>
------

Abstract instantiable class
'''''''''''''''''''''''''''

Summary
       

The class that corresponds to a line.

Superclasses
            

*<path>*

Init-keywords
             

-  *start-x:* An instance of *<real>*.
-  *start-y:* An instance of *<real>*.
-  *end-x:* An instance of *<real>*.
-  *end-y:* An instance of *<real>*.
-  *points:* Instances of `See <point> <geom.htm#15734>`_.

Description
           

The class that corresponds to a line. This is a subclass of *<path>*.

This is the instantiable class that implements a line segment.
*make-line* instantiates an object of type *<line>*.

Operations
          

The following operations are exported from the *DUIM-Extended-Geometry*
module.

`See do-polygon-coordinates <ext-geom.htm#77912>`_ `See
do-polygon-segments <ext-geom.htm#21726>`_ `See
draw-design <ext-geom.htm#88093>`_ `See line? <ext-geom.htm#45120>`_
`See line-end-point <ext-geom.htm#89761>`_ `See
line-end-position <ext-geom.htm#26663>`_ `See
line-start-point <ext-geom.htm#55978>`_ `See
line-start-position <ext-geom.htm#94563>`_ `See
polygon-coordinates <ext-geom.htm#41304>`_ `See
polygon-points <ext-geom.htm#39099>`_ `See
polyline-closed? <ext-geom.htm#26106>`_

The following operations are exported from the *DUIM-Geometry* module.

`See box-edges <geom.htm#52858>`_ `See
transform-region <geom.htm#33126>`_

See also
        

`See <path> <geom.htm#44109>`_

`See make-line <ext-geom.htm#59981>`_

line?
-----

Generic function
''''''''''''''''

Summary
       

Returns *#t* if an object is a line.

Signature
         

*line?* *object* => *boolean*

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if *object* is a line, otherwise returns *#f.*

line-end-point
--------------

Generic function
''''''''''''''''

Summary
       

Returns the ending point of a line.

Signature
         

*line-end-point* *line* => *point*

Arguments
         

-  *line* An instance of type *<line>*.

Values
      

-  *point* An instance of type `See <point> <geom.htm#15734>`_.

Description
           

Returns the ending point of *line* as a *<point>* object.

See also
        

`See line-start-point <ext-geom.htm#55978>`_

line-end-position
-----------------

Generic function
''''''''''''''''

Summary
       

Returns the ending point of a line.

Signature
         

*line-end-position* *line* => *x y*

Arguments
         

-  *line* An instance of type *<line>*.

Values
      

-  *x* An instance of type *<real>*.
-  *y* An instance of type *<real>*.

Description
           

Returns two real numbers representing the *x* and *y* coordinates of the
ending point of *line*.

The arguments *x* and *y* represent the x and y coordinates of the end
of the line, respectively.

See also
        

`See line-start-position <ext-geom.htm#94563>`_

line-start-point
----------------

Generic function
''''''''''''''''

Summary
       

Returns the starting point of a line.

Signature
         

*line-start-point* *line* => *point*

Arguments
         

-  *line* An instance of type *<line>*.

Values
      

-  *point* An instance of type `See <point> <geom.htm#15734>`_.

Description
           

Returns the starting point of *line* as a *<point>* object.

See also
        

`See line-end-point <ext-geom.htm#89761>`_

line-start-position
-------------------

Generic function
''''''''''''''''

Summary
       

Returns the starting point of a line.

Signature
         

*line-start-position* *line* => *x* *y*

Arguments
         

-  *line* An instance of type *<line>*.

Values
      

-  *x* An instance of type *<real>*.
-  *y* An instance of type *<real>*.

Description
           

Returns two real numbers representing the *x* and *y* coordinates of the
starting point of *line*.

The arguments *x* and *y* represent the x and y coordinates of the start
of the line, respectively.

See also
        

`See line-end-position <ext-geom.htm#26663>`_

make-ellipse
------------

Function
''''''''

Summary
       

Returns an object of class *<ellipse>*.

Signature
         

*make-ellipse* *center-x* *center-y* *radius-1-dx* *radius-1-dy*
*radius-2-dx* *radius-2-dy* #key *start-angle* *end-angle* => *ellipse*

*make-ellipse\** *center-point* *radius-1-dx* *radius-1-dy*
*radius-2-dx* *radius-2-dy* #key *start-angle* *end-angle* => *ellipse*

Arguments
         

-  *radius-1-dx* An instance of type *<real>*.
-  *radius-1-dy* An instance of type *<real>*.
-  *radius-2-dx* An instance of type *<real>*.
-  *radius-2-dy* An instance of type *<real>*.
-  *start-angle* An instance of type *false-or(<real>)*.
-  *end-angle* An instance of type *false-or(<real>)*.

The following arguments are specific to *make-ellipse*.

-  *center-x* An instance of type *<real>*.
-  *center-y* An instance of type *<real>*.

The following argument is specific to *make-ellipse*.

-  *center-point* An instance of type `See <point> <geom.htm#15734>`_.

Values
      

-  *ellipse* An instance of type *`See <ellipse> <ext-geom.htm#18828>`_*
   .

Description
           

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

See also
        

See the class `See <ellipse> <ext-geom.htm#18828>`_.

make-elliptical-arc
-------------------

Function
''''''''

Summary
       

Returns an object of class *<elliptical-arc>*.

Signature
         

*make-elliptical-arc* *center-x* *center-y* *radius-1-dx* *radius-1-dy*
*radius-2-dx* *radius-2-dy* #key *start-angle* *end-angle* => *arc*

*make-elliptical-arc\** *center-point* *radius-1-dx* *radius-1-dy*
*radius-2-dx* *radius-2-dy* #key *start-angle* *end-angle* => *arc*

Arguments
         

-  *radius-1-dx* An instance of type *<real>*.
-  *radius-1-dy* An instance of type *<real>*.
-  *radius-2-dx* An instance of type *<real>*.
-  *radius-2-dy* An instance of type *<real>*.
-  *start-angle* An instance of type *false-or(<real>)*.
-  *end-angle* An instance of type *false-or(<real>)*.

The following arguments are specific to *make-elliptical-arc*.

-  *center-x* An instance of type *<real>*.
-  *center-y* An instance of type *<real>*.

The following argument is specific to *make-elliptical-arc\**.

-  *center-point* An instance of type `See <point> <geom.htm#15734>`_.

Values
      

-  *arc* An instance of type *`See
   <elliptical-arc> <ext-geom.htm#65659>`_*.

Description
           

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

See also
        

See the class `See <elliptical-arc> <ext-geom.htm#65659>`_.

make-line
---------

Function
''''''''

Summary
       

Returns an object of class *<line>*.t

Signature
         

*make-line* *start-x* *start-y* *end-x* *end-y* => *line*

*make-line\** *start-point* *end-point* => *line*

Arguments
         

-  *start-x* An instance of type *<real>*.
-  *start-y* An instance of type *<real>*.
-  *end-x* An instance of type *<real>*.
-  *end-y* An instance of type *<real>*.
-  *start-point* An instance of type `See <point> <geom.htm#15734>`_.
-  *end-point* An instance of type `See <point> <geom.htm#15734>`_.

Values
      

-  *line* An instance of type *<line>*.

Description
           

Returns an object of class *<line>* that connects the two positions
(*start-x,start-y*) and (e*nd-x,end-y*) or the two points
*start-point* and *end-point*.

make-polygon
------------

Function
''''''''

Summary
       

Returns an object of class *<polygon>.*

Signature
         

*make-polygon* *coord-seq* => *polygon*

*make-polygon\** *point-seq* => *polygon*

Arguments
         

The following argument is specific to *make-polygon*.

-  *coord-seq* An instance of type *limited(<sequence>, of: <real>)*.

The following argument is specific to *make-polygon\**.

-  *point-seq* An instance of type *limited(<sequence>, of:* `See
   <point> <geom.htm#15734>`_*)*.

Values
      

-  *polygon* An instance of type `See <polygon> <ext-geom.htm#24799>`_.

Description
           

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

make-polyline
-------------

Function
''''''''

Summary
       

Returns an object of class *<polyline>.*

Signature
         

*make-polyline* *coord-seq* #key *closed?* => *polyline*

*make-polyline\** *point-seq* #key *closed?* => *polyline*

Arguments
         

-  *closed?* An instance of type *<boolean>*. Default value: *#f*.

The following argument is specific to *make-polyline*.

-  *coord-seq* An instance of type *limited(<sequence>, of: <real>)*.

The following argument is specific to *make-polyline\**.

-  *point-seq* An instance of type *limited(<sequence>, of:* `See
   <point> <geom.htm#15734>`_*)*.

Values
      

-  *polyline* An instance of type *`See
   <polyline> <ext-geom.htm#22516>`_*.

Description
           

Returns an object of class *<polyline>* consisting of the segments
connecting each of the points in *point-seq* or the points represented
by the coordinate pairs in *coord-seq*. *point-seq* is a sequence of
points; *coord-seq* is a sequence of coordinate pairs, which are real
numbers. It is an error if *coord-seq* does not contain an even number
of elements.

If *closed?* is *#t,* then the segment connecting the first point and
the last point is included in the polyline. The default for *closed?*
is** *#f*.

The function *make-polyline\** is identical to *make-polyline*, except
that it passes composite objects, rather than separate coordinates, in
its arguments. You should be aware that using this function may lead to
a loss of performance.

make-rectangle
--------------

Function
''''''''

Summary
       

Returns an object of class *<rectangle>*.

Signature
         

*make-rectangle* *x1* *y1* *x2* *y2* => *rectangle*

*make-rectangle\** *min-point* *max-point* => *rectangle*

Arguments
         

The following arguments are specific to *make-rectangle*.

-  *x1* An instance of type *<real>*. The *x* coordinate of the left
   top of the rectangle.
-  *y1* An instance of type *<real>*. The *y* coordinate of the left
   top of the rectangle
-  *x2* An instance of type *<real>*. The *x* coordinate of the bottom
   right of the rectangle.
-  *y2* An instance of type *<real>*. The *y* coordinate of the bottom
   right of the rectangle.

The following arguments are specific to *make-rectangle\**.

-  *min-point* The minimum point (left top) of the rectangle.
-  *max-point* The maximum point (bottom right) of the rectangle.

Values
      

-  *rectangle* An instance of type *<rectangle>*.

Description
           

Returns an object of class *<rectangle>* whose edges are parallel to the
coordinate axes. One corner is at the point *point1* or the
position*x1,y1* and the opposite corner is at the point *point2* or the
position *x2,y2*. There are no ordering constraints among *point1* and
*point2* or *x1* and *x2*, and *y1* and *y2*.

The function *make-rectangle\** is identical to *make-rectangle*,
except that it passes composite objects, rather than separate
coordinates, in its arguments. You should be aware that using this
function may lead to a loss of performance.

<polygon>
---------

Abstract instantiable class
'''''''''''''''''''''''''''

Summary
       

The class that corresponds to a polygon.

Superclasses
            

*<area>*

Init-keywords
             

-  *coordinates:* An instance of type *limited(<sequence>, of: <real>)*
   .
-  *points:* An instance of type *limited(<sequence>, of: <real>)*.

Description
           

The class that corresponds to a polygon. This is a subclass of *<area>*
.

A polygon can be described either in terms of the individual x and y
coordinates that constitute its vertices, or by using composite points.
If the former is used, then they can be specified at the time of
creation using the *coordinates:* init-keyword, which is a sequence of
real numbers, with x and y coordinates alternating within the sequence.

To describe a polygon in terms of composite point objects, use the
*points:* init-keyword, which is a sequence of instances of *<point>*.
You should be aware that using composite points may lead to a loss of
performance.

Exactly one of *coordinates:* and *points:* is required.

Operations
          

The following operations are exported from the *DUIM-Extended-Geometry*
module.

`See do-polygon-coordinates <ext-geom.htm#77912>`_ `See
do-polygon-segments <ext-geom.htm#21726>`_ `See
draw-design <ext-geom.htm#88093>`_ `See polygon? <ext-geom.htm#31499>`_
`See polygon-coordinates <ext-geom.htm#41304>`_ `See
polygon-points <ext-geom.htm#39099>`_

The following operations are exported from the *DUIM-Geometry* module.

`See box-edges <geom.htm#52858>`_ `See
transform-region <geom.htm#33126>`_

See also
        

`See <area> <geom.htm#53450>`_

`See make-polygon <ext-geom.htm#62078>`_

`See polygon? <ext-geom.htm#31499>`_

`See polygon-coordinates <ext-geom.htm#41304>`_

`See polygon-points <ext-geom.htm#39099>`_

polygon?
--------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if its argument is a polygon.

Signature
         

*polygon?* *object* => *boolean*

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if *object* is a polygon, otherwise returns

*#f*.

See also
        

`See <polygon> <ext-geom.htm#24799>`_

`See polygon-coordinates <ext-geom.htm#41304>`_

`See polygon-points <ext-geom.htm#39099>`_

polygon-coordinates
-------------------

Generic function
''''''''''''''''

Summary
       

Returns a sequence of coordinate pairs that specify the segments in a
polygon or a polyline.

Signature
         

*polygon-coordinates* *polygon-or-polyline* => *coordinates*

Arguments
         

*polygon-or-polyline*
                     

-  An instance of type *type-union(* `See
   <polygon> <ext-geom.htm#24799>`_*, `See
   <polyline> <ext-geom.htm#22516>`_)*.

Values
      

-  *coordinates* An instance of type *limited(<sequence>, of: <real>)*.

Description
           

Returns a sequence of coordinate pairs that specify the segments in
*polygon-or-polyline*.

See also
        

`See <polygon> <ext-geom.htm#24799>`_

`See polygon? <ext-geom.htm#31499>`_

`See polygon-points <ext-geom.htm#39099>`_

polygon-points
--------------

Generic function
''''''''''''''''

Summary
       

Returns a sequence of points that specify the segments in a polygon or a
polyline.

Signature
         

*polygon-points* *polygon-or-polyline* => *points*

Arguments
         

*polygon-or-polyline*
                     

-  An instance of type *type-union(* `See
   <polygon> <ext-geom.htm#24799>`_*, `See
   <polyline> <ext-geom.htm#22516>`_)*.

Values
      

-  *points* An instance of type *limited(<sequence>, of:* `See
   <point> <geom.htm#15734>`_*)*.

Description
           

Returns a sequence of points that specify the segments in
*polygon-or-polyline*.

See also
        

`See <polygon> <ext-geom.htm#24799>`_

`See polygon? <ext-geom.htm#31499>`_

`See polygon-coordinates <ext-geom.htm#41304>`_

<polyline>
----------

Abstract instantiable class
'''''''''''''''''''''''''''

Summary
       

The protocol class that corresponds to a polyline.

Superclasses
            

*<path>*

Init-keywords
             

-  *coordinates:* An instance of type *limited(<sequence>, of: <real>)*
   . Required.
-  *points:* An instance of type *limited(<sequence>, of: <real>)*.
   Required.

Description
           

The protocol class that corresponds to a polyline.

A polyline can be described either in terms of the individual x and y
coordinates that constitute its vertices, or by using composite points.
If the former is used, then they can be specified at the time of
creation using the *coordinates:* init-keyword, which is a sequence of
real numbers, with x and y coordinates alternating within the sequence.

To describe a polyline in terms of composite point objects, use the
*points:* init-keyword, which is a sequence of instances of *<point>*.
You should be aware that using composite points may lead to a loss of
performance.

Exactly one of *coordinates:* and *points:* is required.

Operations
          

The following operations are exported from the *DUIM-Extended-Geometry*
module.

`See do-polygon-coordinates <ext-geom.htm#77912>`_ `See
do-polygon-segments <ext-geom.htm#21726>`_ `See
draw-design <ext-geom.htm#88093>`_ `See
polygon-coordinates <ext-geom.htm#41304>`_ `See
polygon-points <ext-geom.htm#39099>`_ `See
polyline? <ext-geom.htm#64198>`_ `See
polyline-closed? <ext-geom.htm#26106>`_

The following operations are exported from the *DUIM-Geometry* module.

`See box-edges <geom.htm#52858>`_ `See
transform-region <geom.htm#33126>`_

See also
        

`See <path> <geom.htm#44109>`_

`See make-polyline <ext-geom.htm#46392>`_

`See polyline? <ext-geom.htm#64198>`_

`See polyline-closed? <ext-geom.htm#26106>`_

polyline?
---------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if an object is a polyline.

Signature
         

*polyline?* *object* => *boolean*

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if *object* is a polyline, otherwise returns

*#f*.

See also
        

`See <polyline> <ext-geom.htm#22516>`_

`See polyline-closed? <ext-geom.htm#26106>`_

polyline-closed?
----------------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if the polyline is closed.

Signature
         

*polyline-closed?* *polyline* => *boolean*

Arguments
         

-  *polyline* An instance of type *`See
   <polyline> <ext-geom.htm#22516>`_*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if the polyline *polyline* is closed, otherwise returns
*#f*. This function need be implemented only for polylines, not for
polygons.

See also
        

`See <polyline> <ext-geom.htm#22516>`_

`See polyline? <ext-geom.htm#64198>`_

<rectangle>
-----------

Abstract instantiable class
'''''''''''''''''''''''''''

Summary
       

The protocol class that corresponds to a rectangle.

Superclasses
            

*<area>*

Init-keywords
             

-  *min-x:* An instance of type *<real>*.
-  *min-y:* An instance of type *<real>*.
-  *max-x:* An instance of type *<real>*.
-  *max-y:* An instance of type *<real>*.
-  *points:* An instance of type *limited(<sequence>, of:* `See
   <point> <geom.htm#15734>`_*)*.

Description
           

The protocol class that corresponds to a rectangle. This is a subclass
of `See <polygon> <ext-geom.htm#24799>`_.

Rectangles whose edges are parallel to the coordinate axes are a special
case of polygon that can be specified completely by four real numbers
*x1,y1,x2,y2*). They are *not* closed under general affine
transformations (although they are closed under rectilinear
transformations).

Operations
          

The following operations are exported from the *DUIM-Extended-Geometry*
module.

`See do-polygon-coordinates <ext-geom.htm#77912>`_ `See
do-polygon-segments <ext-geom.htm#21726>`_ `See
draw-design <ext-geom.htm#88093>`_ `See
polygon-coordinates <ext-geom.htm#41304>`_ `See
polygon-points <ext-geom.htm#39099>`_ `See
rectangle? <ext-geom.htm#49199>`_ `See
rectangle-edges <ext-geom.htm#83383>`_ `See
rectangle-height <ext-geom.htm#96239>`_ `See
rectangle-max-point <ext-geom.htm#17612>`_ `See
rectangle-max-position <ext-geom.htm#44768>`_ `See
rectangle-min-point <ext-geom.htm#35015>`_ `See
rectangle-min-position <ext-geom.htm#67691>`_ `See
rectangle-size <ext-geom.htm#50143>`_ `See
rectangle-width <ext-geom.htm#35417>`_

The following operations are exported from the *DUIM-Geometry* module.

`See box-edges <geom.htm#52858>`_ `See
transform-region <geom.htm#33126>`_

See also
        

`See <polygon> <ext-geom.htm#24799>`_

`See make-rectangle <ext-geom.htm#94627>`_

`See rectangle? <ext-geom.htm#49199>`_

`See rectangle-edges <ext-geom.htm#83383>`_

`See rectangle-height <ext-geom.htm#96239>`_

`See rectangle-max-point <ext-geom.htm#17612>`_

`See rectangle-max-position <ext-geom.htm#44768>`_

`See rectangle-min-point <ext-geom.htm#35015>`_

`See rectangle-min-position <ext-geom.htm#67691>`_

`See rectangle-size <ext-geom.htm#50143>`_

`See rectangle-width <ext-geom.htm#35417>`_

rectangle?
----------

Generic function
''''''''''''''''

Summary
       

Returns *#t* if the object is a rectangle.

Signature
         

*rectangle?* *object* => *boolean*

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns *#t* if *object* is a rectangle, otherwise returns *#f*.

See also
        

`See <rectangle> <ext-geom.htm#71465>`_

`See rectangle-edges <ext-geom.htm#83383>`_

`See rectangle-height <ext-geom.htm#96239>`_

`See rectangle-max-point <ext-geom.htm#17612>`_

`See rectangle-max-position <ext-geom.htm#44768>`_

`See rectangle-min-point <ext-geom.htm#35015>`_

`See rectangle-min-position <ext-geom.htm#67691>`_

`See rectangle-size <ext-geom.htm#50143>`_

`See rectangle-width <ext-geom.htm#35417>`_

rectangle-edges
---------------

Generic function
''''''''''''''''

Summary
       

Returns the coordinates of the minimum and maximum of the rectangle.

Signature
         

*rectangle-edges* *rectangle* => *x1* *y1* *x2* *y2*

Arguments
         

-  *rectangle* An instance of type *<rectangle>*.

Values
      

-  *min-x* An instance of type *<real>*.
-  *min-y* An instance of type *<real>*.
-  *max-x* An instance of type *<real>*.
-  *max-y* An instance of type *<real>*.

Description
           

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

See also
        

`See <rectangle> <ext-geom.htm#71465>`_

`See rectangle? <ext-geom.htm#49199>`_

`See rectangle-height <ext-geom.htm#96239>`_

`See rectangle-max-point <ext-geom.htm#17612>`_

`See rectangle-max-position <ext-geom.htm#44768>`_

`See rectangle-min-point <ext-geom.htm#35015>`_

`See rectangle-min-position <ext-geom.htm#67691>`_

`See rectangle-size <ext-geom.htm#50143>`_

`See rectangle-width <ext-geom.htm#35417>`_

rectangle-height
----------------

Generic function
''''''''''''''''

Summary
       

Returns height of the rectangle.

Signature
         

*rectangle-height* *rectangle* => *height*

Arguments
         

-  *rectangle* An instance of type *<rectangle>*.

Values
      

-  *height* An instance of type *<real>*.

Description
           

Returns the height of the rectangle, which is the difference between the
maximum *y* and its minimum *y*.

See also
        

`See <rectangle> <ext-geom.htm#71465>`_

`See rectangle? <ext-geom.htm#49199>`_

`See rectangle-edges <ext-geom.htm#83383>`_

`See rectangle-max-point <ext-geom.htm#17612>`_

`See rectangle-max-position <ext-geom.htm#44768>`_

`See rectangle-min-point <ext-geom.htm#35015>`_

`See rectangle-min-position <ext-geom.htm#67691>`_

`See rectangle-size <ext-geom.htm#50143>`_

`See rectangle-width <ext-geom.htm#35417>`_

rectangle-max-point
-------------------

Generic function
''''''''''''''''

Summary
       

Returns the bottom right point of the rectangle.

Signature
         

*rectangle-max-point* *rectangle* => *point*

Arguments
         

-  *rectangle* An instance of type *<rectangle>*.

Values
      

-  *point* An instance of type `See <point> <geom.htm#15734>`_.

Description
           

Returns the bottom right point of the rectangle.

See also
        

`See <rectangle> <ext-geom.htm#71465>`_

`See rectangle? <ext-geom.htm#49199>`_

`See rectangle-edges <ext-geom.htm#83383>`_

`See rectangle-height <ext-geom.htm#96239>`_

`See rectangle-max-position <ext-geom.htm#44768>`_

`See rectangle-min-point <ext-geom.htm#35015>`_

`See rectangle-min-position <ext-geom.htm#67691>`_

`See rectangle-size <ext-geom.htm#50143>`_

`See rectangle-width <ext-geom.htm#35417>`_

rectangle-max-position
----------------------

Generic function
''''''''''''''''

Summary
       

Returns the *x* and *y* coordinates of the bottom right of the
rectangle.

Signature
         

rectangle-max-position *rectangle* => *x2* *y2*

Arguments
         

-  *rectangle* An instance of type *<rectangle>*.

Values
      

-  *x2* An instance of type *<real>*.
-  *y2* An instance of type *<real>*.

Description
           

Returns the *x* and *y* coordinates of the bottom right of the
rectangle.

See also
        

`See <rectangle> <ext-geom.htm#71465>`_

`See rectangle? <ext-geom.htm#49199>`_

`See rectangle-edges <ext-geom.htm#83383>`_

`See rectangle-height <ext-geom.htm#96239>`_

`See rectangle-max-point <ext-geom.htm#17612>`_

`See rectangle-min-point <ext-geom.htm#35015>`_

`See rectangle-min-position <ext-geom.htm#67691>`_

`See rectangle-size <ext-geom.htm#50143>`_

`See rectangle-width <ext-geom.htm#35417>`_

rectangle-min-point
-------------------

Generic function
''''''''''''''''

Summary
       

Returns the left top point of the rectangle.

Signature
         

*rectangle-min-point* *rectangle* => *point*

Arguments
         

-  *rectangle* An instance of type *<rectangle>*.

Values
      

-  *point* An instance of type `See <point> <geom.htm#15734>`_.

Description
           

Returns the left top point of the rectangle.

See also
        

`See <rectangle> <ext-geom.htm#71465>`_

`See rectangle? <ext-geom.htm#49199>`_

`See rectangle-edges <ext-geom.htm#83383>`_

`See rectangle-height <ext-geom.htm#96239>`_

`See rectangle-max-point <ext-geom.htm#17612>`_

`See rectangle-max-position <ext-geom.htm#44768>`_

`See rectangle-min-position <ext-geom.htm#67691>`_

`See rectangle-size <ext-geom.htm#50143>`_

`See rectangle-width <ext-geom.htm#35417>`_

rectangle-min-position
----------------------

Generic function
''''''''''''''''

Summary
       

Returns the *x* and *y* coordinates of the left top of the rectangle.

Signature
         

*rectangle-min-position* *rectangle* => *x1* *y1*

Arguments
         

-  *rectangle* An instance of type *<rectangle>*.

Values
      

-  *x1* An instance of type *<real>*.
-  *y1* An instance of type *<real>*.

Description
           

Returns the *x* and *y* coordinates of the left top of the rectangle.

See also
        

`See <rectangle> <ext-geom.htm#71465>`_

`See rectangle? <ext-geom.htm#49199>`_

`See rectangle-edges <ext-geom.htm#83383>`_

`See rectangle-height <ext-geom.htm#96239>`_

`See rectangle-max-point <ext-geom.htm#17612>`_

`See rectangle-max-position <ext-geom.htm#44768>`_

`See rectangle-min-point <ext-geom.htm#35015>`_

`See rectangle-size <ext-geom.htm#50143>`_

`See rectangle-width <ext-geom.htm#35417>`_

rectangle-size
--------------

Generic function
''''''''''''''''

Summary
       

Returns the width and the height of the rectangle.

Signature
         

*rectangle-size* *rectangle* => *width* *height*

Arguments
         

-  *rectangle* An instance of type *<rectangle>*.

Values
      

-  *width* An instance of type *<real>*.
-  *height* An instance of type *<real>*.

Description
           

Returns two values, the width and the height.

See also
        

`See <rectangle> <ext-geom.htm#71465>`_

`See rectangle? <ext-geom.htm#49199>`_

`See rectangle-edges <ext-geom.htm#83383>`_

`See rectangle-height <ext-geom.htm#96239>`_

`See rectangle-max-point <ext-geom.htm#17612>`_

`See rectangle-max-position <ext-geom.htm#44768>`_

`See rectangle-min-point <ext-geom.htm#35015>`_

`See rectangle-min-position <ext-geom.htm#67691>`_

`See rectangle-width <ext-geom.htm#35417>`_

rectangle-width
---------------

Generic function
''''''''''''''''

Summary
       

Returns the width of the rectangle.

Signature
         

*rectangle-width* *rectangle* => *width*

Arguments
         

-  *rectangle* An instance of type *<rectangle>*.

Values
      

-  *width* An instance of type *<real>*.

Description
           

Returns the width of the rectangle *rectangle*, which is the difference
between the maximum *x* and its minimum *x*.

See also
        

`See <rectangle> <ext-geom.htm#71465>`_

`See rectangle? <ext-geom.htm#49199>`_

`See rectangle-edges <ext-geom.htm#83383>`_

`See rectangle-height <ext-geom.htm#96239>`_

`See rectangle-max-point <ext-geom.htm#17612>`_

`See rectangle-max-position <ext-geom.htm#44768>`_

`See rectangle-min-point <ext-geom.htm#35015>`_

`See rectangle-min-position <ext-geom.htm#67691>`_

`See rectangle-size <ext-geom.htm#50143>`_


