********************
DUIM-Layouts Library
********************

.. current-library:: duim-layouts
.. current-module:: duim-layouts

Overview
========

The DUIM-Layouts library contains interfaces that define a number of
layouts for use in your GUI applications, as well as the necessary
functions, generic functions, and macros for creating, manipulating, and
calculating them automatically. The library contains a single module,
*duim-layouts*, from which all the interfaces described in this chapter
are exposed. `DUIM-Layouts Module`_ contains
complete reference entries for each exposed interface.

Layouts are sheet objects that determine how the interface elements are
presented on the screen. A layout object takes a number of children,
expressed as a vector, and lays out those children according to certain
constraints. Each child of a layout must be an instance of a DUIM class.

The class hierarchy for DUIM-Layouts
====================================

This section presents an overview of the available classes of layout,
and describes the class hierarchy present.

The <layout> class and its subclasses
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The base class for the majority of DUIM layouts is the :class:`<layout>` class,
which is itself a subclass of :class:`<sheet>`. All other layout-oriented
classes are subclasses of :class:`<sheet>`.

The immediate subclasses of :class:`<sheet>` that are exposed by the
DUIM-Layouts library are shown in `The class hierarchy for
DUIM-Layouts`_. Only :class:`<basic-composite-pane>`, :class:`<leaf-pane>`,
and :class:`<layout>` have any subclasses defined. See `Subclasses of
\<layout\>`_ for details of the subclasses of :class:`<layout>`.

.. table:: Overall class hierarchy for the DUIM-Layouts library

    +---------+------------------------+---------------------------------+
    | <sheet> |                        |                                 |
    +---------+------------------------+---------------------------------+
    |         | <basic-composite-pane> |                                 |
    +---------+------------------------+---------------------------------+
    |         |                        | <single-child-composite-pane>   |
    +---------+------------------------+---------------------------------+
    |         |                        | <multiple-child-composite-pane> |
    +---------+------------------------+---------------------------------+
    |         | <layout>               | See `Subclasses of \<layout\>`_ |
    +---------+------------------------+---------------------------------+
    |         | <leaf-pane>            |                                 |
    +---------+------------------------+---------------------------------+
    |         |                        | <null-pane>                     |
    +---------+------------------------+---------------------------------+
    |         | <drawing-pane>         |                                 |
    +---------+------------------------+---------------------------------+
    |         | <simple-pane>          |                                 |
    +---------+------------------------+---------------------------------+
    |         | <top-level-sheet>      |                                 |
    +---------+------------------------+---------------------------------+

All the actual layouts provided by the DUIM-Layouts library are
subclasses of the base :class:`<layout>` class, and are described in
`Subclasses of \<layout\>`_. In addition, a number of
different types of pane are supplied by the DUIM-Layouts library.

-  :class:`<basic-composite-pane>` This is a basic type of pane that is used to
   create any sheet that can contain children. It has two subclasses, one used
   for sheets that take only a single child, and one for sheets that can take
   several children.
-  :class:`<drawing-pane>` This type of pane is used to create sheets on which
   geometric objects are drawn, for example, using the function provided by the
   DUIM-Geometry module or the DUIM-Graphics module. For more information on
   these modules, see :doc:`geom`, and :doc:`graphics`, respectively.
-  :class:`<top-level-sheet>` This class is used for any sheets that are at the
   top level of the hierarchy of windows on the screen: that is, there is no
   other sheet that is the parent of an instance of :class:`<top-level-sheet>`.
-  :class:`<leaf-pane>` In contrast to :class:`<top-level-sheet>`, an instance of
   :class:`<leaf-pane>` cannot have any children, and is at the end of the
   hierarchy of windows on the screen.
-  :class:`<simple-pane>` This class is the most basic type of pane, and is
   used when no other more suitable class is available.

Subclasses of <layout>
^^^^^^^^^^^^^^^^^^^^^^

The subclasses of :class:`<layout>` are shown in :ref:`subclasses-of-the-layout-class`


.. _subclasses-of-the-layout-class:

.. table:: Subclasses of the <layout> class

    +----------+-------------------+---------------+
    | <layout> |                   |               |
    +----------+-------------------+---------------+
    |          | <row-layout>      |               |
    +----------+-------------------+---------------+
    |          | <column-layout>   |               |
    +----------+-------------------+---------------+
    |          | <fixed-layout>    |               |
    +----------+-------------------+---------------+
    |          | <pinboard-layout> |               |
    +----------+-------------------+---------------+
    |          | <stack-layout>    |               |
    +----------+-------------------+---------------+
    |          | <table-layout>    |               |
    +----------+-------------------+---------------+
    |          |                   | <grid-layout> |
    +----------+-------------------+---------------+

The layouts provided by DUIM fall roughly into two categories:

-  Layout classes that calculate the position and size of their children
   for you, subject to some constraints.
-  Layout classes that let you specify precisely the position of their
   children, and, optionally, the size of the children as well.

The classes of layout available are as follows:



-  :class:`<column-layout>` This class lays out its children in a single
   column, with all its children left-aligned by default.
-  :class:`<row-layout>` This class lays out its children in a single row.
-  :class:`<stack-layout>` This class lays out its children one on top of
   another, aligned at the top left corner by default. It is
   specifically for windows that contain a number of layouts, only one
   of which is visible at any one time, such as property sheets, tab
   controls, or wizards.
-  :class:`<table-layout>` This class lays out its children in a table,
   according to a specified number of rows and columns.
-  :class:`<pinboard-layout>` This does not constrain the position of its
   children in any way. It is up to you to position each child individually,
   like pins on a pinboard.
-  :class:`<fixed-layout>` This class is like :class:`<pinboard-layout>`, in that you
   must specify the position of each child. Unlike :class:`<pinboard-layout>`,
   however, you must also specify the size of each child.

In addition to the basic types of layout described above, a subclass of
:class:`<table-layout>` is provided, as follows:

-  :class:`<grid-layout>` This is a specialized version of :class:`<table-layout>`,
   in which all the cells in the table are forced to be the same size.

.. figure:: images/layouts-3.png
   :align: center

   Column, row, and pinboard layouts

DUIM-Layouts Module
===================

This section contains a complete reference of all the interfaces that
are exported from the *duim-layouts* module.

.. generic-function:: allocate-space
   :open:

   Allocates space within a layout for its children.

   :signature: allocate-space *pane* *width height* => ()

   :parameter pane: An instance of type :class:`<sheet>`.
   :parameter width: An instance of type ``<integer>``.
   :parameter height: An instance of type ``<integer>``.

   :description:

     Allocates space within a layout for its children. During the space
     allocation pass, a composite pane arranges its children within the
     available space and allocates space to them according to their space
     requirements and its own composition rules by calling :gf:`allocate-space`
     on each of the child panes. For example, :class:`<column-layout>` arranges
     all its children in a vertical column. The *width* and *height* arguments
     are the width and height of *pane* in device units, that is, pixels. These
     arguments give the amount of space into which all children must fit.

     This function actually calls :class:`do-allocate-space` to perform the
     calculations.  Client code may specialize :class:`do-allocate-space`, but
     not call it. Call :gf:`allocate-space` instead.

   See also

   - :gf:`do-allocate-space`

.. class:: <basic-user-pane>

   The class of basic user panes.

   :superclasses: :class:`<wrapping-layout-pane>`

   :keyword region: An instance of type :class:`<region>`. Default value: :const`$nowhere`.
   :keyword transform: An instance of type :class:`<transform>`. Default value: :const:`$identity-transform`.
   :keyword port: An instance of type *false-or(* :class:`<port>` *)*. Default value: ``#f``.
   :keyword style-descriptor: An instance of type ``false-or(<object>)``. Default value: ``#f``.
   :keyword help-context: An instance of type ``<object-table>``. Default value: ``make(<object-table>)``.
   :keyword help-source: An instance of type ``<object-table>``. Default value: ``make(<object-table>)``.

   :description:

     The class of basic user panes. This is the class that gets subclassed by
     :macro:`define pane`.

     You specify where on the screen the pane is to be displayed using the
     *region:* init-keyword. The region specified should be relative to the
     top left corner of the pane’s parent, since the pane must be displayed
     within the confines of its parent.

     If you wish the location of the pane to be transformed in some way, use
     the *transform:* init-keyword.

     If you wish to use a port other than the default port, use the *port:*
     init-keyword.

     You can specify the appearance for text in the pane using the
     *style-descriptor:* init-keyword.

     The *help-source:* and *help-context:* keywords let you specify pointers
     to valid information available in any online help you supply with your
     application. The *help-context:* keyword should specify a context-ID
     present in the online help. This context-ID identifies the help topic
     that is applicable to the current pane. The *help-source:* init-keyword
     identifies the source file in which the help topic identified by
     *help-context:* can be found. A list of context-IDs should be provided
     by the author of the online help system.

   See also

   - :macro:`define pane`

.. class:: <column-layout>
   :open:
   :abstract:
   :instantiable:

   The class of column layouts.

   :superclasses: :class:`<layout>`

   :keyword border: An instance of type ``<integer>``. Default value: 0.
   :keyword spacing: An instance of type ``<integer>``. Default value: 0.
   :keyword y-spacing: An instance of type ``<integer>``. Default value: 0.
   :keyword equalize-heights?: An instance of type ``<boolean>``. Default value: ``#f``.
   :keyword equalize-widths?: An instance of type ``<boolean>``. Default value: ``#f``.
   :keyword x-alignment: An instance of type ``one-of(#"left", #"right", #"center")``. Default value: ``#"left"``.
   :keyword ratios: An instance of type ``false-or(limited(<sequence>), of: <integer>))``. Default value: ``#f``.
   :keyword y-ratios: An instance of type ``false-or(limited(<sequence>), of: <integer>))``. Default value: ``#f``.

   :description:

     The class of column layouts. A column layout arranges its children in a
     column, automatically calculating the size and placement of each child
     within the specified parameters.

     .. figure:: images/layouts-4.png
        :align: center

     Three buttons arranged in a column layout

     The *border:* init-keyword provides a border of whitespace around the
     children in the layout, and the value of this init-keyword represents
     the size of the border in pixels. This basically has the same effect as
     using the macro ` <gadgets.htm#78138>`_ around the layout, except it
     uses a simpler syntax.

     The *spacing:* or *y-spacing:* init-keywords let you specify how much
     vertical space should be inserted, in pixels, between the children of
     the layout. These two init-keywords can be used interchangeably.

     If true, *equalize-heights?:* ensures that all the children of the
     layout have the same height.

     If true, *equalize-widths?:* ensures that all the children of the layout
     have the same width.

     By default, all the children of a column layout are left-aligned. You
     can specify that they should be right or center-aligned using the
     *x-alignment:* keyword.

     The *ratios:* or *y-ratios:* init-keywords let you specify the
     proportion of the total layout that should be taken up by each
     individual child. These two init-keywords can be used interchangeably.

     The value passed to *ratios:* needs to be a sequence of as many integers
     as there are children in the layout. Each child is then allocated the
     appropriate portion of vertical space in the layout. For example, if the
     value *#(1, 2, 3)* is specified for the *ratios:* init-keyword of a
     column layout containing three children, then the first child would
     claim a sixth of the available vertical space, the second child would
     claim a third of the vertical space, and the third child would claim
     half the vertical space, as shown in the diagram below.

     .. figure:: images/layouts-5.png
        :align: center
        :alt: 

   :example:

     .. code-block:: dylan

         contain(make(<column-layout>,
                      children: vector(make(<button>,
                                            label: "Hello"),
                                       make(<button>,
                                            label: "World"))
                      spacing: 100,
                      x-alignment: #"right",
                      ratios: #(1, 3)));

   See also

   - :class:`<grid-layout>`
   - :class:`<layout>`
   - :class:`<row-layout>`
   - :class:`<stack-layout>`
   - :class:`<table-layout>`
   - :gf:`vertically`


.. generic-function:: compose-space

   Returns the amount of space required for a specified child of a
   composite pane.

   :signature: compose-space *pane* #key *width height* => *space-req*

   :parameter pane: An instance of type :class:`<sheet>`.
   :parameter width: An instance of type ``<integer>``.
   :parameter height: An instance of type ``<integer>``.

   :value space-req: An instance of type :class:`<space-requirement>`.

   :description:

     Returns the amount of space required for *pane*, which is a child of
     a composite pane. During the space composition pass, a composite pane will
     typically ask each of its children how much space it requires by calling
     ``compose-space``. They answer by returning instances of
     :class:`<space-requirement>`. The composite pane then forms its own space
     requirement by composing the space requirements of its children according
     to its own rules for laying out its children.

     The value returned by ``compose-space`` is an instance of
     :class:`<space-requirement>` that represents how much space *pane*
     requires.

     The *width* and *height* arguments are real numbers that the
     ``compose-space`` method for a pane may use as "recommended" values for the
     width and height of the pane. These are used to drive top-down layout.

     This function actually calls :class:`do-compose-space` to perform the
     space calculations. Client code may specialize :class:`do-compose-space`
     but should not call it. Call ``compose-space`` instead.

   See also

   - :gf:`do-compose-space`
   - :class:`<space-requirement>`

.. generic-function:: current-pane

   Returns the current pane.

   :signature: current-pane => *pane*

   :parameter pane: An instance of type :class:`<sheet>`.

   :description:

     Returns the current pane: that is, the pane that has the mouse focus.

.. macro:: define pane
   :defining:

   Defines a new class of DUIM pane.

   :macrocall: define pane *name* ({*supers* },\*) {*slots-and-panes* } end

   :parameter name: A Dylan name*bnf*.
   :parameter supers: A Dylan name*bnf*.
   :parameter slots-and-panes: A Dylan body*bnf*.

   :description:

     This macro lets you define a new class of DUIM pane.

     The *name* argument represents the name of the new class of pane, and
     *supers* is a list of zero or more superclasses for the new class.
     Multiple superclass names are separated by commas.

     The *slots-and-panes* argument represents the slot information for the
     new class, together with any init-keywords and default values that the
     slots should take.

     Panes are sheets which represent a "useful unit" in a GUI. There is no
     protocol class called *<pane>*.

     A.  In most cases (such as when defining a frame using ``define frame``),
         a pane class groups existing gadgets (or panes) to form effectively a
         new gadget, without actually creating a new class of :class:`<gadget>`.
     B.  Sometimes, a pane class implements some complex output-only sheet.
     C.  Sometimes, a pane class implements the `See
         <sheet> <silica.htm#13118>`_ part of a ` <gadgets.htm#34543>`_.

     In general, a pane is best described as a *concrete* sheet.

   :example:

     .. code-block:: dylan

         define pane <my-pane> ()
           slot my-layout,
             init-keyword: layout:;
           slot my-exit-buttons,
             init-keyword: exit-buttons:;
         end pane <my-pane>;

   See also

   - :macro:`define frame`


.. generic-function:: do-allocate-space
   :open:

   Called by :gf:`allocate-space` to calculate space
   requirements for a pane.

   :signature: do-allocate-space *pane width height* => ()

   :parameter pane: An instance of type :class:`<sheet>`.
   :parameter width: An instance of type ``<integer>``.
   :parameter height: An instance of type ``<integer>``.

   :description:

     This function is called by :gf:`allocate-space` to
     calculate space requirements for a pane. When calculating space
     requirements for classes of pane you have defined yourself, you should
     add methods to this function, but not call it directly. Call
     :gf:`allocate-space` instead.

   See also

   - :gf:`allocate-space`

.. generic-function:: do-compose-space
   :open:

   Called by :gf:`compose-space` to calculate space
   requirements for a child.

   :signature: do-compose-space *pane* #key *width height* => *space-req*

   :parameter pane: An instance of type :class:`<sheet>`.
   :parameter width: An instance of type ``<integer>``.
   :parameter height: An instance of type ``<integer>``.

   :value space-req: An instance of type :class:`<space-requirement>`.

   :description:

     This function is called by :gf:`compose-space` to
     calculate space requirements for a child. When calculating space
     requirements for children in classes of pane you have defined yourself,
     you should specialize this function by adding methods for it. However,
     you should not call ``do-compose-space`` explicitly: call
     :gf:`compose-space` instead.

   :example:

     Assume that you have defined a new class of scroll bar as follows:

     .. code-block:: dylan

         define class <my-scroll-bar> (<scroll-bar>, <leaf-pane>)
         end class <test-scroll-bar>;

     A new method for do-compose-space can be defined as follows:

     .. code-block:: dylan

         define method do-compose-space
             (pane :: <my-scroll-bar>, #key width, height)
          => (space-req :: <space-requirement>)
           select (gadget-orientation(pane))
             #"horizontal" =>
               make(<space-requirement>,
                    width: width | 50,
                    min-width: 50,
                    max-width: $fill,
                    height: 10);
             #"vertical" =>
               make(<space-requirement>,
                    width: 10,
                    height: height | 50,
                    min-height: 50,
                    max-height: $fill);
           end
         end method do-compose-space;

   See also

   - :gf:`compose-space`


.. class:: <drawing-pane>
   :open:
   :abstract:
   :instantiable:

   The class of drawing panes.

   :superclasses: :class:`<layout>`

   :keyword display-function: An instance of type *false-or(<function>)*. Default value: ``#f``.

   :description:

     The class of drawing panes. This is a pane that provides event handling
     and a drawing surface. Note that a drawing pane can be wrapped around a
     layout pane to provide a medium for all the children of the layout pane.

     The *display-function:* init-keyword defines the display function for the
     pane. This gets called by the :gf:`handle-repaint` method for
     :class:`<simple-pane>`.

   See also

   - :gf:`handle-repaint`
   - :gf:`pane-display-function`
   - :class:`<simple-pane>`

.. constant:: $fill

   Default value for width and height init-keywords for layout panes.

   :type: :class:`<integer>`

   :value: 100000

   :description:

     This constant is used as the default value for any *width:* and
     *height:* init-keywords in layout panes.

     These defaults gives the intuitive behavior that specifying only the
     width or height of a pane causes it to be allocated at least that much
     space, and it may be given extra space if there is extra space in the
     layout. This default behavior can be changed if either the *min-width:*
     or *min-height:* init-keywords are specified explicitly.

   See also

   - :gf:`make`

.. class:: <fixed-layout>
   :open:
   :abstract:
   :instantiable:

   The class of fixed layouts.

   :superclasses: :class:`<layout>`

   :description:

     The class of fixed layouts. Fixed layouts are similar to pinboard
     layouts, in that the positioning and geometry of the children of a fixed
     layout are entirely determined by the programmer. You can place children
     at any point in a fixed layout, and the layout does not attempt to
     calculate an optimum position or size for any of them.

     Fixed layouts differ from pinboard layouts, however, in that any
     children placed in a fixed layout are left at exactly the size and
     position that they were created: pinboard layouts leave the positions of
     any children alone, but constrains the sizes of the children to obey any
     constraints they have been given.

     Fixed layouts are most useful if you know exactly what size and position
     every child in the layout should be.

   See also

   - :class:`<layout>`
   - :class:`<pinboard-layout>`

.. class:: <grid-layout>
   :open:
   :abstract:
   :instantiable:

   The class of grid layouts.

   :superclasses: :class:`<table-layout>`

   :keyword cell-space-requirement: An instance of type :class:`<space-requirement>`.

   :description:

     The class of grid layouts. A grid layout arranges its children in a
     grid, automatically calculating the size and placement of each child
     within the specified parameters.

     The *cell-space-requirement:* init-keyword lets you specify the
     preferred space requirement for any individual cell in the grid layout.

   See also

   - :class:`<column-layout>`
   - :class:`<row-layout>`
   - :class:`<stack-layout>`
   - :class:`<table-layout>`

.. macro:: horizontally
   :statement:

   Lays out a series of gadgets horizontally.

   :macrocall: horizontally ([*options* ]) {*panes* }+ end

   :parameter options: Dylan arguments*bnf*.
   :parameter panes: One or more occurrences of Dylan body*bnf*.

   :description:

     This macro lays a series of gadgets out horizontally, creating the
     necessary layouts for you automatically.

     The *options* are passed directly to the row layout, and thus can be any
     legitimate combinations of init-keywords for :class:`<row-layout>`. If no
     options are specified, then the default values for row layout are used.

     The *panes* argument consists of a number of Dylan expressions, each of
     which creates an instance of a gadget or layout that is to be included
     in the horizontal layout.

   :example:

     .. code-block:: dylan

         contain(horizontally ()
                   make(<button>, label: "Hello");
                   make(<button>, label: "World")
                 end);

   See also

   - :class:`<row-layout>`
   - :gf:`tabling`
   - :gf:`vertically`


.. class:: <layout>
   :open:
   :abstract:

   The superclass class of all layout classes.

   :superclasses: :class:`<sheet>`

   :keyword space-requirement: An instance of type :class:`<space-requirement>`. Required.
   :keyword width: An instance of type ``<integer>``. Required.
   :keyword height: An instance of type ``<integer>``. Required.
   :keyword min-width: An instance of type ``<integer>``. Default value: 0.
   :keyword min-height: An instance of type ``<integer>``. Default value: 0.
   :keyword max-width: An instance of type ``<integer>``. Default value: :const:`$fill`.
   :keyword max-height: An instance of type ``<integer>``. Default value: :const:`$fill`.
   :keyword resizable?: An instance of type ``<boolean>``. Default value: ``#t``.
   :keyword fixed-width?: An instance of type ``<boolean>``. Default value: ``#f``.
   :keyword fixed-height?: An instance of type ``<boolean>``. Default value: ``#f``.

   :description:

     The class of layouts. This is the basic class from which all other forms
     of layout inherit. You cannot create direct instances of this class.

     The *space-requirement:* init-keyword describes the space required for
     the layout. It is generally calculated automatically based on the values
     of the various width and height init-keywords, and the class of layout
     that is being created.

     The *width:*, *height:*, *min-width:*, *min-height:*, *max-width:*,
     and *max-height:* init-keywords between them describe the configuration
     of the layout. The default values for these init-keywords (where
     applicable) are set such that the layout always fills the available
     space in any given direction.

     Finally, three init-keywords are available that control how the layout
     is affected when the frame containing it is resized. All three
     init-keywords take boolean values. You can specify whether a layout is
     resizeable using the *resizable?:* init-keyword. If *fixed-width?:* or
     *fixed-height?:* are true, then the layout cannot be resized in the
     appropriate direction. Setting both to ``#t`` is equivalent to setting
     resizeable?: to ``#f``. Different subclasses of layout restrict the
     values of these init-keywords in different ways, such that, for
     instance, a row layout has a fixed height.

   See also

   - :class:`<column-layout>`
   - :class:`<grid-layout>`
   - :class:`<pinboard-layout>`
   - :class:`<row-layout>`
   - :class:`<stack-layout>`
   - :class:`<table-layout>`

.. generic-function:: layout-border

   Returns the amount of whitespace around the children in a layout.

   :signature: layout-border *layout* => *border*

   :parameter layout: An instance of type *type-union(* :class:`<row-layout>`, :class:`<column-layout>`, :class:`<table-layout>`, :class:`<grid-layout>`, :class:`<stack-layout>` *)*.

   :value border: An instance of type ``<integer>``.

   :description:

     Returns the amount of whitespace, in pixels, around the children in
     *layout*.

     Note that this function does not apply to pinboard layouts, because the
     positioning of the children in a pinboard layout is completely in the
     control of the programmer.

   See also

   - :gf:`layout-border-setter`

.. generic-function:: layout-border-setter

   Sets the amount of whitespace around the children in a layout.

   :signature: layout-border *border* *layout* => *border*

   :parameter border: An instance of type ``<integer>``.
   :parameter layout: An instance of type *type-union(* :class:`<row-layout>`, :class:`<column-layout>`, :class:`<table-layout>`, :class:`<grid-layout>`, :class:`<stack-layout>` *)*.

   :value border: An instance of type ``<integer>``.

   :description:

     Sets the amount of whitespace, in pixels, around the children in
     *layout*.

     You can also set this value a layout is created using the *border:*
     init-keyword.

     Note that this function does not apply to pinboard layouts, because the
     positioning of the children in a pinboard layout is completely in the
     control of the programmer.

   See also

   - :gf:`layout-border`

.. generic-function:: layout-equalize-heights?

   Returns true if the children of the specified layout are all the same
   height.

   :signature: layout-equalize-heights? *layout* => *equal?*

   :parameter layout: An instance of type *type-union(* :class:`<row-layout>`, :class:`<column-layout>` *)*.

   :value equal?: An instance of type ``<boolean>``.

   :description:

     Returns true if the children of *layout* are all the same height. The
     layout must be either a row or a column layout.

     You can only set this value when a layout is created, using the
     *equalize-heights?:* init-keyword. There is no equivalent setter
     function.

   See also

   - :gf:`layout-equalize-widths?`

.. generic-function:: layout-equalize-widths?

   Returns true if the children of the specified layout are all the same
   width.

   :signature: layout-equalize-widths? *layout* => *equal?*

   :parameter layout: An instance of type *type-union(* :class:`<row-layout>`, :class:`<column-layout>` *)*.

   :value equal?: An instance of type ``<boolean>``.

   :description:

     Returns true if the children of *layout* are all the same width. The
     layout must be either a row or a column layout.

     You can only set this value when a layout is created, using the
     *equalize-widths?:* init-keyword. There is no equivalent setter
     function.

   See also

   - :gf:`layout-equalize-heights?`

.. class:: <leaf-pane>
   :open:
   :abstract:

   The class of leaf panes.

   :superclasses: :class:`<sheet>`

   :description:

     The class of leaf panes. These are sheets that live at the leaf of the
     sheet tree that obeys the layout protocols.

     Subclass this class if you want to create a basic leaf pane.

     -  If you want to do output to it, mix in one of the
        :class:<sheet-with-medium-mixin>` classes.
     -  If you want to do input from it, min in one of the
        :class:`<sheet-with-event-queue>` classes.
     -  If you want to repaint it, mix in one of the
        :class:`<sheet-with-repainting-mixin>` classes.


.. method:: make
   :specializer: <space-requirement>

   Creates an instance of :class:`<space-requirement>`.

   :signature: make *space-requirement-class* #key *width min-width max-width height min-height max-height* => *space-req*

   :parameter space-requirement-class: The class :class:`<space-requirement>`.
   :parameter width: An instance of type ``<integer>``. Default value: :const:`$fill`.
   :parameter min-width: An instance of type ``<integer>``. Default value: *width*.
   :parameter max-width: An instance of type ``<integer>``. Default value: *width*.
   :parameter height: An instance of type ``<integer>``. Default value: :const:`$fill`.
   :parameter min-height: An instance of type ``<integer>``. Default value: *height*.
   :parameter max-height: An instance of type ``<integer>``. Default value: *height*.

   :value space-req: An instance of type :class:`<space-requirement>`.

   :description:

     Creates an instance of :class:`<space-requirement>`.

     The various width and height arguments let you control the values of
     corresponding init-keywords to :class:`<space-requirement>`, thereby
     control the width and height of a layout under various circumstances. See
     :class:`<space-requirement>`, for a full description of this behavior.

   See also

   - :const:`$fill`
   - :class:`<space-requirement>`

.. class:: <multiple-child-composite-pane>
   :open:
   :abstract:

   The class of composite panes that can have multiple children.

   :superclasses: :class:`<layout>`

   :description:

     The class of composite panes that can have multiple children. Subclass
     this class if you want to create a class of pane that can have more than
     one child.

   See also

   - :class:`<single-child-composite-pane>`

.. class:: <null-pane>
   :sealed:
   :instantiable:

   The class of null panes.

   :superclasses: :class:`<leaf-pane>`

   :description:

     The class of null panes. This class acts as a filler: use it when you
     need to "fill space" somewhere in a complex layout.

   See also

   - :class:`<spacing>`
   - :macro:`with-spacing`

.. generic-function:: pane-display-function

   Returns the function used to display the specified pane.

   :signature: pane-display-function *pane* => *pane-display-function*

   :parameter pane: An instance of type :class:`<sheet>`.

   :keyword pane-display-function: An instance of type ``false-or(<function>)``.

   :description:

     Returns the function used to display *pane*, where *pane* is any pane that
     can have a *display-function:* init-keyword specified. The *value*
     returned by *pane-display-function* is the value of the
     *display-function:* init-keyword.

     The display function gets called by the :gf:`handle-repaint` method for
     :class:`<simple-pane>` and :class:`<drawing-pane>`.

   See also

   - :class:`<drawing-pane>`

.. generic-function:: pane-layout

   Returns the layout that contains the specified pane in *define pane*.

   :signature: pane-layout *pane* => *layout-pane*

   :parameter pane: An instance of type :class:`<sheet>`.

   :value layout-pane: An instance of type :class:`<sheet>`.

   :description:

     Returns the layout that contains the specified pane in :macro:`define pane`.

   See also

   - :macro:`define pane`

.. class:: <pinboard-layout>
   :open:
   :abstract:
   :instantiable:

   The class of pinboard layouts.

   :superclasses: :class:`<layout>`

   :keyword stretchable?: An instance of type ``<boolean>``.

   :description:

     The class of pinboard layouts. Unlike other types of layout, pinboard
     layouts are unusual in that the positioning and geometry of the children
     of a pinboard layout are entirely determined by the programmer. You can
     place children at any point in a pinboard layout, and the pinboard
     layout does not attempt to calculate an optimum position or size for any
     of them.

     .. figure:: images/layouts-6.png
        :align: center

        Three buttons arranged in a pinboard layout

     A pinboard layout leaves the subsequent positions of any children placed
     in the layout alone. However, the size of each child is constrained
     according to any constraints that have been specified for those
     children. Compare this to fixed layouts, where the sizes of any children
     are not constrained in this way.

     Because the size of a pinboard layout’s children are constrained,
     pinboard layouts are most useful for placing sheets randomly in a
     layout, since DUIM ensures that the sheets remain a sensible size for
     their contents.

     If *stretchable?:* is true, then the pinboard layout can be resized
     dynamically as its parent is resized (for instance, by the user resizing
     a window on screen).

   See also

   - :class:`<fixed-layout>`
   - :class:`<layout>`

.. generic-function:: relayout-children

   Lays out the children of the specified sheet again.

   :signature: relayout-children *sheet* #key *port-did-it?* => ()

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter port-did-it?: An instance of type ``<boolean>``. Default value: ``#f``.

   :description:

     Lays out the children of *sheet* again.

   See also

   - :gf:`relayout-parent`

.. generic-function:: relayout-parent

   Lays out the parent of the specified sheet again.

   :signature: relayout-parent *sheet* #key *width height* => ()

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter width: An instance of type ``<integer>``.
   :parameter height: An instance of type ``<integer>``.

   :description:

     Lays out the parent of *sheet* again. If *width* and *height* are
     specified, then the parent is laid out in accordance with these
     dimensions.

   See also

   - :gf:`relayout-children`

.. class:: <row-layout>
   :open:
   :abstract:
   :instantiable:

   The class of row layouts.

   :superclasses: :class:`<layout>`

   :keyword border: An instance of type ``<integer>``. Default value: 0.

   :keyword x-spacing: An instance of type ``<integer>``. Default value: 0.
   :keyword spacing: An instance of type ``<integer>``. Default value: 0.
   :keyword equalize-heights?: An instance of type ``<boolean>``. Default value: ``#f``.
   :keyword equalize-widths?: An instance of type ``<boolean>``. Default value: ``#f``.
   :keyword y-alignment: An instance of type ``one-of(#"top", #"bottom", #"center")``. Default value: ``#"top"``.
   :keyword x-ratios: An instance of type ``false-or(<sequence>)``. Default value: ``#f``.
   :keyword ratios: An instance of type ``false-or(<sequence>)``. Default value: ``#f``.

   :description:

     The class of row layouts. A row layout arranges its children in a row,
     automatically calculating the size and placement of each child within
     the specified parameters.

     .. figure:: images/layouts-7.png
        :align: center

        Three buttons arranged in a row layout

     The *border:* init-keyword provides a border of whitespace around the
     children in the layout, and the value of this init-keyword represents
     the size of the border in pixels. This basically has the same effect as
     using the macro ` <gadgets.htm#78138>`_ around the layout, except it
     uses a simpler syntax.

     The *spacing:* or *x-spacing:* init-keywords let you specify how much
     horizontal space, in pixels, should be inserted between the children of
     the layout. These two init-keywords can be used interchangeably.

     If true, *equalize-heights?:* ensures that all the children of the
     layout have the same height.

     If true, *equalize-widths?:* ensures that all the children of the layout
     have the same width.

     By default, all the children of a row layout are aligned at the top. You
     can specify that they should be aligned at the bottom, or in the center,
     using the *y-alignment:* keyword.

     The *ratios:* or *x-ratios:* init-keywords let you specify the
     proportion of the total layout that should be taken up by each
     individual child. These two init-keywords can be used interchangeably.

     The value passed to *ratios:* needs to be a sequence of as many integers
     as there are children in the layout. Each child is then allocated the
     appropriate portion of horizontal space in the layout. For example, if
     the value ``#(1, 2, 3)`` is specified for the *ratios:* init-keyword of a
     row layout containing three children, then the first child would claim a
     sixth of the available horizontal space, the second child would claim a
     third of the horizontal space, and the third child would claim half the
     horizontal space, as shown in the diagram below.

     .. figure:: images/layouts-8.png
        :align: center

   :example:

     To make a row of buttons that are all the same size:

     .. code-block:: dylan

         contain(make(<row-layout>,
                      equalize-widths?: #t,
                      children: buttons))

   See also

   - :class:`<column-layout>`
   - :macro:`horizontally`
   - :class:`<layout>`
   - :class:`<grid-layout>`
   - :class:`<stack-layout>`
   - :class:`<table-layout>`


.. class:: <simple-pane>
   :open:
   :abstract:
   :instantiable:

   The class of simple panes.

   :superclasses: :class:`<layout>`

   :keyword display-function: An instance of type ``false-or(<function>)``. Default value: ``#f``.

   :description:

     The class of simple panes.

     The *display-function:* init-keyword defines the display function for
     the pane. This gets called by the :gf:`handle-repaint` method for
     :class:`<simple-pane>`.

   See also

   - :class:`<drawing-pane>`
   - :gf:`handle-repaint <silica.htm#28833>`
   - :gf:`pane-display-function`

.. class:: <single-child-composite-pane>
   :open:
   :abstract:

   The class of composite panes that can only have one child.

   :superclasses: :class:`<layout>`

   :description:

     The class of composite panes that can only have one child.

   See also

   - :class:`<multiple-child-composite-pane>`

.. class:: <space-requirement>
   :abstract:
   :instantiable:

   The class of all space requirement objects.

   :superclasses: :class:`<object>`

   :keyword width: An instance of type ``<integer>``. Default value: :const:`$fill`.
   :keyword min-width: An instance of type ``<integer>``. Default value: *width*.
   :keyword max-width: An instance of type ``<integer>``. Default value: *width*.
   :keyword height: An instance of type ``<integer>``. Default value: :const:`$fill`.
   :keyword min-height: An instance of type ``<integer>``. Default value: *height*.
   :keyword max-height: An instance of type ``<integer>``. Default value: *height*.
   :keyword label: An instance of type ``type-union(<string>,`` :class:`<image>` ``)``.

   :description:

     The class of all space requirement objects. This type of object is used
     to reserve space when it is required in a layout in order to accommodate
     gadgets or other layouts.

     The various init-keywords let you constrain the width and height of the
     object in a variety of ways.

     If no init-keywords are specified, the object returned tries to fill all
     the available space.

     Specifying *width:* or *height:* specifies the preferred width or height
     of the object.

     Specifying any of the *min-* or *max-* init-keywords lets you minimum
     and maximum width or height for the object.

     The following inequalities hold for all widths and heights:

     ``min-height: <= height: <= max-height:``
     ``min-width: <= width: <= max-width:``

     If either *min-width:* or *min-height:* is 0, the object is "infinitely
     shrinkable" in that direction. If either *max-width:* or *max-height:*
     is :const:`$fill`, the object is "infinitely
     stretchable" in that direction. The latter is a particularly useful way
     of ensuring that objects fill the available width, and can be used, say,
     to ensure that a series of buttons fill the entire width of the layout
     that they occupy.

     An example of the use of *max-width:* to force the size of a button to
     fit the available space can be found in the entry for :class:`<button>`.

     The *label:* init-keyword specifies a label which is measured to give
     the preferred width and height.

   :operations:

   - :gf:`space-requirement-height`
   - :gf:`space-requirement-max-height`
   - :gf:`space-requirement-max-width`
   - :gf:`space-requirement-min-height`
   - :gf:`space-requirement-min-width`
   - :gf:`space-requirement-width`

   :example:

     Given the following definition of a button class:

     .. code-block:: dylan

         define class <basic-test-button> (<leaf-pane>)
         end class <basic-test-button>;

     The following method for :gf:`do-compose-space`
     creates the necessary space requirements to accommodate the new button
     class in a layout.

     .. code-block:: dylan

         define method do-compose-space
             (pane :: <basic-test-button>, #key width, height)
          => (space-req :: <space-requirement>)
            ignore(width, height);
            make(<space-requirement>,
                 width: 40,
                 height: 15)
         end method do-compose-space;

   See also

   - :const:`$fill`

.. generic-function:: space-requirement?

   Returns true if the specified object is a space requirement.

   :signature: space-requirement? *object* => *boolean*

   :parameter object: An instance of type ``<object>``.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns true if *object* is an instance of :class:`<space-requirement>`.

   See also

   - :class:`<space-requirement>`

.. generic-function:: space-requirement-height

   Returns the preferred height of the specified space requirement.

   :signature: space-requirement-height *sheet* *space-req* => *height*

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter space-req: An instance of type :class:`<space-requirement>`.

   :value height: An instance of type ``<number>``.

   :description:

     Returns preferred the height of *space-req*. This is the value of the
     *height:* init-keyword that was passed when the object was created.

   See also

   - :gf:`space-requirement-max-height`
   - :gf:`space-requirement-min-height`

.. generic-function:: space-requirement-max-height

   Returns the maximum allowed height of the specified space requirement.

   :signature: space-requirement-max-height *sheet* *space-req* => *max-height*

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter space-req: An instance of type :class:`<space-requirement>`.

   :value max-height: An instance of type ``<number>``.

   :description:

     Returns the maximum allowed height of *space-req*. This is the value of
     the *max-height:* init-keyword that was passed when the object was
     created.

   See also

   - :gf:`space-requirement-height`
   - :gf:`space-requirement-min-height`

.. generic-function:: space-requirement-max-width

   Returns the maximum allowed width of the specified space requirement.

   :signature: space-requirement-max-width *sheet* *space-req* => *max-width*

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter space-req: An instance of type :class:`<space-requirement>`.

   :value max-width: An instance of type ``<number>``.

   :description:

     Returns the maximum allowed width of *space-req*. This is the value of
     the *max-width:* init-keyword that was passed when the object was
     created.

   See also

   - :gf:`space-requirement-min-width`
   - :gf:`space-requirement-width`

.. generic-function:: space-requirement-min-height

   Returns the minimum allowed height of the specified space requirement.

   :signature: space-requirement-min-height *sheet* *space-req* => *min-height*

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter space-req: An instance of type :class:`<space-requirement>`.

   :value min-height: An instance of type ``<number>``.

   :description:

     Returns the minimum allowed height of *space-req*. This is the value of
     the *min-height:* init-keyword that was passed when the object was
     created.

   See also

   - :gf:`space-requirement-height`
   - :gf:`space-requirement-max-height`

.. generic-function:: space-requirement-min-width

   Returns the minimum allowed width of the specified space requirement.

   :signature: space-requirement-min-width *sheet* *space-req* => *min-width*

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter space-req: An instance of type :class:`<space-requirement>`.

   :value min-width: An instance of type ``<number>``.

   :description:

     Returns the minimum allowed width of *space-req*. This is the value of
     the *min-width:* init-keyword that was passed when the object was
     created.

   See also

   - :gf:`space-requirement-max-width`
   - :gf:`space-requirement-width`

.. generic-function:: space-requirement-width

   Returns the preferred width of the specified space requirement.

   :signature: space-requirement-width *sheet* *space-req* => *width*

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter space-req: An instance of type :class:`<space-requirement>`.

   :value width: An instance of type ``<number>``.

   :description:

     Returns the preferred width of *space-req*. This is the value of the
     *width:* init-keyword that was passed when the object was created.

   See also

   - :gf:`space-requirement-max-width`
   - :gf:`space-requirement-min-width`

.. class:: <stack-layout>
   :open:
   :abstract:
   :instantiable:

   The class of stack layouts.

   :superclasses: :class:`<layout>`

   :keyword border: An instance of type ``<integer>``. Default value: 0.
   :keyword mapped-page: An instance of *<sheet>*.

   :description:

     The class of stack layouts. Stack layouts position all of their children
     at the top-left one on top of the other. The layout sizes itself to be
     large enough to fit the largest child. They are primarily useful for
     creating layouts that simulate sets of several pages where only one
     child is visible at a time, and all the others are withdrawn, and are
     used to control the layout of elements such as tab controls or wizard
     frames. To make a new page appear, you withdraw the current page, and
     then map the new page. The new page is automatically the correct size
     and in the correct position.

     The *border:* init-keyword provides a border of whitespace around the
     children in the layout, and the value of this init-keyword represents
     the size of the border in pixels. This basically has the same effect as
     using the macro ` <gadgets.htm#78138>`_ around the layout, except it
     uses a simpler syntax.

     The *mapped-page:* init-keyword allows you to assign a page to be mapped
     onto the screen when a stack layout is first created. If it is not
     specified, then the first page in the stack layout is mapped.

   See also

   - :class:`<column-layout>`
   - :class:`<grid-layout>`
   - :class:`<layout>`
   - :class:`<row-layout>`
   - :class:`<table-layout>`

.. generic-function:: stack-layout-mapped-page

   Returns the currently mapped page for a stack layout.

   :signature: stack-layout-mapped-page *stack-layout* => *page*

   :parameter stack-layout: An instance of :class:`<stack-layout>`.

   :value page: An instance of :class:`<sheet>`.

   :description:

     Returns the currently mapped *page* for the specified *stack-layout*.

.. generic-function:: stack-layout-mapped-page-setter

   Sets the mapped page for a stack layout.

   :signature: stack-layout-mapped-page *page* *stack-layout* => *page*

   :parameter page: An instance of :class:`<sheet>`.
   :parameter stack-layout: An instance of :class:`<stack-layout>`.

   :value page: An instance of :class:`<sheet>`.

   :description:

     Sets the mapped page for the specified *stack-layout* to *page*.

.. generic-function:: table-contents

   Returns the contents of the specified table.

   :signature: table-contents *table* => *contents*

   :parameter table: An instance of type :class:`<table-layout>`.

   :value contents: An instance of type :class:`<sheet>`.

   :description:

     Returns the contents of *table*.

   See also

   - :gf:`table-contents-setter`

.. generic-function:: table-contents-setter

   Sets the contents of the specified table.

   :signature: table-contents-setter *contents table* => *contents*

   :parameter contents: An instance of type :class:`<sheet>`.
   :parameter table: An instance of type :class:`<table-layout>`.

   :value contents: An instance of type :class:`<sheet>`.

   :description:

     Sets the contents of *table*.

   See also

   - :gf:`table-contents`

.. class:: <table-layout>
   :open:
   :abstract:
   :instantiable:

   The class of table layouts.

   :superclasses: :class:`<layout>`

   :keyword border: An instance of type ``<integer>``. Default value: 0.
   :keyword rows: An instance of type ``false-or(<integer>)``. Default value: ``#f``.
   :keyword columns: An instance of type ``false-or(<integer>)``. Default value: ``#f``.
   :keyword contents: An instance of type ``limited(<sequence>, of: limited(<sequence>, of: <sheet>))``.
   :keyword x-spacing: An instance of type ``<integer>``. Default value: 0.
   :keyword y-spacing: An instance of type ``<integer>``. Default value: 0.
   :keyword x-ratios: An instance of type ``false-or(<sequence>)``. Default value: ``#f``.
   :keyword y-ratios: An instance of type ``false-or(<sequence>)``. Default value: ``#f``.
   :keyword x-alignment: An instance of type ``one-of(#"left", #"right", #"center")``. Default value: ``#"left"``.
   :keyword y-alignment: An instance of type ``one-of(#"top", #"bottom", #"center")``. Default value: ``#"top"``.

   :description:

     The class of table layouts.

     The *border:* init-keyword provides a border of whitespace around the
     children in the layout, and the value of this init-keyword represents
     the size of the border in pixels. This basically has the same effect as
     using the macro ` <gadgets.htm#78138>`_ around the layout, except it
     uses a simpler syntax.

     The *rows:* and *columns:* init-keywords are used to specify the number
     of rows and columns for the table layout.

     The *contents:* init-keyword is used to specify the contents of each
     cell of the table. It should consist of a sequence of sequences of
     sheets. If *contents:* is not specified, you should supply the children
     of the table with a number of rows and columns. You should not supply
     both children and rows and columns, however.

     The *x-spacing:* and *y-spacing:* init-keywords let you specify how much
     vertical and horizontal space should be inserted, in pixels, between the
     children of the layout.

     The *x-ratios:* and *y-ratios:* init-keywords let you specify the
     proportion of the total horizontal and vertical space that should be
     taken up by each individual child.

     The value passed to *x-ratios:* needs to be a sequence of as many
     integers as there are columns of children in the layout. The value
     passed to *y-ratios:* needs to be a sequence of as many integers as
     there are rows of children in the layout. Each child is then allocated
     the appropriate portion of horizontal and vertical space in the layout,
     according to the combination of the values for these two keywords.

     The two init-keywords can be used on their own, or together, as
     described in the examples below.

     For example, if the value ``#(1, 2, 3)`` is specified for the *x-ratios:*
     init-keyword of a table layout containing three columns of children,
     then the first column would claim a sixth of the available horizontal
     space, the second column would claim a third of the horizontal space,
     and the third column would claim half the horizontal space, as shown in
     the diagram below.

     .. figure:: images/layouts-9.png
        :align: center

     Alternatively, if the value ``#(1, 2, 3)`` is specified for the
     *y-ratios:* init-keyword of a table layout containing three rows of
     children, then the first row would claim a sixth of the available
     vertical space, the second row would claim a third of the vertical
     space, and the third row would claim half the vertical space, as shown
     in the diagram below.

     .. figure:: images/layouts-10.png
        :align: center

     Finally, if both the *x-ratios:* and *y-ratios:* init-keywords are
     specified, then each child in the layout is affected individually, as
     shown in the diagram below.

     .. figure:: images/layouts-11.png
        :align: center

     By default, all the children of a table layout are left-aligned. You can
     specify that they should be right or center-aligned using the
     *x-alignment:* keyword.

     By default, all the children of a table layout are aligned at the top.
     You can specify that they should be aligned at the bottom, or in the
     center, using the *y-alignment:* keyword.

   :operations:

     - :gf:`table-contents`
     - :gf:`table-contents-setter`

   :example:

   .. code-block:: dylan

       *t* := make(<vector>, size: 9);
       for (i from 1 to 9)
         *t*[i - 1] := make(<button>, label: format-to-string("%d", i))
       end;

       contain(make(<table-layout>,
                    x-spacing: 10, y-spacing: 0,
                    children: *t*, columns: 3));

   See also

   - :class:`<column-layout>`
   - :class:`<grid-layout>`
   - :class:`<layout>`
   - :class:`<row-layout>`
   - :class:`<stack-layout>`
   - :macro:`tabling`

.. macro:: tabling
   :statement:

   Lays out a series of gadgets in a table.

   :macrocall: tabling ([*options* ]) {*panes* }+ end

   :parameter options: Dylan arguments*bnf*.
   :parameter panes: One or more occurrences of Dylan body*bnf*.

   :description:

     This macro lays a series of gadgets out in a table, creating the
     necessary layouts for you automatically.

     The *options* are passed directly to the table layout, and thus can be
     any legitimate combinations of init-keywords for
     :class:`<table-layout>`. If no options are specified, then
     the default values for table layout are used.

     The *panes* argument consists of a number of Dylan expressions, each of
     which creates an instance of a gadget or layout that is to be included
     in the vertical layout.

   See also

   - :macro:`horizontally`
   - :class:`<table-layout>`
   - :macro:`vertically`

.. class:: <top-level-sheet>
   :open:
   :abstract:
   :instantiable:

   The class of top level sheets.

   :superclasses: :class:`<layout>`

   :keyword display: An instance of type *false-or(* :class:`<display>` *)*. Default value: ``#f``.
   :keyword frame: An instance of type *false-or(* :class:`<frame>` *)*. Default value: ``#f``.
   :keyword frame-manager: An instance of type *false-or(* :class:`<frame-manager>` *)*. Default value: ``#f``.
   :keyword container: An instance of type ``false-or(<object>)``. Default value: ``#f``.
   :keyword container-region: An instance of type ``false-or(`` :class:`<region>` ``)``. Default value: ``#f``.

   :description:

     The class of top level sheets.

     The *container:* and *container-region:* init-keywords are for use in
     embedded frames, such as OLE objects in HTML browser windows. The
     *container:* init-keyword denotes the container itself, and
     *container-region:* is used to specify the region of the screen in which
     the container appears. Note that the container referred to is a native
     window system object.

.. macro:: vertically
   :statement:

   Lays out a series of gadgets vertically.

   :macrocall: vertically ([*options* ]) {*panes* }+ end

   :parameter options: Dylan arguments*bnf*.
   :parameter panes: One or more occurrences of Dylan body*bnf*.

   :description:

     This macro lays a series of gadgets out vertically, creating the
     necessary column layout for you automatically.

     The *options* are passed directly to the column layout, and thus can be
     any legitimate combinations of init-keywords for :class:`<column-layout>`.
     If no options are specified, then the default values for table layout are
     used.

     The *panes* argument consists of a number of Dylan expressions, each of
     which creates an instance of a gadget or layout that is to be included
     in the vertical layout.

   :example:

   .. code-block:: dylan

       contain(vertically (border: 5, equalize-widths: #t)
         make(<button>, label: "Hello");
         make(<button>, label: "World")
       end);

   See also

   - :class:`<column-layout>`
   - :macro:`horizontally`
   - :macro:`tabling`
