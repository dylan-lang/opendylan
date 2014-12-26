*******************
DUIM-Sheets Library
*******************

.. current-library:: duim-sheets
.. current-module:: duim-sheets

Overview
========

The elements that comprise a Graphical User Interface (GUI) are arranged
in a hierarchical ordering of object classes. At the top level of the
DUIM hierarchy there are three main classes, :class:`<sheet>`, :class:`<gadget>`,
and :class:`<frame>`, all of which are subclasses of :drm:`<object>`.

Sheets are the most basic visual GUI element, and can be any unique part
of a window: either a control such as a gadget or pane, or a layout.

-  Sheets have a visual presence: size, drawing context and so on.
-  The essential component of a sheet is its region; the area of the
   screen that the sheet occupies.
-  In practice sheets always also have a transform that maps the
   coordinate system of the sheet’s region to the coordinate system of
   its parent, because in practice all sheets maintain a pointer to a
   parent sheet.
-  Sheets can be output-only (labels, for example), input-output (most
   gadgets are like this) or even, in principle, input-only (for
   instance, you may need to provide some kind of simple drag’n’drop
   target).

Most of the sheet classes that you need to use on a day to day basis are
exposed in the DUIM-Gadgets and DUIM-Layouts libraries. The DUIM-Sheets
library contains the basic building blocks to implement these classes,
as well as providing the necessary functionality for you to create and
manipulate your own classes of sheet. In addition, DUIM-Sheets defines a
portable model for handling events. These event handling routines are
used by the DUIM-Frames, DUIM-Gadgets, and DUIM-Layouts libraries
without the need for any special action on your part. However, if you
need to define your own sheet classes, you will also need to handle
events occurring within those classes.

The DUIM-Sheets library contains a single module, *duim-sheets*, from which all
the interfaces described in this chapter are exposed.  `DUIM-Sheets Module`_
contains complete reference entries for each exposed interface.

A sheet is the basic unit in a DUIM window. Inside any window, sheets
are nested in a parent-child hierarchy. All sheets have the following
attributes:

-  :gf:`sheet-region`, expressed in the sheet’s own coordinate system.
-  :gf:`sheet-transform`, which maps the sheet’s coordinate system to the
   coordinate system of its parent.
-  :gf:`sheet-parent`, which is ``#f`` if the sheet has no parent.
-  :gf:`sheet-mapped?`, which tells if the sheet is visible on a display,
   ignoring issues of occluding windows.

The :gf:`sheet-transform` is an instance of a concrete subclass of
:class:`<transform>`. The :gf:`sheet-region` can be an instance of any concrete
subclass of :class:`<region>`, but is usually represented by the region class
:class:`<bounding-box>`.

Some sheets (menu bars, button boxes, or tool bars, for instance) also
have single or multiple children, in which case they have additional
attributes:

-  A :gf:`sheet-children` slot. This is a sequence of sheets. Each sheet in
   the sequence is a child of the current sheet.
-  Methods to add, remove, and replace a child.
-  Methods to map over children.

The functions that maintain the sheet’s region and transform are part of
the *sheet-geometry* protocol. Functions that maintain a sheet’s parent
and children are part of the *sheet-genealogy* protocol. Note that the
sheet geometry and genealogy protocols are independent. Adding a child
to a sheet that is larger than its parent does not cause the parent’s
region to grow. Shrinking the region of a parent does not cause the
children to shrink. You must maintain the region yourself, either by
explicitly setting the sheet’s region and transform, or by using the
layout facilities (:gf:`compose-space` and :gf:`allocate-space`).

As a convenience, there are some glue functions that mediate between
geometry and layout: :gf:`set-sheet-position`, :gf:`set-sheet-size`,
and :gf:`set-sheet-edges`.

Some classes of sheet can receive input. These have:

-  A :gf:`sheet-event-queue` slot.
-  Methods for :class:`<handle-event>`.

Sheets that can be repainted have methods for :gf:`handle-repaint`.

Sheets that can do output, have a :gf:`sheet-medium` slot.

Some sheets act as *controls* such as push buttons, scroll bars, and
sliders. These are represented by the :class:`<gadget>` class and its
subclasses.

Other sheets act as layout controls, which allow you to specify how the
elements in a sheet are laid out, whether they are placed vertically or
horizontally, whether they are left, right, or center-aligned, and so
on. These are represented by the :class:`<layout>` class and its subclasses,
and are described in :doc:`layouts`.

A sheet can be associated with a :class:`<display>`, which is an object that
represents a single display (or screen) on some display server.

A display (and all the sheets attached to the display) is associated
with a :class:`<port>` that is a connection to a display server. The port
manages:

-  a primary input device, such as a keyboard.
-  a pointing device, such as a mouse.
-  an event processor that *dispatches* events to the appropriate sheet.

There is a protocol for using the Windows clipboard. In order to
manipulate the Windows clipboard from within DUIM, the clipboard needs
to be locked, so that its contents can be manipulated. DUIM uses the
functions :gf:`open-clipboard` and :gf:`close-clipboard` to create and free
clipboard locks. The :gf:`open-clipboard` function creates an instance of
the class :class:`<clipboard>` which is used to hold the contents of the
clipboard for the duration of the lock. For general use of the
clipboard, use the macro :macro:`with-clipboard`, rather than calling
:gf:`open-clipboard` and :gf:`close-clipboard` explicitly. This lets you
manipulate the clipboard easily, sending the results of any code
evaluated to the clipboard.

Once a clipboard lock has been created, you can use :gf:`add-clipboard-data`
and :gf:`add-clipboard-data-as` to add data to the clipboard. Use
:gf:`get-clipboard-data-as` to query the contents of the clipboard, and use
:gf:`clear-clipboard` to empty the locked clipboard. Finally, use
:gf:`clipboard-data-available?` to see if the clipboard contains data of a
particular type.

You can put arbitrary Dylan objects onto the clipboard, and retrieve
them within the same process. This gives you the ability to cut and
paste more interesting pieces of an application within the application’s
own domain than would normally be possible.

The DUIM GUI test suite contains a demonstration of how to use the
clipboard in DUIM, in the file

::

    sources/duim/tests/gui/clipboard.dylan

in the Open Dylan installation directory.

The class hierarchy for DUIM-Sheets
===================================

This section presents an overview of the available classes exposed by
the DUIM-Sheets library, and describes the class hierarchy present.

The base classes in the DUIM-Sheets library
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The base classes for the majority of subclasses exposed from the
DUIM-Sheets library are :class:`<sheet>` and :class:`<event>`, although a number of
additional subclasses of :drm:`<object>` are also exposed.

The base classes exposed by the DUIM-Sheets library are shown in the following
table . Only :class:`<sheet>`, and :class:`<event>` have any subclasses
defined. An :class:`<event>` is an object representing some sort of event. See
`Subclasses of \<event\>`_ for details of the subclasses of :class:`<event>`.

Overall class hierarchy for the DUIM-Sheets library

+----------+-----------------+-----------+
| <object> |                 |           |
+----------+-----------------+-----------+
|          | <sheet>         |           |
+          +-----------------+-----------+
|          |                 | <display> |
+          +-----------------+-----------+
|          | <port>          |           |
+          +-----------------+-----------+
|          | <clipboard>     |           |
+          +-----------------+-----------+
|          | <caret>         |           |
+          +-----------------+-----------+
|          | <pointer>       |           |
+          +-----------------+-----------+
|          | <medium>        |           |
+          +-----------------+-----------+
|          | <frame-manager> |           |
+          +-----------------+-----------+
|          | <event>         |           |
+----------+-----------------+-----------+

-  :class:`<sheet>` As already mentioned, a sheet is the basic unit of window
   applications, and they can be nested in a parent-child hierarchy. A
   subclass of sheet is provided — :class:`<display>` — which is an object that
   represents a single display (or screen) on a display server. All
   sheets can be attached to a display.
-  :class:`<port>` A port is a connection to a display server. A display,
   together with all the sheets attached to it, is associated with a
   port, which manages a primary input device, such as a keyboard, a
   pointing device, such as a mouse, and an event processor that
   dispatches events to the appropriate sheet.
-  :class:`<clipboard>` This class is used as a clipboard that can be used to
   hold information temporarily while it is transferred from one sheet
   to another, or between applications. Clipboards provide support for
   the standard *Cut*, *Copy*, and *Paste* commands common in most
   applications.
-  :class:`<caret>` and :class:`<pointer>` These two classes form an interface
   between the keyboard and the display, and the pointing device and the
   display, respectively.
-  The :class:`<caret>` represents the position on screen that characters typed
   on the keyboard will be placed. This is often a position in a
   document.
-  The :class:`<pointer>` represents the position of the pointing device on the
   screen, and thus shows the area that will be affected by any events
   generated with the pointing device, such as pressing or clicking one
   of the buttons on the device.
-  :class:`<pointer-drag-event>` The class of events where the pointer for the
   pointing device attached to the computer is moving, and one of the buttons
   on the pointing device is pressed down as well. The effects of this event
   are rather like a combination of the :class:`<button-press-event>` and
   :class:`<pointer-motion-event>` classes. For more information about these
   and other pointer event classes, see `Subclasses of \<device-event\>`_.
-  :class:`<pointer-enter-event>` This event is used to describe the event
   where the pointer for the pointing device enters a specified area of the
   screen, such as a sheet. For more information about these and other pointer
   event classes, see `Subclasses of \<device-event\>`_.
-  :class:`<medium>` A medium represents a destination for drawn or written
   output. It has several items associated with it, such as a drawing
   plane, foreground and background colors, and default line and text
   styles.
-  :class:`<frame-manager>` A frame manager represents the "look and feel" of
   a frame. This controls standard interface elements for the platform you are
   delivering on, such as the appearance and behavior of title bars, borders,
   menu commands and scroll bars. Unless you are developing for more than one
   platform, you do not need to be too concerned with frame managers, since you
   will only using the default frame manager.

Subclasses of <event>
^^^^^^^^^^^^^^^^^^^^^

The following table shows the subclasses of the :class:`<event>` class that are
exposed by the DUIM-Sheets library.

+---------+---------------+-------------------------+---------------------------------------+
| <event> |               |                         |                                       |
+---------+---------------+-------------------------+---------------------------------------+
|         | <frame-event> |                         |                                       |
+         +---------------+-------------------------+---------------------------------------+
|         |               | <port-terminated-event> |                                       |
+         +---------------+-------------------------+---------------------------------------+
|         |               | <timer-event>           |                                       |
+         +---------------+-------------------------+---------------------------------------+
|         | <sheet-event> |                         |                                       |
+         +---------------+-------------------------+---------------------------------------+
|         |               | <device-event>          |                                       |
+         +---------------+-------------------------+---------------------------------------+
|         |               | <window-event>          | See `Subclasses of \<device-event\>`_ |
+         +---------------+-------------------------+---------------------------------------+
|         |               |                         | <window-configuration-event>          |
+         +---------------+-------------------------+---------------------------------------+
|         |               |                         | <window-repaint-event>                |
+---------+---------------+-------------------------+---------------------------------------+

The classes of event that are exposed by the DUIM-Sheets library fall
into two categories:

-  Events that occur in frames: subclasses of the :class:`<frame-event>` class
-  Events that occur in sheets: subclasses of the :class:`<sheet-event>` class

Most subclasses of :class:`<frame-event>` are exposed by the DUIM-Frames
library. See :doc:`frames`, for full details about these
subclasses. However, two subclasses of :class:`<frame-event>` are exposed by
the DUIM-Sheets library:

- :class:`<port-terminated-event>` This class represents the event of a port
  — a connection to a display — being terminated.
- :class:`<timer-event>` This is the class of any event that is timed.

Subclasses of :class:`<sheet-event>` fall into two categories:

-  Device events that occur to devices attached to the computer
   (typically the keyboard and the pointing device). These are described
   in `Subclasses of <device-event>`_.
-  Window events that occur in a window.

Events that occur in a window are subclasses of :class:`<window-event>`. Two
such events are supplied:

- :class:`<window-configuration-event>` This event occurs whenever the
  configuration of sheets in a window needs to be recalculated. This may occur
  in property frames, for example, when clicking on the available tabs to
  display different pages of information.  Sometimes, dialog boxes have buttons
  that allow you to show or hide additional details, which are themselves
  displayed in an extra pane at the bottom or on the right hand side of the
  dialog. Clicking on such a button would also create
  a :class:`<window-configuration-event>`, as the additional pane would need to
  be displayed or hidden, forcing a recalculation of the layout of the sheets
  in the frame.
- :class:`<window-repaint-event>` This event occurs whenever a region of
  a window needs to be repainted. This may occur when refreshing a chart or
  drawing in a frame.

Subclasses of <device-event>
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following table shows the subclasses of the :class:`<device-event>` class
that are exposed by the DUIM-Sheets library.  Device events, broadly speaking,
describe any event that can occur on a device connected to the computer.

+----------------+------------------+------------------------+--------------------------+------------------------+
| <device-event> |                  |                        |                          |                        |
+----------------+------------------+------------------------+--------------------------+------------------------+
|                | <pointer-event>  |                        |                          |                        |
+----------------+------------------+------------------------+--------------------------+------------------------+
|                |                  | <pointer-button-event> |                          |                        |
+----------------+------------------+------------------------+--------------------------+------------------------+
|                |                  |                        | <button-press-event>     |                        |
+----------------+------------------+------------------------+--------------------------+------------------------+
|                |                  |                        | <button-release-event>   |                        |
+----------------+------------------+------------------------+--------------------------+------------------------+
|                |                  |                        | <button-click-event>     |                        |
+----------------+------------------+------------------------+--------------------------+------------------------+
|                |                  |                        | <double-click-event>     |                        |
+----------------+------------------+------------------------+--------------------------+------------------------+
|                |                  |                        | <pointer-drag-event>     |                        |
+----------------+------------------+------------------------+--------------------------+------------------------+
|                |                  | <pointer-motion-event> |                          |                        |
+----------------+------------------+------------------------+--------------------------+------------------------+
|                |                  |                        | <pointer-drag-event>     |                        |
+----------------+------------------+------------------------+--------------------------+------------------------+
|                |                  |                        | <pointer-boundary-event> |                        |
+----------------+------------------+------------------------+--------------------------+------------------------+
|                | <keyboard-event> |                        |                          |  <pointer-exit-event>  |
+----------------+------------------+------------------------+--------------------------+------------------------+
|                |                  | <key-press-event>      |                          |  <pointer-enter-event> |
+----------------+------------------+------------------------+--------------------------+------------------------+
|                |                  | <key-release-event>    |                          |                        |
+----------------+------------------+------------------------+--------------------------+------------------------+

*Note:* The :class:`<pointer-drag-event>` class is a subclass of both
:class:`<pointer-button-event>` and :class:`<pointer-motion-event>`.

Device events fall into two distinct categories:

-  Keyboard events that occur on the keyboard attached to the computer:
   subclasses of :class:`<keyboard-event>`
-  Pointer events that occur on the pointing device attached to the
   computer: subclasses of :class:`<pointer-event>`

There are two classes of keyboard event. The classes :class:`<key-press-event>`
and :class:`<key-release-event>` describe the events that occur when any key on
the keyboard is pressed or released, respectively.

There are three classes of pointer event, some of which provide a number
of subclasses. Note that there are another two classes of pointer event
that are immediate subclasses of :drm:`<object>`. These are described in
`The base classes in the DUIM-Sheets library`_.

- :class:`<pointer-button-event>` These events occur whenever there is any
  activity on one of the buttons on the pointing device. Several subclasses of
  this class are provided.
- :class:`<pointer-exit-event>` This is an event that occurs when the pointer
  leaves a specified area such as a sheet.
- :class:`<pointer-motion-event>` This class of events occur when the pointer
  is in motion. There is one subclass provided,
  :class:`<pointer-boundary-event>`, for the specific case when the motion of
  the pointer causes the boundary of a sheet to be crossed.  *Note:* Unlike
  :class:`<pointer-drag-event>`, no button needs to be pressed on the attached
  pointing device.

The subclasses provided for :class:`<pointer-button-event>` are as follows:

- :class:`<button-press-event>` This event occurs when any button on the
  pointing device is pressed down by the user. Note that this is distinct from
  :class:`<button-click-event>`, described below.
- :class:`<button-release-event>`
  This event occurs when any previously pressed button on the pointing device
  is released by the user.
- :class:`<button-click-event>` This event occurs when any button on the
  pointing device is pressed down by the user and then released again within
  a certain time frame.  An instance of this class is created if the creation
  of an instance of :class:`<button-press-event>` is closely followed by the
  creation of an instance of :class:`<button-release-event>`. The necessary
  time frame is dictated by the configuration of your computer. In Windows, for
  example, this time can be set using the Control Panel.
- :class:`<double-click-event>` This event occurs when a button is clicked
  twice within a certain time frame. An instance of this class is created if
  the creation of an instance of :class:`<button-click-event>` is closely
  followed by the creation of another instance of
  :class:`<button-click-event>`. The necessary time frame is dictated by the
  configuration of your computer.

DUIM-Sheets Module
==================

This section contains a complete reference of all the interfaces that
are exported from the *duim-sheets* module.

.. generic-function:: =

   Returns true if the specified gestures are the same.

   :signature: = *gesture1* *gesture2* => *equal?*

   :parameter gesture1: An instance of type :class:`<gesture>`.
   :parameter gesture2: An instance of type :class:`<gesture>`.

   :value equal?: An instance of type ``<boolean>``.

   :description:

     Returns true if *gesture1* and *gesture2* are the same.

   See also

   - :gf:`gesture-spec-equal`

.. generic-function:: add-child

   Adds a child to the specified sheet.

   :signature: add-child *sheet* *child* #key *index* => *sheet*

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter child: An instance of type :class:`<sheet>`.
   :parameter index: An instance of type *false-or(<integer>)*.

   :value sheet: An instance of type :class:`<sheet>`.

   :description:

     Adds a child to *sheet*.

   See also

   - :gf:`remove-child`
   - :gf:`replace-child`

.. generic-function:: add-clipboard-data

   Adds data to a clipboard.

   :signature: add-clipboard-data *clipboard* *data* => *success?*

   :parameter clipboard: An instance of :class:`<clipboard>`.
   :parameter data: An instance of :drm:`<object>`.

   :value success?: An instance of :class:`<boolean>`.

   :description:

     This generic function adds *data* to *clipboard*. It returns ``#t`` if
     *data* was successfully added to the clipboard.

.. generic-function:: add-clipboard-data-as

   Coerces data to a particular type and then adds it to a clipboard.

   :signature: add-clipboard-data *type clipboard data* => *success?*

   :parameter type: An instance of *type-union(<symbol>,* *<type>)*.
   :parameter clipboard: An instance of :class:`<clipboard>`.
   :parameter data: An instance of :drm:`<object>`.

   :value success?: An instance of :class:`<boolean>`.

   :description:

     This generic function adds *data* to *clipboard*, first coercing it to
     *type*. The argument *type* is an instance of *type-union(<symbol>,*
     *<type>)*. It returns ``#t`` if *data* was successfully added to the
     clipboard.

.. constant:: $alt-key

   A constant that represents the ALT key on the keyboard.

   :type: :class:`<integer>`

   :value: :const:`$meta-key`

   :description:

     A constant that represents the ALT key on the keyboard. This is set to
     the same value as the META key, to deal with the case where the META key
     is not present on the keyboard.

   See also

   - :const:`$control-key`
   - :const:`$hyper-key`
   - :const:`$meta-key`
   - :gf:`modifier-key-index`
   - :gf:`modifier-key-index-name`
   - :const:`$modifier-keys`
   - :const:`$option-key`
   - :const:`$shift-key`
   - :const:`$super-key`

.. generic-function:: beep

   :signature: beep *drawable* => ()

   :parameter drawable: An instance of type *type-union(* :class:`<sheet>`, :class:`<medium>` *)*.

   :description:

.. generic-function:: boundary-event-kind

   Returns the kind of boundary event for the specified event.

   :signature: boundary-event-kind *event* => *symbol*

   :parameter event: An instance of type :class:`<event>`.

   :value symbol: An instance of type *one-of(#"ancestor", #"virtual", #"inferior", #"nonlinear", #"nonlinear-virtual", #f)*.

   :description:

     Returns the kind of boundary event for *event*. These correspond to the
     detail members for X11 enter and exit events.

   See also

   - :class:`<pointer-boundary-event>`

.. function:: button-index

   Returns the index for the specified pointer button.

   :signature: button-index *button* => *index*

   :parameter button: An instance of type *one-of(#"left", #"middle", #"right")*.

   :value index: An instance of type ``<integer>``.

   :description:

     Returns the index for *button*, a button on the pointer device
     connected to the computer (typically a mouse). The *index* returned is
     either 0, 1, or 2, for the left, middle, or right buttons, respectively.

   See also

   - :gf:`button-index-name`
   - :const:`$pointer-buttons`

.. function:: button-index-name

   Returns the button on the pointer device represented by the specified
   index.

   :signature: button-index-name *index* => *button*

   :parameter index: An instance of type ``<integer>``.

   :value button: An instance of type *one-of(#"left", #"middle", #"right")*.

   :description:

     Returns the button on the pointer device connected to the computer
     (typically a mouse) represented by *index*. The *index* is either 0, 1,
     or 2, these values corresponding to the left, middle, or right buttons,
     respectively.

   See also

   - :gf:`button-index`
   - :const:`$pointer-buttons`

.. class:: <button-press-event>
   :sealed:
   :instantiable:

   The class of events representing button presses.

   :superclasses: :class:`<pointer-button-event>`

   :description:

     The class of events representing button presses. A instance of this
     class is generated if a button press is detected, and a second button
     press is not detected within the allowed interval for a double-click
     event. Alternatively, if a double-click event has just been generated,
     then an instance of this class is generated when a subsequent button
     press is detected.

   See also

   - :class:`<button-release-event>`
   - :class:`<double-click-event>`

.. class:: <button-release-event>
   :sealed:
   :instantiable:

   The class of events representing button releases.

   :superclasses: :class:`<pointer-button-event>`

   :description:

     The class of events representing button releases. An instance of this
     class is generated if the mouse button is released after a period of
     being pressed, for example, at the end of a drag and drop maneuver.

   See also

   - :class:`<button-press-event>`

.. class:: <caret>
   :abstract:
   :instantiable:

   The class of carets.

   :superclasses: :drm:`<object>`

   :keyword sheet: An instance of type false-or(:class:`<sheet>`).
   :keyword x: An instance of type ``<integer>``. Default value: 0.
   :keyword y: An instance of type ``<integer>``. Default value: 0.
   :keyword width: An instance of type ``<integer>``. Default value: 0.
   :keyword height: An instance of type ``<integer>``. Default value: 0.

   :description:

     The class of carets, or text cursors. A cursor can actually be any
     instance of :class:`<symbol>` or any instance of :class:`<image>`.

     The *sheet:* init-keyword specifies the sheet that the caret is
     positioned in.

     The *x:*, *y:*, *width:*, and *height:* init-keywords define the
     position and size of the caret, with respect to the sheet that contains
     it. The position of the caret is measured from the top left of the
     sheet. All units are measured in pixels.

   :operations:

     - :gf:`caret-position`
     - :gf:`caret-sheet`
     - :gf:`caret-size`
     - :gf:`caret-visible?`
     - :gf:`caret-visible?-setter`
     - :gf:`display`
     - :gf:`port`
     - :gf:`set-caret-position`

   See also

   - :gf:`caret-position`
   - :gf:`caret-sheet`
   - :gf:`caret-size`
   - :gf:`caret-visible?`
   - :class:`<cursor>`

.. generic-function:: caret-position

   Returns the position of the specified caret.

   :signature: cursor-position *caret* => *x y*

   :parameter caret: An instance of type :class:`<caret>`.

   :value x: An instance of type ``<integer>``.
   :value y: An instance of type ``<integer>``.

   :description:

     Returns the position of *caret*.

   See also

   - :gf:`caret-sheet`
   - :gf:`caret-size`

.. generic-function:: caret-sheet

   Returns the sheet that owns the specified caret.

   :signature: cursor-sheet *caret* => *sheet*

   :parameter caret: An instance of type :class:`<caret>`.

   :value sheet: An instance of type :class:`<sheet>`.

   :description:

     Returns the sheet that owns *caret*.

   See also

   - :gf:`caret-position`
   - :gf:`caret-size`

.. generic-function:: caret-size

   Returns the size of the specified caret.

   :signature: cursor-size *caret* => *width height*

   :parameter caret: An instance of type :class:`<caret>`.

   :value width: An instance of type ``<integer>``.
   :value height: An instance of type ``<integer>``.

   :description:

     Returns the size of *caret*.

   See also

   - :gf:`caret-position`
   - :gf:`caret-sheet`

.. generic-function:: caret-visible?

   Returns true if the specified caret is visible.

   :signature: cursor-visible? *caret* => *visible?*

   :parameter caret: An instance of type :class:`<caret>`.

   :value visible?: An instance of type ``<boolean>``.

   :description:

     Returns true if *caret* is visible.

   See also

   - :class:`<cursor>`
   - :gf:`caret-visible?-setter`

.. generic-function:: caret-visible?-setter

   Specifies whether or not the specified caret is visible.

   :signature: cursor-visible?-setter *visible? caret* => *boolean*

   :parameter visible?: An instance of type ``<boolean>``.
   :parameter caret: An instance of type :class:`<caret>`.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Specifies whether or not *caret* is visible.

   See also

   - :class:`<cursor>`
   - :gf:`caret-visible?`

.. generic-function:: child-containing-position

   Returns the topmost child of the specified sheet that occupies a
   specified position.

   :signature: child-containing-position *sheet x y* => *value*

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter x: An instance of type ``<real>``.
   :parameter y: An instance of type ``<real>``.

   :value value: An instance of type false-or(:class:`<sheet>`).

   :description:

     Returns the topmost enabled direct child of *sheet* whose region
     contains the position *(* *x* *,* *y* *)*. The position is expressed in
     the coordinate system used by *sheet*.

   See also

   - :gf:`children-overlapping-region`
   - :gf:`do-children-containing-position`

.. generic-function:: children-overlapping-region

   Returns any children of the specified sheet whose regions overlap a
   specified region.

   :signature: children-overlapping-region *sheet region* => *sheets*

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter region: An instance of type `<region> <geom.htm#79228>`_.

   :value sheets: An instance of type limited(``<sequence>``, of: :class:`<sheet>`).

   :description:

     Returns the list of enabled direct children of *sheet* whose region
     overlaps *region*.

   See also

   - :gf:`child-containing-position`
   - :gf:`do-children-overlapping-region`

.. generic-function:: choose-color

   Displays the built-in color dialog for the target platform.

   :signature: choose-color #key *frame owner title documentation exit-boxes name default* => *color*

   :parameter frame: An instance of type :class:`<frame>`. Default value: ``#f``.
   :parameter owner: An instance of type :class:`<sheet>`. Default value: ``#f``.
   :parameter title: An instance of type :class:`<string>`.
   :parameter documentation: An instance of type :class:`<string>`.
   :parameter exit-boxes: An instance of type :drm:`<object>`.
   :parameter name: An instance of type :drm:`<object>`.
   :parameter default: An instance of type :drm:`<object>`.

   :value color: An instance of type :class:`<color>`

   :description:

     Displays the built-in color dialog for the target platform, which allows
     the user to choose a color from the standard palette for whatever
     environment the application is running in.

     .. figure:: images/sheets-3.png
        :align: center

        The standard Choose Color dialog

     If the *frame* argument is specified, the top-level sheet of *frame*
     becomes the owner of the dialog.

     Alternatively, you can specify the owner directly using the *owner*
     argument, which takes an instance of :class:`<sheet>` as its value.

     By default, both *frame* and *owner* are ``#f``, meaning the dialog has
     no owner. You should not specify both of these values.

     If you wish, you can specify a *title* for the dialog; this is displayed
     in the title bar of the frame containing the dialog.

     Example

     The following example illustrates how you can define a class of frame
     that contains a button that displays the Choose Color dialog, using the
     pre-built dialog classes for your target environment. The frame also
     contains an ellipse whose color is set to the color chosen from the
     dialog.

     .. code-block:: dylan

          define frame <color-dialog-frame> (<simple-frame>)
            pane ellipse-pane (frame)
            make(<ellipse-pane>, foreground: $red);
            pane choose-color-button (frame)
            make(<menu-button>,
                 label: "Choose Color...",
                 documentation:
                 "Example of standard 'choose color' dialog",
                 activate-callback:
                 method (button)
                   let color = choose-color(owner: frame);
                   color & change-ellipse-color(frame, color)
                 end);
          end frame <color-dialog-frame>;

   See also

   - :gf:`choose-directory`
   - :gf:`choose-file`
   - :gf:`notify-user`

.. generic-function:: choose-directory

   Displays the built-in directory dialog for the target platform.

   :signature: choose-directory #key *frame owner title documentation exit-boxes name default* => *locator*

   :parameter frame: An instance of type :class:`<frame>`. Default value: ``#f``.
   :parameter owner: An instance of type :class:`<sheet>`. Default value: ``#f``.
   :parameter title: An instance of type :class:`<string>`.
   :parameter documentation: An instance of type :class:`<string>`.
   :parameter exit-boxes: An instance of type :drm:`<object>`.
   :parameter name: An instance of type :drm:`<object>`.
   :parameter default: An instance of type :drm:`<object>`.

   :value locator: An instance of type *type-union(<string>, <locator>)*.

   :description:

     Displays the built-in directory dialog for the target platform, which
     allows the user to choose a directory from any of the local or networked
     drives currently connected to the computer.

     If the *frame* argument is specified, the top-level sheet of *frame*
     becomes the owner of the dialog.

     Alternatively, you can specify the owner directly using the *owner*
     argument, which takes an instance of :class:`<sheet>` as its value.

     By default, both *frame* and *owner* are ``#f``, meaning the dialog has
     no owner. You should not specify both of these values.

     If you wish, you can specify a *title* for the dialog; this is displayed
     in the title bar of the frame containing the dialog.

     Example

     The following example illustrates how you can define a class of frame
     that contains a button that displays the Choose Directory dialog, using
     the pre-built dialog classes for your target environment.

     .. code-block:: dylan

         define frame <directory-dialog-frame> (<simple-frame>)
           pane dir-file-button (frame)
             make(<menu-button>,
                  label: "Choose directory ...",
                  documentation:
                  "Example of standard 'Choose Dir' dialog",
                  activate-callback:
                  method (button)
                    let dir = choose-directory (owner: frame);
                    if (dir)
                      frame-status-message(frame) := format-to-string 
                                                     ("Chose directory %s", dir);
                    end
                  end);
           pane dir-layout (frame)
             vertically ()
             frame.dir-file-button;
           end;
           layout (frame) frame.dir-layout;
           keyword title: = "Choose directory example";
         end frame <directory-dialog-frame>;

   See also

   - :gf:`choose-color`
   - :gf:`choose-file`
   - :gf:`notify-user`

.. generic-function:: choose-file

   Displays the built-in file dialog for the target platform.

   :signature: choose-file #key *frame owner title documentation exit-boxes name default* => *locator*

   :parameter frame: An instance of type :class:`<frame>`. Default value: ``#f``.
   :parameter owner: An instance of type :class:`<sheet>`. Default value: ``#f``.
   :parameter title: An instance of type :class:`<string>`.
   :parameter documentation: An instance of type :class:`<string>`.
   :parameter direction: An instance of type *one-of(#"input", #"output")*. Default value: *#"input"*.
   :parameter filters: An instance of type *limited(<sequence>, of: <sequence>)*.
   :parameter exit-boxes: An instance of type :drm:`<object>`.
   :parameter name: An instance of type :drm:`<object>`.
   :parameter default: An instance of type :class:`<string>`.

   :value locator: An instance of type :class:`<string>`.

   :description:

     Displays the built-in file dialog for the target platform, which allows
     the user to choose a file from any of the local or networked drives
     currently connected to the computer. The function returns the name of
     the file chosen by the user.

     .. figure:: images/sheets-4.png
        :align: center

        Typical appearance of a choose-file dialog

     If the *frame* argument is specified, the top-level sheet of *frame*
     becomes the owner of the dialog.

     Alternatively, you can specify the owner directly using the *owner*
     argument, which takes an instance of :class:`<sheet>` as its value.

     By default, both *frame* and *owner* are ``#f``, meaning the dialog has
     no owner. You should not specify both of these values.

     If you wish, you can specify a *title* for the dialog; this is displayed
     in the title bar of the frame containing the dialog.

     The *direction* argument is used to specify whether the file chosen is
     being opened (that is, information in the file is loaded into the
     application) or saved to (that is, information in the application is
     being saved to a file on disk).

     The *filters* argument lets you specify the file filters that should be
     offered to the user in the dialog. These filters are typically available
     in a drop-down list box, and let the user display only certain types of
     file, such as text files. Each filter is described as a sequence of
     strings:

     1.  The first string in the sequence is a description of the files that
         are displayed when this filter is chosen.
     2.  Each subsequent string is a regular expression that describes which
         files to display in the dialog.

     For example, to specify a filter that lets the user choose to display
     either text files, HTML files, or Dylan source files, the following
     sequence should be passed to the filters argument:

     .. code-block:: dylan

          #[#["Text files", "\*.txt", "\*.text"],

          #["HTML files", "\*.htm", "\*.html"],

          #["Dylan files", "\*.dylan"]

     Here, text files are defined as any file with a filename suffix of
     *.txt* or *.text*, HTML files have filenames with a suffix of either
     *.htm* or *.html*, and Dylan files have filenames with a suffix of
     *.dylan*.

     The *default* argument is used to specify a default filename to pass to
     the dialog. This is a convenient way to suggest a file in which some
     information may be saved, or a file to be loaded into an application.

     Example

     The following example illustrates how you can define a class of frame
     that contains buttons to display both Open and Save As dialogs, using
     the pre-built dialog classes for your target environment.

     .. code-block:: dylan

          define frame <open-save-dialog-frame> (<simple-frame>)
            pane open-file-button (frame)
              make(<menu-button>,
                   label: "Open...",
                   documentation:
                   "Example of standard file 'Open' dialog",
                   activate-callback:
                   method (button)
                     let file = choose-file(direction: #"input",
                                            owner: frame);
                     if (file)
                       frame-status-message(frame) := format-to-string
                                                      ("Opened file %s", file);
                     end
                   end);
            pane save-file-button (frame)
              make(<menu-button>,
                   label: "Save As...",
                   documentation:
                   "Example of standard file 'Save As' dialog",
                   activate-callback:
                   method (button)
                     let file = choose-file(direction: #"output",
                                            owner: frame);
                     if (file)
                       frame-status-message(frame) := format-to-string
                                                      ("Saved file as %s", file);
                     end
                   end);
          end frame <open-save-dialog-frame>;

   See also

   - :gf:`choose-color`
   - :gf:`choose-directory`
   - :gf:`notify-user`

.. generic-function:: choose-from-dialog

   Prompt the user to choose from a collection of items, using a dialog
   box.

   :signature: choose-from-dialog *items*  #key *frame owner title value default-item label-key value-key selection-mode gadget-class gadget-options width height foreground background text-style* => *value success?*

   :parameter items: An instance of *type-union(* :class:`<sequence>`, :class:`<menu>` *)*.
   :parameter frame: An instance of type :class:`<frame>`. Default value: ``#f``.
   :parameter owner: An instance of type :class:`<sheet>`. Default value: ``#f``.
   :parameter title: An instance of type :class:`<string>`.
   :parameter default-item: An instance of type :drm:`<object>`.
   :parameter label-key: An instance of type ``<function>``. Default value: *identity*.
   :parameter value-key: An instance of type ``<function>``. Default value: *identity*.
   :parameter selection-mode: An instance of :class:`<symbol>`. Default value: *#"single"*.
   :parameter gadget-class: An instance of type :class:`<gadget>`.
   :parameter gadget-options: An instance of type :class:`<sequence>`.
   :parameter foreground: An instance of type :class:`<ink>`.
   :parameter background: An instance of type :class:`<ink>`.
   :parameter text-style: An instance of type :class:`<text-style>`.

   :value value: An instance of type :drm:`<object>`.
   :value success?: An instance of type ``<boolean>``.

   :description:

     Prompt the user to choose from a collection of *items*, using a dialog
     box. This generic function is similar to *choose-from-menu*.

     The function returns the values chosen by the user, and a boolean value:
     ``#t`` if a value was chosen, ``#f`` if nothing was chosen. Unlike
     *choose-from-menu*, the user can choose several values if desired,
     depending on the value of *selection-mode*, described below.

     At its most basic, *choose-from-dialog* can be passed a simple sequence
     of items, as follows:

     .. code-block:: dylan

         choose-from-dialog(range(from: 1, to: 10));

     However, any of a large number of keywords can be supplied to specify
     more clearly the dialog that is created. A range of typical options can
     be chosen: The *frame* keyword specifies a frame whose top level sheet
     becomes the owner of the menu. Alternatively, you can specify this top
     level sheet explicitly using *owner*. The *title* keyword lets you
     choose a title for the dialog. By default, each of these values is ``#f``
     .

     In addition, *choose-from-dialog* offers options similar to collection
     gadgets, that can act upon the items specified. The *default-item*
     keyword lets you specify an item that is returned by default if no value
     is chosen explicitly (thereby ensuring that *success?* will always be
     ``#t``). You can also specify a *value-key* or *label-key* for the items
     in the menu. The *selection-mode* keyword is used to make the dialog box
     single-selection (the user can only choose one value) or
     multiple-selection (the user can return any number of values). The
     default value of *selection-mode* is *#"single"*. By specifying
     *selection-mode: #"multiple"*, the user can choose several values from
     the dialog box. The *gadget-class* keyword lets you specify which type
     of collection gadget is displayed in the dialog box. This lets you, for
     example, display a list of check boxes or radio boxes. Finally,
     *gadget-options* let you specify a set of options to be applied to the
     collection gadgets in the dialog box.

     You can also configure the appearance of the menu itself. The *width*
     and *height* keywords let you set the size of the menu. The *foreground*
     and *background* keywords let you set the text color and the menu color
     respectively. The *text-style* keyword lets you specify a font to
     display the menu items.

   See also

   - :gf:`choose-from-menu`

.. generic-function:: choose-from-menu

   Prompt the user to choose from a collection of items, using a pop-up
   menu.

   :signature: choose-from-menu *items*  #key *frame owner title value default-item label-key value-key width height foreground background text-style multiple-sets?* => *value success?*

   :parameter items: An instance of *type-union(* :class:`<sequence>`, :class:`<menu>` *)*.
   :parameter frame: An instance of type :class:`<frame>`. Default value: ``#f``.
   :parameter owner: An instance of type :class:`<sheet>`. Default value: ``#f``.
   :parameter title: An instance of type :class:`<string>`. Default value: ``#f``.
   :parameter default-item: An instance of type :drm:`<object>`.
   :parameter label-key: An instance of type ``<function>``. Default value: *identity*.
   :parameter value-key: An instance of type ``<function>``. Default value: *identity*.
   :parameter foreground: An instance of type :class:`<ink>`.
   :parameter background: An instance of type :class:`<ink>`.
   :parameter text-style: An instance of type :class:`<text-style>`.

   :value value: An instance of type :drm:`<object>`.
   :value success?: An instance of type ``<boolean>``.

   :description:

     Prompt the user to choose from a collection of *items*, using a pop-up
     menu.This generic function is similar to *choose-from-dialog*.

     The function returns the value chosen by the user, and a boolean value:
     ``#t`` if a value was chosen, ``#f`` if nothing was chosen.

     At its most basic, *choose-from-menu* can be passed a simple sequence of
     items, as follows:

     .. code-block:: dylan

         choose-from-menu(#(1, 2, 3));

     However, any of a large number of keywords can be supplied to specify
     more clearly the menu that is created. A range of typical options can be
     chosen: The *frame* keyword specifies a frame whose top level sheet
     becomes the owner of the menu. Alternatively, you can specify this top
     level sheet explicitly using *owner*. The *title* keyword lets you
     choose a title for the dialog. By default, each of these values is ``#f``
     .

     In addition, *choose-from-menu* offers options similar to collection
     gadgets, that can act upon the items specified. The *default-item*
     keyword lets you specify an item that is returned by default if no value
     is chosen explicitly (thereby ensuring that *success?* will always be
     ``#t``). You can also specify a *value-key* or *label-key* for the items
     in the menu.

     Finally, you can configure the appearance of the menu itself. The
     *width* and *height* keywords let you set the size of the menu. The
     *foreground* and *background* keywords let you set the text color and
     the menu color respectively. The *text-style* keyword lets you specify a
     font to display the menu items.

   See also

   - :gf:`choose-from-dialog`

.. generic-function:: choose-text-style

   Displays the built-in font dialog for the target platform, thereby
   letting the user choose a font.

   :signature: choose-text-style #key *frame* *owner* *title* => *font*

   :parameter frame: An instance of type :class:`<frame>`. Default value: ``#f``.
   :parameter owner: An instance of type :class:`<sheet>`. Default value: ``#f``.
   :parameter title: An instance of type :class:`<string>`. Default value: ``#f``.

   :value font: An instance of :class:`<text-style>`.

   :description:

     Displays the built-in font dialog for the target platform, thereby
     letting the user choose a font.

     The *frame* keyword specifies a frame whose top-level sheet becomes the
     owner of the menu. Alternatively, you can specify this top level sheet
     explicitly using *owner*. The *title* keyword lets you choose a title
     for the dialog. By default, each of these values is ``#f``.

     If you wish, you can specify a *title* for the dialog; this is an
     instance of :class:`<string>` and is displayed in the title bar of the frame
     containing the dialog. If you do not specify *title*, then DUIM uses
     the default title for that type of dialog on the target platform.

.. generic-function:: clear-box

   Clears a box-shaped area in the specified drawable.

   :signature: clear-box *drawable left top right bottom* => ()
   :signature: clear-box\* *drawable region* => ()

   :parameter drawable: An instance of type type-union(:class:`<sheet>`, :class:`<medium>`).

   The following arguments are specific to *clear-box*.

   :parameter left: An instance of type :class:`<coordinate>`.
   :parameter top: An instance of type :class:`<coordinate>`.
   :parameter right: An instance of type :class:`<coordinate>`.
   :parameter bottom: An instance of type :class:`<coordinate>`.

   The following argument is specific to *clear-box\**.

   :parameter region: An instance of type :class:`<region>`.

   :description:

     Clears a box-shaped area in the specified drawable, removing anything
     that was drawn in that region.

     The function *clear-box\** is identical to *clear-box*, except that it
     passes composite objects, rather than separate coordinates, in its
     arguments. You should be aware that using this function may lead to a
     loss of performance.

.. generic-function:: clear-clipboard

   Clears the contents of a clipboard.

   :signature: clear-clipboard *clipboard* => ()

   :parameter clipboard: An instance of :class:`<clipboard>`.

   :description:

     Clears the contents of *clipboard*, which represents the locked
     clipboard.

.. class:: <clipboard>
   :open:
   :abstract:

   The class of clipboard objects.

   :description:

     The class of clipboard objects. An instance of this class is created when
     a clipboard lock is created, and is used to hold the contents of the
     Windows clipboard for the duration of the lock. You do not need to worry
     about creating instances of :class:`<clipboard>` yourself, since this is
     handled automatically by the macro :macro:`with-clipboard`.

   See also

   - :gf:`add-clipboard-data`
   - :gf:`add-clipboard-data-as`
   - :gf:`clear-clipboard`
   - :gf:`clipboard-data-available?`
   - :gf:`clipboard-sheet`
   - :gf:`clipboard-owner`
   - :gf:`close-clipboard`
   - :gf:`get-clipboard-data-as`
   - :gf:`open-clipboard`
   - :gf:`with-clipboard`

.. generic-function:: clipboard-data-available?

   Returns false if there is any data of a particular type on a clipboard.

   :signature: clipboard-data-available? *type clipboard* => *available?*

   :parameter type: An instance of *type-union(<symbol>,* *<type>)*.
   :parameter clipboard: An instance of :class:`<clipboard>`.

   :value available?: An instance of :class:`<boolean>`.

   :description:

     Returns ``#f`` if and only if there is any data of type *type* on the
     clipboard. The argument *type* is an instance of *type-union(<symbol>,*
     *<type>)*.

   See also

   - :gf:`add-clipboard-data`
   - :gf:`add-clipboard-data-as`
   - :class:`<clipboard>`
   - :gf:`get-clipboard-data-as`

.. generic-function:: clipboard-sheet

   Returns the sheet with the clipboard lock.

   :signature: clipboard-sheet *clipboard* => *sheet*

   :parameter clipboard: An instance of :class:`<clipboard>`.

   :value sheet: An instance of :class:`<sheet>`.

   :description:

     Returns the sheet with the clipboard lock.

   See also

   - :class:`<clipboard>`

.. generic-function:: clipboard-owner

   Returns the sheet that owns the current clipboard data.

   :signature: clipboard-owner *clipboard* => *owner*

   :parameter clipboard: An instance of :class:`<clipboard>`.

   :value owner: An instance of :class:`<sheet>`.

   :description:

     Returns the sheet that owns the current clipboard data.

   See also

   - :class:`<clipboard>`

.. function:: close-clipboard

   Closes the current clipboard lock for a sheet on a port.

   :signature: close-clipboard *port sheet* => ()

   :parameter port: An instance of :class:`<port>`.
   :parameter sheet: An instance of :class:`<sheet>`.

   :description:

     Closes the current clipboard lock for *sheet* on *port*. A clipboard
     lock needs to be closed safely after it the clipboard has been used, to
     free the clipboard for further use.

     You should not normally call *close-clipboard* yourself to close a
     clipboard lock. Use the macro :macro:`with-clipboard` to create and free the
     lock for you.

   See also

   - :class:`<clipboard>`
   - :gf:`with-clipboard`

.. constant:: $control-key

   A constant that represents the CONTROL key on the keyboard.

   :type: :class:`<integer>`

   :value: ash(1, %modifier\_base + 1);

   :description:

     A constant that represents the CONTROL key on the keyboard.

   See also

   - :const:`$alt-key`
   - :const:`$hyper-key`
   - :const:`$meta-key`
   - :gf:`modifier-key-index`
   - :gf:`modifier-key-index-name`
   - :const:`$modifier-keys`
   - :const:`$option-key`
   - :const:`$shift-key`
   - :const:`$super-key`

.. class:: <cursor>

   The class of cursor objects.

   Equivalent: ``type-union(<symbol>, <image>)``

   :description:

     The class of cursor objects. The cursor is the small image that is used
     to display the location of the mouse pointer at any time. A cursor can
     actually be any instance of :class:`<symbol>` or any instance of
     :class:`<image>`.

   :operations:

     - :gf:`pointer-cursor-setter`
     - :gf:`set-caret-position`
     - :gf:`sheet-pointer-cursor-setter`

   See also

   - :class:`<caret>`
   - :gf:`cursor?`

.. generic-function:: cursor?

   Returns true if the specified object is a cursor.

   :signature: cursor? *object* => *cursor?*

   :parameter object: An instance of type :drm:`<object>`.

   :value cursor?: An instance of type ``<boolean>``.

   :description:

     Returns true if *object* is a cursor. In practice, you can create a cursor
     from any instance of :class:`<symbol>` or :class:`<image>`.

   See also

   - :class:`<cursor>`

.. function:: default-port

   Returns the default port for the specified server.

   :signature: default-port #key *server-path* => *port*

   :parameter server-path: An instance of type :class:`<vector>`. Default value: *#(#"local")*.

   :parameter port: An instance of type false-or(:class:`<port>`).

   :description:

     Returns the default port for server specified by *server-path*.

   See also

   - :gf:`default-port-setter`
   - :gf:`destroy-port`

.. function:: default-port-setter

   Sets the default port.

   :signature: default-port-setter *port* => *port*

   :parameter port: An instance of type :class:`<port>`. Default value: ``#f``.

   :value port: An instance of type :class:`<port>`.

   :description:

     Sets the default port.

   See also

   - :gf:`default-port`
   - :gf:`destroy-port`

.. generic-function:: destroy-port

   Destroys the specified port.

   :signature: destroy-port *port* => ()

   :parameter port: An instance of type :class:`<port>`.

   :description:

     Destroys *port*.

   See also

   - :gf:`default-port`
   - :gf:`default-port-setter`

.. generic-function:: destroy-sheet

   Destroys the specified sheet.

   :signature: destroy-sheet *sheet* => ()

   :parameter sheet: An instance of type :class:`<sheet>`.

   :description:

     Destroys *sheet*.

.. class:: <device-event>
   :open:
   :abstract:

   The class of device events.

   :superclasses: :class:`<sheet-event>`

   :keyword sheet: An instance of type :class:`<sheet>`.
   :keyword modifier-state: An instance of type ``<integer>``. Default value: 0.

   :description:

     The class of device events.

     The *modifier-state:* init-keyword is used to record the state of the
     device at the time the event occurred.

   :operations:

     - :gf:`event-modifier-state`

.. class:: <display>
   :open:
   :abstract:

   The class of displays.

   :superclasses: :class:`<sheet>`

   :keyword orientation: An instance of type *one-of(#"vertical", #"horizontal", #"default")*. Default value: *#"default"*.
   :keyword units: An instance of type *one-of(#"device", #"mm", #"pixels")*. Default value: *#"device"*.

   :description:

     The class of displays. An instance of :class:`<display>` is an object that
     represents a single display (or screen) on some display server. Any sheet
     can be attached to an instance of :class:`<display>`, and a display, and
     all the sheets attached to it, are associated with a :class:`<port>` that
     is a connection to a display server.

     The *orientation:* init-keyword is used to specify the orientation of a
     display.

     The *units:* init-keyword is used to specify the units in which height
     and width measurements are made with respect to the display. The default
     is whatever units are standard for the display device (usually pixels).

   :operations:

     - :gf:`display`
     - :gf:`display?`
     - :gf:`display-depth`
     - :gf:`display-height`
     - :gf:`display-mm-height`
     - :gf:`display-mm-width`
     - :gf:`display-orientation`
     - :gf:`display-pixel-height`
     - :gf:`display-pixels-per-point`
     - :gf:`display-pixel-width`
     - :gf:`display-units`
     - :gf:`display-width`

   See also

   - :gf:`display`
   - :gf:`display?`
   - :gf:`display-depth`
   - :gf:`display-height`
   - :gf:`display-orientation`
   - :gf:`display-units`
   - :gf:`display-width`
   - :class:`<port>`
   - :class:`<sheet>`

.. generic-function:: display

   Returns the display for the specified object.

   :signature: display *object* => *display*

   :parameter object: An instance of type :drm:`<object>`.

   :parameter display: An instance of type false-or(:class:`<display>`).

   :description:

     Returns the display used to display *object*.

   See also

   - :class:`<display>`
   - :gf:`frame-manager`
   - :gf:`port`

.. generic-function:: display?

   Returns true if the specified object is a display.

   :signature: display? *object* => *display?*

   :parameter object: An instance of type :drm:`<object>`.

   :value display?: An instance of type ``<boolean>``.

   :description:

     Returns true if *object* is a display.

   See also

   - :class:`<display>`

.. generic-function:: display-depth

   Returns the color depth of the specified display.

   :signature: display-depth *display* => *depth*

   :parameter display: An instance of type :class:`<display>`.

   :value depth: An instance of type ``<integer>``.

   :description:

     Returns the color depth of *display*. By default, the color depth of
     any display is assumed to be 8.

   See also

   - :gf:`display-height`
   - :gf:`display-orientation`
   - :gf:`display-width`

.. generic-function:: display-height

   Returns the height of the specified display.

   :signature: display-height *display* #key *units* => *height*

   :parameter display: An instance of type :class:`<display>`.
   :parameter units: An instance of *one-of(#"device", #"mm", #"pixels")*. Default value: *#"device"*.

   :value height: An instance of type :class:`<number>`.

   :description:

     Returns the height of *display*, in device-independent units. If
     *units* is specified, then the value returned is converted into the
     appropriate type of units.

   See also

   - :gf:`display-depth`
   - :gf:`display-mm-height`
   - :gf:`display-orientation`
   - :gf:`display-pixel-height`
   - :gf:`display-units`
   - :gf:`display-width`

.. generic-function:: display-mm-height

   Returns the height of the specified display in millimeters.

   :signature: display-mm-height *display* => *height*

   :parameter display: An instance of type :class:`<display>`.

   :value height: An instance of type :class:`<number>`.

   :description:

     Returns the height of *display* in millimeters. This is equivalent to
     calling :gf:`display-height` with the *units* argument set to *#"mm"*.

   See also

   - :gf:`display-height`
   - :gf:`display-mm-width`
   - :gf:`display-pixel-height`
   - :gf:`display-units`

.. generic-function:: display-mm-width

   Returns the width of the specified display in millimeters.

   :signature: display-mm-width *display* => *width*

   :parameter display: An instance of type :class:`<display>`.

   :value width: An instance of type :class:`<number>`.

   :description:

     Returns the width of *display* in millimeters. This is equivalent to
     calling :gf:`display-width` with the *units* argument set to *#"mm"*.

   See also

   - :gf:`display-mm-height`
   - :gf:`display-pixel-width`
   - :gf:`display-units`
   - :gf:`display-width`

.. generic-function:: display-orientation

   Returns the orientation of the specified display.

   :signature: display-orientation *display* => *orientation*

   :parameter display: An instance of type :class:`<display>`.

   :value orientation: An instance of type *one-of(#"vertical", #"horizontal", #"default")*.

   :description:

     Returns the orientation of *display*. Unless specified otherwise, the
     orientation of any display is *#"default"*.

   See also

   - :gf:`display-depth`
   - :gf:`display-height`
   - :gf:`display-width`

.. generic-function:: display-pixel-height

   Returns the height of the specified display in pixels.

   :signature: display-pixel-height *display* => *height*

   :parameter display: An instance of type :class:`<display>`.

   :value height: An instance of type ``<integer>``.

   :description:

     Returns the height of *display* in pixels. This is equivalent to calling
     :gf:`display-height` with the *units* argument set to *#"pixels"*.

   See also

   - :gf:`display-height`
   - :gf:`display-mm-height`
   - :gf:`display-pixel-width`
   - :gf:`display-units`

.. generic-function:: display-pixels-per-point

   Returns the number of pixels per point for the specified display.

   :signature: display-pixels-per-point *display* => *number*

   :parameter display: An instance of type :class:`<display>`.

   :value number: An instance of type :class:`<number>`.

   :description:

     Returns the number of pixels per point for *display*.

   See also

   - :gf:`display-pixel-height`
   - :gf:`display-pixel-width`
   - :gf:`display-units`

.. generic-function:: display-pixel-width

   Returns the width of the specified display in pixels.

   :signature: display-pixel-width *display* => *width*

   :parameter display: An instance of type :class:`<display>`.

   :value width: An instance of type ``<integer>``.

   :description:

     Returns the height of *display* in pixels. This is equivalent to calling
     :gf:`display-width` with the *units* argument set to *#"pixels"*.

   See also

   - :gf:`display-mm-width`
   - :gf:`display-pixel-height`
   - :gf:`display-units`
   - :gf:`display-width`

.. generic-function:: display-units

   Returns the default units for the specified display.

   :signature: display-units *display* => *value*

   :parameter display: An instance of type :class:`<display>`.

   :value value: An instance of type *one-of(#"device", #"pixels", #"mm")*.

   :description:

     Returns the default units for *display*. These are the units in which
     height and width measurements are made, both for the display, and for
     any children of the display. Unless otherwise specified, the value
     returned is *#"default"*, so as to maintain a device-independent
     measurement as far as possible.

   See also

   - :gf:`display-height`
   - :gf:`display-width`

.. generic-function:: display-width

   Returns the width of the specified display.

   :signature: display-width *display* #key *units* => *width*

   :parameter display: An instance of type :class:`<display>`.
   :parameter units: An instance of *one-of(#"device", #"mm", #"pixels")*. Default value: *#"device"*.

   :value width: An instance of type :class:`<number>`.

   :description:

     Returns the width of *display*, in device-independent units. If *units*
     is specified, then the value returned is converted into the appropriate
     type of units.

   See also

   - :gf:`display-depth`
   - :gf:`display-height`
   - :gf:`display-mm-width`
   - :gf:`display-orientation`
   - :gf:`display-pixel-width`
   - :gf:`display-units`

.. generic-function:: do-children-containing-position

   Invokes a function on any children that occupy a specified position in
   the specified sheet.

   :signature: do-children-containing-position *function sheet x y* => ()

   :parameter function: An instance of type ``<function>``.
   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter x: An instance of type ``<real>``.
   :parameter y: An instance of type ``<real>``.

   :description:

     Invokes *function* on any children that occupy position *(* *x* *,* *y*
     *)* in *sheet*. This is used by :gf:`child-containing-position` to
     ascertain which children occupy the position. The function
     :gf:`child-containing-position` then decides which of the children
     returned is the topmost direct enabled child.

   See also

   - :gf:`child-containing-position`

.. generic-function:: do-children-overlapping-region

   Invokes a function on any children of the specified sheet whose regions
   overlap a specified region.

   :signature: do-children-overlapping-region *function sheet region* => ()

   :parameter function: An instance of type ``<function>``.
   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter region: An instance of type :class:`<region>`.

   :description:

     Invokes *function* on any children of *sheet* whose regions overlap
     *region*. This is used by :gf:`children-overlapping-region` to ascertain
     which children overlap *region*.

   See also

   - :gf:`children-overlapping-region`
   - :gf:`do-children-containing-position`

.. function:: do-displays

   Runs a function on all the displays attached to a given port.

   :signature: do-displays *function port* => ()

   :parameter function: An instance of type ``<function>``.
   :parameter port: An instance of type :class:`<port>`.

   :description:

     Runs a function on all the displays attached to a given port. By
     default, the current port is used, unless *port* is specified.

.. generic-function:: do-frames

   Runs a function on all the frames managed by a given frame manager.

   :signature: do-frames *function* #key *port frame-manager* => ()

   :parameter function: An instance of type ``<function>``.
   :parameter port: An instance of type :class:`<port>`.
   :parameter frame-manager: An instance of type :class:`<frame-manager>`.

   :description:

     Runs a function on all the frames managed by a given frame manager. By
     default, the current frame manager on the current port is used, unless
     *port* or *frame-manager* are specified.

.. function:: do-ports

   Runs a function on all the current ports.

   :signature: do-ports *function* => ()

   :parameter function: An instance of type ``<function>``.

   :description:

     Runs a function on all the current ports.

.. generic-function:: do-sheet-children

   Runs a function on all the immediate children of the specified sheet.

   :signature: do-sheet-children *function sheet* => ()

   :parameter function: An instance of type ``<function>``.
   :parameter sheet: An instance of type :class:`<sheet>`.

   :description:

     Runs *function* on all the immediate children of *sheet*. This function
     calls :gf:`sheet-children` to find the children of *sheet*.

   See also

   - :gf:`sheet-children`

.. generic-function:: do-sheet-tree

   Runs a function on all the children in the hierarchy of the specified
   sheet.

   :signature: do-sheet-tree *function sheet* => ()

   :parameter function: An instance of type ``<function>``.
   :parameter sheet: An instance of type :class:`<sheet>`.

   :description:

     Runs a function on all the children in the hierarchy of the specified
     sheet. The function is run on *sheet*, then on the children of *sheet*,
     then on the children of the children of *sheet*, and so on.

.. class:: <double-click-event>
   :sealed:
   :instantiable:

   The class of double-click events on the pointer device.

   :superclasses: :class:`<button-press-event>`

   :description:

     The class of double-click events on the pointer device. An instance of
     this class is generated when a button press is detected within a certain
     (small) amount of time after a previous button press. If a double click
     event is generated, the clock is reset, so that the next press generated
     is an instance of :class:`<button-press-event>`.

   See also

   - :class:`<button-press-event>`

.. generic-function:: do-with-drawing-options

   Runs some code on a drawable in a given drawing context.

   :signature: do-with-drawing-options *drawable function* #key *brush pen text-style clipping-region transform* => #rest *values*

   :parameter drawable: An instance of type *type-union(* :class:`<sheet>`, :class:`<medium>` *)*.
   :parameter function: An instance of type ``<function>``.
   :parameter brush: An instance of type :class:`<brush>`.
   :parameter pen: An instance of type :class:`<pen>`.
   :parameter text-style: An instance of type :class:`<text-style>`.
   :parameter clipping-region: An instance of type :class:`<region>`.
   :parameter transform: An instance of type :class:`<transform>`.

   :value values: An instance of type :drm:`<object>`.

   :description:

     Runs some code on a drawable in a given drawing context. This function
     is called by the macro :macro:`with-drawing-options`,
     and you should define new methods on it for new classes of drawable.

     The *function* passed to *do-with-drawing-options* is the result of
     encapsulating the body passed to :macro:`with-drawing-options` as
     a stand-alone method.

     The values returned are the values that are returned from
     :macro:`with-drawing-options`.

     The various keywords specify a drawing context in which function is run.

   See also

   - :macro:`with-drawing-options`

.. generic-function:: do-with-pointer-grabbed

   Runs some specified code, forwarding all pointer events to a sheet.

   :signature: do-with-pointer-grabbed *port sheet continuation* #key => #rest *values*

   :parameter port: An instance of type :class:`<port>`.
   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter continuation: An instance of type ``<function>``.

   :value values: An instance of type :drm:`<object>`.

   :description:

     Runs the code specified in *continuation*, forwarding all pointer
     events to *sheet*, even if the pointer leaves the sheet-region of
     *sheet*. The argument continuation is an instance of :class:`<function>`.

     This function is called by :macro:`with-pointer-grabbed`, and
     *continuation* is actually the result of creating a stand-alone method
     from the body of code passed to :macro:`with-pointer-grabbed`.

   See also

   - :macro:`with-pointer-grabbed`

.. generic-function:: do-with-sheet-medium

   Runs a continuation function on a sheet.

   :signature: do-with-sheet-medium *sheet continuation* => #rest *values*

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter continuation: An instance of type ``<function>``.

   :value values: An instance of type :drm:`<object>`.

   :description:

     Runs a continuation function on a sheet.

   See also

   - :macro:`with-sheet-medium`

.. generic-function:: do-with-text-style

   Runs some code on a drawable in the context of a given text style.

   :signature: do-with-text-style *drawable function text-style* => ()

   :parameter drawable: An instance of type *type-union(* :class:`<sheet>`, :class:`<medium>` *)*.
   :parameter function: An instance of type ``<function>``.
   :parameter text-style: An instance of type `<text-style> <dcs.htm#85385>`_.

   :description:

     Runs some code on a drawable in the context of a given text style.

   See also

   - :macro:`with-text-style`

.. generic-function:: do-with-transform

   Returns the result of running a function in a transform defined on a
   specified medium.

   :signature: do-with-transform *drawable function transform* => #rest *values*

   :parameter drawable: An instance of type *type-union(* :class:`<sheet>`, :class:`<medium>` *)*.
   :parameter function: An instance of type ``<function>``.
   :parameter transform: An instance of type :class:`<transform>`.

   :value values: An instance of type :drm:`<object>`.

   :description:

   Returns the result of running a function in a transform defined on
   a specified medium. Methods on this function are called by
   :macro:`with-transform`, which in turn is used by the similar macros
   :macro:`with-rotation`, :macro:`with-scaling`, and
   :macro:`with-translation`.

   See also

   - :gf:`with-transform`

.. class:: <event>
   :open:
   :abstract:

   The base class of all DUIM events.

   :superclasses: :drm:`<object>`

   :keyword timestamp: An instance of type ``<integer>``. Default value: *next-event-timestamp()*.

   :description:

     The base class of all DUIM events.

     The *timestamp:* init-keyword is used to give a unique identifier for
     the event.

   :operations:

     - :gf:`event?`
     - :gf:`event-matches-gesture?`
     - :gf:`handle-event`
     - :gf:`queue-event`

   See also

   - :class:`<frame-event>`
   - :class:`<sheet-event>`

.. generic-function:: event?

   Returns true if the specified object is an event.

   :signature: event? *object* => *event?*

   :parameter object: An instance of type :drm:`<object>`.

   :value event?: An instance of type ``<boolean>``.

   :description:

     Returns true if *object* is an instance of :class:`<event>` or one of its
     subclasses.

   See also

   - :class:`<event>`

.. generic-function:: event-button

   Returns an integer corresponding to the mouse button that was pressed or
   released.

   :signature: event-button *event* => *integer*

   :parameter event: An instance of type :class:`<event>`.

   :value integer: An instance of type ``<integer>``.

   :description:

     Returns an integer corresponding to the mouse button that was pressed or
     released, which will be one of :const:`$left-button`,
     :const:`$middle-button`, or :const:`$right-button`.

     *Note:* The function *event-button* records the button state at the time
     that the event occurred, and hence can be different from
     :gf:`pointer-button-state`.

   See also

   - :const:`$left-button`
   - :const:`$middle-button`
   - :class:`<pointer-button-event>`
   - :gf:`pointer-button-state`
   - :const:`$right-button`

.. generic-function:: event-character

   Returns the character that was pressed on the keyboard.

   :signature: event-character *event* => *value*

   :parameter event: An instance of type :class:`<event>`.

   :value value: An instance of type *false-or(<character>)*.

   :description:

     Returns the character associated with the keyboard event, if there is
     any.

   See also

   - :gf:`event-key-name`
   - :class:`<keyboard-event>`

.. generic-function:: event-key-name

   Returns the name of the key that was pressed or released on the
   keyboard.

   :signature: event-key-name *event* => name

   :parameter event: An instance of type :class:`<event>`.

   :value name: An instance of type :class:`<symbol>`.

   :description:

     Returns the name of the key that was pressed or released in a keyboard
     event. This will be a symbol whose value is specific to the current
     port.

   See also

   - :gf:`event-character`
   - :class:`<keyboard-event>`

.. generic-function:: event-matches-gesture?

   Returns true if an event matches a defined gesture.

   :signature: event-matches-gesture? *event gesture-name* => *matches?*

   :parameter event: An instance of type :class:`<event>`.
   :parameter gesture-name: An instance of type *type-union(* :class:`<gesture>`, ``<character>`` *)*.

   :value matches?: An instance of type ``<boolean>``.

   :description:

     Returns true if an event matches a defined gesture.

.. generic-function:: event-modifier-state

   Returns an integer value that encodes the state of all the modifier keys
   on the keyboard.

   :signature: event-modifier-state *event* => *integer*

   :parameter event: An instance of type :class:`<event>`.

   :value integer: An instance of type ``<integer>``.

   :description:

     Returns an integer value that encodes the state of all the modifier keys on
     the keyboard. This is a mask consisting of the *logior* of
     :const:`$shift-key`, :const:`$control-key`, :const:`$meta-key`,
     :const:`$super-key`, and :const:`$hyper-key`.

   See also

   - :gf:`event-sheet`
   - :gf:`gesture-modifier-state`
   - :gf:`make-modifier-state`
   - :gf:`port-modifier-state`

.. generic-function:: event-pointer

   Returns the pointer object to which the specified event refers.

   :signature: event-pointer *event* => *pointer*

   :parameter event: An instance of type :class:`<event>`.

   :value pointer: An instance of type :class:`<pointer>`.

   :description:

     Returns the pointer object to which *event* refers.

   See also

   - :class:`<pointer>`
   - :gf:`event-x`
   - :gf:`event-y`

.. generic-function:: event-region

   Returns the region in the sheet that is affected by the specified event.

   :signature: event-region *event* => *region*

   :parameter event: An instance of type :class:`<event>`.

   :value region: An instance of type :class:`<region>`.

   :description:

     Returns the region of the sheet that is affected by *event*.

   See also

   - :gf:`event-x`
   - :gf:`event-y`
   - :class:`<window-event>`

.. generic-function:: event-sheet

   Returns the sheet associated with the specified event.

   :signature: event-sheet *event* => *sheet*

   :parameter event: An instance of type :class:`<event>`.

   :value sheet: An instance of type :class:`<sheet>`.

   :description:

     Returns the sheet associated with *event*.

   See also

   - :gf:`event-modifier-state`

.. generic-function:: event-x

   Returns the x position of the pointer at the time the event occurred.

   :signature: event-x *event* => *x*

   :parameter event: An instance of type :class:`<event>`.

   :value x: An instance of type ``<integer>``.

   :description:

     Returns the x position of the pointer at the time the event occurred, in
     the coordinate system of the sheet that received the event.

   See also

   - :gf:`event-pointer`
   - :gf:`event-region`
   - :gf:`event-y`

.. generic-function:: event-y

   Returns the y position of the pointer at the time the event occurred.

   :signature: event-y *event* => *y*

   :parameter event: An instance of type :class:`<event>`.

   :value y: An instance of type ``<integer>``.

   :description:

     Returns the y position of the pointer at the time the event occurred, in
     the coordinate system of the sheet that received the event.

   See also

   - :gf:`event-pointer`
   - :gf:`event-region`
   - :gf:`event-x`

.. function:: find-display

   Returns a suitable display for the specified port and server-path
   criteria.

   :signature: find-display #key *server-path port orientation units* => *display*

   :parameter server-path: An instance of type :class:`<symbol>`. Default value: *#(#"local")*.
   :parameter port: An instance of type :class:`<port>`.
   :parameter orientation: An instance of type *one-of(#"default")*. Default value: *#"default"*.
   :parameter units: An instance of type *one-of(#"device", #"pixels", #"mm")*. Default value: *#"device"*.

   :value display: An instance of type :class:`<display>`.

   :description:

     Returns a suitable display for the specified port and server-path
     criteria.

     The *orientation* and *units* arguments can be used to specify the
     orientation and display units that the returned *display* needs to use.

   See also

   - :gf:`find-port`

.. function:: find-frame-manager

   Returns a suitable frame manager for the specified criteria.

   :signature: find-frame-manager #rest *options* #key *port server-path class palette* => *framem*

   :parameter options: An instance of type :drm:`<object>`.
   :parameter port: An instance of type :class:`<port>`.
   :parameter server-path: An instance of type :drm:`<object>`.
   :parameter class: An instance of type :class:`<type>`.
   :parameter palette: An instance of type :class:`<palette>`.

   :value framem: An instance of type :class:`<frame-manager>`.

   :description:

     Returns a suitable frame manager for the specified criteria.

     If necessary, you can specify a *port*, *server-path*, *class*, or
     *palette*. If any of these are not specified, then the default value is
     used in each case. The *class* argument specifies the class of frame
     manager that should be returned.

.. function:: find-port

   Returns a suitable port for the specified server-path.

   :signature: find-port #rest *initargs* #key *server-path* => *port*

   :parameter initargs: An instance of type :drm:`<object>`.
   :parameter server-path: An instance of type :drm:`<object>`. Default value: *\*default-server-path\**.

   :value port: An instance of type :class:`<port>`.

   :description:

     Returns a suitable port for the specified server-path.

   See also

   - :gf:`find-display`

.. generic-function:: fixed-width-font?

   Returns true if the specified text style uses a fixed-width font.

   :signature: fixed-width-font? *text-style port* #key *character-set* => *fixed?*

   :parameter text-style: An instance of type `<text-style> <dcs.htm#85385>`_.
   :parameter port: An instance of type :class:`<port>`.
   :parameter character-set: An instance of type :drm:`<object>`. Default value: *$standard-character-set*.

   :value fixed?: An instance of type ``<boolean>``.

   :description:

     Returns true if *text-style* uses a fixed-width font.

.. generic-function:: font-ascent

   Returns the ascent of the font in the specified text style.

   :signature: font-ascent *text-style port* #key *character-set* => *ascent*

   :parameter text-style: An instance of type `<text-style> <dcs.htm#85385>`_.
   :parameter port: An instance of type :class:`<port>`.
   :parameter character-set: An instance of type :drm:`<object>`. Default value: *$standard-character-set*.

   :value ascent: An instance of type ``<real>``.

   :description:

     Returns the ascent of the font in the *text-style* on *port*.

   See also

   - :gf:`font-descent`
   - :gf:`font-height`
   - :gf:`font-metrics`
   - :gf:`font-width`

.. generic-function:: font-descent

   Returns the descent of the font in the specified text style.

   :signature: font-descent *text-style port* #key *character-set* => *descent*

   :parameter text-style: An instance of type :class:`<text-style>`.
   :parameter port: An instance of type :class:`<port>`.
   :parameter character-set: An instance of type :drm:`<object>`.

   :value descent: An instance of type ``<real>``.

   :description:

     Returns the descent of the font in the *text-style* on *port*.

   See also

   - :gf:`font-ascent`
   - :gf:`font-height`
   - :gf:`font-metrics`
   - :gf:`font-width`

.. generic-function:: font-height

   Returns the height of the font in the specified text style.

   :signature: font-height *text-style port* #key *character-set* => *height*

   :parameter text-style: An instance of type :class:`<text-style>`.
   :parameter port: An instance of type :class:`<port>`.
   :parameter character-set: An instance of type :drm:`<object>`.

   :value height: An instance of type ``<real>``.

   :description:

     Returns the height of the font in the *text-style* on *port*.

   See also

   - :gf:`font-ascent`
   - :gf:`font-descent`
   - :gf:`font-metrics`
   - :gf:`font-width`

.. generic-function:: font-metrics

   Returns the metrics of the font in the specified text style.

   :signature: font-metrics *text-style port* #key *character-set* => *font width height ascent descent*

   :parameter text-style: An instance of type :class:`<text-style>`.
   :parameter port: An instance of type :class:`<port>`.
   :parameter character-set: An instance of type :drm:`<object>`.

   :value font: An instance of type :drm:`<object>`.
   :value width: An instance of type ``<real>``.
   :value height: An instance of type ``<real>``.
   :value ascent: An instance of type ``<real>``.
   :value descent: An instance of type ``<real>``.

   :description:

     Returns the metrics of the font in the *text-style* on *port*.

   See also

   - :gf:`font-ascent`
   - :gf:`font-descent`
   - :gf:`font-height`
   - :gf:`font-width`

.. generic-function:: font-width

   Returns the width of the font in the specified text style.

   :signature: font-width *text-style port* #key *character-set* => *width*

   :parameter text-style: An instance of type :class:`<text-style>`.
   :parameter port: An instance of type :class:`<port>`.
   :parameter character-set: An instance of type :drm:`<object>`.

   :value width: An instance of type ``<real>``.

   :description:

     Returns the with of the font in the *text-style* on *port*.

   See also

   - :gf:`font-ascent`
   - :gf:`font-descent`
   - :gf:`font-height`
   - :gf:`font-metrics`

.. generic-function:: force-display

   Forces the specified drawable object to be displayed.

   :signature: force-display *drawable* => ()

   :parameter drawable: An instance of type *type-union(* :class:`<sheet>`, :class:`<medium>` *)*.

   :description:

     Forces *drawable* to be displayed.

.. class:: <frame-event>
   :open:
   :abstract:

   The class of events that occur in frames.

   :superclasses: :class:`<event>`

   :parameter frame: An instance of type :class:`<frame>`. Required.

   :description:

     The class of events that occur in frames. The *frame:* init-keyword
     specified the frame in which the event occurs.

   See also

   - :class:`<frame-created-event>`
   - :class:`<frame-destroyed-event>`
   - :class:`<frame-exited-event>`
   - :class:`<frame-exit-event>`
   - :class:`<frame-mapped-event>`
   - :class:`<frame-unmapped-event>`

.. class:: <frame-manager>
   :open:
   :abstract:

   The class of frame managers.

   :superclasses: :drm:`<object>`

   :description:

     The class of frame managers.

     Frame managers control the realization of the look and feel of a frame.
     The frame manager interprets the specification of the application frame
     in the context of the available window system facilities, taking into
     account preferences expressed by the user.

     In addition, the frame manager takes care of attaching the pane
     hierarchy of an application frame to an appropriate place in a window
     hierarchy.

     Thus, the frame manager decides the following:

     A.  What concrete gadget to create for an abstract gadget.
     B.  How to layout the various parts of a frame, such as its menu, tool,
         and status bars.
     C.  How to lay out dialogs and their exit buttons.
     D.  How much spacing to use in various conventional layouts.

     In addition, a frame manager maps dialog functions such as
     :gf:`choose-file` to their appropriate native dialogs.

   :operations:

     The following operations are exported from the *DUIM-Sheets* module.

     - :gf:`display`
     - :gf:`frame-manager?`
     - :gf:`frame-manager-frames`
     - :gf:`frame-manager-palette`
     - :gf:`frame-manager-palette-setter`
     - :gf:`port`

     The following operations are exported from the *DUIM-Frames* module.

     - :gf:`clear-progress-note`
     - :gf:`display-progress-note`
     - :gf:`make-menus-from-command-table`

     The following operation is exported from the *DUIM-DCs* module.

     - :gf:`find-color`

   See also

   - :gf:`frame-manager`
   - :gf:`frame-manager?`

.. generic-function:: frame-manager

   Returns the frame manager for the specified object.

   :signature: frame-manager *object* => *value*

   :parameter object: An instance of type :drm:`<object>`.

   :parameter value: An instance of type *false-or(* :class:`<frame-manager>` *)*.

   :description:

     Returns the frame manager used to control the look and feel of the
     display of *object*.

   See also

   - :gf:`display`
   - :class:`<frame-manager>`
   - :gf:`frame-manager?`
   - :gf:`port`

.. generic-function:: frame-manager?

   Returns true if the specified object is a frame manager.

   :signature: frame-manager? *object* => *framem?*

   :parameter object: An instance of type :drm:`<object>`.

   :value framem?: An instance of type ``<boolean>``.

   :description:

     Returns true if *object* is a frame manager.

   See also

   - :class:`<frame-manager>`
   - :gf:`frame-manager`

.. generic-function:: frame-manager-frames

   Returns the frames managed by the specified frame manager.

   :signature: frame-manager-frames *framem* => *frames*

   :parameter framem: An instance of type :class:`<frame-manager>`.

   :parameter frames: An instance of type *limited(<sequence>, of:* :class:`<frame>` *)*.

   :description:

     Returns the frames managed by *framem*.

.. generic-function:: frame-manager-palette

   Returns the palette used by the specified frame manager.

   :signature: frame-manager-palette *framem* => *palette*

   :parameter framem: An instance of type :class:`<frame-manager>`.

   :value palette: An instance of type :class:`<palette>`.

   :description:

     Returns the palette used by *framem*.

   See also

   - :gf:`frame-manager-palette-setter`

.. generic-function:: frame-manager-palette-setter

   Sets the palette used by the specified frame manager.

   :signature: frame-manager-palette-setter *palette framem* => *palette*

   :parameter palette: An instance of type :class:`<palette>`.
   :parameter framem: An instance of type :class:`<frame-manager>`.

   :value palette: An instance of type :class:`<palette>`.

   :description:

     Sets the palette used by *framem*.

   See also

   - :gf:`frame-manager-palette`

.. class:: <gesture>
   :abstract:
   :instantiable:

   The base class of all gestures.

   :superclasses: :drm:`<object>`

   :keyword keysym: An instance of type :class:`<symbol>`. Required.
   :keyword button: An instance of type ``<integer>``. Required.
   :keyword modifier-state: An instance of type ``<integer>``. Required.
   :keyword modifiers: An instance of type :class:`<sequence>`.

   :description:

     The base class of all gestures.

   :operations:

     - :gf:`add-command`
     - :gf:`add-command-table-menu-item`
     - :gf:`event-matches-gesture?`
     - :gf:`gadget-accelerator-setter`
     - :gf:`gesture-modifier-state`
     - :gf:`gesture-spec-equal`

   See also

   - :class:`<keyboard-gesture>`
   - :class:`<pointer-gesture>`

.. generic-function:: gesture-button

   Returns the button associated with the specified gesture.

   :signature: gesture-button *pointer-gesture* => *button*

   :parameter pointer-gesture: An instance of type :class:`<pointer-gesture>`.

   :value button: An instance of type ``<integer>``.

   :description:

     Returns the button associated with *pointer-gesture*.

   See also

   - :class:`<pointer-gesture>`

.. generic-function:: gesture-keysym

   Returns the keysym associated with the specified gesture.

   :signature: gesture-keysym *keyboard-gesture* => *keysym*

   :parameter keyboard-gesture: An instance of type :class:`<keyboard-gesture>`.

   :value keysym: An instance of type :class:`<symbol>`.

   :description:

     Returns the keysym associated with *keyboard-gesture*.

   See also

   - :class:`<keyboard-gesture>`

.. generic-function:: gesture-modifier-state

   Returns the modifier-state associated with the specified gesture.

   :signature: gesture-modifier-state *gesture* => *modifier-state*

   :parameter gesture: An instance of type :class:`<gesture>`.

   :value modifier-state: An instance of type ``<integer>``.

   :description:

     Returns the modifier-state associated with *gesture*.

   See also

   - :gf:`event-modifier-state`
   - :class:`<keyboard-gesture>`
   - :gf:`make-modifier-state`
   - :gf:`port-modifier-state`

.. function:: gesture-spec-equal

   Returns true if the two specified gestures are equivalent.

   :signature: gesture-spec-equal *gesture1 gesture2* => *equal?*

   :parameter gesture1: An instance of type :class:`<gesture>`.
   :parameter gesture2: An instance of type :class:`<gesture>`.

   :value equal?: An instance of type ``<boolean>``.

   :description:

     Returns true if *gesture1* and *gesture2* are equivalent.

   See also

   - :gf:`=`

.. generic-function:: get-clipboard-data-as

   Returns data of a given type from a clipboard.

   :signature: get-clipboard-data-as *type clipboard* => *data*

   :parameter type: An instance of *type-union(<symbol>,* *<type>)*.
   :parameter clipboard: An instance of :class:`<clipboard>`.

   :value data: Instances of :drm:`<object>`.

   :description:

     This generic function returns *data* of *type* from the clipboard. The
     argument *type* is an instance of *type-union(<symbol>, <type>)*.

   See also

   - :gf:`add-clipboard-data`
   - :gf:`add-clipboard-data-as`
   - :class:`<clipboard>`
   - :gf:`clipboard-data-available?`

.. generic-function:: get-default-background

   Returns the default background for the specified sheet.

   :signature: get-default-background *port sheet* #key *background* => *background*

   :parameter port: An instance of type :class:`<port>`.
   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter background: An instance of type :class:`<ink>`.

   :value background: An instance of type :class:`<ink>`.

   :description:

     Returns the default background for *sheet* on *port*.

     If *background* is specified, then this is used instead of the default.

   See also

   - :gf:`get-default-foreground`
   - :gf:`get-default-text-style`

.. generic-function:: get-default-foreground

   Returns the default foreground for the specified sheet.

   :signature: get-default-foreground *port sheet* #key *foreground* => *foreground*

   :parameter port: An instance of type :class:`<port>`.
   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter foreground: An instance of type :class:`<ink>`.

   :value foreground: An instance of type :class:`<ink>`.

   :description:

     Returns the default foreground for *sheet* on *port*.

     If *foreground* is specified, then this is used instead of the default.

   See also

   - :gf:`get-default-background`
   - :gf:`get-default-text-style`

.. generic-function:: get-default-text-style

   Returns the default text style for the specified sheet.

   :signature: get-default-text-style *port sheet* #key *text-style* => *text-style*

   :parameter port: An instance of type :class:`<port>`.
   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter text-style: An instance of type :class:`<text-style>`.

   :value text-style: An instance of type :class:`<text-style>`.

   :description:

     Returns the default text style for *sheet* on *port*.

     If *text-style* is specified, then this is used instead of the default.

   See also

   - :gf:`get-default-background`
   - :gf:`get-default-foreground`

.. generic-function:: handle-event

   Implements any defined policies of the specified sheet with respect to
   the specified event.

   :signature: handle-event *sheet event* => ()

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter event: An instance of type :class:`<event>`.

   :description:

     Implements any defined policies of *sheet* with respect to *event*.
     Methods defined on this generic are called by DUIM to do the handling.

     For example, to highlight a sheet in response to an event that informs
     the sheet when the pointer has entered the region it occupies, there
     should be a method to carry out the policy that specializes the
     appropriate sheet and event classes.

     DUIM itself implements no semantically meaningful *handle-event*
     methods; It is the responsibility of any application to implement all of
     its own *handle-event* methods. It is also the responsibility of the
     application to decide the protocol and relationship between all of these
     methods.

     Take care when adding *next-method()* calls in any *handle-event*
     methods that you write. Because DUIM itself supplies no built-in
     methods, you must ensure that you have supplied a valid method yourself.
     For each event class you are handling, you should decide whether a call
     to *next-method* is actually required.

   See also

   - :gf:`handle-repaint`
   - :gf:`queue-event`

.. generic-function:: handle-repaint

   Implements region repainting for a given sheet class.

   :signature: handle-repaint *sheet medium region* => ()

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter medium: An instance of type :class:`<medium>`.
   :parameter region: An instance of type :class:`<region>`.

   :description:

     Implements region repainting for a given sheet class. Methods on this
     generic are called by DUIM in an application thread in order to handle
     repainting a given part of the screen. By calling available methods, it
     repaints the *region* of the *sheet* on *medium*.

     DUIM itself implements no semantically meaningful *handle-repaint*
     methods; It is the responsibility of any application to implement all of
     its own *handle-repaint* methods. It is also the responsibility of the
     application to decide the protocol and relationship between all of these
     methods.

     Take care when adding *next-method()* calls in any *handle-repaint*
     methods that you write. Because DUIM itself supplies no built-in
     methods, you must ensure that you have supplied a valid method yourself.
     For each sheet class you are handling, you should decide whether a call
     to *next-method* is actually required.

     The *sheet* on *medium* is repainted and *region* is the region to
     repaint.

   See also

   - :class:`<drawing-pane>`
   - :gf:`pane-display-function`
   - :gf:`queue-repaint`
   - :gf:`repaint-sheet`
   - :class:`<simple-pane>`
   - :class:`<window-repaint-event>`

.. constant:: $hyper-key

   A constant that represents the HYPER key on the keyboard.

   :type: :class:`<integer>`

   :value: ash(1, %modifier\_base + 4);

   :description:

     A constant that represents the HYPER key on the keyboard.

   See also

   - :const:`$alt-key`
   - :const:`$control-key`
   - :const:`$meta-key`
   - :gf:`modifier-key-index`
   - :gf:`modifier-key-index-name`
   - :const:`$modifier-keys`
   - :const:`$option-key`
   - :const:`$shift-key`
   - :const:`$super-key`

.. class:: <keyboard-event>
   :open:
   :abstract:

   The base class of all keyboard events.

   :superclasses: :class:`<device-event>`

   :keyword key-name: An instance of type *false-or(<symbol>)*. Default value: ``#f``.
   :keyword character: An instance of type *false-or(<character>)*. Default value: ``#f``.

   :description:

     The base class of all keyboard events.

     The key-name: init-keyword represents the name of the key on the
     keyboard that was pressed.

     The *character:* init-keyword represents the keyboard character that was
     pressed for characters in the standard character set.

   :operations:

     - :gf:`event-character`
     - :gf:`event-key-name`
     - :gf:`event-matches-gesture?`

   See also

   - :gf:`event-character`
   - :gf:`event-key-name`
   - :class:`<key-press-event>`
   - :class:`<key-release-event>`

.. class:: <keyboard-gesture>
   :sealed:
   :instantiable:

   The base class of all keyboard gestures.

   :superclasses: :class:`<gesture>`

   :keyword keysym: An instance of type :class:`<symbol>`.
   :keyword modifier-state: An instance of type ``<integer>``.

   :description:

     The base class of all keyboard gestures.

     The *keysym:* init-keyword represents the keysym for the gesture, and
     the *modifier-state:* init-keyword represents its modifier state.

   :operations:

     - :gf:`gesture-keysym`

   See also

   - :gf:`gesture-keysym`
   - :gf:`gesture-modifier-state`

.. class:: <key-press-event>
   :sealed:
   :instantiable:

   The class of events passed when a key is pressed.

   :superclasses: :class:`<keyboard-event>`

   :description:

     The class of events passed when a key is pressed.

   :operations:

   See also

   - :class:`<keyboard-event>`
   - :class:`<key-release-event>`

.. class:: <key-release-event>
   :sealed:
   :instantiable:

   The class of events passed when a key is released.

   :superclasses: :class:`<keyboard-event>`

   :description:

     The class of events passed when a key is released.

   :operations:

   See also

   - :class:`<keyboard-event>`
   - :class:`<key-press-event>`

.. constant:: $left-button

      A constant that represents the left button on the attached pointing
      device.

   :type: :class:`<integer>`

   :value: ash(1, %button\_base + 0)

   :description:

     A constant that represents the left button on the attached pointing
     device.

   See also

   - :const:`$middle-button`
   - :const:`$pointer-buttons`
   - :const:`$right-button`

.. generic-function:: lower-sheet

   Lowers the specified sheet to the bottom of the current hierarchy of
   sheets.

   :signature: lower-sheet *sheet* => ()

   :parameter sheet: An instance of type :class:`<sheet>`.

   :description:

     Lowers *sheet* to the bottom of the current hierarchy of sheets.

   See also

   - :gf:`lower-frame`
   - :gf:`raise-frame`
   - :gf:`raise-sheet`

.. generic-function:: make-frame-manager

   Returns an instance of :class:`<frame-manager>` on the specified port.

   :signature: make-frame-manager *port* #key *palette* => *framem*

   :parameter port: An instance of type :class:`<port>`.
   :parameter palette: An instance of type :class:`<palette>`.

   :parameter framem: An instance of type :class:`<frame-manager>`.

   :description:

     Returns an instance of :class:`<frame-manager>` on *port*. If specified,
     the palette described by *palette* is used.

   See also

   - :class:`<frame-manager>`

.. function:: make-modifier-state

   Returns a modifier state for the specified modifiers.

   :signature: make-modifier-state #rest *modifiers* => *integer*

   :parameter modifiers: An instance of type *limited(<sequence>, of: <integer>)*.

   :value integer: An instance of type ``<integer>``.

   :description:

     Returns a modifier state for *modifiers*.

   See also

   - :gf:`event-modifier-state`
   - :gf:`gesture-modifier-state`
   - :gf:`port-modifier-state`

.. generic-function:: make-pane

   Selects and returns an instance of a suitable class of pane for the
   supplied options.

   :signature: make-pane *pane-class* #rest *pane-options* #key *frame-manager* => *sheet*

   :parameter pane-class: An instance of type :class:`<class>`.
   :parameter pane-options: Instances of type :drm:`<object>`.
   :parameter frame-manager: An instance of type :class:`<frame-manager>`.

   :value sheet: An instance of type :class:`<sheet>`.

   :description:

     Selects a class that implements the behavior of pane-class and
     constructs a pane of that class.

.. class:: <medium>
   :open:
   :abstract:
   :instantiable:

   The class of all mediums.

   :superclasses: :drm:`<object>`

   :description:

     The class of all mediums.

     Mediums have the following elements associated with them:

     - A drawing plane, to which text and lines may be drawn
     - A foreground color, which describes the default color of anything
       drawn on the drawing plane
     - A background color, which describes the background color of the
       drawing plane
     - A transformation which describes the position of the drawing plane
       relative to the sheet which is its parent
     - A clipping region, on which any editing operations (such as cutting,
       copying, or pasting) will have effect.
     - A line style that describes the appearance of any lines drawn on the
       drawing plane
     - A text style that describes the appearance of any text written to the
       drawing plane

   :operations:

     The following operations are exported from the *DUIM-Sheets* module.

     - :gf:`beep`
     - :gf:`clear-box`
     - :gf:`display`
     - :gf:`do-with-drawing-options`
     - :gf:`do-with-text-style`
     - :gf:`do-with-transform`
     - :gf:`force-display`
     - :gf:`handle-repaint`
     - :gf:`medium?`
     - :gf:`medium-background`
     - :gf:`medium-background-setter`
     - :gf:`medium-brush`
     - :gf:`medium-brush-setter`
     - :gf:`medium-clipping-region`
     - :gf:`medium-clipping-region-setter`
     - :gf:`medium-default-text-style`
     - :gf:`medium-default-text-style-setter`
     - :gf:`medium-drawable`
     - :gf:`medium-drawable-setter`
     - :gf:`medium-foreground`
     - :gf:`medium-foreground-setter`
     - :gf:`medium-merged-text-style`
     - :gf:`medium-pen`
     - :gf:`medium-pen-setter`
     - :gf:`medium-pixmap`
     - :gf:`medium-pixmap-setter`
     - :gf:`medium-sheet`
     - :gf:`medium-text-style`
     - :gf:`medium-text-style-setter`
     - :gf:`medium-transform`
     - :gf:`medium-transform-setter`
     - :gf:`port`
     - :gf:`synchronize-display`
     - :gf:`text-size`

     The following operations are exported from the *DUIM-Graphics* module.

     - :gf:`copy-area`
     - :gf:`copy-from-pixmap`
     - :gf:`copy-to-pixmap`
     - :gf:`do-with-output-to-pixmap`
     - :gf:`draw-bezier-curve`
     - :gf:`draw-image`
     - :gf:`make-pixmap`

     The following operations are exported from the *DUIM-Extended-Geometry*
     module.

     - :gf:`draw-design`

   See also

   - :gf:`medium?`
   - :class:`<pixmap-medium>`

.. generic-function:: medium?

   Returns true if the specified object is a medium.

   :signature: medium? *object* => *medium?*

   :parameter object: An instance of type :drm:`<object>`.

   :value medium?: An instance of type ``<boolean>``.

   :description:

     Returns true if *object* is a medium.

   See also

   - :class:`<medium>`
   - :gf:`sheet?`

.. generic-function:: medium-background

   Returns the background for the specified medium.

   :signature: medium-background *medium* => *ink*

   :parameter medium: An instance of type :class:`<medium>`.

   :value ink: An instance of type :class:`<ink>`.

   :description:

     Returns the background for *medium*.

   See also

   - :gf:`medium-background-setter`
   - :gf:`medium-foreground`

.. generic-function:: medium-background-setter

   Sets the background for the specified medium.

   :signature: medium-background-setter *background medium* => *background*

   :parameter background: An instance of type :class:`<ink>`.
   :parameter medium: An instance of type :class:`<medium>`.

   :value background: An instance of type :class:`<ink>`.

   :description:

     Sets the background for *medium*.

   See also

   - :gf:`medium-background`
   - :gf:`medium-foreground-setter`

.. generic-function:: medium-brush

   Returns the brush for the specified medium.

   :signature: medium-brush *medium* => *brush*

   :parameter medium: An instance of type :class:`<medium>`.

   :value brush: An instance of type :class:`<brush>`.

   :description:

     Returns the brush for *medium*. This brush is used by all subsequent
     painting operations on *medium*.

   See also

   - :gf:`medium-brush-setter`
   - :gf:`medium-pen`

.. generic-function:: medium-brush-setter

   Sets the brush for the specified medium.

   :signature: medium-brush-setter *brush medium* => *brush*

   :parameter brush: An instance of type :class:`<brush>`.
   :parameter medium: An instance of type :class:`<medium>`.

   :value brush: An instance of type :class:`<brush>`.

   :description:

     Sets the brush for *medium*. This brush is used by all subsequent
     painting operations on *medium*.

   See also

   - :gf:`medium-brush`
   - :gf:`medium-pen-setter`

.. generic-function:: medium-clipping-region

   Returns the clipping region for the specified medium.

   :signature: medium-clipping-region *medium* => *region*

   :parameter medium: An instance of type :class:`<medium>`.

   :value region: An instance of type :class:`<region>`.

   :description:

     Returns the clipping region for *medium*.

   See also

   - :gf:`medium-clipping-region-setter`

.. generic-function:: medium-clipping-region-setter

   Sets the clipping region for the specified medium.

   :signature: medium-clipping-region-setter *region medium* => *region*

   :parameter region: An instance of type :class:`<region>`.
   :parameter medium: An instance of type :class:`<medium>`.

   :value region: An instance of type :class:`<region>`.

   :description:

     Sets the clipping region for *medium*.

   See also

   - :gf:`medium-clipping-region`

.. generic-function:: medium-default-text-style

   Returns the default text style for the specified medium.

   :signature: medium-default-text-style *medium* => *text-style*

   :parameter medium: An instance of type :class:`<medium>`.

   :value text-style: An instance of type :class:`<text-style>`.

   :description:

     Returns the default text style for *medium*. This style is used for any
     subsequent text that is written to *medium*.

   See also

   - :gf:`medium-default-text-style-setter`
   - :gf:`medium-merged-text-style`
   - :gf:`medium-text-style`

.. generic-function:: medium-default-text-style-setter

   Sets the default text style for the specified medium.

   :signature: medium-default-text-style-setter *text-style medium* => *text-style*

   :parameter text-style: An instance of type :class:`<text-style>`.
   :parameter medium: An instance of type :class:`<medium>`.

   :value text-style: An instance of type :class:`<text-style>`.

   :description:

     Sets the default text style for *medium*. This style is used for any
     subsequent text that is written to *medium*.

   See also

   - :gf:`medium-default-text-style`
   - :gf:`medium-text-style-setter`

.. generic-function:: medium-drawable

   Returns the drawable for the specified medium.

   :signature: medium-drawable *medium* => *drawable*

   :parameter medium: An instance of type :class:`<medium>`.

   :value drawable: An instance of type :drm:`<object>`.

   :description:

     Returns the drawable for *medium*.

   See also

   - :gf:`medium-drawable-setter`

.. generic-function:: medium-drawable-setter

   Sets the drawable for the specified medium.

   :signature: medium-drawable-setter *drawable medium* => *object*

   :parameter drawable: An instance of type *type-union(* :class:`<sheet>`, :class:`<medium>` *)*.
   :parameter medium: An instance of type :class:`<medium>`.

   :value object: An instance of type :drm:`<object>`.

   :description:

     Sets the drawable for *medium*.

   See also

   - :gf:`medium-drawable`

.. generic-function:: medium-foreground

   Returns the foreground of the specified medium.

   :signature: medium-foreground *medium* => *ink*

   :parameter medium: An instance of type :class:`<medium>`.

   :value ink: An instance of type :class:`<ink>`.

   :description:

     Returns the foreground of *medium*.

   See also

   - :gf:`medium-background`
   - :gf:`medium-foreground-setter`

.. generic-function:: medium-foreground-setter

   Sets the foreground of the specified medium.

   :signature: medium-foreground-setter *foreground medium* => *foreground*

   :parameter foreground: An instance of type :class:`<ink>`.
   :parameter medium: An instance of type :class:`<medium>`.

   :value foreground: An instance of type :class:`<ink>`.

   :description:

     Sets the foreground of *medium*.

   See also

   - :gf:`medium-background-setter`
   - :gf:`medium-foreground`

.. generic-function:: medium-merged-text-style

   Returns the merged text style of the specified medium.

   :signature: medium-merged-text-style *medium* => *text-style*

   :parameter medium: An instance of type :class:`<medium>`.

   :value text-style: An instance of type :class:`<text-style>`.

   :description:

     Returns the merged text style of *medium*.

   See also

   - :gf:`medium-default-text-style`
   - :gf:`medium-text-style`

.. generic-function:: medium-pen

   Returns the pen for the specified medium.

   :signature: medium-pen *medium* => *pen*

   :parameter medium: An instance of type :class:`<medium>`.

   :value pen: An instance of type :class:`<pen>`.

   :description:

     Returns the pen for *medium*. This brush is used by all subsequent
     drawing operations on *medium*.

   See also

   - :gf:`medium-brush`
   - :gf:`medium-pen-setter`

.. generic-function:: medium-pen-setter

   Sets the pen for the specified medium.

   :signature: medium-pen-setter *pen medium* => *pen*

   :parameter pen: An instance of type :class:`<pen>`.
   :parameter medium: An instance of type :class:`<medium>`.

   :value pen: An instance of type :class:`<pen>`.

   :description:

     Sets the pen for *medium*. This brush is used by all subsequent drawing
     operations on *medium*.

   See also

   - :gf:`medium-brush-setter`
   - :gf:`medium-pen`

.. generic-function:: medium-pixmap

   Returns the pixmap for the specified medium.

   :signature: medium-pixmap *medium* => *value*

   :parameter medium: An instance of type :class:`<medium>`.

   :value value: An instance of type *false-or(* :class:`<pixmap>` *)*.

   :description:

     Returns the pixmap for *medium*.This pixmap is used by all subsequent
     pixmap operations on *medium*.

   See also

   - :gf:`medium-pixmap-setter`

.. generic-function:: medium-pixmap-setter

   Sets the pixmap for the specified medium.

   :signature: medium-pixmap-setter *pixmap medium* => *value*

   :parameter pixmap: An instance of type :class:`<pixmap>`.
   :parameter medium: An instance of type :class:`<medium>`.

   :value value: An instance of type *false-or(* :class:`<pixmap>` *)*.

   :description:

     Returns the pixmap for *medium*.This pixmap is used by all subsequent
     pixmap operations on *medium*.

   See also

   - :gf:`medium-pixmap`

.. generic-function:: medium-sheet

   Returns the sheet for the specified medium.

   :signature: medium-sheet *medium* => *sheet*

   :parameter medium: An instance of type :class:`<medium>`.

   :value sheet: An instance of type *false-or(* :class:`<sheet>` *)*.

   :description:

     Returns the sheet for *medium*, if there is one.

.. generic-function:: medium-text-style

   Returns the text style for the specified medium.

   :signature: medium-text-style *medium* => *text-style*

   :parameter medium: An instance of type :class:`<medium>`.

   :value text-style: An instance of type :class:`<text-style>`.

   :description:

     Returns the text style for *medium*.

   See also

   - :gf:`medium-default-text-style`
   - :gf:`medium-merged-text-style`
   - :gf:`medium-text-style-setter`

.. generic-function:: medium-text-style-setter

   Sets the text style for the specified medium.

   :signature: medium-text-style-setter *text-style medium* => *text-style*

   :parameter text-style: An instance of type :class:`<text-style>`.
   :parameter medium: An instance of type :class:`<medium>`.

   :value text-style: An instance of type :class:`<text-style>`.

   :description:

     Sets the text style for *medium*.

   See also

   - :gf:`medium-default-text-style-setter`
   - :gf:`medium-text-style`

.. generic-function:: medium-transform

   Returns the transform for the specified medium.

   :signature: medium-transform *medium* => *transform*

   :parameter medium: An instance of type :class:`<medium>`.

   :value transform: An instance of type :class:`<transform>`.

   :description:

     Returns the transform for *medium*.

   See also

   - :gf:`medium-transform-setter`
   - :gf:`sheet-transform`

.. generic-function:: medium-transform-setter

   Sets the transform for the specified medium.

   :signature: medium-transform-setter *transform medium* => *transform*

   :parameter transform: An instance of type :class:`<transform>`.
   :parameter medium: An instance of type :class:`<medium>`.

   :value transform: An instance of type :class:`<transform>`.

   :description:

     Sets the transform for *medium*.

   See also

   - :gf:`medium-transform`
   - :gf:`sheet-transform-setter`

.. constant:: $meta-key

   A constant that represents the META key on the keyboard.

   :type: :class:`<integer>`

   :value: ash(1, %modifier\_base + 2);

   :description:

     A constant that represents the META key on the keyboard, if it exists.
     To deal with the case where there is no META key, the value of the
     constant :const:`$alt-key` is bound to this constant.

   See also

   - :const:`$alt-key`
   - :const:`$control-key`
   - :const:`$hyper-key`
   - :gf:`modifier-key-index`
   - :gf:`modifier-key-index-name`
   - :const:`$modifier-keys`
   - :const:`$option-key`
   - :const:`$shift-key`
   - :const:`$super-key`

.. constant:: $middle-button

   A constant that represents the middle button on the attached pointing
   device.

   :type: :class:`<integer>`

   :value: ash(1, %button\_base + 1)

   :description:

     A constant that represents the middle button on the attached pointing
     device.

   See also

   - :const:`$left-button`
   - :const:`$pointer-buttons`
   - :const:`$right-button`

.. function:: modifier-key-index

   Returns the index number of the specified modifier key.

   :signature: modifier-key-index *key-name* => *index*

   :parameter key-name: An instance of type :class:`<symbol>`.

   :value index: An instance of type ``<integer>``.

   :description:

     Returns the index number of the specified modifier key. The *key-name*
     specified may be any of the elements of :const:`$modifier-keys`

     The returned index value is either 0, 1, 2, 3, or 4.

   See also

   - :const:`$alt-key`
   - :const:`$control-key`
   - :const:`$hyper-key`
   - :const:`$meta-key`
   - :gf:`modifier-key-index-name`
   - :const:`$modifier-keys`
   - :const:`$option-key`
   - :const:`$shift-key`
   - :const:`$super-key`

.. function:: modifier-key-index-name

   Returns the key name of the specified modifier key index.

   :signature: modifier-key-index-name *index* => *key-name*

   :parameter index: An instance of type ``<integer>``.

   :value key-name: An instance of type :class:`<symbol>`.

   :description:

     Returns the key name of the specified modifier key index. The *index*
     specified is either 0, 1, 2, 3, or 4.

     The *key-name* returned may be any of the elements of
     :const:`$modifier-keys`

   See also

   - :const:`$alt-key`
   - :const:`$control-key`
   - :const:`$hyper-key`
   - :const:`$meta-key`
   - :gf:`modifier-key-index`
   - :const:`$modifier-keys`
   - :const:`$option-key`
   - :const:`$shift-key`
   - :const:`$super-key`

.. constant:: $modifier-keys

   The default list of keys on the keyboard that are used as modifiers.

   :type: :class:`<sequence>`

   :value: #[#"shift", #"control", #"meta", #"super", #"hyper"]

   :description:

     The default list of keys on the keyboard that are used as modifiers for
     keyboard accelerators and mnemonics.

   See also

   - :const:`$alt-key`
   - :const:`$control-key`
   - :const:`$hyper-key`
   - :const:`$meta-key`
   - :gf:`modifier-key-index`
   - :gf:`modifier-key-index-name`
   - :const:`$option-key`
   - :const:`$shift-key`
   - :const:`$super-key`

.. generic-function:: notify-user

   Creates and displays an alert dialog box with the specified criteria.

   :signature: notify-user *message-string* #key *frame owner title documentation exit-boxes name style foreground background text-style* => *boolean*

   :parameter message-string: An instance of type :class:`<string>`.
   :parameter frame: An instance of type :class:`<frame>`. Default value: :gf:`current-frame` ().
   :parameter owner: An instance of type :class:`<sheet>`.
   :parameter title: An instance of type :class:`<string>`.
   :parameter documentation: An instance of type *false-or(<string>)*. Default value: ``#f``.
   :parameter exit-boxes: An instance of type :drm:`<object>`.
   :parameter name: An instance of type :drm:`<object>`.
   :parameter style: An instance of type *one-of(#"information", #"question", #"warning", #"error", #"serious-error", #"fatal-error")*.
   :parameter foreground: An instance of type *false-or(* :class:`<ink>` *)*. Default value: ``#f``.
   :parameter background: An instance of type *false-or(* :class:`<ink>` *)*. Default value: ``#f``.
   :parameter text-style: An instance of type *false-or(* :class:`<text-style>` *)*. Default value: ``#f``.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Creates and displays an alert dialog box with the specified criteria.
     Use this function as a way of easily displaying simple messages to the
     user.

     .. figure:: images/sheets-5.png
        :align: center

        Simple output from notify-user

     The *message-string* is the message that is displayed in the dialog. The
     arguments frame, owner, title, and documentation let you specify
     different attributes for the dialog in the same way as they can be
     specified for any other frame or dialog.

     The *exit-boxes* argument lets you specify the buttons that are
     available in the dialog. If not supplied, then a single *OK* button is
     used by default, unless the *style* of the dialog is set to
     *#"question"*, in which case, two buttons are created, to allow the
     user to respond "yes" or "no".

     The *style* argument lets you specify the style of dialog that is
     produced. The different styles available reflect the Motif specification
     for dialog box types. Depending on the style of dialog you choose, the
     appearance of the dialog created may vary. For example, a different icon
     is commonly used to distinguish between error, informational, and
     warning messages.

     The *foreground*, *background*, and *text-style* arguments let you
     specify foreground and background colors, and the font to use in the
     message text.

   See also

   - :gf:`choose-color`
   - :gf:`choose-directory`
   - :gf:`choose-file`

.. function:: open-clipboard

   Creates a clipboard lock for a sheet on a port.

   :signature: open-clipboard *port sheet* => *clipboard*

   :parameter port: An instance of :class:`<port>`.
   :parameter sheet: An instance of :class:`<sheet>`.

   :value clipboard: An instance of :class:`<clipboard>`.

   :description:

     Creates a clipboard lock for *sheet* on *port*. Once a clipboard lock
     has been created, you can manipulate the clipboard contents safely. An
     instance of :class:`<clipboard>` is returned, which is
     used to hold the clipboard contents.

     You should not normally call *open-clipboard* yourself to create
     a clipboard lock. Use the macro :macro:`with-clipboard` to create and free
     the lock for you.

   See also

   - :class:`<clipboard>`
   - :gf:`with-clipboard`

.. constant:: $option-key

   A constant that represents the OPTION key on the keyboard.

   :type: :class:`<integer>`

   :value: :const:`$super-key`

   :description:

     A constant that represents the OPTION key on the keyboard. This is set
     to the same value as the SUPER key, to deal with the case where the
     OPTION key is not present on the keyboard.

   See also

   - :const:`$alt-key`
   - :const:`$control-key`
   - :const:`$hyper-key`
   - :const:`$meta-key`
   - :gf:`modifier-key-index`
   - :gf:`modifier-key-index-name`
   - :const:`$modifier-keys`
   - :const:`$shift-key`
   - :const:`$super-key`

.. class:: <pointer>
   :open:
   :abstract:
   :instantiable:

   The class of all pointers.

   :superclasses: :drm:`<object>`

   :keyword port: An instance of type :class:`<port>`.

   :description:

     The class of all pointers.

   :operations:

     The following operations are exported from the *DUIM-Sheets* module.

     - :gf:`display`
     - :gf:`pointer?`
     - :gf:`pointer-button-state`
     - :gf:`pointer-cursor`
     - :gf:`pointer-cursor-setter`
     - :gf:`pointer-position`
     - :gf:`pointer-sheet`
     - :gf:`port`
     - :gf:`set-pointer-position`

   See also

   - :gf:`pointer?`

.. generic-function:: pointer?

   Returns true if the specified object is a pointer.

   :signature: pointer? *object* => *pointer?*

   :parameter object: An instance of type :drm:`<object>`.

   :value pointer?: An instance of type ``<boolean>``.

   :description:

     Returns true if *object* is a pointer.

   See also

   - :class:`<pointer>`

.. class:: <pointer-boundary-event>
   :sealed:
   :instantiable:

   The class that corresponds to a pointer motion event that crosses a
   sheet boundary.

   :superclasses: :class:`<pointer-motion-event>`

   :keyword kind: An instance of type *one-of(#"ancestor", #"virtual", #"inferior", #"nonlinear", #"nonlinear-virtual", #f)*. Default value: ``#f``.

   :description:

     The class that corresponds to a pointer motion event that crosses some
     sort of sheet boundary.

     The *kind:* init-keyword represents the boundary event kind. These
     correspond to the detail members for X11 enter and exit events.

   :operations:

     The following operation is exported from the *DUIM-Sheets* module.

     - :gf:`boundary-event-kind`

   See also

   - :gf:`boundary-event-kind`
   - :class:`<pointer-enter-event>`
   - :class:`<pointer-exit-event>`

.. class:: <pointer-button-event>
   :open:
   :abstract:

   The class of events that occur when mouse buttons are pressed.

   :superclasses: :class:`<pointer-event>`

   :keyword button: An instance of type *one-of(* :const:`$left-button`, :const:`$middle-button`, :const:`$right-button` *)*.

   :description:

     The class of events that occur when mouse buttons are pressed.

   :operations:

     The following operations are exported from the *DUIM-Sheets* module.

     - :gf:`event-button`
     - :gf:`event-matches-gesture?`
     - :gf:`handle-event`

   See also

   - :gf:`event-button`
   - :const:`$left-button`
   - :const:`$middle-button`
   - :gf:`pointer-button-state`
   - :class:`<pointer-drag-event>`
   - :const:`$right-button`

.. constant:: $pointer-buttons

   The constant representing the possible buttons on the pointing device.

   :type: :class:`<sequence>`

   :value: #[#"left", #"middle", #"right"];

   :description:

     The constant representing the possible buttons on the pointing device
     attached to the computer, typically a mouse. Up to three buttons are
     provided for.

     The order of the elements in this sequence must match the order of the
     values of :const:`$left-button`, :const:`$middle-button`, and
     :const:`$right-button`.

   See also

   - :gf:`button-index`
   - :gf:`button-index-name`
   - :const:`$left-button`
   - :const:`$middle-button`
   - :const:`$right-button`

.. generic-function:: pointer-button-state

   Returns the state of the specified pointer.

   :signature: pointer-button-state *pointer* => *integer*

   :parameter pointer: An instance of type :class:`<pointer>`.

   :value integer: An instance of type ``<integer>``.

   :description:

     Returns the state of *pointer*.

.. generic-function:: pointer-cursor

   Returns the cursor used for the specified pointer.

   :signature: pointer-cursor *pointer* => *cursor*

   :parameter pointer: An instance of type :class:`<pointer>`.

   :value cursor: An instance of type :class:`<cursor>`.

   :description:

     Returns the cursor used for *pointer*.

   See also

   - :gf:`pointer-cursor-setter`

.. generic-function:: pointer-cursor-setter

   Sets the cursor used for the specified pointer.

   :signature: pointer-cursor-setter *cursor pointer* => cursor

   :parameter cursor: An instance of type :class:`<cursor>`.
   :parameter pointer: An instance of type :class:`<pointer>`.

   :value cursor: An instance of type :class:`<cursor>`.

   :description:

     Sets the cursor used for *pointer*.

   See also

   - :gf:`pointer-cursor`

.. class:: <pointer-drag-event>
   :sealed:
   :instantiable:

   The class of events describing drag movements.

   :superclasses: :class:`<pointer-motion-event>` :class:`<pointer-button-event>`

   :keyword button: An instance of type *one-of(* :const:`$left-button`, :const:`$middle-button`, :const:`$right-button` *)*.

   :description:

     The class of events describing drag movements. This is the same as
     :class:`<pointer-motion-event>`, except that a button on the attached
     pointing device must also be held down as the pointer is moving.

     The *button:* init-keyword is inherited from the superclass
     :class:`<pointer-button-event>`.

   :operations:

   See also

   - :class:`<pointer-motion-event>`

.. class:: <pointer-enter-event>
   :sealed:
   :instantiable:

   The class of events that describe a pointer entering an area such as a
   sheet.

   :superclasses: :class:`<pointer-boundary-event>`

   :description:

     The class of events that describe a pointer entering an area such as a
     sheet.

   :operations:

   See also

   - :class:`<pointer-exit-event>`

.. class:: <pointer-event>
   :open:
   :abstract:

   The base class of events occurring on pointers.

   :superclasses: :class:`<device-event>`

   :keyword x: An instance of type ``<real>``.
   :keyword y: An instance of type ``<real>``.
   :keyword pointer: An instance of type :class:`<pointer>`.

   :description:

     The base class of events occurring on pointers on the computer screen.

     The *x:* and *y:* init-keywords specify the location of the pointer when
     the event occurs. The *pointer:* init-keyword specifies the pointer to
     which the event occurs.

   :operations:

   See also

   - :class:`<pointer-button-event>`
   - :class:`<pointer-exit-event>`
   - :class:`<pointer-motion-event>`

.. class:: <pointer-exit-event>
   :sealed:
   :instantiable:

   The class of events that describe a pointer leaving an area such as a
   sheet.

   :superclasses: :class:`<pointer-boundary-event>`

   :description:

     The class of events that describe a pointer leaving an area such as a
     sheet.

   :operations:

   See also

   - :class:`<pointer-button-event>`
   - :class:`<pointer-enter-event>`
   - :class:`<pointer-motion-event>`

.. class:: <pointer-gesture>
   :sealed:
   :instantiable:

   The class of all gestures that occur on pointers.

   :superclasses: :class:`<gesture>`

   :keyword button: An instance of type ``<integer>``.
   :keyword modifier-state: An instance of type ``<integer>``.

   :description:

     The class of all gestures that occur on pointers.

     The *button:* init-keyword specifies the button on the attached pointer
     device on which the gesture has occurred, and the *modifier-state:*
     init-keyword specifies the modifier-state of the gesture.

   :operations:

   - :gf:`gesture-button`

.. class:: <pointer-motion-event>
   :sealed:
   :instantiable:

   The class of events that describe a pointer that is moving.

   :superclasses: :class:`<pointer-event>`

   :description:

     The class of events that describe a pointer that is moving.

   :operations:

   See also

   - :class:`<pointer-button-event>`
   - :class:`<pointer-drag-event>`
   - :class:`<pointer-enter-event>`
   - :class:`<pointer-event>`
   - :class:`<pointer-exit-event>`

.. generic-function:: pointer-position

   Returns the current position of the specified pointer.

   :signature: pointer-position *pointer* #key *sheet* => *x y*

   :parameter pointer: An instance of type :class:`<pointer>`.
   :parameter sheet: An instance of type :class:`<sheet>`.

   :value x: An instance of type ``<real>``.
   :value y: An instance of type ``<real>``.

   :description:

     Returns the current position of *pointer*. If *sheet* is specified,
     then the pointer must be over it.

   See also

   - :gf:`pointer-sheet`
   - :gf:`set-pointer-position`

.. generic-function:: pointer-sheet

   Returns the sheet under the specified pointer.

   :signature: pointer-sheet *pointer* => *sheet*

   :parameter pointer: An instance of type :class:`<pointer>`.

   :parameter sheet: An instance of type *false-or(* :class:`<sheet>` *)*.

   :description:

     Returns the sheet under *pointer*, or #f if there is no sheet under the
     pointer.

   See also

   - :gf:`pointer-position`

.. class:: <port>
   :open:
   :abstract:

   The class of all ports.

   :superclasses: :drm:`<object>`

   :description:

     The class of all ports. A display, and all the sheets attached to a
     display, is associated with a port that is a connection to a display
     server. The port manages:

     - A primary input device (usually a keyboard)
     - A pointing device, such as a mouse or trackball
     - An event processor that dispatched events to the appropriate sheet.

   :operations:

     The following operations are exported from the *DUIM-Sheets* module.

     - :gf:`beep`
     - :gf:`default-port-setter`
     - :gf:`destroy-port`
     - :gf:`force-display`
     - :gf:`get-default-background`
     - :gf:`get-default-foreground`
     - :gf:`get-default-text-style`
     - :gf:`port`
     - :gf:`port?`
     - :gf:`port-modifier-state`
     - :gf:`port-pointer`
     - :gf:`port-server-path`
     - :gf:`synchronize-display`
     - :gf:`text-size`
     - :gf:`text-style-mapping`
     - :gf:`text-style-mapping-setter`

     The following operation is exported from the *DUIM-DCs* module.

     - :gf:`find-color`

   See also

   - :class:`<display>`
   - :class:`<sheet>`

.. generic-function:: port

   Returns the port for the specified object.

   :signature: port *object* => *value*

   :parameter object: An instance of type :drm:`<object>`.

   :parameter value: An instance of type *false-or(* :class:`<port>` *)*.

   :description:

     Returns the port used to display *object*.

   See also

   - :gf:`display`
   - :gf:`frame-manager`
   - :class:`<port>`
   - :gf:`port?`

.. generic-function:: port?

   Returns true if the specified object is a port.

   :signature: port? *object* => *boolean*

   :parameter object: An instance of type :drm:`<object>`.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns true if *object* is a port.

   See also

   - :class:`<port>`
   - :class:`<port>`

.. generic-function:: port-modifier-state

   Returns the modifier state of the specified port.

   :signature: port-modifier-state *port* => *integer*

   :parameter port: An instance of type :class:`<port>`.

   :value integer: An instance of type ``<integer>``.

   :description:

     Returns the modifier state of *port*.

   See also

   - :gf:`event-modifier-state`
   - :gf:`gesture-modifier-state`
   - :gf:`make-modifier-state`
   - :gf:`port-name`
   - :gf:`port-pointer`
   - :gf:`port-server-path`
   - :gf:`port-type`

.. generic-function:: port-name

   Returns the name of the specified port.

   :signature: port-name *port* => *name*

   :parameter port: An instance of type :class:`<port>`.

   :value name: An instance of type :drm:`<object>`.

   :description:

     Returns the name of *port*.

   See also

   - :gf:`port-modifier-state`
   - :gf:`port-pointer`
   - :gf:`port-server-path`
   - :gf:`port-type`

.. generic-function:: port-pointer

   Returns the pointer used on the specified port.

   :signature: port-pointer *port* => *pointer*

   :parameter port: An instance of type :class:`<port>`.

   :value pointer: An instance of type :class:`<pointer>`.

   :description:

     Returns the pointer used on *port*.

   See also

   - :gf:`port-modifier-state`
   - :gf:`port-name`
   - :gf:`port-server-path`
   - :gf:`port-type`

.. generic-function:: port-server-path

   Returns the server path of the specified port.

   :signature: port-server-path *port* => *object*

   :parameter port: An instance of type :class:`<port>`.

   :value object: An instance of type :drm:`<object>`.

   :description:

     Returns the server path of *port*.

   See also

   - :gf:`port-modifier-state`
   - :gf:`port-name`
   - :gf:`port-pointer`
   - :gf:`port-type`

.. class:: <port-terminated-event>
   :sealed:
   :instantiable:

   The class of events that describe the termination of a port.

   :superclasses: :class:`<frame-event>`

   :keyword condition: An instance of type :class:`<condition>`. Required.

   :description:

     The class of events that describe the termination of a port.

     The *condition:* init-keyword returns the error condition signalled when
     the port was terminated.

   :operations:

.. generic-function:: port-type

   Returns the type of the specified port.

   :signature: port-type *port* => *type*

   :parameter port: An instance of type :class:`<port>`.

   :value type: An instance of type :class:`<symbol>`.

   :description:

     Returns the type of *port*.

   See also

   - :gf:`port-modifier-state`
   - :gf:`port-name`
   - :gf:`port-pointer`
   - :gf:`port-server-path`

.. generic-function:: queue-event

   Queues an event for the specified sheet.

   :signature: queue-event *sheet event* => ()

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter event: An instance of type :class:`<event>`.

   :description:

     Queues *event* on the event-queue for *sheet*.

   See also

   - :gf:`handle-event`

.. generic-function:: queue-repaint

   Queues a repaint for the specified region of the specified sheet.

   :signature: queue-repaint *sheet region* => ()

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter region: An instance of type :class:`<region>`.

   :description:

     Queues a repaint for the area** of *sheet* defined by *region*.

   See also

   - :gf:`handle-repaint`
   - :gf:`repaint-sheet`
   - :class:`<window-repaint-event>`

.. generic-function:: raise-sheet

   Raises the specified sheet to the top of the current hierarchy of
   sheets.

   :signature: raise-sheet *sheet* => ()

   :parameter sheet: An instance of type :class:`<sheet>`.

   :description:

     Raises *sheet* to the top of the current hierarchy of sheets.

   See also

   - :gf:`lower-frame`
   - :gf:`lower-sheet`
   - :gf:`raise-frame`

.. generic-function:: remove-child

   Removes a child from the specified sheet.

   :signature: remove-child *sheet child* => *sheet*

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter child: An instance of type :class:`<sheet>`.

   :value sheet: An instance of type :class:`<sheet>`.

   :description:

     Removes *child* from *sheet*. The remaining children in the sheet are
     laid out again appropriately.

   See also

   - :gf:`add-child`
   - :gf:`replace-child`

.. generic-function:: repaint-sheet

   Repaints the specified region of a sheet.

   :signature: repaint-sheet *sheet region* #key *medium* => ()

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter region: An instance of type :class:`<region>`.
   :parameter medium: An instance of type :class:`<medium>`.

   :description:

     Repaints the are of *sheet* defined by *region*. If specified, the
     appropriate *medium* is used.

   See also

   - :gf:`handle-repaint`
   - :gf:`queue-repaint`
   - :class:`<window-repaint-event>`

.. generic-function:: replace-child

   Replaces a child from the specified sheet with a new one.

   :signature: replace-child *sheet old-child new-child* => *sheet*

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter old-child: An instance of type :drm:`<object>`.
   :parameter new-child: An instance of type :drm:`<object>`.

   :value sheet: An instance of type :class:`<sheet>`.

   :description:

     Replaces *old-child* with *new-child* in *sheet*. The children in the
     sheet are laid out again appropriately.

   See also

   - :gf:`add-child`
   - :gf:`remove-child`

.. constant:: $right-button

   A constant that represents the right button on the attached pointing
   device.

   :type: :class:`<integer>`

   :value: ash(1, %button\_base + 2)

   :description:

     A constant that represents the right button on the attached pointing
     device.

   See also

   - :const:`$left-button`
   - :const:`$middle-button`
   - :const:`$pointer-buttons`

.. generic-function:: set-caret-position

   Sets the position of the specified cursor.

   :signature: set-cursor-position *cursor x y* => ()

   :parameter cursor: An instance of type :class:`<caret>`.
   :parameter x: An instance of type ``<real>``.
   :parameter y: An instance of type ``<real>``.


   :description:

     Sets the position of *cursor* to *(* *x* *,* *y* *)*.

   See also

   - :gf:`caret-position`
   - :gf:`set-pointer-position`

.. generic-function:: set-pointer-position

   Sets the position of the specified pointer.

   :signature: set-pointer-position *pointer x y* #key *sheet* => ()

   :parameter pointer: An instance of type :class:`<pointer>`.
   :parameter x: An instance of type ``<real>``.
   :parameter y: An instance of type ``<real>``.
   :parameter sheet: An instance of type :class:`<sheet>`.

   :description:

     Sets the position of *pointer* to *(* *x* *,* *y* *)*, relative to the
     top left corner of *sheet*, if specified. Units are measured in pixels.

   See also

   - :gf:`pointer-position`
   - :gf:`set-pointer-position`

.. generic-function:: set-sheet-edges

   Sets the edges of the specified sheet relative to its parent.

   :signature: set-sheet-edges *sheet left top right bottom* => ()

   -  *sheet* An instance of type :class:`<sheet>`.
   -  *left* An instance of type ``<integer>``.
   -  *top* An instance of type ``<integer>``.
   -  *right* An instance of type ``<integer>``.
   -  *bottom* An instance of type ``<integer>``.

   :description:

     Sets the edges of *sheet* to *top*, *left*, *right*, and *bottom*.
     Each edge is specified relative to the corresponding edge of the parent
     of *sheet*. The layout of *sheet* is recalculated automatically.

   See also

   - :gf:`set-sheet-position`
   - :gf:`set-sheet-size`
   - :gf:`sheet-edges`

.. generic-function:: set-sheet-position

   Sets the position of the specified sheet relative to its parent.

   :signature: set-sheet-position *sheet x y* => ()

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter x: An instance of type ``<real>``.
   :parameter y: An instance of type ``<real>``.

   :description:

     Sets the position of *sheet* to *(* *x* *,* *y* *)* relative to the
     position of its parent. The layout of *sheet* is recalculated
     automatically.

   See also

   - :gf:`set-sheet-edges`
   - :gf:`set-sheet-size`
   - :gf:`sheet-position`

.. generic-function:: set-sheet-size

   Sets the size of the specified sheet.

   :signature: set-sheet-size *sheet width height* => ()

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter width: An instance of type ``<integer>``.
   :parameter height: An instance of type ``<integer>``.

   :description:

     Sets the size of *sheet*. The layout of *sheet* is recalculated
     automatically.

   See also

   - :gf:`set-sheet-edges`
   - :gf:`set-sheet-position`

.. class:: <sheet>
   :open:
   :abstract:

   The base object class for DUIM windows.

   :superclasses: :drm:`<object>`

   :keyword region: An instance of type :class:`<region>`. Default value *$nowhere*.
   :keyword transform: An instance of type :class:`<transform>`. Default value :const:`$identity-transform`.
   :keyword port: An instance of type *false-or(* :class:`<port>` *)*. Default value ``#f``.
   :keyword style-descriptor: An instance of type *false-or(style-descriptor)*. Default value ``#f``.
   :keyword help-context: An instance of type :class:`<object-table>`. Default value *make(<object-table>)*.
   :keyword help-source: An instance of type :class:`<object-table>`. Default value *make(<object-table>)*.
   :keyword parent: An instance of type *false-or(<sheet>)*. Default value: ``#f``.
   :keyword child: An instance of type *false-or(<sheet>)*. Default value: ``#f``.
   :keyword children: An instance of type *limited(<sequence>, of: <sheet>)*. Default value: *#[]*.
   :keyword x: An instance of type ``<integer>``.
   :keyword y: An instance of type ``<integer>``.
   :keyword withdrawn?: An instance of type ``<boolean>``. Default value: ``#f``.
   :keyword accepts-focus?: An instance of type ``<boolean>``. Default value: ``#t``.
   :keyword cursor: An instance of type :class:`<cursor>`.
   :keyword caret: An instance of type *type-union(<caret>, one-of(#f, #t))*. Default value: ``#f``.
   :keyword foreground: An instance of type :class:`<ink>`.
   :keyword background: An instance of type :class:`<ink>`.
   :keyword text-style: An instance of type :class:`<text-style>`.
   :keyword fixed-width?: An instance of type ``<boolean>``.
   :keyword fixed-height?: An instance of type ``<boolean>``.
   :keyword resizable?: An instance of type ``<boolean>``.

   :description:

     The *port:* init-keyword is true if the pane (and its mirror, if it has
     one) has been mapped, ``#f`` otherwise. In this case, the term *mapped*
     means visible on the display, ignoring issues of occlusion.

     The *help-source:* and *help-context:* keywords let you specify pointers
     to valid information available in any online help you supply with your
     application. The *help-context:* keyword should specify a context-ID
     present in the online help. This context-ID identifies the help topic
     that is applicable to the current pane. The *help-source:* init-keyword
     identifies the source file in which the help topic identified by
     *help-context:* can be found. A list of context-IDs should be provided
     by the author of the online help system.

     The *parent:*, *child:*, and *children:* init-keywords let you specify
     a lineage for the sheet if you wish, specifying the parent of the sheet
     and as many children as you wish.

     The *x:* and *y:* init-keywords specify the initial position of the
     sheet relative to its parent. When *accepts-focus?:* is true, the sheet
     will accept the pointer focus.

     The init-keywords *cursor:*, *foreground:*, *background:*, and
     *text-style:* can be used to specify the appearance of elements in the
     sheet.

     The *caret:* init-keyword is used to specify the caret to be used within
     the drawing pane, if one is to be used at all.

     The *fixed-width?:* and *fixed-height?:* init-keywords are used to fix
     the width or height of a sheet to the size defined by other appropriate
     init-keywords. This is a useful way of ensuring that the default size
     defined for a sheet is fixed in either direction. The init-keywords
     force the space requirements for the sheet to make the minimum and
     maximum sizes equal to the size defined at the time of creation. These
     keywords are most useful when creating sheets of unknown size, when you
     want to ensure that any child of that sheet is fixed at that size,
     whatever it may be.

     If *resizable?:* is ``#t`` then the sheet can be resized in either
     direction. If *resizable?:* is ``#f`` then it cannot be resized in either
     direction. If *resizable?:* is ``#t``, but one of *fixed-width?:* or
     *fixed-height?:* is ``#t``, then the sheet can only be resized in one
     direction as appropriate.

   :operations:

     The following operations are exported from the *DUIM-Sheets* module.

     - :gf:`add-child`
     - :gf:`beep`
     - :gf:`child-containing-position`
     - :gf:`children-overlapping-region`
     - :gf:`clear-box`
     - :gf:`destroy-sheet`
     - :gf:`display`
     - :gf:`do-children-containing-position`
     - :gf:`do-children-overlapping-region`
     - :gf:`do-sheet-children`
     - :gf:`do-sheet-tree`
     - :gf:`do-with-drawing-options`
     - :gf:`do-with-pointer-grabbed`
     - :gf:`do-with-sheet-medium`
     - :gf:`do-with-text-style`
     - :gf:`do-with-transform`
     - :gf:`force-display`
     - :gf:`frame-manager`
     - :gf:`get-default-background`
     - :gf:`get-default-foreground`
     - :gf:`get-default-text-style`
     - :gf:`handle-event`
     - :gf:`handle-repaint`
     - :gf:`medium-background`
     - :gf:`medium-background-setter`
     - :gf:`medium-brush`
     - :gf:`medium-brush-setter`
     - :gf:`medium-clipping-region`
     - :gf:`medium-clipping-region-setter`
     - :gf:`medium-default-text-style`
     - :gf:`medium-default-text-style-setter`
     - :gf:`medium-foreground`
     - :gf:`medium-foreground-setter`
     - :gf:`medium-pen`
     - :gf:`medium-pen-setter`
     - :gf:`medium-text-style`
     - :gf:`medium-text-style-setter`
     - :gf:`medium-transform`
     - :gf:`medium-transform-setter`
     - :gf:`port`
     - :gf:`queue-event`
     - :gf:`queue-repaint`
     - :gf:`raise-sheet`
     - :gf:`remove-child`
     - :gf:`repaint-sheet`
     - :gf:`replace-child`
     - :gf:`set-sheet-edges`
     - :gf:`set-sheet-position`
     - :gf:`set-sheet-size`
     - :gf:`sheet?`
     - :gf:`sheet-ancestor?`
     - :gf:`sheet-child`
     - :gf:`sheet-children`
     - :gf:`sheet-children-setter`
     - :gf:`sheet-child-setter`
     - :gf:`sheet-edges`
     - :gf:`sheet-frame`
     - :gf:`sheet-mapped?`
     - :gf:`sheet-mapped?-setter`
     - :gf:`sheet-medium`
     - :gf:`sheet-parent`
     - :gf:`sheet-parent-setter`
     - :gf:`sheet-position`
     - :gf:`sheet-region`
     - :gf:`sheet-region-setter`
     - :gf:`sheet-size`
     - :gf:`sheet-state`
     - :gf:`sheet-transform`
     - :gf:`sheet-transform-setter`
     - :gf:`sheet-withdrawn?`
     - :gf:`synchronize-display`
     - :gf:`text-size`
     - :gf:`top-level-sheet`

     The following operations are exported from the *DUIM-Gadgets* module.

     - :gf:`scroll-position`
     - :gf:`set-scroll-position`

     The following operations are exported from the *DUIM-Layouts* module.

     - :gf:`allocate-space`
     - :gf:`compose-space`
     - :gf:`do-allocate-space`
     - :gf:`do-compose-space`
     - :gf:`relayout-children`
     - :gf:`relayout-parent`
     - :gf:`space-requirement-height`
     - :gf:`space-requirement-max-height`
     - :gf:`space-requirement-max-width`
     - :gf:`space-requirement-min-height`
     - :gf:`space-requirement-min-width`
     - :gf:`space-requirement-width`

     The following operations are exported from the *DUIM-Frames* module.

     - :gf:`exit-dialog`

     The following operations are exported from the *DUIM-Graphics* module.

     - :gf:`abort-path`
     - :gf:`arc-to`
     - :gf:`close-path`
     - :gf:`copy-area`
     - :gf:`curve-to`
     - :gf:`do-with-output-to-pixmap`
     - :gf:`draw-bezier-curve`
     - :gf:`draw-ellipse`
     - :gf:`draw-image`
     - :gf:`draw-line`
     - :gf:`draw-lines`
     - :gf:`draw-pixmap`
     - :gf:`draw-point`
     - :gf:`draw-points`
     - :gf:`draw-polygon`
     - :gf:`draw-rectangle`
     - :gf:`draw-text`
     - :gf:`end-path`
     - :gf:`fill-path`
     - :gf:`line-to`
     - :gf:`move-to`
     - :gf:`restore-clipping-region`
     - :gf:`start-path`
     - :gf:`stroke-path`

     The following operations are exported from the *DUIM-DCS* module.

     - :gf:`default-background <dcs.htm#19900>`
     - :gf:`default-foreground <dcs.htm#40602>`
     - :gf:`default-text-style <dcs.htm#95321>`

     The following operations are exported from the *DUIM-Geometry* module.

     - :gf:`box-edges`

     The following operations are exported from the *DUIM-Extended-Geometry*
     module.

     - :gf:`draw-design`

     Examples

     To make a text editor that is fixed at 10 lines high:

     .. code-block:: dylan

         make(<text-editor>, lines: 10, fixed-height?: #t);

   See also

   - :class:`<display>`
   - :class:`<port>`

.. generic-function:: sheet?

   Returns true if the specified object is a sheet.

   :signature: sheet? *object* => *boolean*

   :parameter object: An instance of type :drm:`<object>`.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns true if *object* is a sheet.

   See also

   - :gf:`medium?`

.. generic-function:: sheet-ancestor?

   Returns true if the specified sheet has the specified ancestor.

   :signature: sheet-ancestor? *sheet putative-ancestor* => *boolean*

   :parameter sheet: An instance of type :class:`<sheet>`.
   :parameter putative-ancestor: An instance of type :class:`<sheet>`.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns true if *putative-ancestor* is an ancestor of *sheet*.

   See also

   - :gf:`sheet?`

.. generic-function:: sheet-child

   Returns the child of the specified sheet.

   :signature: sheet-child *sheet* => *child*

   :parameter sheet: An instance of type :class:`<sheet>`.

   :parameter child: An instance of type *false-or(* :class:`<sheet>` *)*.

   :description:

     Returns the child of *sheet*.

   See also

   - :gf:`sheet-children`
   - :gf:`sheet-child-setter`

.. generic-function:: sheet-children

   Returns a list of sheets that are the children of the specified sheet.

   :signature: sheet-children *sheet* => *sheets*

   :parameter sheet: An instance of type :class:`<sheet>`.

   :parameter sheets: An instance of type *limited(<sequence>, of:* :class:`<sheet>` *)*.

   :description:

     Returns a list of sheets that are the children of *sheet*. Some sheet
     classes support only a single child; in this case, the return value of
     sheet-children is a list of one element.

   See also

   - :gf:`do-sheet-children`
   - :gf:`sheet-child`
   - :gf:`sheet-children-setter`

.. generic-function:: sheet-children-setter

   Sets the children of the specified sheet.

   :signature: sheet-children-setter *children sheet* => *sheets*

   :parameter children: An instance of type *limited(<sequence>, of:* :class:`<sheet>` *)*.
   :parameter sheet: An instance of type :class:`<sheet>`.

   :parameter children: An instance of type *limited(<sequence>, of:* :class:`<sheet>` *)*.

   :description:

     Sets the children of *sheet*. Some sheet classes support only a single
     child; in this case, *children* is a list of one element.

   See also

   - :gf:`sheet-children`
   - :gf:`sheet-child-setter`

.. generic-function:: sheet-child-setter

   Sets the child of the specified sheet.

   :signature: sheet-child-setter *child sheet* => *child*

   :parameter child: An instance of type :class:`<sheet>`.
   :parameter sheet: An instance of type :class:`<sheet>`.

   :parameter child: An instance of type *false-or(* :class:`<sheet>` *)*.

   :description:

     Sets the child of *sheet*.

   See also

   - :gf:`sheet-child`
   - :gf:`sheet-children-setter`

.. generic-function:: sheet-edges

   Returns the edges of the specified sheet, relative to its parent.

   :signature: sheet-edges *sheet* => *left top right bottom*

   :parameter sheet: An instance of type :class:`<sheet>`.

   :parameter left: An instance of type :class:`<coordinate>`.
   :parameter top: An instance of type :class:`<coordinate>`.
   :parameter right: An instance of type :class:`<coordinate>`.
   :parameter bottom: An instance of type :class:`<coordinate>`.

   :description:

     Returns the edges of *sheet*. Each edge is specified relative to the
     corresponding edge of the parent of *sheet*.

   See also

   - :gf:`set-sheet-edges`
   - :gf:`sheet-position`
   - :gf:`sheet-size`
   - :gf:`sheet-transform`

.. class:: <sheet-event>
   :open:
   :abstract:

   The class of events that can occur in sheets.

   :superclasses: :class:`<event>`

   :keyword sheet: An instance of type *false-or(* :class:`<sheet>` *)*. Required.

   :description:

     The class of events that can occur in sheets.

     The required init-keyword *sheet:* specifies a sheet in which the event
     occurs.

   :operations:

     The following operation is exported from the *DUIM-Sheets* module.

     - :gf:`event-sheet`

   See also

   - :class:`<device-event>`

.. generic-function:: sheet-event-mask

   Returns the event mask of the specified sheet.

   :signature: sheet-event-mask *sheet* => *integer*

   :parameter sheet: An instance of type :class:`<sheet>`.

   :value integer: An instance of type ``<integer>``.

   :description:

     Returns the event mask of *sheet*.

   See also

   - :gf:`sheet-event-mask-setter`
   - :gf:`sheet-event-queue`

.. generic-function:: sheet-event-mask-setter

   Sets the event mask of the specified sheet.

   :signature: sheet-event-mask-setter *mask sheet* => *mask*

   :parameter mask: An instance of type ``<integer>``.
   :parameter sheet: An instance of type :class:`<sheet>`.

   :value mask: An instance of type ``<integer>``.

   :description:

     Sets the event mask of *sheet*.

   See also

   - :gf:`sheet-event-mask`

.. generic-function:: sheet-event-queue

   Returns the event queue of the specified sheet.

   :signature: sheet-event-queue *sheet* => *event-queue*

   :parameter sheet: An instance of type :class:`<sheet>`.

   :value event-queue: An instance of type :class:`<event-queue>`.

   :description:

     Returns the event mask of *sheet*. This is a list of all the events
     that are currently queued ready for execution.

   See also

   - :gf:`sheet-event-mask`

.. generic-function:: sheet-frame

   Returns the frame associated with the specified sheet.

   :signature: sheet-frame *sheet* => *frame*

   :parameter sheet: An instance of type :class:`<sheet>`.

   :parameter frame: An instance of type *false-or(* :class:`<frame>` *)*.

   :description:

     Returns the frame associated with *sheet*.

   See also

   - :gf:`sheet-medium`
   - :gf:`sheet-parent`

.. generic-function:: sheet-mapped?

   Returns true if the specified sheet is mapped.

   :signature: sheet-mapped? *sheet* => *mapped?*

   :parameter sheet: An instance of type :class:`<sheet>`.

   :value mapped?: An instance of type ``<boolean>``.

   :description:

     Returns true if *sheet* is mapped, that is, displayed on screen (issues
     of occluding windows notwithstanding).

   See also

   - :gf:`sheet-mapped?-setter`
   - :gf:`sheet-withdrawn?`

.. generic-function:: sheet-mapped?-setter

   Specifies whether the specified sheet is mapped.

   :signature: sheet-mapped?-setter *mapped? sheet* => *boolean*

   :parameter mapped?: An instance of type ``<boolean>``.
   :parameter sheet: An instance of type :class:`<sheet>`.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Specifies whether *sheet* is mapped, that is, displayed on screen
     (issues of occluding windows notwithstanding). If ``#t``, *sheet* is
     mapped, if ``#f``, it is not.

   See also

   - :gf:`sheet-mapped?`

.. generic-function:: sheet-medium

   Returns the medium associated with the specified sheet.

   :signature: sheet-medium *sheet* => *medium*

   :parameter sheet: An instance of type :class:`<sheet>`.

   :parameter medium: An instance of type *false-or(* :class:`<medium>` *)*.

   :description:

     Returns the medium associated with *sheet*.

   See also

   - :gf:`sheet-frame`

.. generic-function:: sheet-parent

   Returns the parent of the specified sheet.

   :signature: sheet-parent *sheet* => *parent*

   :parameter sheet: An instance of type :class:`<sheet>`.

   :parameter parent: An instance of type *false-or(* :class:`<sheet>` *)*.

   :description:

     Returns the parent of *sheet*.

   See also

   - :gf:`sheet-medium`
   - :gf:`sheet-parent-setter`
   - :gf:`sheet-position`

.. generic-function:: sheet-parent-setter

   Sets the parent of the specified sheet.

   :signature: sheet-parent-setter *parent sheet* => *value*

   :parameter parent: An instance of type *false-or(* :class:`<sheet>` *)*.
   :parameter sheet: An instance of type :class:`<sheet>`.

   :value value: An instance of type *false-or(* :class:`<sheet>` *)*.

   :description:

     Sets the parent of *sheet*.

   See also

   - :gf:`sheet-parent`

.. generic-function:: sheet-pointer-cursor

   Returns the pointer cursor associated with the specified sheet.

   :signature: sheet-pointer-cursor *sheet* => *cursor*

   :parameter sheet: An instance of type :class:`<sheet>`.

   :value cursor: An instance of type :class:`<cursor>`.

   :description:

     Returns the pointer cursor associated with *sheet*. This is the cursor
     used to represent the mouse pointer whenever the mouse pointer is inside
     the boundary of *sheet*.

   See also

   - :gf:`sheet-pointer-cursor-setter`
   - :gf:`sheet-text-cursor`

.. generic-function:: sheet-pointer-cursor-setter

   Sets the pointer cursor associated with the specified sheet.

   :signature: sheet-pointer-cursor-setter *cursor sheet* => *cursor*

   :parameter cursor: An instance of type :class:`<cursor>`.
   :parameter sheet: An instance of type :class:`<sheet>`.

   :value cursor: An instance of type :class:`<cursor>`.

   :description:

     Sets the pointer cursor associated with *sheet*. This is the cursor
     used to represent the mouse pointer whenever the mouse pointer is inside
     the boundary of *sheet*.

   See also

   - :gf:`sheet-pointer-cursor`

.. generic-function:: sheet-position

   Returns the position of the specified sheet relative to its parent.

   :signature: sheet-position *sheet* => *x y*

   :parameter sheet: An instance of type :class:`<sheet>`.

   :value x: An instance of type ``<real>``.
   :value y: An instance of type ``<real>``.

   :description:

     Returns the position of *sheet*. The position is represented by the
     coordinate (x,y), as measured relative to the parent of *sheet*, or
     relative to the top left of the screen if *sheet* has no parent.

   See also

   - :gf:`set-sheet-position`
   - :gf:`sheet-edges`
   - :gf:`sheet-parent`
   - :gf:`sheet-size`
   - :gf:`sheet-transform`

.. generic-function:: sheet-region

   Returns the region associated with the specified sheet.

   :signature: sheet-region *sheet* => *region*

   :parameter sheet: An instance of type :class:`<sheet>`.

   :value region: An instance of type :class:`<region>`.

   :description:

     Returns an instance of :class:`<region>` that represents
     the set of points to which *sheet* refers. The region is expressed in
     the same coordinate system as *sheet*.

   See also

   - :gf:`sheet-region-setter`

.. generic-function:: sheet-region-setter

   Sets the region associated with the specified sheet.

   :signature: sheet-region-setter *region sheet* => *region*

   :parameter region: An instance of type :class:`<region>`.
   :parameter sheet: An instance of type :class:`<sheet>`.

   :value region: An instance of type :class:`<region>`.

   :description:

     Creates or modifies an instance of :class:`<region>` that represents the
     set of points to which *sheet* refers. The region is expressed in the same
     coordinate system as *sheet*.

   See also

   - :gf:`sheet-region`

.. generic-function:: sheet-size

   Returns the width and height of the specified sheet.

   :signature: sheet-size *sheet* => *width height*

   :parameter sheet: An instance of type :class:`<sheet>`.

   :value width: An instance of type ``<integer>``.
   :value height: An instance of type ``<integer>``.

   :description:

     Returns the width and height of the specified sheet. Use
     :gf:`set-sheet-size` to set or modify the size of a sheet.

   See also

   - :gf:`set-sheet-size`
   - :gf:`sheet-edges`
   - :gf:`sheet-position`
   - :gf:`sheet-transform`

.. generic-function:: sheet-state

   Returns the current state of the specified sheet.

   :signature: sheet-state *sheet* => *value*

   :parameter sheet: An instance of type :class:`<sheet>`.

   :value value: An instance of type *one-of(#"withdrawn", #"managed", #"mapped", #"unknown")*.

   :description:

     Returns the current state of *sheet*. The state of a sheet tells you
     whether the sheet is currently mapped on screen, or whether it has been
     withdrawn from the list of sheets.

.. generic-function:: sheet-text-cursor

   Returns the text cursor associated with the specified sheet.

   :signature: sheet-text-cursor *sheet* => *text-cursor*

   :parameter sheet: An instance of type :class:`<sheet>`.

   :parameter text-cursor: An instance of type *false-or(* :class:`<cursor>` *)*.

   :description:

     Returns the text cursor associated with *sheet*. The text cursor
     associated with a sheet is distinct from the pointer cursor associated
     with the same sheet: the pointer cursor represents the current position
     of the pointer associated with the attached pointer device, while the
     text cursor represents the position in the sheet that any text typed
     using the keyboard will be added. Only those sheets that contain
     children that allow some form of text-based input have an associated
     text cursor.

   See also

   - :gf:`sheet-pointer-cursor`

.. generic-function:: sheet-transform

   Returns the transform associated with the specified sheet.

   :signature: sheet-transform *sheet* => *transform*

   :parameter sheet: An instance of type :class:`<sheet>`.

   :value transform: An instance of type :class:`<transform>`.

   :description:

     Returns the transform associated with *sheet*.

   See also

   - :gf:`medium-transform`
   - :gf:`sheet-edges`
   - :gf:`sheet-position`
   - :gf:`sheet-size`

.. generic-function:: sheet-transform-setter

   Sets the transform associated with the specified sheet.

   :signature: sheet-transform-setter *transform sheet* => *transform*

   :parameter transform: An instance of type :class:`<transform>`.
   :parameter sheet: An instance of type :class:`<sheet>`.

   :value transform: An instance of type :class:`<transform>`.

   :description:

     Sets or modifies the transform associated with *sheet*.

   See also

   - :gf:`medium-transform-setter`

.. generic-function:: sheet-withdrawn?

   Returns true if the specified sheet has been withdrawn from the display.

   :signature: sheet-withdrawn? *sheet* => *withdrawn?*

   :parameter sheet: An instance of type :class:`<sheet>`.

   :value withdrawn?: An instance of type ``<boolean>``.

   :description:

     Returns true if *sheet* has been withdrawn from the display, and is no
     longer mapped.

   See also

   - :gf:`sheet-mapped?`

.. constant:: $shift-key

   A constant that represents the SHIFT key on the keyboard.

   :type: :class:`<integer>`

   :value: ash(1, %modifier\_base + 0);

   :description:

     A constant that represents the SHIFT key on the keyboard.

   See also

   - :const:`$alt-key`
   - :const:`$control-key`
   - :const:`$hyper-key`
   - :const:`$meta-key`
   - :gf:`modifier-key-index`
   - :gf:`modifier-key-index-name`
   - :const:`$modifier-keys`
   - :const:`$option-key`
   - :const:`$super-key`

.. constant:: $super-key

   A constant that represents the SUPER key on the keyboard.

   :type: :class:`<integer>`

   :value: ash(1, %modifier\_base + 3);

   :description:

     A constant that represents the SUPER key on the keyboard, if it exists.
     To deal with the case where there is no SUPER key, the value of the
     constant :const:`$option-key` is bound to this
     constant.

   See also

   - :const:`$alt-key`
   - :const:`$control-key`
   - :const:`$hyper-key`
   - :const:`$meta-key`
   - :gf:`modifier-key-index`
   - :gf:`modifier-key-index-name`
   - :const:`$modifier-keys`
   - :const:`$option-key`
   - :const:`$shift-key`

.. generic-function:: synchronize-display

   Synchronizes all displays on which the specified drawable is mapped.

   :signature: synchronize-display *drawable* => ()

   :parameter drawable: An instance of type *type-union(* :class:`<sheet>`, :class:`<medium>` *)*.


   :description:

     Synchronizes all displays on which the specified drawable is mapped.

.. generic-function:: text-size

   Returns information about the size of the specified text on the
   specified medium.

   :signature: text-size *medium text* #key *text-style start end do-newlines?* => *largest-x largest-y cursor-x cursor-y baseline*

   :parameter medium: An instance of type :class:`<medium>`.
   :parameter text: An instance of type *type-union(<string>, <character>)*.
   :parameter text-style: An instance of type :class:`<text-style>`.
   :parameter start: An instance of type ``<integer>``. Default value: 0.
   :parameter end: An instance of type ``<integer>``. Default value: *size(* *text* *)*.
   :parameter do-newlines?: An instance of type ``<boolean>``. Default value: ``#f``.
   :parameter do-tabs?: An instance of type ``<boolean>``. Default value: ``#f``.

   :value largest-x: An instance of type ``<integer>``.
   :value total-height: An instance of type ``<integer>``.
   :value last-x: An instance of type ``<integer>``.
   :value last-y: An instance of type ``<integer>``.
   :value baseline: An instance of type ``<integer>``.

   :description:

     Returns information about the size of *text* on *medium*.

     If *text-style* is specified, then the information that *text-size*
     returns is based on the text style it describes.

     If *start* and *end* are specified, then these values represent a
     portion of the string specified by *text*, and only the characters they
     represent are examined by *text-size*. Both *start* and *end* represent
     the index of each character in *text*, starting at 0. By default, the
     whole of *text* is examined.

     The *do-newlines?* and *do-tabs?* arguments let you specify how newline
     or tab characters in *text* should be handled. If either of these
     arguments is true, then any newline or tab characters in text are
     examined, as appropriate. By default, newline characters are ignored.

.. generic-function:: text-style-mapping

   Returns the mapping for the specified text style on the specified port.

   :signature: text-style-mapping *port text-style* #key *character-set* => *font*

   :parameter port: An instance of type :class:`<port>`.
   :parameter text-style: An instance of type :class:`<text-style>`
   :parameter character-set: An instance of type :drm:`<object>`.

   :value font: An instance of type :drm:`<object>`.

   :description:

     Returns the mapping for *text-style* on *port*. Mapping text styles
     onto fonts lets you control how different text styles are displayed on
     different servers, depending on the connection. For instance, it is
     possible to define how colored text is displayed on monochrome displays,
     or how fonts specified by *text-style* are mapped onto fonts available
     on the display.

     If *character-set* is specified, then this character set is used instead
     of the default. This is most useful for non-English displays.

   See also

   - :gf:`text-style-mapping-exists?`
   - :gf:`text-style-mapping-setter`
   - :class:`<undefined-text-style-mapping>`

.. generic-function:: text-style-mapping-exists?

   Returns true if a mapping exists for the specified text style on the
   specified port.

   :signature: text-style-mapping-exists? *port text-style* #key *character-set exact-size?* => *boolean*

   :parameter port: An instance of type :class:`<port>`.
   :parameter text-style: An instance of type :class:`<text-style>`.
   :parameter character-set: An instance of type :drm:`<object>`.
   :parameter exact-size?: An instance of type ``<boolean>``. Default value: ``#f``.

   :value boolean: An instance of type ``<boolean>``.

   :description:

     Returns true if a mapping exists for *text-style* on *port*. This
     control function is useful if, for example, you are setting up text
     style mappings for a range of text styles in one go, or for a range of
     different ports. Using this function, you can test for the existence of
     a previous mapping before creating a new one, thereby ensuring that
     existing mappings are not overwritten.

   See also

   - :gf:`text-style-mapping`
   - :gf:`text-style-mapping-setter`
   - :class:`<undefined-text-style-mapping>`

.. generic-function:: text-style-mapping-setter

   Sets the mapping for the specified text style on the specified port.

   :signature: text-style-mapping-setter *font port text-style* #key *character-set* => *font*

   :parameter font: An instance of type :drm:`<object>`.
   :parameter port: An instance of type :class:`<port>`.
   :parameter text-style: An instance of type :class:`<text-style>`.
   :parameter character-set: An instance of type :drm:`<object>`.

   :value font: An instance of type :drm:`<object>`.

   :description:

     Sets the mapping for *text-style* on *port* to the specified *font*.
     This function lets you have some control over the way in which different
     text styles are displayed on different servers, depending on the
     connection. Using this function, for instance, it is possible to define
     how colored text is displayed on monochrome displays, or how fonts
     specified by *text-style* are mapped onto fonts available on the
     display.

     If *character-set* is specified, then this character set is used instead
     of the default. This is most useful for non-English displays.

   See also

   - :gf:`text-style-mapping`
   - :gf:`text-style-mapping-exists?`
   - :class:`<undefined-text-style-mapping>`

.. class:: <timer-event>
   :sealed:
   :instantiable:

   The class of timed events.

   :superclasses: :class:`<frame-event>`

   :description:

     The class of timed events.

   :operations:

.. generic-function:: top-level-sheet

   Returns the top level sheet for the specified object.

   :signature: top-level-sheet *object* => *top-level-sheet*

   :parameter object: An instance of type :drm:`<object>`.

   :value top-level-sheet: An instance of type *false-or(* :class:`<sheet>` *)*.

   :description:

     Returns the top level sheet for *object*. This is the sheet that has as
     its descendents all of the panes of *object*.

.. class:: <undefined-text-style-mapping>
   :sealed:
   :instantiable:

   The class of undefined text style mappings.

   :superclasses: :class:`<error>`

   :keyword port:: An instance of type :class:`<port>`. Required.
   :keyword text-style:: An instance of type :class:`<text-style>`. Required.

   :description:

     The class of undefined text style mappings. This class is used for any
     text styles that have not had mappings defined for a given port.

   :operations:

   See also

   - :gf:`text-style-mapping`
   - :gf:`text-style-mapping-exists?`
   - :gf:`text-style-mapping-setter`

.. class:: <window-configuration-event>
   :sealed:
   :instantiable:

   The class of events involving changes to the window configuration.

   :superclasses: :class:`<window-event>`

   :description:

     The class of events involving changes to the window configuration.

   :operations:

   See also

   - :class:`<window-repaint-event>`

.. class:: <window-event>
   :open:
   :abstract:

   The base class of events that occur in windows.

   :superclasses: :class:`<sheet-event>`

   :keyword region: An instance of type :class:`<region>`. Required.

   :description:

     The base class of events that occur in windows. Two types of event can
     occur:

     - Changes to the configuration of the window.
     - Changes that require the window to be repainted.

     The *region:* init-keyword specifies a region in which the event occurs.

   :operations:

     The following operation is exported from the *DUIM-Sheets* module.

     - :gf:`event-region`

   See also

   - :gf:`event-region`
   - :class:`<window-configuration-event>`
   - :class:`<window-repaint-event>`

.. class:: <window-repaint-event>
   :sealed:
   :instantiable:

   The class of events involving repainting of a window.

   :superclasses: :class:`<window-event>`

   :description:

     The class of events involving repainting of a window.

   :operations:

   See also

   - :gf:`handle-repaint`
   - :gf:`queue-repaint`
   - :gf:`repaint-sheet`
   - :class:`<window-configuration-event>`

.. macro:: with-brush

   Executes the supplied code using the specified brush characteristics.

   :macrocall: with-brush ({*medium* } #rest {*brush-initargs* }\*) {*body* } end

   :parameter medium: A Dylan body*bnf*.
   :parameter brush-initargs: Dylan arguments*bnf*.
   :parameter body: A Dylan body*bnf*.

   :description:

     Executes *body* using the brush characteristics specified by
     *brush-initargs*, and applies the results to *medium*. The *medium*
     specified should be an instance of type :class:`<medium>`. The
     *brush-initargs* can be any valid arguments that specify an instance of
     :class:`<brush>`.

   See also

   - :gf:`with-pen`

.. macro:: with-clipboard

   Evaluates a body of code with a clipboard grabbed.

   :macrocall: with-clipboard (*clipboard* = *sheet*) *body* end

   :parameter clipboard: A Dylan variable-name*bnf*.
   :parameter sheet: A Dylan variable-name*bnf*.
   :parameter body: A Dylan body*bnf*.

   :value values: Instances of :drm:`<object>`.

   :description:

     Evaluates *body* with the clipboard grabbed, returning the results to
     the clipboard.

     The macro grabs a lock on the clipboard, using *open-clipboard*, and
     then executes *body*. Once the results of evaluating *body* have been
     sent to the clipboard, the clipboard lock is freed using
     :gf:`close-clipboard`. The *clipboard* argument is a
     Dylan variable-name*bnf* used locally in the call to *with-clipboard*.
     The *sheet* argument is a Dylan variable-name*bnf* that evaluates to the
     sheet associated with *clipboard*.

     This macro is the easiest way of manipulating the clipboard from DUIM,
     since it removes the need to create and destroy a clipboard lock
     yourself.

     You can add more than one format of your data to the clipboard within
     the scope of this macro. So, for example, you could place an arbitrary
     object onto the clipboard, for use within your own application, and a
     string representation for other tools applications to see.

   See also

   - :class:`<clipboard>`

.. macro:: with-clipping-region

   Executes the supplied code using the specified clipping region.

   :macrocall: with-clipping-region ({*medium* } {*region* }) {*body* } end

   :parameter medium: A Dylan expression*bnf*.
   :parameter region: A Dylan expression*bnf*.
   :parameter body: A Dylan body*bnf*.

   :description:

     Executes *body* using the clipping region specified by *region*, and
     applies the results to *medium*. The *region* and *medium* expressions
     should evaluate to instances of :class:`<region>` and
     :class:`<medium>`, respectively.

.. macro:: with-cursor-visible

   Executes the supplied code using the specified cursor settings for a
   sheet.

   :macrocall: with-cursor-visible ({*sheet* } {*visible?* }) {*body* } end

   :parameter sheet: A Dylan expression*bnf*.
   :parameter visible?: A Dylan expression*bnf*.
   :parameter body: A Dylan body*bnf*.

   :description:

     Executes *body* on the specified *sheet*. If *visible?* is true, then the
     pointer cursor associated with *sheet* is visible throughout the
     operation. If *visible?* is false, then the pointer cursor is hidden.

     The expression *sheet* should evaluate to an instance of :class:`<sheet>`.
     The expression *visible?* should evaluate to a boolean value.

.. macro:: with-drawing-options

   Runs a body of code in the context of a set of drawing options.

   :macrocall: with-drawing-options ({*medium* } #rest {*options* }\*) {*body* } end

   :parameter medium: A Dylan expression*bnf*.
   :parameter options: Dylan arguments*bnf*.
   :parameter body: A Dylan body*bnf*.

   :description:

     Runs a body of code in the context of a set of drawing options. The
     options specified are passed to the function :gf:`do-with-drawing-options`
     for execution.

     The *medium* expression should evaluate to an instance of :class:`<medium>`.

     Note that when using *with-drawing-options* in conjunction with a loop.
     it is computationally much quicker to use a medium (as shown here) rather
     than a sheet, and to place the call to with-drawing-options outside the
     loop. If necessary, use :macro:`with-sheet-medium` to associate the sheet
     with the medium, thus:

     .. code-block:: dylan

         with-sheet-medium (medium = sheet)
           with-drawing-options (medium, brush: color)
             for (x :: <integer> from 0 to 199)
               for (y :: <integer> from 0 to 199)
                 draw-point(medium, x, y)
                end
              end
            end
          end

     Example

     .. code-block:: dylan

         with-drawing-options (medium, brush: $red)
           draw-rectangle (medium, 0, 0, 100, 200, filled?: #t)
         end;

   See also

   - :gf:`do-with-drawing-options`
   - :gf:`with-sheet-medium`

.. generic-function:: withdraw-sheet

   Withdraws the specified sheet from the current display.

   :signature: withdraw-sheet *sheet* => ()

   :parameter sheet: An instance of type :class:`<sheet>`.

   :description:

     Withdraws the specified sheet from the current display.

.. macro:: with-frame-manager

   Executes the supplied code in the context of the specified frame
   manager.

   :macrocall: with-frame-manager ({*framem* }) {*body* } end

   :parameter framem: A Dylan expression*bnf*.
   :parameter body: A Dylan body*bnf*.

   :description:

     Executes *body* in the context of *framem*, by dynamically binding the
     expression *framem* to *\*current-frame-manager\**.

     In practice, you do not need to use *with-frame-manager* unless you are
     certain that your code needs to run on a non-primary frame manager.

     The main place where you need to use this macro is when you call *make*
     to create a gadget *outside* of one of the pane or layout clauses in
     *define frame*.

     Unless you are developing code that needs to run on more than one
     platform, this is unlikely to be the case, and you can forego use of
     this macro.

   See also

   - :class:`<frame-manager>`

.. macro:: with-identity-transform

   Executes the supplied code while retaining the current transform.

   :macrocall: with-identity-transform ({*medium* }) {*body* } end

   :parameter medium: A Dylan expression*bnf*.
   :parameter body: A Dylan body*bnf*.

   :description:

     Executes *body* while retaining the current transform for *medium*.

     The *medium* expression should evaluate to an instance of
     :class:`<medium>`.

.. macro:: with-pen

   Executes the supplied code using the specified pen characteristics.

   :macrocall: with-pen ({*medium* } #rest {*pen-initargs* }\*) {*body* } end

   :parameter medium: A Dylan expression*bnf*.
   :parameter pen-initargs: Dylan arguments*bnf*.
   :parameter body: A Dylan body*bnf*.

   :description:

     Executes *body* using the pen characteristics specified by
     *pen-initargs*, and applies the results to the expression *medium*.

     The *medium* specified should be an instance of type
     :class:`<medium>`. The *pen-initargs* can be any valid
     arguments that specify an instance of :class:`<pen>`.

   See also

   - :gf:`with-brush`

.. macro:: with-pointer-grabbed

   Executes a body of code, forwarding all pointer events to a sheet.

   :macrocall: with-pointer-grabbed ({*sheet* } #rest {*options* }\*) {*body* } end

   :parameter sheet: A Dylan expression*bnf*.
   :parameter options: Dylan arguments*bnf*.
   :parameter body: A Dylan body*bnf*.

   :description:

     Executes a body of code, forwarding all pointer events to *sheet*, even
     if the pointer leaves the sheet-region of *sheet*. The *sheet*
     specified should be an instance of type :class:`<sheet>`.

     The macro calls methods for *do-with-pointer-grabbed*. The code
     specified by *body* is used to create a stand-alone method that is used
     as the code that is run by *do-with-pointer-grabbed*.

   See also

   - :gf:`do-with-pointer-grabbed`

.. macro:: with-rotation

   Executes a body of code with a specified rotation.

   :macrocall: with-rotation ({*medium* } {*angle* }) {*body* } end

   :parameter medium: A Dylan expression*bnf*.
   :parameter angle: A Dylan argument*bnf*.
   :parameter body: A Dylan body*bnf*.

   :description:

     Executes a body of code with a specified rotation. The rotation occurs
     within the expression *medium*. This macro calls :macro:`with-transform`
     to perform the rotation.

     The *medium* specified should be an instance of type :class:`<medium>`.
     The *angle* should evaluate to an instance of type ``<real>``.

   See also

   - :gf:`with-scaling`
   - :gf:`with-transform`
   - :gf:`with-translation`

.. macro:: with-scaling

   Executes a body of code with a specified scaling.

   :macrocall: with-scaling ({*medium* } {*scale-x* } {*scale-y* }) {*body* } end

   :parameter medium: A Dylan expression*bnf*.
   :parameter scale-x: A Dylan argument*bnf*.
   :parameter scale-y: A Dylan argument*bnf*.
   :parameter body: A Dylan body*bnf*.

   :description:

     Executes a body of code with a specified scaling, denoted by *scale-x*
     and *scale-y*. The scaling occurs within the expression *medium*. This
     macro calls :macro:`with-transform` to perform the
     scaling.

     The *medium* specified should be an instance of type :class:`<medium>`.
     The *scale-x* and *scale-y* should evaluate to an instance of type
     ``<real>``.

   See also

   - :gf:`with-rotation`
   - :gf:`with-transform`
   - :gf:`with-translation`

.. macro:: with-sheet-medium

   Associates a sheet with a medium.

   :macrocall: with-sheet-medium ({*medium* = *sheet* }) {*body* } end

   :parameter medium: A Dylan name*bnf*.
   :parameter sheet: A Dylan expression*bnf*.
   :parameter body: A Dylan body*bnf*.

   :description:

     Associates a sheet with a medium.

     Within *body*, the variable *medium* is bound to the medium allocated to
     *sheet*. The *sheet* specified should be an instance of type
     :class:`<sheet>`.  If *sheet* does not have a medium permanently
     allocated, one is allocated and associated with *sheet* for the duration
     of *body*, and then unassociated from *sheet* and deallocated when *body*
     has been exited. The values of the last form of *body* are returned as the
     values of *with-sheet-medium*.

     The *medium* argument is not evaluated, and must be a symbol that is bound
     to a medium. The *body* may have zero or more declarations as its first
     forms.

     This macro is a useful way of speeding up drawing operations, since
     drawing on a sheet requires finding the medium for that sheet. You can use
     *with-sheet-medium* to associate a known sheet with a medium, and then
     draw directly onto that medium, as shown in the example.

     Example

     .. code-block:: dylan

         with-sheet-medium (medium = sheet)
           with-drawing-options (medium, brush: color)
             for (x :: <integer> from 0 to 199)
               for (y :: <integer> from 0 to 199)
                 draw-point(medium, x, y)
               end
             end
           end
         end

   See also

   - :gf:`do-with-sheet-medium`
   - :gf:`with-drawing-options`

.. macro:: with-text-style

   Runs a body of code in the context of a text style.

   :macrocall: with-text-style ({*medium* } #rest {*style-initargs* }\*) {*body* } end
   :parameter medium: A Dylan expression*bnf*.
   :parameter style-initargs: Dylan arguments*bnf*.
   :parameter body: A Dylan body*bnf*.

   :description:

     Executes *body* using the text style characteristics specified by
     *style-initargs*, and applies the results to *medium*.

     The *medium* specified should be an instance of type :class:`<medium>`.
     The *style-initargs* can be any valid arguments that specify an instance
     of :class:`<text-style>`.

     Methods for :gf:`do-with-text-style` are invoked to run the code.

   See also

   - :gf:`do-with-text-style`

.. macro:: with-transform

   Executes a body of code with a specified transform.

   :macrocall: with-transform ({*medium* } {*transform* }) {*body* } end

   :parameter medium: A Dylan expression*bnf*.
   :parameter transform: A Dylan expression*bnf*.
   :parameter body: A Dylan body*bnf*.

   :description:

     Executes a body of code with a specified *transform*. The transform occurs
     within *medium*. This macro is used by :macro:`with-rotation`,
     :macro:`with-scaling`, and :macro:`with-translation`, and calls methods
     for :macro:`do-with-transform`.

     The *medium* specified should be an instance of type :class:`<medium>`.
     The *transform* specified should be an instance of type
     :class:`<transform>`.

   See also

   - :gf:`do-with-transform`
   - :gf:`with-rotation`
   - :gf:`with-scaling`
   - :gf:`with-translation`

.. macro:: with-translation

   Executes a body of code with a specified translation.

   :macrocall: with-translation ({*medium* } {*dx* } {*dy* }) {*body* } end

   :parameter medium: A Dylan expression*bnf*.
   :parameter dx: A Dylan argument*bnf*.
   :parameter dy: A Dylan argument*bnf*.
   :parameter body: A Dylan body*bnf*.

   :description:

     Executes a body of code with a specified translation, denoted by *dx*
     and *dy*. The translation occurs within *medium*. This macro calls
     :macro:`with-transform` to perform the translation.

     The *medium* specified should be an instance of type :class:`<medium>`.
     The *dx* and*dy* should evaluate to an instance of type ``<real>``.

   See also

   - :gf:`with-rotation`
   - :gf:`with-scaling`
   - :gf:`with-transform`
