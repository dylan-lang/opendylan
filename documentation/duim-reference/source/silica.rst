*******************
DUIM-Sheets Library
*******************

Overview
========

The elements that comprise a Graphical User Interface (GUI) are arranged
in a hierarchical ordering of object classes. At the top level of the
DUIM hierarchy there are three main classes*,* *<sheet>*, *<gadget>*,
and *<frame>*, all of which are subclasses of *<object>.*

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

The DUIM-Sheets library contains a single module, *duim-sheets*, from
which all the interfaces described in this chapter are exposed. `See
DUIM-Sheets Module`_ contains complete reference
entries for each exposed interface.

A sheet is the basic unit in a DUIM window. Inside any window, sheets
are nested in a parent-child hierarchy. All sheets have the following
attributes:

-  *sheet-region*, expressed in the sheet’s own coordinate system.
-  *sheet-transform*, which maps the sheet’s coordinate system to the
   coordinate system of its parent.
-  *sheet-parent*, which is *#f* if the sheet has no parent.
-  *sheet-mapped?*, which tells if the sheet is visible on a display,
   ignoring issues of occluding windows.

The *sheet-transform* is an instance of a concrete subclass of
*<transform>*. The *sheet-region* can be an instance of any concrete
subclass of *<region>*, but is usually represented by the region class
*<bounding-box>*.

Some sheets (menu bars, button boxes, or tool bars, for instance) also
have single or multiple children, in which case they have additional
attributes:
                                                                                                                                                       

-  A *sheet-children* slot. This is a sequence of sheets. Each sheet in
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
layout facilities (*compose-space* and *allocate-space*)*.*

As a convenience, there are some glue functions that mediate between
geometry and layout: *set-sheet-position*, *set-sheet-size*,
and*set-sheet-edges*.

Some classes of sheet can receive input. These have:

-  A *sheet-event-queue* slot.
-  Methods for *<handle-event>*.

Sheets that can be repainted have methods for *handle-repaint*.

Sheets that can do output, have a *sheet-medium* slot.

Some sheets act as *controls* such as push buttons, scroll bars, and
sliders. These are represented by the *<gadget>* class and its
subclasses.

Other sheets act as layout controls, which allow you to specify how the
elements in a sheet are laid out, whether they are placed vertically or
horizontally, whether they are left, right, or center-aligned, and so
on. These are represented by the *<layout>* class and its subclasses,
and are described in ` <layouts.htm#21962>`_.

A sheet can be associated with a *<display>*, which is an object that
represents a single display (or screen) on some display server.

A display (and all the sheets attached to the display) is associated
with a *<port>* that is a connection to a display server. The port
manages:

-  a primary input device, such as a keyboard.
-  a pointing device, such as a mouse.
-  an event processor that *dispatches* events to the appropriate sheet.

There is a protocol for using the Windows clipboard. In order to
manipulate the Windows clipboard from within DUIM, the clipboard needs
to be locked, so that its contents can be manipulated. DUIM uses the
functions *open-clipboard* and *close-clipboard* to create and free
clipboard locks. The *open-clipboard* function creates an instance of
the class *<clipboard>* which is used to hold the contents of the
clipboard for the duration of the lock. For general use of the
clipboard, use the macro *with-clipboard*, rather than calling
*open-clipboard* and *close-clipboard* explicitly. This lets you
manipulate the clipboard easily, sending the results of any code
evaluated to the clipboard.

Once a clipboard lock has been created, you can use *add-clipboard-data*
and *add-clipboard-data-as* to add data to the clipboard. Use
*get-clipboard-data-as* to query the contents of the clipboard, and use
*clear-clipboard* to empty the locked clipboard. Finally, use
*clipboard-data-available?* to see if the clipboard contains data of a
particular type.

You can put arbitrary Dylan objects onto the clipboard, and retrieve
them within the same process. This gives you the ability to cut and
paste more interesting pieces of an application within the application’s
own domain than would normally be possible.

The DUIM GUI test suite contains a demonstration of how to use the
clipboard in DUIM, in the file

Examples\\duim\\duim-gui-test-suite\\clipboard.dylan
                                                    

in the Harlequin Dylan installation directory.

The class hierarchy for DUIM-Sheets
===================================

This section presents an overview of the available classes exposed by
the DUIM-Sheets library, and describes the class hierarchy present.

The base classes in the DUIM-Sheets library
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The base classes for the majority of subclasses exposed from the
DUIM-Sheets library are *<sheet>* and *<event>*, although a number of
additional subclasses of *<object>* are also exposed.

The base classes exposed by the DUIM-Sheets library are shown in `See
Overall class hierarchy for the DUIM-Sheets
library`_. Only *<sheet>*, and *<event>* have any
subclasses defined. An *<event>* is an object representing some sort of
event. See `Subclasses of <event>`_ for details
of the subclasses of *<event>*.

Overall class hierarchy for the DUIM-Sheets library
                                                   

.. figure:: images/silica-2.png
   :align: center
   :alt: 
<object>

<sheet>

<display>

<port>

<clipboard>

<caret>

<pointer>

<medium•

<frame-manager•

<event>

-  *<sheet>* As already mentioned, a sheet is the basic unit of window
   applications, and they can be nested in a parent-child hierarchy. A
   subclass of sheet is provided — *<display>* — which is an object that
   represents a single display (or screen) on a display server. All
   sheets can be attached to a display.
-  *<port>* A port is a connection to a display server. A display,
   together with all the sheets attached to it, is associated with a
   port, which manages a primary input device, such as a keyboard, a
   pointing device, such as a mouse, and an event processor that
   dispatches events to the appropriate sheet.
-  *<clipboard>* This class is used as a clipboard that can be used to
   hold information temporarily while it is transferred from one sheet
   to another, or between applications. Clipboards provide support for
   the standard *Cut*, *Copy*, and *Paste* commands common in most
   applications.

*<caret>* and *<pointer>*
                         

-  These two classes form an interface between the keyboard and the
   display, and the pointing device and the display, respectively.
-  The *<caret>* represents the position on screen that characters typed
   on the keyboard will be placed. This is often a position in a
   document.
-  The *<pointer>* represents the position of the pointing device on the
   screen, and thus shows the area that will be affected by any events
   generated with the pointing device, such as pressing or clicking one
   of the buttons on the device.

*<pointer-drag-event>*
                      

-  The class of events where the pointer for the pointing device
   attached to the computer is moving, and one of the buttons on the
   pointing device is pressed down as well. The effects of this event
   are rather like a combination of the *<button-press-event>* and
   *<pointer-motion-event>* classes. For more information about these
   and other pointer event classes, see `Subclasses of
   <device-event>`_.

*<pointer-enter-event>*
                       

-  This event is used to describe the event where the pointer for the
   pointing device enters a specified area of the screen, such as a
   sheet. For more information about these and other pointer event
   classes, see `Subclasses of <device-event>`_.
-  *<medium>* A medium represents a destination for drawn or written
   output. It has several items associated with it, such as a drawing
   plane, foreground and background colors, and default line and text
   styles.

*<frame-manager>*
                 

-  A frame manager represents the “look and feel” of a frame. This
   controls standard interface elements for the platform you are
   delivering on, such as the appearance and behavior of title bars,
   borders, menu commands and scroll bars. Unless you are developing for
   more than one platform, you do not need to be too concerned with
   frame managers, since you will only using the default frame manager.

Subclasses of <event>
^^^^^^^^^^^^^^^^^^^^^

`Subclasses of the <event> class`_ shows the
subclasses of the *<event>* class that are exposed by the DUIM-Sheets
library.

Subclasses of the *<event>* class
                                 

.. figure:: images/silica-2.png
   :align: center
   :alt: 
<event>

<frame-event>

<port-terminated-event>

<timer-event>

<sheet-event>

<device-event>

See `Subclasses of <device-event>`_

<window-event>

<window-configuration-event>

<window-repaint-event>

The classes of event that are exposed by the DUIM-Sheets library fall
into two categories:

-  Events that occur in frames: subclasses of the *<frame-event>* class
-  Events that occur in sheets: subclasses of the *<sheet-event>* class

Most subclasses of *<frame-event>* are exposed by the DUIM-Frames
library. See ` <frames.htm#89815>`_, for full details about these
subclasses. However, two subclasses of *<frame-event>* are exposed by
the DUIM-Sheets library:

*<port-terminated-event>*
                         

-  This class represents the event of a port — a connection to a display
   — being terminated.
-  *<timer-event>* This is the class of any event that is timed.

Subclasses of *<sheet-event>* fall into two categories:
                                                       

-  Device events that occur to devices attached to the computer
   (typically the keyboard and the pointing device). These are described
   in `Subclasses of <device-event>`_.
-  Window events that occur in a window.

Events that occur in a window are subclasses of *<window-event>*. Two
such events are supplied:

*<window-configuration-event>*
                              

-  This event occurs whenever the configuration of sheets in a window
   needs to be recalculated. This may occur in property frames, for
   example, when clicking on the available tabs to display different
   pages of information.
-  Sometimes, dialog boxes have buttons that allow you to show or hide
   additional details, which are themselves displayed in an extra pane
   at the bottom or on the right hand side of the dialog. Clicking on
   such a button would also create a *<window-configuration-event>*, as
   the additional pane would need to be displayed or hidden, forcing a
   recalculation of the layout of the sheets in the frame.

*<window-repaint-event>*
                        

-  This event occurs whenever a region of a window needs to be
   repainted. This may occur when refreshing a chart or drawing in a
   frame.

Subclasses of <device-event>
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

`Subclasses of the <device-event> class`_ shows
the subclasses of the *<device-event>* class that are exposed by the
DUIM-Sheets library. Device events, broadly speaking, describe any event
that can occur on a device connected to the computer.

Subclasses of the *<device-event>* class
                                        

.. figure:: images/silica-2.png
   :align: center
   :alt: 
<device-event>

<pointer-event>

<pointer-button-event>

<button-press-event>

<button-release-event>

<button-click-event>

<double-click-event>

<pointer-drag-event>

<pointer-motion-event>

<pointer-drag-event>

<pointer-boundary-event>

<keyboard-event>

<pointer-exit-event>

<key-press-event>

<pointer-enter-event

<key-release-event>

*Note:* The *<pointer-drag-event>* class is a subclass of both
*<pointer-button-event>* and *<pointer-motion-event>*.

Device events fall into two distinct categories:

-  Keyboard events that occur on the keyboard attached to the computer:
   subclasses of *<keyboard-event>*
-  Pointer events that occur on the pointing device attached to the
   computer: subclasses of *<pointer-event>*

There are two classes of keyboard event. The classes *<key-press-event>*
and *<key-release-event>* describe the events that occur when any key on
the keyboard is pressed or released, respectively.

There are three classes of pointer event, some of which provide a number
of subclasses. Note that there are another two classes of pointer event
that are immediate subclasses of *<object>*. These are described in
`The base classes in the DUIM-Sheets library`_.

*<pointer-button-event>*
                        

-  These events occur whenever there is any activity on one of the
   buttons on the pointing device. Several subclasses of this class are
   provided.

*<pointer-exit-event>*
                      

-  This is an event that occurs when the pointer leaves a specified area
   such as a sheet.

*<pointer-motion-event>*
                        

-  This class of events occur when the pointer is in motion. There is
   one subclass provided, *<pointer-boundary-event>*, for the specific
   case when the motion of the pointer causes the boundary of a sheet to
   be crossed.
-  *Note:* Unlike *<pointer-drag-event>*, no button needs to be pressed
   on the attached pointing device.

The subclasses provided for *<pointer-button-event>* are as follows:

*<button-press-event>*
                      

-  This event occurs when any button on the pointing device is pressed
   down by the user. Note that this is distinct from
   *<button-click-event>*, described below.

*<button-release-event>*
                        

-  This event occurs when any previously pressed button on the pointing
   device is released by the user.

*<button-click-event>*
                      

-  This event occurs when any button on the pointing device is pressed
   down by the user and then released again within a certain time frame.
   An instance of this class is created if the creation of an instance
   of *<button-press-event>* is closely followed by the creation of an
   instance of *<button-release-event>*. The necessary time frame is
   dictated by the configuration of your computer. In Windows, for
   example, this time can be set using the Control Panel.

*<double-click-event>*
                      

-  This event occurs when a button is clicked twice within a certain
   time frame. An instance of this class is created if the creation of
   an instance of *<button-click-event>* is closely followed by the
   creation of another instance of *<button-click-event>*. The
   necessary time frame is dictated by the configuration of your
   computer.

DUIM-Sheets Module
==================

This section contains a complete reference of all the interfaces that
are exported from the *duim-sheets* module.

=
~

G.f. method
'''''''''''

Summary
       

Returns true if the specified gestures are the same.

Signature
         

= *gesture1* *gesture2* => *equal?*
                                   

Arguments
         

-  *gesture1* An instance of type `<gesture>`_.
-  *gesture2* An instance of type `<gesture>`_.

Values
      

-  *equal?* An instance of type *<boolean>*.

Description
           

Returns true if *gesture1* and *gesture2* are the same.

See also
        

`gesture-spec-equal`_

add-child
---------

Generic function
''''''''''''''''

Summary
       

Adds a child to the specified sheet.

Signature
         

add-child *sheet* *child* #key *index* => *sheet*
                                                 

Arguments
         

-  *sheet* An instance of type `<sheet>`_.
-  *child* An instance of type `<sheet>`_.
-  *index* An instance of type *false-or(<integer>)*.

Values
      

-  *sheet* An instance of type `<sheet>`_.

Description
           

Adds a child to *sheet*.

See also
        

`remove-child`_

`replace-child`_

add-clipboard-data
------------------

Generic function
''''''''''''''''

Summary
       

Adds data to a clipboard.

Signature
         

add-clipboard-data *clipboard* *data* => *success?*
                                                   

Arguments
         

-  *clipboard* An instance of `<clipboard>`_.
-  *data* An instance of *<object>*.

Values
      

-  *success?* An instance of *<boolean>*.

Description
           

This generic function adds *data* to *clipboard*. It returns *#t* if
*data* was successfully added to the clipboard.

add-clipboard-data-as
---------------------

Generic function
''''''''''''''''

Summary
       

Coerces data to a particular type and then adds it to a clipboard.

Signature
         

add-clipboard-data *type clipboard data* => *success?*
                                                      

Arguments
         

-  *type* An instance of *type-union(<symbol>,* *<type>)*.
-  *clipboard* An instance of `<clipboard>`_.
-  *data* An instance of *<object>*.

Values
      

-  *success?* An instance of *<boolean>*.

Description
           

This generic function adds *data* to *clipboard*, first coercing it to
*type*. The argument *type* is an instance of *type-union(<symbol>,*
*<type>)*. It returns *#t* if *data* was successfully added to the
clipboard.

$alt-key
--------

Constant
''''''''

Summary
       

A constant that represents the ALT key on the keyboard.

Type
    

*<integer>*
           

Value
     

`$meta-key`_

Description
           

A constant that represents the ALT key on the keyboard. This is set to
the same value as the META key, to deal with the case where the META key
is not present on the keyboard.

See also
        

`$control-key`_

`$hyper-key`_

`$meta-key`_

`modifier-key-index`_

`modifier-key-index-name`_

`$modifier-keys`_

`$option-key`_

`$shift-key`_

`$super-key`_

beep
----

Generic function
''''''''''''''''

Summary
       

Signature
         

beep *drawable* => ()
                     

Arguments
         

-  *drawable* An instance of type *type-union(`See
   <sheet>`_, `<medium>`_)*.

Values
      

None

Description
           

boundary-event-kind
-------------------

Generic function
''''''''''''''''

Summary
       

Returns the kind of boundary event for the specified event.

Signature
         

boundary-event-kind *event* => *symbol*
                                       

Arguments
         

-  *event* An instance of type `<event>`_.

Values
      

-  *symbol* An instance of type *one-of(#"ancestor", #"virtual",
   #"inferior", #"nonlinear", #"nonlinear-virtual", #f)*.

Description
           

Returns the kind of boundary event for *event*. These correspond to the
detail members for X11 enter and exit events.

See also
        

`<pointer-boundary-event>`_

button-index
------------

Inline function
'''''''''''''''

Summary
       

Returns the index for the specified pointer button.

Signature
         

button-index *button* => *index*
                                

Arguments
         

-  *button* An instance of type *one-of(#"left", #"middle", #"right")*.

Values
      

-  *index* An instance of type *<integer>*.

Description
           

Returns the index for *button*, a button on the pointer device
connected to the computer (typically a mouse). The *index* returned is
either 0, 1, or 2, for the left, middle, or right buttons, respectively.

See also
        

`button-index-name`_

`$pointer-buttons`_

button-index-name
-----------------

Function
''''''''

Summary
       

Returns the button on the pointer device represented by the specified
index.

Signature
         

button-index-name *index* => *button*
                                     

Arguments
         

-  *index* An instance of type *<integer>*.

Values
      

-  *button* An instance of type *one-of(#"left", #"middle", #"right")*.

Description
           

Returns the button on the pointer device connected to the computer
(typically a mouse) represented by *index*. The *index* is either 0, 1,
or 2, these values corresponding to the left, middle, or right buttons,
respectively.

See also
        

`button-index`_

`$pointer-buttons`_

<button-press-event>
--------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events representing button presses.

Superclasses
            

`<pointer-button-event>`_
                                                

Init-keywords
             

None.

Description
           

The class of events representing button presses. A instance of this
class is generated if a button press is detected, and a second button
press is not detected within the allowed interval for a double-click
event. Alternatively, if a double-click event has just been generated,
then an instance of this class is generated when a subsequent button
press is detected.

Operations
          

-  None.

See also
        

`<button-release-event>`_

`<double-click-event>`_

<button-release-event>
----------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events representing button releases.

Superclasses
            

`<pointer-button-event>`_
                                                

Init-keywords
             

None.

Description
           

The class of events representing button releases. An instance of this
class is generated if the mouse button is released after a period of
being pressed, for example, at the end of a drag and drop maneuver.

Operations
          

-  None.

See also
        

`<button-press-event>`_

<caret>
-------

Abstract instantiable class
'''''''''''''''''''''''''''

Summary
       

The class of carets.

Superclasses
            

*<object>*
          

Init-keywords
             

-  *sheet:* An instance of type *false-or(`See
   <sheet>`_)*.
-  *x:* An instance of type *<integer>*. Default value: 0.
-  *y:* An instance of type *<integer>*. Default value: 0.
-  *width:* An instance of type *<integer>*. Default value: 0.
-  *height:* An instance of type *<integer>*. Default value: 0.

Description
           

The class of carets, or text cursors. A cursor can actually be any
instance of *<symbol>* or any instance of `See
<image> <dcs.htm#51234>`_.

The *sheet:* init-keyword specifies the sheet that the caret is
positioned in.

The *x:*, *y:*, *width:*, and *height:* init-keywords define the
position and size of the caret, with respect to the sheet that contains
it. The position of the caret is measured from the top left of the
sheet. All units are measured in pixels.

Operations
          

The following operations are exported from the *DUIM-Sheets* module.

`caret-position`_ `See
caret-sheet`_ `caret-size`_
`caret-visible?`_ `See
caret-visible?-setter`_ `See
display`_ `port`_ `See
set-caret-position`_

See also
        

`caret-position`_

`caret-sheet`_

`caret-size`_

`caret-visible?`_

`<cursor>`_

caret-position
--------------

Generic function
''''''''''''''''

Summary
       

Returns the position of the specified caret.

Signature
         

cursor-position *caret* => *x y*
                                

Arguments
         

-  *caret* An instance of type `<caret>`_.

Values
      

-  *x* An instance of type *<integer>*.
-  *y* An instance of type *<integer>*.

Description
           

Returns the position of *caret*.

See also
        

`caret-sheet`_

`caret-size`_

caret-sheet
-----------

Generic function
''''''''''''''''

Summary
       

Returns the sheet that owns the specified caret.

Signature
         

cursor-sheet *caret* => *sheet*
                               

Arguments
         

-  *caret* An instance of type `<caret>`_.

Values
      

-  *sheet* An instance of type `<sheet>`_.

Description
           

Returns the sheet that owns *caret*.

See also
        

`caret-position`_

`caret-size`_

caret-size
----------

Generic function
''''''''''''''''

Summary
       

Returns the size of the specified caret.

Signature
         

cursor-size *caret* => *width height*
                                     

Arguments
         

-  *caret* An instance of type `<caret>`_.

Values
      

-  *width* An instance of type *<integer>*.
-  *height* An instance of type *<integer>*.

Description
           

Returns the size of *caret*.

See also
        

`caret-position`_

`caret-sheet`_

caret-visible?
--------------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified caret is visible.

Signature
         

cursor-visible? *caret* => *visible?*
                                     

Arguments
         

-  *caret* An instance of type `<caret>`_.

Values
      

-  *visible?* An instance of type *<boolean>*.

Description
           

Returns true if *caret* is visible.

See also
        

`<cursor>`_

`caret-visible?-setter`_

caret-visible?-setter
---------------------

Generic function
''''''''''''''''

Summary
       

Specifies whether or not the specified caret is visible.

Signature
         

cursor-visible?-setter *visible? caret* => *boolean*
                                                    

Arguments
         

-  *visible?* An instance of type *<boolean>*.
-  *caret* An instance of type `<caret>`_.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Specifies whether or not *caret* is visible.

See also
        

`<cursor>`_

`caret-visible?`_

child-containing-position
-------------------------

Generic function
''''''''''''''''

Summary
       

Returns the topmost child of the specified sheet that occupies a
specified position.

Signature
         

child-containing-position *sheet x y* => *value*
                                                

Arguments
         

-  *sheet* An instance of type `<sheet>`_.
-  *x* An instance of type *<real>*.
-  *y* An instance of type *<real>*.

Values
      

-  *value* An instance of type *false-or(`See
   <sheet>`_)*.

Description
           

Returns the topmost enabled direct child of *sheet* whose region
contains the position *(* *x* *,* *y* *)*. The position is expressed in
the coordinate system used by *sheet*.

See also
        

`children-overlapping-region`_

`do-children-containing-position`_

children-overlapping-region
---------------------------

Generic function
''''''''''''''''

Summary
       

Returns any children of the specified sheet whose regions overlap a
specified region.

Signature
         

children-overlapping-region *sheet region* => *sheets*
                                                      

Arguments
         

-  *sheet* An instance of type `<sheet>`_.
-  *region* An instance of type `<region> <geom.htm#79228>`_.

Values
      

-  *sheets* An instance of type *limited(<sequence>, of: `See
   <sheet>`_)*.

Description
           

Returns the list of enabled direct children of *sheet* whose region
overlaps *region*.

See also
        

`child-containing-position`_

`do-children-overlapping-region`_

choose-color
------------

Generic function
''''''''''''''''

Summary
       

Displays the built-in color dialog for the target platform.

Signature
         

choose-color #key *frame owner title documentation exit-boxes name
default* => *color*
                                                                                      

Arguments
         

-  *frame* An instance of type ` <frames.htm#16922>`_. Default value:
   *#f*.
-  *owner* An instance of type `<sheet>`_.
   Default value: *#f*.
-  *title* An instance of type *<string>*.
-  *documentation* An instance of type *<string>*.
-  *exit-boxes* An instance of type *<object>*.
-  *name* An instance of type *<object>*.
-  *default* An instance of type *<object>*.

Values
      

-  *color* An instance of type `<color> <dcs.htm#55341>`_.

Description
           

Displays the built-in color dialog for the target platform, which allows
the user to choose a color from the standard palette for whatever
environment the application is running in.

The standard Choose Color dialog
                                

.. figure:: images/silica-2.png
   :align: center
   :alt: 

.. figure:: images/silica-3.png
   :align: center
   :alt: 
If the *frame* argument is specified, the top-level sheet of *frame*
becomes the owner of the dialog.

Alternatively, you can specify the owner directly using the *owner*
argument, which takes an instance of *<sheet>* as its value.

By default, both *frame* and *owner* are *#f*, meaning the dialog has
no owner. You should not specify both of these values.

If you wish, you can specify a *title* for the dialog; this is displayed
in the title bar of the frame containing the dialog.

Example
       

The following example illustrates how you can define a class of frame
that contains a button that displays the Choose Color dialog, using the
pre-built dialog classes for your target environment. The frame also
contains an ellipse whose color is set to the color chosen from the
dialog.

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
        

`choose-directory`_

`choose-file`_

`notify-user`_

choose-directory
----------------

Generic function
''''''''''''''''

Summary
       

Displays the built-in directory dialog for the target platform.

Signature
         

choose-directory #key *frame owner title documentation exit-boxes name
default* => *locator*
                                                                                            

Arguments
         

-  *frame* An instance of type ` <frames.htm#16922>`_. Default value:
   *#f*.
-  *owner* An instance of type `<sheet>`_.
   Default value: *#f*.
-  *title* An instance of type *<string>*.
-  *documentation* An instance of type *<string>*.
-  *exit-boxes* An instance of type *<object>*.
-  *name* An instance of type *<object>*.
-  *default* An instance of type *<object>*.

Values
      

-  *locator* An instance of type *type-union(<string>, <locator>)*.

Description
           

Displays the built-in directory dialog for the target platform, which
allows the user to choose a directory from any of the local or networked
drives currently connected to the computer.

If the *frame* argument is specified, the top-level sheet of *frame*
becomes the owner of the dialog.

Alternatively, you can specify the owner directly using the *owner*
argument, which takes an instance of *<sheet>* as its value.

By default, both *frame* and *owner* are *#f*, meaning the dialog has
no owner. You should not specify both of these values.

If you wish, you can specify a *title* for the dialog; this is displayed
in the title bar of the frame containing the dialog.

Example
       

The following example illustrates how you can define a class of frame
that contains a button that displays the Choose Directory dialog, using
the pre-built dialog classes for your target environment.

define frame <directory-dialog-frame> (<simple-frame>)
                                                      

pane dir-file-button (frame)

make(<menu-button>,

label: "Choose directory ...",

documentation:

"Example of standard 'Choose Dir' dialog",

activate-callback:

method (button)

let dir = choose-directory (owner: frame);

if (dir) frame-status-message(frame)

:= format-to-string

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
        

`choose-color`_

`choose-file`_

`notify-user`_

choose-file
-----------

Generic function
''''''''''''''''

Summary
       

Displays the built-in file dialog for the target platform.

Signature
         

choose-file #key *frame owner title documentation exit-boxes name
default* => *locator*
                                                                                       

Arguments
         

-  *frame* An instance of type ` <frames.htm#16922>`_. Default value:
   *#f*.
-  *owner* An instance of type `<sheet>`_.
   Default value: *#f*.
-  *title* An instance of type *<string>*.
-  *documentation* An instance of type *<string>*.
-  *direction* An instance of type *one-of(#"input", #"output")*.
   Default value: *#"input"*.
-  *filters* An instance of type *limited(<sequence>, of: <sequence>)*.
-  *exit-boxes* An instance of type *<object>*.
-  *name* An instance of type *<object>*.
-  *default* An instance of type *<string>*.

Values
      

-  *locator* An instance of type *<string>*.

Description
           

Displays the built-in file dialog for the target platform, which allows
the user to choose a file from any of the local or networked drives
currently connected to the computer. The function returns the name of
the file chosen by the user.

Typical appearance of a choose-file dialog
                                          

.. figure:: images/silica-2.png
   :align: center
   :alt: 

.. figure:: images/silica-4.png
   :align: center
   :alt: 
If the *frame* argument is specified, the top-level sheet of *frame*
becomes the owner of the dialog.

Alternatively, you can specify the owner directly using the *owner*
argument, which takes an instance of *<sheet>* as its value.

By default, both *frame* and *owner* are *#f*, meaning the dialog has
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

-  The first string in the sequence is a description of the files that
   are displayed when this filter is chosen.
-  Each subsequent string is a regular expression that describes which
   files to display in the dialog.

For example, to specify a filter that lets the user choose to display
either text files, HTML files, or Dylan source files, the following
sequence should be passed to the filters argument:

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

if (file) frame-status-message(frame)

:= format-to-string

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

if (file) frame-status-message(frame)

:= format-to-string

("Saved file as %s", file);

end

end);

end frame <open-save-dialog-frame>;
                                   

See also
        

`choose-color`_

`choose-directory`_

`notify-user`_

choose-from-dialog
------------------

Generic function
''''''''''''''''

Summary
       

Prompt the user to choose from a collection of items, using a dialog
box.

Signature
         

choose-from-dialog *items*
 #key *frame owner title value default-item label-key value-key
* *selection-mode gadget-class gadget-options width height
* *foreground background text-style
* => *value success?*
                                                               

Arguments
         

-  *items* An instance of *type-union(<sequence>,*
   ` <gadgets.htm#81833>`_*)*.
-  *frame* An instance of type ` <frames.htm#16922>`_. Default value:
   *#f*.
-  *owner* An instance of type `<sheet>`_.
   Default value: *#f*.
-  *title* An instance of type *<string>*.
-  *default-item* An instance of type *<object>*.
-  *label-key* An instance of type *<function>*. Default value:
   *identity*.
-  *value-key* An instance of type *<function>*. Default value:
   *identity*.
-  *selection-mode* An instance of *<symbol>*. Default value:
   *#"single"*.
-  *gadget-class* An instance of type ` <gadgets.htm#34543>`_.
-  *gadget-options* An instance of type *<sequence>*.
-  *foreground* An instance of type `<ink> <dcs.htm#15007>`_.
-  *background* An instance of type `<ink> <dcs.htm#15007>`_.
-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.

Values
      

-  *value* An instance of type *<object>*.
-  *success?* An instance of type *<boolean>*.

Description
           

Prompt the user to choose from a collection of *items*, using a dialog
box. This generic function is similar to *choose-from-menu*.

The function returns the values chosen by the user, and a boolean value:
*#t* if a value was chosen, *#f* if nothing was chosen. Unlike
*choose-from-menu*, the user can choose several values if desired,
depending on the value of *selection-mode*, described below.

At its most basic, *choose-from-dialog* can be passed a simple sequence
of items, as follows:

choose-from-dialog(range(from: 1, to: 10));
                                           

However, any of a large number of keywords can be supplied to specify
more clearly the dialog that is created. A range of typical options can
be chosen: The *frame* keyword specifies a frame whose top level sheet
becomes the owner of the menu. Alternatively, you can specify this top
level sheet explicitly using *owner*. The *title* keyword lets you
choose a title for the dialog. By default, each of these values is *#f*
.

In addition, *choose-from-dialog* offers options similar to collection
gadgets, that can act upon the items specified. The *default-item*
keyword lets you specify an item that is returned by default if no value
is chosen explicitly (thereby ensuring that *success?* will always be
*#t*). You can also specify a *value-key* or *label-key* for the items
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
        

`choose-from-menu`_

choose-from-menu
----------------

Generic function
''''''''''''''''

Summary
       

Prompt the user to choose from a collection of items, using a pop-up
menu.

Signature
         

choose-from-menu *items*
 #key *frame owner title value default-item label-key value-key
* *width height foreground background text-style multiple-sets?*
 => *value success?*
                                                                

Arguments
         

-  *items* An instance of *type-union(<sequence>,*
   ` <gadgets.htm#81833>`_*)*.
-  *frame* An instance of type ` <frames.htm#16922>`_. Default value:
   *#f*.
-  *owner* An instance of type `<sheet>`_.
   Default value: *#f*.
-  *title* An instance of type *<string>*. Default value: *#f*.
-  *default-item* An instance of type *<object>*.
-  *label-key* An instance of type *<function>*. Default value:
   *identity*.
-  *value-key* An instance of type *<function>*. Default value:
   *identity*.
-  *foreground* An instance of type `<ink> <dcs.htm#15007>`_.
-  *background* An instance of type `<ink> <dcs.htm#15007>`_.
-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.

Values
      

-  *value* An instance of type *<object>*.
-  *success?* An instance of type *<boolean>*.

Description
           

Prompt the user to choose from a collection of *items*, using a pop-up
menu.This generic function is similar to *choose-from-dialog*.

The function returns the value chosen by the user, and a boolean value:
*#t* if a value was chosen, *#f* if nothing was chosen.

At its most basic, *choose-from-menu* can be passed a simple sequence of
items, as follows:

choose-from-menu(#(1, 2, 3));
                             

However, any of a large number of keywords can be supplied to specify
more clearly the menu that is created. A range of typical options can be
chosen: The *frame* keyword specifies a frame whose top level sheet
becomes the owner of the menu. Alternatively, you can specify this top
level sheet explicitly using *owner*. The *title* keyword lets you
choose a title for the dialog. By default, each of these values is *#f*
.

In addition, *choose-from-menu* offers options similar to collection
gadgets, that can act upon the items specified. The *default-item*
keyword lets you specify an item that is returned by default if no value
is chosen explicitly (thereby ensuring that *success?* will always be
*#t*). You can also specify a *value-key* or *label-key* for the items
in the menu.

Finally, you can configure the appearance of the menu itself. The
*width* and *height* keywords let you set the size of the menu. The
*foreground* and *background* keywords let you set the text color and
the menu color respectively. The *text-style* keyword lets you specify a
font to display the menu items.

See also
        

`choose-from-dialog`_

choose-text-style
-----------------

Generic function
''''''''''''''''

Summary
       

Displays the built-in font dialog for the target platform, thereby
letting the user choose a font.

Signature
         

choose-text-style #key *frame* *owner* *title* => *font*
                                                        

Arguments
         

-  *frame* An instance of type ` <frames.htm#16922>`_. Default value:
   *#f*.
-  *owner* An instance of type `<sheet>`_.
   Default value: *#f*.
-  *title* An instance of type *<string>*. Default value: *#f*.

Values
      

-  *font* An instance of *<text-style>*.

Description
           

Displays the built-in font dialog for the target platform, thereby
letting the user choose a font.

The *frame* keyword specifies a frame whose top-level sheet becomes the
owner of the menu. Alternatively, you can specify this top level sheet
explicitly using *owner*. The *title* keyword lets you choose a title
for the dialog. By default, each of these values is *#f*.

If you wish, you can specify a *title* for the dialog; this is an
instance of *<string>* and is displayed in the title bar of the frame
containing the dialog. If you do not specify *title*, then DUIM uses
the default title for that type of dialog on the target platform.

clear-box
---------

Generic function
''''''''''''''''

Summary
       

Clears a box-shaped area in the specified drawable.

Signature
         

clear-box *drawable left top right bottom* => ()
                                                

clear-box\* *drawable region* => ()
                                   

Arguments
         

-  *drawable* An instance of type *type-union(* `See
   <sheet>`_*, `<medium>`_)*.

The following arguments are specific to *clear-box*.

-  *left* An instance of type *<coordinate>*.
-  *top* An instance of type *<coordinate>*.
-  *right* An instance of type *<coordinate>*.
-  *bottom* An instance of type *<coordinate>*.

The following argument is specific to *clear-box\**.

-  *region* An instance of type `<region> <geom.htm#79228>`_.

Values
      

None

Description
           

Clears a box-shaped area in the specified drawable, removing anything
that was drawn in that region.

The function *clear-box\** is identical to *clear-box*, except that it
passes composite objects, rather than separate coordinates, in its
arguments. You should be aware that using this function may lead to a
loss of performance.

clear-clipboard
---------------

Generic function
''''''''''''''''

Summary
       

Clears the contents of a clipboard.

Signature
         

clear-clipboard *clipboard* => ()
                                 

Arguments
         

-  *clipboard* An instance of *<clipboard>*.

Values
      

-  None.

Description
           

Clears the contents of *clipboard*, which represents the locked
clipboard.

<clipboard>
-----------

Open abstract class
'''''''''''''''''''

Summary
       

The class of clipboard objects.

Init-keywords
             

None.

Description
           

The class of clipboard objects. An instance of this class is created
when a clipboard lock is created, and is used to hold the contents of
the Windows clipboard for the duration of the lock. You do not need to
worry about creating instances of *<clipboard>* yourself, since this is
handled automatically by the macro *with-clipboard*.

See also
        

`add-clipboard-data`_

`add-clipboard-data-as`_

`clear-clipboard`_

`clipboard-data-available?`_

`clipboard-sheet`_

`clipboard-owner`_

`close-clipboard`_

`get-clipboard-data-as`_

`open-clipboard`_

`with-clipboard`_

clipboard-data-available?
-------------------------

Generic function
''''''''''''''''

Summary
       

Returns false if there is any data of a particular type on a clipboard.

Signature
         

clipboard-data-available? *type clipboard* => *available?*
                                                          

Arguments
         

-  *type* An instance of *type-union(<symbol>,* *<type>)*.
-  *clipboard* An instance of *<clipboard>*.

Values
      

-  *available?* An instance of *<boolean>*.

Description
           

Returns *#f* if and only if there is any data of type *type* on the
clipboard. The argument *type* is an instance of *type-union(<symbol>,*
*<type>)*.

See also
        

`add-clipboard-data`_

`add-clipboard-data-as`_

`<clipboard>`_

`get-clipboard-data-as`_

clipboard-sheet
---------------

Generic function
''''''''''''''''

Summary
       

Returns the sheet with the clipboard lock.

Signature
         

clipboard-sheet *clipboard* => *sheet*
                                      

-  *clipboard* An instance of *<clipboard>*.

Values
      

-  *sheet* An instance of *<sheet>*.

Description
           

Returns the sheet with the clipboard lock.

See also
        

`<clipboard>`_

clipboard-owner
---------------

Generic function
''''''''''''''''

Summary
       

Returns the sheet that owns the current clipboard data.

Signature
         

clipboard-owner *clipboard* => *owner*
                                      

Arguments
         

-  *clipboard* An instance of *<clipboard>*.

Values
      

-  *owner* An instance of *<sheet>*.

Description
           

Returns the sheet that owns the current clipboard data.

See also
        

`<clipboard>`_

close-clipboard
---------------

Function
''''''''

Summary
       

Closes the current clipboard lock for a sheet on a port.

Signature
         

close-clipboard *port sheet* => ()
                                  

Arguments
         

-  *port* An instance of *<port>*.
-  *sheet* An instance of *<sheet>*.

Values
      

-  None.

Description
           

Closes the current clipboard lock for *sheet* on *port*. A clipboard
lock needs to be closed safely after it the clipboard has been used, to
free the clipboard for further use.

You should not normally call *close-clipboard* yourself to close a
clipboard lock. Use the macro *with-clipboard* to create and free the
lock for you.

See also
        

`<clipboard>`_

`with-clipboard`_

$control-key
------------

Constant
''''''''

Summary
       

A constant that represents the CONTROL key on the keyboard.

Type
    

*<integer>*
           

Value
     

ash(1, %modifier\_base + 1);
                            

Description
           

A constant that represents the CONTROL key on the keyboard.

See also
        

`$alt-key`_

`$hyper-key`_

`$meta-key`_

`modifier-key-index`_

`modifier-key-index-name`_

`$modifier-keys`_

`$option-key`_

`$shift-key`_

`$super-key`_

<cursor>
--------

Type
''''

Summary
       

The class of cursor objects.

Equivalent
          

*type-union(<symbol>,* `<image> <dcs.htm#51234>`_*)*
                                                        

Init-keywords
             

None.

Description
           

The class of cursor objects. The cursor is the small image that is used
to display the location of the mouse pointer at any time. A cursor can
actually be any instance of *<symbol>* or any instance of `See
<image> <dcs.htm#51234>`_.

Operations
          

`pointer-cursor-setter`_ `See
set-caret-position`_ `See
sheet-pointer-cursor-setter`_

See also
        

`<caret>`_

`cursor?`_

cursor?
-------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified object is a cursor.

Signature
         

cursor? *object* => *cursor?*
                             

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *cursor?* An instance of type *<boolean>*.

Description
           

Returns true if *object* is a cursor. In practice, you can create a
cursor from any instance of *<symbol>* or `See
<image> <dcs.htm#51234>`_.

See also
        

`<cursor>`_

default-port
------------

Function
''''''''

Summary
       

Returns the default port for the specified server.

Signature
         

default-port #key *server-path* => *port*
                                         

Arguments
         

-  *server-path* An instance of type *<vector>*. Default value:
   *#(#"local")*.

Values
      

-  *port* An instance of type *false-or(`See
   <port>`_)*.

Description
           

Returns the default port for server specified by *server-path*.

See also
        

`default-port-setter`_

`destroy-port`_

default-port-setter
-------------------

Function
''''''''

Summary
       

Sets the default port.

Signature
         

default-port-setter *port* => *port*
                                    

Arguments
         

-  *port* An instance of type `<port>`_. Default
   value: *#f*.

Values
      

-  *port* An instance of type `<port>`_.

Description
           

Sets the default port.

See also
        

`default-port`_

`destroy-port`_

destroy-port
------------

Generic function
''''''''''''''''

Summary
       

Destroys the specified port.

Signature
         

destroy-port *port* => ()
                         

Arguments
         

-  *port* An instance of type `<port>`_.

Values
      

None

Description
           

Destroys *port*.

See also
        

`default-port`_

`default-port-setter`_

destroy-sheet
-------------

Generic function
''''''''''''''''

Summary
       

Destroys the specified sheet.

Signature
         

destroy-sheet *sheet* => ()
                           

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

None

Description
           

Destroys *sheet*.

<device-event>
--------------

Open abstract class
'''''''''''''''''''

Summary
       

The class of device events.

Superclasses
            

`<sheet-event>`_
                                       

Init-keywords
             

-  *sheet:* An instance of type `<sheet>`_.
-  *modifier-state:* An instance of type *<integer>*. Default value: 0.

Description
           

The class of device events.

The *modifier-state:* init-keyword is used to record the state of the
device at the time the event occurred.

Operations
          

The following operation is exported from the *DUIM-Sheets* module.

-  `event-modifier-state`_

<display>
---------

Open abstract class
'''''''''''''''''''

Summary
       

The class of displays.

Superclasses
            

`<sheet>`_
                                 

Init-keywords
             

-  *orientation:* An instance of type *one-of(#"vertical",
   #"horizontal", #"default")*. Default value: *#"default"*.
-  *units:* An instance of type *one-of(#"device", #"mm", #"pixels")*.
   Default value: *#"device"*.

Description
           

The class of displays. An instance of *<display>* is an object that
represents a single display (or screen) on some display server. Any
sheet can be attached to an instance of *<display>*, and a display, and
all the sheets attached to it, are associated with a `See
<port>`_ that is a connection to a display server.

The *orientation:* init-keyword is used to specify the orientation of a
display.

The *units:* init-keyword is used to specify the units in which height
and width measurements are made with respect to the display. The default
is whatever units are standard for the display device (usually pixels).

Operations
          

The following operations are exported from the *DUIM-Sheets* module.

`display`_ `display?`_
`display-depth`_ `See
display-height`_ `See
display-mm-height`_ `See
display-mm-width`_ `See
display-orientation`_
 `display-pixel-height`_ `See
display-pixels-per-point`_ `See
display-pixel-width`_ `See
display-units`_ `See
display-width`_

See also
        

`display`_

`display?`_

`display-depth`_

`display-height`_

`display-orientation`_

`display-units`_

`display-width`_

`<port>`_

`<sheet>`_

display
-------

Generic function
''''''''''''''''

Summary
       

Returns the display for the specified object.

Signature
         

display *object* => *display*
                             

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *display* An instance of type *false-or(`See
   <display>`_)*.

Description
           

Returns the display used to display *object*.

See also
        

`<display>`_

`frame-manager`_

`port`_

display?
--------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified object is a display.

Signature
         

display? *object* => *display?*
                               

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *display?* An instance of type *<boolean>*.

Description
           

Returns true if *object* is a display.

See also
        

`<display>`_

display-depth
-------------

Generic function
''''''''''''''''

Summary
       

Returns the color depth of the specified display.

Signature
         

display-depth *display* => *depth*
                                  

Arguments
         

-  *display* An instance of type *`<display>`_*.

Values
      

-  *depth* An instance of type *<integer>*.

Description
           

Returns the color depth of *display*. By default, the color depth of
any display is assumed to be 8.

See also
        

`display-height`_

`display-orientation`_

`display-width`_

display-height
--------------

Generic function
''''''''''''''''

Summary
       

Returns the height of the specified display.

Signature
         

display-height *display* #key *units* => *height*
                                                 

Arguments
         

-  *display* An instance of type *`<display>`_*.
-  *units* An instance of *one-of(#"device", #"mm", #"pixels")*.
   Default value: *#"device"*.

Values
      

-  *height* An instance of type *<number>*.

Description
           

Returns the height of *display*, in device-independent units. If
*units* is specified, then the value returned is converted into the
appropriate type of units.

See also
        

`display-depth`_

`display-mm-height`_

`display-orientation`_

`display-pixel-height`_

`display-units`_

`display-width`_

display-mm-height
-----------------

Generic function
''''''''''''''''

Summary
       

Returns the height of the specified display in millimeters.

Signature
         

display-mm-height *display* => *height*
                                       

Arguments
         

-  *display* An instance of type *`<display>`_*.

Values
      

-  *height* An instance of type *<number>*.

Description
           

Returns the height of *display* in millimeters. This is equivalent to
calling `display-height`_ with the *units*
argument set to *#"mm"*.

See also
        

`display-height`_

`display-mm-width`_

`display-pixel-height`_

`display-units`_

display-mm-width
----------------

Generic function
''''''''''''''''

Summary
       

Returns the width of the specified display in millimeters.

Signature
         

display-mm-width *display* => *width*
                                     

Arguments
         

-  *display* An instance of type *`<display>`_*.

Values
      

-  *width* An instance of type *<number>*.

Description
           

Returns the width of *display* in millimeters. This is equivalent to
calling `display-width`_ with the *units*
argument set to *#"mm"*.

See also
        

`display-mm-height`_

`display-pixel-width`_

`display-units`_

`display-width`_

display-orientation
-------------------

Generic function
''''''''''''''''

Summary
       

Returns the orientation of the specified display.

Signature
         

display-orientation *display* => *orientation*
                                              

Arguments
         

-  *display* An instance of type *`<display>`_*.

Values
      

-  *orientation* An instance of type *one-of(#"vertical", #"horizontal",
   #"default")*.

Description
           

Returns the orientation of *display*. Unless specified otherwise, the
orientation of any display is *#"default"*.

See also
        

`display-depth`_

`display-height`_

`display-width`_

display-pixel-height
--------------------

Generic function
''''''''''''''''

Summary
       

Returns the height of the specified display in pixels.

Signature
         

display-pixel-height *display* => *height*
                                          

Arguments
         

-  *display* An instance of type *`<display>`_*.

Values
      

-  *height* An instance of type *<integer>*.

Description
           

Returns the height of *display* in pixels. This is equivalent to calling
`display-height`_ with the *units* argument set
to *#"pixels"*.

See also
        

`display-height`_

`display-mm-height`_

`display-pixel-width`_

`display-units`_

display-pixels-per-point
------------------------

Generic function
''''''''''''''''

Summary
       

Returns the number of pixels per point for the specified display.

Signature
         

display-pixels-per-point *display* => *number*
                                              

Arguments
         

-  *display* An instance of type *`<display>`_*.

Values
      

-  *number* An instance of type *<number>*.

Description
           

Returns the number of pixels per point for *display*.

See also
        

`display-pixel-height`_

`display-pixel-width`_

`display-units`_

display-pixel-width
-------------------

Generic function
''''''''''''''''

Summary
       

Returns the width of the specified display in pixels.

Signature
         

display-pixel-width *display* => *width*
                                        

Arguments
         

-  *display* An instance of type *`<display>`_*.

Values
      

-  *width* An instance of type *<integer>*.

Description
           

Returns the height of *display* in pixels. This is equivalent to calling
`display-width`_ with the *units* argument set to
*#"pixels"*.

See also
        

`display-mm-width`_

`display-pixel-height`_

`display-units`_

`display-width`_

display-units
-------------

Generic function
''''''''''''''''

Summary
       

Returns the default units for the specified display.

Signature
         

display-units *display* => *value*
                                  

Arguments
         

-  *display* An instance of type *`<display>`_*.

Values
      

-  *value* An instance of type *one-of(#"device", #"pixels", #"mm")*.

Description
           

Returns the default units for *display*. These are the units in which
height and width measurements are made, both for the display, and for
any children of the display. Unless otherwise specified, the value
returned is *#"default"*, so as to maintain a device-independent
measurement as far as possible.

See also
        

`display-height`_

`display-width`_

display-width
-------------

Generic function
''''''''''''''''

Summary
       

Returns the width of the specified display.

Signature
         

display-width *display* #key *units* => *width*
                                               

Arguments
         

-  *display* An instance of type *`<display>`_*.
-  *units* An instance of *one-of(#"device", #"mm", #"pixels")*.
   Default value: *#"device"*.

Values
      

-  *width* An instance of type *<number>*.

Description
           

Returns the width of *display*, in device-independent units. If *units*
is specified, then the value returned is converted into the appropriate
type of units.

See also
        

`display-depth`_

`display-height`_

`display-mm-width`_

`display-orientation`_

`display-pixel-width`_

`display-units`_

do-children-containing-position
-------------------------------

Generic function
''''''''''''''''

Summary
       

Invokes a function on any children that occupy a specified position in
the specified sheet.

Signature
         

do-children-containing-position *function sheet x y* => ()
                                                          

Arguments
         

-  *function* An instance of type *<function>*.
-  *sheet* An instance of type `<sheet>`_.
-  *x* An instance of type *<real>*.
-  *y* An instance of type *<real>*.

Values
      

None

Description
           

Invokes *function* on any children that occupy position *(* *x* *,* *y*
*)* in *sheet*. This is used by `See
child-containing-position`_ to ascertain which
children occupy the position. The function `See
child-containing-position`_ then decides which of the
children returned is the topmost direct enabled child.

See also
        

`child-containing-position`_

do-children-overlapping-region
------------------------------

Generic function
''''''''''''''''

Summary
       

Invokes a function on any children of the specified sheet whose regions
overlap a specified region.

Signature
         

do-children-overlapping-region *function sheet region* => ()
                                                            

Arguments
         

-  *function* An instance of type *<function>*.
-  *sheet* An instance of type `<sheet>`_.
-  *region* An instance of type `<region> <geom.htm#79228>`_.

Values
      

None

Description
           

Invokes *function* on any children of *sheet* whose regions overlap
*region*. This is used by `See
children-overlapping-region`_ to ascertain which
children overlap *region*.

See also
        

`children-overlapping-region`_

`do-children-containing-position`_

do-displays
-----------

Inline function
'''''''''''''''

Summary
       

Runs a function on all the displays attached to a given port.

Signature
         

do-displays *function port* => ()
                                 

Arguments
         

-  *function* An instance of type *<function>*.
-  *port* An instance of type `<port>`_.

Values
      

None

Description
           

Runs a function on all the displays attached to a given port. By
default, the current port is used, unless *port* is specified.

do-frames
---------

Generic function
''''''''''''''''

Summary
       

Runs a function on all the frames managed by a given frame manager.

Signature
         

do-frames *function* #key *port frame-manager* => ()
                                                    

Arguments
         

-  *function* An instance of type *<function>*.
-  *port* An instance of type `<port>`_.
-  *frame-manager* An instance of type `See
   <frame-manager>`_.

Values
      

None

Description
           

Runs a function on all the frames managed by a given frame manager. By
default, the current frame manager on the current port is used, unless
*port* or *frame-manager* are specified.

do-ports
--------

Function
''''''''

Summary
       

Runs a function on all the current ports.

Signature
         

do-ports *function* => ()
                         

Arguments
         

-  *function* An instance of type *<function>*.

Values
      

None

Description
           

Runs a function on all the current ports.

do-sheet-children
-----------------

Generic function
''''''''''''''''

Summary
       

Runs a function on all the immediate children of the specified sheet.

Signature
         

do-sheet-children *function sheet* => ()
                                        

Arguments
         

-  *function* An instance of type *<function>*.
-  *sheet* An instance of type `<sheet>`_.

Values
      

None

Description
           

Runs *function* on all the immediate children of *sheet*. This function
calls `sheet-children`_ to find the children of
*sheet*.

See also
        

`sheet-children`_

do-sheet-tree
-------------

Generic function
''''''''''''''''

Summary
       

Runs a function on all the children in the hierarchy of the specified
sheet.

Signature
         

do-sheet-tree *function sheet* => ()
                                    

Arguments
         

-  *function* An instance of type *<function>*.
-  *sheet* An instance of type `<sheet>`_.

Values
      

None

Description
           

Runs a function on all the children in the hierarchy of the specified
sheet. The function is run on *sheet*, then on the children of *sheet*
, then on the children of the children of *sheet*, and so on.

<double-click-event>
--------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of double-click events on the pointer device.

Superclasses
            

`<button-press-event>`_
                                              

Init-keywords
             

None.

Description
           

The class of double-click events on the pointer device. An instance of
this class is generated when a button press is detected within a certain
(small) amount of time after a previous button press. If a double click
event is generated, the clock is reset, so that the next press generated
is an instance of `<button-press-event>`_.

Operations
          

-  None.

See also
        

`<button-press-event>`_

do-with-drawing-options
-----------------------

Generic function
''''''''''''''''

Summary
       

Runs some code on a drawable in a given drawing context.

Signature
         

do-with-drawing-options *drawable function* #key *brush pen text-style
clipping-region transform* => #rest *values*
                                                                                                                   

Arguments
         

-  *drawable* An instance of type *type-union(* `See
   <sheet>`_*, `<medium>`_)*.
-  *function* An instance of type *<function>*.
-  *brush* An instance of type `<brush> <dcs.htm#29492>`_.
-  *pen* An instance of type `<pen> <dcs.htm#41757>`_.
-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.
-  *clipping-region* An instance of type `See
   <region> <geom.htm#79228>`_.
-  *transform* An instance of type `<transform> <geom.htm#54995>`_.

Values
      

-  *values* An instance of type *<object>*.

Description
           

Runs some code on a drawable in a given drawing context. This function
is called by the macro `with-drawing-options`_,
and you should define new methods on it for new classes of drawable.

The *function* passed to *do-with-drawing-options* is the result of
encapsulating the body passed to `See
with-drawing-options`_ as a stand-alone method.

The values returned are the values that are returned from `See
with-drawing-options`_.

The various keywords specify a drawing context in which function is run.

See also
        

`with-drawing-options`_

do-with-pointer-grabbed
-----------------------

Generic function
''''''''''''''''

Summary
       

Runs some specified code, forwarding all pointer events to a sheet.

Signature
         

do-with-pointer-grabbed *port sheet continuation* #key => #rest *values*
                                                                        

Arguments
         

-  *port* An instance of type `<port>`_.
-  *sheet* An instance of type `<sheet>`_.
-  *continuation* An instance of type *<function>*.

Values
      

-  *values* An instance of type *<object>*.

Description
           

Runs the code specified in *continuation*, forwarding all pointer
events to *sheet*, even if the pointer leaves the sheet-region of
*sheet*. The argument continuation is an instance of *<function>*.

This function is called by `See
with-pointer-grabbed`_, and *continuation* is
actually the result of creating a stand-alone method from the body of
code passed to *with-pointer-grabbed*.

See also
        

`with-pointer-grabbed`_

do-with-sheet-medium
--------------------

Generic function
''''''''''''''''

Summary
       

Runs a continuation function on a sheet.

Signature
         

do-with-sheet-medium *sheet continuation* => #rest *values*
                                                           

Arguments
         

-  *sheet* An instance of type `<sheet>`_.
-  *continuation* An instance of type *<function>*.

Values
      

-  *values* An instance of type *<object>*.

Description
           

Runs a continuation function on a sheet.

See also
        

`with-sheet-medium`_

do-with-text-style
------------------

Generic function
''''''''''''''''

Summary
       

Runs some code on a drawable in the context of a given text style.

Signature
         

do-with-text-style *drawable function text-style* => ()
                                                       

Arguments
         

-  *drawable* An instance of type *type-union(* `See
   <sheet>`_*, `<medium>`_)*.
-  *function* An instance of type *<function>*.
-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.

Values
      

None

Description
           

Runs some code on a drawable in the context of a given text style.

See also
        

`with-text-style`_

do-with-transform
-----------------

Generic function
''''''''''''''''

Summary
       

Returns the result of running a function in a transform defined on a
specified medium.

Signature
         

do-with-transform *drawable function transform* => #rest *values*
                                                                 

Arguments
         

-  *drawable* An instance of type *type-union(* `See
   <sheet>`_*, `<medium>`_)*.
-  *function* An instance of type *<function>*.
-  *transform* An instance of type `<transform> <geom.htm#54995>`_.

Values
      

-  *values* An instance of type *<object>*.

Description
           

Returns the result of running a function in a transform defined on a
specified medium. Methods on this function are called by `See
with-transform`_, which in turn is used by the
similar macros `with-rotation`_, `See
with-scaling`_, and
 `with-translation`_.

See also
        

`with-transform`_

<event>
-------

Open abstract class
'''''''''''''''''''

Summary
       

The base class of all DUIM events.

Superclasses
            

*<object>*
          

Init-keywords
             

-  *timestamp:* An instance of type *<integer>*. Default value:
   *next-event-timestamp()*.

Description
           

The base class of all DUIM events.

The *timestamp:* init-keyword is used to give a unique identifier for
the event.

Operations
          

The following operations are exported from the *DUIM-Sheets* module.

`event?`_ `See
event-matches-gesture?`_ `See
handle-event`_ `queue-event`_

See also
        

`<frame-event>`_

`<sheet-event>`_

event?
------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified object is an event.

Signature
         

event? *object* => *event?*
                           

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *event?* An instance of type *<boolean>*.

Description
           

Returns true if *object* is an instance of `See
<event>`_ or one of its subclasses.

See also
        

`<event>`_

event-button
------------

Generic function
''''''''''''''''

Summary
       

Returns an integer corresponding to the mouse button that was pressed or
released.

Signature
         

event-button *event* => *integer*
                                 

Arguments
         

-  *event* An instance of type `<event>`_.

Values
      

-  *integer* An instance of type *<integer>*.

Description
           

Returns an integer corresponding to the mouse button that was pressed or
released, which will be one of `$left-button`_,
`$middle-button`_, or `See
$right-button`_.

*Note:* The function *event-button* records the button state at the time
that the event occurred, and hence can be different from `See
pointer-button-state`_.

See also
        

`$left-button`_

`$middle-button`_

`<pointer-button-event>`_

`pointer-button-state`_

`$right-button`_

event-character
---------------

Generic function
''''''''''''''''

Summary
       

Returns the character that was pressed on the keyboard.

Signature
         

event-character *event* => *value*
                                  

Arguments
         

-  *event* An instance of type `<event>`_.

Values
      

-  *value* An instance of type *false-or(<character>)*.

Description
           

Returns the character associated with the keyboard event, if there is
any.

See also
        

`event-key-name`_

`<keyboard-event>`_

event-key-name
--------------

Generic function
''''''''''''''''

Summary
       

Returns the name of the key that was pressed or released on the
keyboard.

Signature
         

event-key-name *event* => name
                              

Arguments
         

-  *event* An instance of type `<event>`_.

Values
      

-  *name* An instance of type *<symbol>*.

Description
           

Returns the name of the key that was pressed or released in a keyboard
event. This will be a symbol whose value is specific to the current
port.

See also
        

`event-character`_

`<keyboard-event>`_

event-matches-gesture?
----------------------

Generic function
''''''''''''''''

Summary
       

Returns true if an event matches a defined gesture.

Signature
         

event-matches-gesture? *event gesture-name* => *matches?*
                                                         

Arguments
         

-  *event* An instance of type `<event>`_.
-  *gesture-name* An instance of type *type-union(`See
   <gesture>`_, <character>)*.

Values
      

-  *matches?* An instance of type *<boolean>*.

Description
           

Returns true if an event matches a defined gesture.

event-modifier-state
--------------------

Generic function
''''''''''''''''

Summary
       

Returns an integer value that encodes the state of all the modifier keys
on the keyboard.

Signature
         

event-modifier-state *event* => *integer*
                                         

Arguments
         

-  *event* An instance of type `<event>`_.

Values
      

-  *integer* An instance of type *<integer>*.

Description
           

Returns an integer value that encodes the state of all the modifier keys
on the keyboard. This is a mask consisting of the *logior* of `See
$shift-key`_, `$control-key`_,
`$meta-key`_, `See
$super-key`_, and `See
$hyper-key`_.

See also
        

`event-sheet`_

`gesture-modifier-state`_

`make-modifier-state`_

`port-modifier-state`_

event-pointer
-------------

Generic function
''''''''''''''''

Summary
       

Returns the pointer object to which the specified event refers.

Signature
         

event-pointer *event* => *pointer*
                                  

Arguments
         

-  *event* An instance of type `<event>`_.

Values
      

-  *pointer* An instance of type `<pointer>`_.

Description
           

Returns the pointer object to which *event* refers.

See also
        

`<pointer>`_

`event-x`_

`event-y`_

event-region
------------

Generic function
''''''''''''''''

Summary
       

Returns the region in the sheet that is affected by the specified event.

Signature
         

event-region *event* => *region*
                                

Arguments
         

-  *event* An instance of type `<event>`_.

Values
      

-  *region* An instance of type `<region> <geom.htm#79228>`_.

Description
           

Returns the region of the sheet that is affected by *event*.

See also
        

`event-x`_

`event-y`_

`<window-event>`_

event-sheet
-----------

Generic function
''''''''''''''''

Summary
       

Returns the sheet associated with the specified event.

Signature
         

event-sheet *event* => *sheet*
                              

Arguments
         

-  *event* An instance of type `<event>`_.

Values
      

-  *sheet* An instance of type `<sheet>`_.

Description
           

Returns the sheet associated with *event*.

See also
        

`event-modifier-state`_

event-x
-------

Generic function
''''''''''''''''

Summary
       

Returns the x position of the pointer at the time the event occurred.

Signature
         

event-x *event* => *x*
                      

Arguments
         

-  *event* An instance of type `<event>`_.

Values
      

-  *x* An instance of type *<integer>*.

Description
           

Returns the x position of the pointer at the time the event occurred, in
the coordinate system of the sheet that received the event.

See also
        

`event-pointer`_

`event-region`_

`event-y`_

event-y
-------

Generic function
''''''''''''''''

Summary
       

Returns the y position of the pointer at the time the event occurred.

Signature
         

event-y *event* => *y*
                      

Arguments
         

-  *event* An instance of type `<event>`_.

Values
      

-  *y* An instance of type *<integer>*.

Description
           

Returns the y position of the pointer at the time the event occurred, in
the coordinate system of the sheet that received the event.

See also
        

`event-pointer`_

`event-region`_

`event-x`_

find-display
------------

Function
''''''''

Summary
       

Returns a suitable display for the specified port and server-path
criteria.

Signature
         

find-display #key *server-path port orientation units* => *display*
                                                                   

Arguments
         

-  *server-path* An instance of type *<symbol>*. Default value:
   *#(#"local")*.
-  *port* An instance of type `<port>`_.
-  *orientation* An instance of type *one-of(#"default")*. Default
   value: *#"default"*.
-  *units* An instance of type *one-of(#"device", #"pixels", #"mm")*.
   Default value: *#"device"*.

Values
      

-  *display* An instance of type *`<display>`_*.

Description
           

Returns a suitable display for the specified port and server-path
criteria.

The *orientation* and *units* arguments can be used to specify the
orientation and display units that the returned *display* needs to use.

See also
        

`find-port`_

find-frame-manager
------------------

Function
''''''''

Summary
       

Returns a suitable frame manager for the specified criteria.

Signature
         

find-frame-manager #rest *options* #key *port server-path class palette*
=> *framem*
                                                                                    

Arguments
         

-  *options* An instance of type *<object>*.
-  *port* An instance of type `<port>`_.
-  *server-path* An instance of type *<object>*.
-  *class* An instance of type *<type>*.
-  *palette* An instance of type `<palette> <dcs.htm#11494>`_.

Values
      

-  *framem* An instance of type `See
   <frame-manager>`_.

Description
           

Returns a suitable frame manager for the specified criteria.

If necessary, you can specify a *port*, *server-path*, *class*, or
*palette*. If any of these are not specified, then the default value is
used in each case. The *class* argument specifies the class of frame
manager that should be returned.

find-port
---------

Function
''''''''

Summary
       

Returns a suitable port for the specified server-path.

Signature
         

find-port #rest *initargs* #key *server-path* => *port*
                                                       

Arguments
         

-  *initargs* An instance of type *<object>*.
-  *server-path* An instance of type *<object>*. Default value:
   *\*default-server-path\**.

Values
      

-  *port* An instance of type `<port>`_.

Description
           

Returns a suitable port for the specified server-path.

See also
        

`find-display`_

fixed-width-font?
-----------------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified text style uses a fixed-width font.

Signature
         

fixed-width-font? *text-style port* #key *character-set* => *fixed?*
                                                                    

Arguments
         

-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.
-  *port* An instance of type `<port>`_.
-  *character-set* An instance of type *<object>*. Default value:
   *$standard-character-set*.

Values
      

-  *fixed?* An instance of type *<boolean>*.

Description
           

Returns true if *text-style* uses a fixed-width font.

font-ascent
-----------

Generic function
''''''''''''''''

Summary
       

Returns the ascent of the font in the specified text style.

Signature
         

font-ascent *text-style port* #key *character-set* => *ascent*
                                                              

Arguments
         

-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.
-  *port* An instance of type `<port>`_.
-  *character-set* An instance of type *<object>*. Default value:
   *$standard-character-set*.

Values
      

-  *ascent* An instance of type *<real>*.

Description
           

Returns the ascent of the font in the *text-style* on *port*.

See also
        

`font-descent`_

`font-height`_

`font-metrics`_

`font-width`_

font-descent
------------

Generic function
''''''''''''''''

Summary
       

Returns the descent of the font in the specified text style.

Signature
         

font-descent *text-style port* #key *character-set* => *descent*
                                                                

Arguments
         

-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.
-  *port* An instance of type `<port>`_.
-  *character-set* An instance of type *<object>*.

Values
      

-  *descent* An instance of type *<real>*.

Description
           

Returns the descent of the font in the *text-style* on *port*.

See also
        

`font-ascent`_

`font-height`_

`font-metrics`_

`font-width`_

font-height
-----------

Generic function
''''''''''''''''

Summary
       

Returns the height of the font in the specified text style.

Signature
         

font-height *text-style port* #key *character-set* => *height*
                                                              

Arguments
         

-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.
-  *port* An instance of type `<port>`_.
-  *character-set* An instance of type *<object>*.

Values
      

-  *height* An instance of type *<real>*.

Description
           

Returns the height of the font in the *text-style* on *port*.

See also
        

`font-ascent`_

`font-descent`_

`font-metrics`_

`font-width`_

font-metrics
------------

Generic function
''''''''''''''''

Summary
       

Returns the metrics of the font in the specified text style.

Signature
         

font-metrics *text-style port* #key *character-set* => *font width
height ascent descent*
                                                                                         

Arguments
         

-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.
-  *port* An instance of type `<port>`_.
-  *character-set* An instance of type *<object>*.

Values
      

-  *font* An instance of type *<object>*.
-  *width* An instance of type *<real>*.
-  *height* An instance of type *<real>*.
-  *ascent* An instance of type *<real>*.
-  *descent* An instance of type *<real>*.

Description
           

Returns the metrics of the font in the *text-style* on *port*.

See also
        

`font-ascent`_

`font-descent`_

`font-height`_

`font-width`_

font-width
----------

Generic function
''''''''''''''''

Summary
       

Returns the width of the font in the specified text style.

Signature
         

font-width *text-style port* #key *character-set* => *width*
                                                            

Arguments
         

-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.
-  *port* An instance of type `<port>`_.
-  *character-set* An instance of type *<object>*.

Values
      

-  *width* An instance of type *<real>*.

Description
           

Returns the with of the font in the *text-style* on *port*.

See also
        

`font-ascent`_

`font-descent`_

`font-height`_

`font-metrics`_

force-display
-------------

Generic function
''''''''''''''''

Summary
       

Forces the specified drawable object to be displayed.

Signature
         

force-display *drawable* => ()
                              

Arguments
         

-  *drawable* An instance of type *type-union(* `See
   <sheet>`_*, `<medium>`_)*.

Values
      

None

Description
           

Forces *drawable* to be displayed.

<frame-event>
-------------

Open abstract class
'''''''''''''''''''

Summary
       

The class of events that occur in frames.

Superclasses
            

`<event>`_
                                 

Init-keywords
             

-  *frame:* An instance of type ` <frames.htm#16922>`_. Required.

Description
           

The class of events that occur in frames. The *frame:* init-keyword
specified the frame in which the event occurs.

Operations
          

-  None.

See also
        

` <frames.htm#53985>`_

` <frames.htm#27782>`_

` <frames.htm#20261>`_

` <frames.htm#25227>`_

` <frames.htm#43736>`_

` <frames.htm#49435>`_

<frame-manager>
---------------

Open abstract class
'''''''''''''''''''

Summary
       

The class of frame managers.

Superclasses
            

*<object>*
          

Init-keywords
             

None.

Description
           

The class of frame managers.

Frame managers control the realization of the look and feel of a frame.
The frame manager interprets the specification of the application frame
in the context of the available window system facilities, taking into
account preferences expressed by the user.

In addition, the frame manager takes care of attaching the pane
hierarchy of an application frame to an appropriate place in a window
hierarchy.

Thus, the frame manager decides the following:

-  What concrete gadget to create for an abstract gadget.
-  How to layout the various parts of a frame, such as its menu, tool,
   and status bars.
-  How to lay out dialogs and their exit buttons.
-  How much spacing to use in various conventional layouts.

In addition, a frame manager maps dialog functions such as `See
choose-file`_ to their appropriate native dialogs.

Operations
          

The following operations are exported from the *DUIM-Sheets* module.

`display`_ `See
frame-manager?`_ `See
frame-manager-frames`_ `See
frame-manager-palette`_ `See
frame-manager-palette-setter`_ `See
port`_

The following operations are exported from the *DUIM-Frames* module.

` <frames.htm#15853>`_ ` <frames.htm#90512>`_ ` <frames.htm#10131>`_

The following operation is exported from the *DUIM-DCs* module.

`find-color <dcs.htm#33969>`_

See also
        

`frame-manager`_

`frame-manager?`_

frame-manager
-------------

Generic function
''''''''''''''''

Summary
       

Returns the frame manager for the specified object.

Signature
         

frame-manager *object* => *value*
                                 

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *value* An instance of type *false-or
    (`<frame-manager>`_)*.

Description
           

Returns the frame manager used to control the look and feel of the
display of *object*.

See also
        

`display`_

`<frame-manager>`_

`frame-manager?`_

`port`_

frame-manager?
--------------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified object is a frame manager.

Signature
         

frame-manager? *object* => *framem?*
                                    

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *framem?* An instance of type *<boolean>*.

Description
           

Returns true if *object* is a frame manager.

See also
        

`<frame-manager>`_

`frame-manager`_

frame-manager-frames
--------------------

Generic function
''''''''''''''''

Summary
       

Returns the frames managed by the specified frame manager.

Signature
         

frame-manager-frames *framem* => *frames*
                                         

Arguments
         

-  *framem* An instance of type `See
   <frame-manager>`_.

Values
      

-  *frames* An instance of type *limited(<sequence>, of:*
   ` <frames.htm#16922>`_*)*.

Description
           

Returns the frames managed by *framem*.

frame-manager-palette
---------------------

Generic function
''''''''''''''''

Summary
       

Returns the palette used by the specified frame manager.

Signature
         

frame-manager-palette *framem* => *palette*
                                           

Arguments
         

-  *framem* An instance of type `See
   <frame-manager>`_.

Values
      

-  *palette* An instance of type `<palette> <dcs.htm#11494>`_.

Description
           

Returns the palette used by *framem*.

See also
        

`frame-manager-palette-setter`_

frame-manager-palette-setter
----------------------------

Generic function
''''''''''''''''

Summary
       

Sets the palette used by the specified frame manager.

Signature
         

frame-manager-palette-setter *palette framem* => *palette*
                                                          

Arguments
         

-  *palette* An instance of type `<palette> <dcs.htm#11494>`_.
-  *framem* An instance of type `See
   <frame-manager>`_.

Values
      

-  *palette* An instance of type `<palette> <dcs.htm#11494>`_.

Description
           

Sets the palette used by *framem*.

See also
        

`frame-manager-palette`_

<gesture>
---------

Abstract instantiable class
'''''''''''''''''''''''''''

Summary
       

The base class of all gestures.

Superclasses
            

*<object>*
          

Init-keywords
             

-  *keysym:* An instance of type *<symbol>*. Required.
-  *button:* An instance of type *<integer>*. Required.
-  *modifier-state:* An instance of type *<integer>*. Required.
-  *modifiers:* An instance of type *<sequence>*.

Description
           

The base class of all gestures.

Operations
          

` <frames.htm#89020>`_ ` <frames.htm#44746>`_ `See
event-matches-gesture?`_ ` <gadgets.htm#80239>`_ `See
gesture-modifier-state`_ `See
gesture-spec-equal`_

See also
        

`<keyboard-gesture>`_

`<pointer-gesture>`_

gesture-button
--------------

Generic function
''''''''''''''''

Summary
       

Returns the button associated with the specified gesture.

Signature
         

gesture-button *pointer-gesture* => *button*
                                            

Arguments
         

-  *pointer-gesture* An instance of type `See
   <pointer-gesture>`_.

Values
      

-  *button* An instance of type *<integer>*.

Description
           

Returns the button associated with *pointer-gesture*.

See also
        

`<pointer-gesture>`_

gesture-keysym
--------------

Generic function
''''''''''''''''

Summary
       

Returns the keysym associated with the specified gesture.

Signature
         

gesture-keysym *keyboard-gesture* => *keysym*
                                             

Arguments
         

-  *keyboard-gesture* An instance of type `See
   <keyboard-gesture>`_.

Values
      

-  *keysym* An instance of type *<symbol>*.

Description
           

Returns the keysym associated with *keyboard-gesture*.

See also
        

`<keyboard-gesture>`_

gesture-modifier-state
----------------------

Generic function
''''''''''''''''

Summary
       

Returns the modifier-state associated with the specified gesture.

Signature
         

gesture-modifier-state *gesture* => *modifier-state*
                                                    

Arguments
         

-  *gesture* An instance of type `<gesture>`_.

Values
      

-  *modifier-state* An instance of type *<integer>*.

Description
           

Returns the modifier-state associated with *gesture*.

See also
        

`event-modifier-state`_

`<keyboard-gesture>`_

`make-modifier-state`_

`port-modifier-state`_

gesture-spec-equal
------------------

Function
''''''''

Summary
       

Returns true if the two specified gestures are equivalent.

Signature
         

gesture-spec-equal *gesture1 gesture2* => *equal?*
                                                  

Arguments
         

-  *gesture1* An instance of type `<gesture>`_.
-  *gesture2* An instance of type `<gesture>`_.

Values
      

-  *equal?* An instance of type *<boolean>*.

Description
           

Returns true if *gesture1* and *gesture2* are equivalent.

See also
        

`=`_

get-clipboard-data-as
---------------------

Generic function
''''''''''''''''

Summary
       

Returns data of a given type from a clipboard.

Signature
         

get-clipboard-data-as *type clipboard* => *data*
                                                

Arguments
         

-  *type* An instance of *type-union(<symbol>,* *<type>)*.
-  *clipboard* An instance of `<clipboard>`_.

Values
      

-  *data* Instances of *<object>*.

Description
           

This generic function returns *data* of *type* from the clipboard. The
argument *type* is an instance of *type-union(<symbol>, <type>)*.

See also
        

`add-clipboard-data`_

`add-clipboard-data-as`_

`<clipboard>`_

`clipboard-data-available?`_

get-default-background
----------------------

Generic function
''''''''''''''''

Summary
       

Returns the default background for the specified sheet.

Signature
         

get-default-background *port sheet* #key *background* => *background*
                                                                     

Arguments
         

-  *port* An instance of type `<port>`_.
-  *sheet* An instance of type `<sheet>`_.
-  *background* An instance of type `<ink> <dcs.htm#15007>`_.

Values
      

-  *background* An instance of type `<ink> <dcs.htm#15007>`_.

Description
           

Returns the default background for *sheet* on *port*.

If *background* is specified, then this is used instead of the default.

See also
        

`get-default-foreground`_

`get-default-text-style`_

get-default-foreground
----------------------

Generic function
''''''''''''''''

Summary
       

Returns the default foreground for the specified sheet.

Signature
         

get-default-foreground *port sheet* #key *foreground* => *foreground*
                                                                     

Arguments
         

-  *port* An instance of type `<port>`_.
-  *sheet* An instance of type `<sheet>`_.
-  *foreground* An instance of type `<ink> <dcs.htm#15007>`_.

Values
      

-  *foreground* An instance of type `<ink> <dcs.htm#15007>`_.

Description
           

Returns the default foreground for *sheet* on *port*.

If *foreground* is specified, then this is used instead of the default.

See also
        

`get-default-background`_

`get-default-text-style`_

get-default-text-style
----------------------

Generic function
''''''''''''''''

Summary
       

Returns the default text style for the specified sheet.

Signature
         

get-default-text-style *port sheet* #key *text-style* => *text-style*
                                                                     

Arguments
         

-  *port* An instance of type `<port>`_.
-  *sheet* An instance of type `<sheet>`_.
-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.

Values
      

-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.

Description
           

Returns the default text style for *sheet* on *port*.

If *text-style* is specified, then this is used instead of the default.

See also
        

`get-default-background`_

`get-default-foreground`_

handle-event
------------

Generic function
''''''''''''''''

Summary
       

Implements any defined policies of the specified sheet with respect to
the specified event.

Signature
         

handle-event *sheet event* => ()
                                

Arguments
         

-  *sheet* An instance of type `<sheet>`_.
-  *event* An instance of type `<event>`_.

Values
      

None

Description
           

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
        

`handle-repaint`_

`queue-event`_

handle-repaint
--------------

Generic function
''''''''''''''''

Summary
       

Implements region repainting for a given sheet class.

Signature
         

handle-repaint *sheet medium region* => ()
                                          

Arguments
         

-  *sheet* An instance of type `<sheet>`_.
-  *medium* An instance of type *`<medium>`_*.
-  *region* An instance of type `<region> <geom.htm#79228>`_.

Values
      

None

Description
           

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
        

` <layouts.htm#68254>`_

` <layouts.htm#40953>`_

`queue-repaint`_

`repaint-sheet`_

` <layouts.htm#78056>`_

`<window-repaint-event>`_

$hyper-key
----------

Constant
''''''''

Summary
       

A constant that represents the HYPER key on the keyboard.

Type
    

*<integer>*
           

Value
     

ash(1, %modifier\_base + 4);
                            

Description
           

A constant that represents the HYPER key on the keyboard.

See also
        

`$alt-key`_

`$control-key`_

`$meta-key`_

`modifier-key-index`_

`modifier-key-index-name`_

`$modifier-keys`_

`$option-key`_

`$shift-key`_

`$super-key`_

<keyboard-event>
----------------

Open abstract class
'''''''''''''''''''

Summary
       

The base class of all keyboard events.

Superclasses
            

`<device-event>`_
                                        

Init-keywords
             

-  *key-name:* An instance of type *false-or(<symbol>)*. Default value:
   *#f*.
-  *character:* An instance of type *false-or(<character>)*. Default
   value: *#f*.

Description
           

The base class of all keyboard events.

The key-name: init-keyword represents the name of the key on the
keyboard that was pressed.

The *character:* init-keyword represents the keyboard character that was
pressed for characters in the standard character set.

Operations
          

The following operations are exported from the *DUIM-Sheets* module.

-  `event-character`_ `See
   event-key-name`_ `See
   event-matches-gesture?`_

See also
        

`event-character`_

`event-key-name`_

`<key-press-event>`_

`<key-release-event>`_

<keyboard-gesture>
------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The base class of all keyboard gestures.

Superclasses
            

`<gesture>`_
                                   

Init-keywords
             

-  *keysym:* An instance of type *<symbol>*.
-  *modifier-state:* An instance of type *<integer>*.

Description
           

The base class of all keyboard gestures.

The *keysym:* init-keyword represents the keysym for the gesture, and
the *modifier-state:* init-keyword represents its modifier state.

Operations
          

-  `gesture-keysym`_

See also
        

`gesture-keysym`_

`gesture-modifier-state`_

<key-press-event>
-----------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events passed when a key is pressed.

Superclasses
            

`<keyboard-event>`_
                                          

Init-keywords
             

None.

Description
           

The class of events passed when a key is pressed.

Operations
          

-  None.

See also
        

`<keyboard-event>`_

`<key-release-event>`_

<key-release-event>
-------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events passed when a key is released.

Superclasses
            

`<keyboard-event>`_
                                          

Init-keywords
             

None.

Description
           

The class of events passed when a key is released.

Operations
          

-  None.

See also
        

`<keyboard-event>`_

`<key-press-event>`_

$left-button
------------

Constant
''''''''

Summary
       

A constant that represents the left button on the attached pointing
device.

Type
    

*<integer>*
           

Value
     

ash(1, %button\_base + 0)
                         

Description
           

A constant that represents the left button on the attached pointing
device.

See also
        

`$middle-button`_

`$pointer-buttons`_

`$right-button`_

lower-sheet
-----------

Generic function
''''''''''''''''

Summary
       

Lowers the specified sheet to the bottom of the current hierarchy of
sheets.

Signature
         

lower-sheet *sheet* => ()
                         

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

None

Description
           

Lowers *sheet* to the bottom of the current hierarchy of sheets.

See also
        

` <frames.htm#85200>`_

` <frames.htm#28075>`_

`raise-sheet`_

make-frame-manager
------------------

Generic function
''''''''''''''''

Summary
       

Returns an instance of *<frame-manager>* on the specified port.

Signature
         

make-frame-manager *port* #key *palette* => *framem*
                                                    

Arguments
         

-  *port* An instance of type `<port>`_.
-  *palette* An instance of type `<palette> <dcs.htm#11494>`_.

Values
      

-  *framem* An instance of type `See
   <frame-manager>`_.

Description
           

Returns an instance of *<frame-manager>* on *port*. If specified, the
palette described by *palette* is used.

See also
        

`<frame-manager>`_

make-modifier-state
-------------------

Function
''''''''

Summary
       

Returns a modifier state for the specified modifiers.

Signature
         

make-modifier-state #rest *modifiers* => *integer*
                                                  

Arguments
         

-  *modifiers* An instance of type *limited(<sequence>, of: <integer>)*
   .

Values
      

-  *integer* An instance of type *<integer>*.

Description
           

Returns a modifier state for *modifiers*.

See also
        

`event-modifier-state`_

`gesture-modifier-state`_

`port-modifier-state`_

make-pane
---------

Generic function
''''''''''''''''

Summary
       

Selects and returns an instance of a suitable class of pane for the
supplied options.

Signature
         

make-pane *pane-class* #rest *pane-options* #key *frame-manager* =>
*sheet*
                                                                           

Arguments
         

-  *pane-class* An instance of type *<class>*.
-  *pane-options* Instances of type *<object>*.
-  *frame-manager* An instance of type `See
   <frame-manager>`_.

Values
      

-  *sheet* An instance of type `<sheet>`_.

Description
           

Selects a class that implements the behavior of pane-class and
constructs a pane of that class.

<medium>
--------

Open abstract instantiable class
''''''''''''''''''''''''''''''''

Summary
       

The class of all mediums.

Superclasses
            

*<object>*
          

Init-keywords
             

None.

Description
           

The class of all mediums.

Mediums have the following elements associated with them:

-  A drawing plane, to which text and lines may be drawn
-  A foreground color, which describes the default color of anything
   drawn on the drawing plane
-  A background color, which describes the background color of the
   drawing plane
-  A transformation which describes the position of the drawing plane
   relative to the sheet which is its parent
-  A clipping region, on which any editing operations (such as cutting,
   copying, or pasting) will have effect.
-  A line style that describes the appearance of any lines drawn on the
   drawing plane
-  A text style that describes the appearance of any text written to the
   drawing plane

Operations
          

The following operations are exported from the *DUIM-Sheets* module.

`beep`_ `clear-box`_ `See
display`_ `See
do-with-drawing-options`_ `See
do-with-text-style`_ `See
do-with-transform`_ `See
force-display`_ `See
handle-repaint`_ `medium?`_
`medium-background`_ `See
medium-background-setter`_ `See
medium-brush`_ `See
medium-brush-setter`_ `See
medium-clipping-region`_ `See
medium-clipping-region-setter`_ `See
medium-default-text-style`_ `See
medium-default-text-style-setter`_ `See
medium-drawable`_ `See
medium-drawable-setter`_ `See
medium-foreground`_ `See
medium-foreground-setter`_ `See
medium-merged-text-style`_ `See
medium-pen`_ `See
medium-pen-setter`_ `See
medium-pixmap`_ `See
medium-pixmap-setter`_ `See
medium-sheet`_ `See
medium-text-style`_ `See
medium-text-style-setter`_ `See
medium-transform`_ `See
medium-transform-setter`_ `See
port`_`synchronize-display`_
`text-size`_

The following operations are exported from the *DUIM-Graphics* module.

` <graphics.htm#62343>`_ ` <graphics.htm#64636>`_
` <graphics.htm#28334>`_ ` <graphics.htm#69389>`_
` <graphics.htm#62628>`_ ` <graphics.htm#64653>`_
` <graphics.htm#55252>`_

The following operations are exported from the *DUIM-Extended-Geometry*
module.

`draw-design <ext-geom.htm#88093>`_

See also
        

`medium?`_

` <graphics.htm#46456>`_

medium?
-------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified object is a medium.

Signature
         

medium? *object* => *medium?*
                             

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *medium?* An instance of type *<boolean>*.

Description
           

Returns true if *object* is a medium.

See also
        

`<medium>`_

`sheet?`_

medium-background
-----------------

Generic function
''''''''''''''''

Summary
       

Returns the background for the specified medium.

Signature
         

medium-background *medium* => *ink*
                                   

Arguments
         

-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *ink* An instance of type `<ink> <dcs.htm#15007>`_.

Description
           

Returns the background for *medium*.

See also
        

`medium-background-setter`_

`medium-foreground`_

medium-background-setter
------------------------

Generic function
''''''''''''''''

Summary
       

Sets the background for the specified medium.

Signature
         

medium-background-setter *background medium* => *background*
                                                            

Arguments
         

-  *background* An instance of type `<ink> <dcs.htm#15007>`_.
-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *background* An instance of type `<ink> <dcs.htm#15007>`_.

Description
           

Sets the background for *medium*.

See also
        

`medium-background`_

`medium-foreground-setter`_

medium-brush
------------

Generic function
''''''''''''''''

Summary
       

Returns the brush for the specified medium.

Signature
         

medium-brush *medium* => *brush*
                                

Arguments
         

-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *brush* An instance of type `<brush> <dcs.htm#29492>`_.

Description
           

Returns the brush for *medium*. This brush is used by all subsequent
painting operations on *medium*.

See also
        

`medium-brush-setter`_

`medium-pen`_

medium-brush-setter
-------------------

Generic function
''''''''''''''''

Summary
       

Sets the brush for the specified medium.

Signature
         

medium-brush-setter *brush medium* => *brush*
                                             

Arguments
         

-  *brush* An instance of type `<brush> <dcs.htm#29492>`_.
-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *brush* An instance of type `<brush> <dcs.htm#29492>`_.

Description
           

Sets the brush for *medium*. This brush is used by all subsequent
painting operations on *medium*.

See also
        

`medium-brush`_

`medium-pen-setter`_

medium-clipping-region
----------------------

Generic function
''''''''''''''''

Summary
       

Returns the clipping region for the specified medium.

Signature
         

medium-clipping-region *medium* => *region*
                                           

Arguments
         

-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *region* An instance of type `<region> <geom.htm#79228>`_.

Description
           

Returns the clipping region for *medium*.

See also
        

`medium-clipping-region-setter`_

medium-clipping-region-setter
-----------------------------

Generic function
''''''''''''''''

Summary
       

Sets the clipping region for the specified medium.

Signature
         

medium-clipping-region-setter *region medium* => *region*
                                                         

Arguments
         

-  *region* An instance of type `<region> <geom.htm#79228>`_.
-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *region* An instance of type `<region> <geom.htm#79228>`_.

Description
           

Sets the clipping region for *medium*.

See also
        

`medium-clipping-region`_

medium-default-text-style
-------------------------

Generic function
''''''''''''''''

Summary
       

Returns the default text style for the specified medium.

Signature
         

medium-default-text-style *medium* => *text-style*
                                                  

Arguments
         

-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.

Description
           

Returns the default text style for *medium*. This style is used for any
subsequent text that is written to *medium*.

See also
        

`medium-default-text-style-setter`_

`medium-merged-text-style`_

`medium-text-style`_

medium-default-text-style-setter
--------------------------------

Generic function
''''''''''''''''

Summary
       

Sets the default text style for the specified medium.

Signature
         

medium-default-text-style-setter *text-style medium* => *text-style*
                                                                    

Arguments
         

-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.
-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.

Description
           

Sets the default text style for *medium*. This style is used for any
subsequent text that is written to *medium*.

See also
        

`medium-default-text-style`_

`medium-text-style-setter`_

medium-drawable
---------------

Generic function
''''''''''''''''

Summary
       

Returns the drawable for the specified medium.

Signature
         

medium-drawable *medium* => *drawable*
                                      

Arguments
         

-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *drawable* An instance of type *<object>*.

Description
           

Returns the drawable for *medium*.

See also
        

`medium-drawable-setter`_

medium-drawable-setter
----------------------

Generic function
''''''''''''''''

Summary
       

Sets the drawable for the specified medium.

Signature
         

medium-drawable-setter *drawable medium* => *object*
                                                    

Arguments
         

-  *drawable* An instance of type *type-union(* `See
   <sheet>`_*, `<medium>`_)*.
-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *object* An instance of type *<object>*.

Description
           

Sets the drawable for *medium*.

See also
        

`medium-drawable`_

medium-foreground
-----------------

Generic function
''''''''''''''''

Summary
       

Returns the foreground of the specified medium.

Signature
         

medium-foreground *medium* => *ink*
                                   

Arguments
         

-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *ink* An instance of type `<ink> <dcs.htm#15007>`_.

Description
           

Returns the foreground of *medium*.

See also
        

`medium-background`_

`medium-foreground-setter`_

medium-foreground-setter
------------------------

Generic function
''''''''''''''''

Summary
       

Sets the foreground of the specified medium.

Signature
         

medium-foreground-setter *foreground medium* => *foreground*
                                                            

Arguments
         

-  *foreground* An instance of type `<ink> <dcs.htm#15007>`_.
-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *foreground* An instance of type `<ink> <dcs.htm#15007>`_.

Description
           

Sets the foreground of *medium*.

See also
        

`medium-background-setter`_

`medium-foreground`_

medium-merged-text-style
------------------------

Generic function
''''''''''''''''

Summary
       

Returns the merged text style of the specified medium.

Signature
         

medium-merged-text-style *medium* => *text-style*
                                                 

Arguments
         

-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.

Description
           

Returns the merged text style of *medium*.

See also
        

`medium-default-text-style`_

`medium-text-style`_

medium-pen
----------

Generic function
''''''''''''''''

Summary
       

Returns the pen for the specified medium.

Signature
         

medium-pen *medium* => *pen*
                            

Arguments
         

-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *pen* An instance of type `<pen> <dcs.htm#41757>`_.

Description
           

Returns the pen for *medium*. This brush is used by all subsequent
drawing operations on *medium*.

See also
        

`medium-brush`_

`medium-pen-setter`_

medium-pen-setter
-----------------

Generic function
''''''''''''''''

Summary
       

Sets the pen for the specified medium.

Signature
         

medium-pen-setter *pen medium* => *pen*
                                       

Arguments
         

-  *pen* An instance of type `<pen> <dcs.htm#41757>`_.
-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *pen* An instance of type `<pen> <dcs.htm#41757>`_.

Description
           

Sets the pen for *medium*. This brush is used by all subsequent drawing
operations on *medium*.

See also
        

`medium-brush-setter`_

`medium-pen`_

medium-pixmap
-------------

Generic function
''''''''''''''''

Summary
       

Returns the pixmap for the specified medium.

Signature
         

medium-pixmap *medium* => *value*
                                 

Arguments
         

-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *value* An instance of type *false-or(` <graphics.htm#45866>`_)*.

Description
           

Returns the pixmap for *medium*.This pixmap is used by all subsequent
pixmap operations on *medium*.

See also
        

`medium-pixmap-setter`_

medium-pixmap-setter
--------------------

Generic function
''''''''''''''''

Summary
       

Sets the pixmap for the specified medium.

Signature
         

medium-pixmap-setter *pixmap medium* => *value*
                                               

Arguments
         

-  *pixmap* An instance of type ` <graphics.htm#45866>`_.
-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *value* An instance of type *false-or(* ` <graphics.htm#45866>`_*)*.

Description
           

Returns the pixmap for *medium*.This pixmap is used by all subsequent
pixmap operations on *medium*.

See also
        

`medium-pixmap`_

medium-sheet
------------

Generic function
''''''''''''''''

Summary
       

Returns the sheet for the specified medium.

Signature
         

medium-sheet *medium* => *sheet*
                                

Arguments
         

-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *sheet* An instance of type *false-or(* `See
   <sheet>`_*)*.

Description
           

Returns the sheet for *medium*, if there is one.

medium-text-style
-----------------

Generic function
''''''''''''''''

Summary
       

Returns the text style for the specified medium.

Signature
         

medium-text-style *medium* => *text-style*
                                          

Arguments
         

-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.

Description
           

Returns the text style for *medium*.

See also
        

`medium-default-text-style`_

`medium-merged-text-style`_

`medium-text-style-setter`_

medium-text-style-setter
------------------------

Generic function
''''''''''''''''

Summary
       

Sets the text style for the specified medium.

Signature
         

medium-text-style-setter *text-style medium* => *text-style*
                                                            

Arguments
         

-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.
-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.

Description
           

Sets the text style for *medium*.

See also
        

`medium-default-text-style-setter`_

`medium-text-style`_

medium-transform
----------------

Generic function
''''''''''''''''

Summary
       

Returns the transform for the specified medium.

Signature
         

medium-transform *medium* => *transform*
                                        

Arguments
         

-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *transform* An instance of type `<transform> <geom.htm#54995>`_.

Description
           

Returns the transform for *medium*.

See also
        

`medium-transform-setter`_

`sheet-transform`_

medium-transform-setter
-----------------------

Generic function
''''''''''''''''

Summary
       

Sets the transform for the specified medium.

Signature
         

medium-transform-setter *transform medium* => *transform*
                                                         

Arguments
         

-  *transform* An instance of type `<transform> <geom.htm#54995>`_.
-  *medium* An instance of type *`<medium>`_*.

Values
      

-  *transform* An instance of type `<transform> <geom.htm#54995>`_.

Description
           

Sets the transform for *medium*.

See also
        

`medium-transform`_

`sheet-transform-setter`_

$meta-key
---------

Constant
''''''''

Summary
       

A constant that represents the META key on the keyboard.

Type
    

*<integer>*
           

Value
     

ash(1, %modifier\_base + 2);
                            

Description
           

A constant that represents the META key on the keyboard, if it exists.
To deal with the case where there is no META key, the value of the
constant `$alt-key`_ is bound to this constant.

See also
        

`$alt-key`_

`$control-key`_

`$hyper-key`_

`modifier-key-index`_

`modifier-key-index-name`_

`$modifier-keys`_

`$option-key`_

`$shift-key`_

`$super-key`_

$middle-button
--------------

Constant
''''''''

Summary
       

A constant that represents the middle button on the attached pointing
device.

Type
    

*<integer>*
           

Value
     

ash(1, %button\_base + 1)
                         

Description
           

A constant that represents the middle button on the attached pointing
device.

See also
        

`$left-button`_

`$pointer-buttons`_

`$right-button`_

modifier-key-index
------------------

Function
''''''''

Summary
       

Returns the index number of the specified modifier key.

Signature
         

modifier-key-index *key-name* => *index*
                                        

Arguments
         

-  *key-name* An instance of type *<symbol>*.

Values
      

-  *index* An instance of type *<integer>*.

Description
           

Returns the index number of the specified modifier key. The *key-name*
specified may be any of the elements of `See
$modifier-keys`_

The returned index value is either 0, 1, 2, 3, or 4.

See also
        

`$alt-key`_

`$control-key`_

`$hyper-key`_

`$meta-key`_

`modifier-key-index-name`_

`$modifier-keys`_

`$option-key`_

`$shift-key`_

`$super-key`_

modifier-key-index-name
-----------------------

Function
''''''''

Summary
       

Returns the key name of the specified modifier key index.

Signature
         

modifier-key-index-name *index* => *key-name*
                                             

Arguments
         

-  *index* An instance of type *<integer>*.

Values
      

-  *key-name* An instance of type *<symbol>*.

Description
           

Returns the key name of the specified modifier key index. The *index*
specified is either 0, 1, 2, 3, or 4.

The *key-name* returned may be any of the elements of `See
$modifier-keys`_

See also
        

`$alt-key`_

`$control-key`_

`$hyper-key`_

`$meta-key`_

`modifier-key-index`_

`$modifier-keys`_

`$option-key`_

`$shift-key`_

`$super-key`_

$modifier-keys
--------------

Constant
''''''''

Summary
       

The default list of keys on the keyboard that are used as modifiers.

Type
    

*<sequence>*
            

Value
     

#[#"shift", #"control", #"meta", #"super", #"hyper"]
                                                    

Description
           

The default list of keys on the keyboard that are used as modifiers for
keyboard accelerators and mnemonics.

See also
        

`$alt-key`_

`$control-key`_

`$hyper-key`_

`$meta-key`_

`modifier-key-index`_

`modifier-key-index-name`_

`$option-key`_

`$shift-key`_

`$super-key`_

notify-user
-----------

Generic function
''''''''''''''''

Summary
       

Creates and displays an alert dialog box with the specified criteria.

Signature
         

notify-user *message-string* #key *frame owner title documentation
exit-boxes name style foreground background text-style* => *boolean*
                                                                                                                                       

Arguments
         

-  *message-string* An instance of type *<string>*.
-  *frame* An instance of type ` <frames.htm#16922>`_. Default value:
   *` <frames.htm#78942>`_()*.
-  *owner* An instance of type `<sheet>`_.
-  *title* An instance of type *<string>*.
-  *documentation* An instance of type *false-or(<string>)*. Default
   value: *#f*.
-  *exit-boxes* An instance of type *<object>*.
-  *name* An instance of type *<object>*.
-  *style* An instance of type *one-of(#"information", #"question",
   #"warning", #"error", #"serious-error", #"fatal-error")*.
-  *foreground* An instance of type *false-or(* `See
   <ink> <dcs.htm#15007>`_*)*. Default value: *#f*.
-  *background* An instance of type *false-or(* `See
   <ink> <dcs.htm#15007>`_*)*. Default value: *#f*.
-  *text-style* An instance of type *false-or(* `See
   <text-style> <dcs.htm#85385>`_*)*. Default value: *#f*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Creates and displays an alert dialog box with the specified criteria.
Use this function as a way of easily displaying simple messages to the
user.

Simple output from notify-user
                              

.. figure:: images/silica-2.png
   :align: center
   :alt: 

.. figure:: images/silica-5.png
   :align: center
   :alt: 
The *message-string* is the message that is displayed in the dialog. The
arguments frame, owner, title, and documentation let you specify
different attributes for the dialog in the same way as they can be
specified for any other frame or dialog.

The *exit-boxes* argument lets you specify the buttons that are
available in the dialog. If not supplied, then a single *OK* button is
used by default, unless the *style* of the dialog is set to
*#"question"*, in which case, two buttons are created, to allow the
user to respond “yes” or “no”.

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
        

`choose-color`_

`choose-directory`_

`choose-file`_

open-clipboard
--------------

Function
''''''''

Summary
       

Creates a clipboard lock for a sheet on a port.

Signature
         

open-clipboard *port sheet* => *clipboard*
                                          

Arguments
         

-  *port* An instance of `<port>`_.
-  *sheet* An instance of `<sheet>`_.

Values
      

-  *clipboard* An instance of `<clipboard>`_.

Description
           

Creates a clipboard lock for *sheet* on *port*. Once a clipboard lock
has been created, you can manipulate the clipboard contents safely. An
instance of `<clipboard>`_ is returned, which is
used to hold the clipboard contents.

You should not normally call *open-clipboard* yourself to create a
clipboard lock. Use the macro `with-clipboard`_
to create and free the lock for you.

See also
        

`<clipboard>`_

`with-clipboard`_

$option-key
-----------

Constant
''''''''

Summary
       

A constant that represents the OPTION key on the keyboard.

Type
    

*<integer>*
           

Value
     

`$super-key`_
                                    

Description
           

A constant that represents the OPTION key on the keyboard. This is set
to the same value as the SUPER key, to deal with the case where the
OPTION key is not present on the keyboard.

See also
        

`$alt-key`_

`$control-key`_

`$hyper-key`_

`$meta-key`_

`modifier-key-index`_

`modifier-key-index-name`_

`$modifier-keys`_

`$shift-key`_

`$super-key`_

<pointer>
---------

Open abstract instantiable class
''''''''''''''''''''''''''''''''

Summary
       

The class of all pointers.

Superclasses
            

*<object>*
          

Init-keywords
             

-  *port:* An instance of type `<port>`_.

Description
           

The class of all pointers.

Operations
          

The following operations are exported from the *DUIM-Sheets* module.

`display`_ `pointer?`_
`pointer-button-state`_ `See
pointer-cursor`_ `See
pointer-cursor-setter`_ `See
pointer-position`_ `See
pointer-sheet`_ `port`_ `See
set-pointer-position`_

See also
        

`pointer?`_

pointer?
--------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified object is a pointer.

Signature
         

pointer? *object* => *pointer?*
                               

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *pointer?* An instance of type *<boolean>*.

Description
           

Returns true if *object* is a pointer.

See also
        

`<pointer>`_

<pointer-boundary-event>
------------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class that corresponds to a pointer motion event that crosses a
sheet boundary.

Superclasses
            

`<pointer-motion-event>`_
                                                

Init-keywords
             

-  *kind:* An instance of type *one-of(#"ancestor", #"virtual",
   #"inferior", #"nonlinear", #"nonlinear-virtual", #f)*. Default
   value: *#f*.

Description
           

The class that corresponds to a pointer motion event that crosses some
sort of sheet boundary.

The *kind:* init-keyword represents the boundary event kind. These
correspond to the detail members for X11 enter and exit events.

Operations
          

The following operation is exported from the *DUIM-Sheets* module.

-  `boundary-event-kind`_

See also
        

`boundary-event-kind`_

`<pointer-enter-event>`_

`<pointer-exit-event>`_

<pointer-button-event>
----------------------

Open abstract class
'''''''''''''''''''

Summary
       

The class of events that occur when mouse buttons are pressed.

Superclasses
            

`<pointer-event>`_
                                         

Init-keywords
             

-  *button:* An instance of type *one-of(`See
   $left-button`_, `See
   $middle-button`_, `See
   $right-button`_)*.

Description
           

The class of events that occur when mouse buttons are pressed.

Operations
          

The following operations are exported from the *DUIM-Sheets* module.

-  `event-button`_ `See
   event-matches-gesture?`_ `See
   handle-event`_

See also
        

`event-button`_

`$left-button`_

`$middle-button`_

`pointer-button-state`_

`<pointer-drag-event>`_

`$right-button`_

$pointer-buttons
----------------

Constant
''''''''

Summary
       

The constant representing the possible buttons on the pointing device.

Type
    

*<sequence>*
            

Value
     

#[#"left", #"middle", #"right"];
                                

Description
           

The constant representing the possible buttons on the pointing device
attached to the computer, typically a mouse. Up to three buttons are
provided for.

The order of the elements in this sequence must match the order of the
values of `$left-button`_, `See
$middle-button`_, and `See
$right-button`_

See also
        

`button-index`_

`button-index-name`_

`$left-button`_

`$middle-button`_

`$right-button`_

pointer-button-state
--------------------

Generic function
''''''''''''''''

Summary
       

Returns the state of the specified pointer.

Signature
         

pointer-button-state *pointer* => *integer*
                                           

Arguments
         

-  *pointer* An instance of type `<pointer>`_.

Values
      

-  *integer* An instance of type *<integer>*.

Description
           

Returns the state of *pointer*.

pointer-cursor
--------------

Generic function
''''''''''''''''

Summary
       

Returns the cursor used for the specified pointer.

Signature
         

pointer-cursor *pointer* => *cursor*
                                    

Arguments
         

-  *pointer* An instance of type `<pointer>`_.

Values
      

-  *cursor* An instance of type `<cursor>`_.

Description
           

Returns the cursor used for *pointer*.

See also
        

`pointer-cursor-setter`_

pointer-cursor-setter
---------------------

Generic function
''''''''''''''''

Summary
       

Sets the cursor used for the specified pointer.

Signature
         

pointer-cursor-setter *cursor pointer* => cursor
                                                

Arguments
         

-  *cursor* An instance of type `<cursor>`_.
-  *pointer* An instance of type `<pointer>`_.

Values
      

-  *cursor* An instance of type `<cursor>`_.

Description
           

Sets the cursor used for *pointer*.

See also
        

`pointer-cursor`_

<pointer-drag-event>
--------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events describing drag movements.

Superclasses
            

*`<pointer-motion-event>`_ `See
<pointer-button-event>`_*
                                                                                                   

Init-keywords
             

-  *button:* An instance of type *one-of(`See
   $left-button`_, `See
   $middle-button`_, `See
   $right-button`_)*.

Description
           

The class of events describing drag movements. This is the same as `See
<pointer-motion-event>`_, except that a button on the
attached pointing device must also be held down as the pointer is
moving.

The *button:* init-keyword is inherited from the superclass `See
<pointer-button-event>`_.

Operations
          

-  None.

See also
        

`<pointer-motion-event>`_

<pointer-enter-event>
---------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events that describe a pointer entering an area such as a
sheet.

Superclasses
            

`<pointer-boundary-event>`_
                                                  

Init-keywords
             

None.

Description
           

The class of events that describe a pointer entering an area such as a
sheet.

Operations
          

-  None.

See also
        

`<pointer-exit-event>`_

<pointer-event>
---------------

Open abstract class
'''''''''''''''''''

Summary
       

The base class of events occurring on pointers.

Superclasses
            

`<device-event>`_
                                        

Init-keywords
             

-  *x:* An instance of type *<real>*.
-  *y:* An instance of type *<real>*.
-  *pointer:* An instance of type `<pointer>`_.

Description
           

The base class of events occurring on pointers on the computer screen.

The *x:* and *y:* init-keywords specify the location of the pointer when
the event occurs. The *pointer:* init-keyword specifies the pointer to
which the event occurs.

Operations
          

-  None.

See also
        

`<pointer-button-event>`_

`<pointer-exit-event>`_

`<pointer-motion-event>`_

<pointer-exit-event>
--------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events that describe a pointer leaving an area such as a
sheet.

Superclasses
            

`<pointer-boundary-event>`_
                                                  

Init-keywords
             

None.

Description
           

The class of events that describe a pointer leaving an area such as a
sheet.

Operations
          

-  None.

See also
        

`<pointer-button-event>`_

`<pointer-enter-event>`_

`<pointer-motion-event>`_

<pointer-gesture>
-----------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of all gestures that occur on pointers.

Superclasses
            

`<gesture>`_
                                   

Init-keywords
             

-  *button:* An instance of type *<integer>*.
-  *modifier-state:* An instance of type *<integer>*.

Description
           

The class of all gestures that occur on pointers.

The *button:* init-keyword specifies the button on the attached pointer
device on which the gesture has occurred, and the *modifier-state:*
init-keyword specifies the modifier-state of the gesture.

Operations
          

-  `gesture-button`_

<pointer-motion-event>
----------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events that describe a pointer that is moving.

Superclasses
            

`<pointer-event>`_
                                         

Init-keywords
             

None.

Description
           

The class of events that describe a pointer that is moving.

Operations
          

-  None.

See also
        

`<pointer-button-event>`_

`<pointer-drag-event>`_

`<pointer-enter-event>`_

`<pointer-event>`_

`<pointer-exit-event>`_

pointer-position
----------------

Generic function
''''''''''''''''

Summary
       

Returns the current position of the specified pointer.

Signature
         

pointer-position *pointer* #key *sheet* => *x y*
                                                

Arguments
         

-  *pointer* An instance of type `<pointer>`_.
-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *x* An instance of type *<real>*.
-  *y* An instance of type *<real>*.

Description
           

Returns the current position of *pointer*. If *sheet* is specified,
then the pointer must be over it.

See also
        

`pointer-sheet`_

`set-pointer-position`_

pointer-sheet
-------------

Generic function
''''''''''''''''

Summary
       

Returns the sheet under the specified pointer.

Signature
         

pointer-sheet *pointer* => *sheet*
                                  

Arguments
         

-  *pointer* An instance of type `<pointer>`_.

Values
      

-  *sheet* An instance of type *false-or(* `See
   <sheet>`_*)*.

Description
           

Returns the sheet under *pointer*, or #f if there is no sheet under the
pointer.

See also
        

`pointer-position`_

<port>
------

Open abstract class
'''''''''''''''''''

Summary
       

The class of all ports.

Superclasses
            

*<object>*
          

Init-keywords
             

None.

Description
           

The class of all ports. A display, and all the sheets attached to a
display, is associated with a port that is a connection to a display
server. The port manages:

-  A primary input device (usually a keyboard)
-  A pointing device, such as a mouse or trackball
-  An event processor that dispatched events to the appropriate sheet.

Operations
          

The following operations are exported from the *DUIM-Sheets* module.

`beep`_ `See
default-port-setter`_ `See
destroy-port`_ `See
force-display`_ `See
get-default-background`_ `See
get-default-foreground`_ `See
get-default-text-style`_ `See
port`_ `port?`_ `See
port-modifier-state`_ `See
port-pointer`_ `See
port-server-path`_ `See
synchronize-display`_ `See
text-size`_ `See
text-style-mapping`_ `See
text-style-mapping-setter`_

The following operation is exported from the *DUIM-DCs* module.

`find-color <dcs.htm#33969>`_

See also
        

`<display>`_

`<sheet>`_

port
----

Generic function
''''''''''''''''

Summary
       

Returns the port for the specified object.

Signature
         

port *object* => *value*
                        

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *value* An instance of type *false-or(* `See
   <port>`_*)*.

Description
           

Returns the port used to display *object*.

See also
        

`display`_

`frame-manager`_

`<port>`_

`port?`_

port?
-----

Generic function
''''''''''''''''

Summary
       

Returns true if the specified object is a port.

Signature
         

port? *object* => *boolean*
                           

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns true if *object* is a port.

See also
        

`<port>`_

`<port>`_

port-modifier-state
-------------------

Generic function
''''''''''''''''

Summary
       

Returns the modifier state of the specified port.

Signature
         

port-modifier-state *port* => *integer*
                                       

Arguments
         

-  *port* An instance of type `<port>`_.

Values
      

-  *integer* An instance of type *<integer>*.

Description
           

Returns the modifier state of *port*.

See also
        

`event-modifier-state`_

`gesture-modifier-state`_

`make-modifier-state`_

`port-name`_

`port-pointer`_

`port-server-path`_

`port-type`_

port-name
---------

Generic function
''''''''''''''''

Summary
       

Returns the name of the specified port.

Signature
         

port-name *port* => *name*
                          

Arguments
         

-  *port* An instance of type `<port>`_.

Values
      

-  *name* An instance of type *<object>*.

Description
           

Returns the name of *port*.

See also
        

`port-modifier-state`_

`port-pointer`_

`port-server-path`_

`port-type`_

port-pointer
------------

Generic function
''''''''''''''''

Summary
       

Returns the pointer used on the specified port.

Signature
         

port-pointer *port* => *pointer*
                                

Arguments
         

-  *port* An instance of type `<port>`_.

Values
      

-  *pointer* An instance of type `<pointer>`_.

Description
           

Returns the pointer used on *port*.

See also
        

`port-modifier-state`_

`port-name`_

`port-server-path`_

`port-type`_

port-server-path
----------------

Generic function
''''''''''''''''

Summary
       

Returns the server path of the specified port.

Signature
         

port-server-path *port* => *object*
                                   

Arguments
         

-  *port* An instance of type `<port>`_.

Values
      

-  *object* An instance of type *<object>*.

Description
           

Returns the server path of *port*.

See also
        

`port-modifier-state`_

`port-name`_

`port-pointer`_

`port-type`_

<port-terminated-event>
-----------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events that describe the termination of a port.

Superclasses
            

` <frames.htm#20261>`_
                      

Init-keywords
             

-  *condition:* An instance of type *<condition>*. Required.

Description
           

The class of events that describe the termination of a port.

The *condition:* init-keyword returns the error condition signalled when
the port was terminated.

Operations
          

-  None.

port-type
---------

Generic function
''''''''''''''''

Summary
       

Returns the type of the specified port.

Signature
         

port-type *port* => *type*
                          

Arguments
         

-  *port* An instance of type `<port>`_.

Values
      

-  *type* An instance of type *<symbol>*.

Description
           

Returns the type of *port*.

See also
        

`port-modifier-state`_

`port-name`_

`port-pointer`_

`port-server-path`_

queue-event
-----------

Generic function
''''''''''''''''

Summary
       

Queues an event for the specified sheet.

Signature
         

queue-event *sheet event* => ()
                               

Arguments
         

-  *sheet* An instance of type `<sheet>`_.
-  *event* An instance of type `<event>`_.

Values
      

None

Description
           

Queues *event* on the event-queue for *sheet*.

See also
        

`handle-event`_

queue-repaint
-------------

Generic function
''''''''''''''''

Summary
       

Queues a repaint for the specified region of the specified sheet.

Signature
         

queue-repaint *sheet region* => ()
                                  

Arguments
         

-  *sheet* An instance of type `<sheet>`_.
-  *region* An instance of type `<region> <geom.htm#79228>`_.

Values
      

None

Description
           

Queues a repaint for the area** of *sheet* defined by *region*.

See also
        

`handle-repaint`_

`repaint-sheet`_

`<window-repaint-event>`_

raise-sheet
-----------

Generic function
''''''''''''''''

Summary
       

Raises the specified sheet to the top of the current hierarchy of
sheets.

Signature
         

raise-sheet *sheet* => ()
                         

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

None

Description
           

Raises *sheet* to the top of the current hierarchy of sheets.

See also
        

` <frames.htm#85200>`_

`lower-sheet`_

` <frames.htm#28075>`_

remove-child
------------

Generic function
''''''''''''''''

Summary
       

Removes a child from the specified sheet.

Signature
         

remove-child *sheet child* => *sheet*
                                     

Arguments
         

-  *sheet* An instance of type `<sheet>`_.
-  *child* An instance of type `<sheet>`_.

Values
      

-  *sheet* An instance of type `<sheet>`_.

Description
           

Removes *child* from *sheet*. The remaining children in the sheet are
laid out again appropriately.

See also
        

`add-child`_

`replace-child`_

repaint-sheet
-------------

Generic function
''''''''''''''''

Summary
       

Repaints the specified region of a sheet.

Signature
         

repaint-sheet *sheet region* #key *medium* => ()
                                                

Arguments
         

-  *sheet* An instance of type `<sheet>`_.
-  *region* An instance of type `<region> <geom.htm#79228>`_.
-  *medium* An instance of type *`<medium>`_*.

Values
      

None

Description
           

Repaints the are of *sheet* defined by *region*. If specified, the
appropriate *medium* is used.

See also
        

`handle-repaint`_

`queue-repaint`_

`<window-repaint-event>`_

replace-child
-------------

Generic function
''''''''''''''''

Summary
       

Replaces a child from the specified sheet with a new one.

Signature
         

replace-child *sheet old-child new-child* => *sheet*
                                                    

Arguments
         

-  *sheet* An instance of type `<sheet>`_.
-  *old-child* An instance of type *<object>*.
-  *new-child* An instance of type *<object>*.

Values
      

-  *sheet* An instance of type `<sheet>`_.

Description
           

Replaces *old-child* with *new-child* in *sheet*. The children in the
sheet are laid out again appropriately.

See also
        

`add-child`_

`remove-child`_

$right-button
-------------

Constant
''''''''

Summary
       

A constant that represents the right button on the attached pointing
device.

Type
    

*<integer>*
           

Value
     

ash(1, %button\_base + 2)
                         

Description
           

A constant that represents the right button on the attached pointing
device.

See also
        

`$left-button`_

`$middle-button`_

`$pointer-buttons`_

set-caret-position
------------------

Generic function
''''''''''''''''

Summary
       

Sets the position of the specified cursor.

Signature
         

set-cursor-position *cursor x y* => ()
                                      

Arguments
         

-  *cursor* An instance of type `<caret>`_.
-  *x* An instance of type *<real>*.
-  *y* An instance of type *<real>*.

Values
      

None

Description
           

Sets the position of *cursor* to *(* *x* *,* *y* *)*.

See also
        

`caret-position`_

`set-pointer-position`_

set-pointer-position
--------------------

Generic function
''''''''''''''''

Summary
       

Sets the position of the specified pointer.

Signature
         

set-pointer-position *pointer x y* #key *sheet* => ()
                                                     

Arguments
         

-  *pointer* An instance of type `<pointer>`_.
-  *x* An instance of type *<real>*.
-  *y* An instance of type *<real>*.
-  *sheet* An instance of type `<sheet>`_.

Values
      

None

Description
           

Sets the position of *pointer* to *(* *x* *,* *y* *)*, relative to the
top left corner of *sheet*, if specified. Units are measured in pixels.

See also
        

`pointer-position`_

`set-pointer-position`_

set-sheet-edges
---------------

Generic function
''''''''''''''''

Summary
       

Sets the edges of the specified sheet relative to its parent.

Signature
         

set-sheet-edges *sheet left top right bottom* => ()
                                                   

Arguments
         

-  *sheet* An instance of type `<sheet>`_.
-  *left* An instance of type *<integer>*.
-  *top* An instance of type *<integer>*.
-  *right* An instance of type *<integer>*.
-  *bottom* An instance of type *<integer>*.

Values
      

None

Description
           

Sets the edges of *sheet* to *top*, *left*, *right*, and *bottom*.
Each edge is specified relative to the corresponding edge of the parent
of *sheet*. The layout of *sheet* is recalculated automatically.

See also
        

`set-sheet-position`_

`set-sheet-size`_

`sheet-edges`_

set-sheet-position
------------------

Generic function
''''''''''''''''

Summary
       

Sets the position of the specified sheet relative to its parent.

Signature
         

set-sheet-position *sheet x y* => ()
                                    

Arguments
         

-  *sheet* An instance of type `<sheet>`_.
-  *x* An instance of type *<real>*.
-  *y* An instance of type *<real>*.

Values
      

None

Description
           

Sets the position of *sheet* to *(* *x* *,* *y* *)* relative to the
position of its parent. The layout of *sheet* is recalculated
automatically.

See also
        

`set-sheet-edges`_

`set-sheet-size`_

`sheet-position`_

set-sheet-size
--------------

Generic function
''''''''''''''''

Summary
       

Sets the size of the specified sheet.

Signature
         

set-sheet-size *sheet width height* => ()
                                         

Arguments
         

-  *sheet* An instance of type `<sheet>`_.
-  *width* An instance of type *<integer>*.
-  *height* An instance of type *<integer>*.

Values
      

None

Description
           

Sets the size of *sheet*. The layout of *sheet* is recalculated
automatically.

See also
        

`set-sheet-edges`_

`set-sheet-position`_

<sheet>
-------

Open abstract class
'''''''''''''''''''

Summary
       

The base object class for DUIM windows.

Superclasses
            

*<object>*
          

Init-keywords
             

-  *region:* An instance of type `<region> <geom.htm#79228>`_.
   Default value *$nowhere*.
-  *transform:* An instance of type `<transform> <geom.htm#54995>`_.
   Default value *`$identity-transform <geom.htm#70198>`_*.
-  *port:* An instance of type *false-or(* `See
   <port>`_*)*. Default value *#f*.

*style-descriptor:*
                   

-  An instance of type *false-or(style-descriptor)*. Default value *#f*
   .
-  *help-context:* An instance of type *<object-table>*. Default value
   *make(<object-table>)*.
-  *help-source:* An instance of type *<object-table>*. Default value
   *make(<object-table>)*.
-  *parent:* An instance of type *false-or(<sheet>)*. Default value:
   *#f*.
-  *child:* An instance of type *false-or(<sheet>)*. Default value:
   *#f*.
-  *children:* An instance of type *limited(<sequence>, of: <sheet>)*.
   Default value: *#[]*.
-  *x:* An instance of type *<integer>*.
-  *y:* An instance of type *<integer>*.
-  *withdrawn?:* An instance of type *<boolean>*. Default value: *#f*.

*accepts-focus?:*
                 

-  An instance of type *<boolean>*. Default value: *#t*.
-  *cursor:* An instance of type `<cursor>`_.
-  *caret:* An instance of type *type-union(<caret>, one-of(#f, #t))*.
   Default value: *#f*.
-  *foreground:* An instance of type `<ink> <dcs.htm#15007>`_.
-  *background:* An instance of type `<ink> <dcs.htm#15007>`_.
-  *text-style:* An instance of type `See
   <text-style> <dcs.htm#85385>`_.
-  *fixed-width?:* An instance of type *<boolean>*.
-  *fixed-height?:* An instance of type *<boolean>*.
-  *resizable?:* An instance of type *<boolean>*.

Description
           

The *port:* init-keyword is true if the pane (and its mirror, if it has
one) has been mapped, *#f* otherwise. In this case, the term *mapped*
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

If *resizable?:* is *#t* then the sheet can be resized in either
direction. If *resizable?:* is *#f* then it cannot be resized in either
direction. If *resizable?:* is *#t*, but one of *fixed-width?:* or
*fixed-height?:* is *#t*, then the sheet can only be resized in one
direction as appropriate.

Operations
          

The following operations are exported from the *DUIM-Sheets* module.

`add-child`_ `beep`_ `See
child-containing-position`_ `See
children-overlapping-region`_ `See
clear-box`_ ` <graphics.htm#33334>`_ `See
destroy-sheet`_ `display`_

`do-children-containing-position`_ `See
do-children-overlapping-region`_ `See
do-sheet-children`_ `See
do-sheet-tree`_ `See
do-with-drawing-options`_ `See
do-with-pointer-grabbed`_ `See
do-with-sheet-medium`_ `See
do-with-text-style`_ `See
do-with-transform`_

`force-display`_ `See
frame-manager`_

`get-default-background`_ `See
get-default-foreground`_ `See
get-default-text-style`_

`handle-event`_ `See
handle-repaint`_

`medium-background`_ `See
medium-background-setter`_ `See
medium-brush`_ `See
medium-brush-setter`_ `See
medium-clipping-region`_ `See
medium-clipping-region-setter`_ `See
medium-default-text-style`_ `See
medium-default-text-style-setter`_ `See
medium-foreground`_ `See
medium-foreground-setter`_ `See
medium-pen`_ `See
medium-pen-setter`_ `See
medium-text-style`_
 `medium-text-style-setter`_ `See
medium-transform`_ `See
medium-transform-setter`_ `See
port`_

`queue-event`_ `See
queue-repaint`_

`raise-sheet`_ `See
remove-child`_ `See
repaint-sheet`_ `See
replace-child`_

`set-sheet-edges`_ `See
set-sheet-position`_ `See
set-sheet-size`_ `sheet?`_

`sheet-ancestor?`_ `See
sheet-child`_ `See
sheet-children`_ `See
sheet-children-setter`_ `See
sheet-child-setter`_ `See
sheet-edges`_ `sheet-frame`_
`sheet-mapped?`_ `See
sheet-mapped?-setter`_ `See
sheet-medium`_
 `sheet-parent`_ `See
sheet-parent-setter`_ `See
sheet-position`_ `See
sheet-region`_ `See
sheet-region-setter`_ `See
sheet-size`_ `sheet-state`_
`sheet-transform`_ `See
sheet-transform-setter`_ ` <gadgets.htm#94297>`_
` <gadgets.htm#63140>`_ `sheet-withdrawn?`_

`synchronize-display`_ `See
text-size`_ `top-level-sheet`_

The following operations are exported from the *DUIM-Gadgets* module.

` <gadgets.htm#50508>`_ ` <gadgets.htm#55381>`_

The following operations are exported from the *DUIM-Layouts* module.

` <layouts.htm#93434>`_ ` <layouts.htm#58068>`_

` <layouts.htm#78360>`_ ` <layouts.htm#46699>`_

` <layouts.htm#49008>`_ ` <layouts.htm#15793>`_

` <layouts.htm#25115>`_ ` <layouts.htm#38366>`_ ` <layouts.htm#82762>`_
` <layouts.htm#23614>`_ ` <layouts.htm#67986>`_
 ` <layouts.htm#52993>`_

The following operations are exported from the *DUIM-Frames* module.

` <frames.htm#48986>`_

The following operations are exported from the *DUIM-Graphics* module.

` <graphics.htm#49050>`_ ` <graphics.htm#33456>`_
` <graphics.htm#93626>`_ ` <graphics.htm#62343>`_
` <graphics.htm#33334>`_ ` <graphics.htm#69389>`_
` <graphics.htm#62628>`_ ` <graphics.htm#13339>`_
` <graphics.htm#64653>`_ ` <graphics.htm#67791>`_
` <graphics.htm#73313>`_ ` <graphics.htm#15200>`_
` <graphics.htm#82363>`_ ` <graphics.htm#77171>`_
` <graphics.htm#85474>`_ ` <graphics.htm#84651>`_
` <graphics.htm#17442>`_ ` <graphics.htm#17265>`_
` <graphics.htm#38731>`_ ` <graphics.htm#92428>`_
` <graphics.htm#83384>`_ ` <graphics.htm#22290>`_
` <graphics.htm#88171>`_ ` <graphics.htm#37942>`_

The following operations are exported from the *DUIM-DCS* module.

`default-background <dcs.htm#19900>`_ `See
default-foreground <dcs.htm#40602>`_ `See
default-text-style <dcs.htm#95321>`_

The following operations are exported from the *DUIM-Geometry* module.

`box-edges <geom.htm#52858>`_

The following operations are exported from the *DUIM-Extended-Geometry*
module.

`draw-design <ext-geom.htm#88093>`_

Examples
        

To make a text editor that is fixed at 10 lines high:

make(<text-editor>, lines: 10, fixed-height?: #t);
                                                  

See also
        

`<display>`_

`<port>`_

sheet?
------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified object is a sheet.

Signature
         

sheet? *object* => *boolean*
                            

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns true if *object* is a sheet.

See also
        

`medium?`_

sheet-ancestor?
---------------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified sheet has the specified ancestor.

Signature
         

sheet-ancestor? *sheet putative-ancestor* => *boolean*
                                                      

Arguments
         

-  *sheet* An instance of type `<sheet>`_.
-  *putative-ancestor* An instance of type `See
   <sheet>`_.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns true if *putative-ancestor* is an ancestor of *sheet*.

See also
        

`sheet?`_

sheet-child
-----------

Generic function
''''''''''''''''

Summary
       

Returns the child of the specified sheet.

Signature
         

sheet-child *sheet* => *child*
                              

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *child* An instance of type *false-or(* `See
   <sheet>`_*)*.

Description
           

Returns the child of *sheet*.

See also
        

`sheet-children`_

`sheet-child-setter`_

sheet-children
--------------

Generic function
''''''''''''''''

Summary
       

Returns a list of sheets that are the children of the specified sheet.

Signature
         

sheet-children *sheet* => *sheets*
                                  

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *sheets* An instance of type *limited(<sequence>, of:* `See
   <sheet>`_*)*.

Description
           

Returns a list of sheets that are the children of *sheet*. Some sheet
classes support only a single child; in this case, the return value of
sheet-children is a list of one element.

See also
        

`do-sheet-children`_

`sheet-child`_

`sheet-children-setter`_

sheet-children-setter
---------------------

Generic function
''''''''''''''''

Summary
       

Sets the children of the specified sheet.

Signature
         

sheet-children-setter *children sheet* => *sheets*
                                                  

Arguments
         

-  *children* An instance of type *limited(<sequence>, of:* `See
   <sheet>`_*)*.
-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *children* An instance of type *limited(<sequence>, of:* `See
   <sheet>`_*)*.

Description
           

Sets the children of *sheet*. Some sheet classes support only a single
child; in this case, *children* is a list of one element.

See also
        

`sheet-children`_

`sheet-child-setter`_

sheet-child-setter
------------------

Generic function
''''''''''''''''

Summary
       

Sets the child of the specified sheet.

Signature
         

sheet-child-setter *child sheet* => *child*
                                           

Arguments
         

-  *child* An instance of type `<sheet>`_.
-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *child* An instance of type *false-or(* `See
   <sheet>`_*)*.

Description
           

Sets the child of *sheet*.

See also
        

`sheet-child`_

`sheet-children-setter`_

sheet-edges
-----------

Generic function
''''''''''''''''

Summary
       

Returns the edges of the specified sheet, relative to its parent.

Signature
         

sheet-edges *sheet* => *left top right bottom*
                                              

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *left* An instance of type *<coordinate>*.
-  *top* An instance of type *<coordinate>*.
-  *right* An instance of type *<coordinate>*.
-  *bottom* An instance of type *<coordinate>*.

Description
           

Returns the edges of *sheet*. Each edge is specified relative to the
corresponding edge of the parent of *sheet*.

See also
        

`set-sheet-edges`_

`sheet-position`_

`sheet-size`_

`sheet-transform`_

<sheet-event>
-------------

Open abstract class
'''''''''''''''''''

Summary
       

The class of events that can occur in sheets.

Superclasses
            

`<event>`_
                                 

Init-keywords
             

-  *sheet:* An instance of type *false-or(`See
   <sheet>`_)*. Required.

Description
           

The class of events that can occur in sheets.

The required init-keyword *sheet:* specifies a sheet in which the event
occurs.

Operations
          

The following operation is exported from the *DUIM-Sheets* module.

-  `event-sheet`_

See also
        

`<device-event>`_

sheet-event-mask
----------------

Generic function
''''''''''''''''

Summary
       

Returns the event mask of the specified sheet.

Signature
         

sheet-event-mask *sheet* => *integer*
                                     

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *integer* An instance of type *<integer>*.

Description
           

Returns the event mask of *sheet*.

See also
        

`sheet-event-mask-setter`_

`sheet-event-queue`_

sheet-event-mask-setter
-----------------------

Generic function
''''''''''''''''

Summary
       

Sets the event mask of the specified sheet.

Signature
         

sheet-event-mask-setter *mask sheet* => *mask*
                                              

Arguments
         

-  *mask* An instance of type *<integer>*.
-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *mask* An instance of type *<integer>*.

Description
           

Sets the event mask of *sheet*.

See also
        

`sheet-event-mask`_

sheet-event-queue
-----------------

Generic function
''''''''''''''''

Summary
       

Returns the event queue of the specified sheet.

Signature
         

sheet-event-queue *sheet* => *event-queue*
                                          

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *event-queue* An instance of type *<event-queue>*.

Description
           

Returns the event mask of *sheet*. This is a list of all the events
that are currently queued ready for execution.

See also
        

`sheet-event-mask`_

sheet-frame
-----------

Generic function
''''''''''''''''

Summary
       

Returns the frame associated with the specified sheet.

Signature
         

sheet-frame *sheet* => *frame*
                              

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *frame* An instance of type *false-or(` <frames.htm#16922>`_)*.

Description
           

Returns the frame associated with *sheet*.

See also
        

`sheet-medium`_

`sheet-parent`_

sheet-mapped?
-------------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified sheet is mapped.

Signature
         

sheet-mapped? *sheet* => *mapped?*
                                  

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *mapped?* An instance of type *<boolean>*.

Description
           

Returns true if *sheet* is mapped, that is, displayed on screen (issues
of occluding windows notwithstanding).

See also
        

`sheet-mapped?-setter`_

`sheet-withdrawn?`_

sheet-mapped?-setter
--------------------

Generic function
''''''''''''''''

Summary
       

Specifies whether the specified sheet is mapped.

Signature
         

sheet-mapped?-setter *mapped? sheet* => *boolean*
                                                 

Arguments
         

-  *mapped?* An instance of type *<boolean>*.
-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Specifies whether *sheet* is mapped, that is, displayed on screen
(issues of occluding windows notwithstanding). If *#t*, *sheet* is
mapped, if *#f*, it is not.

See also
        

`sheet-mapped?`_

sheet-medium
------------

Generic function
''''''''''''''''

Summary
       

Returns the medium associated with the specified sheet.

Signature
         

sheet-medium *sheet* => *medium*
                                

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *medium* An instance of type *false-or(`See
   <medium>`_)*.

Description
           

Returns the medium associated with *sheet*.

See also
        

`sheet-frame`_

sheet-parent
------------

Generic function
''''''''''''''''

Summary
       

Returns the parent of the specified sheet.

Signature
         

sheet-parent *sheet* => *parent*
                                

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *parent* An instance of type *false-or(* `See
   <sheet>`_*)*.

Description
           

Returns the parent of *sheet*.

See also
        

`sheet-medium`_

`sheet-parent-setter`_

`sheet-position`_

sheet-parent-setter
-------------------

Generic function
''''''''''''''''

Summary
       

Sets the parent of the specified sheet.

Signature
         

sheet-parent-setter *parent sheet* => *value*
                                             

Arguments
         

-  *parent* An instance of type *false-or(* `See
   <sheet>`_*)*.
-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *value* An instance of type *false-or(* `See
   <sheet>`_*)*.

Description
           

Sets the parent of *sheet*.

See also
        

`sheet-parent`_

sheet-pointer-cursor
--------------------

Generic function
''''''''''''''''

Summary
       

Returns the pointer cursor associated with the specified sheet.

Signature
         

sheet-pointer-cursor *sheet* => *cursor*
                                        

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *cursor* An instance of type `<cursor>`_.

Description
           

Returns the pointer cursor associated with *sheet*. This is the cursor
used to represent the mouse pointer whenever the mouse pointer is inside
the boundary of *sheet*.

See also
        

`sheet-pointer-cursor-setter`_

`sheet-text-cursor`_

sheet-pointer-cursor-setter
---------------------------

Generic function
''''''''''''''''

Summary
       

Sets the pointer cursor associated with the specified sheet.

Signature
         

sheet-pointer-cursor-setter *cursor sheet* => *cursor*
                                                      

Arguments
         

-  *cursor* An instance of type `<cursor>`_.
-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *cursor* An instance of type `<cursor>`_.

Description
           

Sets the pointer cursor associated with *sheet*. This is the cursor
used to represent the mouse pointer whenever the mouse pointer is inside
the boundary of *sheet*.

See also
        

`sheet-pointer-cursor`_

sheet-position
--------------

Generic function
''''''''''''''''

Summary
       

Returns the position of the specified sheet relative to its parent.

Signature
         

sheet-position *sheet* => *x y*
                               

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *x* An instance of type *<real>*.
-  *y* An instance of type *<real>*.

Description
           

Returns the position of *sheet*. The position is represented by the
coordinate (x,y), as measured relative to the parent of *sheet*, or
relative to the top left of the screen if *sheet* has no parent.

See also
        

`set-sheet-position`_

`sheet-edges`_

`sheet-parent`_

`sheet-size`_

`sheet-transform`_

sheet-region
------------

Generic function
''''''''''''''''

Summary
       

Returns the region associated with the specified sheet.

Signature
         

sheet-region *sheet* => *region*
                                

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *region* An instance of type `<region> <geom.htm#79228>`_.

Description
           

Returns an instance of `<region> <geom.htm#79228>`_ that represents
the set of points to which *sheet* refers. The region is expressed in
the same coordinate system as *sheet*.

See also
        

`sheet-region-setter`_

sheet-region-setter
-------------------

Generic function
''''''''''''''''

Summary
       

Sets the region associated with the specified sheet.

Signature
         

sheet-region-setter *region sheet* => *region*
                                              

Arguments
         

-  *region* An instance of type `<region> <geom.htm#79228>`_.
-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *region* An instance of type `<region> <geom.htm#79228>`_.

Description
           

Creates or modifies an instance of `<region> <geom.htm#79228>`_ that
represents the set of points to which *sheet* refers. The region is
expressed in the same coordinate system as *sheet*.

See also
        

`sheet-region`_

sheet-size
----------

Generic function
''''''''''''''''

Summary
       

Returns the width and height of the specified sheet.

Signature
         

sheet-size *sheet* => *width height*
                                    

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *width* An instance of type *<integer>*.
-  *height* An instance of type *<integer>*.

Description
           

Returns the width and height of the specified sheet. Use `See
set-sheet-size`_ to set or modify the size of a
sheet.

See also
        

`set-sheet-size`_

`sheet-edges`_

`sheet-position`_

`sheet-transform`_

sheet-state
-----------

Generic function
''''''''''''''''

Summary
       

Returns the current state of the specified sheet.

Signature
         

sheet-state *sheet* => *value*
                              

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *value* An instance of type *one-of(#"withdrawn", #"managed",
   #"mapped", #"unknown")*.

Description
           

Returns the current state of *sheet*. The state of a sheet tells you
whether the sheet is currently mapped on screen, or whether it has been
withdrawn from the list of sheets.

sheet-text-cursor
-----------------

Generic function
''''''''''''''''

Summary
       

Returns the text cursor associated with the specified sheet.

Signature
         

sheet-text-cursor *sheet* => *text-cursor*
                                          

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *text-cursor* An instance of type *false-or(`See
   <cursor>`_)*.

Description
           

Returns the text cursor associated with *sheet*. The text cursor
associated with a sheet is distinct from the pointer cursor associated
with the same sheet: the pointer cursor represents the current position
of the pointer associated with the attached pointer device, while the
text cursor represents the position in the sheet that any text typed
using the keyboard will be added. Only those sheets that contain
children that allow some form of text-based input have an associated
text cursor.

See also
        

`sheet-pointer-cursor`_

sheet-transform
---------------

Generic function
''''''''''''''''

Summary
       

Returns the transform associated with the specified sheet.

Signature
         

sheet-transform *sheet* => *transform*
                                      

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *transform* An instance of type `<transform> <geom.htm#54995>`_.

Description
           

Returns the transform associated with *sheet*.

See also
        

`medium-transform`_

`sheet-edges`_

`sheet-position`_

`sheet-size`_

sheet-transform-setter
----------------------

Generic function
''''''''''''''''

Summary
       

Sets the transform associated with the specified sheet.

Signature
         

sheet-transform-setter *transform sheet* => *transform*
                                                       

Arguments
         

-  *transform* An instance of type `<transform> <geom.htm#54995>`_.
-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *transform* An instance of type `<transform> <geom.htm#54995>`_.

Description
           

Sets or modifies the transform associated with *sheet*.

See also
        

`medium-transform-setter`_

sheet-withdrawn?
----------------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified sheet has been withdrawn from the display.

Signature
         

sheet-withdrawn? *sheet* => *withdrawn?*
                                        

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  *withdrawn?* An instance of type *<boolean>*.

Description
           

Returns true if *sheet* has been withdrawn from the display, and is no
longer mapped.

See also
        

`sheet-mapped?`_

$shift-key
----------

Constant
''''''''

Summary
       

A constant that represents the SHIFT key on the keyboard.

Type
    

*<integer>*
           

Value
     

ash(1, %modifier\_base + 0);
                            

Description
           

A constant that represents the SHIFT key on the keyboard.

See also
        

`$alt-key`_

`$control-key`_

`$hyper-key`_

`$meta-key`_

`modifier-key-index`_

`modifier-key-index-name`_

`$modifier-keys`_

`$option-key`_

`$super-key`_

$super-key
----------

Constant
''''''''

Summary
       

A constant that represents the SUPER key on the keyboard.

Type
    

*<integer>*
           

Value
     

ash(1, %modifier\_base + 3);
                            

Description
           

A constant that represents the SUPER key on the keyboard, if it exists.
To deal with the case where there is no SUPER key, the value of the
constant `$option-key`_ is bound to this
constant.

See also
        

`$alt-key`_

`$control-key`_

`$hyper-key`_

`$meta-key`_

`modifier-key-index`_

`modifier-key-index-name`_

`$modifier-keys`_

`$option-key`_

`$shift-key`_

synchronize-display
-------------------

Generic function
''''''''''''''''

Summary
       

Synchronizes all displays on which the specified drawable is mapped.

Signature
         

synchronize-display *drawable* => ()
                                    

Arguments
         

-  *drawable* An instance of type *type-union(* `See
   <sheet>`_*, `<medium>`_)*.

Values
      

None

Description
           

Synchronizes all displays on which the specified drawable is mapped.

text-size
---------

Generic function
''''''''''''''''

Summary
       

Returns information about the size of the specified text on the
specified medium.

Signature
         

text-size *medium text* #key *text-style start end do-newlines?* =>
*largest-x largest-y cursor-x cursor-y baseline*
                                                                                                                    

Arguments
         

-  *medium* An instance of type *`<medium>`_*.
-  *text* An instance of type *type-union(<string>, <character>)*.
-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.
-  *start* An instance of type *<integer>*. Default value: 0.
-  *end* An instance of type *<integer>*. Default value: *size(* *text*
   *)*.
-  *do-newlines?* An instance of type *<boolean>*. Default value: *#f*
   .
-  *do-tabs?* An instance of type *<boolean>*. Default value: *#f*.

Values
      

-  *largest-x* An instance of type *<integer>*.
-  *total-height* An instance of type *<integer>*.
-  *last-x* An instance of type *<integer>*.
-  *last-y* An instance of type *<integer>*.
-  *baseline* An instance of type *<integer>*.

Description
           

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

text-style-mapping
------------------

Generic function
''''''''''''''''

Summary
       

Returns the mapping for the specified text style on the specified port.

Signature
         

text-style-mapping *port text-style* #key *character-set* => *font*
                                                                   

Arguments
         

-  *port* An instance of type `<port>`_.
-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.
-  *character-set* An instance of type *<object>*.

Values
      

-  *font* An instance of type *<object>*.

Description
           

Returns the mapping for *text-style* on *port*. Mapping text styles
onto fonts lets you control how different text styles are displayed on
different servers, depending on the connection. For instance, it is
possible to define how colored text is displayed on monochrome displays,
or how fonts specified by *text-style* are mapped onto fonts available
on the display.

If *character-set* is specified, then this character set is used instead
of the default. This is most useful for non-English displays.

See also
        

`text-style-mapping-exists?`_

`text-style-mapping-setter`_

`<undefined-text-style-mapping>`_

text-style-mapping-exists?
--------------------------

Generic function
''''''''''''''''

Summary
       

Returns true if a mapping exists for the specified text style on the
specified port.

Signature
         

text-style-mapping-exists? *port text-style* #key *character-set
exact-size?* => *boolean*
                                                                                          

Arguments
         

-  *port* An instance of type `<port>`_.
-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.
-  *character-set* An instance of type *<object>*.
-  *exact-size?* An instance of type *<boolean>*. Default value: *#f*.

Values
      

-  *boolean* An instance of type *<boolean>*.

Description
           

Returns true if a mapping exists for *text-style* on *port*. This
control function is useful if, for example, you are setting up text
style mappings for a range of text styles in one go, or for a range of
different ports. Using this function, you can test for the existence of
a previous mapping before creating a new one, thereby ensuring that
existing mappings are not overwritten.

See also
        

`text-style-mapping`_

`text-style-mapping-setter`_

`<undefined-text-style-mapping>`_

text-style-mapping-setter
-------------------------

Generic function
''''''''''''''''

Summary
       

Sets the mapping for the specified text style on the specified port.

Signature
         

text-style-mapping-setter *font port text-style* #key *character-set* =>
*font*
                                                                               

Arguments
         

-  *font* An instance of type *<object>*.
-  *port* An instance of type `<port>`_.
-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.
-  *character-set* An instance of type *<object>*.

Values
      

-  *font* An instance of type *<object>*.

Description
           

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
        

`text-style-mapping`_

`text-style-mapping-exists?`_

`<undefined-text-style-mapping>`_

<timer-event>
-------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of timed events.

Superclasses
            

`<frame-event>`_
                                       

Init-keywords
             

None.

Description
           

The class of timed events.

Operations
          

-  None.

top-level-sheet
---------------

Generic function
''''''''''''''''

Summary
       

Returns the top level sheet for the specified object.

Signature
         

top-level-sheet *object* => *top-level-sheet*
                                             

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *top-level-sheet* An instance of type *false-or(* `See
   <sheet>`_*)*.

Description
           

Returns the top level sheet for *object*. This is the sheet that has as
its descendents all of the panes of *object*.

<undefined-text-style-mapping>
------------------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of undefined text style mappings.

Superclasses
            

*<error>*
         

Init-keywords
             

-  *port:* An instance of type `<port>`_.
   Required.
-  *text-style:* An instance of type `See
   <text-style> <dcs.htm#85385>`_. Required.

Description
           

The class of undefined text style mappings. This class is used for any
text styles that have not had mappings defined for a given port.

Operations
          

-  None.

See also
        

`text-style-mapping`_

`text-style-mapping-exists?`_

`text-style-mapping-setter`_

<window-configuration-event>
----------------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events involving changes to the window configuration.

Superclasses
            

`<window-event>`_
                                        

Init-keywords
             

None.

Description
           

The class of events involving changes to the window configuration.

Operations
          

-  None.

See also
        

`<window-repaint-event>`_

<window-event>
--------------

Open abstract class
'''''''''''''''''''

Summary
       

The base class of events that occur in windows.

Superclasses
            

<sheet-event>
             

Init-keywords
             

-  *region:* An instance of type `<region> <geom.htm#79228>`_.
   Required.

Description
           

The base class of events that occur in windows. Two types of event can
occur:

-  Changes to the configuration of the window.
-  Changes that require the window to be repainted.

The *region:* init-keyword specifies a region in which the event occurs.

Operations
          

The following operation is exported from the *DUIM-Sheets* module.

-  `event-region`_

See also
        

`event-region`_

`<window-configuration-event>`_

`<window-repaint-event>`_

<window-repaint-event>
----------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events involving repainting of a window.

Superclasses
            

`<window-event>`_
                                        

Init-keywords
             

None.

Description
           

The class of events involving repainting of a window.

Operations
          

-  None.

See also
        

`handle-repaint`_

`queue-repaint`_

`repaint-sheet`_

`<window-configuration-event>`_

with-brush
----------

Statement macro
'''''''''''''''

Summary
       

Executes the supplied code using the specified brush characteristics.

Macro call
          

with-brush ({*medium* } #rest {*brush-initargs* }\*) {*body* } end
                                                                  

Arguments
         

-  *medium* A Dylan body*bnf*.
-  *brush-initargs* Dylan arguments*bnf*.
-  *body* A Dylan body*bnf*.

Values
      

-  None.

Description
           

Executes *body* using the brush characteristics specified by
*brush-initargs*, and applies the results to *medium*. The *medium*
specified should be an instance of type `See
<medium>`_. The *brush-initargs* can be any valid
arguments that specify an instance of `<brush> <dcs.htm#29492>`_.

See also
        

`with-pen`_

with-clipboard
--------------

Statement macro
'''''''''''''''

Summary
       

Evaluates a body of code with a clipboard grabbed.

Signature
         

with-clipboard (*clipboard* = *sheet*) *body* end
                                                  

Arguments
         

-  *clipboard* A Dylan variable-name*bnf*.
-  *sheet* A Dylan variable-name*bnf*.
-  *body* A Dylan body*bnf*.

Values
      

-  *values* Instances of *<object>*.

Description
           

Evaluates *body* with the clipboard grabbed, returning the results to
the clipboard.

The macro grabs a lock on the clipboard, using *open-clipboard*, and
then executes *body*. Once the results of evaluating *body* have been
sent to the clipboard, the clipboard lock is freed using `See
close-clipboard`_. The *clipboard* argument is a
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
        

`<clipboard>`_

with-clipping-region
--------------------

Statement macro
'''''''''''''''

Summary
       

Executes the supplied code using the specified clipping region.

Macro call
          

with-clipping-region ({*medium* } {*region* }) {*body* } end
                                                            

Arguments
         

-  *medium* A Dylan expression*bnf*.
-  *region* A Dylan expression*bnf*.
-  *body* A Dylan body*bnf*.

Values
      

-  None.

Description
           

Executes *body* using the clipping region specified by *region*, and
applies the results to *medium*. The *region* and *medium* expressions
should evaluate to instances of *`<region> <geom.htm#79228>`_* and
*`<medium>`_*, respectively.

with-cursor-visible
-------------------

Statement macro
'''''''''''''''

Summary
       

Executes the supplied code using the specified cursor settings for a
sheet.

Macro call
          

with-cursor-visible ({*sheet* } {*visible?* }) {*body* } end
                                                            

Arguments
         

-  *sheet* A Dylan expression*bnf*.
-  *visible?* A Dylan expression*bnf*.
-  *body* A Dylan body*bnf*.

Values
      

-  None.

Description
           

Executes *body* on the specified *sheet*. If *visible?* is true, then
the pointer cursor associated with *sheet* is visible throughout the
operation. If *visible?* is false, then the pointer cursor is hidden.

The expression *sheet* should evaluate to an instance of `See
<sheet>`_. The expression *visible?* should evaluate
to a boolean value.

with-drawing-options
--------------------

Statement macro
'''''''''''''''

Summary
       

Runs a body of code in the context of a set of drawing options.

Macro call
          

with-drawing-options ({*medium* } #rest {*options* }\*) {*body* } end
                                                                     

Arguments
         

-  *medium* A Dylan expression*bnf*.
-  *options* Dylan arguments*bnf*.
-  *body* A Dylan body*bnf*.

Values
      

-  None.

Description
           

Runs a body of code in the context of a set of drawing options. The
options specified are passed to the function `See
do-with-drawing-options`_ for execution.

The *medium* expression should evaluate to an instance of *`See
<medium>`_*.

Note that when using *with-drawing-options* in conjunction with a loop.
it is computationally much quicker to use a medium (as shown here)
rather than a sheet, and to place the call to with-drawing-options
outside the loop. If necessary, use `See
with-sheet-medium`_ to associate the sheet with the
medium, thus:

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
       

with-drawing-options (medium, brush: $red)
                                          

draw-rectangle (medium, 0, 0, 100, 200, filled?: #t)

end;
    

See also
        

`do-with-drawing-options`_

`with-sheet-medium`_

withdraw-sheet
--------------

Generic function
''''''''''''''''

Summary
       

Withdraws the specified sheet from the current display.

Signature
         

withdraw-sheet *sheet* => ()
                            

Arguments
         

-  *sheet* An instance of type `<sheet>`_.

Values
      

-  None.

Description
           

Withdraws the specified sheet from the current display.

with-frame-manager
------------------

Statement macro
'''''''''''''''

Summary
       

Executes the supplied code in the context of the specified frame
manager.

Macro call
          

with-frame-manager ({*framem* }) {*body* } end
                                              

Arguments
         

-  *framem* A Dylan expression*bnf*.
-  *body* A Dylan body*bnf*.

Values
      

-  None.

Description
           

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
        

`<frame-manager>`_

with-identity-transform
-----------------------

Statement macro
'''''''''''''''

Summary
       

Executes the supplied code while retaining the current transform.

Macro call
          

with-identity-transform ({*medium* }) {*body* } end
                                                   

Arguments
         

-  *medium* A Dylan expression*bnf*.
-  *body* A Dylan body*bnf*.

Values
      

-  None.

Description
           

Executes *body* while retaining the current transform for *medium*.

The *medium* expression should evaluate to an instance of *`See
<medium>`_*.

with-pen
--------

Statement macro
'''''''''''''''

Summary
       

Executes the supplied code using the specified pen characteristics.

Macro call
          

with-pen ({*medium* } #rest {*pen-initargs* }\*) {*body* } end
                                                              

Arguments
         

-  *medium* A Dylan expression*bnf*.
-  *pen-initargs* Dylan arguments*bnf*.
-  *body* A Dylan body*bnf*.

Values
      

-  None.

Description
           

Executes *body* using the pen characteristics specified by
*pen-initargs*, and applies the results to the expression *medium*.

The *medium* specified should be an instance of type `See
<medium>`_. The *pen-initargs* can be any valid
arguments that specify an instance of `<pen> <dcs.htm#41757>`_.

See also
        

`with-brush`_

with-pointer-grabbed
--------------------

Statement macro
'''''''''''''''

Summary
       

Executes a body of code, forwarding all pointer events to a sheet.

Macro call
          

with-pointer-grabbed ({*sheet* } #rest {*options* }\*) {*body* } end
                                                                    

Arguments
         

-  *sheet* A Dylan expression*bnf*.
-  *options* Dylan arguments*bnf*.
-  *body* A Dylan body*bnf*.

Values
      

-  None.

Description
           

Executes a body of code, forwarding all pointer events to *sheet*, even
if the pointer leaves the sheet-region of *sheet*. The *sheet*
specified should be an instance of type *<sheet>*.

The macro calls methods for *do-with-pointer-grabbed*. The code
specified by *body* is used to create a stand-alone method that is used
as the code that is run by *do-with-pointer-grabbed*.

See also
        

`do-with-pointer-grabbed`_

with-rotation
-------------

Statement macro
'''''''''''''''

Summary
       

Executes a body of code with a specified rotation.

Macro call
          

with-rotation ({*medium* } {*angle* }) {*body* } end
                                                    

Arguments
         

-  *medium* A Dylan expression*bnf*.
-  *angle* A Dylan argument*bnf*.
-  *body* A Dylan body*bnf*.

Values
      

-  None.

Description
           

Executes a body of code with a specified rotation. The rotation occurs
within the expression *medium*. This macro calls `See
with-transform`_ to perform the rotation.

The *medium* specified should be an instance of type `See
<medium>`_. The *angle* should evaluate to an
instance of type *<real>*.

See also
        

`with-scaling`_

`with-transform`_

`with-translation`_

with-scaling
------------

Statement macro
'''''''''''''''

Summary
       

Executes a body of code with a specified scaling.

Macro call
          

with-scaling ({*medium* } {*scale-x* } {*scale-y* }) {*body* } end
                                                                  

Arguments
         

-  *medium* A Dylan expression*bnf*.
-  *scale-x* A Dylan argument*bnf*.
-  *scale-y* A Dylan argument*bnf*.
-  *body* A Dylan body*bnf*.

Values
      

-  None.

Description
           

Executes a body of code with a specified scaling, denoted by *scale-x*
and *scale-y*. The scaling occurs within the expression *medium*. This
macro calls `with-transform`_ to perform the
scaling.

The *medium* specified should be an instance of type `See
<medium>`_. The *scale-x* and*scale-y* should
evaluate to an instance of type *<real>*.

See also
        

`with-rotation`_

`with-transform`_

`with-translation`_

with-sheet-medium
-----------------

Statement macro
'''''''''''''''

Summary
       

Associates a sheet with a medium.

Macro call
          

with-sheet-medium ({*medium* = *sheet* }) {*body* } end
                                                       

Arguments
         

-  *medium* A Dylan name*bnf*.
-  *sheet* A Dylan expression*bnf*.
-  *body* A Dylan body*bnf*.

Values
      

-  None.

Description
           

Associates a sheet with a medium.

Within *body*, the variable *medium* is bound to the medium allocated
to *sheet*. The *sheet* specified should be an instance of type `See
<sheet>`_. If *sheet* does not have a medium
permanently allocated, one is allocated and associated with *sheet* for
the duration of *body*, and then unassociated from *sheet* and
deallocated when *body* has been exited. The values of the last form of
*body* are returned as the values of *with-sheet-medium*.

The *medium* argument is not evaluated, and must be a symbol that is
bound to a medium. The *body* may have zero or more declarations as its
first forms.

This macro is a useful way of speeding up drawing operations, since
drawing on a sheet requires finding the medium for that sheet. You can
use *with-sheet-medium* to associate a known sheet with a medium, and
then draw directly onto that medium, as shown in the example.

Example
       

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
        

`do-with-sheet-medium`_

`with-drawing-options`_

with-text-style
---------------

Statement macro
'''''''''''''''

Summary
       

Runs a body of code in the context of a text style.

Macro call
          

with-text-style ({*medium* } #rest {*style-initargs* }\*) {*body* } end
                                                                       

Arguments
         

-  *medium* A Dylan expression*bnf*.
-  *style-initargs* Dylan arguments*bnf*.
-  *body* A Dylan body*bnf*.

Values
      

-  None.

Description
           

Executes *body* using the text style characteristics specified by
*style-initargs*, and applies the results to *medium*.

The *medium* specified should be an instance of type `See
<medium>`_. The *style-initargs* can be any valid
arguments that specify an instance of `See
<text-style> <dcs.htm#85385>`_.

Methods for `do-with-text-style`_ are invoked to
run the code.

See also
        

`do-with-text-style`_

with-transform
--------------

Statement macro
'''''''''''''''

Summary
       

Executes a body of code with a specified transform.

Macro call
          

with-transform ({*medium* } {*transform* }) {*body* } end
                                                         

Arguments
         

-  *medium* A Dylan expression*bnf*.
-  *transform* A Dylan expression*bnf*.
-  *body* A Dylan body*bnf*.

Values
      

-  None.

Description
           

Executes a body of code with a specified *transform*. The transform
occurs within *medium*. This macro is used by `See
with-rotation`_, `See
with-scaling`_, and `See
with-translation`_, and calls methods for `See
do-with-transform`_.

The *medium* specified should be an instance of type `See
<medium>`_. The *transform* specified should be an
instance of type `<transform> <geom.htm#33417>`_.

See also
        

`do-with-transform`_

`with-rotation`_

`with-scaling`_

`with-translation`_

with-translation
----------------

Statement macro
'''''''''''''''

Summary
       

Executes a body of code with a specified translation.

Macro call
          

with-translation ({*medium* } {*dx* } {*dy* }) {*body* } end
                                                            

Arguments
         

-  *medium* A Dylan expression*bnf*.
-  *dx* A Dylan argument*bnf*.
-  *dy* A Dylan argument*bnf*.
-  *body* A Dylan body*bnf*.

Values
      

-  None.

Description
           

Executes a body of code with a specified translation, denoted by *dx*
and *dy*. The translation occurs within *medium*. This macro calls
`with-transform`_ to perform the translation.

The *medium* specified should be an instance of type `See
<medium>`_. The *dx* and*dy* should evaluate to an
instance of type *<real>*.

See also
        

`with-rotation`_

`with-scaling`_

`with-transform`_


