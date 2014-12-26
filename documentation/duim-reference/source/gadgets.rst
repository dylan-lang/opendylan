********************
DUIM-Gadgets Library
********************

.. current-library:: duim
.. current-module:: duim-gadgets

Overview
========

The elements that comprise a Graphical User Interface (GUI) are arranged
in a hierarchical ordering of object classes. At the top level of the
DUIM hierarchy there are three main classes, :class:`<sheet>`,
:class:`<gadget>`, and :class:`<frame>`, all of which are subclasses of
:drm:`<object>`.

The DUIM-Gadgets library contains classes that define a wide variety of
gadgets for use in your GUI applications, such as push buttons, radio
buttons, and check boxes. The library also provides the necessary
functions, generic functions, and macros for creating and manipulating
these classes. The library contains a single module, *duim-gadgets*,
from which all the interfaces described in this chapter are exposed.
`DUIM-Gadgets Module`_ contains complete
reference entries for each exposed interface.

Gadgets are the basic behavioral GUI element (above the level of
events).

-  Gadgets do not *need* to have a visual presence, though in practice
   every gadget provided by DUIM does, since all general instances of
   :class:`<gadget>` are also general instances of :class:`<sheet>`.
-  Many classes of gadget maintain some kind of state for their
   behavior, and in practice some of this is usually reflected in the
   UI. For example, you can tell that a check box is selected just by
   looking at it.
-  They handle events and turn these into callbacks, for convenience.

Some of the more important types of gadget are as follows:

Buttons
   A wide variety of buttons are provided by DUIM. These
   include not only standard buttons such as push buttons and radio
   buttons, but items that can be placed within menus.

Action gadgets
   An action gadget is any gadget that can be used to
   perform an action, such as a button, or menu command.

Value gadgets
   A value gadget is any gadget that can have a value
   associated with it. In principle, this is true of the majority of
   gadgets, but the value of a gadget is more important for certain
   types of gadget (for instance, lists or radio boxes) than for others
   (for instance, push buttons).

Value range gadgets
   Value range gadgets are those value gadgets for which the possible
   value sits within a defined range. This includes gadgets such as
   scroll bars and sliders.

Collection gadgets
   Collection gadgets are those gadgets that can contain a number of
   "child" gadgets, the specification of which can be described in terms
   of a Dylan collection, and includes gadgets such as list controls and
   groups of buttons. Usually, the behavior of each of the "child"
   gadgets is interdependent in some way; for example, only one button
   in a group of radio buttons may be selected at any time. With
   collection gadgets, you can specify the "child" gadgets very simply,
   without having to worry about defining each "child" explicitly.

Each of these types of gadget is described in more detail in subsequent
sections, and full reference entries for every interface exposed in the
DUIM-Gadgets library are available in `DUIM-Gadgets
Module`_. For a more general introduction to the
gadgets provided in DUIM, see the tour in the *Building Applications
using DUIM* book. See the same book for a more practical example of
implementing an application using the DUIM library.

Callbacks and keys
==================

When an event occurs in a user interface (for example, a button is
pressed, a menu command is chosen, or an item in a list is
double-clicked), you usually want some operation to be performed. If the
user of your application chooses the *File > Open* command, a File Open
dialog should be displayed. If the user clicks on an *OK* button in a
dialog, the dialog should be dismissed and the appropriate changes to
the application state to be performed. In DUIM, you can provide this
functionality by specifying a function known as a *callback*.

Generally speaking, a callback gets passed a single argument, which is
the gadget that is affected. Thus, the argument passed to the callback
for a button is the button itself. Callbacks do not need to have a
return value, although they are not forbidden either. If a value is
returned by a callback function, then it is just ignored.

Callbacks are used in preference to event handlers because Dylan does
not let you write methods that specialize on individual instances. In
languages such as C, you uniquely name each element in an interface, and
then provide behavior for each element by writing event handlers that
contain case statements that let you discriminate on individual
elements. This is a somewhat inelegant solution. Instead, in Dylan you
specify the names of the callbacks for each element in an interface when
you *create* the elements. It is then a simple matter for the system to
know what behavior goes with what elements, and is much less tedious
than having to write many cumbersome methods for :gf:`handle-event`.

In Dylan, you use events in order to create new kinds of class. If you
were creating a new kind of button, you would need to define a new
method for :gf:`handle-event` in order to describe what happens when
you click on an instance of that button. You would then write callbacks
to deal with particular instance of the new class of button.

By contrast with callbacks, you can also provide functions in DUIM known
as *keys*, which are specific to collection gadgets. A key is used to
set the value of some aspect of the collection gadget for which the key
is defined. With keys, therefore, the values returned by the function
are fundamental to the operation of the gadget. There are two keys that
are generally used by gadgets, known as the value key and the label key.
The value key is a function that is used to calculate the value of the
gadget for which the key is defined. The label key is used to calculate
the printed representation, or label, of all the items in a collection
gadget.

Gadget protocols
================

Gadgets are objects that make up an interface: the menus, buttons,
sliders, check lists, tool bars, menu bars, and so on. Gadget classes
may support three protocols, *value*, *items*, and *activate*.

-  Gadgets that support the *value* protocol respond to the
   :gf:`gadget-value` message, a value-changed callback, and have a setter
   function associated with them.
-  Gadgets that support the *items* protocol respond to :gf:`gadget-items`
   and have a gadget setter function associated with them.
-  Gadgets that support the *activate* protocol have an activation
   callback associated with them.

Gadgets have a set of slots, or properties, associated with them:
:gf:`gadget-label`, :gf:`gadget-value`, :gf:`gadget-items`, and
:gf:`gadget-enabled?`.  Every gadget has some or all of these properties.

:gf:`gadget-label`
   This slot holds the label that appears on the gadget on the screen.
   If a gadget does not have a label, the :gf:`gadget-label` function
   returns ``#f``.

:gf:`gadget-value`
   This slot holds the value(s) of the gadget. If a gadget does not have
   any values, the :gf:`gadget-value` function returns ``#f``.

:gf:`gadget-items`
   This slot is a list of the contents of the gadget. If the gadget does
   not have items, for example a button, :gf:`gadget-items` returns nothing.

:gf:`gadget-enabled?`
   This slot tests whether or not the gadget is active. All gadgets have
   a :gf:`gadget-enabled?` slot.

An introduction to the protocols supported by different sorts of gadget
can also be found in the *Building Applications using DUIM* book.

The class hierarchy for DUIM-Gadgets
====================================

This section presents an overview of the available classes of gadget,
and describes the class hierarchy present.

In each table below, classes that support the *items* protocol are
displayed in *bold text*, and classes that support the activate
protocol are displayed using *italic text*.

*Note:* In `Subclasses of the <collection-gadget>
class`_, every subclass shown supports the *items*
protocol, though for clarity, no bold is used.

All subclasses of *<value-gadget>* support the *value* protocol. These
are described in `Subclasses of
<value-gadget>`_, `Subclasses of
<button>`_, and `Subclasses of
<collection-gadget>`_.

The <gadget> class and its subclasses
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The base class for the majority of DUIM gadgets is the *<gadget>* class,
which is itself a subclass of *<object>*. All other DUIM gadgets are
subclasses of *<gadget>*, with the exception of *<list-item>*,
*<tree-node>*, and *<table-item>*.

The immediate subclasses of *<gadget>* are shown in `Overall class
hierarchy for the DUIM-Gadgets library`_. Only :class:`<value-gadget>`
and :class:`<page>` have any subclasses defined. See
`Subclasses of <value-gadget>`_ and `Subclasses of <page>`_ for details
of these subclasses.

The :class:`<gadget>` class provides a number of subclasses that allow
particular parts of a user interface to be created:

:class:`<menu>`
   Use this class to add a menu to the menu bar of any application
   frame. Menus themselves contain commands created using the
   menu-specific button and collection gadgets described in
   `Subclasses of <button>`_ and `Subclasses of <collection-gadget>`_.

:class:`<tool-bar>`
   This class is used to add a tool bar to an application frame. A
   tool bar is a row of buttons that duplicates the functionality
   of the most commonly used menu commands, thereby providing the
   user with quick access to the most useful operations in the
   application.

:class:`<scroller>`
   This is a generic scrolling gadget that can be used in a number
   of situations.

:class:`<viewport>`
   A viewport can be used to create a generic pane for displaying
   specialized contents that you may have defined. Use this
   class when there is no other class provided for displaying the
   objects in question.

:class:`<splitter>`
   This class can be used to split the current view in half. This
   allows the user, for example, to create a second view of the
   same document.

The :class:`<gadget>` class provides a number of subclasses that allow
general spatial and grouping capability, in addition to the layout
functionality described in `DUIM-Layouts Library <layouts.htm#21962>`_.
These are as follows:

:class:`<label>`
   This class is used to assign label to many other types of
   gadget. Many gadgets can be assigned one or more labels, usually by
   means of a label: init-keyword. This class is used to assign any
   label.

:class:`<separator>`
   This allows a line to be drawn between any two gadgets
   or groups of gadgets, so as to provide a visible barrier between
   them.

:class:`<spacing>`
   This allows you to specify how much space should be
   placed between any two gadgets or groups of gadgets.

:class:`<border>`
   This allows a visible border to be placed around any number of gadgets.

:class:`<group-box>`
   This allows you to group together any number of related
   gadgets in a frame. Grouped elements are usually displayed with a
   border and label identifying the grouping.

Overall class hierarchy for the DUIM-Gadgets library

<object>

<gadget>

<action-gadget>

<value-gadget>

See `Subclasses of <value-gadget>`_

<label>

:class:`<menu>`

*<tool-bar>*

<scroller>

<separator>

<viewport>

<spacing>

<border>

<group-box>

<splitter>

<page>

See `Subclasses of <page>`_

<list-item>

<tree-node>

<table-item>

Subclasses of <value-gadget>
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Any gadget that can take a value of some sort is a subclass of
:class:`<value-gadget>`. As might be expected, this includes the majority of
the gadgets in the DUIM-Gadgets library.

Every subclass of :class:`<value-gadget>` supports the *value* protocol, as
described in `Overview`_.

Several subclasses of :class:`<value-gadget>` themselves have a number of
subclasses defined. These include:

:class:`<text-gadget>`
   Any gadget into which you can type text. These
   include both text editors (multiple line edit controls) and text
   fields (single line edit controls).

:class:`<value-range-gadget>`
   Value gadgets whose value can vary within a known range, such as
   scroll bars.

:class:`<button>`
   Any button, such as a radio button, check button, or push
   button. See `Subclasses of <button>`_ for
   more details about the classes of button available.

:class:`<collection-gadget>`
   Any gadget whose contents form a collection, such as a list, a tree
   control, or a group of buttons. See `Subclasses of
   <collection-gadget>`_ for more details about the
   classes of collection gadget available.

Also provided are the following specific GUI elements:

:class:`<menu-bar>`
   This used to create the standard menu bar that is
   commonly found across the top of an application frame.

:class:`<status-bar>`
   This is used to create a status bar, usually placed at
   the bottom of an application frame. A status bar is used to display
   miscellaneous information about the current state of the application.

:class:`<tab-control>`
   Tab controls are analogous to dividers in a filing
   cabinet or notebook, with multiple logical pages of information
   displayed within the same window. Clicking on any of the tabs
   displayed in a tab control displays a new page of information.

The subclasses of :class:`<value-gadget>` are as shown in `Subclasses of
the <value-gadget> class`_.

Subclasses of the *<value-gadget>* class

<value-gadget>

<text-gadget>

*<password-field>*

*<text-editor>*

*<text-field>*

<value-range-gadget>

<slider>

<scroll-bar>

<progress-bar>

<button>

See `Subclasses of <button>`_

:class:`<menu-bar>`

<status-bar>

<tab-control>

<collection-gadget>

See `Subclasses of <collection-gadget>`_

Subclasses of <page>
^^^^^^^^^^^^^^^^^^^^

The `<page>` class is the base class of gadgets that are used to display
a whole page of information within a "parent" element, with the page
itself optionally containing other layouts or gadgets. Pages are used in
situations where different sets of information (the pages themselves)
need to be displayed in a common parent.

The subclasses of :class:`<page>` are as shown in `Subclasses of the <page>
class`_.

Subclasses of the *<page>* class

<page>

<tab-control-page>

<property-page>

<wizard-page>

The :class:`<tab-control-page>` class is used to define the elements that are
associated with each tab in a tab control.

.. figure:: images/gadgets-3.png
   :align: center

   A tab control page

The :class:`<property-page>` class performs a similar job for property frames
(visually, a property frame looks like a tab control in a dialog box,
and is one way of implementing a dialog box that has several pages of
information. Property frames are so named because they are often used to
display the user-configurable properties of an application.

.. figure:: images/gadgets-4.png
   :align: center

   A property page

The :class:`<wizard-page>` class is used to define the elements in each
page of a wizard frame. Wizard frames are another form of multi-page
dialog, but consist of several physically distinct windows that are
presented to the user in a strict order.

.. figure:: images/gadgets-5.png
   :align: center

   A tab control page

Subclasses of <button>
^^^^^^^^^^^^^^^^^^^^^^

The subclasses of :class:`<button>` are as shown in `Subclasses of the
<button> class`_. These subclasses include not only
buttons that can appear in any sheet, but also their equivalent classes
of menu item. Thus, an instance of :class:`<check-button>` represents a button
whose state can toggle a specific value on and off, and an instance of
:class:`<check-menu-button>` represents a menu item whose state can toggle a
specific value on and off in the same way.

Since all the subclasses of :class:`<button>` are themselves value gadgets,
each one supports the *value* protocol, as described in `Overview`_.

Subclasses of the *<button>* class

<button>

<check-button>

<check-menu-button>

<menu-button>

*<push-button>*

<push-menu-button>

<radio-button>

<radio-menu-button>

Subclasses of <collection-gadget>
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The subclasses of :class:`<collection-gadget>` are as shown in `Subclasses
of the <collection-gadget> class`_. All of these
subclasses support the *items* protocol, even though they are not
displayed in bold.

Subclasses of the *<collection-gadget>* class

<collection-gadget>

<button-box>

<check-box>

<push-box>

<radio-box>

*<list-box>*

*<menu-box>*

*<check-menu-box>*

*<push-menu-box>*

*<radio-menu-box>*

<option-box>

<combo-box>

<spin-box>

*<list-control>*

*<tree-control>*

*<table-control>*

Two subclasses themselves have a number of subclasses defined: those
subclasses representing collections of buttons:

:class:`<button-box>`
   These are used to create collections of buttons of the
   same type. You can create collections of any of the three basic types
   of button available: check buttons, radio buttons, or push buttons.

:class:`<menu-box>`
   These are used to create collections of menu items of the same type.
   As with :class:`<button-box>`, you can create collections of
   any of the three basic types of menu button available: check, radio,
   or push menu buttons.

In addition, the following types of list are provided:

:class:`<list-box>`
   These are standard list boxes, allowing a list of items
   to be displayed in a pane, with a scroll bar allowing the complete
   list to be viewed if necessary. List boxes may be single, multiple,
   or no selection.

:class:`<option-box>`
   A standard drop-down list box. This is similar to a
   list box, except that the entire list of options is only displayed on
   demand. In its closed state, only the current selection is visible.

:class:`<combo-box>`
   A combo box combines an option box with a text field,
   providing a list box whose contents can be displayed on demand, or
   edited by typing into the box in its closed state. Any new values
   typed in by the user are automatically added to the list of options
   subsequently displayed.

:class:`<spin-box>`
   A spin box is a text box that will only accept a limited
   number of input values, themselves making up a loop. A typical
   example might be the integers between 0 and 10. Spin boxes also
   incorporate small buttons (up-down controls) that allow the user to
   change the value by clicking the button in the appropriate direction.

Three controls are also available for displaying more general pieces of
information:

:class:`<list-control>`
   List controls provide an extended list box
   functionality that let you display a collection of items, each item
   consisting of an icon and a label. A number of different views are
   available, allowing you to view the items in different ways.

:class:`<tree-control>`
   Tree controls are a special list control that
   displays a set of objects in an indented outline based on the logical
   hierarchical relationship between the objects.

:class:`<table-control>`
   These allow you to display information as a table, with information
   divided into a number of column headings.

Since all the subclasses of :class:`<collection-gadget>` are themselves
value gadgets, each one supports the *value* protocol, as described in
`Overview`_.

Button gadgets
==============

Broadly speaking, buttons are gadgets whose value can be changed, or for
which some user-defined functionality can be invoked, by clicking on the
gadget with the pointer device. Buttons encompass obvious controls such
as push buttons, radio buttons, and check boxes, and, less obviously,
menu items.

.. figure:: images/gadgets-6.png
   :align: center

   A selection of button and equivalent menu buttons

Text gadgets
============

A text gadget is a gadget into which you can type textual information.
There are three different classes of text gadget available in DUIM, each
of which is a subclass of the :class:`<text-gadget>` class.

:class:`<text-field>`
   This is the most basic type of text gadget: the single line.

:class:`<text-editor>`

:class:`<password-field>`

Collection gadgets
==================

A collection gadget is any gadget whose items may themselves form a
Dylan collection. Often, a collection gadget is used to group together a
number other gadgets, such as buttons, in such a way that the
functionality of those gadgets is connected in some way. For example, a
:class:`<radio-box>` is a collection of radio buttons connected in such a way
that only one of the buttons can be selected at any time (as is the
standard behavior for a group of radio buttons). The items contained in
a collection gadget are expressed using the :gf:`gadget-items` slot.

Note that collection gadgets are not defined as collections of other
gadgets, even though this might be a convenient way to think of them.
When defining a collection gadget, you give the :gf:`gadget-items` slot a
standard Dylan collection. The type of collection gadget you are
creating then determines the type of gadget that is contained in the
resulting collection gadget.

The most simple types of collection gadget mirror the standard buttons
and menu buttons available, allowing you to create collections of push
buttons, radio buttons, check buttons, and their menu button
equivalents. Separators are automatically added to collections of menu
buttons so as to delineate them visually from other menu buttons in the
same menu.

.. figure:: images/gadgets-10.png
   :align: center

   A variety of simple collection gadgets

Value range gadgets
===================

A value range gadget is any gadget whose value falls within a defined
Dylan range.

.. figure:: images/gadgets-11.png
   :align: center

   A variety of value range gadgets

Sliders, scroll bars, and scroll bars are all examples of value range
gadgets. Value range gadgets provide immediate visual feedback of the
value of the gadget at any time, as shown in `A variety of value
range gadgets`_. In the case of sliders and scroll
bars, the user can set the :gf:`gadget-value` by dragging the appropriate
part of the gadget to a new point on the scale. Progress bars are
typically used only to provide the user with feedback about the progress
of a task.

Page gadgets
============

A page gadget is used to define the contents of a page in any control
that consists of multiple pages. Different classes of page gadget are
used for different types of multi-page control. There are three types of
page available:

:class:`<tab-control-page>`
   These are pages that are used within a tab control. Clicking on any
   tab in a tab control displays a different page of information.

:class:`<property-page>`
   These are pages that are displayed in property frames: modeless
   dialog boxes that contain several pages of information displayed as
   tabbed pages. This class is similar to :class:`<tab-control-page>`,
   except that its use is limited to modeless dialog boxes. For more
   information about property frames, see ` <frames.htm#89815>`_.

:class:`<wizard-page>`
   This type of page is used exclusively in wizard frames, in which the
   user is guided through a sequence of steps in order to perform a
   specified operation. For more information about wizard frames,
   see ` <frames.htm#89815>`_.

.. figure:: images/gadgets-12.png
   :align: center

   A tab control page, a property page, and a wizard page

.. note:: The :class:`<wizard-page>` and :class:`<property-page>` classes
   are actually exposed by the DUIM-Frames library, rather than the
   DUIM-Gadgets library. See ` <frames.htm#89815>`_for full details
   on this library.

Gadgets that can have children
==============================

Most gadgets cannot have any children associated with them; they are
leaf elements in the sheet hierarchy. However, a number of specialized
gadgets exist which can take children. This section describes those
classes.

For all the classes described in this section, the children of any
instance of the class are defined using the children: init-keyword. In
addition, the children of an instance of any of these classes must
themselves be gadgets of some kind. In some cases (menu bars, for
instance), the type of gadgets that can be defined as a child is
constrained.

Menus and menu bars
^^^^^^^^^^^^^^^^^^^

You can define a system of menus for a DUIM application by creating a
hierarchy of menu bar, menu, and menu button objects. Menu bars can be
defined for any application written using DUIM using the :class:`<menu-bar>`
class. For most applications, a single menu bar is defined for each
window in the application that contains a system of menus. Each menu bar
contains a number of menus: the children of the menu bar. Each menu in
an application is an instance of the :class:`<menu>` class. The menus of an
application can be populated using several different classes of gadget,
all of which are subclasses of the :class:`<menu-button>` class.

Status bars
^^^^^^^^^^^

You can add a status bar to a window in a DUIM application by creating
an instance of the :class:`<status-bar>` class. A status bar is typically used
to provide feedback to the user, and by default shows displays the
documentation string for any menu command currently under the mouse
cursor. In addition, you can define status bars that display any textual
information your application requires, and to this end, status bars can
take a number of children.

.. figure:: images/gadgets-13.png
   :align: center

   A status bar

In word processing applications, the status bar may also display the
current position of the insertion point, and information about the
current font family, size, and variation, if appropriate. In an e-mail
client application, the status bar may display the number of messages in
the current folder. Often, the system time is displayed in the status
bar for an application.

Tab controls
^^^^^^^^^^^^

An instance of the class :class:`<tab-control>` lets you define a sheet that
contains several "pages" of information. Each page of information is
displayed by clicking on the appropriate tab along the top of the sheet.

.. figure:: images/gadgets-14.png
   :align: center

   A tab control

This children of a tab control are the pages of information themselves.
Each child should be an instance of the :class:`<page>` class. The various
types of page available are described in `Page gadgets`_.

Group boxes
^^^^^^^^^^^

|image0| The :class:`<group-box>` class allows you to group together any number
of gadgets that are associated to some degree in an interface. A group
box creates a purely visual grouping, and does not affect the behavior
or interaction between its children in any way. For this reason, there
are no constraints on the types of gadget that you can group together;
the children of a group box can be any type of gadget.

DUIM-Gadgets Module
===================

This section contains a complete reference of all the interfaces that
are exported from the *duim-gadgets* module.

.. class:: <action-gadget>
   :open:
   :abstract:

   The protocol class for gadgets that have action callbacks.

   :superclasses: :class:`<gadget>`

   :keyword activate-callback: An instance of type ``false-or(<function>)``. Default value: ``#f``.

   :description:

     The class used by gadgets that have an action callback that allows some
     type of action to be performed, such as a push button. Action gadgets
     can only be activated when they are enabled.

   :operations:

    - :gf:`gadget-activate-callback`
    - :gf:`gadget-activate-callback-setter`

   See also

   - :class:`<gadget>`

.. generic-function:: activate-gadget

   Activates the specified gadget.

   :signature: activate-gadget *gadget* => ()

   :param gadget: An instance of type :class:`<gadget>`.


   :description:

     Activates gadget by calling the activate callback. For example, in the
     case of a button, calling this generic function would be as if the user
     had pressed the button.

.. generic-function:: add-column

   Adds a column to the specified table.

   :signature: add-column *table heading generator index* => ()

   :param table: An instance of type :class:`<table-control>`.
   :param heading: An instance of type ``type-union(<string>, <label>)``.
   :param generator: An instance of type ``<function>``.
   :param index: An instance of type ``<integer>``.


   :description:

     Adds a column *table*, with a table heading given by *heading*. The
     contents of the column are generated by calling the *generator* function
     on the item for each row of *table*. The *index* specifies where in the
     column order the new column should be added.

   See also

   - :gf:`remove-column`

.. generic-function:: add-item

   Adds an item to the specified list or table control.

   :signature: add-item *list-or-table item* #key *after* => *item*

   :param list-or-table: An instance of ``type-union(<list-control>, <table-control>)``.
   :param item: An instance of type ``type-union(<list-item>, <table-item>)``.
   :param #key after: An instance of type ``type-union(<list-item>, <table-item>)``.
   :value item: An instance of type ``type-union(<list-item>, <table-item>)``.

   :description:

     Adds an *item* to the specified *list-or-table*. The new item is
     created via a call to :gf:`make-item`.

     The *after* argument indicates which item to place the new item after.

   See also

   - :gf:`find-item`
   - :class:`<list-control>`
   - :class:`<list-item>`
   - :gf:`make-item`
   - :gf:`remove-item`
   - :class:`<table-control>`
   - :class:`<table-item>`

.. generic-function:: add-node

   Adds node to the specified tree control.

   :signature: add-node *tree parent node* #key *after setting-roots?* => *node*

   :param tree: An instance of :class:`<tree-control>`.
   :param parent: An instance of :class:`<tree-control>`.
   :param node: An instance of type :class:`<tree-node>`.
   :param #key after: An instance of type :class:`<tree-node>`.
   :param #key setting-roots?: An instance of type ``<boolean>``.
   :value node: An instance of type :class:`<tree-node>`.

   :description:

     Adds a *node* to the specified *tree* with the specified *parent*. The
     new item is created via a call to :gf:`make-node`.

     The *after* argument indicates which node to place the new node after.
     If *setting-roots?* is true, then the new node is added at the root of
     *tree*.

   See also

   - :gf:`find-node`
   - :gf:`make-node`
   - :gf:`remove-node`
   - :class:`<tree-control>`

.. class:: <border>
   :open:
   :abstract:
   :instantiable:

   The class of bordering gadgets.

   :superclasses: :class:`<gadget>` :class:`<single-child-composite-pane>`

   :keyword thickness: An instance of type ``<integer>``. Default value: 1.
   :keyword type: An instance of type ``one-of(#f, #"flat", #"sunken",
     #"raised", #"ridge", #"groove", #"input", #"output")``. Default
     value: ``#f``.

   :description:

     The base class of gadgets that provide borders to their children.

     The thickness of the border is specified by the ``thickness:``
     init-keyword, and is given in pixels.

     The ``type:`` init-keyword represents the kind of border to be created.
     Borders may appear raised from the area they surround, or lowered with
     respect to it. Alternatively, a border may be displayed as a thin ridge
     or groove. Input and output borders represent "logical" borders.

     .. figure:: images/gadgets-16.png
        :align: center

        Different types of border

     Borders are usually created using the :macro:`with-border` macro, rather
     than by making direct instances of this class.

   See also

   - :class:`<group-box>`
   - :macro:`with-border`

.. class:: <button>
   :open:
   :abstract:
   :instantiable:

   The class of all button gadgets.

   :superclasses: :class:`<value-gadget>`

   :keyword accelerator: An instance of type ``false-or(<gesture>)``. Default value: ``#f``.
   :keyword mnemonic: An instance of type ``false-or(<character>)``. Default value: ``#f``.

   :description:

     The class of all button gadgets.

     The ``accelerator:`` init-keyword is used to specify a keyboard
     accelerator for the button. This is a key press that gives the user a
     method for activating the button using a short key sequence rather than
     by clicking the button itself. Keyboard accelerators usually combine the
     CONTROL and possibly SHIFT keys with an alphanumeric character.

     When choosing accelerators, you should be aware of style guidelines that
     might be applicable for the operating system you are developing for. For
     example, a common accelerator for the command *File > Open* in Windows
     is CTRL+O.

     Keyboard accelerators are mostly used in menu buttons, though they can
     be applied to other forms of button as well.

     The ``mnemonic:`` init-keyword is used to specify a keyboard mnemonic for
     the button. This is a key press that involves pressing the ALT key
     followed by a number of alphanumeric keys.

     Note that the choice of keys is more restrictive than for keyboard
     accelerators. They are determined in part by the names of button itself
     (and, in the case of menu buttons, the menu that contains it), as well
     as by any appropriate style guidelines. For example, a common mnemonic
     for the *File > Open* command is ALT, F, O.

     Mnemonics have the advantage that the letters forming the mnemonic are
     automatically underlined in the button label on the screen (and, for
     menu buttons, the menu itself). This means that they do not have to be
     remembered. In addition, when the user makes use of a mnemonic in a
     menu, the menu itself is displayed on screen, as if the command had been
     chosen using the mouse. This does not happen if the keyboard accelerator
     is used.

     Buttons are intrinsically "non-stretchy" gadgets. That is, the width and
     height of a button is generally calculated on the basis of the button’s
     label, and the button will be sized so that it fits the label
     accordingly. Sometimes, however, you want a button to occupy all the
     available space that is given it, however large that space may be. To
     force a button to use all the available width or height, specify
     ``max-width: $fill`` or ``max-height: $fill`` accordingly in the button
     definition. See the second example below to see how this is done.

   :operations:

     - `<frames.htm#56017>`
     - `<frames.htm#56015>`
     - `<frames.htm#24406>`
     - `<frames.htm#37806>`
     - `<frames.htm#48310>`
     - `<frames.htm#91817>`
     - `<frames.htm#56017>`

   :example:

     .. code-block:: dylan

        contain
          (make(<button>, label: "Hello",
                activate-callback:
                method (gadget)
                  notify-user
                    (format-to-string
                      ("Pressed button %=", gadget),
                     owner: gadget)
                end));

     The following example creates a column layout that contains two
     elements.

     - The first is a row layout that itself contains two buttons with short
       labels.
     - The second is a button with a long label.

     The use of ``equalize-widths?:`` in the call to ``vertically`` ensures that
     these two elements have the same width.

     The interesting part of this example is in the use of ``max-width: $fill``
     in the definition of the buttons with shorter labels. If this was not
     used, then each button would be sized such that it just fit its own
     label, and there would be empty space in the row layout. However, using
     ``max-width: $fill`` ensures that each button is made as large as
     possible, so as to fit the entire width of the row layout.

     .. code-block:: dylan

        vertically (equalize-widths?: #t)
          horizontally ()
            make(<button>, label: "Red", max-width: $fill);
            make(<button>, label: "Ultraviolet",
                 max-width: $fill);
          end;
          make(<button>,
               label:
                 "A button with a really really long label");
        end

   See also

   - `<button-box>`
   - `<check-button>`
   - `$fill`
   - `gadget-accelerator`
   - `<menu-button>`
   - `<radio-button>`
   - `<space-requirement>`

.. class:: <button-box>
   :open:
   :abstract:
   :instantiable:

   A class that groups buttons.

   :superclasses: :class:`<collection-gadget>` :class:`<multiple-child-composite-pane>`

   :keyword rows: An instance of type ``false-or(<integer>)``.
   :keyword columns: An instance of type ``false-or(<integer>)``.
   :keyword orientation: An instance of type
     ``one-of(#"horizontal", #"vertical")``.
     Default value: ``#"horizontal"``.
   :keyword layout-class: An instance of type ``subclass(<layout>)``.
     Default value: :class:`<column-layout>` or
     :class:`<row-layout>`, depending on orientation.
   :keyword child: An instance of type ``false-or(<sheet>)``. Default value: ``#f``.

   :description:

     The class of grouped buttons; the superclass of :class:`<check-box>` and
     :class:`<radio-box>`.

     The ``rows:`` and ``columns:`` init-keywords allow you to specify how many
     rows or columns should be used to lay out the buttons. In addition, you
     can set the orientation of the button box by specifying the
     ``orientation:`` init-keyword.

     An instance of the class that is specified by ``layout-class:`` is used to
     parent the buttons that are created, and any extra arguments that are
     specified, such as ``x-alignment:`` and ``x-spacing:``, are passed along to
     this layout.

     You can use the ``child:`` init-keyword to specify a sheet hierarchy to be
     used in place of a list of items. Under normal circumstances, the items
     defined for any button box are realized in terms of their "natural"
     gadget class. For example, if you create a radio button box, DUIM
     creates a radio button for each item that you specify. By using the
     ``child:`` init-keyword, you can define sheet hierarchies that override
     these "natural" gadget classes, letting you specify more complex
     arrangements of gadgets: in this way, you could create a check button
     box where each check button is itself surrounded by a group box. For an
     example of the use of the ``child:`` init-keyword, look at the initial
     dialog box that is displayed when you first start the Dylan environment.
     In this dialog, a number of radio buttons are presented, each delineated
     by its own group box. In fact, this dialog is implemented s a radio
     button box in which the ``child:`` init-keyword has been used rather than
     the ``items:`` init-keyword.

     If you use ``child:``, then the :gf:`gadget-value` returned by the gadget is
     the :gf:`gadget-id` of the selected button. Contrast this with ``items:``,
     where the selected item is returned as the :gf:gadget-value`.

   :examples:

     .. code-block:: dylan

        contain(make(<button-box>,
                     selection-mode: #"multiple",
                     items: range(from: 0, to: 20)));

     The following examples illustrate the use of some of the init-keywords
     described. They each create an instance of a subclass of
     :class:`<button-box>`.  Note that the ``selection-mode:`` init-keyword
     may be used instead, rather than creating a direct instance of one of
     the subclasses.

     .. code-block:: dylan

        contain(make(<check-box>, items: range(from: 1, to: 9),
                     columns: 3));
        contain(make(<radio-box>, items: #("Yes", "No"),
                     orientation: #"vertical");
        contain(make(<check-box>, items: #(1, 2, 3. 4),
                     layout-class: <table-layout>
                     rows: 2));

   See also

   - :class:`<check-box>`
   - :class:`<push-box>`
   - :class:`<radio-box>`

.. class:: <check-box>
   :open:
   :abstract:
   :instantiable:

   The class of check boxes, or groups of check buttons.

   :superclasses: :class:`<button-box>` :class:`<action-gadget>`

   :description:

     |image1| The instantiable class that implements an abstract check box,
     that is, a gadget that constrains a number of toggle buttons, zero or
     more of which may be selected at any one time.

     The value of a check box is a sequence of all the currently selected
     items in the check box.

   :examples:

     .. code-block:: dylan

        contain(make(<check-box>, items: #(1, 2, 3, 4, 5)));
        contain(make(<check-box>, items: range(from: 1, to: 9),
                     columns: 3));
        contain(make(<check-box>, items: #(1, 2, 3, 4),
                     layout-class: <table-layout>
                     rows: 2));

   See also

   - :class:`<group-box>`
   - :class:`<push-box>`
   - :class:`<radio-box>`

.. class:: <check-button>
   :open:
   :abstract:
   :instantiable:

   The class of check buttons.

   :superclasses: :class:`<button>` :class:`<action-gadget>`

   :description:

     |image2| The class of check buttons. The value of a check button is
     either ``#t`` or ``#f``, depending whether or not it is currently selected.

     Internally, this class maps into the check box Windows control.

   :example:

     .. code-block:: dylan

        contain(make(<check-button>, label: "Check button"));

   See also

   - :class:`<check-menu-button>`
   - :class:`<push-button>`
   - :class:`<radio-button>`

.. class:: <check-menu-box>
   :open:
   :abstract:
   :instantiable:

   The class of groups of check buttons displayed in a menu.

   :superclasses: :class:`<menu-box>` :class:`<action-gadget>`

   :description:

     The class of groups of check buttons displayed in a menu|image3| .

     Internally, this class maps into the menu Windows control.

   :example:

     The following example creates a menu that shows an example of a check
     menu box.

     .. code-block:: dylan

        contain(make(<menu>,
                     label: "Hello...",
                     children: vector
                       (make(<radio-menu-box>,
                             items:
                               #("You", "All",
                                 "Everyone")),
        )));

   See also

   - :class:`<menu-box>`
   - :class:`<push-menu-box>`
   - :class:`<radio-menu-box>`

.. class:: <check-menu-button>
   :open:
   :abstract:
   :instantiable:

   The class of check buttons that can be displayed in a menu.

   :superclasses: :class:`<menu-button>`

   :description:

     |image4| The class of check buttons that can be displayed in a menu. The
     values of a menu button is either ``#t`` or ``#f``.

     Internally, this class maps into the menu item Windows control.

   :example:

     .. code-block:: dylan

        contain
          (make(<check-menu-button>,
                label: "Menu button",
                activate-callback:
                  method (gadget)
                    notify-user(format-to-string
                      ("Toggled button %=", gadget)) end));

   See also

   - :class:`<check-button>`
   - :class:`<radio-menu-button>`

.. class:: <collection-gadget>
   :open:
   :abstract:

   The class of all gadgets that contain collections.

   :superclasses: :class:`<value-gadget>`

   :keyword items: An instance of type :drm:`<sequence>`. Default value: ``#[]``.
   :keyword label-key: An instance of type ``<function>``.
   :keyword value-key: An instance of type ``<function>``. Default value: :drm:`identity`.
   :keyword test: An instance of type ``<function>``. Default value: ``==``.
   :keyword selection: An instance of type ``limited(<sequence>, of: <integer>)``.
     Default value: ``#[]``.
   :keyword selection-mode: An instance of type ``one-of(#"single", #"multiple", #"none")``.
     Default value: ``#"single"``.
   :keyword key-press-callback: An instance of type
     ``false-or(<frames.htm#40934>, <function>)``.

   :description:

     The class of all gadgets that can contain collections.

     The ``items:`` init-keyword is used to specify the collection of items
     that the collection gadget contains.

     The ``label-key:`` and ``value-key:`` init-keywords are functions that are
     used to calculate the labels and the value of the gadget respectively.

     The value of a collection gadget is determined by calling the value key
     of the gadget on each selected item in the gadget. The "printed
     representation" of a collection gadget is determined by calling the
     label key of the gadget on each item.

     By default, the label key returns the numeric label of the gadget items
     (for example, the buttons in a button box would be labeled 1, 2, 3, and
     so on). In general, the label key can be trusted to "do the right thing"
     by default.

     By default, the value key returns the collection gadget itself.

     Note also that the :gf:`gadget-value` method for collection gadgets
     is different for single and multiple selection gadgets. For single
     selection, the item that is selected is returned.  For multiple
     selection, a sequence of the selected items is returned.

     The ``test:`` init-keyword is the function used to test whether two items
     of the collection are considered identical.

     The ``selection:`` init-keyword is available only to those subclasses
     of :class:`<collection-gadget>` that contain items that may be
     selected. The selection is a collection containing the selected
     keys from the items collection.

     Subclasses of :class:`<collection-gadget>` that can have selections
     are:

     - :class:`<list-box>`
     - :class:`<option-box>`
     - :class:`<list-control>`
     - :class:`<tree-control>`
     - :class:`<table-control>`
     - :class:`<radio-box>`
     - :class:`<check-box>`
     - :class:`<check-menu-box>`
     - :class:`<radio-menu-box>`
     - :class:`<combo-box>`

     The ``key-press-callback:`` init-keyword lets you specify a key-press
     callback. This type of callback is invoked whenever a key on the
     keyboard is pressed while the gadget has focus. It applies only to graph
     controls, list controls, tab controls, and table controls. See
     :gf:`gadget-key-press-callback`, for a fuller description of
     key-press callbacks.

   :operations:

     - :gf:`gadget-items`
     - :gf:`gadget-items-setter`
     - :gf:`gadget-key-press-callback`
     - :gf:`gadget-key-press-callback-setter`
     - :gf:`gadget-label-key`
     - :gf:`gadget-selection`
     - :gf:`gadget-selection-mode`
     - :gf:`gadget-selection-setter`
     - :gf:`gadget-test`
     - :gf:`gadget-value-key`

   See also

   - :class:`<button-box>`
   - :class:`<check-box>`
   - :class:`<check-menu-box>`
   - :class:`<combo-box>`
   - :class:`<list-box>`
   - :class:`<list-control>`
   - :class:`<option-box>`
   - :class:`<radio-box>`
   - :class:`<radio-menu-box>`
   - :class:`<table-control>`
   - :class:`<tree-control>`

.. class:: <combo-box>
   :open:
   :abstract:
   :instantiable:

   The class of combo boxes, which combine options boxes with text fields.

   :superclasses: :class:`<collection-gadget>` :class:`<action-gadget>` :class:`<text-gadget>`

   :keyword borders: An instance of type ``one-of(#f, #"none", #"flat",
     #"sunken", #"raised", #"ridge", #"groove", #"input", #"output")``.
     Default value: ``#f``.
   :keyword scroll-bars: An instance of type ``one-of(#f, #"none",
     #"horizontal", #"vertical", #"both", #"dynamic")``. Default value:
     ``#"both"``.

   :description:

     The class of combo boxes. Combo boxes are similar to option boxes,
     except that the text field is editable, so that new values can be
     specified in addition to those already provided in the drop-down list.
     Users may either choose an existing option from the list, or type in
     their own.|image5|

     It is common for additional items typed by the user to be added to the
     list of options available. A combo box is often used to specify text in
     a Find dialog box, for example, and any previous search terms can be
     recalled by choosing them from the list. If you wish to provide this
     functionality, then you can do so using a combination of :gf:`add-item`
     and :gf:`find-item`, to search for the presence of an item and add it
     if it does not already exist.

     The ``borders:`` init-keyword lets you specify a border around the combo
     box. If specified, a border of the appropriate type is drawn around the
     gadget.

     The ``scroll-bars:`` init-keyword lets you specify the scroll bar behavior
     for the gadget.

     Internally, this class maps into the Windows combo box control.

   :example:

     .. code-block:: dylan

        contain(make(<combo-box>, value-type: <integer>
                     items: range(from: 1 to: 5)));

   See also

   - :class:`<option-box>`
   - :class:`<text-field>`

.. generic-function:: contract-node

   Contracts the specified node in a tree control.

   :signature: contract-node *tree-control* *node* => ()

   :param tree-control: An instance of :class:`<tree-control>`.
   :param node: An instance of type :class:`<tree-node>`.

   :description:

     Contracts the specified *node* in *tree-control*, thereby hiding any
     children of the node that were displayed.

   See also

   - :gf:`expand-node`

.. generic-function:: display-menu

   Displays the specified menu.

   :signature: display-menu *menu* #key *x y* => ()

   :param menu: An instance of type :class:`<menu>`.
   :param #key x: An instance of type ``false-or(<integer>)``. Default value: ``#f``.
   :param #key y: An instance of type ``false-or(<integer>)``. Default value: ``#f``.

   :description:

     Displays the specified menu, optionally at a precise position on the
     screen, specified by *x* and *y*, where *x* and *y* are both relative
     to the owner of the menu.

     The function returns when the menu has been popped down again.

   See also

   - :class:`<menu>`

.. generic-function:: expand-node

   Expands the specified node in a tree control.

   :signature: expand-node *tree-control* *node* #key *sort-function* => ()

   :param tree-control: An instance of :class:`<tree-control>`.
   :param node: An instance of type :class:`<tree-node>`.
   :param #key sort-function: An instance of type :drm:`<function>`.

   :description:

     Expands the specified node in a *tree-control*, thereby displaying any
     children that the node has.

     If no children have been explicitly added to the node before it is
     expanded, they are generated by calling the tree’s children generating
     function on the node.

   See also

   - :gf:`contract-node`
   - :gf:`tree-control-children-generator`

.. generic-function:: find-item

   Finds an item in a list control or a table control.

   :signature: find-item *list-or-table* *object* #key => *found-item*

   :param list-or-table: An instance of ``type-union(<list-control>, <table-control>)``.
   :param object: An instance of type :drm:`<object>`.
   :value found-item: An instance of type ``type-union(<list-item>, <table-item>, #f)``.

   :description:

     Finds the item in a list control or a table control that corresponds to
     *object*.

   See also

   - :gf:`add-item`
   - :class:`<list-control>`
   - :class:`<list-item>`
   - :gf:`make-item`
   - :gf:`remove-item`
   - :class:`<table-control>`
   - :class:`<table-item>`

.. generic-function:: find-node

   Finds a node in a tree control.

   :signature: find-item *tree* *object* #key *parent-node* => *found-item*

   :param tree: An instance of :class:`<tree-control>`.
   :param object: An instance of :drm:`<object>`.
   :param #key parent-node: An instance of type :class:`<tree-node>`.
   :value found-item: An instance of type :class:`<tree-node>`.

   :description:

     Finds the item in a tree control that corresponds to *object*.

   See also

   - :gf:`add-node`
   - :gf:`make-node`
   - :gf:`remove-node`
   - :class:`<tree-control>`

.. class:: <gadget>
   :open:
   :abstract:

   The protocol class of all gadgets.

   :superclasses: :drm:`<object>`

   :keyword id: An instance of type ``false-or(<object>)``. Default value: ``#f``.
   :keyword client: An instance of type ``false-or(<object>)``. Default value: ``#f``.
   :keyword label: An instance of type ``type-union(<string>, <image>)``. Required.
   :keyword documentation: An instance of type ``false-or(<string>)``. Default value: ``#f``.
   :keyword enabled?: An instance of type ``<boolean>``. Default value: ``#t``.
   :keyword read-only?: An instance of type ``<boolean>``. Default value: ``#f``.

   :description:

     The class of all gadgets. You should not create a direct instance of
     this class.

     The ``id:`` init-keyword lets you specify a unique identifier for the
     action gadget. This is a useful way of identifying gadgets, and provides
     you with an additional way of controlling execution of your code,
     allowing you to create simple branching statements such as:

     .. code-block:: dylan

        select (gadget-id)
          #"ok" => do-okay();
          #"cancel" => do-cancel();
        end select;

     Note, however, that specifying ``id:`` is not generally necessary. The
     ``id:`` init-keyword is useful in the case of tab controls, where it is
     returned by :gf:`gadget-value`.

     Every gadget has a ``client:`` that is specified when the gadget is
     created. Typically, ``client:`` is a frame or a composite sheet.

     The ``label:`` init-keyword lets you assign a label to any gadget. A label
     may be any string, or an image of an appropriate size (usually a small
     icon).

     The ``documentation:`` init-keyword is used to provide a short piece of
     online help for the gadget. Any documentation supplied for a gadget may
     be used in a tooltip or a status bar. For example, moving the mouse over
     a menu command may display the supplied documentation for that command
     in the status bar of your application, or moving the mouse over any of
     the buttons in a tool bar may display a tooltip (a piece of pop-up text)
     that contains the supplied documentation.

     If ``enabled?:`` is true, then the gadget is enabled; that is, the user
     can interact with the gadget in an appropriate way. If the gadget is not
     enabled, then the user cannot interact with it. For example, if a push
     button is not enabled, it cannot be clicked, or if a check box is not
     enabled, its setting cannot be switched on or off. Gadgets that are not
     enabled are generally grayed out on the screen.

     If ``read-only?``: is true, then the user cannot alter any of the values
     displayed in the gadget; this typically applies to text gadgets. Note
     that this is not the same as disabling the gadget — if a gadget is set
     to read-only, it is not grayed out, and the user may still interact with
     it: only the values cannot be changed.

   :operations:

     - :gf:`activate-gadget`
     - :gf:`choose-from-dialog`
     - :gf:`gadget-accelerator`
     - :gf:`gadget-accelerator-setter`
     - :gf:`gadget-client`
     - :gf:`gadget-client-setter`
     - :gf:`gadget-command`
     - :gf:`gadget-command-setter`
     - :gf:`gadget-default?`
     - :gf:`gadget-default?-setter`
     - :gf:`gadget-documentation`
     - :gf:`gadget-documentation-setter`
     - :gf:`gadget-value-changing-callback`
     - :gf:`gadget-value-changing-callback-setter`
     - :gf:`gadget-enabled?`
     - :gf:`gadget-enabled?-setter`
     - :gf:`gadget-id`
     - :gf:`gadget-id-setter`
     - :gf:`gadget-label`
     - :gf:`gadget-label-setter`
     - :gf:`gadget-mnemonic`
     - :gf:`gadget-mnemonic-setter`
     - :gf:`gadget-orientation`
     - :gf:`gadget-popup-menu-callback`
     - :gf:`gadget-popup-menu-callback-setter`
     - :gf:`gadget-read-only?`
     - :gf:`gadget-scrolling-horizontally?`
     - :gf:`gadget-scrolling-vertically?`
     - :gf:`update-gadget`

   See also

   - :class:`<action-gadget>`
   - :class:`<border>`
   - :gf:`gadget-value`
   - :class:`<group-box>`
   - :class:`<label>`
   - :class:`<menu>`
   - :class:`<page>`
   - :class:`<separator>`
   - :class:`<spacing>`
   - :class:`<tool-bar>`
   - :class:`<value-gadget>`
   - :class:`<viewport>`

.. generic-function:: gadget?

   Returns true if the specified object is a gadget.

   :signature: gadget? *object* => *gadget?*

   :param object: An instance of type :drm:`<object>`.
   :value gadget?: An instance of type ``<boolean>``.

   :description:

     Returns true if *object* is a gadget.

   :example:

     .. code-block:: dylan

        *gadget* := contain(make
                             (<radio-menu-box>,
                              items: range(from: 0, to: 20)));
        gadget?(*gadget*);

   See also

   - :class:`<gadget>`

.. generic-function:: gadget-accelerator

   Returns the keyboard accelerator of the specified gadget.

   :signature: gadget-accelerator *gadget* => *accelerator*

   :param gadget: An instance of type :class:`<gadget>`.
   :value accelerator: An instance of type :class:`<gesture>`.

   :description:

     Returns the keyboard accelerator of the specified gadget. An accelerator
     is a keyboard gesture that activates a gadget (that is, it invokes the
     activate callback for the gadget) without needing to use the mouse.

     Accelerators are of most use with button gadgets, and in particular menu
     button gadgets.

   See also

   - :class:`<button>`
   - :gf:`gadget-accelerator-setter`
   - :class:`<gesture>`
   - :class:`<menu-button>`

.. generic-function:: gadget-accelerator-setter

   Sets the keyboard accelerator of the specified gadget.

   :signature: gadget-accelerator-setter *accelerator gadget* => *accelerator*

   :param accelerator: An instance of type :class:`<gesture>`.
   :param gadget: An instance of type :class:`<gadget>`.
   :value accelerator: An instance of type :class:`<gesture>`.

   :description:

     Sets the keyboard accelerator of the specified gadget. An accelerator is
     a keyboard gesture that invokes the activate callback of a gadget
     without needing to use the mouse.

     Accelerators are of most use with button gadgets, and in particular menu
     button gadgets.

   See also

   - :class:`<button>`
   - :gf:`gadget-accelerator`
   - :class:`<gesture>`
   - :class:`<menu-button>`

.. generic-function:: gadget-activate-callback

   Returns the activate callback of the specified gadget.

   :signature: gadget-activate-callback *gadget* => *activate-callback*

   :param gadget: An instance of type :class:`<action-gadget>`.
   :value activate-callback: An instance of type ``false-or(<function>)``.

   :description:

     Returns the function that will be called when *gadget* is activated.
     This function will be invoked with one argument, the gadget itself.

     When this function returns ``#f``, this indicates that there is no
     activate callback for the gadget.

   See also

   - :gf:`gadget-activate-callback-setter`

.. generic-function:: gadget-activate-callback-setter

   Sets the activate callback for the specified gadget.

   :signature: gadget-activate-callback-setter *activate-callback gadget* => *activate-callback*

   :param activate-callback: An instance of type ``false-or(<function>)``.
   :param gadget: An instance of type :class:`<action-gadget>`.
   :value activate-callback: An instance of type ``false-or(<function>)``.

   :description:

     Sets the activate callback for *gadget* to *activate-callback*.

   See also

   - :gf:`gadget-activate-callback`

.. generic-function:: gadget-client

   Returns the client of the specified gadget.

   :signature: gadget-client *gadget* => *client*

   :param gadget: An instance of type :class:`<gadget>`.
   :param client: An instance of type :drm:`<object>`.

   :description:

     Returns the client of *gadget*. The client is the gadget or frame that
     gadget should look to for callback information.

     In any sheet hierarchy, the client is usually the immediate parent of
     gadget. This often means that the client is a frame, but it can also be
     another gadget. In the majority of cases, you need not be concerned with
     the client of a gadget. However, rather like the gadget-id, you are free
     to assign your own client to a given gadget whenever it is necessary for
     your code.

     In less obvious cases, the client may not be the immediate parent: for
     example, in the case of a radio box, the client of each button in the
     radio box is the radio box itself. At the implementation level, the
     radio box is not the immediate parent of the buttons that it contains,
     since there is an intervening layout object that arranges the buttons
     within the box. See :class:`<action-gadget>`, for more details.

     Gadget clients enable you to pass messages between the gadget and its
     client when a callback is received.

   See also

   - :gf:`gadget-client-setter`

.. generic-function:: gadget-client-setter

   Sets the client of the specified gadget.

   :signature: gadget-client-setter *client gadget* => *client*

   :param client: An instance of type :drm:`<object>`.
   :param gadget: An instance of type :class:`<gadget>`.
   :value client: An instance of type :drm:`<object>`.

   :description:

     Sets the *client* of the specified *gadget*.

     The client is often a frame, but it could be another gadget (for
     example, in the case of a push button that is contained in a radio box,
     the client of the button could be the radio box). See
     :class:`<action-gadget>`, for more details.

     Gadget clients enable you to pass messages between the gadget and its
     client when a callback is received.

   See also

   - :gf:`gadget-client`

.. generic-function:: gadget-command

   Returns the command associated with the specified gadget.

   :signature: gadget-command *gadget* => *command*

   :param gadget: An instance of type :class:`<gadget>`.
   :value command: An instance of type ``false-or(<frames.htm#40934>)``.

   :description:

     Returns the command associated with *gadget*.

     A command is typically associated with a gadget if that gadget has been
     created by using a command table. For example, the command associated
     with a menu button would represent the callback that is invoked when the
     user chooses the relevant menu command.

   See also

   - :gf:`gadget-command-setter`

.. generic-function:: gadget-command-setter

   Sets the command of the specified gadget.

   :signature: gadget-command-setter *command* *gadget* => *command*

   :param command: An instance of type ``false-or(<frames.htm#40934>)``.
   :param gadget: An instance of type :class:`<gadget>`.
   :value command: An instance of type ``false-or(<frames.htm#40934>)``.

   :description:

     Sets the command of the specified *gadget*.

     A command is typically associated with a gadget if that gadget has been
     created by using a command table. For example, the command associated
     with a menu button would represent the callback that is invoked when the
     user chooses the relevant menu command.

   See also

   - :gf:`gadget-command`

.. generic-function:: gadget-default?

    Returns true if the specified gadget is the default gadget in a frame.

   :signature: gadget-default? *gadget* => *default?*

   :param gadget: An instance of type :class:`<gadget>`.
   :value default?: An instance of type ``<boolean>``.

   :description:

     Returns true if the specified gadget is the default gadget for the frame
     it is part of.

     It is generally useful to set a default gadget in a frame, or a default
     menu if there is no suitable gadget.

     When a default gadget is specified, using the default keyboard gesture
     in the frame invokes the activate callback for the default gadget. The
     default gesture is usually pressing the RETURN button.

   See also

   - :gf:`gadget-default?-setter`

.. generic-function:: gadget-default?-setter

   Toggles whether the specified button is the default for the current
   frame.

   :signature: gadget-default?-setter *default? button* => *default?*

   :param default?: An instance of type ``<boolean>``.
   :param button: An instance of type :class:`<button>`.
   :value default?: An instance of type ``<boolean>``.

   :description:

     If *default?* is true, *button* becomes the default gadget for the
     current frame. If *default?* is ``#f``, *button* is not the default
     gadget for the current frame, regardless of any previous value the
     ``gadget-default?`` slot may have had.

     It is generally useful to set a default gadget in a frame, or a default
     menu if there is no suitable gadget.

     When a default gadget is specified, using the default keyboard gesture
     in the frame invokes the activate callback for the default gadget. The
     default gesture is usually pressing the RETURN button.

   See also

   - :gf:`gadget-default?`

.. generic-function:: gadget-documentation

   Returns the documentation string for the specified gadget.

   :signature: gadget-documentation *gadget* => *documentation*

   :param gadget: An instance of type :class:`<gadget>`.
   :value documentation: An instance of type ``false-or(<string>)``.

   :description:

     Returns the documentation string for *gadget*.

     The documentation string can be used to specify a short piece of online
     help text describing the action performed by the gadget. This text can
     then be displayed in a number of different ways. On Windows, for
     example, the documentation for a menu button might be displayed in the
     status bar of the application, and the documentation for a button might
     be displayed as a tooltip (a piece of pop-up text that appears next to
     the mouse pointer when the pointer is inside the region occupied by the
     gadget).

     You are strongly encouraged to supply documentation strings for
     significant gadgets in your application. Because of the nature of their
     presentation, you should keep them as short as possible.

   See also

   - :gf:`gadget-documentation-setter`

.. generic-function:: gadget-documentation-setter

   Sets the documentation string for the specified gadget.

   :signature: gadget-documentation-setter *documentation gadget* => *documentation*

   :param documentation: An instance of type :drm:`<string>`.
   :param gadget: An instance of type :class:`<gadget>`.
   :value documentation: An instance of type :drm:`<string>`.

   :description:

     Sets the documentation string for *gadget* to *documentation*.

     The documentation string can be used to specify a short piece of online
     help text describing the action performed by the gadget. This text can
     then be displayed in a number of different ways. On Windows, for
     example, the documentation for a menu button might be displayed in the
     status bar of the application, and the documentation for a button might
     be displayed as a tooltip (a piece of pop-up text that appears next to
     the mouse pointer when the pointer is inside the region occupied by the
     gadget).

     You are strongly encouraged to supply documentation strings for
     significant gadgets in your application. Because of the nature of their
     presentation, you should keep them as short as possible.

   See also

   - :gf:`gadget-documentation`
   - :class:`<status-bar>`

.. generic-function:: gadget-enabled?

   Returns true if the gadget is enabled.

   :signature: gadget-enabled? *gadget* => *enabled?*

   :param gadget: An instance of type :class:`<gadget>`.
   :value enabled?: An instance of type ``<boolean>``.

   :description:

     Returns true if *gadget* is enabled.

     If the gadget is enabled, the user can interact with it in an
     appropriate way. If the gadget is not enabled, then the user cannot
     interact with it. For example, if a push button is not enabled, it
     cannot be clicked, or if a check box is not enabled, its setting cannot
     be switched on or off. Gadgets that are not enabled are generally grayed
     out on the screen.

   :example:

     .. code-block:: dylan

        *gadget* := contain(make
                             (<radio-box>,
                              items: range(from: 0, to: 20)));
        gadget-enabled?(*gadget*);

   See also

   - :class:`<gadget>`
   - :gf:`gadget-enabled?-setter`

.. generic-function:: gadget-enabled?-setter

   Toggles the enabled state of the specified gadget.

   :signature: gadget-enabled?-setter *enabled?* *gadget* => *enabled?*

   :param enabled?: An instance of type ``<boolean>``.
   :param gadget: An instance of type :class:`<gadget>`.
   :value enabled?: An instance of type ``<boolean>``.

   :description:

     Causes *gadget* to become active (that is, available for input) or
     inactive, by toggling its enabled state. If *enabled?* is true, then
     *gadget* is enabled, otherwise, *gadget* is not enabled.

     If the gadget is enabled, the user can interact with it in an
     appropriate way. If the gadget is not enabled, then the user cannot
     interact with it. For example, if a push button is not enabled, it
     cannot be clicked, or if a check box is not enabled, its setting cannot
     be switched on or off. Gadgets that are not enabled are generally grayed
     out on the screen.

   :example:

     .. code-block:: dylan

        *gadget* := contain(make
                             (<radio-box>,
                              items: range(from: 0, to: 20)));
        gadget-enabled?(*gadget*) := #f;

   See also

   - :class:`<gadget>`
   - :gf:`gadget-enabled?`

.. generic-function:: gadget-id

   Returns the ID of the specified gadget.

   :signature: gadget-id *gadget* => *id*

   :param gadget: An instance of type :class:`<gadget>`.
   :value id: An instance of type :drm:`<object>`.

   :description:

     Returns the identifier of *gadget*. The identifier is typically a
     simple Dylan object that uniquely identifies the gadget. For most
     gadgets, it is usually not necessary. Making use of a gadget ID provides
     you with an additional way of controlling execution of your code,
     allowing you to create simple branching statements such as:

     .. code-block:: dylan

        select (gadget-id)
          #"modify" => do-modify();
          #"add" => do-add();
          #"remove" => do-remove();
          #"done" => do-done();
        end select;

     In the specific case of tab controls, it is more important that you
     specify an ID. The gadget ID for a tab control is returned as the gadget
     value for that tab control.

   :example:

     .. code-block:: dylan

        *gadget* := contain(make(<button>, id: #test,
                                 label: "Test"));
        gadget-id(*gadget*);

   See also

   - :gf:`gadget-id-setter`
   - :gf:`gadget-value`
   - :class:`<tab-control>`

.. generic-function:: gadget-id-setter

   Sets the ID of the specified gadget.

   :signature: gadget-id-setter *id gadget* => *id*

   :param id: An instance of type :drm:`<object>`.
   :param gadget: An instance of type :class:`<gadget>`.
   :value id: An instance of type :drm:`<object>`.

   :description:

     Sets the identifier of *gadget*. The identifier is typically a simple
     Dylan object that uniquely identifies the gadget. For most gadgets, it
     is usually not necessary, though it does provide you with an additional
     way of controlling execution of your code based on the gadget returned.

     In the specific case of tab controls, it is more important that you
     specify an ID. The gadget ID for a tab control is returned as the gadget
     value for that tab control.

   :example:

     .. code-block:: dylan

        *gadget* := contain(make(<button>, id: #test,
                                 label: "Test"));
        gadget-id(*gadget*) := #test-two;
        gadget-id(*gadget*);

   See also

   - :gf:`gadget-id`
   - :gf:`gadget-value`
   - :class:`<tab-control>`

.. generic-function:: gadget-items

   Returns the items for the specified gadget.

   :signature: gadget-items *gadget* => *items*

   :param gadget: An instance of type :class:`<collection-gadget>`.
   :value items: An instance of type :drm:`<sequence>`. Default value: ``#[]``.

   :description:

     Returns the items for *gadget*. The items of any collection gadget is
     the collection of items that the collection gadget contains. In a list
     box, for example, the items are the list items themselves.

   :example:

     The following code creates a list box whose items are the lower-cased
     equivalents of the symbols stated. Note that the label key for a gadget
     is a function that computes the label for the items in that gadget.

     .. code-block:: dylan

        *gadget* := contain(make(<list-box>,
                                 items: #(#"One", #"Two", #"Three"),
                                 label-key:
                                   method (symbol)
                                     as-lowercase
                                       (as(<string>, symbol)) end));

      You can return the items in the gadget as follows:

      .. code-block:: dylan

         gadget-items(\*g\*);

      This returns the symbol: ``#(#"one", #"two", #"three")``.

   See also
   - :gf:`gadget-items-setter`
   - :gf:`gadget-label-key`
   - :gf:`gadget-selection`
   - :gf:`gadget-value-key`

.. generic-function:: gadget-items-setter

   Sets the items for the specified gadget.

   :signature: gadget-items-setter *items gadget* => *items*

   :param items: An instance of type :drm:`<sequence>`.
   :param gadget: An instance of type :class:`<collection-gadget>`.
   :value items: An instance of type :drm:`<sequence>`.

   :description:

     Sets the items for *gadget* to the items specified by *items*.

   :example:

     .. code-block:: dylan

        *gadget* := contain(make
                      (<radio-box>,
                       items: range(from: 0, to: 20)));

        gadget-items(*gadget*) := range(from: 0, to: 15);

   See also

   - :gf:`gadget-items`

.. generic-function:: gadget-key-press-callback

   Returns the key-press callback for the specified gadget.

   :signature: gadget-key-press-callback *gadget* => *key-press-callback*

   :param gadget: An instance of type :class:`<collection-gadget>`.
   :value key-press-callback: An instance of type ``false-or(<frames.htm#40934>, <function>)``.

   :description:

     Returns the key-press callback for *gadget*. The key-press callback is
     the callback invoked when a key on the keyboard is pressed while the
     gadget has focus. They are of most use in tab controls, list controls,
     table controls, graph controls, and tree controls.

     In Windows, a good use for the key-press callback would be to mirror the
     behavior of Windows Explorer, where typing a filename, or part of a
     filename, selects the first file in the current folder whose name
     matches that typed.

   See also

   - :gf:`gadget-key-press-callback-setter`
   - :class:`<list-control>`
   - :class:`<tab-control>`
   - :class:`<table-control>`
   - :class:`<tree-control>`

.. generic-function:: gadget-key-press-callback-setter

   Sets the key-press callback for the specified gadget.

   :signature: gadget-key-press-callback-setter *key-press-callback* *gadget * => *key-press-callback*

   :param key-press-callback: An instance of type ``false-or(<frames.htm#40934>, <function>)``.
   :param gadget: An instance of type :class:`<collection-gadget>`.
   :value key-press-callback: An instance of type ``false-or(<frames.htm#40934>, <function>)``.

   :description:

     Sets the key-press callback for *gadget*. The key-press callback is the
     callback invoked when a key on the keyboard is pressed while the gadget
     has focus. They are of most use in tab controls, list controls, table
     controls, graph controls, and tree controls.

     In Windows, a good use for the key-press callback would be to mirror the
     behavior of Windows Explorer, where typing a filename, or part of a
     filename, selects the first file in the current folder whose name
     matches that typed.

   See also

   - :gf:`gadget-key-press-callback`
   - :class:`<list-control>`
   - :class:`<tab-control>`
   - :class:`<table-control>`
   - :class:`<tree-control>`

.. generic-function:: gadget-label

   Returns the label for the specified gadget.

   :signature: gadget-label *gadget* => *label*

   :param gadget: An instance of type :class:`<gadget>`.
   :value label: An instance of type ``type-union(<string>, <image>)``.

   :description:

     Returns the label for *gadget*.

   :example:

    .. code-block:: dylan

       *gadget* := contain(make(<button>, label: "Hello"));
       gadget-label(*gadget*);

   See also

   - :gf:`gadget-label-key`
   - :gf:`gadget-label-setter`

.. generic-function:: gadget-label-key

   Returns the function that is used to compute the label for the items in
   the specified gadget.

   :signature: gadget-label-key *gadget* => *label-key*

   :param gadget: An instance of type :class:`<collection-gadget>`.
   :value label-key: An instance of type ``<function>``.

   :description:

     Returns the function that is used to compute the labels for the items in
     *gadget*. Using a label key can be a useful way of consistently
     specifying labels that are a mapping of, but not directly equivalent to,
     the item names. As shown in the example, it is possible to force the
     case of item labels, and this is useful if the items are specified as
     symbol names, rather than strings.

   :example:

     The following code creates a list box whose items are the lower-cased
     equivalents of the symbols stated.

     .. code-block:: dylan

        *gadget* := contain
                     (make(<list-box>,
                           items: #(#"One", #"Two", #"Three"),
                           label-key:
                             method (symbol)
                               as-lowercase
                                 (as(<string>, symbol))
                             end));

     The label key function can be returned as follows:

     .. code-block:: dylan

        gadget-label-key(*gadget*);

   See also

   - :gf:`gadget-label`
   - :gf:`gadget-label-setter`
   - :gf:`gadget-value-key`

.. generic-function:: gadget-label-setter

   Sets the label for the specified gadget.

   :signature: gadget-label-setter *label gadget* => *label*

   :param label: An instance of type ``type-union(<string>, <image>)``.
   :param gadget: An instance of type :class:`<gadget>`.
   :value label: An instance of type ``type-union(<string>, <image>)``.

   :description:

     Sets the label for *gadget* to *label*. The *label* must be ``#f``, a
     string, or an instance of :class:`<image>`. Changing the
     label of a gadget may result in invoking the layout protocol on the
     gadget and its ancestor sheets, if the new label occupies a different
     amount of space than the old label.

   :example:

     .. code-block:: dylan

        *gadget* := contain(make(<button>, label: "Hello"));
        gadget-label(*gadget*) := "Hello world";

   See also

   - :gf:`gadget-label`
   - :gf:`gadget-label-key`

.. generic-function:: gadget-mnemonic

   Returns the mnemonic for the specified gadget.

   :signature: gadget-mnemonic *gadget* => *mnemonic*

   :param gadget: An instance of type :class:`<gadget>`.
   :value mnemonic: An instance of type ``false-or(<character>)``.

   :description:

     Returns the mnemonic for *gadget*. On Windows, the mnemonic is
     displayed as an underlined character in the label of the gadget, and
     pressing the key for that character activates the gadget or gives it the
     focus.

   See also

   - :gf:`gadget-accelerator`
   - :gf:`gadget-mnemonic-setter`

.. generic-function:: gadget-mnemonic-setter

   Sets the mnemonic for the specified gadget.

   :signature: gadget-mnemonic-setter *mnemonic gadget* => *mnemonic*

   :param mnemonic: An instance of type ``false-or(<character>)``.
   :param gadget: An instance of type :class:`<gadget>`.
   :value mnemonic: An instance of type ``false-or(<character>)``.

   :description:

     Sets the mnemonic for *gadget* to *mnemonic*. On Windows, the mnemonic
     is displayed as an underlined character in the label of the gadget, and
     pressing the key for that character activates the gadget or gives it the
     focus.

   See also

   - :gf:`gadget-accelerator-setter`
   - :gf:`gadget-mnemonic`

.. generic-function:: gadget-orientation

   Returns the orientation of the specified gadget.

   :signature: gadget-orientation *gadget* => *orientation*

   :param gadget: An instance of type :class:`<gadget>`.
   :value orientation: An instance of ``type one-of(#"horizontal", #"vertical", #"none")``.

   :description:

     Returns the orientation of *gadget*: either horizontal or vertical.

   :example:

     The following code creates a vertical row of buttons:

     .. code-block:: dylan

     *buttons* := contain(make(<button-box>,
                               selection-mode: #"multiple",
                               orientation: #"vertical",
                               items: range(from: 0, to: 5)));

     The orientation can be returned as follows:

     .. code-block:: dylan

        gadget-orientation(*buttons*);

.. generic-function:: gadget-popup-menu-callback

   Returns the popup menu callback of the specified gadget.

   :signature: gadget-popup-menu-callback *gadget* => *popup-menu-callback*

   :param gadget: An instance of type :class:`<gadget>`.
   :value popup-menu-callback: An instance of type ``<function>``.

   :description:

     Returns the popup menu callback of *gadget*. This is typically a
     function that is used to create a context-sensitive menu of available
     commands. It is generally invoked when the user right clicks on the
     gadget.

   See also

   - :gf:`gadget-popup-menu-callback-setter`

.. generic-function:: gadget-popup-menu-callback-setter

   Sets the popup menu callback of the specified gadget.

   :signature: gadget-popup-menu-callback-setter *popup-menu-callback gadget* => *popup-menu-callback*

   :param popup-menu-callback: An instance of type ``<function>``.
   :param gadget: An instance of type :class:`<gadget>`.
   :value popup-menu-callback: An instance of type ``<function>``.

   :description:

     Sets the popup menu callback of *gadget* to *function*. The function
     should typically create a menu of commands suited to the context in
     which the function is called. The function is generally invoked by
     right-clicking on the gadget.

   See also

   - :gf:`gadget-popup-menu-callback`

.. generic-function:: gadget-ratios

   Returns the ratios of the windows in *splitter*. This generic function
   lets you query the position of a splitter.

   :signature: gadget-ratios *splitter* => *ratios*

   :param splitter: An instance of type :class:`<splitter>`.
   :value ratios: An instance of type ``false-or(<sequence>)``.

.. generic-function:: gadget-ratios-setter

   Sets the ratios of the windows in *splitter*. This generic function
   lets you set the position of a splitter.

   :signature: gadget-ratios-setter *ratios splitter* => *ratios*

   :param ratios: An instance of type ``false-or(<sequence>)``.
   :param splitter: An instance of type :class:`<splitter>`.
   :value ratios: An instance of type ``false-or(<sequence>)``.

   :description:

     Set *ratios* to ``#f`` if you do not care what ratios are used.

.. generic-function:: gadget-read-only?

   Returns true if the gadget is editable.

   :signature: gadget-read-only? *gadget* => *read-only?*

   :param gadget: An instance of type :class:`<gadget>`.
   :value read-only?: An instance of type ``<boolean>``.

   :description:

     Returns true if *gadget* is read-only. The read-only attribute of a
     gadget is of most use with text gadgets.

   See also

   - :gf:`gadget-enabled?`

.. generic-function:: gadget-scrolling-horizontally?

   Returns true if the specified gadget has an associated horizontal scroll
   bar.

   :signature: gadget-scrolling-horizontally? *gadget* => *horizontal?*

   :param gadget: An instance of type :class:`<gadget>`.
   :value horizontal?: An instance of type ``<boolean>``.

   :description:

     Returns true if the *gadget* has an associated horizontal scroll bar,
     false otherwise.

   See also

   - :gf:`gadget-scrolling-vertically?`

.. generic-function:: gadget-scrolling-vertically?

   Returns true if the specified gadget has an associated vertical scroll
   bar.

   :signature: gadget-scrolling-vertically? *gadget* => *vertical?*

   :param gadget: An instance of type :class:`<gadget>`.
   :value vertical?: An instance of type ``<boolean>``.

   :description:

     Returns true if the *gadget* has an associated vertical scroll bar,
     false otherwise.

   See also

   - :gf:`gadget-scrolling-horizontally?`

.. generic-function:: gadget-selection

   Returns the currently selected items of the specified gadget.

   :signature: gadget-selection *gadget* => *selection*

   :param gadget: An instance of type :class:`<collection-gadget>`.
   :value selection: An instance of type ``limited(<sequence>, of: <integer>)``.
     Default value: ``#[]``.

   :description:

     Returns the keys for the currently selected items of *gadget*.
     Generally, you should use :gf:`gadget-value` to
     return the selected item, rather than :gf:`gadget-selection`, which is best
     used for handling repeated items.

     Single selection gadgets (such as radio boxes) always have exactly one
     key selected. Multiple selection gadgets (such as check boxes) have zero
     or more keys selected. The value of a collection gadget is determined by
     calling the value key of the gadget on each selected item in the gadget.

   :example:

     Create a radio box as follows:

     .. code-block:: dylan

        *radio* := contain(make(<radio-box>,
                                items: range(from: 0, to: 5)));

     Select one of the items in the radio box. This selection can be returned
     with:

     .. code-block:: dylan

        gadget-selection(*radio*);

   See also

   - :gf:`gadget-items`
   - :gf:`gadget-selection-mode`
   - :gf:`gadget-selection-setter`
   - :gf:`gadget-value`

.. generic-function:: gadget-selection-mode

   Returns the type of selection for the specified gadget.

   :signature: gadget-selection-mode *gadget* => *selection-mode*

   :param gadget: An instance of type :class:`<collection-gadget>`.
   :param selection-mode: An instance of ``type one-of(#"single", #"multiple",
     #"none")``.

   :description:

     Returns the selection mode for *gadget*. Typically, gadgets are either
     single or multiple selection (that is, either only one item can be
     selected at a time, or any number of items can be selected), or there is
     no selection behavior (items cannot be selected). Some gadgets, such as
     list boxes and button boxes, can choose a selection mode at
     initialization time using the ``selection-mode:`` init-keyword.

   :example:

     Create a radio box as follows:

     .. code-block:: dylan

        *radio* := contain(make(<radio-box>,
                                items: range(from: 0, to: 5)));

     The selection mode of the radio box is returned with:

     .. code-block:: dylan

        gadget-selection-mode(*radio*);

     Because the gadget is a radio box, only one item of which may be
     selected at a time, the selection mode returned is ``#"single"``.

   See also

   - :gf:`<button-box>`
   - :gf:`gadget-selection`
   - :gf:`gadget-selection-setter`
   - :class:`<list-box>`

.. generic-function:: gadget-selection-setter

   Sets the selection of the specified gadget.

   :signature: gadget-selection-setter *selection gadget* #key *do-callback?* => *selection*

   :param selection: An instance of type ``limited(<sequence>, of: <integer>)``.
   :param gadget: An instance of type :class:`<collection-gadget>`.
   :param do-callback?: An instance of type ``<boolean>``. Default value: ``#f``.

   :value selection: An instance of type ``limited(<sequence>, of: <integer>)``.

   :description:

     Sets the selection of *gadget*. When setting the selection, you need to
     be wary of the selection mode for *gadget*. It is an error to try to
     set multiple items in a single selection mode gadget.

     If *do-callback?* is true, the selection callback for *gadget* is
     invoked.

     As with :gf:`gadget-selection`, you should usually
     use :gf:`gadget-value-setter` to set the selected
     item, rather than *gadget-selection-setter*, which is best used for
     handling repeated items. See :gf:`gadget-selection`
     for more details.

   :example:

     Create a radio box as follows:

     .. code-block:: dylan

        *radio* := contain(make(<radio-box>,
                                items: range(from: 0, to: 5)));

     You can select the third item with:

     .. code-block:: dylan

        gadget-selection(*radio*, do-callback?: #t) := #[3];

     This sets the appropriate item, and invokes the callback that would have
     been invoked had the item been set manually, rather than
     programmatically (assuming that such a callback has been defined).

   See also

   - :gf:`gadget-selection`
   - :gf:`gadget-selection-mode`
   - :gf:`gadget-value-setter`

.. generic-function:: gadget-slug-size

   Returns the slug size of the specified gadget.

   :signature: gadget-slug-size *gadget* => *slug-size*

   :param gadget: An instance of type :class:`<scroll-bar>`.
   :value slug-size: An instance of type ``<real>``.

   :description:

     Returns the slug size of *gadget*. The slug is the part of *gadget*
     that can be dragged using the mouse. The value returned uses the same
     units as those used for :gf:`gadget-value-range`.

     .. note:: The Microsoft Windows Interface Guidelines refer to the slug as
        a *scroll-box*, and the area in which the slug can slide as the
        *scroll-shaft*. You should be aware of this difference if you are using
        those guidelines as a reference.

   See also

   - :gf:`gadget-slug-size-setter`
   - :gf:`gadget-value-range`

.. generic-function:: gadget-slug-size-setter

   Sets the slug size of the specified gadget.

   :signature: gadget-slug-size-setter *slug-size gadget* => *slug-size*

   :param slug-size: An instance of type ``<real>``.
   :param gadget: An instance of type :class:`<gadget>`.
   :value slug-size: An instance of type ``<real>``.

   :description:

     Sets the slug size of *gadget*. The value should use the same units as
     those used for :gf:`gadget-value-range`.

     .. note:: The Microsoft Windows Interface Guidelines refer to the slug as
        a *scroll-box*, and the area in which the slug can slide as the
        *scroll-shaft*. You should be aware of this difference if you are using
        those guidelines as a reference.

   See also

   - :gf:`gadget-slug-size`

.. generic-function:: gadget-test

   Returns the test function for the specified gadget.

   :signature: gadget-test *gadget* => *gadget-test*

   :param gadget: An instance of type :class:`<collection-gadget>`.
   :value gadget-test: An instance of type ``<function>``.

   :description:

     Returns the test function for the specified gadget. This function is
     used to test whether two items of the collection are considered
     identical.

.. generic-function:: gadget-text

   Returns the text for the specified gadget.

   :signature: gadget-text *gadget* => *gadget-text*

   :param gadget: An instance of type :class:`<text-gadget>`.
   :value gadget-text: An instance of type :drm:`<string>`.

   :description:

     Returns the text for the specified gadget.

   :example:

     First, create and display a text field by typing the following into an
     interactor:

     .. code-block:: dylan

        *g* := contain(make(<text-field>,
                            value-type: <integer>));

     Next, type something into the text field. You can return the text string
     that you just typed with the following form:

     .. code-block:: dylan

        gadget-text(*g*);

   See also

   - :gf:`gadget-text-setter`
   - :class:`<text-gadget>`

.. generic-function:: gadget-text-setter

   Sets the text for the specified gadget.

   :signature: gadget-text *gadget-text* *gadget* => *gadget-text*

   :param gadget-text: An instance of type :drm:`<string>`.
   :param gadget: An instance of type :class:`<text-gadget>`.
   :value gadget-text: An instance of type :drm:`<string>`.

   :description:

     Sets the text for the specified gadget.

   :example:

     First, create and display a text field by typing the following into an
     interactor:

     .. code-block:: dylan

       *g* := contain(make(<text-field>,
                           value-type: <integer>));

     Next, set the value of the text field with the following form:

     .. code-block:: dylan

        gadget-text-setter("Hello world", *g*);

   See also

   - :gf:`gadget-text`
   - :class:`<text-gadget>`

.. generic-function:: gadget-value

   Returns the gadget value of the specified gadget.

   :signature: gadget-value *gadget* => *gadget-value*

   :param gadget: An instance of type :class:`<value-gadget>`.
   :value gadget-value: An instance of type :drm:`<object>`.

   :description:

     Returns the gadget value of the specified gadget.

     The interpretation of the value varies from gadget to gadget. Most
     gadgets conceptually have "raw" values that can be determined directly
     using the generic function appropriate to the gadget class concerned
     (:gf:`gadget-text` for an instance of :class:`<text-gadget>`,
     :gf:`gadget-selection` for an instance of :class:`<collection-gadget>`,
     and so on). These gadget classes also have a convenience method on
     :gf:`gadget-value` that wraps up the raw value in some useful way. So,
     text gadgets have a method on :gf:`gadget-value` that converts the
     :gf:`gadget-text` based on the :gf:`gadget-value-type`, for example
     converting the string to an integer for ``value-type: <integer>``.

     The :gf:`gadget-value` method for collection gadgets is different for single
     and multiple selection gadgets. For single selection, the item that is
     selected is returned. For multiple selection, a sequence of the selected
     items is returned.

     .. Note:: If the gadget ID has been specified for a tab control, then this
        is returned as the gadget value.

   :example:

     Create a radio button:

     .. code-block:: dylan

        *radio* := contain(make(<radio-button>,
                                label: "Radio"));

     The gadget value of ``*radio*`` can be returned as follows:

     .. code-block:: dylan

        gadget-value(*radio*);

     If the radio button is selected, :gf:`gadget-value` returns ``#t``. If not
     selected, :gf:`gadget-value` returns ``#f``.

   See also

   - :class:`<gadget>`
   - :gf:`gadget-id`
   - :gf:`gadget-value-key`
   - :gf:`gadget-value-range`
   - :gf:`gadget-value-setter`
   - :gf:`gadget-value-type`

.. generic-function:: gadget-value-changed-callback

   Returns the value-changed callback of the specified gadget.

   :signature: gadget-value-changed-callback *gadget* => *value-changed-callback*

   :param gadget: An instance of type :class:`<value-gadget>`.
   :value value-changed-callback: An instance of type ``false-or(<function>)``.

   :description:

     Returns the value-changed callback of *gadget*. This is the callback
     function that is called once the gadget value of *gadget* has been
     changed.

     The value-changed callback function is invoked with one argument, the
     gadget.

     If :gf:`gadget-value-changed-callback` returns ``#f``, there is no value
     changed callback for *gadget*.

   See also

   - :gf:`gadget-value-changed-callback-setter`

.. generic-function:: gadget-value-changed-callback-setter

   Sets the value-changed-callback of the specified gadget.

   :signature: gadget-value-changed-callback-setter *callback gadget* => *callback*

   :param callback: An instance of type ``false-or(<function>)``.
   :param gadget: An instance of type :class:`<gadget>`.
   :value callback: An instance of type ``false-or(<function>)``.

   :description:

     Sets the value-changed callback of *gadget* to *function*. This is the
     callback function that is called once the gadget value of *gadget* has
     been changed.

     The value-changed callback function is invoked with one argument, the
     gadget.

   See also

   - :gf:`gadget-value-changed-callback`

.. generic-function:: gadget-value-changing-callback

   Returns the value changing callback of the specified gadget.

   :signature: gadget-value-changing-callback *gadget* => *value-changing-callback*

   :param gadget: An instance of type :class:`<gadget>`.
   :value value-changing-callback: An instance of type ``<function>``.

   :description:

     Returns the function that will be called when the value of *gadget* is
     in the process of changing, such as when a slider is being dragged. The
     *function* will be invoked with a two arguments, *gadget* and the new
     value.

   See also

   - :gf:`gadget-value-changing-callback-setter`

.. generic-function:: gadget-value-changing-callback-setter

   Sets the value-changing callback of the specified gadget.

   :signature: gadget-value-changing-callback-setter *value-changing-callback gadget* => *value-chaning-callback*

   :param value-changing-callback: An instance of type ``<function>``.
   :param gadget: An instance of type :class:`<gadget>`.
   :value value-changing-callback: An instance of type ``<function>``.

   :description:

     Sets the function that will be called when the value of *gadget* is in
     the process of changing, such as when a slider is being dragged. The
     *function* will be invoked with a two arguments, *gadget* and the new
     value.

   See also

   - :gf:`gadget-value-changing-callback`

.. generic-function:: gadget-value-key

   Returns the function that is used to calculate the gadget value of the
   specified gadget.

   :signature: gadget-value-key *gadget* => *value-key*

   :param gadget: An instance of type :class:`<collection-gadget>`.
   :value value-key: An instance of type ``<function>``. Default value: :drm:`identity`.

   :description:

     Returns the function that is used to calculate the gadget value of
     *gadget*, given the selected items. The function takes an item and
     returns a value.

   :example:

     The list box defined below has three items, each of which is a pair of
     two symbols. A label-key and a value-key is defined such that the label
     for each item is calculated from the first symbol in each pair, and the
     gadget value is calculated from the second.

     .. code-block:: dylan

        *list* := contain(make(<list-box>,
                               items: #(#("One", #"one"),
                                      #("Two", #"two"),
                                      #("Three", #"three")),
                               label-key: first,
                               value-key: second));

     This ensures that while the label of the first item is displayed
     on-screen as *One*, the value returned from that item is ``#"one"``,
     and similarly for the other items in the list.

     The gadget value key function can be returned with:

     .. code-block:: dylan

        gadget-value-key(*list*);

   See also

   - :gf:`gadget-label-key`
   - :gf:`gadget-value`

.. generic-function:: gadget-value-range

   Returns the range of values for the specified gadget.

   :signature: gadget-value-range *gadget* => *range*

   :param gadget: An instance of type :class:`<value-range-gadget>`.
   :values range: An instance of type :drm:`<range>`.

   :description:

     Returns the range of values for *gadget*. The value range is the
     elements represented by the range specified for gadget.

     .. note:: The value range is not simply the difference between the maximum
        and minimum values in the range. Consider the following range:

        .. code-block:: dylan

           range (from: 10, to: 0, by: -2)

        In this case, the value range is the elements 10, 8, 6, 4, 2, 0.

     The units in which the range is specified are also used for
     :gf:`gadget-slug-size`.

   :example:

     You can create a slider with a given range as follows:

     .. code-block:: dylan

        *slider* := contain(make(<slider>,
                                 value-range: range(from: -20,
                                                    to: 20,
                                                    by: 5)));

     You can return the range of this gadget by executing the following:

     .. code-block:: dylan

        gadget-value-range(*slider*);

     which in this case returns *{range -20 through 20, by 5}*.

   See also

   - :gf:`gadget-slug-size`
   - :gf:`gadget-value`
   - :gf:`gadget-value-range-setter`

.. generic-function:: gadget-value-range-setter

   Sets the range of values for the specified gadget.

   :signature: gadget-value-range-setter *range gadget* => *range*

   :param range: An instance of type :drm:`<range>`.
   :param gadget: An instance of type :class:`<value-range-gadget>`.
   :value range: An instance of type :drm:`<range>`.

   :description:

     Sets the range of values for *gadget*. The value range is the elements
     represented by the range specified for gadget.

   :example:

     Create a slider without specifying a range:

     .. code-block:: dylan

        *slider* := contain(make(<slider>);

     You can specify the range of this gadget by executing the following:

     .. code-block:: dylan

        gadget-value-range(*slider*) :=
          (range (from: -20 to: 20, by: 5});

   See also

   - :gf:`gadget-value-range`

.. generic-function:: gadget-value-setter

   Sets the gadget value of the specified gadget.

   :signature: gadget-value-setter *value gadget* #key *do-callback?* => *value*

   :param value: An instance of type :drm:`<object>`.
   :param gadget: An instance of type :class:`<value-gadget>`.
   :param do-callback?: An instance of type ``<boolean>``. Default value: ``#f``.
   :value value: An instance of type :drm:`<object>`.

   :description:

     Sets the gadget value of *gadget*.

     The *value* that you need to specify varies from gadget to gadget. For
     example, for a scroll bar, *value* might be a number between 0 and 1,
     while for a radio button, *value* is either true or false.

     If *do-callback?* is true, the value-changed callback for *gadget* is
     invoked.

   :example:

     Create a radio button:

     .. code-block:: dylan

        *radio* := contain(make(<radio-button>,
                                label: "Radio"));

     The gadget value of ``*radio*`` can be set with either of the following:

     .. code-block:: dylan

        gadget-value(*radio*) := #t;
        gadget-value(*radio*) := #f;

     Setting the gadget value to ``#t`` selects the button, and setting it to
     ``#f`` deselects it.

   See also

   - :gf:`gadget-value`

.. generic-function:: gadget-value-type

   Returns the type of the gadget value for the specified gadget.

   :signature: gadget-value-type *gadget* => *type*

   :param gadget: An instance of type :class:`<value-gadget>`.
   :value type: An instance of type :drm:`<type>`.

   :description:

     Returns the type of the gadget value for *gadget*.

   :example:

     The following code creates a text field, the contents of which are
     constrained to be an integer.

     .. code-block:: dylan

        *numeric* := contain(make(<text-field>,
                                  value-type: <integer>));

     Evaluating the following code confirms the gadget value type to be the
     class ``<integer>``.

     .. code-block:: dylan

        gadget-value-type(*numeric*);

   See also

   - :gf:`gadget-value`

.. generic-function:: gadget-x-alignment

   Returns the horizontal alignment of the specified gadget.

   :signature: gadget-x-alignment *gadget* => *alignment*

   :param gadget: An instance of type :class:`<gadget>`.
   :value alignment: An instance of type ``one-of(#"left", #"right",
     #"center")``.

   :description:

     Returns the horizontal alignment of *gadget*. You can only set the
     horizontal alignment of a gadget when first initializing that gadget,
     using the ``x-alignment:`` init-keyword.

   See also

   - :gf:`gadget-y-alignment`

.. generic-function:: gadget-y-alignment

   Returns the vertical alignment of the specified gadget.

   :signature: gadget-x-alignment *gadget* => *alignment*

   :param gadget: An instance of type :class:`<gadget>`.
   :value alignment: An instance of type ``one-of(#"top", #"bottom",
     #"center")``.

   :description:

     Returns the vertical alignment of *gadget*. You can only set the
     vertical alignment of a gadget when first initializing that gadget,
     using the ``y-alignment:`` init-keyword.

   See also

   - :gf:`gadget-x-alignment`

.. class:: <group-box>
   :open:
   :abstract:
   :instantiable:

   The class of gadgets that group their children using a labelled border.

   :superclasses: :class:`<gadget>`

   :keyword label: An instance of type :class:`<label>`.
   :keyword label-position: An instance of type ``one-of(#"top", #"bottom")``.
     Default value: ``#"top"``.

   :description:

     The class of gadgets that group their children using a labelled border.
     You can use this gadget class to group together a number of related
     items visually.

     .. figure:: images/gadgets-22.png
        :align: center

        A group box

     The ``label:`` init-keyword specifies a string or icon that is to be used
     as a label for the gadget.

     The ``label-position:`` init-keyword is used to specify whether the label
     should be displayed along the top or the bottom edge of the border.

     Internally, this class maps into the Windows group box control.

   :example:

     .. code-block:: dylan

        contain(make(<group-box>,
                     child: make(<radio-box>, items: #(1,2,3,4),
                     orientation: #"vertical"),
                     label: "Select integer:"));

   See also

   - :class:`<border>`
   - :class:`<check-box>`
   - :class:`<push-box>`
   - :class:`<radio-box>`

.. generic-function:: item-object

   Returns the Dylan object representing an item in a list or table
   control.

   :signature: item-object *item* => *object*

   :param item: An instance of type ``type-union(<list-item>, <table-item>)``.
   :value object: An instance of type :drm:`<object>`.

   :description:

     Returns the Dylan object representing an item in a list or table
     control.

.. class:: <label>
   :open:
   :abstract:
   :instantiable:

   The class of label gadgets.

   :superclasses: :class:`<gadget>`

   :keyword label: An instance of type ``type-union(<string>, <image>)``.

   :description:

     The class of label gadgets.

     The ``label:`` init-keyword specifies a string or image that is to be used
     as a label for the gadget. If you use an image, you should be wary of
     its size: only use images that are the size of a typical icon.

     Internally, this class maps into the Windows static control.

   :operations:

     - :gf:`gadget-label`
     - :gf:`gadget-label-setter`
     - `<frames.htm#74637>`
     - `<frames.htm#10131>`
     - `<frames.htm#68823>`
     - `<frames.htm#14565>`

   :example:

     .. code-block:: dylan

        contain(make(<label>, label: "Hello"));

   See also

   :macro:`labelling`

.. macro:: labelling
   :statement:

    Creates the specified sheet and assigns a label to it.

   :macrocall:

     .. code-block:: dylan

        labelling ([*options* ]) {*pane* } end

   :param options: Dylan arguments *bnf*.
   :param pane: A Dylan expression *bnf*.

   :description:

     Creates *pane* with a label assigned to it, taking into account any of
     the specified *options*.

     The options specified may be any of the legal init-keywords used to
     specify an instance of :class:`<label>`. If no options are specified,
     then the default label is used.

     The *pane* is an expression whose return value is the sheet to which the
     label should be assigned.

   :example:

     .. code-block:: dylan

        labelling ("Color Type:")
          make(<check-box>, items: #("Color", "Monochrome"))
        end;

   See also

   - :class:`<label>`

.. class:: <list-box>
   :open:
   :abstract:
   :instantiable:

   The class of list boxes.

   :superclasses: :class:`<collection-gadget>` :class:`<action-gadget>`

   :keyword borders: An instance of type ``one-of(#f, #"none", #"flat",
     #"sunken", #"raised", #"ridge", #"groove", #"input", #"output")``.
     Default value: ``#f``.
   :keyword scroll-bars: An instance of type ``one-of(#f, #"none",
     #"horizontal", #"vertical", #"both", #"dynamic")``.
     Default value: ``#"both"``.

   :description:

     .. figure:: images/gadgets-23.png
        :align: center

        The class of list boxes.

     The ``borders:`` init-keyword lets you specify a border around the list
     box. If specified, a border of the appropriate type is drawn around the
     gadget.

     The ``scroll-bars:`` init-keyword lets you specify the presence of scroll
     bars around the gadget. By default, both horizontal and vertical scroll
     bars are created. You can also force the creation of only horizontal or
     vertical scroll bars, or you can create scroll bars dynamically: that
     is, have them created only if necessary, dependent on the size of the
     gadget. If ``scroll-bars:`` is ``#f``, no scroll bars are added to the
     gadget.

     Internally, this class maps into the Windows list box control.

   :example:

     The following creates a list of three items, without scroll bars.

     .. code-block:: dylan

        *list* := contain(make(<list-box>,
                               items: #(#("One", #"one"),
                                 #("Two", #"two"),
                                 #("Three", #"three")),
                               label-key: first,
                               value-key: second,
                               scroll-bars: #f));

   See also

   - :class:`<list-control>`
   - :class:`<list-item>`

.. class:: <list-control>
   :open:
   :abstract:
   :instantiable:

   The class of list controls.

   :superclasses: :class:`<collection-gadget>` :class:`<action-gadget>`

   :keyword icon-function: An instance of type ``<function>``.
   :keyword view: An instance of type :class:`<list-control-view>`.
     Default value: ``#"list"``.
   :keyword borders: An instance of type ``one-of(#f, #"none", #"flat",
     #"sunken", #"raised", #"ridge", #"groove", #"input", #"output")``.
     Default value: ``#f``.
   :keyword scroll-bars: An instance of type ``one-of(#f, #"none",
     #"horizontal", #"vertical", #"both", #"dynamic")``.
     Default value: ``#"both"``.
   :keyword popup-menu-callback: An instance of type ``<function>``.
   :keyword key-press-callback: An instance of type ``false-or(<frames.htm#40934>, <function>)``.

   :description:

     |image6| The class of list controls. These are controls that can list
     items in a number of different ways, using a richer format than the
     :class:`<list-box>` class. Examples of list controls are
     the main panels in the Windows Explorer, or the Macintosh Finder. List
     controls can also be seen in the standard Windows 95 Open File dialog
     box.

     The ``icon-function:`` init-keyword lets you specify a function to supply
     icons for display in the control. The function is called with the item
     that needs an icon as its argument, and it should return an instance of
     :class:`<image>` as its result. Typically, you might want to define an icon
     function that returns a different icon for each kind of item in the
     control. For example, if the control is used to display the files and
     directories on a hard disk, you would want to return the appropriate
     icon for each registered file type.

     The ``view:`` init-keyword can be used to specify the way in which the
     items in the list box are displayed. There are three options,
     corresponding to the view options that will be familiar to most users of
     GUI-based operating systems.

     The ``borders:`` init-keyword lets you specify a border around the list
     control. If specified, a border of the appropriate type is drawn around
     the gadget.

     The ``scroll-bars:`` init-keyword lets you specify the presence of scroll
     bars around the gadget. By default, both horizontal and vertical scroll
     bars are created. You can also force the creation of only horizontal or
     vertical scroll bars, or you can create scroll bars dynamically: that
     is, have them created only if necessary, dependent on the size of the
     gadget. If ``scroll-bars:`` is ``#f``, no scroll bars are added to the
     gadget.

     You can use the ``popup-menu-callback:`` init-keyword to specify a
     context-sensitive menu to display for one or more selected items in the
     list control. In Windows 95, for instance, such a context-sensitive menu
     can be displayed by right-clicking on any item or group of selected
     items in the list control.

     The ``key-press-callback:`` init-keyword lets you specify a key-press
     callback. This type of callback is invoked whenever a key on the
     keyboard is pressed while the gadget has focus. See
     :gf:`gadget-key-press-callback`, for a fuller description of
     key-press callbacks.

     Internally, this class maps into the Windows list view control.

   :operations:

     - :gf:`add-item`
     - :gf:`find-item`
     - :gf:`list-control-view`
     - :gf:`list-control-view-setter`
     - :gf:`make-item`
     - :gf:`remove-item`

   See also

   - :gf:`add-item`
   - :gf:`list-control-view`
   - :gf:`make-item`
   - :gf:`remove-item`

.. generic-function:: list-control-icon-function

   Returns the icon function for the specified list control.

   :signature: list-control-icon-function *list-control* => *icon-function*

   :param list-control: An instance of :class:`<list-control>`.
   :value icon-function: An instance of type ``<function>``.

   :description:

     Returns the icon-function for *list-control*. This function lets you
     specify which icon to display for each item in the control. The function
     is called with the item that needs an icon as its argument, and it
     should return an instance of :class:`<image>` as its result. Typically, you
     might want to define an icon function that returns a different icon for
     each kind of item in the control. For example, if the control is used to
     display the files and directories on a hard disk, you would want to
     return the appropriate icon for each registered file type.

     Note that, unlike tree controls, the icon function for a list control
     can be changed once the list control has been created.

   See also

   - :class:`<list-control>`
   - :gf:`list-control-icon-function-setter`

.. generic-function:: list-control-icon-function-setter

   Sets the icon function for the specified list control.

   :signature: list-control-icon-function-setter *icon-function* *list-control* => *icon-function*

   :param icon-function: An instance of type ``<function>``.
   :param list-control: An instance of :class:`<list-control>`.
   :value icon-function: An instance of type ``<function>``.

   :description:

     Sets the icon-function for *list-control*. This function lets you
     specify which icon to display for each item in the control. The function
     is called with the item that needs an icon as its argument, and it
     should return an instance of :class:`<image>` as its result. Typically, you
     might want to define an icon function that returns a different icon for
     each kind of item in the control. For example, if the control is used to
     display the files and directories on a hard disk, you would want to
     return the appropriate icon for each registered file type.

     Note that, unlike tree controls, the icon function for a list control
     can be changed once the list control has been created.

   See also

   - :class:`<list-control>`
   - :gf:`list-control-icon-function`

.. type:: <list-control-view>

   The type of possible views for a list control

   :equivalent: ``one-of(#"small-icon", #"large-icon", #"list")``

   :description:

     This type represents the acceptable values for the view arguments to
     operators of :class:`<list-control>`. You should not
     attempt to redefine this type in any way.

     There are three possible values, corresponding to the view options that
     will be familiar to most users of GUI-based operating systems:

     ``#"small-icon"``
       Displays each item in the list control using a small
       icon to the left of the item. Items are arranged horizontally.

     ``#"large-icon"``
       Displays each item in the list control using a large
       icon to the left of the item. Items are arranged horizontally.

     ``#"list"``
       Displays each item in the list control using a small icon
       to the left of the item. Items are arranged vertically in one column.

   See also

   - :class;`<list-control>`
   - :gf:`list-control-view`
   - :type:`<table-control-view>`

.. generic-function:: list-control-view

   Returns the view for the specified list control.

   :signature: list-control-view *list-control* => *view*

   :param list-control: An instance of :class:`<list-control>`.
   :value view: An instance of type :type:`<list-control-view>`.

   :description:

     Returns the view for *list-control*. The view defines how items in the
     list control are displayed. Three views are available; items are
     accompanied either by a small icon or a large icon. In addition, items
     can be listed vertically, and additional details can be displayed for
     each item. For more details, see the description for
     :type:`<list-control-view>`.

   :example:

     Given a list control created with the following code:

     .. code-block:: dylan

        *list* := contain(make(<list-control>,
                               items: #(#("One", #"one"),
                                 #("Two", #"two"),
                                 #("Three", #"three")),
                               view: #"list"
                               scroll-bars: #f));

     The list control view may be returned with:

     .. code-block:: dylan

        list-control-view(*list*);

   See also

   - :class:`<list-control>`
   - :type:`<list-control-view>`
   - :gf:`list-control-view-setter`

.. generic-function:: list-control-view-setter

   Sets the view for the specified list control.

   :signature: list-control-view-setter *view list-control* => *view*

   :param view: An instance of type :type:`<list-control-view>`.
   :param list-control: An instance of :class:`<list-control>`.
   :value view: An instance of type :type:`<list-control-view>`.

   :description:

     Sets the view for *list-control*. The view defines how items in the
     list control are displayed. Three views are available; items are
     accompanied either by a small icon or a large icon. In addition, items
     can be listed vertically, and additional details can be displayed for
     each item. For more details, see the description for
     :type:`<list-control-view>`.

   :example:

     Given a list control created with the following code:

     .. code-block:: dylan

        *list* := contain(make(<list-control>,
                               items: #("One",
                                        "Two",
                                        "Three")));

     The list control view may be specified with:

     .. code-block:: dylan

        list-control-view(*list*) := #"view";

   See also

   - :class:`<list-control>`
   - :type:`<list-control-view>`
   - :gf:`list-control-view`

.. class:: <list-item>
   :open:
   :abstract:
   :instantiable:

    The class that represents an item in a list control.

   :superclasses: :drm:`<object>`

   :keyword object: An instance of type :drm:`<object>`. Default value: ``#f``.

   :description:

     The class that represents an item in a list control.

   :operations:

     - :gf:`add-item`
     - :gf:`item-object`
     - :gf:`remove-item`

   See also

   - :class:`<list-control>`
   - :class:`<table-item>`

.. generic-function:: make-item

   Creates an item which can be inserted in the specified list control or
   table control.

   :signature: make-item *list-or-table object* #key *frame-manager* => *item*

   :param list-or-table: An instance of ``type-union(<list-control>, <table-control>)``.
   :param object: An instance of type :drm:`<object>`.
   :param #key frame-manager: An instance of type :class:`<frame-manager>`.
   :value item: An instance of type :class:`<list-item>`.

   :description:

     Creates an item that represents *object* which can be inserted in the
     specified *list-or-table*. To insert the item in the list control or
     table control, :gf:`add-item` is used. You would not normally call
     :gf:`make-item` explicitly: just use :gf:`add-item` and the item
     is created automatically before it is added to the list or table control.

     If the *frame-manager* argument is specified, then this is used instead
     of the default frame manager.

   See also

   - :gf:`add-item`
   - :gf:`find-item`
   - :class:`<list-control>`
   - :class:`<list-item>`
   - :gf:`remove-item`
   - :class:`<table-control>`
   - :class:`<table-item>`

.. generic-function:: make-menu-from-items

   Returns a menu object created from the specified items.

   :signature: make-menu-from-items *framem items* #key *owner title label-key value-key foreground background text-style* => *menu*

   :param framem: An instance of type :class:`<frame-manager>`.
   :param items: An instance of type :drm:`<sequence>`.
   :param #key owner: An instance of type :class:`<sheet>`.
   :param #key title: An instance of type :drm:`<string>``.
   :param #key label-key: An instance of ``<function>``. Default value: :drm:`identity`.
   :param #key value-key: An instance of ``<function>``. Default value: :drm:`identity`.
   :param #key foreground: An instance of type ``false-or(<ink>)``. Default value: ``#f``.
   :param #key background: An instance of type ``false-or(<ink>)``. Default value: ``#f``.
   :param #key text-style: An instance of type :class:`<text-style>`.
   :value menu: An instance of type :class:`<menu>`.

   :description:

     Returns a menu object created from the specified *items*.

     The *framem* argument lets you specify a frame manager.

     The *owner* argument is used to specify which sheet owns the menu. If
     you fail to supply this, then the menu will be owned by the entire
     screen.

     You can specify a *title*, if desired.

     The *label-key* and *value-key* can be functions used to compute the
     label and value for each item in the menu, respectively. For more
     information, see *`gadget-label-key`_*, or
     *`gadget-value-key`_*. In general, the label
     key can be trusted to "do the right thing" by default.

     The *text-style* argument specified a text style for the menu. The
     *foreground* and *background* arguments specify foreground and
     background colors for the menu: *foreground* being used for the text in
     the menu, and *background* for the menu itself.

   See also

   - :gf:`display-menu`

.. generic-function:: make-node

    Creates a node which can be inserted in the specified tree control.

   :signature: make-node *tree object* #key #all-keys => *node*

   :param tree: An instance of :class:`<tree-control>`.
   :param object: An instance of type :drm:`<object>`.
   :value node: An instance of type :class:`<tree-node>`.

   :description:

     Creates a node that represents *object* which can be inserted in the
     specified *tree*. To insert the item in the tree control, :gf:`add-node`
     is used. You would not normally call ``make-node`` explicitly: just use
     :gf:`add-node` and the node is created automatically before it is added
     to the tree control.

   See also

   - :gf:`add-node`
   - :gf:`find-node`
   - :gf:`remove-node`
   - :class:`<tree-control>`

.. class:: <menu>
   :open:
   :abstract:
   :instantiable:

   The class of menu gadgets.

   :superclasses: :class:`<gadget>` :class:`<multiple-child-composite-pane>`

   :keyword update-callback: An instance of type ``false-or(<function>)``.
   :keyword owner: An instance of type :class:`<sheet>`.
   :keyword mnemonic: An instance of type ``false-or(<character>)``.
     Default value: ``#f``.
   :keyword command: An instance of ``false-or(<frames.htm#40934>)``.
     Default value: ``#f``.

   :description:

     The class of menu gadgets.

     Support for dynamically modifying the contents of a menu is provided in
     the form of an update callback, If this is supplied using the
     ``update-callback:`` init-keyword, then it is invoked just before the menu
     is displayed. This callback is free to make changes to the contents of
     the menu, which will then appear when the update callback is complete.
     Note that you can also supply an update callback to any menu box which
     forms a part of the menu, using the relevant init-keyword to :class:<menu-box>`.

     The ``owner:`` argument is used to specify which sheet owns the menu. If
     you fail to supply this, then the menu will be owned by the entire
     screen.

     The ``mnemonic:`` init-keyword is used to specify a keyboard mnemonic for
     the button. This is a key press that involves pressing the ALT key
     followed by a number of alphanumeric keys.

     The ``command:`` init-keyword specifies a command that is invoked when the
     menu is chosen. For most menus, you should not specify a command;
     instead, you assign menu buttons as children to the menu, and the menu
     buttons themselves have commands specified. However, in the rare case
     where the menu has no children, and you want the menu itself to invoke a
     command, you can use this init-keyword.

     Internally, this class maps into the menu Windows control.

   :operations:

     - `<frames.htm#89020>`
     - :gf:`choose-from-dialog`
     - :gf:`choose-from-menu`
     - :gf:`display-menu`
     - :gf:`menu-owner`

   :example:

     The following code creates a menu, *Hello*, that contains a single
     button, *World*. Notice how using ``contain`` creates a menu bar for you
     automatically. You should note that using :gf:`display-menu` would not
     have this effect.

     .. code-block:: dylan

        *menu* := contain(make(<menu>,
                               label: "Hello",
                               children:
                                 vector
                                   (make(<menu-button>,
                                         label: "World"))));

   See also

   - :gf:`display-menu`
   - :gf:`make-menu-from-items`

.. class:: <menu-bar>
   :open:
   :abstract:
   :instantiable:

   The class of menu bar gadgets.

   :superclasses: :class:`<value-gadget>` :class:`<multiple-child-composite-pane>`

   :keyword update-callback: An instance of type ``<function>``.

   :description:

     The class of menu bar gadgets.

     Internally, this class maps into the Windows menu control.

   :operations:

     - `<frames.htm#63229>`
     - `<frames.htm#56600>`

   :example:

     The following example is similar to the example for
     :class:`<menu>`, except that here, the menu bar object is
     explicitly defined. In the example for :class:`<menu>`, it is created
     automatically by using ``contain``:

     .. code-block:: dylan

        *menu* := make(<menu-bar>,
                       children:
                         vector(make(<menu>,
                                     label: "Hello",
                                     children: vector
                                       (make(<menu-button>,
                                             label: "World")
        ))));

   See also

   - :class:`<menu>`

.. class:: <menu-box>
   :open:
   :abstract:
   :instantiable:

   A class that groups menu buttons.

   :superclasses: :class:`<collection-gadget>`

   :keyword update-callback: An instance of type ``false-or(<function>)``.

   :description:

     A class that groups menu buttons. Like the :class:`<button-box>`
     class, you can use this class to create groups of menu buttons
     that are related in some way. A visual separator is displayed
     in the menu in which a menu box is inserted,
     separating the menu buttons defined in the menu box from other menu
     buttons or menu boxes in the menu.

     An example of the way in which a menu box may be used is to implement
     the clipboard menu commands usually found in applications. A menu box
     containing items that represent the *Cut*, *Copy*, and *Paste*
     commands can be created and inserted into the *Edit* menu.

     Internally, this class maps into the menu Windows control.

     Support for dynamically modifying the contents of a menu box is provided
     in the form of an update callback, If this is supplied using the
     ``update-callback:`` init-keyword, then it is invoked just before the menu
     box is displayed (this usually occurs at the same time that the menu of
     which the menu box is a part is displayed). This callback is free to
     make changes to the contents of the menu box, which will then appear
     when the update callback is complete.

   :example:

     .. code-block:: dylan

        *menu-box* := contain(make(<menu-box>,
                                   items: range
                                     (from: 0, to: 5)));

   See also

   - :class:`<check-menu-box>`
   - :class:`<push-menu-box>`
   - :class:`<radio-menu-box>`

.. class:: <menu-button>
   :open:
   :abstract:
   :instantiable:

   The class of all buttons that can appear in menus.

   :superclasses: :class:`<button>`

   :keyword update-callback: An instance of type ``<function>``.

   :description:

     The class of all buttons that can appear on menus.

     You should take special care to define keyboard accelerators and
     keyboard mnemonics for any menu buttons you create. For a full
     discussion on this, see the entry for :class:`<button>`

     Internally, this class maps into the menu item Windows control.

   :example:

     .. code-block:: dylan

        contain
          (make(<menu-button>, label: "Hello",
                activate-callback:
                  method (gadget)
                    notify-user
                      (format-to-string
                        ("Pressed button %=", gadget),
                       owner: gadget) end));

   See also

   - :class:`<check-menu-button>`
   - :gf:`gadget-accelerator`
   - :class:`<menu-box>`
   - :class:`<push-menu-button>`
   - :class:`<radio-menu-button>`

.. generic-function:: menu-owner

   Returns the sheet that owns the specified menu.

   :signature: menu-owner *menu* => *sheet*

   :param menu: An instance of type :class:`<menu>`.
   :value sheet: An instance of type :class:`<sheet>`.

   :description:

     Returns the sheet that owns *menu*, that is, the sheet in which *menu*
     is displayed.

     Every menu should specify which sheet it is owned by. If this is not
     specified, then the menu will be owned by the entire screen.

.. generic-function:: node-children

   Returns the children of the specified node in a tree control.

   :signature: node-children *tree-node* => *children*

   :param tree-node: An instance of type :class:`<tree-node>`.
   :value children: An instance of type ``limited(<sequence>, of: <tree-node>)``.

   :description:

     Returns the children of *tree-node* in a tree control.

   See also

   - :gf:`node-children-setter`
   - :gf:`node-parents`
   - :gf:`tree-control-children-generator`
   - :class:`<tree-node>`

.. generic-function:: node-children-setter

   Sets the children of the specified node in a tree control.

   :signature: node-children-setter *children tree-node* => *children*

   :param children: An instance of type ``limited(<sequence>, of: <tree-node>)``.
   :param tree-node: An instance of type :class:`<tree-node>`.
   :value children: An instance of type ``limited(<sequence>, of: <tree-node>)``.

   :description:

     Sets the children of *tree-node* in a tree control.

   See also

   - :gf:`node-children`
   - :gf:`node-parents`
   - :gf:`tree-control-children-generator`
   - :class:`<tree-node>`

.. generic-function:: node-expanded?

   Returns true if the specified node is expanded in a tree control.

   :signature: node-expanded? *tree-node* => *expanded?*

   :param tree-node: An instance of type :class:`<tree-node>`.
   :value expanded?: An instance of type ``<boolean>``.

   :description:

     Returns true if *tree-node* is expanded in a tree control, so that its
     children are displayed in the tree control.

   See also

   - :class:`<tree-node>`

.. generic-function:: node-object

   Returns the object that the specified node in a tree control represents.

   :signature: node-object *tree-node* => *object*

   :param tree-node: An instance of type :class:`<tree-node>`.
   :value object: An instance of type :drm:`<object>`.

   :description:

      Returns the object that *tree-node* represents.

   See also

   - :class:`<tree-node>`

.. generic-function:: node-parents

   Returns the parents of the specified node in a tree control.

   :signature: node-parents *tree-node* => *parents*

   :param tree-node: An instance of type :class:`<tree-node>`.
   :value parents: An instance of type :drm:`<sequence>`.

   :description:

     Returns the parents of *tree-node* in a tree control.

   See also

   - :gf:`node-children`
   - :class:`<tree-node>`

.. generic-function:: node-state

   Returns the state of the specified node in a tree control.

   :signature: node-parents *tree-node* => *state*

   :param tree-node: An instance of type :class:`<tree-node>`.
   :value parents: An instance of type ``one-of(#"expanded", #"contracted", #f)``.

   :description:

     Returns the state of *tree-node* in a tree control, that is, whether it
     is currently expanded or contracted. This function returns ``#f`` if
     tree-node does not exist.

   See also

   - :gf:`node-expanded?`
   - :class:`<tree-node>`

.. class:: <option-box>
   :open:
   :abstract:
   :instantiable:

   The class of option boxes.

   :superclasses: :class:`<collection-gadget>`

   :keyword borders: An instance of type ``one-of(#f, #"none", #"flat",
     #"sunken", #"raised", #"ridge", #"groove", #"input", #"output")``.
     Default value: ``#f``.
   :keyword scroll-bars: An instance of type ``one-of(#f, #"none",
     #"horizontal", #"vertical", #"both", #"dynamic")``. Default value:
     ``#"both"``.

   :description:

     .. figure:: images/gadgets-25.png
        :align: center

        The class of option boxes.

     The ``borders:`` init-keyword lets you specify a border around the option
     box. If specified, a border of the appropriate type is drawn around the
     gadget.

     The ``scroll-bars:`` init-keyword lets you specify the scroll bar behavior
     for the gadget.

     Internally, this class maps into the Windows drop-down list control.

   See also

   - :class:`<combo-box>`

.. class:: <page>
   :open:
   :abstract:
   :instantiable:

   The class that represents a page in a tab control.

   :superclasses: :class:`<gadget>`

   :keyword label: An instance of type ``type-union(<string>, <image>)``.

   :description:

     The class that represents a page in a multi-page frame, such as a tab
     control or wizard frame or property frame.

     The *label:* init-keyword specifies a string or icon that is to be used
     as a label for the gadget. Pages typically appear inside a tab control,
     where the label for the page becomes the label on the tab for the page.

   :operations:

     - `<frames.htm#88015>`
     - `<frames.htm#89408>`

   See also

   - `<frames.htm#93333>`
   - :class:`<tab-control-page>`
   - `<frames.htm#87607>`

.. class:: <password-field>
   :open:
   :abstract:
   :instantiable:

   The class of text fields that do not echo typed text.

   :superclasses: :class:`<text-field>`

   :description:

     |image7| The class of text fields that do not echo typed text. This
     class of gadgets are very similar in appearance to the :class:`<text-field>`
     gadget, except that any text typed by the user is hidden in some way,
     rather than being echoed to the screen in the normal way.

     Internally, this class maps into the Windows single-line edit control
     with ES-PASSWORD style.

   :example:

     .. code-block:: dylan

        *pass* := contain(make(<password-field>));

   See also

   :class:`<text-field>`

.. class:: <progress-bar>
   :open:
   :abstract:
   :instantiable:

   The class of progress bar windows.

   :superclasses: :class:`<value-range-gadget>`

   :keyword orientation: An instance of type ``one-of(#"horizontal",
     #"vertical")``. Default value: ``#"horizontal"``.

   :description:

     .. figure:: images/gadgets-27.png
        :align: center

        The class of progress bar windows.

     The ``orientation:`` init-keyword lets you specify whether the progress
     bar should be horizontal or vertical.

     Internally, this class maps into the Windows progress indicator control.

   :example:

     The following code creates an "empty" progress bar:

     .. code-block:: dylan

        *prog* := contain
                    (make(<progress-bar>,
                          value-range:
                            range(from: 0, to: 100)));

     By setting the gadget value of the progress bar, the progress of a task
     can be monitored as follows:

     .. code-block:: dylan

        for (i from 0 to 100) gadget-value(*prog*) := i end;

   See also

   - :class:`<slider>`

.. class:: <push-box>

   :open:
   :abstract:
   :instantiable:

   The class of grouped push buttons.

   :superclasses: :class:`<button-box>` :class:`<action-gadget>`

   :description:

     .. figure:: images/gadgets-28.png
        :align: center

        The class of grouped push buttons.

     The :gf:`gadget-value` of a push box is always the
     gadget value of the last push button in the box to be pressed. You
     should use the gadget value of a push box as the way of determining
     which button has been pressed in a callback for the push box.

   :example:

     .. code-block:: dylan

        *push-box* := contain
                        (make(<push-box>,
                              items: range(from: 0, to: 5)));

   See also

   - :class:`<check-box>`
   - :class:`<group-box>`
   - :class:`<radio-box>`

.. class:: <push-button>
   :open:
   :abstract:
   :instantiable:

   The class of push buttons.

   :superclasses: :class:`<button>` :class:`<action-gadget>`

   :keyword default?: An instance of type ``<boolean>``. Default value: ``#f``.

   :description:

     .. figure:: images/gadgets-29.png
        :align: center

        The class of push buttons.The push button gadget provides
        press-to-activate switch behavior.

     When the button is activated (by releasing the pointer button over it),
     its activate callback is invoked.

     If you supply a :gf:`gadget-value` for a push button, this can be used
     by any callback defined on the push button.  This is especially useful
     in the case of push boxes, where this value can be used to test which
     button in the push box has been pressed.

     The ``default?:`` init-keyword sets the default property for the push
     button gadget. When true, the push button is drawn with a heavy border,
     indicating that it is the "default operation" for that frame. Usually,
     this means that pressing the Return key invokes the activate callback.

     Internally, this class maps into the push button Windows control.

   :example:

     The following code creates a push button which, when clicked, displays a
     message showing the label of the button.

     .. code-block:: dylan

        contain(make(<push-button>,
                     label: "Hello",
                     activate-callback:
                     method (gadget)
                       notify-user(format-to-string
                                     ("Pressed button %=",
                                      gadget-label(gadget)),
                                   owner: gadget) end));

   See also

   - :class:`<check-button>`
   - :class:`<radio-button>`

.. class:: <push-menu-box>
   :open:
   :abstract:
   :instantiable:

   The class of grouped push buttons in menus.

   :superclasses: :class:`<menu-box>` :class:`<action-gadget>`

   :description:

     .. figure:: images/gadgets-30.png
        :align: center

        The class of grouped push buttons in menus.

     Internally, this class maps into the menu Windows control.

   :example:

     .. code-block:: dylan

        contain(make(<push-menu-box>,
                     items: range(from: 0, to: 5)));

   See also

   - :class:`<check-menu-box>`
   - :class:`<menu-box>`
   - :class:`<radio-menu-box>`

.. class:: <push-menu-button>
   :open:
   :abstract:
   :instantiable:

   The class of push buttons that appear on menus.

   :superclasses: :class:`<push-menu-button>`

   :keyword default?: An instance of type ``<boolean>``. Default value: ``#f``.

   :description:

     .. figure:: images/gadgets-31.png
        :align: center

        The class of push buttons that appear on menus.

     The ``default?:`` init-keyword sets the default value for the push menu
     button gadget.

     Internally, this class maps into the menu item Windows control.

   See also

   - :class:`<check-menu-button>`
   - :class:`<menu-button>`
   - :class:`<radio-menu-button>`

.. class:: <radio-box>
   :open:
   :abstract:
   :instantiable:

   The class of radio boxes, or groups of mutually exclusive radio buttons.

   :superclasses: :class:`<button-box>` :class:`<action-gadget>`

   :description:

     |image8| The instantiable class that implements an abstract radio box,
     that is, a gadget that constrains a number of toggle buttons, only one
     of which may be selected at any one time.

     The value of the radio box is the value of the currently selected item
     in the radio box.


   :example:

     .. code-block:: dylan

        contain(make(<radio-box>, items: #("Yes", "No"),
                     orientation: #"vertical");

     The following example defines a label-key function which formats the
     label of each item in the radio box, rather than just using the item
     itself.

     .. code-block:: dylan

        *radio-box* := contain
          (make(<radio-box>,
                items: #(1, 2, 3, 4, 5),
                orientation: #"vertical",
                label-key:
                  method (item)
                    format-to-string("===%d===",
                      item) end));

   See also

   - :class:`<check-box>`
   - :class:`<group-box>`
   - :class:`<push-box>`

.. class:: <radio-button>
   :open:
   :abstract:
   :instantiable:

   The class of radio buttons.

   :superclasses: :class:`<button>` :class:`<action-gadget>`

   :description:

     |image9| The class of radio buttons. Isolated radio buttons are of
     limited use: you will normally want to combine several instances of such
     buttons using the :class:<radio-box>` gadget.

     Internally, this class maps into the radio button Windows control.

   :example:

     .. code-block:: dylan

        contain(make(<radio-button>, label: "Hello"));

   See also

   - :class:`<button>`
   - :class:`<check-button>`
   - :class:`<menu-button>`
   - :class:`<radio-box>`

.. class:: <radio-menu-box>
   :open:
   :abstract:
   :instantiable:
   
   The class of grouped radio buttons that can appear in menus.

   :superclasses: :class:`<menu-box>` :class:`<action-gadget>`

   :description:

     The class of grouped radio buttons that can appear in menus.

     .. figure:: images/gadgets-34.png
        :align: center

        A radio menu box

     Internally, this class maps into the menu Windows control.

   :example:

     The following example creates a menu that shows an example of a radio
     menu box, as well as several other menu gadgets.

     .. code-block:: dylan

        contain(make(<menu>,
                     label: "Hello...",
                     children: vector
                       (make(<menu-button>,
                             label: "World"),
                        make(<menu-button>,
                             label: "Bonzo"),
                        make(<radio-menu-box>,
                             items:
                               #("You", "All",
                                 "Everyone")),
                        make(<menu>,
                             label: "Others",
                             children:
                               vector
                                 (make(<check-menu-box>,
                                       items: #(1, 2, 3)))
        ))));

   See also

   - :class:`<menu-box>`
   - :class:`<push-menu-box>`
   - :class:`<radio-menu-button>`

.. class:: <radio-menu-button>
   :open:
   :abstract:
   :instantiable:

   The class of radio buttons that can appear in menus.

   :superclasses: :class:`<menu-button>`

   :description:

     |image10| The class of radio buttons that can appear in menus. Isolated
     radio menu buttons are of limited use: you will normally want to combine
     several instances of such buttons using the :class:<radio-menu-box>` gadget.

     Internally, this class maps into the menu radio item Windows control.

   :example:

     .. code-block:: dylan

        contain(make(<radio-menu-button>, label: "Hello"));

   See also

   - :class:`<menu-button>`
   - :class:`<push-menu-button>`
   - :class:`<radio-menu-box>`

.. generic-function:: remove-column

   Removes a column from the specified table.

   :signature: remove-column *table index* =>

   :param table: An instance of type :class:`<table-control>`.
   :param index: An instance of type ``<integer>``.

   :description:

     Removes a column from *table*.

   See also

   - :gf:`add-column`

.. generic-function:: remove-item

   Removes an item from a list control or table control.

   :signature: remove-item *list-or-table item* => ()

   :param list-or-table: An instance of ``type-union(<list-control>, <table-control>)``.
   :param item: An instance of type :class`<list-item>`.

   :description:

     Removes *item* from *list-or-table*.

   See also

   - :gf:`add-item`
   - :gf:`find-item`
   - :class:`<list-control>`
   - :class:`<list-item>`
   - :gf:`make-item`
   - :class:`<table-control>`
   - :class:`<table-item>`

.. generic-function:: remove-node

   Removes a node from a tree control.

   :signature: remove-node *tree node* => ()

   :param tree: An instance of :class:`<tree-control>`.
   :param node: An instance of type :class:`<tree-node>`.


   :description:

     Removes *node* from *tree*.

   See also

   - :gf:`add-node`
   - :gf:`find-node`
   - :gf:`make-node`
   - :class:`<tree-control>`

.. class:: <scroll-bar>
   :open:
   :abstract:
   :instantiable:

   The class of scroll bars.

   :superclasses: :class:`<value-range-gadget>`

   :keyword orientation: An instance of type ``one-of(#"horizontal",
     #"vertical", #"none")``. Default value: ``#"none"``.
   :keyword value-changing-callback: An instance of type ``<function>``.
   :keyword value-changed-callback: An instance of type ``<function>``.
   :keyword slug-size: An instance of type ``<real>``.

   :description:

     |image11| The instantiable class that implements an abstract scroll bar.

     The ``orientation:`` init-keyword defines whether the scroll bar is
     horizontal or vertical.

     The ``value-changing-callback:`` init-keyword is the callback that is
     invoked when the gadget value is in the process of changing, such as
     when the scroll bar slug is dragged.

     The ``value-changed-callback:`` init-keyword is the callback that is
     invoked when the gadget value has changed, such as when the scroll bar
     slug has come to rest after being dragged. You could use this callback,
     for example, to refresh the screen in your application to show a
     different part of a sheet, after the scroll bar had been moved.

     The ``slug-size:`` init-keyword defines the size of the slug in the scroll
     bar, as a proportion of ``value-range:``. For example, if ``value-range:``
     is from 0 to 100, and ``slug-size:`` is 25, then the slug occupies a
     quarter of the total length of the scroll bar. The slug is the part of
     the scroll bar that can be dragged up and down, and represents how much
     of the sheet being scrolled is visible.

     .. note:: The Microsoft Windows Interface Guidelines refer to the slug as
        a *scroll-box*, and the area in which the slug can slide as the
        *scroll-shaft*. You should be aware of this difference if you are using
        those guidelines as a reference.

     Internally, this class maps into the Windows scroll bar control.

   :operations:

     - :gf:`gadget-slug-size`
     - :gf:`gadget-slug-size-setter`

   :example:

     As an example of how the *slug-size:* init-keyword operates, compare the
     two examples of scroll bars below. The second scroll bar has a slug that
     is twice the size of the first.

     contain(make(<scroll-bar>,
                  value-range: range(from: 0, to: 100)
                  slug-size: 10));
     contain(make(<scroll-bar>,
                  value-range: range(from: 0, to: 100)
                  slug-size: 20));

   See also

   - :class:`<slider>`

.. macro:: scrolling
   :statement:

   Places scroll bars around the specified DUIM panes, if required.

   :macrocall:

     .. code-block:: dylan

        scrolling ([*options* ]) {*pane* } end

   :param options: Dylan arguments *bnf*.
   :param pane: A Dylan expression *bnf*.

   :description:

     Places scroll bars around the DUIM panes created by *pane*, if
     required. It is useful to use this macro if you are unsure that the
     panes created can be displayed on the screen successfully without scroll
     bars: this macro only adds scroll bars when it is necessary.

     Creates *pane* with scroll bars attached to it, taking into account any
     of the specified *options*.

     The *pane* is an expression whose return value is the sheet to which the
     scroll bars should be attached.

     The options can be used to specify the properties of the scroll bars. As
     well as all the properties of :class:`<gadget>`, these
     include a ``scroll-bars:`` init-keyword, which may take one of the
     following values: ``#f, #"none", #"horizontal", #"vertical", #"both",
     #"dynamic"``. If no options are specified, then both vertical and
     horizontal scroll bars are used.

     The pane is a body of code whose return value is the sheet to which the
     label should be assigned.

   :example:

     .. code-block:: dylan

        scrolling (scroll-bars: #"vertical")
          make(<radio-box>,
               orientation: #"vertical",
               items: range(from: 1, to: 50))
        end

   See also

   - :class:`<scroll-bar>`
   - :gf:`scroll-position`
   - :gf:`set-scroll-position`

.. generic-function:: scroll-position

   Returns the position of the scroll bar slug in the specified sheet.

   :signature: scroll-position *sheet* => *x y*

   :param sheet: An instance of type :class:`<sheet>`.
   :value x: An instance of type ``<integer>``.
   :value y: An instance of type ``<integer>``.

   :description:

     Returns the position of the scroll bar slug in *sheet*. Note that this
     generic function only returns the position of scroll bar slugs that have
     been created using the :macro:`scrolling` macro. It
     does not work on gadgets with scroll bars defined explicitly.

     .. note:: The Microsoft Windows Interface Guidelines refer to the
        slug as a *scroll-box*, and the area in which the slug can slide
        as the *scroll-shaft*. You should be aware of this difference
        if you are using those guidelines as a reference.

   See also

   - :macro:`scrolling`
   - :gf:`set-scroll-position`

.. class:: <separator>
   :open:
   :abstract:
   :instantiable:

   The class of gadgets used as a visual separator.

   :superclasses: :class:`<gadget>`

   :keyword orientation: An instance of type ``one-of(#"horizontal", #"vertical")``.
      Default value: ``#"horizontal"``.

   :description:

     The class of gadgets used as a visual separator.

     .. figure:: images/gadgets-37.png
        :align: center

        A separator

     The ``orientation:`` init-keyword specifies whether the separator is
     vertical or horizontal.

   :example:

     The following example creates a column layout and places two buttons in
     it, separated with a separator.

     .. code-block:: dylan

        contain(vertically ()
                  make(<button>, label: "Hello");
                  make(<separator>);
                  make(<button>, label: "World")
                end);

   See also

   - :class:`<spacing>`

.. generic-function:: set-scroll-position

   Scrolls the window on the specified sheet.

   :signature: set-scroll-position *sheet x y* => ()

   :param sheet: An instance of type :class:`<sheet>`.
   :param x: An instance of type ``<integer>``.
   :param y: An instance of type ``<integer>``.

   :description:

     Scrolls the window on *sheet* by setting the position of the scroll bar
     slug. Note that this generic function only sets the position of scroll
     bar slugs that have been created using the :macro:`scrolling` macro.
     It does not work on gadgets with scroll bars defined explicitly.

     .. note:: The Microsoft Windows Interface Guidelines refer to the
        slug as a *scroll-box*, and the area in which the slug can slide
        as the *scroll-shaft*. You should be aware of this difference
        if you are using those guidelines as a reference.

   See also

   - :gf:`scroll-position`
   - :macro:`scrolling`

.. generic-function:: sheet-viewport

   Returns the viewport that is clipping the specified sheet.

   :signature: sheet-viewport *sheet* => *viewport*

   :param sheet: An instance of type :class:`<sheet>`.
   :value viewport: An instance of type ``false-or(`<viewport>)``.

   :description:

     Returns the viewport that is clipping *sheet*.

   See also

   - :gf:`sheet-viewport-region`
   - :class:`<viewport>`

.. generic-function:: sheet-viewport-region

   Returns the sheet region of the specified sheet’s viewport, if it has
   one.

   :signature: sheet-viewport-region *sheet* => *region*

   :param sheet: An instance of type :class:`<sheet>`.
   :value region: An instance of type :class:`<region>`.

   :description:

     Returns the sheet region of *sheet* ’s viewport, if it has one. If sheet
     has no viewport, it returns *sheet* ’s own region.

   See also

   - :gf:`sheet-viewport`
   - :class:`<viewport>`

.. class:: <slider>
   :open:
   :abstract:
   :instantiable:

   The class of slider gadgets.

   :superclasses: :class:`<value-range-gadget>`

   :keyword min-label: An instance of type ``type-union(<string>, <image>)``.
   :keyword max-label: An instance of type ``type-union(<string>, <image>)``.
   :keyword borders: An instance of type ``one-of(#f, #"none", #"flat",
     #"sunken", #"raised", #"ridge", #"groove", #"input", #"output")``.
     Default value: ``#f``.
   :keyword tick-marks: An instance of type ``false-or(<integer>)``.
     Default value: ``#f``
   :keyword orientation: An instance of type ``one-of(#"horizontal", #"vertical")``.
     Default value: ``#"horizontal"``.
   :keyword value-changing-callback: An instance of type ``<function>``.

   :description:

     |image12| The class of slider gadgets. This is a gadget used for setting
     or adjusting the value on a continuous range of values, such as a volume
     or brightness control.

     You can specify a number of attributes for the labels in a slider. The
     ``min-label:`` and ``max-label:`` init-keywords let you specify a label to
     be displayed at the minimum and maximum points of the slider bar,
     respectively. In addition, the ``range-label-text-style:`` init-keyword
     lets you specify a text style for these labels.

     The ``borders:`` init-keyword lets you specify a border around the slider.
     If specified, a border of the appropriate type is drawn around the
     gadget.

     The ``tick-marks:`` init-keyword specifies the number of tick-marks that
     are shown on the slider. Displaying tick marks gives the user a better
     notion of the position of the slug at any time.

     The ``orientation:`` init-keyword specifies whether the slider is
     horizontal or vertical.

     The ``value-changing-callback:`` init-keyword is the callback that is
     invoked when the slider slug is dragged.

     Internally, this class maps into the Windows trackbar control.

     When designing a user interface, you will find that spin boxes are a
     suitable alternative to spin boxes in many situations.

   :example:

     .. code-block:: dylan

        contain(make(<slider>,
                     value-range:
                       range(from: -20, to: 20, by: 5)));

   See also

   - :class:`<scroll-bar>`
   - :class:`<spin-box>`

.. class:: <spacing>
   :open:
   :abstract:
   :instantiable:

   The class of gadgets that can be used to provide spacing around a sheet.

   :superclasses: :class:`<gadget>`

   :keyword child: An instance of type ``limited(<sequence> of: <sheet>)``.
   :keyword thickness: An instance of type ``<integer>``. Default value: 1.

   :description:

     The class of gadgets that can be used to provide spacing around a sheet.

     The ``child:`` init-keyword is the sheet or sheets that you are adding
     spacing around.

     The ``thickness:`` init-keyword specifies the thickness of the spacing
     required.

     It is usually clearer to use the :macro:`with-spacing` macro, rather
     than to create an instance of :class:`<spacing>` explicitly.

   :example:

     The following creates a vertical layout containing two buttons separated
     by a text field that has spacing added to it.

     .. code-block:: dylan

        contain(vertically ()
          make(<button>, label: "Hello");
          make(<spacing>,
               child: make(<text-field>),
               thickness: 10);
          make(<button>, label: "World")
        end);

   See also

   - :class:`<null-pane>`
   - :class:`<separator>`
   - :macro:`with-spacing`

.. class:: <spin-box>
   :open:
   :abstract:
   :instantiable:

   The class of spin box gadgets.

   :superclasses: :class:`<collection-gadget>`

   :keyword borders: An instance of type ``one-of(#f, #"none", #"flat",
     #"sunken", #"raised", #"ridge", #"groove", #"input", #"output")*.
     Default value: ``#f``.

   :description:

     |image13| The class of spin box gadgets. A spin box gadget is a text box
     that only accepts a limited range of values that form an ordered loop.
     As well as typing a value directly into the text box, small buttons are
     placed on its right hand side (usually with up and down arrow icons as
     labels). You can use these buttons to increase or decrease the value
     shown in the text box.

     A spin box may be used when setting a percentage value, for example. In
     this case, only the values between 0 and 100 are valid, and a spin box
     is a suitable way of ensuring that only valid values are specified by
     the user.

     The *borders:* init-keyword lets you specify a border around the spin
     box. If specified, a border of the appropriate type is drawn around the
     gadget.

     When designing a user interface, you will find that sliders are a
     suitable alternative to spin boxes in many situations.

   :example:

     .. code-block:: dylan

        contain(make(<spin-box>,
                     items: range(from: 1, to: 10)));

   See also

   - :class:`<slider>`

.. class:: <splitter>
   :abstract:
   :instantiable:

   The class of splitter gadgets. Splitters are subclasses of both
   :class:`<gadget>` and :class:`<layout>`. Splitters (sometimes referred
   to as split bars in Microsoft documentation) are gadgets that allow
   you to split a pane into two resizable portions. For example, you could create a
   splitter that would allow more than one view of a single document. In a
   word processor, this may be used to let the user edit disparate pages on
   screen at the same time.

   A splitter consists of two components: a button that is used to create
   the splitter component itself (referred to as a split box), and the
   splitter component (referred to as the split bar). The split box is
   typically placed adjacent to the scroll bar. When the user clicks on the
   split box, a movable line is displayed in the associated pane which,
   when clicked, creates the split bar.

   The ``split-box-callback:`` init-keyword is an instance of type
   ``false-or(<function>)``, and specifies the callback that is invoked when
   the split box is clicked.

   The ``split-bar-moved-callback:`` init-keyword is an instance of type
   ``false-or<function>)``, and specifies a callback that is invoked when
   the user moves the split bar.

   The ``horizontal-split-box?:`` init-keyword is an instance of type
   :drm:`<boolean>`, and if true a horizontal split bar is created.

   The ``vertical-split-box?:`` init-keyword is an instance of type
   :drm:`<boolean>`, and if true a vertical split bar is created.

.. generic-function:: splitter-split-bar-moved-callback

   Returns the function invoked when the split bar of *splitter* is moved.

   :signature: splitter-split-bar-moved-callback *splitter* => *function*

   :param splitter: An instance of type :class:`<splitter>`.
   :value function: An instance of type ``<function>``.

.. generic-function:: splitter-split-bar-moved-callback-setter

   Sets the callback invoked when the split bar of *splitter* is moved.

   :signature: splitter-split-bar-moved-callback-setter *function splitter* => *function*

   :param function: An instance of type ``<function>``.
   :param splitter: An instance of type :class:`<splitter>`.
   :value function: An instance of type ``<function>``.

.. generic-function:: splitter-split-box-callback

   Returns the callback invoked when the split box of *splitter* is
   clicked.

   :signature: splitter-split-box-callback *splitter* => *function*

   :param splitter: An instance of type :class:`<splitter>`.
   :value function: An instance of type ``<function>``.

.. generic-function:: splitter-split-box-callback-setter

   Sets the callback invoked when the split box of *splitter* is clicked.

   :signature: splitter-split-box-callback-setter *function splitter* => *function*


   :param function: An instance of type ``<function>``.
   :param splitter: An instance of type :class:`<splitter>`.
   :value function: An instance of type ``<function>``.

.. class:: <status-bar>
   :open:
   :abstract:
   :instantiable:

   The class of status bars.

   :superclasses: :class:`<value-range-gadget>`

   :keyword label: An instance of type ``type-union(<string>, <image>)``.
   :keyword label-pane: An instance of ``false-or(<gadget>)``.
     Default value: ``#f``.
   :keyword progress-bar?: An instance of type ``<boolean>``.
     Default value: ``#f``.
   :keyword progress-bar: An instance of ``false-or(<progress-bar>)``.
     Default value: ``#f``.
   :keyword value: An instance of type :drm:`<object>`.
   :keyword value-range: An instance of type :drm:`<range>`.

   :description:

     The class of status bars. Status bars are often used at the bottom of an
     application window, and can provide a range of feedback on the current
     state of an application. Some examples of information that is often
     placed in a status bar are:

     - Documentation strings for the currently selected menu button.
     - Progress indicators to show the state of operations such as loading
       and saving files.
     - The current position of the caret on the screen.
     - Currently selected configurable values (such as the current font
       family, size, and style in a word processor).
     - The current time.

     In particular, it is trivial to add an in-built progress bar to a status
     bar. Any documentation strings specified for menu buttons in a frame are
     automatically displayed in the label pane of a status bar when the mouse
     pointer is placed over the menu button itself.

     The ``label:`` init-keyword specifies a string or icon that is to be used
     as a label for the gadget. Alternatively, the ``label-pane:`` init-keyword
     specifies a pane that should be used as the label. You should only use
     one of these init-keywords; see the discussion about creating status
     bars below.

     If ``progress-bar?:`` is true, then the status bar has a progress bar.
     Alternatively, the ``progress-bar:`` init-keyword specifies a pane that
     should be used as the label. You should only use one of these
     init-keywords; see the discussion about creating status bars below.

     The ``value:`` init-keyword specifies the gadget value of the progress
     bar, if there is one.

     The ``value-range:`` init-keyword is the range of values across which the
     progress bar can vary, if there is one.

     Internally, this class maps into the Windows status window control.

     There are two ways that you can create a status bar:

     - The simple way is to provide a simple status bar that only has a
       label and, optionally, a progress bar.
     - The more complicated way is to define all the elements of a status
       bar from scratch, as children of the status bar.

     If you want to create a simple status bar, then use the ``label:``
     init-keyword to specify the text to be displayed in the status bar. In
     addition, you can set or check the label using :gf:`gadget-label` once the
     status bar has been created.

     You can create a basic progress bar by setting ``progress-bar?:`` to true.
     If you create a progress bar in this way, then it will respond to the
     :gf:`gadget-value` and :gf:`gadget-value-range` protocols: you can use
     :gf:`gadget-value` to set the position of the progress bar explicitly, or to
     check it, and you can use :gf:`gadget-value-range` to define the range of
     values that the progress bar can take, just like any other value gadget.
     By default, the range of possible values is 0 to 100.

     The more complicated way to create a status bar is to define all its
     children from scratch. You need to do this if you want to provide the
     user with miscellaneous feedback about the application state, such as
     online documentation for menu commands, or the current position of the
     cursor. Generally speaking, if you need to provide pane in which to
     display information, you should define instances of :class:`<label>`
     for each piece of information you want to
     use. However, if you wish you can add any type of gadget to your status
     bar in order to create a more interactive status bar. For instance, many
     word processors include gadgets in the status bar that let you select
     the zoom level at which to view the current document from a drop-down
     list of options.

     If you define the children of a status bar from scratch in this way, you
     should make appropriate use of the ``label-pane:`` and ``progress-bar:``
     init-keywords. The ``label-pane:`` init-keyword lets you specify the pane
     that is to act as the label for the status bar; that is, the pane that
     responds to the *gadget-label* protocol. The ``progress-bar:``
     init-keyword lets you define a progress bar to add to the status bar. If
     you create a status bar from scratch, you should not use either the
     ``label:`` or ``progress-bar?:`` init-keywords.

   :operations:

     - `<frames.htm#32720>`
     - `<frames.htm#56600>`
     - :gf:status-bar-label-pane`
     - :gf:status-bar-progress-bar`

   :example:

     The following creates a basic status bar with the given label, and a
     progress bar with the given range of values.

     .. code-block:: dylan

        contain(make(<status-bar>,
                     progress-bar?: #t,
                     value-range: range(from: 0, to: 50)
                     label: "Status"));

   See also

   - `<frames.htm#12376>`
   - `<frames.htm#36830>`
   - :gf:`gadget-documentation`
   - :gf:`status-bar-label-pane`
   - :gf:`status-bar-progress-bar`

.. generic-function:: status-bar-label-pane

   Returns the gadget that displays the label of the specified status bar.

   :signature: status-bar-label-pane *status-bar* => *label*

   :param status-bar: An instance of type :class:`<status-bar>`.
   :value label: An instance of type ``false-or(<label>`)``.

   :description:

     Returns the gadget that displays the label of *status-bar*.

   :example:

     Create a status bar with a label as follows:

     .. code-block:: dylan

        *status* := contain(make(<status-bar>,
                                 value-range:
                                   range(from: 0, to: 100),
                                 label: "Status"));

     The pane that the label of the status bar is displayed in can be
     returned with the following call:

     .. code-block:: dylan

        status-bar-label-pane(*status*);

   See also

   - :class:`<status-bar>`
   - :gf:`status-bar-progress-bar`

.. generic-function:: status-bar-progress-bar

   Returns the progress bar for the specified status bar.

   :signature: status-bar-progress-bar *status-bar* => *progress-bar*

   :param status-bar: An instance of type :class:`<status-bar>`.
   :value progress-bar: An instance of type ``false-or(<progress-bar>)``.

   :description:

     Returns the progress bar for *status-bar*, if there is one.

   See also

   - :class:`<progress-bar>`

.. class:: <tab-control>
   :open:
   :abstract:
   :instantiable:

   The class of tab controls.

   :superclasses: :class:`<value-gadget>`

   :keyword pages: An instance of type ``limited(<sequence>, of: <page>)``.
   :keyword current-page: An instance of type ``false-or(<sheet>)``.
   :keyword key-press-callback: An instance of type ``false-or(<frames.htm#40934>, <function>)``.

   :description:

     |image14| The class of tab controls. These controls let you implement a
     multi-page environment in a window or dialog. Each page in a tab control
     has its own associated layout of sheets and gadgets, and an accompanying
     tab (usually displayed at the top of the page, rather like the tab
     dividers commonly used in a filing cabinet. Each page in a tab control
     can be displayed by clicking on the appropriate tab.

     The *pages:* init-keyword is used to define the pages that the tab
     control contains. Each page in the tab control is an instance of the
     class :class:`<page>`.

     The ``current-page:`` init-keyword specifies which tab is visible when the
     tab control is first displayed.

     The ``key-press-callback:`` init-keyword lets you specify a key-press
     callback. This type of callback is invoked whenever a key on the
     keyboard is pressed while the gadget has focus. In a tab control, a
     key-press callback might be used as a quick way to display each page in
     the tab control. See :gf:`gadget-key-press-callback`, for a fuller
     description of key-press callbacks.

     The :gf:`gadget-id` of a tab control is particularly useful, because
     it is returned by :gf:`gadget-value`.

     Internally, this class maps into the Windows tab control.

   :operations:

     - :gf:`tab-control-current-page`
     - :gf:`tab-control-current-page-setter`
     - :gf:`tab-control-labels`
     - :gf:`tab-control-pages`
     - :gf:`tab-control-pages-setter`

   :example:

     The following example creates a tab control that has two pages. The
     first page contains a button, and the second page contains a list.

     .. code-block:: dylan

        contain(make(<tab-control>,
                     pages:
                       vector(make(<tab-control-page>,
                                   label: "First",
                                   child: make(<push-button>,
                                               label: "One")),
                              make(<tab-control-page>,
                                   label: "Second",
                                   child: make(<list-box>,
                                               items:
                                                 #(1, 2, 3)
        )))));

   See also

   - :class:`<page>`

.. generic-function:: tab-control-current-page

    Returns the current visible page of the specified tab control.

   :signature: tab-control-current-page *tab-control* => *visible-page*

   :param tab-control: An instance of type :class:`<tab-control>`.
   :value visible-page: An instance of type :class:`<page>`.

   :description:

     Returns the current visible page of *tab-control*.

   :example:

     The following example creates a tab control that has two pages.

     .. code-block:: dylan

        *tab* := contain
                   (make
                     (<tab-control>,
                      pages:
                       vector(make(<tab-control-page>,
                                   label: "First",
                                   child: make(<push-button>,
                                               label: "One")),
                              make(<tab-control-page>,
                                   label: "Second",
                                   child: make(<list-box>,
                                               items:
                                                 #(1, 2, 3)
        )))));

     The current page of the tab control can be returned with the following
     code:

     .. code-block:: dylan

        tab-control-current-page(*tab*);

   See also

   - :class:`<page>`
   - :class:`<tab-control>`
   - :gf:`tab-control-current-page-setter`
   - :gf:`tab-control-pages`

.. generic-function:: tab-control-current-page-setter

   Sets the current visible page of the specified tab control.

   :signature: tab-control-current-page-setter *visible-page tab-control* => *visible-page*

   :param visible-page: An instance of type :class:`<page>`.
   :param tab-control: An instance of type :class:`<tab-control>`.
   :value visible-page: An instance of type :class:`<page>`.

   :description:

     Sets the current visible page of *tab-control*.

   :example:

     The following example creates a tab control that has two pages.

     .. code-block:: dylan

        *tab* := contain
                  (make
                    (<tab-control>,
                     pages:
                       vector(make(<tab-control-page>,
                                   label: "First",
                                   child: make(<push-button>,
                                               label: "One")),
                              make(<tab-control-page>,
                                   label: "Second",
                                   child: make(<list-box>,
                                               items:
                                               #(1, 2, 3)
        )))));

     Assign a variable to the current page of the tab control as follows:

     .. code-block:: dylan

        *page* := tab-control-current-page(*tab*);

     Next, change the current page of the tab control by clicking on the tab
     for the hidden page. The, set the current page to be the original
     current page as follows:

     .. code-block:: dylan

        tab-control-current-page(*tab*) := *page*;

   See also

   - :class:`<page>`
   - :class:`<tab-control>`
   - :gf:`tab-control-current-page`

.. generic-function:: tab-control-labels

   Returns the tab labels of the specified pane.

   :signature: tab-control-labels *tab-control* => *labels*

   :param tab-control: An instance of type :class:`<tab-control>`.
   :value labels: An instance of type ``limited(<sequence>, of: <label>)``.

   :description:

     Returns the tab labels of *tab-control*, as a sequence. Each element in
     *labels* is an instance of :class:`<label>`.

   :example:

     Given the tab control created by the code below:

     .. code-block:: dylan

        *tab* := contain
                   (make
                     (<tab-control>,
                       pages:
                         vector(make(<tab-control-page>,
                                     label: "First"),
                                make(<tab-control-page>,
                                     label: "Second"),
                                make(<tab-control-page>,
                                     label: "Third"),
                                make(<tab-control-page>,
                                     label: "Fourth"),
                                make(<tab-control-page>,
                                     label: "Fifth"))));

     You can return a list of the labels as follows:

     .. code-block:: dylan

        tab-control-labels(*tab*);

   See also

   - :class:`<tab-control>`
   - :gf:`tab-control-pages`

.. class:: <tab-control-page>
   :open:
   :abstract:
   :instantiable:

   The class that represents a page in a tab control.

   :superclasses: :class:`<page>`

   :description:

     The class that represents a page in a tab control.

     .. figure:: images/gadgets-42.png
        :align: center
     
        A tab control page

   See also

   - :class:`<page>`
   - :class:`<tab-control>`
   - `<frames.htm#93333>`
   - `<frames.htm#87607>`

.. generic-function:: tab-control-pages

   Returns the tab pages of the specified pane.

   :signature: tab-control-pages *tab-control* => *pages*

   :param tab-control: An instance of type :class:`<tab-control>`.
   :value pages: An instance of type ``limited(<sequence>, of: <page>)``.
     Default value: ``#[]``.

   :description:

     Returns the tab pages of *pane*.

   :example:

     Given the tab control created by the code below:

     .. code-block:: dylan

       *tab* := contain
                 (make
                   (<tab-control>,
                    pages:
                      vector(make(<tab-control-page>,
                                  label: "First"),
                             make(<tab-control-page>,
                                  label: "Second"),
                             make(<tab-control-page>,
                                  label: "Third"),
                             make(<tab-control-page>,
                                  label: "Fourth"),
                             make(<tab-control-page>,
                                  label: "Fifth"))));

    You can return a list of the pages as follows:

    .. code-block:: dylan

       tab-control-pages(*tab*);

   See also

   - :class:`<page>`
   - :class:`<tab-control>`
   - :gf:`tab-control-current-page`
   - :gf:`tab-control-labels`
   - :gf:`tab-control-pages-setter`

.. generic-function:: tab-control-pages-setter

   Sets the tab pages of the specified tab control.

   :signature: tab-control-pages-setter *pages tab-control* #key *page* => *pages*

   :param pages: An instance of type ``limited(<sequence>, of: <page>)``.
   :param tab-control: An instance of :class:`<tab-control>`.
   :param page: An instance of :class:`<page>`.
   :value pages: An instance of type ``limited(<sequence>, of: <page>)``.

   :description:

     Sets the tab pages available to *tab-control*, optionally setting
     *page* to the default page to be displayed. The pages argument is an
     instance of ``limited(<sequence>, of: <page>)``. The *page* argument is
     an instance of :class:`<page>` and, moreover, must be one of the pages
     contained in *pages*.

   :example:

     The ``tab-control-pages-setter`` function is used as follows:

     .. code-block:: dylan

        tab-control-pages(my-tab-control, page: my-page)
          := my-pages

   See also

   - :class:`<page>`
   - :class:`<tab-control>`
   - :gf:`tab-control-pages`

.. class:: <table-column>
   :sealed:

   The class of columns in table controls.

   :superclasses: :drm:`<object>`

   :keyword heading: An instance of type :drm:`<string>`.
   :keyword width: An instance of type ``<integer>``. Default value: 100.
   :keyword alignment: An instance of type ``one-of(#"left", #"right",
     #"center")``. Default value: ``#"left"``.
   :keyword generator: An instance of type ``<function>``.
   :keyword callback: An instance of type ``false-or(<function>)``.
     Default value: ``#f``.

   :description:

     The class of columns in table controls.

     The ``width:`` init-keyword lets you specify the width of the column. The
     alignment: init-keyword is used to specify how the column should be
     aligned in the table.

     To populate the table column, the function specified by ``generator:`` is
     invoked. This function is called for each item in the table control, and
     the value returned is placed at the appropriate place in the column.

     In addition, you can also specify a callback that can be used for
     sorting the items in the table column, using the ``callback:``
     init-keyword.

   See also

   - :class:`<table-control>`

.. class:: <table-control>
   :open:
   :abstract:
   :instantiable:

   The class of table controls.

   :superclasses: :class:`<collection-gadget>` :class:`<action-gadget>`

   :keyword headings: An instance of type ``limited(<sequence>, of: <string>)``.
   :keyword generators: An instance of type ``limited(<sequence>, of: <function>)``.
   :keyword view: An instance of type :type:`<table-control-view>`. Default value: ``#"table"``.
   :keyword borders: An instance of type ``one-of(#f, #"none", #"flat",
     #"sunken", #"raised", #"ridge", #"groove", #"input", #"output")``.
     Default value: ``#f``.
   :keyword scroll-bars: An instance of type ``one-of(#f, #"none",
     #"horizontal", #"vertical", #"both", #"dynamic")``.
     Default value: ``#"both"``.
   :keyword popup-menu-callback: An instance of type ``<function>``.
   :keyword key-press-callback: An instance of type ``false-or(<frames.htm#40934>, <function>)``.
   :keyword widths: An instance of type ``limited(<sequence>, of: <integer>)``.

   :description:

     The class of table controls.

     .. figure:: images/gadgets-43.png
        :align: center

     The ``view:`` init-keyword can be used to specify how the items in the
     table control are displayed. See :type:`<table-control-view>`, for more details.

     The ``borders:`` init-keyword lets you specify a border around the table
     control. If specified, a border of the appropriate type is drawn around
     the gadget.

     The ``scroll-bars:`` init-keyword defined the scroll bar behavior for the
     gadget.

     You can use the ``popup-menu-callback:`` init-keyword to specify a
     context-sensitive menu to display for one or more selected items in the
     table control. In Windows 95, for instance, such a context-sensitive
     menu can be displayed by right-clicking on any item or group of selected
     items in the list control.

     The ``key-press-callback:`` init-keyword lets you specify a key-press
     callback. This type of callback is invoked whenever a key on the
     keyboard is pressed while the gadget has focus. In a table control, a
     key-press callback might be used as a quick way to select an item in the
     control. See :gf:`gadget-key-press-callback`, for a
     fuller description of key-press callbacks.

     The ``headings:`` and ``generators:`` init-keywords can be used to specify
     the titles of each column in the control, and a sequence of functions
     that are used to generate the contents of each column. The headings
     should be a sequence of strings, and the generators should be a sequence
     of functions.

     The first item in the sequence of headings is used as the title for the
     first column, the second is used as the title of the second column, and
     so on. Similarly, the first function in the sequence of generators is
     invoked on each item in the control, thereby generating the contents of
     the first column, the second is used to generate the contents of the
     second column by invoking it on each item in the control, and so on.
     This is illustrated in `Defining column headings and contents in
     table controls`_.

     .. figure:: images/gadgets-44.png
        :align: center

        Defining column headings and contents in table controls

     If you do not specify both of these init-keywords, you must supply
     columns for the table control, using the :class:`<table-column>` class.

     The ``widths:`` init-keyword lets you specify the width of each column in
     the table control. It takes a sequence of integers, each of which
     represents the width, in pixels, of the respective column in the
     control. Note that there must be as many widths as there are columns.

     Internally, this class maps into the Windows list view control with
     LVS-REPORT style.

   :operations:

     - :gf:`add-column`
     - :gf:`remove-column`
     - :gf:`table-control-view`
     - :gf:`table-control-view-setter`

   See also

   - :class: `<table-column>`
   - :type:`<table-control-view>`

.. type:: <table-control-view>

   The type of possible views for a table control

   :equivalent: ``one-of(#"table", #"small-icon", #"large-icon", #"list")``

   :description:

     This type represents the acceptable values for the view arguments to
     operators of :class:`<table-control>`.

     There are four possible values, corresponding to the view options that
     will be familiar to most users of GUI-based operating systems:

     ``#"small-icon"``
        Displays each item in the table with a small icon to
        the left of the item. Items are arranged horizontally.

     ``#"large-icon"``
        Displays each item in the table with a large icon to
        the left of the item. Items are arranged horizontally.

     ``#"list"``
        Displays each item in the table with a small icon to the
        left of the item. Items are arranged vertically in one column.

     ``#"table"``
        Displays each item in the list with a small icon to the
        left of the item. Items are arranged vertically in one column.
        Additional details not available in other views are also displayed.
        The details that are displayed depend on the nature of the items in
        the table control. For example, if filenames are displayed in the
        table control, additional details may include the size, modification
        date, and creation date of each file. If e-mail messages are
        displayed in the table control, additional details may include the
        author of the e-mail, its subject, and the date and time it was sent.

   See also

   - :class:`<list-control-view>`
   - :class:`<table-control>`
   - :gf:`table-control-view`

.. generic-function:: table-control-view

   Returns the current view of the specified table control.

   :signature: table-control-view *table-control* => *view*

   :param table-control: An instance of type :class:`<table-control>`.
   :value view: An instance of type :class:`<table-control-view>`.

   :description:

     Returns the current view of *table-control*. The available views are
     described in the entry for :class:`<table-control-view>`.

   See also

   - :class:`<table-control-view>`
   - :gf:`table-control-view-setter`

.. generic-function:: table-control-view-setter

   Sets the current view of the specified table control.

   :signature: table-control-view-setter *view table-control* => *view*

   :param view: An instance of type :class:`<table-control-view>`.
   :param table-control: An instance of type :class:`<table-control>`.
   :value view: An instance of type :class:`<table-control-view>`.

   :description:

     Sets the current view of *table-control*.

     The *view* argument is used to specify the way in which the items in the
     table control are displayed.

   See also

   - :class:`<table-control-view>`
   - :gf:`table-control-view`

.. class:: <table-item>
   :open:
   :abstract:
   :instantiable:

   The class that represents an item in a table control.

   :superclasses: :drm:`<object>`

   :keyword object: An instance of type :drm:`<object>`.

   :description:

     The class that represents an item in a table control.

     The ``object:`` init-keyword describes the object that an instance of
     table item represents.

   See also

   - :gf:`add-item`
   - :gf:`find-item`
   - :gf:`make-item`
   - :gf:`remove-item`
   - :class:`<table-control>`

.. class:: <text-editor>
   :open:
   :abstract:
   :instantiable:

   The class of multiple line text editors.

   :superclasses: :class:`<text-field>`

   :keyword columns: An instance of type ``false-or(<integer>)``. Default value: ``#f``.
   :keyword lines: An instance of type ``false-or(<integer>)``. Default value: ``#f``.
   :keyword scroll-bars: An instance of type ``one-of(#f, #"none",
     #"horizontal", #"vertical", #"both", #"dynamic")``.
     Default value: *#"both"*.

   :description:

     The class of multiple line text editors.

     .. figure:: images/gadgets-45.png
        :align: center

     The ``columns:`` and ``lines:`` init-keywords specify the number of columns
     and lines of characters visible in the text editor, respectively.

     The ``scroll-bars:`` init-keyword specifies whether the text editor has
     scroll bars or not.

     Internally, this class maps into the multi-line edit control Windows
     control.

   :example:

     To constrain the number of lines and columns when an editor is first
     displayed:

     .. code-block:: dylan

        *editor* := contain(make(<text-editor>,
                                 lines: 20, columns: 80));

     To make a text editor that is fixed at 10 lines high:

     .. code-block:: dylan

        make(<text-editor>, lines: 10, fixed-height?: #t);

   See also

   - :class:`<text-field>`

.. class:: <text-field>
   :open:
   :abstract:
   :instantiable:

   The class of single line text fields.

   :superclasses: :class:`<text-gadget>`

   :keyword x-alignment: An instance of type one-of(#"left", #"right",
     #"center")``. Default value: ``#"left"``.
   :keyword case: An instance of type ``one-of(#f, #"lower", #"upper")``.
     Default value: ``#f``.
   :keyword auto-scroll?: An instance of type ``<boolean>``. Default value: ``#f``.

   :description:

     The class of single line text fields.

     .. figure:: images/gadgets-46.png
        :align: center

     The ``x-alignment:`` init-keyword is used to align the text in the text
     field.

     The ``case:`` init-keyword lets you specify which case is used to display
     the text in the text field. You can specify either upper or lower case.
     The default is to display letters of either case.

     If ``auto-scroll?``: is true, then text scrolls off to the left of the
     text field if more text is typed than can be displayed in the text field
     itself.

     Internally, this class maps into the single-line edit control Windows
     control.

   :example:

     To make a text field with a fixed width:

     .. code-block:: dylan

        make(<text-field>, width: 200, fixed-width?: #t);

     The following example creates a text field which, after pressing Return,
     invokes a callback that displays the gadget value in a dialog box.

     .. code-block:: dylan

        *text* := contain
                   (make(<text-field>,
                         value-changed-callback:
                           method (gadget)
                             notify-user
                               (format-to-string
                                  ("Changed to %=",
                                   gadget-value(gadget)),
                                owner: gadget) end));

   See also

   - :class:`<password-field>`

.. class:: <text-gadget>
   :open:
   :abstract:

   The class of all text gadgets.

   :superclasses: :class:`<value-gadget>` :class:`<action-gadget>`

   :keyword text: An instance of type ``<string>``. Default value: ``""``.
   :keyword value-type: An instance of type ``<type>``. Default value: ``<string>``.
   :keyword value-changing-callback: An instance of type ``false-or(<function>)``.

   :description:

     The class of all text gadgets. You should not create a direct instance
     of this class.

     The ``text:`` init-keyword specifies a text value for the combo box.

     The ``value-type:`` init-keyword specifies the type of the gadget value of
     the text gadget, which by default is ``<string>``. Other supported types
     are ``<integer>`` and ``<symbol>``. The string entered in the text gadget
     is parsed, and converted to the appropriate type automatically.

     Text gadgets have a method on :gf:`gadget-value`
     that converts the :gf:`gadget-text` based on the
     :gf:`gadget-value-type`, for example converting the
     string to an integer for ``value-type: <integer>``.

     The :gf:`gadget-text` generic function always
     returns the exact text typed into a text gadget. However,
     :gf:`gadget-value` always returns a "parsed" value of
     the appropriate type, depending on the value of
     :gf:`gadget-value-type`. If the string contains any
     characters that are not appropriate to the
     :gf:`gadget-value-type` (for example, if the string
     contains any non-integers, and the
     :gf:`gadget-value-type` is ``<integer>``), then
     :gf:`gadget-value` returns ``#f``.

     Setting the gadget value "prints" the value and inserts the appropriate
     text into the text field.

     The ``value-changing-callback:`` init-keyword allows you to specify a
     callback that is invoked as the value of the text gadget is changing
     during the course of "casual" typing. Generally, this means when the
     user is typing text, but before the text is committed (usually by
     pressing the RETURN key).

     Conversely, the value-changed callback of a text gadget is invoked when
     the change to the gadget value is committed (again, usually by pressing
     the RETURN key).

     The action required to "commit" a text change is defined by the back-end
     for the platform that you are writing for, and is not configurable.

   :operations:

     - :gf:`gadget-text`

   :example:

     .. code-block:: dylan

        contain(make(<text-field>, value-type: <integer>
                     text: "1234"));

   See also

   - :class:`<combo-box>`
   - :gf:`gadget-value-type`
   - :class:`<password-field>`
   - :class:`<text-editor>`
   - :class:`<text-field>`

.. class:: <tool-bar>
   :open:
   :abstract:
   :instantiable:

   The class of tool bars.

   :superclasses: :class:`<gadget>` :class:`<multiple-child-composite-pane>`

   :keyword update-callback: An instance of type ``<function>``.

   :description:

     The class of tool bars. A tool bar is a gadget that contains, as
     children, a number of buttons that give the user quick access to the
     more common commands in an application. Typically, the label for each
     button is an icon that pictorially represents the operation that
     clicking the button performs.

     .. figure:: images/gadgets-47.png
        :align: center

        A tool bar

     A tool bar is often placed underneath the menu bar of an application,
     although its position is very often configurable, and a tool bar may
     often be "docked" against any edge of the application’s frame. In
     addition, a tool bar can sometimes be displayed as a free-floating
     window inside the application.

     Internally, this class maps into the Windows toolbar control.

   :operations:

     - `<frames.htm#88622>`
     - `<frames.htm#56600>`

   See also

   - :class:`<button-box>`
   - :class:`<status-bar>`

.. class:: <tree-control>
   :open:
   :abstract:
   :instantiable:

   The class of tree controls.

   :superclasses: :class: `<collection-gadget>`

   :keyword children-generator: An instance of type ``<function>``.
   :keyword children-predicate: An instance of type ``<function>``.
   :keyword icon-function: An instance of type ``<function>``.
   :keyword show-edges?: An instance of type ``<boolean>``. Default value: ``#t``.
   :keyword show-root-edges?: An instance of type ``<boolean>``. Default value: ``#t``.
   :keyword show-buttons?: An instance of type ``<boolean>``. Default value: ``#t``.
   :keyword initial-depth: An instance of type ``<integer>``. Default value: 0.
   :keyword scroll-bars: An instance of type ``one-of(#f, #"none",
     #"horizontal", #"vertical", #"both", #"dynamic")``.
     Default value: ``#"both"``.
   :keyword popup-menu-callback: An instance of type ``<function>``.
   :keyword key-press-callback: An instance of type ``false-or(<frames.htm#40934>, <function>)``.
   :keyword roots: An instance of type :drm:`<sequence>`. Default value: ``#[]``.

   :description:

     The class of tree controls.

     .. figure:: images/gadgets-48.png
        :align: center

     The ``children-generator:`` is the function that is used to generate the
     children below the root of the tree control. It is called with one
     argument, an object.

     The ``icon-function:`` init-keyword lets you specify a function to supply
     icons for display in the control. The function is called with the item
     that needs an icon as its argument, and it should return an instance of
     :class:`<image>` as its result. Typically, you might want to define an icon
     function that returns a different icon for each kind of item in the
     control. For example, if the control is used to display the files and
     directories on a hard disk, you would want to return the appropriate
     icon for each registered file type.

     The ``show-edges?:``, ``show-root-edges?:``, and ``show-buttons?:``
     init-keywords define whether lines are displayed for the edges of items
     in the tree control, the roots in the tree control, and whether the
     icons of items in the tree control are displayed, respectively. By
     default, all three are visible.

     The number of levels of outline that are shown when the tree control is
     first displayed is controlled by the ``initial-depth:`` init-keyword. The
     default value of this is 0, meaning that only the top level of the
     outline is shown, with no nodes expanded.

     The ``scroll-bars:`` init-keyword specifies whether the tree control has
     scroll bars or not.

     You can use the ``popup-menu-callback:`` init-keyword to specify a
     context-sensitive menu to display for one or more selected items in the
     tree control. In Windows 95, for instance, such a context-sensitive menu
     can be displayed by right-clicking on any item or group of selected
     items in the list control.

     The ``key-press-callback:`` init-keyword lets you specify a key-press
     callback. This type of callback is invoked whenever a key on the
     keyboard is pressed while the gadget has focus. For tree controls, a
     typical key-press callback might select an item in the control. See
     :gf:`gadget-key-press-callback`, for a fuller
     description of key-press callbacks.

     The ``roots:`` init-keyword is used to specify any roots for the tree
     control. It is a sequence.

     Internally, this class maps into the Windows tree view control.

   :operations:

    - :gf:`contract-node`
    - :gf:`expand-node`
    - :gf:`tree-control-children-predicate`
    - :gf:`tree-control-children-predicate-setter`
    - :gf:`tree-control-children-generator`
    - :gf:`tree-control-children-generator-setter`
    - :gf:`tree-control-roots`
    - :gf:`tree-control-roots-setter`

   :example:

     .. code-block:: dylan

        make(<tree-control>,
             roots: #[1],
             children-generator:
               method (x) vector(x * 2, 1 + (x * 2)) end,
               icon-function: method (item :: <integer>)
                               case
                                 odd?(item) => $odd-icon;
                                 even?(item) => $even-icon;
                               end);

   See also

   - :gf:`add-node`
   - :gf:`find-node`
   - :gf:`make-node`
   - :gf:`remove-node`

.. generic-function:: tree-control-children-predicate

   Returns the children predicate function of the specified tree control.

   :signature: tree-control-children-predicate *tree-control* => *children-predicate*

   :param tree-control: An instance of type :class:`<tree-control>`.
   :value children-predicate: An instance of type ``<function>``.

   :description:

     Returns the children predicate function of *tree-control*.

   See also

   - :class:`<tree-control>`
   - :gf:`tree-control-children-predicate-setter`
   - :gf:`tree-control-children-generator`

.. generic-function:: tree-control-children-predicate-setter

   Sets the children predicate function of the specified tree control.

   :signature: tree-control-children-predicate-setter *children-predicate tree-control* => *children-predicate*

   :param children-predicate: An instance of type ``<function>``.
   :param tree-control: An instance of type :class:`<tree-control>`.
   :value children-predicate: An instance of type ``<function>``.

   :description:

     Sets the children predicate function of *tree-control*.

   See also

   - :class:`<tree-control>`
   - :gf:`tree-control-children-predicate`
   - :gf:`tree-control-children-generator-setter`

.. generic-function:: tree-control-children-generator

   Returns the function that generates the children of the specified tree control.

   :signature: tree-control-children-generator *tree-control* => *children-generator*

   :param tree-control: An instance of type :class:`<tree-control>`.
   :value children-generator: An instance of type ``<function>``.

   :description:

     Returns the function that generates the children of *tree-control*.
     This is the function that is used to generate the children below the
     root of *tree-control*.

   See also

   - :class:`<tree-control>`
   - :gf:`tree-control-children-predicate`
   - :gf:`tree-control-children-generator-setter`

.. generic-function:: tree-control-children-generator-setter

   Sets the function that generates the children of the specified tree
   control.

   :signature: tree-control-children-generator-setter *children-generator tree-control * => *children-generator*

   :param children-generator: An instance of type ``<function>``.
   :param tree-control: An instance of type :class:`<tree-control>`.
   :value children-generator: An instance of type ``<function>``.

   :description:

     Sets the function that generates the children of *tree-control*. This
     is the function that is used to generate the children below the root of
     *tree-control*.

   See also

   - :class:`<tree-control>`
   - :gf:`tree-control-children-predicate-setter`
   - :gf:`tree-control-children-generator`

.. generic-function:: tree-control-icon-function

   Returns the icon function for the specified list control.

   :signature: tree-control-icon-function *tree-control* => *icon-function*

   :param tree-control: An instance of :class:`<tree-control>`.
   :value icon-function: An instance of type ``<function>``.

   :description:

     Returns the icon function for *tree-control*. This function lets you
     specify which icon to display for each item in the control. The function
     is called with the item that needs an icon as its argument, and it
     should return an instance of :class:`<image>` as its result. Typically, you
     might want to define an icon function that returns a different icon for
     each kind of item in the control. For example, if the control is used to
     display the files and directories on a hard disk, you would want to
     return the appropriate icon for each registered file type.

     Note that, unlike list controls, the icon function for a tree control
     cannot be changed once the list control has been created.

   See also

   - :gf:`list-control-icon-function`
   - :class:`<tree-control>`

.. generic-function:: tree-control-initial-depth

   Returns the initial depth of the specified tree control.

   :signature: tree-control-initial-depth *tree-control* => *initial-depth*

   :param tree-control: An instance of type :class:`<tree-control>`.
   :value initial-depth: An instance of type ``<integer>``.

   :description:

     Returns the initial depth of *tree-control*. This is the number of
     levels of outline that are visible in the tree control when it is first
     displayed. A return value of 0 indicates that only the top level of the
     outline is displayed initially. A return value of 1 indicates that
     outline is expanded to a depth of one (that is, any direct subnodes of
     the top level are displayed, but no others).

   See also

   - :class:`<tree-control>`
   - :gf:`tree-control-initial-depth-setter`

.. generic-function:: tree-control-initial-depth-setter

   Sets the initial depth of the specified tree control.

   :signature: tree-control-inital-depth *initial-depth* *tree-control* => *initial-depth*

   :param initial-depth: An instance of type ``<integer>``.
   :param tree-control: An instance of type :class:`<tree-control>`.
   :value initial-depth: An instance of type ``<integer>``.

   :description:

     Sets the initial depth of *tree-control*. This is the number of levels
     of outline that are visible in the tree control when it is first
     displayed. A return value of 0 indicates that only the top level of the
     outline is displayed initially. A return value of 1 indicates that
     outline is expanded to a depth of one (that is, any direct subnodes of
     the top level are displayed, but no others).

   See also

   - :class:`<tree-control>`
   - :gf:`tree-control-initial-depth`

.. generic-function:: tree-control-roots

    Returns the roots of the specified tree control.

   :signature: tree-control-roots *tree* => *roots*

   :param tree: An instance of type :class:`<tree-control>`.
   :value roots: An instance of type :drm:`<sequence>`.

   :description:

     Returns the roots of *tree*.

   :example:

     Create a tree control as follows:

     .. code-block:: dylan

        *tree* := contain(make(<tree-control>,
                               roots: #(1, 2, 3),
                               children-generator:
                                 method (x)
                                   vector(x, x + 1)
                                 end));

     You can return the roots of this tree control as follows:

     .. code-block:: dylan

        tree-control-roots(*tree*);

   See also

   - :class:`<tree-control>`
   - :gf:`tree-control-roots-setter`

.. generic-function:: tree-control-roots-setter

   Sets the roots of the specified tree control.

   :signature: tree-control-roots-setter *roots tree* #key *frame-manager* => *roots*

    :param roots: An instance of type :drm:`<sequence>`.
    :param tree: An instance of type :class:`<tree-control>`.
    :param frame-manager: An instance of type :class:`<frame-manager>`.
    :value roots: An instance of type :drm:`<sequence>`.

   :description:

     Sets the roots of *tree*.

   :example:

     Create a tree control without specifying any roots as follows:

     .. code-block:: dylan

        *tree* := contain(make(<tree-control>,
                               children-generator:
                                 method (x)
                                   vector(x, x + 1)
                                 end));

     You can set the roots of this tree control as follows:

     .. code-block:: dylan

       tree-control-roots(*tree*) := #(1, 2, 3);

     The tree control is updated on the screen to reflect this change.

   See also

   - :class:`<tree-control>`
   - :gf:`tree-control-roots`

.. class:: <tree-node>
   :open:
   :abstract:
   :instantiable:

   The class of nodes in tree controls.

   :superclasses: :drm:`<object>`

   :keyword parent-nodes: An instance of type :drm:`<sequence>`.
   :keyword child-nodes: An instance of type :drm:`<sequence>`.
   :keyword generation: An instance of type ``<integer>``. Default value: 0.
   :keyword object: An instance of type :drm:`<object>`.

   :description:

     The class of nodes in tree controls. A tree node represents an object,
     and is displayed as a text label accompanied by an icon. Tree nodes are
     analogous to list items in a list control or table items in a table
     control.

     To the left of a tree node is a small plus or minus sign. If a plus sign
     is displayed, this indicates that the node contains subnodes that are
     currently not visible. If a minus sign is displayed, this indicates
     either that the node does not contain any subnodes, or that the subnodes
     are already visible.

     The ``parent-nodes:`` and ``child-nodes:`` init-keywords let you specify any
     parents and children that the node has.

     The ``object:`` init-keyword specifies the object that is represented by
     the tree node. For example, in the case of a file manager application,
     this might be a directory on disk.

   :operations:

     - :gf:contract-node`
     - :gf:expand-node`
     - :gf:node-children`
     - :gf:node-expanded?`
     - :gf:node-parents`

   See also

   - :class:`<tree-control>`

.. generic-function:: update-gadget

   Forces the specified gadget to be redrawn.

   :signature: update-gadget *gadget* => ()

   :param gadget: An instance of type :class:`<gadget>`.

   :description:

     Forces *gadget* to be redrawn. This can be useful if a number of changes
     have been made which have not been reflected in the gadget automatically
     (for example, by using pixmaps to perform image operations

.. class:: <value-gadget>
   :open:
   :abstract:

   The class of gadgets that can have values.

   :superclasses: :class:`<gadget>`

   :keyword value: An instance of type :drm:`<object>`.
   :keyword value-changed-callback: An instance of type
     ``false-or(<frames.htm#40934>, <function>)``.

   :description:

     The class of gadgets that can take values.

     The ``value:`` init-keyword specifies the current gadget value. For tab
     controls, if the gadget ID is specified, then that is passed as the
     gadget value whether or not ``value:`` is specified.

     The ``value-changed-callback:`` init-keyword is the callback that is
     invoked when the gadget value has changed, such as when a scroll bar
     slug has come to rest after being dragged, or when the changes to text
     in a text field have been committed by pressing the RETURN key.

   :operations:

     - :gf:`gadget-value`
     - :gf:`gadget-value-changed-callback`
     - :gf:`gadget-value-changed-callback-setter`
     - :gf:`gadget-value-setter`
     - :gf:`gadget-value-type`

   See also

   - :gf:`gadget-value`
   - :gf:`gadget-value-changed-callback`

.. class:: <value-range-gadget>
   :open:
   :abstract:

   The class of all value gadgets with ranges.

   :superclasses: :class:`<value-gadget>`

   :keyword value-range: An instance of type :drm:`<range>`.
     Default value: ``range(from: 0, to: 100)``.

   :description:

     The class of all value gadgets with ranges. You should not create a
     direct instance of this class.

     The ``value-range:`` init-keyword is the range of values that the gadget
     value of a value range gadget can take. This may be different in any
     given situation: when downloading a file or compiling source code, you
     might want to use a value range of 0-100, to indicate percentage done
     (this is the default). When downloading e-mail messages from a mail
     server, however, you may want to use a range equal to the number of
     messages being downloaded.

   :operations:

     - :gf:`gadget-value-range`
     - :gf:`gadget-value-range-setter`

   :example:

     .. code-block:: dylan

        contain(make(<slider>,
                     value-range:
                       range(from: -20, to: 20, by: 5)));

   See also

   - :class:`<progress-bar>`
   - :class:`<scroll-bar>`
   - :class:`<slider>`
   - :class:`<value-gadget>`

.. class:: <viewport>
   :open:
   :abstract:
   :instantiable:

   The class of viewports.

   :superclasses: :class:`<gadget>` :class:`<single-child-composite-pane>`

   :keyword horizontal-scroll-bar: An instance of type
     ``false-or(<scroll-bar>)``. Default value: ``#f``.
   :keyword vertical-scroll-bar: An instance of type
     ``false-or(<scroll-bar>)``. Default value: ``#f``.

   :description:

     The class of viewports. A viewport is a sheet "through" which other
     sheets are visible; they are used to implement a clipping region for
     scrolling.

     The ``horizontal-scroll-bar:`` and ``vertical-scroll-bar:`` init-keywords
     specify whether the viewport has horizontal and vertical scroll bars,
     respectively.

     In most applications, you should not need to use a viewport yourself.
     However, there are some circumstances in which defining your own
     viewports is invaluable. In particular, if you need to use a single
     scroll bar to scroll more than one window at the same time, you should
     define each window as a viewport, and use the same scroll bar to scroll
     each window. There are two situations where this behavior is quite
     common:

     - In applications which have vertical or horizontal rulers around a
       document window, such as a drawing application. In this case, the
       rulers must scroll with the drawing itself.
     - In applications such as spreadsheets, where row and column headings
       need to scroll with the document. Note that you may also choose to
       implement this kind of functionality using a table control.

   :operations:

     - :gf:`viewport-region`

   See also

   - :gf:`sheet-viewport`
   - :gf:`sheet-viewport-region`
   - :gf:`viewport?`
   - :gf:`viewport-region`

.. generic-function:: viewport?

   Returns true if the specified object is a viewport.

   :signature: viewport? *object* => *viewport?*

   :param object: An instance of type :drm:`<object>`.
   :value viewport?: An instance of type ``<boolean>``.

   :description:

     Returns true if *object* is a viewport.

   :example:

     To test whether the gadget ``*gadget*`` is a viewport:

     .. code-block:: dylan

        viewport?(*gadget*);

   See also

   - :class:`<viewport>`
   - :class:`<button-box>`
   - :class:`<border>`

.. generic-function:: viewport-region

   Returns the region for the specified viewport.

   :signature: viewport-region *viewport* => *region*

   :param viewport: An instance of type :class:`<viewport>`.
   :value region: An instance of type :class:`<region>`.

   :description:

     Returns the region for *viewport*.

   :example:

     To return the region for a viewport ``*viewer*``:

     .. code-block:: dylan

        viewport-region(*viewer*);

   See also

   - :class:`<viewport>`

.. macro:: with-border
   :statement:

   Creates the specified sheet and places a border around it.

   :macrocall:
     .. code-block:: dylan

        with-border ([*options* ]) {*pane* } end

   :param options: Dylan arguments *bnf*.
   :param pane: A Dylan expression *bnf*.

   :description:

     Creates *pane* with a border around it, taking into account any of the
     specified *options*.

     The options specified may be any of the legal init-keywords used to
     specify an instance of :class:`<border>`. If no options are specified,
     then the default border is used.

     The pane is an expression whose return value is the sheet around which a
     border should be placed.

   :example:

     To create a button in a border:

     .. code-block:: dylan

        contain(with-border (type: #"raised")
                  make(<button>,
                       label: "Hello") end);

   See also

   - :class:`<border>`
   - :macro:`labelling`
   - :macro:`with-spacing`

.. macro:: with-spacing
   :statement:

   Creates the specified sheet and places spacing around it.

   :macrocall:
     .. code-block:: dylan

        with-spacing ([*options* ]) {*pane* } end

   :param options: Dylan arguments *bnf*.
   :param pane: A Dylan expression *bnf*.

   :description:

     Creates *pane* with spacing around it, taking into account any of the
     specified *options*.

     The options specified may be any of the legal init-keywords used to
     specify an instance of :class:`<spacing>`. If no options are specified,
     then the default spacing is used.

     The pane is an expression whose return value is the sheet around which
     spacing should be placed.

   :example:

     .. code-block:: dylan

        contain(with-spacing (thickness: 10)
                 (vertically () make(<button>,
                                     label: "Hello");
                                make(<button>,
                                     label: "World")
                  end)
                end);

   See also

   - :class:`<null-pane>`
   - :class:`<spacing>`
   - :macro:`with-border`

.. |image0| image:: images/gadgets-15.png
.. |image1| image:: images/gadgets-17.png
.. |image2| image:: images/gadgets-18.png
.. |image3| image:: images/gadgets-19.png
.. |image4| image:: images/gadgets-20.png
.. |image5| image:: images/gadgets-21.png
.. |image6| image:: images/gadgets-24.png
.. |image7| image:: images/gadgets-26.png
.. |image8| image:: images/gadgets-32.png
.. |image9| image:: images/gadgets-33.png
.. |image10| image:: images/gadgets-35.png
.. |image11| image:: images/gadgets-36.png
.. |image12| image:: images/gadgets-38.png
.. |image13| image:: images/gadgets-39.png
.. |image14| image:: images/gadgets-41.png
