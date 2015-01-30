*******************
DUIM-Frames Library
*******************

.. current-library:: duim
.. current-module:: duim-frames

Overview
========

The DUIM-Frames library contains interfaces that define a wide variety
of frames for use in your GUI applications, as well as the necessary
functions, generic functions, and macros for creating and manipulating
them. The library contains a single module, *duim-frames*, from which
all the interfaces described in this chapter are exposed.
`DUIM-Frames Module`_ contains complete reference
entries for each exposed interface.

Frames are the basic components used to display DUIM objects on-screen.
An instance of type :class:`<frame>` is an object representing some state in a
user application, plus the sheets in its interface. Frames control the
overall appearance of the entire window, allowing you to distinguish,
for example, between a normal window and a dialog box, or allowing you
to specify modal or modeless dialog boxes, and might include such things
as a menu bar, a tool bar, and a status bar.

Frames exist on windows and contain sheets, which can be instances of
:class:`<layout>` or :class:`<gadget>`, or any of their subclasses, and an event
loop. The event loop associated with a frame is represented by an
instance of a subclass of :class:`<event>`. An overview of these subclasses is
provided in `Subclasses of \<frame-event\>`_.

The class hierarchy for DUIM-Frames
===================================

This section presents an overview of the available classes of frame,
frame event, and command-related classes, and describes the class
hierarchy present.

The <frame> class and its subclasses
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The base class for all DUIM frames is the :class:`<frame>` class, which is
itself a subclass of :drm:`<object>`. In addition, there are a number of
classes related to commands that are subclasses of :drm:`<object>`, together
with a number of classes related to events that occur in frames.

- :drm:`<object>`

  - :class:`<gadget>`
  - :class:`<page>`

    - See `Subclasses of \<page\>`_

  - :class:`<frame>`

    - See `Subclasses of \<frame\>`_

  - :class:`<event>`

    - :class:`<frame-event>`

      - See `Subclasses of \<frame-event\>`_

  - :class:`<simple-command>`
  - :class:`<simple-undoable-command>`
  - :class:`<command-table>`
  - :class:`<command-table-menu-item>`

The :class:`<frame>` class represents the base class for all types of frame. An
introduction to the subclasses available is given in `Subclasses of \<frame\>`_.

The :class:`<event>` class represents the base class for all events that can
occur. Although this class and the :class:`<frame-event>` subclass are exposed
by the DUIM-Sheets library, the subclasses of :class:`<frame-event>` itself are
exposed by the DUIM-Frames library. See `Subclasses of \<frame-event\>`_ for an
introduction to these subclasses. See  the :doc:`DUIM-Sheets Library <sheets>`, for a
complete description of the DUIM-Sheets library.

The remaining four classes exposed by the DUIM-Frames library relate to
commands and their use in application menus.

:class:`<simple-command>`
   This class is used to create the most basic type of command. A
   command is an operation that can be invoked as a callback from a menu
   item, a button, or other suitable interface control.

:class:`<simple-undoable-command>`
   This class is used to define commands whose effects can be reversed.
   Typically, the user chooses the command *Edit > Undo* to reverse the
   effects of a command of this class.

:class:`<command-table>`
   The :class:`<command-table>` class is used to define the complete menu
   structure of an application frame, from the menu bar and menus to the
   menu items on each menu.

:class:`<command-table-menu-item>`
   This class represents a menu item on a menu defined in a command
   table.

Subclasses of <frame>
^^^^^^^^^^^^^^^^^^^^^

A number of subclasses of :class:`<frame>` are provided to allow you to create
a variety of common types of frame.

- :class:`<frame>`

  - :class:`<simple-frame>`
  - :class:`<dialog-frame>`
  - :class:`<property-frame>`
  - :class:`<wizard-frame>`

:class:`<simple-frame>`
   This class is the most common sort of frame and is used to create
   a standard window in an application.

:class:`<dialog-frame>`
   This class is used to create dialog boxes for use in an application.

:class:`<property-frame>`
   This class is used to create property sheets for use in an
   application. Property sheets are a special type of dialog box which
   make use of tab controls to display several pages of information
   within the same dialog.

:class:`<wizard-frame>`
   This class is used to create wizards for use in an application.
   Wizards are a special type of multi-page dialog in which the user is
   guided through a series of sequential steps, filling out any
   information requested and using *Next* and *Back* buttons to navigate
   to the next or previous steps in the process.

Subclasses of <frame-event>
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :class:`<frame-event>` class provides a number of subclasses that describe
various events that can occur in frames.

- :class:`<frame-event>`

  - :class:`<frame-created-event>`
  - :class:`<frame-destroyed-event>`
  - :class:`<frame-mapped-event>`
  - :class:`<frame-unmapped-event>`
  - :class:`<frame-exit-event>`
  - :class:`<frame-exited-event>`
  - :class:`<application-exited-event>`

The name of each of these subclasses accurately reflects the type of
event that they are used to represent. The classes
:class:`<frame-created-event>` and :class:`<frame-destroyed-event>`
represent a frame being created or destroyed. The classes
:class:`<frame-mapped-event>` and :class:`<frame-unmapped-event>`
represent the events that occur when a frame is displayed on the
computer screen or removed from it. The class :class:`<frame-exit-event>`
represents the act of exiting a frame, and the class
:class:`<frame-exited-event>` represents the event where a frame has been
successfully exited.

In addition, the class :class:`<frame-exited-event>` has a subclass
:class:`<application-exited-event>`. This is reserved for the special case
where the frame that has been exited is actually the parent frame for
the whole application, in which case the whole application is exited,
together with any other frames that may have been spawned as a result of
using the application.

.. note:: The classes :class:`<frame-mapped-event>` and
   :class:`<frame-unmapped-event>` are distinct from the
   classes :class:`<frame-created-event>` and
   :class:`<frame-destroyed-event>`. A frame is not necessarily
   mapped as soon as it is created, and any frame can be unmapped
   from the screen without actually destroying it (for example,
   a frame may be iconized).

Subclasses of <page>
^^^^^^^^^^^^^^^^^^^^

Although the :class:`<page>` class is itself a subclass of :class:`<gadget>`, and is
exposed by the DUIM-Gadgets library, two of its subclasses are exposed
by the DUIM-Frames library: :class:`<wizard-page>` and :class:`<property-page>`. See
`Subclasses of <page> <gadgets.htm#31084>`_ for an introduction to
these classes.

DUIM-Commands Library
=====================

All commands-related interfaces are now defined directly in the Commands
library. However, these same interfaces are imported to and re-exported
from DUIM-Frames, so they can be used in almost the same way as for
Harlequin Dylan 1.0. You should continue to look for commands-related
documentation in this chapter.

A consequence of the introduction of the Commands library is that a
slight change in syntax is required in the definition of commands in
command tables. In Harlequin Dylan 1.0, two approaches could be taken
when specifying a command in a table. For example, a menu item could be
specified by either of the following:

.. code-block:: dylan

   menu-item "My Command" = make(<command>, function: my-command),
   menu-item "My Command" = my-command,

Beginning with Harlequin Dylan 1.1, only the last of these may be used.
This may require you to change some of your code.

DUIM-Frames Module
==================

This section contains a complete reference of all the interfaces that
are exported from the *duim-frames* module.

.. method:: =
   :specializer: <command>

   Returns true if the specified commands are the same.

   :signature: = *command1* *command2* => *equal?*

   :param command1: An instance of type :class:`<command>`.
   :param command2: An instance of type :class:`<command>`.
   :value equal?: An instance of type ``<boolean>``.

   :description:

     Returns true if *command1* and *command2* are the same.

.. generic-function:: add-command

   Adds a command to the specified command table.

   :signature: add-command *command-table* *command* #key *name* *menu image* *accelerator* *mnemonic* *error?* => ()

   :param command-table: An instance of type :class:`<command-table>`.
   :param command: An instance of type ``type-union(<command>, <function>)``.
   :param #key name: An instance of type ``false-or(<string>)``.
   :param #key menu: An instance of type ``false-or(<menu>)``.
   :param #key image: An instance of type ``false-or(<image>)``.
   :param #key accelerator: An instance of type ``false-or(<gesture>)``.
   :param #key mnemonic: An instance of type ``false-or(<gesture>)``.
   :param #key error?: An instance of type ``<boolean>``. Default value: ``#t``.

   :description:

     You can supply a keyboard accelerator or a mnemonic using the
     *accelerator* and *mnemonic* arguments respectively.

     Adds *command* to *command-table*.

     The argument *name* is the command-line name for the command.

     - When *name* is ``#f``, the command is not available via command-line
       interactions.
     - When *name* is a string, that string is the command-line name for the
       command.

     For the purposes of command-line name lookup, the character case of
     *name* is ignored.

     The argument *menu* is a menu for *command*.

     - When *menu* is ``#f``, *command* is not available via menus.
     - When *menu* is a string, the string is used as the menu name.
     - When *menu* is ``#t`` and *name* is a string, then *name* is used as
       the menu name.
     - When *menu* is ``#t`` and *name* is not a string, a menu name is
       automatically generated.
     - When *menu* is a list of the form ``(string, menu-options)``,
       *string* is the menu name and *menu-options* consists of a list of
       keyword-value pairs. Each keyword-value pair is itself a list. The
       valid keywords are ``after:``, ``documentation:``, and ``text-style:``,
       which are interpreted as for :gf:`add-command-table-menu-item`.

     You can supply an image that will appear on the menu next to the command
     name using the *image* argument. When supplying an image, bear in mind
     the size of the menu: you should only supply a small icon-sized image
     for a menu command. There may also be other interface guidelines that
     you wish to follow when using images in menu items.

     The value for *accelerator* is either keyboard gesture or ``#f``. When it
     is a gesture, this gesture represents the keystroke accelerator for the
     command; otherwise the command is not available via keystroke
     accelerators. Similarly, if mnemonic is supplied, this gesture is used
     as a mnemonic for the command.

     If *command* is already present in the command table and *error?* is
     ``#t``, an error is signalled. When *command* is already present in the
     command table and *error?* is ``#f``, then the old command-line name,
     menu, and keystroke accelerator are removed from the command table
     before creating the new one.

   :seealso:

     - :gf:`remove-command`

.. generic-function:: add-command-table-menu-item

   Adds a menu item to the specified command table.

   :signature: add-command-table-menu-item *command-table* *string* *type* *value* #key
     *documentation* *after* *accelerator* *mnemonic* *text-style* *error?*
     *items* *label-key* *value-key* *test* *callback* => *menu-item*

   :param command-table: An instance of type :class:`<command-table>`.
   :param string: An instance of type ``false-or(<string>)``.
   :param type: An instance of type ``one-of(#"command", #"function", #"menu", #"divider")``.
   :param value: An instance of type :drm:`<object>`.
   :param #key documentation: An instance of type :drm:`<string>`.
   :param #key after: An instance of type ``one-of(#"start", #"end", #"sort")``, or
     an instance of :drm:`<string>`. Default value: ``#"end"``.
   :param #key accelerator: An instance of type ``false-or(<gesture>)``.
   :param #key mnemonic: An instance of type ``false-or(<gesture>)``.
   :param #key text-style: An instance of type :class:`<text-style>`.
   :param #key error?: An instance of type ``<boolean>``. Default value: ``#t``.
   :param #key items: An instance of type ``limited(<sequence>, of: )``.
   :param #key label-key: An instance of type ``<function>``.
   :param #key value-key: An instance of type ``<function>``.
   :param #key test: An instance of type ``<function>``.
   :param #key callback: An instance of type ``<function>``.
   :value menu-item: An instance of type :class:`<command-table-menu-item>`.

   :description:

     Adds a command menu item to the menu in *command-table*. The *string*
     argument is the name of the command menu item; its character case is
     ignored. The *type* of the item is either ``#"command"``, ``#"function"``,
     ``#"menu"``, or ``#"divider"``.

     When *type* is ``#"command"``, *value* must be one of the following:

     - A command (a list consisting of a command name followed by a list of
       the arguments for the command).
     - A command name. In this case, *value* behaves as though a command
       with no arguments was supplied.

     When all the required arguments for the command are supplied, clicking
     on an item in the menu invokes the command immediately. Otherwise, the
     user is prompted for the remaining required arguments.

     When *type* is ``#"function"``, *value* must be a function having
     indefinite extent that, when called, returns a command. The function is
     called with two arguments:

     - The gesture used to select the item (either a keyboard or button
       press event).
     - A "numeric argument".

     When *type* is ``#"menu"``, this indicates that a sub-menu is required,
     and *value* must be another command table or the name of another command
     table.

     When *type* is ``#"divider"``, some sort of a dividing line is displayed
     in the menu at that point. If *string* is supplied, it will be drawn as
     the divider instead of a line. If the look and feel provided by the
     underlying window system has no corresponding concept, ``#"divider"``
     items may be ignored. When *type* is ``#"divider"``, *value* is ignored.

     The argument *documentation* specifies a documentation string, This can
     be used to provide the user with some online documentation for the menu
     item. Documentation strings are often displayed in a status bar at the
     bottom of an application; highlighting the menu item using the mouse
     pointer displays the documentation string in the status bar.

     The *text-style* argument, if supplied, represents text style. This
     specifies the font family, style, and weight with which to display the
     menu item in the menu. For most menu items, you should just use the
     default text style (that is, the one that the user chooses for all
     applications). However, in certain cases, some variation is allowed.

     The *text-style* argument is of most use in context sensitive pop-up
     menus, which often have a default menu item. This is usually the command
     that is invoked by pressing the RETURN key on the current selection: for
     example, in a list of files, the default command usually opens the
     selected file in the application associated with it. In Windows 95, the
     default command is displayed using a bold font, to differentiate it from
     other commands in the menu, and you should use the text-style argument
     to specify this.

     When altering the text style of a menu item, you should always try to
     stick to any relevant interface guidelines.

     The *items* argument is used to specify the gadgets that are to be
     supplied to the command table as menu items. You can supply either push
     boxes, check boxes, or radio boxes.

     The *after* argument denotes where in the menu the new item is to be
     added. It must be one of the following:

     - ``#"start"`` Adds the new item to the beginning of the menu.
     - ``#"end"`` Adds the new item to the end of the menu.

     A string naming an existing entry

     - Adds the new item after that entry.
     - ``#"sort"`` Insert the item in such as way as to maintain the menu in
       alphabetical order.

     If *mnemonic* is supplied, the item is added to the keyboard mnemonic
     table for the command table. The value of *mnemonic* must be a keyboard
     gesture name.

     When *mnemonic* is supplied and *type* is ``#"command"`` or ``#"function"``,
     typing a key on the keyboard that matches the mnemonic invokes the
     command specified by *value*.

     When *type* is ``#"menu"``, the command is read from the submenu
     indicated by *value* in a window system specific manner. This usually
     means that the submenu itself is displayed, allowing the user to see the
     available options at that point.

     When *accelerator* is supplied, typing a key sequence on the keyboard
     that matches the accelerator invokes the command specified by *value*,
     no matter what *type* is.

     If the item named by *string* is already present in the command table
     and *error?* is ``#t``, then an error is signalled. When the item is
     already present in the command table and *error?* is ``#f``, the old item
     is removed from the menu before adding the new item. Note that the
     character case of *string* is ignored when searching the command table.

   :seealso:

     - :class:`<command-table-menu-item>`
     - :gf:`remove-command-table-menu-item`

.. class:: <application-exited-event>
   :sealed:
   :instantiable:

   The class of events signalled when an application exits.

   :superclasses: :class:`<frame-exited-event>`

   :description:

     The class of events signalled when an application exits. An instance of
     this class is distributed when your application is exited, for instance
     by choosing *File > Exit* from its main menu bar.

   :seealso:

     - :gf:`exit-frame`
     - :class:`<frame-exited-event>`

.. generic-function:: apply-in-frame

   Applies the specified function to the given arguments in the main thread
   of the frame.

   :signature: apply-in-frame *frame function arg* #rest *args* => ()

   :param frame: An instance of type :class:`<frame>`.
   :param function: An instance of type ``<function>``.
   :param arg: An instance of type :drm:`<object>`.
   :param #rest args: Instances of type :drm:`<object>`.


   :description:

     Applies *function* to the given arguments in the main thread of *frame*.
     You must supply at least one argument (*arg*), though you can
     optionally supply as many additional arguments as you like.

   :seealso:

     - :gf:`call-in-frame`

.. generic-function:: call-in-frame

   Calls the specified function with the given arguments in the main thread
   of the frame.

   :signature: call-in-frame *frame function* #rest *args* => ()

   :param frame: An instance of type :class:`<frame>`.
   :param function: An instance of type ``<function>``.
   :param #rest args: Instances of type :drm:`<object>`.

   :description:

     Calls *function* with the given arguments in the main thread of *frame*.

   :seealso:

     - :gf:`apply-in-frame`

.. generic-function:: cancel-dialog

   Cancels the specified dialog.

   :signature: cancel-dialog *dialog* #key *destroy?* => ()

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :param destroy?: An instance of type ``<boolean>``. Default value: ``#t``.

   :description:

     Cancels *dialog* and removes it from the screen. Any changes that the
     user has made to information displayed in the dialog is discarded.

     If *destroy?* is ``#t`` then the dialog is unmapped from the screen.

     This is the default callback used for the cancel button in a dialog.

   :example:

     The following example defines a button, ``*no-button*``, that calls
     :gf:`cancel-dialog` as its activate-callback. This button is then used in a
     dialog that simply replaces the standard cancel button for the newly
     defined dialog. Note that the example assumes the existence of a similar
     ``*yes-button*`` to replace the exit button.

     .. code-block:: dylan

        define variable *no-button*
          = make(<push-button>, label: "No",
                 activate-callback: cancel-dialog,
                 max-width: $fill);
        define variable *dialog*
          = make(<dialog-frame>,
                 exit-button?: #f,
                 cancel-button?: #f,
                 layout: vertically ()
                   make(<label>,
                        label: "Simple dialog");
                   horizontally ()
                     *yes-button*;
                     *no-button*;
                   end
                 end);

        start-frame(*dialog*);

   :seealso:

     - :gf:`dialog-cancel-callback`
     - :class:`<dialog-frame>`
     - :gf:`start-dialog`
     - :gf:`exit-dialog`

.. generic-function:: clear-progress-note

   Clears the specified progress note.

   :signature: clear-progress-note *framem* *progress-note* => ()

   :param framem: An instance of type :class:`<frame-manager>`.
   :param progress-note: An instance of type :class:`<progress-note>`.

   :description:

     Clears the specified progress note.

.. class:: <command>
   :open:
   :abstract:
   :instantiable:

   The class of commands.

   :superclasses: :drm:`<object>`

   :keyword function: An instance of type ``<function>``.
   :keyword arguments: An instance of type :drm:`<sequence>`. Default value: ``#[]``.

   :description:

     The class of commands. These are commands that can be grouped together
     in a command table to form the set of commands available to an
     application (available, for example, from the menu bar of the
     application). The resulting command object can then be executed by
     calling :gf:`execute-command`.

     The ``function:`` init-keyword is the command function that is called by
     the command object. A command function is rather like a callback to a
     ``<command>`` object: a command can be executed via :gf:`execute-command`,
     which then invokes the command function. Command functions take at least
     one argument: a :class:`<frame>` object.

     The ``arguments:`` init-keyword are the arguments passed to the command
     function.

   :operations:

     - `=`
     - :gf:`add-command`
     - :gf:`command-arguments`
     - :gf:`command-enabled?`
     - :gf:`command-enabled?-setter`
     - :gf:`command-function`
     - :gf:`command-undoable?`
     - :gf:`dialog-cancel-callback-setter`
     - :gf:`dialog-exit-callback-setter`
     - :gf:`execute-command`
     - :gf:`gadget-command`
     - :gf:`gadget-command-setter`
     - :gf:`gadget-key-press-callback-setter`
     - :gf:`redo-command`
     - :gf:`remove-command`
     - :gf:`undo-command`

   :seealso:

     - :gf:`command?`
     - :gf:`command-arguments`
     - :gf:`command-function`
     - :gf:`execute-command`
     - :class:`<simple-command>`

.. generic-function:: command?

   Returns true if the specified object is a command.

   :signature: command? *object* => *command?*

   :param object: An instance of type :drm:`<object>`.
   :value command?: An instance of type ``<boolean>``.

   :description:

     Returns true if *object* is an instance of :class:`<command>`.

   :seealso:

     - :class:`<command>`

.. generic-function:: command-arguments

   Returns the arguments to the specified command.

   :signature: command-arguments *command* => *arguments*

   :param command: An instance of type :class:`<command>`.
   :value arguments: An instance of type :drm:`<sequence>`.

   :description:

     Returns the arguments to *command*.

   :seealso:

     - :class:`<command>`

.. generic-function:: command-enabled?

   Returns true if the specified command is enabled.

   :signature: command-enabled? *command* *frame* #key => *enabled?*

   :param command: An instance of type ``type-union(<command>, <command-table>)``.
   :param frame: An instance of type :class:`<frame>`.
   :value enabled?: An instance of type ``<boolean>``.

   :description:

     Returns true if *command* in *frame* is enabled.

   :seealso:

     - :class:`<command>`
     - :gf:`command-enabled?-setter`

.. generic-function:: command-enabled?-setter

   Enables or disables the specified command.

   :signature: command-enabled?-setter *enabled?* *command* *frame* => *enabled?*

   :param enabled?: An instance of type ``<boolean>``.
   :param command: An instance of type ``type-union(<command>, <command-table>)``.
   :param frame: An instance of type :class:`<frame>`.
   :value enabled?: An instance of type ``<boolean>``.

   :description:

     Enables or disables *command* in *frame*. If *enabled?* is true, then
     *command* is enabled, otherwise it is disabled. Enabling and disabling a
     command enables and disables all the gadgets that are associated with
     the command, such as menu items and tool bar buttons.

     This function is useful when manipulating the disabled commands in
     *frame*. For example, it is common to disable the *Save* menu command
     immediately after saving a file, enabling it again only when the file
     has been modified.

   :seealso:

     - :gf:`command-enabled?`

.. generic-function:: command-function

   Returns the function associated with the specified command.

   :signature: command-function *command* => *function*

   :param command: An instance of type :class:`<command>`.
   :value function: An instance of type ``<function>``.

   :description:

     Returns the function associated with *command*. A command function is
     the function that is called by a :class:`<command>` object. Command functions
     are similar to callbacks, in that they are user functions that are
     invoked in order to perform some action. Command functions take at least
     one argument: a :class:`<frame>` object.

   :seealso:

     - :class:`<command>`
     - :gf:`execute-command`

.. class:: <command-table>
   :open:
   :abstract:
   :instantiable:

   The class of command tables.

   :superclasses: :drm:`<object>`

   :keyword name: An instance of type :drm:`<object>`. Required.
   :keyword inherit-from: An instance of type ``limited(<sequence>, of: <command-table>)``. Required.
   :keyword resource-id: An instance of type ``false-or(<object>)``. Default
     value: ``#f``.

   :description:

     The class of command tables. The command table for an application gives
     a complete specification of the commands available to that application,
     through its menus, tool bars, mnemonics, and accelerators.

     The ``name:`` init-keyword is a symbol that names the current command
     table.

     The ``inherit-from:`` init-keyword is a sequence of command tables whose
     behavior the current command table should inherit. All command tables
     inherit the behavior of the command table specified by
     :var:`*global-command-table*`, and can also inherit the
     behavior specified by :var:`*user-command-table*`.

     You do not normally need to specify a unique ``resource-id:`` yourself. As
     with most other DUIM classes, the ``name:`` init-keyword serves as a
     sufficient unique identifier.

   :operations:

     - :gf:`add-command`
     - :gf:`add-command-table-menu-item`
     - :gf:`command-table-accelerators`
     - :gf:`command-table-commands`
     - :gf:`command-table-menu`
     - :gf:`command-table-name`
     - :gf:`frame-command-table-setter`
     - :meth:`make(<frame>)`
     - :gf:`make-menu-from-command-table`
     - :gf:`make-menus-from-command-table`
     - :gf:`remove-command`
     - :gf:`remove-command-table`
     - :gf:`remove-command-table-menu-item`

   :example:

     .. code-block:: dylan

        define command-table *clipboard-command-table*
            =(*global-command-table*)
          menu-item "Cut" = cut-selection,
            documentation: $cut-doc;
          menu-item "Copy" = copy-selection,
            documentation: $copy-doc;
          menu-item "Paste" = paste-from-clipboard,
            documentation: $paste-doc;
          menu-item "Delete" = delete-selection,
            documentation: $delete-doc;
        end command-table *clipboard-command-table*;

   :seealso:

     - :var:`*global-command-table*`
     - :var:`*user-command-table*`

.. generic-function:: command-table?

   Returns true if the specified object is a command table.

   :signature: command-table? *object* => *command-table?*

   :param object: An instance of type :drm:`<object>`.
   :value command-table?: An instance of type ``<boolean>``.

   :description:

     Returns true if *object* is a command table.

   :seealso:

     - :class:`<command-table>`

.. generic-function:: command-table-accelerators

   Returns the keyboard accelerators for the specified command table.

   :signature: command-table-accelerators *command-table* => *accelerators*

   :param command-table: An instance of type :class:`<command-table>`.
   :value accelerators: An instance of type ``limited(<sequence>, of: <gesture>)``.

   :description:

     Returns the keyboard accelerators for *command-table*.

   :seealso:

     - :gf:`command-table-commands`

.. generic-function:: command-table-commands

   Returns the commands for the specified command table.

   :signature: command-table-commands *command-table* => *commands*

   :param command-table: An instance of type :class:`<command-table>`.
   :value commands: An instance of type ``limited(<sequence>, of: <command>)``.

   :description:

     Returns the commands defined for *command-table*.

   :seealso:

     - :gf:`command-table-accelerators`
     - :gf:`command-table-menu`

.. generic-function:: command-table-menu

   Returns the menu items in the specified command table.

   :signature: command-table-menu *command-table* => *menu-items*

   :param command-table: An instance of type :class:`<command-table>`.
   :value menu-items: An instance of type :drm:`<stretchy-vector>`.

   :description:

     Returns the menu items in *command-table*.

   :seealso:

     - :gf:`command-table-commands`
     - :gf:`command-table-name`

.. class:: <command-table-menu-item>
   :sealed:
   :instantiable:

   The class of menu items in command tables.

   :superclasses: :drm:`<object>`

   :keyword name: An instance of type ``false-or(<string>)``. Default value:
     ``#f``.
   :keyword image: An instance of type ``false-or(type-union(<string>, <image>))``.
     Default value: ``#f``
   :keyword type: An instance of type ``one-of(#"command", #"function", #"menu", #"divider")``.
   :keyword value: An instance of type :drm:`<object>`. Default value: ``#f``.
   :keyword options: An instance of type :drm:`<sequence>`. Default value: ``#()``.
   :keyword accelerator: An instance of type ``false-or(<gesture>)``. Default value: ``#f``.
   :keyword mnemonic: An instance of type ``false-or(<gesture>)``. Default value: ``#f``.

   :description:

     The class of menu items in command tables. This class models menu items,
     tool bar items, accelerators, and mnemonics for a command table entry.

     The ``type:`` init-keyword denotes what type of menu item has been
     created. This is either ``#"command"``, ``#"function"``, ``#"menu"``, or
     ``#"divider"``.

     When ``type:`` is ``#"command"``, ``value:`` must be one of the following:

     - A command (a list consisting of a command name followed by a list of
       the arguments for the command).
     - A command name. In this case, ``value:`` behaves as though a command
       with no arguments was supplied.

     When all the required arguments for the command are supplied, clicking
     on an item in the menu invokes the command immediately. Otherwise, the
     user is prompted for the remaining required arguments.

     When ``type:`` is ``#"function"``, ``value:`` must be a function having
     indefinite extent that, when called, returns a command. The function is
     called with two arguments:

     - The gesture used to select the item (either a keyboard or button
       press event).
     - A "numeric argument".

     When ``type:`` is ``#"menu"``, this indicates that a sub-menu is required,
     and ``value:`` must be another command table or the name of another command
     table.

     When ``type:`` is ``#"divider"``, some sort of a dividing line is displayed
     in the menu at that point. If a string is supplied using the ``options:``
     init-keyword, it will be drawn as the divider instead of a line. If the
     look and feel provided by the underlying window system has no
     corresponding concept, ``#"divider"`` items may be ignored. When ``type:``
     is ``#"divider"``, ``value:`` is ignored.

     The ``accelerator:`` and ``mnemonic:`` init-keywords let you specify a
     keyboard accelerator and mnemonic for the menu item.

   :operations:

     - :gf:`add-command-table-menu-item`
     - :gf:`menu-item-accelerator`
     - :gf:`menu-item-mnemonic`
     - :gf:`menu-item-name`
     - :gf:`menu-item-options`
     - :gf:`menu-item-type`
     - :gf:`menu-item-value`

   :seealso:

     - :gf:`add-command-table-menu-item`

.. generic-function:: command-table-name

   Returns the name of the specified command table.

   :signature: command-table-name *command-table* => *name*

   :param command-table: An instance of type :class:`<command-table>`.
   :value name: An instance of type :drm:`<object>`.

   :description:

     Returns the name of *command-table*, as defined by the ``name:``
     init-keyword for :class:`<command-table>`.

   :seealso:

     - :class:`<command-table>`
     - :gf:`command-table-menu`

.. generic-function:: command-undoable?

   Returns true if the specified command is undoable.

   :signature: command-undoable? *command* => *undoable?*

   :param command: An instance of type :class:`<command>`.
   :param undoable?: An instance of type ``<boolean>``.

   :description:

     Returns true if *command* is undoable, that is, there is a specified
     command that the user can choose (for instance, by choosing *Edit >
     Undo*) that will reverse the effects of command.

   :seealso:

     - :gf:`undo-command`

.. generic-function:: complete-from-generator

   Completes a string based on a generated list of completions.

   :signature: complete-from-generator *string generator delimiters*
    #key *action predicate*
    => *string success object nmatches completions*

   :param string: An instance of type :drm:`<string>`.
   :param generator: An instance of type ``<function>``.
   :param delimiters: An instance of type ``limited(<sequence>, of: <character>)``.
   :param action: An instance of type ``one-of(#"complete",
     #"complete-limited", #"complete-maximal", #"completions",
     #"apropos-completions")``. Default value ``#"complete"``.
   :param predicate: An instance of type ``false-or(<function>)``. Default
     value ``#f``.
   :value string: An instance of type ``false-or(<string>)``.
   :value success: An instance of type ``<boolean>``.
   :value object: An instance of type :drm:`<object>`.
   :value nmatches: An instance of type ``<integer>``.
   :value completions: An instance of type :drm:`<sequence>`.

   :description:

     Completes *string* chunk-wise against a list of possibilities derived
     from *generator*, using the specified *delimiters* to break both
     *string* and the generated possibilities into chunks. This function is
     identical to :gf:`complete-from-sequence`, except
     that the list of possibilities is derived from the *generator* function,
     rather than passed explicitly. The *generator* is a function of two
     arguments: the string to be completed and a continuation co-routine to
     call that performs the completion. It should call the continuation with
     two arguments: the completion string and an object.

     The argument *predicate* (if supplied) is applied to filter out unwanted
     objects.

     The function returns five values: the completed string (if there is
     one), whether or not the completion successfully matched, the object
     associated with the completion, the number of things that matched, and
     (if *action* is ``#"completions"``) a sequence of possible completions.

     The *action* argument can take any of the following values:

     - ``#"complete"`` Completes the input as much as possible, except that if
       the user’s input exactly matches one of the possibilities, the
       shorter possibility is returned as the result, even if it is a left
       substring of another possibility.
     - ``#"complete-limited"`` Completes the input up to the next partial
       delimiter.
     - ``#"complete-maximal"`` Completes the input as much as possible.
     - ``#"completions"`` or ``#"apropos-completions"`` Returns a sequence
       of the possible completions.

   :example:

     .. code-block:: dylan

        complete-from-generator
          ("th", method (string, completer)
                   for (b in #["one", "two", "three", "four"])
                     completer(b, b)
                   end
                 end method, #[' ', '-'])

   :seealso:

     - :gf:`complete-from-sequence`

.. generic-function:: complete-from-sequence

   Completes a string based on a list of possible completions.

   :signature: complete-from-sequence *string possibilities delimiters*
     #key *action predicate label-key value-key*
     => *string success object nmatches completions*

   :param string: An instance of type :drm:`<string>`.
   :param possibilities: An instance of type ``limited(<sequence>, of: <string>)``.
   :param delimiters: An instance of type ``limited(<sequence>, of: <character>)``.
   :param #key action: An instance of type ``one-of(#"complete",
     #"complete-limited", #"complete-maximal", #"completions",
     #"apropos-completions")``. Default value ``#"complete"``.
   :param #key predicate: An instance of type ``false-or(<function>)``. Default
     value ``#f``.
   :param #key label-key: An instance of type ``<function>``. Default value :drm:`first`.
   :param #key value-key: An instance of type ``<function>``. Default value :drm:`second`.
   :value string: An instance of type ``false-or(<string>)``.
   :value success: An instance of type ``<boolean>``.
   :value object: An instance of type :drm:`<object>`.
   :value nmatches: An instance of type ``<integer>``.
   :valuecompletions: An instance of type :drm:`<sequence>`.

   :description:

     Completes *string* chunk-wise against the list of *possibilities*,
     using the specified *delimiters* to break both *string* and the strings
     in *possibilities* into chunks.

     *The label-key* and *value-key* arguments are used to extract the
     completion string and object from the entries in *possibilities*, and
     *predicate* (if supplied) is applied to filter out unwanted objects.

     The function returns five values: the completed string (if there is
     one), whether or not the completion successfully matched, the object
     associated with the completion, the number of things that matched, and
     (if *action* is ``#"completions"``) a sequence of possible completions.

     The *action* argument can take any of the following values:

     - ``#"complete"`` Completes the input as much as possible, except that if
       the user’s input exactly matches one of the possibilities, the
       shorter possibility is returned as the result, even if it is a left
       substring of another possibility.
     - ``#"complete-limited"`` Completes the input up to the next partial
       delimiter.
     - ``#"complete-maximal"`` Completes the input as much as possible.
     - ``#"completions"`` or ``#"apropos-completions"`` Returns a sequence of
       the possible completions.

   :example:

     .. code-block:: dylan

        complete-from-sequence("s w ma",
          #["one fish two fish",
            "red fish blue fish",
            "single white male",
            "on beyond zebra"],
          #[' ', '-'],
          label-key: identity,
          value-key: identity)

   :seealso:

     - :gf:`complete-from-generator`

.. generic-function:: compute-next-page

   Returns the next page in the specified wizard frame.

   :signature: compute-next-page *dialog* => *next-page*

   :param dialog: An instance of type :class:`<wizard-frame>`.
   :next-page: An instance of type ``false-or(<sheet>)``.

   :description:

     Returns the next page in *dialog*, which must be a wizard.

   :seealso:

     - :gf:`compute-previous-page`
     - :class:`<wizard-frame>`

.. generic-function:: compute-previous-page

   Returns the previous page in the specified wizard frame.

   :signature: compute-previous-page *dialog* => *prev-page*

   :param dialog: An instance of type :class:`<wizard-frame>`.
   :value prev-page: An instance of type ``false-or(<sheet>)``.

   :description:

     Returns the previous page in *dialog*, which must be a wizard.

   :seealso:

     - :gf:`compute-next-page`
     - :class:`<wizard-frame>`

.. generic-function:: contain

   Creates and returns a frame containing the specified object.

   :signature: contain *object* #rest *initargs* #key *own-thread?* #all-keys => *sheet* *frame*

   :param object: An instance of type ``type-union(<sheet>, <class>, <frame>)``.
   :param initargs: Instances of type :drm:`<object>`.
   :param #key own-thread?: An instance of type :drm:`<boolean>`.
   :value sheet: An instance of type :class:`<sheet>`.
   :value frame: An instance of type :class:`<frame>`.

   :description:

     Creates and returns a frame containing *object*. This function is
     intended to be used as a convenience function when testing sections of
     code in development; you are not recommended to use it in your final
     source code. The function wraps a set of DUIM objects in a frame and
     displays them on screen, without you needing to worry about the
     creation, management, or display of frames on the computer screen. The
     ``contain`` function is most useful when testing code interactively using
     the Dylan Interactor.

     If *own-thread?* is ``#t``, then the window that is created by ``contain``
     runs in its own thread. If not supplied, *own-thread?* is ``#f``.

     Consider the following expression that calls ``contain``:

     .. code-block:: dylan

        contain(make(<button>));

     This is equivalent to the fuller expression:

     .. code-block:: dylan

        begin
          let frame = make(<simple-frame>,
                           title: "container",
                           layout: make(<button>));
          start-frame(frame);
        end;

     As can be seen, when testing short pieces of code interactively in the
     environment, the former section of code is easier to use than the
     latter.

   :example:

     Assigning the result of a contain expression allows you to manipulate
     the DUIM objects being contained interactively, as shown in the example
     below.

     You should assume the following code is typed into the Dylan Interactor,
     and that each expression is evaluated by pressing the RETURN key at the
     points indicated.

     .. code-block:: dylan

        *g* := contain
          (make
            (<list-box>,
             items: #(#"One", #"Two", #"Three"),
             label-key:
               method (symbol) as-lowercase
                                 (as(<string>, symbol))
               end)); // RETURN
        gadget-items(*g*); // RETURN

     As you would expect, evaluating the call to
     :gf:`gadget-items` returns the following result:

     .. code-block:: dylan

        #(#"one", #"two", #"three")

     In a similar way, you can destructively modify the slot values of any
     contained DUIM objects

.. function:: current-frame

   Returns the current frame

   :signature: current-frame => *frame*
   :value frame: An instance of type :class:`<frame>`

   :description:

     Returns the current frame.

.. macro:: define command-table
   :defining:

   Defines a new class of command table with the specified name and
   properties.

   :macrocall:

     .. code-block:: dylan

        define command-table *name* ({*supers* }, *) {*options* } end

   :param name: A Dylan name *bnf*.
   :param supers: A Dylan name *bnf*.
   :param options: A Dylan body *bnf*.

   :description:

     Defines a new class of command table with the specified name and
     properties. This macro is equivalent to :drm:`define class <define_class>`,
     but with additional options.

     The *supers* argument specifies a comma-separated list of command tables
     from which the command table you are creating should inherit. If you are
     not explicitly inheriting the behavior of other command tables, then
     *supers* should have the value :var:`*global-command-table*`.

     Each one of the *options* supplied describes a command for the command
     table. This can be either a menu item, a separator, or another command
     table to be included in the command table. You can supply any number of
     options. Each option take one of the following forms:

     .. code-block:: dylan

        menu-item *menu-item-descriptor* ;

        include *command-table-name* ;

        separator;

     To add a menu item or menu to a command table, include an option of the
     following form:

     .. code-block:: dylan

        menu-item *label* = *command-function* #key *accelerator documentation*

     *label*
       An instance of :drm:`<string>`. This is the label that appears in
       the menu.

     *command-function*
       An instance of ``type-union(<command>, <command-table>, <function>)``.
       The command function is the callback that is invoked to perform the
       intended operation for the menu item. Note that this can itself be
       a command table.

     *accelerator*
       An instance of ``false-or(<gesture>)``. Default value: ``#f``. This
       defines a keyboard accelerator that can be used to invoke *command-function*
       in preference to the menu item itself.

     *documentation*
       An instance of ``false-or(<string>)``. Default value:
       ``#f``. This specifies a documentation string for the menu item that
       can be used to provide online help to the user. For menu items,
       documentation strings are usually displayed in the status bar of your
       application, when the mouse pointer is placed over the menu item
       itself.

     To add a separator to a menu, just include the following option at the
     point you want the separator to appear:

     .. code-block:: dylan

        separator;

     To include another command table in the current table, include the
     following option at the point you want the command table to appear:

     .. code-block:: dylan

        include *command-table-name* ;

     The commands defined in *command-table-name* are added to the current
     command table at the appropriate point.

   :example:

     The following example shows how you might create a command table for the
     standard Windows *File* menu, and how this could be integrated into the
     menu bar for an application. The example assumes that the appropriate
     command functions have already been defined for each command in the
     command table.

     .. code-block:: dylan

        define command-table
            *file-menu-command-table* (*global-command-table*)
          menu-item "New..." = frame-new-file,
            accelerator:
              make-keyboard-gesture(#"n", #"control"),
            documentation: "Creates a new document."
          menu-item "Open..." = frame-open-file,
            accelerator:
              make-keyboard-gesture(#"o", #"control"),
            documentation: "Opens an existing document.";
          menu-item "Close" = frame-close-file,
            documentation: "Closes an open document.";
          separator;
          include *save-files-command-table*;
          separator;
          menu-item "Exit"
            = make(<command>,
                   function: exit-frame);
        end command-table *file-menu-command-table*;

        define command-table
            *application-command-table* (*global-command-table*)
          menu-item "File" = *file-menu-command-table*;
          menu-item "Edit" = *edit-menu-command-table*;
          menu-item "View" = *view-menu-command-table*;
          menu-item "Windows" = *windows-menu-command-table*;
          menu-item "Help" = *help-menu-command-table*;
        end command-table *application-command-table*;
        
   :seealso:

     - :var:`*global-command-table*`

.. macro:: define frame
   :defining:

   Defines a new class of frame with the specified properties.

   :macrocall:

     .. code-block:: dylan

        define frame *name* ({*supers* }, *) {*slots-panes-options* } end

   :param name: A Dylan name *bnf*.
   :param supers: A Dylan name *bnf*.
   :param slots-panes-options: A Dylan body *bnf*.

   :description:

     Defines a new class of frame called *name* with the specified
     properties. This macro is equivalent to :drm:`define class <define_class>`,
     but with additional options.

     The *supers* argument lets you specify any classes from which the frame
     you are creating should inherit. You must include at least one concrete
     frame class, such as :class:`<simple-frame>` or :class:`<dialog-frame>`.

     The *slots-panes-options* supplied describe the state variables of the
     frame class; that is, the total composition of the frame. This includes,
     but is not necessarily limited to, any panes, layouts, tool bar, menus,
     and status bar contained in the frame. You can specify arbitrary slots
     in the definition of the frame. You may specify any of the following:

     - A number of slots for defining per-instance values of the frame
       state.
     - A number of named panes. Each pane defines a sheet of some sort.
     - A single layout.
     - A tool bar.
     - A status bar.
     - A menu bar.
     - A command table.
     - A number of sequential pages for inclusion in a multi-page frame such
       as a wizard or property dialog.

     .. note:: If the frame has a menu bar, either define the menu bar and its
        panes, or a command table, but not both. See the discussion below for
        more details.

     The syntax for each of these options is described below.

     The *slot* option allows you to define any slot values that the new
     frame class should allow. This option has the same syntax as slot
     specifiers in :drm:`define class <define_class>`, allowing you to
     define init-keywords, required init-keywords, init-functions and
     so on for the frame class.

     For each of the remaining options, the syntax is as follows::

       *option* *name* (*owner*) *body* ;

     The argument *option* is the name of the option used, taken from the
     list described below, *name* is the name you assign to the option for
     use within your code, *owner* is the owner of the option, usually the
     frame itself, and *body* contains the definition of value returned by
     the option.

     *pane* specifies a single pane in the frame. The default is ``#f``,
     meaning that there is no single pane. This is the simplest way to define
     a pane hierarchy.

     *layout* specifies the layout of the frame. The default is to lay out
     all of the named panes in horizontal strips. The value of this option
     must evaluate to an instance of a layout.

     *command-table* defines a command table for the frame. The default is to
     create a command table with the same name as the frame. The value of
     this option must evaluate to an instance of :class:`<command-table>`.

     *menu-bar* is used to specify the commands that will in the menu bar of
     the frame. The default is ``#t``. If used, it typically specifies the
     top-level commands of the frame. The value of this option can evaluate
     to any of the following:

     - ``#f`` The frame has no menu bar.
     - ``#t``, The menu bar for the frame is defined by the value of the
       *command-table* option.
     - A command table - The menu bar for the frame is defined by this command table.
     - A body of code This is interpreted the same way as the *menu-item*
       options to :macro:`define command-table`.

     *disabled-commands* is used to specify a list of command names that are
     initially disabled in the application frame. The default is ``#[]``. The
     set of enabled and disabled commands can be modified via
     :gf:`command-enabled?-setter`.

     *tool-bar* is used to specify a tool bar for the frame. The default is
     ``#f``. The value of this option must evaluate to an instance of
     :class:`<tool-bar>`.

     *top-level* specifies a function that executes the top level loop of the
     frame. It has as its argument a list whose first element is the name of
     a function to be called to execute the top-level loop. The function must
     take at least one argument, which is the frame itself. The rest of the
     list consists of additional arguments to be passed to the function.

     *icon* specifies an :class:`<image>` to be used in the window
     decoration for the frame. This icon may be used in the
     title bar of the frame, or when the frame is iconized, for example.

     *geometry* specifies the geometry for the frame.

     *pages* is used to define the pages of a wizard or property frame. This
     evaluates to a list of pages, each of which can be defined as panes
     within the frame definition itself. For example:

     .. code-block:: dylan

        define frame <wizard-type> (<wizard-frame>)
          ...
          pages (frame)
            vector(frame.page-1, frame.page-2, frame.page-3);
        end frame <wizard-type>

     The *name*, *supers*, and slot arguments are not evaluated. The values
     of each of the options are evaluated.

   :example:

     .. code-block:: dylan

        define frame <multiple-values-dialog> (<dialog-frame>)
          pane label-pane (frame)
            make(<option-box>, items: #("&Red", "&Green",
                                        "&Blue"));
          pane check-one (frame)
            make(<check-button>, label: "Check box test text");
          pane check-two (frame)
            make(<check-button>, label: "Check box test text");
          pane radio-box (frame)
            make(<radio-box>,
                 items: #("Option &1", "Option &2",
                          "Option &3", "Option &4"),
                 orientation: #"vertical");
          pane first-group-box (frame)
            grouping ("Group box", max-width: $fill)
              vertically (spacing: 4)
                make(<label>, label: "Label:");
                horizontally (spacing: 4,
                              y-alignment: #"center")
                  frame.label-pane;
                  make(<button>, label: "Button");
                end;
                frame.check-one;
                frame.check-two;
              end
            end;
          pane second-group-box (frame)
            grouping ("Group box", max-width: $fill)
              frame.radio-box
            end;
        layout (frame)
          vertically (spacing: 4)
            frame.first-group-box;
            frame.second-group-box;
          end;
        end frame <multiple-values-dialog>;

   :seealso:

     - :class:`<simple-frame>`
     - :class:`<wizard-frame>`

.. generic-function:: deiconify-frame

   Displays a frame that has previously been iconified on screen.

   :signature: deiconify-frame *frame* => ()

   :param frame: An instance of type :class:`<frame>`.

   :description:

     Displays a frame that has previously been iconified on screen.

   :example:

     The following example creates and displays a simple frame, then
     iconifies it and deiconifies it.

     .. code-block:: dylan

        define variable *frame* =
          make(<simple-frame>, title: "A frame",
               layout: make(<button>));
        start-frame(*frame*);
        iconify-frame(*frame*);
        deiconify-frame(*frame*);

   :seealso:

     - :gf:`destroy-frame`
     - :gf:`exit-frame`
     - :gf:`frame-icon`
     - :gf:`iconify-frame`

.. generic-function:: destroy-frame

   Unmaps the specified frame and destroys it.

   :signature: destroy-frame *frame* => ()

   :param frame: An instance of type :class:`<frame>`.

   :description:

     Unmaps *frame* from the screen and destroys it. Generally, you should
     not need to call this function explicitly, since
     :gf:`exit-frame` performs all necessary operations in the
     correct order, including calling :gf:`destroy-frame` if the *destroy?*
     argument to :gf:`exit-frame` is true.

   :seealso:

     - :gf:`deiconify-frame`
     - :gf:`exit-frame`
     - :class:`<frame-destroyed-event>`
     - :gf:`iconify-frame`
     - :gf:`lower-frame`
     - :gf:`raise-frame`

.. generic-function:: dialog-apply-button

   Returns the Apply button in the specified dialog.

   :signature: dialog-apply-button *dialog* => *apply-button*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value apply-button: An instance of type ``false-or(<button>)``.

   :description:

     Returns the Apply button in *dialog*. As well as having OK and Cancel
     buttons, many dialogs also have an Apply button that lets the user apply
     the changes that have been made in the dialog, without removing the
     dialog from the screen itself.

   :seealso:

     - :gf:`dialog-cancel-button`
     - :gf:`dialog-apply-button-setter`
     - :gf:`dialog-apply-callback`
     - :gf:`dialog-help-button`

.. generic-function:: dialog-apply-button-setter

   Specifies the Apply button in the specified dialog.

   :signature: dialog-apply-button-setter *apply-button dialog* => *apply-button*

   :param apply-button: An instance of type ``false-or(<button>)``.
   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value apply-button: An instance of type ``false-or(<button>)``.

   :description:

     Specifies the Apply button in *dialog*. As well as having OK and Cancel
     buttons, many dialogs also have an Apply button that lets the user apply
     the changes that have been made in the dialog, without removing the
     dialog from the screen itself.

   :seealso:

     - :gf:`dialog-cancel-button`
     - :gf:`dialog-apply-button`
     - :gf:`dialog-apply-callback`
     - :gf:`dialog-help-button`

.. generic-function:: dialog-apply-callback

   Returns the callback invoked when the Apply button is clicked in the
   specified dialog.

   :signature: dialog-apply-callback *dialog* => *callback*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value callback: An instance of type ``false-or(<command>, <function>)``.

   :description:

     Returns the callback invoked when the Apply button is clicked in
     *dialog*. As well as having OK and Cancel buttons, many dialogs also
     have an Apply button that lets the user apply the changes that have been
     made in the dialog, without removing the dialog from the screen itself.

     .. note:: If you supply ``#f`` as the callback, then the button does not
        appear.

   :seealso:

     - :gf:`dialog-cancel-button`
     - :gf:`dialog-apply-button`
     - :gf:`dialog-apply-button-setter`
     - :gf:`dialog-help-button`

.. generic-function:: dialog-back-button

   Returns the Back button in the specified multi-page dialog.

   :signature: dialog-back-button *dialog* => *back-button*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value back-button: An instance of type ``false-or(<button>)``.

   :description:

     Returns the Back button in *dialog*. This is most useful in multi-page
     dialogs such as property frames and wizard frames, which typically have
     Back and Next buttons that let the user navigate forward and backward
     through the sequence of pages that comprise the dialog.

   :seealso:

     - :gf:`dialog-back-button-setter`
     - :gf:`dialog-back-callback`
     - :gf:`dialog-exit-button`
     - :gf:`dialog-help-button`

.. generic-function:: dialog-back-button-setter

   Specifies the Back button in the specified multi-page dialog.

   :signature: dialog-back-button-setter *back-button dialog* => *back-button*

   :param back-button: An instance of type :class:`<button>`.
   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value back-button: An instance of type :class:`<button>`.

   :description:

     Specifies the Back button in *dialog*. This is most useful in wizard
     frames, which typically have Back and Next buttons that let the user
     navigate forward and backward through the sequence of pages that
     comprise the dialog.

   :seealso:

     - :gf:`dialog-back-button`
     - :gf:`dialog-back-callback`
     - :gf:`dialog-exit-button-setter`
     - :gf:`dialog-help-button`

.. generic-function:: dialog-back-callback

   Returns the callback invoked when the Back button is clicked in the
   specified multi-page dialog.

   :signature: dialog-apply-callback *dialog* => *callback*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value callback: An instance of type ``false-or(<command>, <function>)``.

   :description:

     Returns the callback invoked when the Back button is clicked in *dialog*.
     This is most useful in wizard frames, which typically have Back and
     Next buttons that let the user navigate forward and backward through the
     sequence of pages that comprise the dialog.

     .. note:: If you do not explicitly supply this callback, the previous page
        in the sequence for the multi-page dialog is displayed when the Back
        button is clicked. Specifying your own callback gives you flexibility in
        describing how the user can navigate through the sequence of pages in
        the dialog.

   :seealso:

     - :gf:`dialog-back-button`
     - :gf:`dialog-back-button-setter`
     - :gf:`dialog-exit-callback`
     - :gf:`dialog-help-button`

.. generic-function:: dialog-cancel-button

   Returns the Cancel button in the specified dialog.

   :signature: dialog-cancel-button *dialog* => *cancel-button*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value cancel-button: An instance of type ``false-or(<button>)``.

   :description:

     Returns the Cancel button in *dialog*.

   :seealso:

     - :gf:`dialog-cancel-button-setter`
     - :gf:`dialog-cancel-callback`
     - :gf:`dialog-exit-button`
     - :gf:`dialog-help-button`

.. generic-function:: dialog-cancel-button-setter

   Specifies the Cancel button in the specified dialog.

   :signature: dialog-cancel-button-setter *cancel-button* *dialog* => *cancel-button*

   :param cancel-button: An instance of type :class:`<button>`.
   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value cancel-button: An instance of type :class:`<button>`.

   :description:

     Specifies the Cancel button in *dialog*.

   :example:

     In the following example, a simple dialog frame is created, and then its
     cancel button is redefined before the dialog is displayed on screen.

     .. code-block:: dylan

        define variable *dialog*
          = make(<dialog-frame>,
                 exit-button?: #t,
                 cancel-button?: #t,
                 help-callback:
                   method (gadget)
                     notify-user (format-to-string
                       ("Here is some help",
                        gadget))
                   end);
        dialog-cancel-button-setter
          (make(<push-button>, label: "No",
                activate-callback: cancel-dialog,
                max-width: $fill), *dialog*);
        start-frame(*dialog*);

   :seealso:

     - :gf:`dialog-cancel-button`
     - :gf:`dialog-cancel-callback`
     - :gf:`dialog-exit-button-setter`
     - :gf:`dialog-help-button-setter`

.. generic-function:: dialog-cancel-callback

   Returns the function invoked when the cancel button is clicked in the
   specified dialog.

   :signature: dialog-cancel-callback *dialog* => *callback*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value callback: An instance of type ``false-or(type-union(<command>, <function>))``.

   :description:

     Returns the function invoked when the cancel button is clicked in
     *dialog*. This defaults to :gf:`cancel-dialog`.

   :seealso:

     - :gf:`cancel-dialog`
     - :gf:`dialog-cancel-button`
     - :gf:`dialog-cancel-button-setter`
     - :gf:`dialog-exit-callback`
     - :gf:`dialog-help-callback`

.. generic-function:: dialog-cancel-callback-setter

   Sets the function invoked when the cancel button is clicked in the
   specified dialog.

   :signature: dialog-cancel-callback-setter *callback* *dialog* => *callback*

    :param callback: An instance of type ``false-or(<command>, <function>)``.
      Default value: :gf:`cancel-dialog`.
    :param dialog: An instance of type :class:`<dialog-frame>`.
    :value callback: An instance of type ``false-or(<command>, <function>)``.

   :description:

     Sets the function invoked when the cancel button is clicked in *dialog*.

   :seealso:

     - :gf:`dialog-cancel-button`
     - :gf:`dialog-cancel-button-setter`
     - :gf:`dialog-exit-callback`
     - :gf:`dialog-help-callback`

.. generic-function:: dialog-current-page

   Returns the current page in the specified multi-page dialog.

   :signature: dialog-current-page *dialog* => *page*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value page: An instance of type ``false-or(<page>)``.

   :description:

     Returns the current page in *dialog*.

   :seealso:

     - :gf:`dialog-current-page-setter`

.. generic-function:: dialog-current-page-setter

   Sets the current page in the specified multi-page dialog.

   :signature: dialog-current-page-setter *page* *dialog* => *page*

   :param page: An instance of type :class:`<page>`.
   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value page: An instance of type :class:`<page>`.

   :description:

     Sets the current page in *dialog*.

   :seealso:

     - :gf:`dialog-current-page`

.. generic-function:: dialog-exit-button

   Returns the Exit button in the specified dialog.

   :signature: dialog-exit-button *dialog* => *exit-button*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value exit-button: An instance of type ``false-or(<button>)``.

   :description:

     Returns the Exit button in *dialog*. The Exit button is commonly found
     in multi-page dialogs, where the user is given the option to exit the
     sequence at any point (as well as navigate through the sequence using
     Next and Back buttons).

   :seealso:

     - :gf:`dialog-cancel-button`
     - :gf:`dialog-exit-button-setter`
     - :gf:`dialog-exit-enabled?`
     - :gf:`dialog-exit-callback`
     - :gf:`dialog-help-button`

.. generic-function:: dialog-exit-button-setter

   Specifies the Exit button in the specified dialog.

   :signature: dialog-exit-button-setter *exit-button* *dialog* => *exit-button*

   :param exit-button: An instance of type :class:`<button>`.
   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value exit-button: An instance of type :class:`<button>`.

   :description:

     Sets the Exit button in *dialog*. The Exit button is commonly found in
     multi-page dialogs, where the user is given the option to exit the
     sequence at any point (as well as navigate through the sequence using
     Next and Back buttons).

   :example:

     In the following example, a simple dialog frame is created, and then its
     exit button is redefined before the dialog is displayed on screen.

     .. code-block:: dylan

        define variable *dialog*
          = make(<dialog-frame>,
                 exit-button?: #t,
                 cancel-button?: #t,
                 help-callback:
                   method (gadget)
                     notify-user (format-to-string
                      ("Here is some help",
                       gadget))
                   end);
        dialog-exit-button-setter
          (make(<push-button>, label: "Yes",
                activate-callback: exit-dialog,
                max-width: $fill), *dialog*);
        start-frame(*dialog*);

   :seealso:

     - :gf:`dialog-cancel-button-setter`
     - :gf:`dialog-exit-button`
     - :gf:`dialog-exit-enabled?`
     - :gf:`dialog-exit-callback`
     - :gf:`dialog-help-button-setter`

.. generic-function:: dialog-exit-callback

   Returns the callback invoked when the Exit button is clicked in the
   specified dialog.

   :signature: dialog-exit-callback *dialog* => *callback*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value callback: An instance of type ``false-or(type-union(<command>, <function>))``.
     Default value: :gf:`exit-dialog`.

   :description:

     Returns the callback invoked when the Exit button is clicked in *dialog*.
     The Exit button is commonly found in multi-page dialogs, where the
     user is given the option to exit the sequence at any point (as well as
     navigate through the sequence using Next and Back buttons).

   :seealso:

     - :gf:`dialog-cancel-callback`
     - :gf:`dialog-exit-button`
     - :gf:`dialog-exit-button-setter`
     - :gf:`dialog-exit-callback-setter`
     - :gf:`dialog-help-callback`

.. generic-function:: dialog-exit-callback-setter

   Sets the callback invoked when the Exit button is clicked in the
   specified dialog.

   :signature: dialog-exit-callback *callback* *dialog* => *callback*

   :param callback: An instance of type ``false-or(type-union(<command>, <function>))``.
   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value callback: An instance of type ``false-or(type-union(<command>, <function>))``.

   :description:

     Sets the callback invoked when the Exit button is clicked in *dialog*.
     The Exit button is commonly found in multi-page dialogs, where the user
     is given the option to exit the sequence at any point (as well as
     navigate through the sequence using Next and Back buttons).

     If you do not supply this callback, then the default behavior is to quit
     the dialog when the Exit button is clicked. This is normally the action
     that you will want. Specifying your own callback gives you flexibility
     in describing other actions to be performed when the dialog is exited.
     In addition, supplying ``#f`` means that no Exit button is displayed at
     all.

   :seealso:

     - :gf:`dialog-cancel-callback-setter`
     - :gf:`dialog-exit-button`
     - :gf:`dialog-exit-button-setter`
     - :gf:`dialog-exit-callback`
     - :gf:`dialog-help-callback`

.. generic-function:: dialog-exit-enabled?

   Returns true if the Exit button has been enabled for the specified
   dialog.

   :signature: dialog-exit-enabled? *dialog* => *enabled?*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value enabled?: An instance of type ``<boolean>``.

   :description:

     Returns true if the Exit button has been enabled for *dialog*. The Exit
     button is commonly found in multi-page dialogs, where the user is given
     the option to exit the sequence at any point (as well as navigate
     through the sequence using Next and Back buttons).

   :seealso:

     - :gf:`dialog-exit-button`
     - :gf:`dialog-exit-button-setter`
     - :gf:`dialog-exit-enabled?-setter`
     - :gf:`dialog-exit-callback`

.. generic-function:: dialog-exit-enabled?-setter

   Enables or disables the Exit button for the specified dialog.

   :signature: dialog-exit-enabled?-setter *enabled?* *dialog* => *enabled?*

   :param enabled?: An instance of type ``<boolean>``.
   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value enabled?: An instance of type ``<boolean>``.

   :description:

     Enables or disables the Exit button for *dialog*. The Exit button is
     commonly found in multi-page dialogs, where the user is given the option
     to exit the sequence at any point (as well as navigate through the
     sequence using Next and Back buttons).

   :example:

     In this example, a dialog is created, and then its exit button is
     disabled. When displayed on the screen, the exit button is grayed out
     and you cannot click on it.

     .. code-block:: dylan

        define variable *dialog* =
          make(<dialog-frame>,
               exit-button?: #t,
               cancel-button?: #t,
               help-callback:
                 method (gadget)
                   notify-user
                     (format-to-string
                       ("Here is some help",
                      gadget))
                 end);
        dialog-exit-enabled?-setter(#f, *dialog*);
        start-frame(*dialog*);

   :seealso:

     - :gf:`dialog-exit-button`
     - :gf:`dialog-exit-button-setter`
     - :gf:`dialog-exit-enabled?`
     - :gf:`dialog-exit-callback`

.. class:: <dialog-frame>
   :open:
   :abstract:
   :instantiable:

   The class of dialog frames.

   :superclasses: :class:`<simple-frame>`

   :keyword mode: An instance of type ``one-of("modal", #"modeless", #"system-modal")``.
     Default value: ``#"modal"``.
   :keyword exit-callback: An instance of type ``false-or(type-union(<command>, <function>))``.
     Default value: :gf:`exit-dialog`.
   :keyword exit-button: An instance of type ``false-or(<button>)``.
     Default value: ``#f``.
   :keyword exit-enabled?: An instance of type ``<boolean>``.
     Default value: ``#t``.
   :keyword cancel-callback: An instance of type ``false-or(type-union(<command>, <function>))``.
     Default value: :gf:`cancel-dialog`.
   :keyword cancel-button: An instance of type ``false-or(<button>)``.
     Default value: ``#f``.
   :keyword help-callback: An instance of type ``false-or(type-union(<command>, <function>))``.
     Default value: ``#f``.
   :keyword help-button: An instance of type ``false-or(<button>)``.
     Default value: ``#f``.
   :keyword exit-buttons-position: An instance of type ``one-of(#"top", #"bottom", #"left", #"right")``. 
     Default value: ``#"bottom"``.
   :keyword pages: An instance of type ``false-or(<sequence>)``.
     Default value: ``#f``.
   :keyword page-changed-callback: An instance of type ``false-or(<function>)``.
     Default value: ``#f``.

   :description:

     The class of dialog frames. These frames let you create dialog boxes for
     use in your applications. All buttons in a dialog frame are
     automatically made the same size, and are placed at the bottom of the
     dialog by default. When at the bottom of the dialog, buttons are
     right-aligned.

     .. figure:: images/frames-4.png
        :align: center

        A typical dialog

     By default, all dialogs are modal, that is, when displayed, they take
     over the entire application thread, preventing the user from using any
     other part of the application until the dialog has been removed from the
     screen. To create a modeless dialog (that is, one that can remain
     displayed on the screen while the user interacts with the application in
     other ways) you should set the ``mode:`` keyword to ``#"modeless"``. Note,
     however, that you should not normally need to do this: if you need to
     create a modeless dialog, then you should consider using a normal DUIM
     frame, rather than a dialog frame.

     The init-keywords ``exit-button:``, and ``cancel-button:`` specify the exit
     and cancel buttons in the dialog. The user clicks on the exit button to
     dismiss the dialog and save any changes that have been made as a result
     of editing the information in the dialog. The user clicks on the cancel
     button in order to dismiss the dialog and discard any changes that have
     been made.

     In addition, the ``exit-callback:`` and ``cancel-callback:`` init-keywords
     specify the callback that is invoked when the Exit or Cancel buttons in
     the dialog are clicked on. These both default to the appropriate
     function for each button, but you have the flexibility to specify an
     alternative if you wish. If you do not require a Cancel button in your
     dialog, specify ``cancel-callback: #f``. Similarly, specify
     ``exit-callback: #f`` if you do not require an Exit button.

     All dialogs should have an exit button, and most dialogs should have a
     cancel button too. You should only omit the cancel button in cases when
     the information being displayed in the dialog cannot be changed by the
     user. For example, a dialog containing an error message can have only an
     exit button, but any dialog that contains information the user can edit
     should have both exit and cancel buttons.

     Two init-keywords are available for each button so that a given button
     may be specified for a particular dialog, but need only be displayed in
     certain circumstances. This lets you define subtly different behavior in
     different situations.

     The ``exit-enabled?:`` init-keyword is used to specify whether the exit
     button on the dialog is enabled or not. If ``#f``, then the exit button
     is displayed on the dialog, but it is grayed out.

     The ``help-button:`` init-keyword specifies the help button in the dialog.
     Note that, in contrast to the exit and cancel buttons, specifying the
     button gadget to use in a dialog determines its presence in the dialog:
     it is not possible to define a help button and then only display it in
     certain circumstances. You are strongly encouraged to provide a help
     button in all but the most trivial dialogs.

     The ``help-callback:`` init-keyword defines a callback function that is
     invoked when the help button is clicked. This should normally display a
     context-sensitive help topic from the help file supplied with the
     application, although you might also choose to display an alert box with
     the relevant information.

     The ``exit-buttons-position:`` init-keyword defines the position in the
     dialog that the exit and cancel buttons occupy (and any other standard
     buttons, if they have been specified). By default, buttons are placed
     where the interface guidelines for the platform recommend, and this
     position is encouraged in most interface design guidelines. Usually,
     this means that buttons are placed at the bottom of the dialog. Less
     commonly, buttons may also be placed on the right side of the dialog.
     Buttons are not normally placed at the top or on the left of the dialog,
     though this is possible if desired.

     The ``pages:`` init-keyword is used for multi-page dialogs such as
     property frames and wizard frames. If used, it should be a sequence of
     elements, each of which evaluates to an instance of a page.

     The ``page-changed-callback:`` is a callback function that is invoked when
     a different page in a multi-page dialog is displayed.

   :operations:

     - :gf:`cancel-dialog`
     - :gf:`dialog-cancel-button`
     - :gf:`dialog-cancel-button-setter`
     - :gf:`dialog-cancel-callback`
     - :gf:`dialog-cancel-callback-setter`
     - :gf:`dialog-exit-button`
     - :gf:`dialog-exit-button-setter`
     - :gf:`dialog-exit-callback`
     - :gf:`dialog-exit-callback-setter`
     - :gf:`dialog-exit-enabled?`
     - :gf:`dialog-exit-enabled?-setter`
     - :gf:`dialog-help-button`
     - :gf:`dialog-help-button-setter`
     - :gf:`dialog-help-callback`
     - :gf:`exit-dialog`
     - :gf:`start-dialog`

   :example:

     The following example creates and displays a simple dialog that contains
     only an exit button, cancel button, and help button, and assigns a
     callback to the help button.

     .. code-block:: dylan

        define variable *dialog*
          = make(<dialog-frame>,
                 exit-button?: #t,
                 cancel-button?: #t,
                 help-callback:
                   method (gadget)
                     notify-user (format-to-string
                       ("Here is some help",
                        gadget))
                   end);

        start-frame(*dialog*);

   :seealso:

     - :gf:`cancel-dialog`
     - :gf:`exit-dialog`
     - :class:`<property-frame>`
     - :class:`<simple-frame>`
     - :class:`<wizard-frame>`

.. generic-function:: dialog-help-button

   Returns the Help button in the specified dialog.

   :signature: dialog-help-button *dialog* => *help-button*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value help-button: An instance of type ``false-or(<button>)``.

   :description:

     Returns the Help button in *dialog*. Many dialogs contain a Help button
     that, when clicked, displays a relevant topic from the online help
     system for the application.

   :seealso:

     - :gf:`dialog-cancel-button`
     - :gf:`dialog-exit-button`
     - :gf:`dialog-help-button-setter`
     - :gf:`dialog-help-callback`

.. generic-function:: dialog-help-button-setter

   Specifies the Help button in the specified dialog.

   :signature: dialog-help-button-setter *help-button* *dialog* => *help-button*

   :param help-button: An instance of type ``false-or(<button>)``.
   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value help-button: An instance of type ``false-or(<button>)``

   :description:

     Specifies the Help button in *dialog*. Many dialogs contain a Help
     button that, when clicked, displays a relevant topic from the online
     help system for the application.

   :example:

     In the following example, a simple dialog frame is created, and then its
     help button is redefined before the dialog is displayed on screen.

     .. code-block:: dylan

        define variable *dialog*
          = make(<dialog-frame>,
                 exit-button?: #t,
                 cancel-button?: #t,
                 help-callback:
                   method (gadget)
                     notify-user (format-to-string
                       ("Here is some help",
                        gadget))
                   end);

        dialog-help-button-setter
          (make(<push-button>, label: "Help Me!",
                activate-callback:
                  method (gadget)
                    notify-user
                      (format-to-string
                         ("Here is some help",
                       gadget))
                  end);
                max-width: $fill), *dialog*);

        start-frame(*dialog*);

   :seealso:

     - :gf:`dialog-cancel-button-setter`
     - :gf:`dialog-exit-button-setter`
     - :gf:`dialog-help-button`
     - :gf:`dialog-help-callback`

.. generic-function:: dialog-help-callback

   Returns the callback invoked when the Help button is clicked in the
   specified dialog.

   :signature: dialog-help-callback *dialog* => *help-callback*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value help-callback: An instance of type ``false-or(type-union(<command>, <function>))``.

   :description:

     Returns the callback invoked when the Help button is clicked in *dialog*.
     Many dialogs contain a Help button that, when clicked, displays a
     relevant topic from the online help system for the application.

     .. note:: You must specify this callback in order to create a Help button
        in any dialog. If the callback is ``#f``, then there will be no Help
        button present in the dialog.

   :seealso:

     - :gf:`dialog-cancel-callback`
     - :gf:`dialog-exit-callback`
     - :gf:`dialog-help-button`
     - :gf:`dialog-help-button-setter`

.. generic-function:: dialog-next-button

   Returns the Next button in the specified multi-page dialog.

   :signature: dialog-next-button *dialog* => *next-button*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value next-button: An instance of type ``false-or(<button>)``.

   :description:

     Returns the Next button in *dialog*. This is most useful in multi-page
     dialogs such as property frames and wizard frames, which typically have
     Back and Next buttons that let the user navigate forward and backward
     through the sequence of pages that comprise the dialog.

   :seealso:

     - :gf:`dialog-back-button`
     - :gf:`dialog-exit-button`
     - :gf:`dialog-next-button-setter`
     - :gf:`dialog-next-callback`

.. generic-function:: dialog-next-button-setter

   Specifies the Next button in the specified multi-page dialog.

   :signature: dialog-next-button-setter *next-button dialog* => *next-button*

   :param next-button: An instance of type ``false-or(<button>)``.
   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value next-button: An instance of type ``false-or(<button>)``.

   :description:

     Specifies the Next button in *dialog*. This is most useful in
     multi-page dialogs such as property frames and wizard frames, which
     typically have Back and Next buttons that let the user navigate forward
     and backward through the sequence of pages that comprise the dialog.

   :seealso:

     - :gf:`dialog-back-button-setter`
     - :gf:`dialog-exit-button`
     - :gf:`dialog-next-button`
     - :gf:`dialog-next-callback`

.. generic-function:: dialog-next-callback

   Returns the callback invoked when the Next button is clicked in the
   specified multi-page dialog.

   :signature: dialog-next-callback *dialog* => *callback*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value callback: An instance of type ``false-or(type-union(<command>, <function>))``.

   :description:

     Returns the callback invoked when the Next button is clicked in *dialog*.
     This is most useful in multi-page dialogs such as property frames and
     wizard frames, which typically have Back and Next buttons that let the
     user navigate forward and backward through the sequence of pages that
     comprise the dialog.

     .. note:: If you do not explicitly supply this callback, the next page in
        the sequence for the multi-page dialog is displayed when the Next button
        is clicked. Specifying your own callback gives you flexibility in
        describing how the user can navigate through the sequence of pages in
        the dialog.

     The default value for this callback is :gf:`move-to-next-page`.

   :seealso:

     - :gf:`dialog-back-button`
     - :gf:`dialog-exit-callback`
     - :gf:`dialog-next-button`
     - :gf:`dialog-next-button-setter`
     - :gf:`move-to-next-page`

.. generic-function:: dialog-next-enabled?

   Returns true if the Next button has been enabled for the specified
   multi-page dialog.

   :signature: dialog-next-enabled? *dialog* => *enabled?*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value enabled?: An instance of type ``<boolean>``.

   :description:

     Returns true if the Next button has been enabled for *dialog*. This
     button is most useful in multi-page dialogs such as property frames and
     wizard frames, which typically have Back and Next buttons that let the
     user navigate forward and backward through the sequence of pages that
     comprise the dialog.

   :seealso:

     - :class:`<dialog-frame>`
     - :gf:`dialog-next-button`
     - :gf:`dialog-next-button-setter`
     - :gf:`dialog-next-enabled?-setter`
     - :gf:`dialog-next-callback`

.. generic-function:: dialog-next-enabled?-setter

   Enables or disables the Next button for the specified multi-page dialog.

   :signature: dialog-next-enabled?-setter *enabled?* *dialog* => *enabled?*

   :param enabled?: An instance of type ``<boolean>``.
   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value enabled?: An instance of type ``<boolean>``.

   :description:

     Enables or disables the Next button for *dialog*. This button is most
     useful in multi-page dialogs such as property frames and wizard frames,
     which typically have Back and Next buttons that let the user navigate
     forward and backward through the sequence of pages that comprise the
     dialog.

     It is useful to be able to enable and disable the Next button at any
     point in order to ensure that the user supplies all necessary
     information before proceeding to the next page of the dialog. You can do
     this by testing to see if the information on the page has been specified
     with :gf:`dialog-page-complete?`, and then enabling
     or disabling the Next button as appropriate.

   :seealso:

     - :gf:`dialog-next-button`
     - :gf:`dialog-next-button-setter`
     - :gf:`dialog-next-callback`
     - :gf:`dialog-next-enabled?`

.. generic-function:: dialog-next-page

   Returns the next page in sequence for the specified multi-page dialog.

   :signature: dialog-next-page *dialog* => *next-page*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value next-page: An instance of type ``false-or(<page>)``.

   :description:

     Returns the next page in sequence for *dialog*. This is for use in
     multi-page dialogs such as property frames and wizard frames, which
     typically have Back and Next buttons that let the user navigate forward
     and backward through the sequence of pages that comprise the dialog.

     The default method for the Next button in *dialog* uses the value of
     this function. When the Next button is clicked, the current page is set
     to the next logical page in the sequence, but you are free to
     dynamically change it as the state of the dialog changes.

   :seealso:

     - :gf:`dialog-next-button`
     - :gf:`dialog-next-button-setter`
     - :gf:`dialog-next-callback`
     - :gf:`dialog-next-page-setter`
     - :gf:`dialog-previous-page`

.. generic-function:: dialog-next-page-setter

   Specifies the next page in sequence for the specified multi-page dialog.

   :signature: dialog-next-page-setter *next-page* *dialog* => *next-page*

   :param next-page: An instance of type ``false-or(<page>)``.
   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value next-page: An instance of type ``false-or(<page>)``.

   :description:

     Specifies the next page in sequence for *dialog*. This is for use in
     multi-page dialogs such as property frames and wizard frames, which
     typically have Back and Next buttons that let the user navigate forward
     and backward through the sequence of pages that comprise the dialog.

     The default method for the Next button in *dialog* uses the value of
     this function. When the Next button is clicked, the current page is set
     to the next logical page in the sequence, but you are free to
     dynamically change it as the state of the dialog changes.

   :seealso:

     - :gf:`dialog-next-button`
     - :gf:`dialog-next-button-setter`
     - :gf:`dialog-next-callback`
     - :gf:`dialog-next-page`
     - :gf:`dialog-previous-page-setter`

.. generic-function:: dialog-page-changed-callback

   Returns the page-changed callback of the specified multi-page dialog.

   :signature: dialog-page-changed-callback *dialog* => *callback*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value callback: An instance of type ``false-or(type-union(<command>, <function>))``.

   :description:

     Returns the page-changed-callback of *dialog*. This is the callback
     function used to test whether the information in the current page of
     *dialog* has changed. This callback is useful when using multi-page
     dialogs, as a test that can be performed before the next page of the
     dialog is displayed.

   :seealso:

     - :class:`<dialog-frame>`
     - :gf:`dialog-page-changed-callback-setter`
     - :class:`<property-frame>`
     - :class:`<wizard-frame>`

.. generic-function:: dialog-page-changed-callback-setter

   Sets the page-changed callback of the specified multi-page dialog.

   :signature: dialog-page-changed-callback-setter *callback* *dialog* => *callback*

   :param callback: An instance of type ``false-or(type-union(<command>, <function>))``.
   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value callback: An instance of type ``false-or(type-union(<command>, <function>))``.

   :description:

     Sets the page-changed-callback of *dialog*. This is the callback
     function used to test whether the information in the current page of
     *dialog* has changed. This callback is useful when using multi-page
     dialogs, as a test that can be performed before the next page of the
     dialog is displayed.

   :seealso:

     - :class:`<dialog-frame>`
     - :gf:`dialog-page-changed-callback`
     - :class:`<property-frame>`
     - :class:`<wizard-frame>`

.. generic-function:: dialog-page-complete?

   Returns true if all the information required on the current page of the
   specified multi-page dialog has been specified.

   :signature: dialog-page-complete? *dialog* => *complete?*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value complete?: An instance of type ``<boolean>``.

   :description:

     Returns true if all the information required on the current page in
     *dialog* has been specified by the user. This generic function has two
     uses:

     - It can be used within wizards to test whether all the necessary
       information has been supplied, before moving on to the next page of
       the wizard.
     - It can be used within property pages to test whether all the
       necessary information has been supplied, before allowing the user to
       apply any changes.

   :seealso:

     - :gf:`dialog-page-complete?-setter`

.. generic-function:: dialog-page-complete?-setter

   Sets the slot that indicates all the information required on the current
   page of the specified multi-page dialog has been specified.

   :signature: dialog-page-complete? *complete?* *dialog* => *complete?*

   :param complete?: An instance of type ``<boolean>``.
   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value complete?: An instance of type ``<boolean>``.

   :description:

     Sets the slot that indicates all the information required on the current
     page in *dialog* has been specified by the user. This generic function
     has two uses:

     - It can be used within wizards to indicate that the necessary
       information has been supplied, so that the next page of the wizard
       can be displayed safely.
     - It can be used within property pages to indicate that the necessary
       information has been supplied, so that the user can apply any
       changes.

   :seealso:

     - :gf:`dialog-page-complete?`

.. generic-function:: dialog-pages

   Returns the pages of the specified multi-page dialog.

   :signature: dialog-pages *dialog* => *pages*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value pages: An instance of type ``limited(<sequence>, of: <page>)``.

   :description:

     Returns the pages of *dialog*. Each of the items in sequence is an
     instance of :class:`<page>`.

   :seealso:

     - :class:`<dialog-frame>`
     - :gf:`dialog-pages-setter`
     - :class:`<property-frame>`
     - :class:`<wizard-frame>`

.. generic-function:: dialog-pages-setter

   Sets the pages of the specified multi-page dialog.

   :signature: dialog-pages-setter *pages* *dialog* => *pages*
   :param pages: An instance of type ``limited(<sequence>, of: <page>)``.
   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value pages: An instance of type ``limited(<sequence>, of: <page>)``.

   :description:

     Sets the pages of *dialog*. Each of the items in sequence must be an
     instance of :class:`<page>`.

   :seealso:

     - :class:`<dialog-frame>`
     - :gf:`dialog-pages`
     - :class:`<property-frame>`
     - :class:`<wizard-frame>`

.. generic-function:: dialog-previous-page

   Returns the previous page in sequence for the specified multi-page
   dialog.

   :signature: dialog-previous-page *dialog* => *previous-page*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value previous-page: An instance of type ``false-or(<page>)``.

   :description:

     Returns the previous page in sequence for *dialog*. This is for use in
     multi-page dialogs such as property frames and wizard frames, which
     typically have Back and Next buttons that let the user navigate forward
     and backward through the sequence of pages that comprise the dialog.

     The default method for the Back button in *dialog* uses the value of
     this function. When the Back button is clicked, the current page is set
     to the previous logical page in the sequence, but you are free to
     dynamically change it as the state of the dialog changes.

   :seealso:

     - :gf:`dialog-back-button`
     - :gf:`dialog-back-button-setter`
     - :gf:`dialog-back-callback`
     - :gf:`dialog-next-page`
     - :gf:`dialog-previous-page-setter`

.. generic-function:: dialog-previous-page-setter

   Specifies the previous page in sequence for the specified multi-page
   dialog.

   :signature: dialog-previous-page-setter *previous-page* *dialog* => *previous-page*

   :param previous-page: An instance of type ``false-or(<page>)``.
   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value previous-page: An instance of type ``false-or(<page>)``.

   :description:

     Specifies the previous page in sequence for *dialog*. This is for use
     in multi-page dialogs such as property frames and wizard frames, which
     typically have Back and Next buttons that let the user navigate forward
     and backward through the sequence of pages that comprise the dialog.

     The default method for the Back button in *dialog* uses the value of
     this function. When the Back button is clicked, the current page is set
     to the previous logical page in the sequence, but you are free to
     dynamically change it as the state of the dialog changes.

   :seealso:

     - :gf:`dialog-back-button`
     - :gf:`dialog-back-button-setter`
     - :gf:`dialog-back-callback`
     - :gf:`dialog-next-page-setter`
     - :gf:`dialog-previous-page`

.. generic-function:: display-progress-note

   Displays the specified progress note.

   :signature: display-progress-note *framem* *progress-note* => ()

   :param framem: An instance of type :class:`<frame-manager>`.
   :param progress-note: An instance of type :class:`<progress-note>`.


   :description:

     Displays the specified *progress-note* in the frame managed by *framem*.

.. generic-function:: event-destroy-frame?

   Returns information about the frame was destroyed in the specified
   event.

   :signature: event-destroy-frame? *event* => *destroyed?*

   :param event: An instance of type :class:`<frame-exit-event>`.
   :value destroyed?: An instance of type ``<boolean>``.

   :description:

     Returns information about the frame was destroyed in *event*.

   :seealso:

     - :class:`<frame-exit-event>`

.. generic-function:: event-status-code

   Returns the status code of the specified event.

   :signature: event-status-code *event* => *code*

   :param event: An instance of type :class:`<frame-exited-event>`.
   :value code: An instance of type ``false-or(<integer>)``.

   :description:

     Returns the status code of *event*.

   :seealso:

     - :class:`<frame-exited-event>`

.. generic-function:: execute-command

   Executes a command for the specified frame.

   :signature: execute-command *command* *frame* => #rest *values*

   :param command: An instance of type :class:`<command>`.
   :param frame: An instance of type :class:`<frame>`.
   :value #rest values: Instances of type :drm:`<object>`.

   :description:

     Executes *command* for *frame*. The values returned are those values
     returned as a result of evaluating the command function of *command*.

.. generic-function:: exit-dialog

   Exits the specified dialog.

   :signature: exit-dialog *dialog* #key *destroy?* => ()

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :destroy?: An instance of type ``<boolean>``. Default value: ``#t``.

   :description:

     Exits *dialog*, recording any changes to the information displayed in
     the dialog that have been made by the user.

     This is the default callback used for the exit button in a dialog. This
     is the button that is typically labeled *OK*.

     If *destroy?* is ``#t``, then dialog is destroyed.

   :example:

     The following example defines a button, ``*yes-button*``, that calls
     :gf:`exit-dialog` as its *activate-callback*. This button is then used in a
     dialog that simply replaces the standard exit button for the newly
     defined dialog. Note that the example assumes the existence of a similar
     ``*no-button*`` to replace the cancel button.

     .. code-block:: dylan

        define variable *yes-button*
          = make(<push-button>, label: "Yes",
                 activate-callback: exit-dialog,
                 max-width: $fill);

        define variable *dialog*
          = make(<dialog-frame>,
                 exit-button?: #f,
                 cancel-button?: #f,
                 layout: vertically
                           (x-alignment: #"center",
                            y-spacing: 5)
                           make(<label>,
                                label: "Here is a label");
                           horizontally (x-spacing: 2)
                             *yes-button*;
                             *no-button*;
                           end
                         end);

        start-frame(*dialog*);

   :seealso:

     - :gf:`cancel-dialog`
     - :class:`<dialog-frame>`
     - :gf:`start-dialog`

.. generic-function:: exit-frame

   Unmaps the specified frame destroying it required.

   :signature: exit-frame *frame* #key *destroy?* => ()

   :param frame: An instance of type :class:`<frame>`.
   :param destroy?: An instance of type ``<boolean>``. Default value: ``#t``.

   :description:

     Unmaps *frame*, removing the associated sheet and its children from the
     screen. If *destroy?* is true, then the frame is destroyed completely,
     via a call to :gf:`destroy-frame`.

     If *destroy?* is ``#t``, then dialog is destroyed.

   :example:

     The following example creates a simple frame, then displays it and exits
     it. You should run this code in the interactor, pressing the RETURN key
     at the points indicated.

     .. code-block:: dylan

        define variable *frame* =
          make(<simple-frame>, title: "A frame",
               layout: make(<button>)); // RETURN
        start-frame(*frame*); // RETURN
        exit-frame(*frame*); // RETURN

   :seealso:

     - :gf:`destroy-frame`
     - :gf:`frame-can-exit?`
     - :class:`<frame-exited-event>`
     - :class:`<frame-exit-event>`
     - :gf:`frame-mapped?-setter`
     - :gf:`start-frame`

.. function:: find-frame

   Returns a frame of the specified type, creating one if necessary.

   :signature: find-frame *frame-class* #rest *initargs* #key *create?* *activate?* *own-thread?* *port* *frame-manager* *test* #all-keys => *frame*

   :param frame-class: An instance of type :drm:`<object>`.
   :param #rest initargs: An instance of type :drm:`<object>`.
   :param #key create?: An instance of type ``<boolean>``. Default value: ``#t``.
   :param #key activate?: An instance of type ``<boolean>``. Default value: ``#t``.
   :param #key own-thread?: An instance of type ``<boolean>``. Default value: ``#t``.
   :param #key port: An instance of type :class:`<port>`.
   :param #key frame-manager: An instance of type :class:`<frame-manager>`.
   :param #key test: An instance of type ``<function>``. Default value: :drm:`identity`.
   :value frame: An instance of type :class:`<frame>`.

   :description:

     This function creates a frame of the specified type if one does not
     already exist, and then runs it, possibly in its own thread. If one
     already exists, then it is selected.

     The *frame-class* argument specifies the class of frame that is being
     searched for. By default, if a match is not found, then an instance of
     this class will be created.

     The *init-args* supplied are the slot values that should be passed to
     the instance of frame-class. Either an existing frame must be found that
     has the specified slot values, or a new one will be created.

     If *create?* is ``#f``, then a new frame will not be created if it does
     not already exist.

     If *own-thread?* is true, the frame will run in its own thread if one is
     created.

     The *port* and *frame-manager* arguments specify a port and frame
     manager which control the frame being searched for, or under the control
     of which a new frame should be created.
     
     If desired, you can supply a *test* which must evaluate to true for a
     frame to match successfully.

   :seealso:

     - :class:`<frame>`

.. class:: <frame>
   :open:
   :abstract:

   The base class of all frames.

   :superclasses: :drm:`<object>`

   :keyword owner: An instance of type ``false-or(<frame>)``. Default value: ``#f``.
   :keyword mode: An instance of type ``one-of(#"modeless", #"modal", #"system-modal")``.
     Default value: ``#"modeless"``.
   :keyword default-button: An instance of type ``false-or(<button>)``.
     Default value: ``#f``.
   :keyword x: An instance of type ``<integer>``.
   :keyword y: An instance of type ``<integer>``.
   :keyword width: An instance of type ``<integer>``.
   :keyword height: An instance of type ``<integer>``.
   :keyword disabled-commands: An instance of type :drm:`<sequence>`.
   :keyword top-level-sheet: An instance of type ``false-or(<sheet>)``.
     Default value: ``#f``.
   :keyword layout: An instance of type :class:`<layout>`.
   :keyword icon: An instance of type ``false-or(<image>)``.
   :keyword title: An instance of type ``false-or(<string>)``.
     Default value: ``#f``.
   :keyword calling-frame: An instance of type :class:`<frame>`.
   :keyword state: An instance of type ``one-of(#"detached", #"unmapped",
     #"mapped", #"iconified")``.
     Default value: ``#"detached"``.
   :keyword thread: An instance of type ``false-or(<thread>)``.
     Default value: ``#f``.
   :keyword event-queue: An instance of type ``false-or(<event-queue>)``.
     Default value: ``#f``.
   :keyword input-focus: An instance of type ``false-or(<sheet>)``.
     Default value: ``#f``.
   :keyword foreground: An instance of type ``false-or(<ink>)``.
   :keyword background: An instance of type ``false-or(<int>)``.
   :keyword text-style: An instance of type ``false-or(<text-style>)``.
   :keyword palette: An instance of type ``false-or(<palette>)``.
     Default value: ``#f``.
   :keyword document: An instance of type ``false-or(<object>)``.
     Default value: ``#f``.
   :keyword resource-id: An instance of type ``false-or(<integer>)``.
   :keyword resizable?: An instance of type ``<boolean>``. Default value: ``#t``.
   :keyword fixed-width?: An instance of type ``<boolean>``. Default value: ``#f``.
   :keyword fixed-height?: An instance of type ``<boolean>``. Default value: ``#f``.

   :description:

     The class of all frames.

     The ``owner:`` init-keyword is the parent of the frame.

     The ``mode:`` init-keyword lets you specify the mode for the frame. By
     default, frames are modeless, that is, they do not take over control of
     the whole application when they are mapped, and the user can interact
     with other frames in the application normally. Modal frames, on the
     other hand, behave like a :class:`<dialog-frame>`,
     restricting the user’s interaction with other frames in the application
     until the modal frame has been dismissed.

     The ``default-button:`` init-keyword is used to specify which button is
     the default action for the frame. The default button is usually the one
     whose callback is invoked by pressing the RETURN key.

     The ``x:``, ``y:``, ``width:`` and ``height:`` init-keywords lets you specify
     the initial size and position of the frame. The position is specified
     using ``x:`` and ``y:``, which represent the number of pixels from the top
     left corner of the screen, and the ``width:`` and ``height:`` init-keywords
     specify the initial size of the frame.

     The ``title:`` init-keyword is used to specify a title for the frame.

     The ``state:`` init-keyword is used to specify the initial state of the
     frame. This describes whether the frame is mapped, whether it is
     iconified, and so on. By default, new frames are detached.

     By default, new frames run in their own thread. If desired, a frame can
     be run in an existing thread by setting the ``thread:`` init-keyword to
     the thread object concerned. For more information about threads, see the
     manual *Library Reference: Core Features*.

     As with threads, new frame run in their own event-queue by default. To
     run the frame in an existing event-queue, use the ``event-queue:``
     init-keyword.

     You can specify which sheet in the frame initially has the input-focus
     using the ``input-focus:`` init-keyword. The input-focus dictates where
     information can be typed by default.

     The ``foreground:`` ``background:``, and ``text-style:`` init-keywords
     describes the colors and fonts used in the frame.

     Specify a palette for the frame using the ``palette:`` init-keyword.

     Specify a resource-id for the frame using the ``resource-id:``
     init-keyword. This is a platform-specific ID or determining which
     resource to use to fill in a frame.

     The ``resizable?:``, ``fixed-width?:``, and ``fixed-height?:`` init-keywords
     let you specify whether or not the user can resize the frame. If
     ``resizable?:`` is ``#t``, then the frame can be resized in either
     direction; if it is #f, then it cannot be resized at all. In addition,
     if ``resizable?:`` is ``#t``, and one of ``fixed-width?:`` or
     ``fixed-height?:`` is also ``#t``, then the frame is resizable, but is
     fixed in the appropriate direction. For example, if ``resizable?:`` is ``#t``
     and ``fixed-height?:`` is also ``#t``, then only the width of the frame can
     be resized.

   :operations:

     The following operations are exported from the *DUIM-Frames* module.

     - :gf:`apply-in-frame`
     - :gf:`call-in-frame`
     - :gf:`command-enabled?`
     - :gf:`command-enabled?-setter`
     - :gf:`deiconify-frame`
     - :gf:`destroy-frame`
     - :gf:`execute-command`
     - :gf:`exit-frame`
     - :gf:`frame?`
     - :gf:`frame-accelerators`
     - :gf:`frame-accelerators-setter`
     - :gf:`frame-can-exit?`
     - :gf:`frame-default-button`
     - :gf:`frame-default-button-setter`
     - :gf:`frame-event-queue`
     - :gf:`frame-icon`
     - :gf:`frame-icon-setter`
     - :gf:`frame-input-focus`
     - :gf:`frame-input-focus-setter`
     - :gf:`frame-mapped?`
     - :gf:`frame-mapped?-setter`
     - :gf:`frame-mode`
     - :gf:`frame-owner`
     - :gf:`frame-palette`
     - :gf:`frame-palette-setter`
     - :gf:`frame-position`
     - :gf:`frame-size`
     - :gf:`frame-state`
     - :gf:`frame-thread`
     - :gf:`frame-title`
     - :gf:`frame-title-setter`
     - :gf:`iconify-frame`
     - :gf:`lower-frame`
     - :gf:`layout-frame`
     - :gf:`raise-frame`
     - :gf:`redo-command`
     - :gf:`set-frame-position`
     - :gf:`set-frame-size`
     - :gf:`undo-command`

     The following operations are exported from the *DUIM-Sheets* module.

     - :gf:`beep`
     - :gf:`display`
     - :gf:`force-display`
     - :gf:`frame-manager`
     - :gf:`handle-event`

     The following operations are exported from the *DUIM-DCs* module.

     - :gf:`default-background`
     - :gf:`default-foreground`
     - :gf:`default-text-style`
     - :gf:`find-color`
     - :gf:`port`
     - :gf:`queue-event`
     - :gf:`synchronize-display`
     - :gf:`top-level-sheet`

.. generic-function:: frame?

   Returns true if the specified object is a frame.

   :signature: frame? *object* => *frame?*

   :param object: An instance of type :drm:`<object>`.
   :value frame?: An instance of type ``<boolean>``.

   :description:

     Returns true if *object* is a frame. Use this generic function to test
     that an object is a frame before carrying out frame-related operations
     on it.

   :seealso:

     - :gf:`current-frame`
     - :class:`<frame>`

.. generic-function:: frame-accelerators

   Returns the keyboard accelerators defined for the specified frame.

   :signature: frame-accelerators *frame* => *accelerators*

   :param frame: An instance of type :class:`<frame>`.
   :value accelerators: An instance of type ``false-or(limited(<sequence>, of: <gesture>))``.

   :description:

     Returns the keyboard accelerators defined for *frame*.

   :seealso:

     - :gf:`frame-accelerators-setter`

.. generic-function:: frame-accelerators-setter

   Defines the keyboard accelerators for the specified frame.

   :signature: frame-accelerators *accelerators* *frame* => *accelerators*

   :param accelerators: An instance of type ``false-or(limited(<sequence>, of: <gesture>))``.
   :param frame: An instance of type :class:`<frame>`.
   :value accelerators: An instance of type ``false-or(limited(<sequence>, of: <gesture>))``.

   :description:

     Defines the keyboard accelerators for *frame*.

   :seealso:

     :gf:`frame-accelerators`

.. generic-function:: frame-can-exit?
   :open:

   Returns true if the specified frame can be exited dynamically.

   :signature: frame-can-exit? *frame* => *can-exit?*

   :param frame: An instance of type :class:`<frame>`.
   :value can-exit?: An instance of type ``<boolean>``.

   :description:

     Returns true if *frame* can be exited dynamically. You can add methods
     to this generic function in order to allow the user to make a dynamic
     decision about whether a frame should exit.

   :example:

     .. code-block:: dylan

        define method frame-can-exit?
            (frame :: <abstract-test-frame>)
         => (can-exit? :: <boolean>)
          notify-user("Really exit?",
                      frame: frame,
                      style: #"question")
        end method frame-can-exit?;

   :seealso:

     - :gf:`exit-frame`

.. generic-function:: frame-command-table

   Returns the command table associated with the specified frame.

   :signature: frame-command-table *frame* => *command-table*

   :param frame: An instance of type :class:`<frame>`.
   :value command-table: An instance of type :class:`<command-table>`.

   :description:

     Returns the command table associated with *frame*.

   :seealso:

     - :gf:`frame-command-table-setter`

.. generic-function:: frame-command-table-setter

   Specifies the command table associated with the specified frame.

   :signature: frame-command-table-setter *command-table* *frame* => *command-table*

   :param command-table: An instance of type :class:`<command-table>`.
   :param frame: An instance of type :class:`<frame>`.
   :value command-table: An instance of type :class:`<command-table>`.

   :description:

     Specifies the command table associated with *frame*.

   :seealso:

     - :gf:`frame-command-table`

.. class:: <frame-created-event>
   :sealed:
   :instantiable:

   The class of events that indicate a frame has been created.

   :superclasses: :class:`<frame-event>`

   :description:

     The class of events that indicate a frame has been created. An instance
     of this class is distributed to the frame when it is created. Only one
     of these events is passed during the lifetime of any frame.

   :seealso:

     - :class:`<frame-destroyed-event>`
     - :class:`<frame-exited-event>`

.. class:: <frame-destroyed-event>
   :sealed:
   :instantiable:

   The class of events that indicate a frame has been destroyed.

   :superclasses: :class:`<frame-event>`

   :description:

     The class of events that indicate a frame has been destroyed. An
     instance of this class is distributed to the frame when it is destroyed.
     Only one of these events is passed during the lifetime of any frame.

   :seealso:

     - :gf:`destroy-frame`
     - :class:`<frame-created-event>`
     - :class:`<frame-exited-event>`

.. generic-function:: frame-default-button

   Returns the default button associated with the specified frame.

   :signature: frame-default-button *frame* => *default-button*

   :param frame: An instance of type :class:`<frame>`.
   :value default-button: An instance of type ``false-or(<button>)``.

   :description:

     Returns the default button associated with *frame*.

   :seealso:

     - :gf:`frame-default-button-setter`

.. generic-function:: frame-default-button-setter

   Sets the default button associated with the specified frame.

   :signature: frame-default-button-setter *default-button* *frame* => *default-button*

   :param default-button: An instance of type ``false-or(<button>)``.
   :param frame: An instance of type :class:`<frame>`.
   :value default-button: An instance of type ``false-or(<button>)``.

   :description:

     Sets the default button associated with *frame*.

   :seealso:

     - :gf:`frame-default-button`

.. generic-function:: frame-event-queue

   Returns the event queue that the specified frame is running in.

   :signature: frame-event-queue *frame* => *event-queue*

   :param frame: An instance of type :class:`<frame>`.
   :value event-queue: An instance of type :class:`<event-queue>`.

   :description:

     Returns the event queue that *frame* is running in.

   :seealso:

     - :class:`<frame>`

.. class:: <frame-exited-event>
   :sealed:
   :instantiable:

   The class of events that indicate a frame has been exited.

   :superclasses: :class:`<frame-event>`

   :keyword status-code: An instance of type ``false-or(<integer>)``.

   This class also inherits the ``frame:`` init-keyword from its superclass.

   :description:

   :example:

     The class of events that indicate a frame has been exited. An instance
     of this class is distributed to the frame when it is exited. Only one of
     these events is passed during the lifetime of any frame.

     The ``status-code:`` init-keyword is used to pass a status code, if
     desired. This code can be used to pass the reason that the frame was
     exited.

   :seealso:

     - :class:`<application-exited-event>`
     - :gf:`exit-frame`
     - :class:`<frame-created-event>`
     - :class:`<frame-destroyed-event>`

.. class:: <frame-exit-event>
   :sealed:
   :instantiable:

   The class of events distributed when a frame is about to exit.

   :superclasses: :class:`<frame-event>`

   :keyword destroy-frame?: An instance of type ``<boolean>``.
     Default value: ``#f``.

   :description:

     The class of events distributed when a frame is about to exit. Contrast
     this with :class:`<frame-exited-event>`, which is
     passed after the frame is exited.

     The default method uses :gf:`frame-can-exit?` to
     decide whether or not to exit.

     If ``destroy-frame?:`` is ``#t``, then the frame is destroyed.

   :seealso:

     - :gf:`event-destroy-frame?`
     - :gf:`frame-can-exit?`
     - :class:`<frame-exited-event>`

.. class:: <frame-focus-event>
   :sealed:
   :instantiable:

   The class of events distributed when a frame receives focus.

   :superclasses: :class:`<frame-event>`

   :description:

     The class of events distributed when a frame receives the mouse focus.

   :seealso:

     - :gf:`event-destroy-frame?`
     - :gf:`frame-can-exit?`
     - :class:`<frame-exited-event>`

.. generic-function:: frame-fixed-height?

   Returns true if the height of the specified frame is not resizable.

   :signature: frame-fixed-width? *frame* => *fixed-height?*

   :param frame: An instance of type :class:`<frame>`.
   :value fixed-height?: An instance of type ``<boolean>``.

   :description:

     Returns true if the height of *frame* is not resizable.

   :seealso:

     - :gf:`frame-fixed-width?`
     - :gf:`frame-resizable?`

.. generic-function:: frame-fixed-width?

   Returns true if the width of the specified frame is not resizable.

   :signature: frame-fixed-width? *frame* => *fixed-width?*

   :param frame: An instance of type :class:`<frame>`.
   :value fixed-width?: An instance of type ``<boolean>``.

   :description:

     Returns true if the width of *frame* is not resizable.

   :seealso:

     - :gf:`frame-fixed-height?`
     - :gf:`frame-resizable?`

.. generic-function:: frame-icon

   Returns the icon associated with the specified frame.

   :signature: frame-icon *frame* => *icon*

   :param frame: An instance of type :class:`<frame>`.
   :value icon: An instance of type ``false-or(<image>)``.

   :description:

     Returns the icon associated with *frame*. This is the icon used to
     represent the frame when it has been iconized. In Windows 95 and Windows
     NT 4.0, this icon is also visible in the left hand corner of the title
     bar of the frame when it is not iconized.

   :seealso:

     - :gf:`deiconify-frame`
     - :gf:`frame-icon-setter`
     - :gf:`iconify-frame`

.. generic-function:: frame-icon-setter

   Specifies the icon associated with the specified frame.

   :signature: frame-icon-setter *icon* *frame* => *icon*

   :param icon: An instance of type ``false-or(<image>)``.
   :param frame: An instance of type :class:`<frame>`.
   :value icon: An instance of type ``false-or(<image>)``.

   :description:

     Specifies the icon associated with *frame*. This icon is used when the
     frame is iconified, and in Windows 95 and Windows NT 4.0 is also visible
     on the left hand side of the title bar of the frame.

   :seealso:

     - :gf:`frame-icon`

.. generic-function:: frame-input-focus

   Returns the sheet in the specified frame that has the input focus.

   :signature: frame-input-focus *frame* => *focus*

   :param frame: An instance of type :class:`<frame>`.
   :value focus: An instance of type ``false-or(<sheet>)``.

   :description:

     Returns the sheet in *frame* that has the input focus.

   :seealso:

     - :gf:`frame-input-focus-setter`

.. generic-function:: frame-input-focus-setter

   Sets which sheet in the specified frame has the input focus.

   :signature: frame-input-focus-setter *focus frame* => *focus*

   :param focus: An instance of type ``false-or(<sheet>)``.
   :param frame: An instance of type :class:`<frame>`.
   :value focus: An instance of type ``false-or(<sheet>)``.

   :description:

     Sets which sheet in *frame* has the input focus.

   :seealso:

     - :gf:`frame-input-focus`

.. generic-function:: frame-layout

   Returns the layout used in the specified frame.

   :signature: frame-layout *frame* => *layout*

   :param frame: An instance of type :class:`<frame>`.
   :value layout: An instance of type ``false-or(<sheet>)``.

   :description:

     Returns the layout used in *frame*.

   :seealso:

     - :gf:`frame-layout-setter`

.. generic-function:: frame-layout-setter

   Specifies the layout used in the specified frame.

   :signature: frame-layout-setter *layout* *frame* => *layout*

   :param layout: An instance of type ``false-or(<sheet>)``.
   :param frame: An instance of type :class:`<frame>`.
   :value layout: An instance of type ``false-or(<sheet>)``.

   :description:

     Specifies the layout used in *frame*.

   :seealso:

     - :gf:`frame-layout`

.. generic-function:: frame-mapped?

   Returns true if the specified frame is mapped.

   :signature: frame-mapped? *frame* => *mapped?*

   :param frame: An instance of type :class:`<frame>`.
   :value mapped?: An instance of type ``<boolean>``.

   :description:

     Returns true if *frame* is mapped, that is, is currently displayed
     on-screen. Note that a frame is considered to be mapped if it is
     anywhere on the screen, even if it is not completely visible because
     other windows are covering it either partially or completely, or if it
     is iconized.

   :example:

     The following example creates a simple frame, then displays it and exits
     it. In between starting and exiting the frame, *frame-mapped?* is
     called. You should run this code in the interactor, pressing the RETURN
     key at the points indicated.

     .. code-block:: dylan

        define variable *frame* =
          make(<simple-frame>, title: "A frame",
               layout: make(<button>)); // RETURN
        start-frame(*frame*); // RETURN
        frame-mapped?(*frame*); // RETURN
        => #t

        exit-frame(*frame*); // RETURN
        frame-mapped?(*frame*); // RETURN
        => #f

   :seealso:

     - :gf:`frame-mapped?-setter`

.. class:: <frame-mapped-event>
   :sealed:
   :instantiable:

   The class of events that indicate a frame has been mapped.

   :superclasses: :class:`<frame-event>`

   :description:

     The class of events that indicate a frame has been mapped, that is,
     displayed on screen. An instance of this class is distributed whenever a
     frame is mapped.

   :example:

     The following example defines a method that can inform you when an
     instance of a class of frame you have defined is mapped.

     .. code-block:: dylan

        define method handle-event
            (frame :: <my-frame>,
             event :: <frame-mapped-event>)
         => ()
          notify-user
            (format-to-string("Frame %= mapped", frame))
        end method handle-event;

   :seealso:

     - :class:`<frame-unmapped-event>`

.. generic-function:: frame-mapped?-setter

   Maps or unmaps the specified frame.

   :signature: frame-mapped?-setter *mapped?* *frame* => *mapped?*

   :param mapped?: An instance of type ``<boolean>``.
   :param frame: An instance of type :class:`<frame>`.
   :value mapped?: An instance of type ``<boolean>``.

   :description:

     Maps or unmaps *frame*, that is, displays frame on the screen or
     removes it from the screen, depending on whether *mapped?* is true or
     false. Note that a frame is considered to be mapped if it is anywhere on
     the screen, even if it is not completely visible because other windows
     are covering it either partially or completely, or if it is iconized.

   :example:

     The following example creates a simple frame, then displays it and
     unmaps it using *frame-mapped?-setter* rather than
     :gf:`start-frame` and :gf:`exit-frame`. You should run this code in the
     interactor, pressing the RETURN key at the points indicated.

     .. code-block:: dylan

        define variable *frame* =
          make(<simple-frame>, title: "A frame",
               layout: make(<button>));    // RETURN
        frame-mapped?-setter(#t, *frame*); // RETURN
        frame-mapped?-setter(#f, *frame*); // RETURN

   :seealso:

     - :gf:`exit-frame`
     - :gf:`frame-mapped?`
     - :gf:`start-frame`

.. generic-function:: frame-menu-bar

   Returns the menu bar used in the specified frame.

   :signature: frame-menu-bar *frame* => *menu-bar*

   :param frame: An instance of type :class:`<frame>`.
   :value menu-bar: An instance of type ``false-or(<menu-bar>)``.

   :description:

     Returns the menu bar used in *frame*.

   :seealso:

     - :gf:`frame-menu-bar-setter`

.. generic-function:: frame-menu-bar-setter

   Sets the menu bar used in the specified frame.

   :signature: frame-menu-bar-setter *menu-bar* *frame* => *menu-bar*

   :value menu-bar: An instance of type ``false-or(<menu-bar>)``.
   :param frame: An instance of type :class:`<frame>`.
   :value menu-bar: An instance of type ``false-or(<menu-bar>)``.

   :description:

     Sets the menu bar used in *frame*.

   :seealso:

     - :gf:`frame-menu-bar`

.. generic-function:: frame-mode

   Returns the mode of the specified frame.

   :signature: frame-mode *frame* => *mode*

   :param frame: An instance of type :class:`<frame>`.
   :value mode: An instance of type ``one-of(#"modeless", #"modal", #"system-modal")``.

   :description:

     Returns the mode of *frame*. This is the same value as was specified
     for the ``mode:`` init-keyword when the frame was created.

     If *frame* is modal, such as a dialog, then it must be dismissed before
     the user can interact with the user interface of an application (for
     instance, before a menu can be displayed).

     If *frame* is modeless, then the user can interact with its parent frame
     while the frame is still visible. Typically, the user will move the
     frame to a convenient position on the screen and continue work, keeping
     the frame on screen for as long as is desired. For example it is often
     useful to make the Find dialog box in an application modeless, so that
     the user can keep it on screen while performing other tasks.

     If *frame* is system-modal, then it prevents the user from interacting
     with *any* other running applications, such as the Shutdown dialog in
     Windows 95. System modal frames are rarely used, and should be used with
     caution.

     .. note:: You can only set the mode of a frame when it is first created.
        The mode cannot subsequently be changed.

   :seealso:

     - :class:`<frame>`

.. generic-function:: frame-owner

   Returns the controlling frame for the specified frame.

   :signature: frame-owner *frame* => *owner*

   :param frame: An instance of type :class:`<frame>`.
   :value owner: An instance of type ``false-or(<frame>)``.

   :description:

     Returns the controlling frame for *frame*. The controlling frame for
     any hierarchy of existing frames is the one that owns the thread in
     which the frames are running. Thus, the controlling frame for *frame* is
     not necessarily its direct owner: it may be the owner of *frame* ’s
     owner, and so on, depending on the depth of the hierarchy.

.. generic-function:: frame-palette

   Returns the palette used in the specified frame.

   :signature: frame-palette *frame* => *palette*

   :param frame: An instance of type :class:`<frame>`.
   :value palette: An instance of type :class:`<palette>`.

   :description:

     Returns the palette used in *frame*.

   :seealso:

     - :gf:`frame-palette-setter`

.. generic-function:: frame-palette-setter

   Sets the palette used in the specified frame.

   :signature: frame-palette-setter *palette* *frame* => *palette*

   :param palette: An instance of type :class:`<palette>`.
   :param frame: An instance of type :class:`<frame>`.
   :value palette: An instance of type :class:`<palette>`.

   :description:

     Sets the palette used in *frame*.

   :seealso:

     - :gf:`frame-palette`

.. generic-function:: frame-position

   Returns the position on the screen of the specified frame.

   :signature: frame-position *frame* => *x* *y*

   :param frame: An instance of type :class:`<frame>`.
   :value x: An instance of type ``<integer>``.
   :value y: An instance of type ``<integer>``.

   :description:

     Returns the position on the screen of *frame*. Coordinates are
     expressed relative to the top left corner of the screen, measured in
     pixels.

   :example:

     The following example creates a simple frame, then displays it and tests
     its position. You should run this code in the interactor, pressing the
     RETURN key at the points indicated.

     .. code-block:: dylan

        define variable *frame* =
          make(<simple-frame>, title: "A frame",
               layout: make(<button>)); // RETURN
        start-frame(*frame*); // RETURN
        frame-position(*frame*); // RETURN

   :seealso:

     - :gf:`frame-size`
     - :gf:`frame-state`
     - :gf:`set-frame-position`

.. generic-function:: frame-resizable?

   Returns true if the specified frame is resizable.

   :signature: frame-resizable? *frame* => *resizable?*

   :param frame: An instance of type :class:`<frame>`.
   :value resizable?: An instance of type ``<boolean>``.

   :description:

     Returns true if *frame* is resizable, that is can have one or both of
     its width and height modified by the user.

   :seealso:

     - :gf:`frame-fixed-height?`
     - :gf:`frame-fixed-width?`

.. generic-function:: frame-size

   Returns the size of the specified frame.

   :signature: frame-size *frame* => *width* *height*

   :param frame: An instance of type :class:`<frame>`.
   :value width: An instance of type ``<integer>``.
   :value height: An instance of type ``<integer>``.

   :description:

     Returns the size of *frame*, measured in pixels.

   :example:

     The following example creates a simple frame, then displays it and tests
     its size. You should run this code in the interactor, pressing the
     RETURN key at the points indicated.

     .. code-block:: dylan

        define variable *frame* =
          make(<simple-frame>, title: "A frame",
               layout: make(<button>)); // RETURN
        start-frame(*frame*); // RETURN
        frame-size(*frame*); // RETURN

   :seealso:

     - :gf:`frame-position`
     - :gf:`frame-state`
     - :gf:`set-frame-size`

.. generic-function:: frame-state

   Returns the visible state of the specified frame.

   :signature: frame-state *frame* => *state*

   :param frame: An instance of type :class:`<frame>`.
   :value state: An instance of type ``one-of(#"detached", #"unmapped",
     #"mapped", #"iconified", #"destroyed")``.

   :description:

     Returns the visible state of the specified frame. The return value from
     this function indicates whether frame is currently iconified, whether it
     is mapped or unmapped, whether it has been destroyed, or whether it has
     become detached from the thread of which it was a part.

   :example:

     The following example creates a simple frame, then displays it and tests
     its position. You should run this code in the interactor, pressing the
     RETURN key at the points indicated.

     .. code-block:: dylan

        define variable *frame* =
          make(<simple-frame>, title: "A frame",
               layout: make(<button>)); // RETURN
        start-frame(*frame*); // RETURN
        frame-state(*frame*); // RETURN
        => #"mapped"

   :seealso:

     - :gf:`frame-position`
     - :gf:`frame-size`

.. generic-function:: frame-status-bar

   Returns the status bar used in the specified frame.

   :signature: frame-status-bar *frame* => *status-bar*

   :param frame: An instance of type :class:`<frame>`.
   :value status-bar: An instance of type ``false-or(<status-bar>)``.

   :description:

     Returns the status bar used in *frame*.

   :seealso:

     - :gf:`frame-status-bar-setter`

.. generic-function:: frame-status-bar-setter

   Sets the status bar used in the specified frame.

   :signature: frame-status-bar-setter *status-bar* *frame* => *status-bar*

   :param status-bar: An instance of type :class:`<status-bar>`.
   :param frame: An instance of type :class:`<frame>`.
   :value status-bar: An instance of type ``false-or(<status-bar>)``.

   :description:

     Sets the status bar used in *frame*.

   :seealso:

     - :gf:`frame-status-bar`

.. generic-function:: frame-status-message
   :open:

   Returns the status message for the specified frame.

   :signature: frame-status-message *frame* => *status-message*

   :param frame: An instance of type :class:`<frame>`.
   :value status-message: An instance of type ``false-or(<string>)``.

   :description:

     Returns the status message for *frame*. This is the label in the status
     bar for the frame. If the frame has no status bar, or if the label is
     not set, this this function returns false.

   :seealso:

     - :gf:`frame-status-bar`
     - :gf:`frame-status-message-setter`
     - :class:`<status-bar>`

.. generic-function:: frame-status-message-setter

   Sets the status message for the specified frame.

   :signature: frame-status-message *status-message* *frame* => *status-message*

   :param status-message: An instance of type ``false-or(<string>)``.
   :param frame: An instance of type :class:`<frame>`.
   :value status-message: An instance of type ``false-or(<string>)``.

   :description:

     Sets the status message for *frame*. This is the label in the status
     bar for the frame. If the frame has no status bar, then attempting to
     set the label fails silently.

   :seealso:

     - :gf:`frame-status-bar-setter`
     - :gf:`frame-status-message`
     - :class:`<status-bar>`

.. generic-function:: frame-thread

   Returns the thread with which the specified frame is associated.

   :signature: frame-thread *frame* => *thread*

   :param frame: An instance of type :class:`<frame>`.
   :value thread: An instance of type :class:`<thread>`.

   :description:

     Returns the thread with which *frame* is associated.

     For more information about threads, refer to the manual *Library
     Reference: Core Features*.

.. generic-function:: frame-title

   Returns the title of the specified frame.

   :signature: frame-title *frame* => *title*

   :param frame: An instance of type :class:`<frame>`.
   :value title: An instance of type ``false-or(<string>)``.

   :description:

     Returns the title of *frame*. If this is ``#f``, then the title bar is
     removed from the frame, if this is possible. If this is not possible,
     then a default message is displayed. Whether the title bar can be
     removed from the frame or not is platform dependent.

   :seealso:

     - :gf:`frame-title-setter`

.. generic-function:: frame-title-setter

   Sets the title of the specified frame.

   :signature: frame-title-setter *title* *frame* => *title*

   :param title: An instance of type ``false-or(<string>)``.
   :param frame: An instance of type :class:`<frame>`.
   :value title: An instance of type ``false-or(<string>)``.

   :description:

     Sets the title of *frame*. The title of a frame is displayed in the
     title bar of the frame. If *title* is ``#f``, then the platform attempts
     to remove the title bar from the frame, if possible.

   :seealso:

     - :gf:`frame-title`

.. generic-function:: frame-tool-bar

   Returns the tool bar used in the specified frame.

   :signature: frame-tool-bar *frame* => *tool-bar*

   :param frame: An instance of type :class:`<frame>`.
   :value tool-bar: An instance of type ``false-or(<tool-bar>)``.

   :description:

     Returns the tool bar used in *frame*.

   :seealso:

     - :gf:`frame-tool-bar-setter`

.. generic-function:: frame-tool-bar-setter

   Sets the tool bar used in the specified frame.

   :signature: frame-tool-bar-setter *tool-bar* *frame* => *tool-bar*

   :param tool-bar: An instance of type ``false-or(<tool-bar>)``.
   :param frame: An instance of type :class:`<frame>`.
   :value tool-bar: An instance of type ``false-or(<tool-bar>)``.

   :description:

     Sets the tool bar used in *frame*.

   :seealso:

     - :gf:`frame-tool-bar`

.. generic-function:: frame-top-level

   Returns the top level loop function for the specified frame.

   :signature: frame-top-level *frame* => *top-level*

   :param frame: An instance of type :class:`<frame>`.
   :value top-level: An instance of type ``<function>``.

   :description:

     Returns the top level loop function for *frame*. The top level loop
     function for a frame is the "command loop" for the frame.

     The default method for :gf:`frame-top-level` calls :gf:`read-event`
     and then :gf:`handle-event`.

   :seealso:

     - :gf:`handle-event`

.. class:: <frame-unmapped-event>
   :sealed:
   :instantiable:

   The class of events that indicate a frame has been unmapped.

   :superclasses: :class:`<frame-event>`

   :description:

     The class of events that indicate a frame has been unmapped, that is,
     removed from the screen. An instance of this class is distributed
     whenever a frame is unmapped. A frame may be unmapped by either
     iconifying it, or by exiting or destroying the frame completely, so that
     it no longer exists.

   :example:

     The following example defines a method that can inform you when an
     instance of a class of frame you have defined is unmapped.

     .. code-block:: dylan

        define method handle-event
            (frame :: <my-frame>,
             event :: <frame-unmapped-event>)
         => ()
          notify-user
            (format-to-string("Frame %= unmapped", frame))
        end method handle-event;

   :seealso:

     - :class:`<frame-mapped-event>`

.. variable:: *global-command-table*

   The command table inherited by all new command tables.

   :type: :class:`<command-table>`

   :description:

     This is the command table from which all other command tables inherit by
     default. You should not explicitly add anything to or remove anything
     from this command table. DUIM can use this command to store internals or
     system-wide commands. You should not casually install any commands or
     translators into this command table.

   :seealso:

     - :class:`<command-table>`
     - :var:`*user-command-table*`

.. generic-function:: iconify-frame

   Iconifies the specified frame.

   :signature: iconify-frame *frame* => ()

   :param frame: An instance of type :class:`<frame>`.

   :description:

     Iconifies *frame*. The appearance of the iconified frame depends on the
     behavior of the operating system in which the application is running.
     For instance, in Windows 95 or Windows NT 4.0, the icon is displayed in
     the task bar at the bottom of the screen.

   :example:

     The following example creates and displays a simple frame, then
     iconifies it. You should run this code in the interactor, pressing the
     RETURN key at the points indicated.

     .. code-block:: dylan

        define variable *frame* =
          make(<simple-frame>, title: "A frame",
               layout: make(<button>)); // RETURN
        start-frame(*frame*); // RETURN
        iconify-frame(*frame*); // RETURN

   :seealso:

     - :gf:`deiconify-frame`
     - :gf:`destroy-frame`
     - :gf:`exit-frame`
     - :gf:`frame-icon`
     - :gf:`lower-frame`
     - :gf:`raise-frame`

.. generic-function:: layout-frame

   Resizes the specified frame and lays out the current pane hierarchy
   inside it.

   :signature: layout-frame *frame* #key *width height* => ()

   :param frame: An instance of type :class:`<frame>`.
   :param width: An instance of type ``false-or(<integer>)``.
   :param height: An instance of type ``false-or(<integer>)``.

   :description:

     Resizes the frame and lays out the current pane hierarchy according to
     the layout protocol. This function is automatically invoked on a frame
     when it is adopted, after its pane hierarchy has been generated.

     If *width* and *height* are provided, then this function resizes the
     frame to the specified size. It is an error to provide just *width*.

     If no optional arguments are provided, this function resizes the frame
     to the preferred size of the top-level pane as determined by the space
     composition pass of the layout protocol.

     In either case, after the frame is resized, the space allocation pass of
     the layout protocol is invoked on the top-level pane.

.. generic-function:: lower-frame

   Lowers the specified frame to the bottom of the stack of visible
   windows.

   :signature: lower-frame *frame* => ()

   :param frame: An instance of type :class:`<frame>`.

   :description:

     Lowers *frame* to the bottom of the stack of visible windows. After
     calling this function, *frame* will appear beneath any occluding windows
     that may be on the screen.

   :example:

     The following example creates and displays a simple frame, then lowers
     it.

     .. code-block:: dylan

        define variable *frame* =
          make(<simple-frame>, title: "A frame",
               layout: make(<button>));
        start-frame(*frame*);
        lower-frame(*frame*);

   :seealso:

     - :gf:`deiconify-frame`
     - :gf:`destroy-frame`
     - :gf:`exit-frame`
     - :gf:`iconify-frame`
     - :gf:`raise-frame`

.. method:: make
   :specializer: <frame>

   Creates an instance of a :class:`<frame>`.

   :signature: make (class == <frame>)
     #key *top-level command-queue layout icon
     pointer-documentation command-table menu-bar tool-bar
     status-bar title calling-frame top-level-sheet state
     geometry resizable? properties thread event-queue
     foreground background text-style palette save-under?
     drop-shadow? dialog-for*
     => *simple-frame*

   :param class: The class :class:`<frame>`.
   :param #key top-level: An instance of type ``false-or(<sheet>)``. Default value: ``#f``.
   :param #key command-queue: An instance of type ``false-or(<event-queue>)``.
     Default value: ``#f``.
   :param #key layout: An instance of type ``false-or(<sheet>)``. Default value: ``#f``.
   :param #key icon: An instance of type ``false-or(<image>)``. Default value: ``#f``.
   :param #key pointer-documentation: An instance of type ``false-or(<string>)``. Default value: ``#f``.
   :param #key command-table: An instance of type ``false-or(<command-table>)``. Default value: ``#f``.
   :param #key menu-bar: An instance of type ``false-or(<menu-bar>)``. Default value: ``#f``.
   :param #key tool-bar: An instance of type ``false-or(<tool-bar>)``. Default value: ``#f``.
   :param #key status-bar: An instance of type ``false-or(<status-bar>)``. Default value: ``#f``.
   :param #key title: An instance of type ``false-or(<string>)``. Default value: ``#f``.
   :param #key calling-frame: An instance of type ``false-or(<frame>)``. Default value: ``#f``.
   :param #key state: An instance of type ``one-of(#"detached", #"unmapped",
     #"mapped", #"iconified")``. Default value: ``#"detached"``.
   :param #key geometry: An instance of type :drm:`<vector>`. Default value: ``vector(#f, #f, #f, #f)``.
   :param #key resizable?: An instance of type ``<boolean>``. Default value: ``#t``.
   :param #key properties: An instance of type :drm:`<stretchy-object-vector>`. Default value: ``make(<stretchy-vector>)``.
   :param #key thread: An instance of type ``false-or(<thread>)``. Default value: ``#f``.
   :param #key event-queue: An instance of type ``false-or(<event-queue>)``. Default value: ``#f``.
   :param #key foreground: An instance of type ``false-or(<ink>)``. Default value: ``#f``.
   :param #key background: An instance of type ``false-or(<ink>)``. Default value: ``#f``.
   :param #key text-style: An instance of type ``false-or(<text-style>)``. Default value: ``#f``.
   :param #key palette: An instance of type ``false-or(<palette>)``. Default value: ``#f``.
   :param #key save-under?: An instance of type ``<boolean>``. Default value: ``#f``.
   :param #key drop-shadow?: An instance of type ``<boolean>``. Default value: ``#f``.
   :param #key dialog-for: An instance of type :class:`<dialog-frame>`.
   :value simple-frame: An instance of type :class:`<frame>`.

   :description:

     Creates and returns an instance of :class:`<frame>` or
     one of its subclasses.

     The *top-level* argument specifies the top-level command-loop in which
     the frame runs.

     The *command-queue* argument specifies a command-queue for the frame.

     The *layout* argument specifies a layout for the frame.

     The *icon* argument specifies an icon that will be used when the frame
     is iconized. In all current versions of Windows, this icon is also
     visible in the left hand corner of the title bar of the frame when it is
     not iconized.

     The *pointer-documentation* argument specifies pointer-documentation for
     the frame.

     The *command-table* argument specifies a command table for the frame.

     The *menu-bar* argument specifies a menu bar for the frame.

     The *tool-bar* argument specifies a tool bar for the frame.

     The *status-bar* argument specifies a status bar for the frame.

     The *title* argument specifies a title for the frame.

     The *calling-frame* argument specifies a calling frame for the frame.

     The *state* argument specifies a frame-state. The frame can be mapped or
     unmapped (that is, visible on the screen, or not), iconified, or
     detached.

     The *geometry* argument specifies a for the frame. The four components
     of this keyword represent the x and y position of the frame, and the
     width and height of the frame, respectively.

     The *resizable?* argument specifies whether or not the frame is
     resizable.

     The *properties* argument specifies properties for the frame.

     The *thread* argument specifies the thread in which the frame will run.
     See the *Library Reference: Core Features* manual for full details about
     how threads are handled.

     The *event-queue* specifies an event-queue for the frame.

     The arguments *foreground* and *background* specify a foreground color
     for the frame. In addition, *text-style* specifies a text style for the
     frame, and *palette* specifies a color palette for the frame.

   :seealso:

     - :class:`<frame>`

.. generic-function:: make-menu-from-command-table-menu

   Returns a menu from the menu definition in the specified command table.

   :signature: make-menu-from-command-table-menu
     *command-table-menu-items* *frame* *framem*
     #key *command-table* *label* *mnemonic* *item-callback*
     => *menu*

   :param command-table-menu-items: An instance of type :drm:`<sequence>`.
   :param frame: An instance of type :class:`<frame>`.
   :param framem: An instance of type :class:`<frame-manager>`.
   :param #key command-table: An instance of type :class:`<command-table>`.
   :param #key label: An instance of type :class:`<label>`.
   :param #key mnemonic: An instance of type ``false-or(<gesture>)``.
   :param #key item-callback: An instance of type ``<function>``.
   :value menu: An instance of type :class:`<menu>`.

   :description:

     Returns a menu from the menu definition in the specified command table.
     This function is used by
     :gf:`make-menus-from-command-table` to individually
     create each menu defined in the command table. The function
     :gf:`make-menus-from-command-table` then puts each of the
     menus created together in the appropriate way.

     The *command-table-menu-items* argument defines the items that are to be
     placed in the menu. It is a sequence of instances of
     :class:`<command-table-menu-item>`.

     The *frame* and *framem* arguments define the frame and the frame
     manager in which the menu created is to be placed.

     The *command-table* argument specifies the command table in which the
     definition of the menu created can be found.

     The *label* argument defines a label for the menu created.

     The *mnemonic* argument defines a keyboard mnemonic for the menu
     created.

   :seealso:

     - :gf:`make-menus-from-command-table`

.. generic-function:: make-menus-from-command-table

   Returns a set of menus from the menu definitions in the specified
   command table.

   :signature: make-menus-from-command-table *command-table* *frame* *framem* #key *label* => *menus*

   :param command-table: An instance of type :class:`<command-table>`.
   :param frame: An instance of type :class:`<frame>`.
   :param framem: An instance of type :class:`<frame-manager>`.
   :param #key label: An instance of type :class:`<label>`.
   :value menus: An instance of type ``limited(<sequence>, of: <menu>)``.

   :description:

     Returns a set of menus from the menu definitions in *command-table*.

     The *frame* and *framem* arguments specify the frame and frame manager
     in which the menus are to be placed.

     The *label* argument lets you specify a label for the set of menus.

   :seealso:

     :gf:`make-menu-from-command-table`

.. generic-function:: menu-item-accelerator

   Returns the accelerator for the specified command table menu item.

   :signature: menu-item-accelerator *menu-item* => *accelerator*

   :param menu-item: An instance of type :class:`<command-table-menu-item>`.
   :value accelerator: An instance of type :class:`<gesture>`.

   :description:

     Returns the keyboard accelerator for *menu-item*. Note that *menu-item*
     must be defined in a command table.

   :seealso:

     - :gf:`menu-item-mnemonic`

.. generic-function:: menu-item-mnemonic

   Returns the mnemonic for the specified menu item.

   :signature: menu-item-mnemonic *menu-item* => *mnemonic*

   :param menu-item: An instance of type :class:`<command-table-menu-item>`.
   :value mnemonic: An instance of type ``false-or(<gesture>)``.

   :description:

     Returns the keyboard mnemonic for *menu-item*.

   :seealso:

     - :gf:`menu-item-accelerator`

.. generic-function:: menu-item-name

   Returns the name of the specified menu item.

   :signature: menu-item-name *menu-item* => *name*

   :param menu-item: An instance of type :class:`<command-table-menu-item>`.
   :value name: An instance of type :drm:`<string>`.

   :description:

     Returns the name of *menu-item*.

   :seealso:

     - :gf:`menu-item-options`
     - :gf:`menu-item-type`
     - :gf:`menu-item-value`

.. generic-function:: menu-item-options

   Returns the options for the specified menu item.

   :signature: menu-item-options *menu-item* => *options*

   :param menu-item: An instance of type :class:`<command-table-menu-item>`.
   :value options: An instance of type :drm:`<object>`.

   :description:

     Returns the options for *menu-item*.

   :seealso:

     - :gf:`menu-item-name`
     - :gf:`menu-item-type`
     - :gf:`menu-item-value`

.. generic-function:: menu-item-type

   Returns the type of the specified menu item.

   :signature: menu-item-type *menu-item* => *type*

   :param menu-item: An instance of type :class:`<command-table-menu-item>`.
   :value type: An instance of type :drm:`<object>`.

   :description:

     Returns the type of *menu-item*.

   :seealso:

     - :gf:`menu-item-name`
     - :gf:`menu-item-options`
     - :gf:`menu-item-value`

.. generic-function:: menu-item-value

   Returns the value of the specified menu item.

   :signature: menu-item-value *menu-item* => *value*

   :param menu-item: An instance of type :class:`<command-table-menu-item>`.
   :value value: An instance of type :drm:`<object>`.

   :description:

     Returns the value of *menu-item*.

   :seealso:

     - :gf:`menu-item-name`
     - :gf:`menu-item-options`
     - :gf:`menu-item-type`

.. generic-function:: move-to-next-page

   Moves to the next page of the specified multi-page dialog.

   :signature: move-to-next-page *wizard* => ()

   :param wizard: An instance of type :class:`<wizard-frame>`.

   :description:

     Moves to the next page in sequence of *wizard*. This is the default
     callback for the Next button in a wizard frame.

   :seealso:

     - :gf:`dialog-next-callback`
     - :class:`<wizard-frame>`

.. generic-function:: move-to-previous-page

   Moves to the previous page of the specified multi-page dialog.

   :signature: move-to-previous-page *wizard* => ()

   :param wizard: An instance of type :class:`<wizard-frame>`.

   :description:

     Moves to the previous page in sequence of *wizard*. This is the default
     callback for the Back button in a wizard frame.

   :seealso:

     - :gf:`dialog-back-callback`
     - :class:`<wizard-frame>`

.. generic-function:: note-progress

   Note the progress of an event in the specified progress note.

   :signature: note-progress *numerator* *denominator* #key *note* *label* *pointer-cursor* => ()

   :param numerator: An instance of type ``<integer>``.
   :param denominator: An instance of type ``<integer>``.
   :param #key note: An instance of type :class:`<progress-note>`.
     Default value: :var:`*progress-note*`.
   :param #key label: An instance of type :class:`<label>`.
   :param #key pointer-cursor: An instance of type :class:`<pointer>`.

   :description:

     Note the progress of an event in *note*.

     If a *numerator* and *denominator* are supplied, then the progress is
     displayed in terms of those figures. For example, if *numerator* is 1,
     and *denominator* is 10, then the progress is displayed in tenths.

     If supplied, *pointer-cursor* is used as a cursor when the mouse pointer
     is placed over the owner frame.

   :seealso:

     - :macro:`noting-progress`
     - :var:`*progress-note*`

.. macro:: noting-progress
   :statement:

   Performs a body of code, noting its progress.

   :macrocall:

     .. code-block:: dylan

        noting-progress ({*sheet* }, {*label* }) {*body* } end

   :param sheet: A Dylan expression *bnf*.
   :param label: A Dylan expression *bnf*.
   :param body: A Dylan body *bnf*.

   :description:

     Performs a body of code, noting its progress, for the specified sheet.

     The sheet argument is an expression that evaluates to an instance of
     :class:`<sheet>`. The label argument is an expression
     that evaluates to an instance of :drm:`<string>`.

   :seealso:

     - :gf:`note-progress`

.. variable:: *progress-note*
   :thread:

   Specifies a default progress note that can be used.

   :type: :drm:`<object>`

   :value: ``#f``

   :description:

     This variable is used to supply a default progress note to use if no
     progress note is explicitly specified.

   :seealso:

     - :gf:`note-progress`

.. class:: <property-frame>
   :open:
   :instantiable:

   The class of property frames.

   :superclasses: :class:`<dialog-frame>`

   :keyword pages: An instance of type ``false-or(limited(<sequence>, of: <page>))``.
     Default value: ``#f``.
   :keyword apply-callback: An instance of type ``false-or(<function>)``. Default value: ``#f``.
   :keyword apply-button: An instance of type ``false-or(<button>)``.
     Default value: ``#f``.

   Note: The following two useful init-keywords are inherited from
   :class:`<dialog-frame>`:

   :keyword pages: An instance of type ``false-or(<sequence>)``.
     Default value: ``#f``.
   :keyword page-changed-callback: An instance of type ``false-or(<function>)``.
     Default value: ``#f``.

   :description:

     The class of property frames. These are dialogs that can contain
     property sheets of some description. This is the class of dialogs with
     several pages, each presented as a label in a tab control.

     .. figure:: images/frames-5.png
        :align: center

        A property frame

     The ``pages:`` init-keyword defines the pages available for the property
     frame.

     The apply callback and button define an additional Apply button
     available in property frames. The Apply button applies any changes made
     in the current page of the dialog, but does not dismiss the dialog from
     the screen. By default, there is no Apply button defined.

     The page-changed callback lets you specified a callback that should be
     invoked if the current page in the property frame is changed by clicking
     on a different page tab.

   :operations:

     - :gf:`dialog-apply-button`
     - :gf:`dialog-apply-button-setter`
     - :gf:`dialog-apply-callback`
     - :gf:`dialog-current-page`
     - :gf:`dialog-current-page-setter`
     - :gf:`dialog-page-changed-callback`
     - :gf:`dialog-page-changed-callback-setter`
     - :gf:`dialog-page-complete?`
     - :gf:`dialog-page-complete?-setter`
     - :gf:`dialog-pages`
     - :gf:`dialog-pages-setter`

   :seealso:

     - :gf:`dialog-apply-button`
     - :gf:`dialog-apply-callback`
     - :class:`<dialog-frame>`
     - :class:`<property-page>`
     - :class:`<wizard-frame>`

.. class:: <property-page>
   :open:
   :instantiable:

   The class of property pages.

   :superclasses: :class:`<page>`

   :description:

     The class of property pages. These are pages that can be displayed in an
     instance of :class:`<property-frame>`.

     .. figure:: images/frames-6.png
        :align: center

        A property page

     Internally, this class maps into the Windows property page control.

   :seealso:

     - :class:`<page>`
     - :class:`<property-frame>`
     - :class:`<property-page>`
     - :class:`<tab-control-page>`
     - :class:`<wizard-page>`

.. generic-function:: raise-frame

   Raises the specified frame to the top of the stack of visible windows.

   :signature: raise-frame *frame* => ()

   :param frame: An instance of type :class:`<frame>`.

   :description:

     Raises *frame* to the top of the stack of visible windows. After calling
     this function, *frame* will appear above any occluding windows that may
     be on the screen.

   :example:

     The following example creates and displays a simple frame, then lowers
     and raises it. You should run this code in the interactor, pressing the
     RETURN key at the points indicated.

     .. code-block:: dylan

        define variable *frame* =
          make(<simple-frame>, title: "A frame",
               layout: make(<button>)); // RETURN
        start-frame(*frame*); // RETURN
        lower-frame(*frame*); // RETURN
        raise-frame(*frame*); // RETURN

   :seealso:

     - :gf:`deiconify-frame`
     - :gf:`destroy-frame`
     - :gf:`exit-frame`
     - :gf:`iconify-frame`
     - :gf:`lower-frame`

.. generic-function:: redo-command

   Performs the last performed command again.

   :signature: redo-command *command* *frame* => #rest *values*

   :param command: An instance of type :class:`<command>`.
   :param frame: An instance of type :class:`<frame>`.
   :param values: Instances of type :drm:`<object>`.

   :description:

     Performs *command* again. The command is the command that was last
     executed using :gf:`execute-command`.

     Note that the command described by *command* must be undoable.

     You can both specialize this function and call it directly in your code.

   :seealso:

     - :gf:`execute-command`

.. generic-function:: remove-command

   Removes a command from the specified command table.

   :signature: remove-command *command-table* *command* => ()

   :param command-table: An instance of type :class:`<command-table>`.
   :param command: An instance of type :class:`<command>`.


   :description:

     Removes *command* from *command-table*.

   :seealso:

     - :gf:`add-command`

.. function:: remove-command-table

   Removes the specified command table.

   :signature: remove-command-table *command-table* => ()

   :param command-table: An instance of type :class:`<command-table>`.

   :description:

     Removes *command-table*.

.. generic-function:: remove-command-table-menu-item

   Removes a menu item from the specified command table.

   :signature: remove-command-table-menu-item *command-table* *string* => ()

   :param command-table: An instance of type :class:`<command-table>`.
   :param string: An instance of type :drm:`<string>`.


   :description:

     Removes the menu item identified by *string* from *command-table*.

   :seealso:

     - :gf:`add-command-table-menu-item`

.. generic-function:: set-frame-position

   Sets the position of the specified frame.

   :signature: set-frame-position *frame* *x* *y* => ()

   :param frame: An instance of type :class:`<frame>`.
   :param x: An instance of type ``<integer>``.
   :param y: An instance of type ``<integer>``.

   :description:

     Sets the position of *frame*. The coordinates *x* and *y* are measured
     from the top left of the screen, measured in pixels.

   :seealso:

     - :gf:`frame-position`
     - :gf:`set-frame-size`

.. generic-function:: set-frame-size

   Sets the size of the specified frame.

   :signature: set-frame-size *frame* *width* *height* => ()

   :param frame: An instance of type :class:`<frame>`.
   :param width: An instance of type ``<integer>``.
   :param height: An instance of type ``<integer>``.

   :description:

     Sets the size of *frame*.

   :example:

     The following example creates and displays a simple frame, then resizes
     it. You should run this code in the interactor, pressing the RETURN key
     at the points indicated.

     .. code-block::dylan

        define variable *frame* =
          make(<simple-frame>, title: "A frame",
               layout: make(<button>, label: "Button")); // RETURN
        set-frame-size(*frame*, 100, 500); // RETURN

   :seealso:

     - :gf:`frame-size`
     - :gf:`set-frame-position`

.. class:: <simple-command>
   :open:
   :abstract:
   :instantiable:

   The class of simple commands.

   :superclasses: :drm:`<object>`

   :keyword function: An instance of type ``<function>``. Required.
   :keyword arguments: An instance of type :drm:`<sequence>`. Default value ``#[]``.

   :description:

     The class of simple commands. A simple command has an associated
     function and some arguments. Simple commands are not undoable.

     The first argument to the function is always the frame.

   :seealso:

     - :class:`<command>`

.. class:: <simple-frame>
   :open:
   :abstract:
   :instantiable:

   The class of simple frames.

   :superclasses: :class:`<frame>`

   :keyword command-queue: An instance of type ``false-or(<event-queue)``.
     Default value: ``make(<event-queue>)``.
   :keyword layout: An instance of type ``false-or(<sheet>).
     Default value: ``#f``.
   :keyword command-table: An instance of type ``false-or(<command-table>)``.
     Default value: ``#f``.
   :keyword menu-bar: An instance of type ``false-or(<menu-bar>)``.
     Default value: ``#f``.
   :keyword tool-bar: An instance of type ``false-or(<tool-bar>)``.
     Default value: ``#f``.
   :keyword status-bar: An instance of type ``false-or(<status-bar>)``.
     Default value: ``#f``.

   :description:

     The class of simple frames.

     The ``command-queue:`` init-keyword specifies a command-queue for the
     frame.

     The ``layout:`` init-keyword specifies a layout for the frame.

     The ``command-table:`` init-keyword specifies a command table for the
     frame.

     The ``menu-bar:`` init-keyword specifies a menu bar for the frame.

     The ``tool-bar:`` init-keyword specifies a tool bar for the frame.

     The ``status-bar:`` init-keyword specifies a status bar for the frame.

   :operations:

     - :gf:`frame-command-table`
     - :gf:`frame-command-table-setter`
     - :gf:`frame-layout`
     - :gf:`frame-layout-setter`
     - :gf:`frame-menu-bar`
     - :gf:`frame-menu-bar-setter`
     - :gf:`frame-status-bar`
     - :gf:`frame-status-bar-setter`
     - :gf:`frame-status-message`
     - :gf:`frame-status-message-setter`
     - :gf:`frame-tool-bar`
     - :gf:`frame-tool-bar-setter`
     - :gf:`frame-top-level`
     - :gf:`start-frame`

.. class:: <simple-undoable-command>
   :open:
   :abstract:
   :instantiable:

   The class of simple commands that can contain an undo action.

   :superclasses: :drm:`<object>`

   :keyword undo-command: An instance of type :class:`<command>`.

   :description:

     The class of simple commands that can contain an undo action. A simple
     undoable command is like a simple command, except that it points to a
     command that can undo it, represented by the ``undo-command:``
     init-keyword.

   :seealso:

     - :class:`<simple-command>`

.. generic-function:: start-dialog

   Displays a DUIM frame as a dialog box.

   :signature: start-dialog *dialog* => #rest *values*

   :param dialog: An instance of type :class:`<dialog-frame>`.
   :value #rest values: Instances of type :drm:`<object>`.

   :description:

     Displays a DUIM frame as a dialog box.

     The function :gf:`start-dialog` dynamically binds an :drm:`<abort>` restart
     around the event loop for the dialog that is started. The restart allows
     the event loop to be re-entered, and enables any callbacks run from the
     dialog to signal an :drm:`<abort>` (via the :drm:`abort` function, for instance),
     in order to terminate execution of the current callback and return to
     event processing. This facility is useful for implementing operations
     that cancel gestures and for debugging DUIM applications from Dylan
     debuggers.

   :seealso:

     - :gf:`cancel-dialog`
     - :class:`<dialog-frame>`
     - :gf:`exit-dialog`
     - :gf:`start-frame`

.. generic-function:: start-frame

   Starts the specified frame.

   :signature: start-frame *frame* #key *owner* *mode* => *status-code*

   :param frame: An instance of type :class:`<frame>`.
   :param owner: An instance of type ``false-or(<frame>)``. Default value: ``#f``.
   :param mode: An instance of type ``one-of("modal", #"modeless", #"system-modal")``.
     Default value: ``#f``.
   :value status-code: An instance of type ``<integer>``.

   :description:

     Starts *frame*, optionally setting the *owner* of the frame and the
     *mode* in which it will run.

     The function :gf:`start-frame` dynamically binds an :drm:`<abort>` restart around
     the event loop for the frame that is started. The restart allows the
     event loop to be re-entered, and enables any callbacks run from the
     frame to signal an :drm:`<abort>` (via the :drm:`abort` function, for instance),
     in order to terminate execution of the current callback and return to
     event processing. This facility is useful for implementing operations
     that cancel gestures and for debugging DUIM applications from Dylan
     debuggers.  

   :example:

     The following example creates a simple frame, then displays it. You
     should run this code in the interactor, pressing the **RETURN** key at the
     points indicated.

     .. code-block:: dylan

        define variable *frame* =
          make(<simple-frame>, title: "A frame",
               layout: make(<button>));  // RETURN

        start-frame(*frame*);  // RETURN

   :seealso:

     - :gf:`exit-frame`
     - :gf:`frame-mapped?-setter`
     - :gf:`start-dialog`

.. generic-function:: undo-command

   Calls the undo command for the specified command.

   :signature: undo-command *command* *frame* => #rest *values*

   :param command: An instance of type :class:`<command>`.
   :param frame: An instance of type :class:`<frame>`.
   :value #rest values: Instances of type :drm:`<object>`.

   :description:

     Calls the undo command for *command*, undoing the effects of calling
     *command*. Note that *command* must be undoable.

     You can call this command directly in your own code, as well as
     specialize it.

   :seealso:

     - :gf:`command-undoable?`

.. variable:: *user-command-table*

   A user-defined command table that can be inherited by other command
   tables.

   :type: :class:`<command-table>`

   :description:

     This is a command table that can be used by the programmer for any
     purpose. DUIM does not use it for anything, and its contents are
     completely undefined.

     If desired, all new command tables can inherit the command table
     specified by this variable.

   :seealso:

     - :class:`<command-table>`
     - :var:`*global-command-table*`

.. class:: <wizard-frame>

   :open:
   :instantiable:

   The class of wizard frames.

   :superclasses: :class:`<dialog-frame>`

   :keyword page: An instance of type :class:`<page>`.
   :keyword pages: An instance of type ``false-or(limited(<sequence>, of:<page>)``.
     Default value: ``#f``.
   :keyword apply-callback: An instance of type ``false-or(<function>)``. Default value: ``#f``.
   :keyword apply-button: An instance of type ``false-or(<button>)``. Default value: ``#f``.

   Note that the following two useful init-keywords are inherited from
   :class:`<dialog-frame>`:

   :keyword pages: An instance of type ``false-or(<sequence>)``. Default value: ``#f``.
   :keyword page-changed-callback: An instance of type ``false-or(<function>)``.
     Default value: ``#f``.

   :description:

     The class of wizard frames. These are frames that are used to create
     wizards (series of connected dialogs) that are used to guide the user
     through a structured task, such as installing an application.

     .. figure:: images/frames-7.png
        :align: center

        A wizard frame

     A wizard frame is a multi-page dialog, in which the user specifies
     requested information before proceeding to the next page in the
     sequence. At the end of the sequence, the user exits the dialog to send
     the relevant information back to the controlling application.

     When a wizard frame is created, each page in the frame automatically has
     a Next and Back button to let the user navigate forward and backward
     through the sequence of pages.

     In addition, if ``apply-button:`` is specified, an Apply button is
     displayed in the frame. By default, clicking on this button lets the
     user apply the changes made so far without dismissing the frame from the
     screen. If specified, the ``apply-callback:`` function is invoked when the
     Apply button is clicked.

     The layout of a wizard frame is controlled using a :class:`<stack-layout>`.

   :operations:

     - :gf:`compute-next-page`
     - :gf:`compute-previous-page`
     - :gf:`dialog-back-button`
     - :gf:`dialog-back-button-setter`
     - :gf:`dialog-back-callback`
     - :gf:`dialog-current-page`
     - :gf:`dialog-current-page-setter`
     - :gf:`dialog-next-button`
     - :gf:`dialog-next-button-setter`
     - :gf:`dialog-next-callback`
     - :gf:`dialog-next-enabled?`
     - :gf:`dialog-next-enabled?-setter`
     - :gf:`dialog-next-page`
     - :gf:`dialog-next-page-setter`
     - :gf:`dialog-page-changed-callback`
     - :gf:`dialog-page-changed-callback-setter`
     - :gf:`dialog-page-complete?`
     - :gf:`dialog-page-complete?-setter`
     - :gf:`dialog-pages`
     - :gf:`dialog-pages-setter`
     - :gf:`dialog-previous-page`
     - :gf:`dialog-previous-page-setter`
     - :gf:`move-to-next-page`
     - :gf:`move-to-previous-page`

   :example:

     .. code-block:: dylan

        define frame <my-wizard> (<wizard-frame>)
          pane name-pane (frame)
            make(<text-field>);
          pane organization-pane (frame)
            make(<text-field>);
          pane job-description-pane (frame)
            make(<text-field>);
          pane years-employed-pane (frame)
            make(<text-field>, value-type: <integer>);
          pane first-page-layout (frame)
            make(<table-layout>,
                 columns: 2,
                 x-alignment: #(#"right", #"left"),
                 children: vector(make(<label>,
                                       label: "Name:"),
                                  frame.name-pane,
                                  make(<label>,
                                       label: "Organization:"),
                                  frame.organization-pane));
          pane second-page-layout (frame)
            make(<table-layout>,
                 columns: 2,
                 x-alignment: #(#"right", #"left"),
                 children: vector
                             (make(<label>,
                                   label: "Job Description:"),
                              frame.job-description-pane,
                              make(<label>,
                                   label: "Years Employed:"),
                              frame.years-employed-pane));
          pane first-page (frame)
            make(<wizard-page>,
                 child: frame.first-page-layout);
          pane second-page (frame)
            make(<wizard-page>,
                 child: frame.second-page-layout);
          pages (frame)
            vector(frame.first-page, frame.second-page);
          keyword title: = "My Wizard";
        end frame <my-wizard>;

   :seealso:

     - :class:`<dialog-frame>`
     - :class:`<property-frame>`
     - :class:`<wizard-page>`

.. class:: <wizard-page>
   :open:
   :instantiable:

   The class of wizard pages.

   :superclasses: :class:`<page>`

   :description:

     The class of wizard pages. These are pages that can be displayed in an
     instance of :class:`<wizard-frame>`, and are used for a
     single dialog in the structured task that the wizard guides the user
     through.

     .. figure:: images/frames-8.png
        :align: center

        A wizard page

   :seealso:

     - :class:`<page>`
     - :class:`<property-page>`
     - :class:`<tab-control-page>`
     - :class:`<wizard-frame>`
