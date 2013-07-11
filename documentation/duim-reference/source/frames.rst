*******************
DUIM-Frames Library
*******************

Overview
========

The DUIM-Frames library contains interfaces that define a wide variety
of frames for use in your GUI applications, as well as the necessary
functions, generic functions, and macros for creating and manipulating
them. The library contains a single module, *duim-frames*, from which
all the interfaces described in this chapter are exposed. `See
DUIM-Frames Module`_ contains complete reference
entries for each exposed interface.

Frames are the basic components used to display DUIM objects on-screen.
An instance of type *<frame>* is an object representing some state in a
user application, plus the sheets in its interface. Frames control the
overall appearance of the entire window, allowing you to distinguish,
for example, between a normal window and a dialog box, or allowing you
to specify modal or modeless dialog boxes, and might include such things
as a menu bar, a tool bar, and a status bar.

Frames exist on windows and contain sheets, which can be instances of
*<layout>* or *<gadget>*, or any of their subclasses, and an event
loop. The event loop associated with a frame is represented by an
instance of a subclass of *<event>*. An overview of these subclasses is
provided in `Subclasses of <frame-event>`_.

The class hierarchy for DUIM-Frames
===================================

This section presents an overview of the available classes of frame,
frame event, and command-related classes, and describes the class
hierarchy present.

The <frame> class and its subclasses
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The base class for all DUIM frames is the *<frame>* class, which is
itself a subclass of *<object>*. In addition, there are a number of
classes related to commands that are subclasses of *<object>*, together
with a number of classes related to events that occur in frames. `See
Overall class hierarchy for the DUIM-Frames library`_
shows the overall class hierarchy for the base classes exported by the
DUIM-Frames library.

Overall class hierarchy for the DUIM-Frames library
                                                   

.. figure:: images/frames-2.png
   :align: center
   :alt: 
<object>

<gadget>

<page>

See `Subclasses of <page>`_

<frame>

See `Subclasses of <frame>`_

<event>

<frame-event>

See `Subclasses of <frame-event>`_

<simple-command>

<simple-undoable-command>

<command-table>

<command-table-menu-item>

The *<frame>* class represents the base class for all types of frame. An
introduction to the subclasses available is given in `Subclasses of
<frame>`_.

The *<event>* class represents the base class for all events that can
occur. Although this class and the *<frame-event>* subclass are exposed
by the DUIM-Sheets library, the subclasses of *<frame-event>* itself are
exposed by the DUIM-Frames library. See `Subclasses of
<frame-event>`_ for an introduction to these
subclasses. See `DUIM-Sheets Library <silica.htm#90521>`_, for a
complete description of the DUIM-Sheets library.

The remaining four classes exposed by the DUIM-Frames library relate to
commands and their use in application menus.

*<simple-command>*
                  

-  This class is used to create the most basic type of command. A
   command is an operation that can be invoked as a callback from a menu
   item, a button, or other suitable interface control.

*<simple-undoable-command>*
                           

-  This class is used to define commands whose effects can be reversed.
   Typically, the user chooses the command *Edit > Undo* to reverse the
   effects of a command of this class.

*<command-table>*
                 

-  The *<command-table>* class is used to define the complete menu
   structure of an application frame, from the menu bar and menus to the
   menu items on each menu.

*<command-table-menu-item>*
                           

-  This class represents a menu item on a menu defined in a command
   table.

Subclasses of <frame>
^^^^^^^^^^^^^^^^^^^^^

A number of subclasses of *<frame>* are provided to allow you to create
a variety of common types of frame. These subclasses are shown in `See
Subclasses of the <frame> class`_.

Subclasses of the *<frame>* class
                                 

.. figure:: images/frames-2.png
   :align: center
   :alt: 
<frame>

<simple-frame>

<dialog-frame>

<property-frame>

<wizard-frame>

-  *<simple-frame>* This class is the most common sort of frame and is
   used to create a standard window in an application.
-  *<dialog-frame>* This class is used to create dialog boxes for use in
   an application.

*<property-frame>*
                  

-  This class is used to create property sheets for use in an
   application. Property sheets are a special type of dialog box which
   make use of tab controls to display several pages of information
   within the same dialog.

*<wizard-frame>*
                

-  This class is used to create wizards for use in an application.
   Wizards are a special type of multi-page dialog in which the user is
   guided through a series of sequential steps, filling out any
   information requested and using *Next* and *Back* buttons to navigate
   to the next or previous steps in the process.

Subclasses of <frame-event>
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The *<frame-event>* class provides a number of subclasses that describe
various events that can occur in frames. These subclasses are shown in
`Subclasses of the <frame-event> class`_.

Subclasses of the *<frame-event>* class
                                       

.. figure:: images/frames-2.png
   :align: center
   :alt: 
<frame-event>

<frame-created-event>

<frame-destroyed-event>

<frame-mapped-event>

<frame-unmapped-event>

<frame-exit-event>

<frame-exited-event>

<application-exited-event>

The name of each of these subclasses accurately reflects the type of
event that they are used to represent. The classes
*<frame-created-event>* and *<frame-destroyed-event>* represent a frame
being created or destroyed. The classes *<frame-mapped-event>* and
*<frame-unmapped-event>* represent the events that occur when a frame is
displayed on the computer screen or removed from it. The class
*<frame-exit-event>* represents the act of exiting a frame, and the
class *<frame-exited-event>* represents the event where a frame has been
successfully exited.

In addition, the class *<frame-exited-event>* has a subclass
*<application-exited-event>*. This is reserved for the special case
where the frame that has been exited is actually the parent frame for
the whole application, in which case the whole application is exited,
together with any other frames that may have been spawned as a result of
using the application.

*Note:* The classes *<frame-mapped-event>* and *<frame-unmapped-event>*
are distinct from the classes *<frame-created-event>* and
*<frame-destroyed-event>*. A frame is not necessarily mapped as soon as
it is created, and any frame can be unmapped from the screen without
actually destroying it (for example, a frame may be iconized).

Subclasses of <page>
^^^^^^^^^^^^^^^^^^^^

Although the *<page>* class is itself a subclass of *<gadget>*, and is
exposed by the DUIM-Gadgets library, two of its subclasses are exposed
by the DUIM-Frames library: *<wizard-page>* and *<property-page>*. See
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

menu-item "My Command" = make(<command>, function: my-command),

menu-item "My Command" = my-command,

Beninning with Harlequin Dylan 1.1, only the last of these may be used.
This may require you to change some of your code.

DUIM-Frames Module
==================

This section contains a complete reference of all the interfaces that
are exported from the *duim-frames* module.

=
~

G.f. method
'''''''''''

Summary
       

Returns true if the specified commands are the same.

Signature
         

= *command1* *command2* => *equal?*

Arguments
         

-  *command1* An instance of type `<command>`_.
-  *command2* An instance of type `<command>`_.

Values
      

-  *equal?* An instance of type *<boolean>*.

Description
           

Returns true if *command1* and *command2* are the same.

add-command
-----------

Generic function
''''''''''''''''

Summary
       

Adds a command to the specified command table.

Signature
         

add-command *command-table* *command* #key *name* *menu image*
*accelerator* *mnemonic* *error?* => ()

Arguments
         

-  *command-table* An instance of type `See
   <command-table>`_.
-  *command* An instance of type *type-union(`See
   <command>`_, <function>)*.
-  *name* An instance of type *false-or(<string>)*.
-  *menu* An instance of type *false-or(* `See
   <menu> <gadgets.htm#81833>`_*)*.
-  *image* An instance of type *false-or(* `See
   <image> <dcs.htm#51234>`_*)*.
-  *accelerator* An instance of type *false-or(* `See
   <gesture> <silica.htm#76256>`_*)*.
-  *mnemonic* An instance of type *false-or(* `See
   <gesture> <silica.htm#76256>`_*)*.
-  *error?* An instance of type *<boolean>*. Default value: *#t*.

Values
      

None

Description
           

You can supply a keyboard accelerator or a mnemonic using the
*accelerator* and *mnemonic* arguments respectively.

Adds *command* to *command-table*.

The argument *name* is the command-line name for the command.

-  When *name* is *#f*, the command is not available via command-line
   interactions.
-  When *name* is a string, that string is the command-line name for the
   command.

For the purposes of command-line name lookup, the character case of
*name* is ignored.

The argument *menu* is a menu for *command*.

-  When *menu* is *#f*, *command* is not available via menus.
-  When *menu* is a string, the string is used as the menu name.
-  When *menu* is *#t* and *name* is a string, then *name* is used as
   the menu name.
-  When *menu* is *#t* and *name* is not a string, a menu name is
   automatically generated.
-  When *menu* is a list of the form *(* *string* *,* *menu-options* *)*
   , *string* is the menu name and *menu-options* consists of a list of
   keyword-value pairs. Each keyword-value pair is itself a list. The
   valid keywords are *after:*, *documentation:*, and *text-style:*,
   which are interpreted as for `See
   add-command-table-menu-item`_.

You can supply an image that will appear on the menu next to the command
name using the *image* argument. When supplying an image, bear in mind
the size of the menu: you should only supply a small icon-sized image
for a menu command. There may also be other interface guidelines that
you wish to follow when using images in menu items.

The value for *accelerator* is either keyboard gesture or *#f*. When it
is a gesture, this gesture represents the keystroke accelerator for the
command; otherwise the command is not available via keystroke
accelerators. Similarly, if mnemonic is supplied, this gesture is used
as a mnemonic for the command.

If *command* is already present in the command table and *error?* is
*#t*, an error is signalled. When *command* is already present in the
command table and *error?* is *#f* }, then the old command-line name,
menu, and keystroke accelerator are removed from the command table
before creating the new one.

See also
        

`remove-command`_

add-command-table-menu-item
---------------------------

Generic function
''''''''''''''''

Summary
       

Adds a menu item to the specified command table.

Signature
         

add-command-table-menu-item *command-table* *string* *type* *value* #key
*documentation* *after* *accelerator* *mnemonic* *text-style* *error?*
*items* *label-key* *value-key* *test* *callback* => *menu-item*

Arguments
         

-  *command-table* An instance of type `See
   <command-table>`_.
-  *string* An instance of type *false-or(<string>)*.
-  *type* An instance of type *one-of(#"command", #"function", #"menu",
   #"divider")*.
-  *value* An instance of type *<object>*.
-  *documentation* An instance of type *<string>*.
-  *after* An instance of type *one-of(#"start", #"end", #"sort")*, or
   an instance of *<string>*. Default value: *#"end"*.
-  *accelerator* An instance of type *false-or(* `See
   <gesture> <silica.htm#76256>`_*)*.
-  *mnemonic* An instance of type *false-or(* `See
   <gesture> <silica.htm#76256>`_*)*.
-  *text-style* An instance of type `<text-style> <dcs.htm#85385>`_.
-  *error?* An instance of type *<boolean>*. Default value: *#t*.
-  *items* An instance of type *limited(<sequence>, of: )*.
-  *label-key* An instance of type *<function>*.
-  *value-key* An instance of type *<function>*.
-  *test* An instance of type *<function>*.
-  *callback* An instance of type *<function>*.

Values
      

-  *menu-item* An instance of type `See
   <command-table-menu-item>`_.

Description
           

Adds a command menu item to the menu in *command-table*. The *string*
argument is the name of the command menu item; its character case is
ignored. The *type* of the item is either *#"command"*, *#"function"*,
*#"menu"*, or *#"divider"*.

When *type* is *#"command"*, *value* must be one of the following:

-  A command (a list consisting of a command name followed by a list of
   the arguments for the command).
-  A command name. In this case, *value* behaves as though a command
   with no arguments was supplied.

When all the required arguments for the command are supplied, clicking
on an item in the menu invokes the command immediately. Otherwise, the
user is prompted for the remaining required arguments.

When *type* is *#"function"*, *value* must be a function having
indefinite extent that, when called, returns a command. The function is
called with two arguments:

-  The gesture used to select the item (either a keyboard or button
   press event).
-  A "numeric argument".

When *type* is *#"menu"*, this indicates that a sub-menu is required,
and *value* must be another command table or the name of another command
table.

When *type* is *#"divider"*, some sort of a dividing line is displayed
in the menu at that point. If *string* is supplied, it will be drawn as
the divider instead of a line. If the look and feel provided by the
underlying window system has no corresponding concept, *#"divider"*
items may be ignored. When *type* is *#"divider"*, *value* is ignored.

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

-  *#"start"* Adds the new item to the beginning of the menu.
-  *#"end"* Adds the new item to the end of the menu.

A string naming an existing entry
                                 

-  Adds the new item after that entry.
-  *#"sort"* Insert the item in such as way as to maintain the menu in
   alphabetical order.

If *mnemonic* is supplied, the item is added to the keyboard mnemonic
table for the command table. The value of *mnemonic* must be a keyboard
gesture name.

When *mnemonic* is supplied and *type* is *#"command"* or *#"function"*
, typing a key on the keyboard that matches the mnemonic invokes the
command specified by *value*.

When *type* is *#"menu"*, the command is read from the submenu
indicated by *value* in a window system specific manner. This usually
means that the submenu itself is displayed, allowing the user to see the
available options at that point.

When *accelerator* is supplied, typing a key sequence on the keyboard
that matches the accelerator invokes the command specified by *value*,
no matter what *type* is.

If the item named by *string* is already present in the command table
and *error?* is *#t*, then an error is signalled. When the item is
already present in the command table and *error?* is *#f*, the old item
is removed from the menu before adding the new item. Note that the
character case of *string* is ignored when searching the command table.

See also
        

`<command-table-menu-item>`_

`remove-command-table-menu-item`_

<application-exited-event>
--------------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events signalled when an application exits.

Superclasses
            

`<frame-exited-event>`_

Init-keywords
             

None.

Description
           

The class of events signalled when an application exits. An instance of
this class is distributed when your application is exited, for instance
by choosing *File > Exit* from its main menu bar.

Operations
          

-  None.

See also
        

`exit-frame`_

`<frame-exited-event>`_

apply-in-frame
--------------

Generic function
''''''''''''''''

Summary
       

Applies the specified function to the given arguments in the main thread
of the frame.

Signature
         

apply-in-frame *frame function arg* #rest *args* => ()

Arguments
         

-  *frame* An instance of type `<frame>`_.
-  *function* An instance of type *<function>*.
-  *arg* An instance of type *<object>*.
-  *args* Instances of type *<object>*.

Values
      

None.

Description
           

Applies *function* to the given arguments in the main thread of *frame*
. You must supply at least one argument (*arg*), though you can
optionally supply as many additional arguments as you like.

See also
        

`call-in-frame`_

call-in-frame
-------------

Generic function
''''''''''''''''

Summary
       

Calls the specified function with the given arguments in the main thread
of the frame.

Signature
         

call-in-frame *frame function* #rest *args* => ()

Arguments
         

-  *frame* An instance of type `<frame>`_.
-  *function* An instance of type *<function>*.
-  *args* Instances of type *<object>*.

Values
      

None.

Description
           

Calls *function* with the given arguments in the main thread of *frame*
.

See also
        

`apply-in-frame`_

cancel-dialog
-------------

Generic function
''''''''''''''''

Summary
       

Cancels the specified dialog.

Signature
         

cancel-dialog *dialog* #key *destroy?* => ()

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.
-  *destroy?* An instance of type *<boolean>*. Default value: *#t*.

Values
      

None

Description
           

Cancels *dialog* and removes it from the screen. Any changes that the
user has made to information displayed in the dialog is discarded.

If *destroy?* is *#t* then the dialog is unmapped from the screen.

This is the default callback used for the cancel button in a dialog.

Example
       

The following example defines a button, *\*no-button\**, that calls
*cancel-dialog* as its activate-callback. This button is then used in a
dialog that simply replaces the standard cancel button for the newly
defined dialog. Note that the example assumes the existence of a similar
*\*yes-button\** to replace the exit button.

define variable \*no-button\*
                             

= make(<push-button>, label: "No",

activate-callback: cancel-dialog,

max-width: $fill);
                  

make(<dialog-frame>,
                    

exit-button?: #f,

cancel-button?: #f,

layout: vertically ()

make(<label>,

label: "Simple dialog");

horizontally ()

\*yes-button\*;

\*no-button\*;

end

end);
     

start-frame(\*dialog\*);

See also
        

`dialog-cancel-callback`_

`<dialog-frame>`_

`start-dialog`_

`exit-dialog`_

clear-progress-note
-------------------

Generic function
''''''''''''''''

Summary
       

Clears the specified progress note.

Signature
         

clear-progress-note *framem* *progress-note* => ()

Arguments
         

-  *framem* An instance of type `See
   <frame-manager> <silica.htm#32466>`_.
-  *progress-note* An instance of type *<progress-note>*.

Values
      

None

Description
           

Clears the specified progress note.

<command>
---------

Open abstract instantiable class
''''''''''''''''''''''''''''''''

Summary
       

The class of commands.

Superclasses
            

*<object>*

Init-keywords
             

-  *function:* An instance of type *<function>*.
-  *arguments:* An instance of type *<sequence>*. Default value: *#[]*
   .

Description
           

The class of commands. These are commands that can be grouped together
in a command table to form the set of commands available to an
application (available, for example, from the menu bar of the
application). The resulting command object can then be executed by
calling
 `execute-command`_.

The *function:* init-keyword is the command function that is called by
the command object. A command function is rather like a callback to a
*<command>* object: a command can be executed via *execute-command*,
which then invokes the command function. Command functions take at least
one argument: a `<frame>`_ object.

The *arguments:* init-keyword are the arguments passed to the command
function.

Operations
          

`=`_ `add-command`_ `See
command-arguments`_ `See
command-enabled?`_
 `command-enabled?-setter`_ `See
command-function`_ `See
command-undoable?`_ `See
dialog-cancel-callback-setter`_ `See
dialog-exit-callback-setter`_ `See
execute-command`_ `See
gadget-command <gadgets.htm#39838>`_ `See
gadget-command-setter <gadgets.htm#66876>`_ `See
gadget-key-press-callback-setter <gadgets.htm#10874>`_ `See
redo-command`_ `See
remove-command`_ `See
undo-command`_

See also
        

`command?`_

`command-arguments`_

`command-function`_

`execute-command`_

`<simple-command>`_

command?
--------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified object is a command.

Signature
         

command? *object* => *command?*

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *command?* An instance of type *<boolean>*.

Description
           

Returns true if *object* is an instance of `See
<command>`_.

See also
        

`<command>`_

command-arguments
-----------------

Generic function
''''''''''''''''

Summary
       

Returns the arguments to the specified command.

Signature
         

command-arguments *command* => *arguments*

Arguments
         

-  *command* An instance of type `<command>`_.

Values
      

-  *arguments* An instance of type *<sequence>*.

Description
           

Returns the arguments to *command*.

See also
        

`<command>`_

command-enabled?
----------------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified command is enabled.

Signature
         

command-enabled? *command* *frame* #key => *enabled?*

Arguments
         

-  *command* An instance of type *type-union(`See
   <command>`_, `See
   <command-table>`_)*.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *enabled?* An instance of type *<boolean>*.

Description
           

Returns true if *command* in *frame* is enabled.

See also
        

`<command>`_

`command-enabled?-setter`_

command-enabled?-setter
-----------------------

Generic function
''''''''''''''''

Summary
       

Enables or disables the specified command.

Signature
         

command-enabled?-setter *enabled?* *command* *frame* => *enabled?*

Arguments
         

-  *enabled?* An instance of type *<boolean>*.
-  *command* An instance of type *type-union(`See
   <command>`_, `See
   <command-table>`_)*.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *enabled?* An instance of type *<boolean>*.

Description
           

Enables or disables *command* in *frame*. If *enabled?* is true, then
*command* is enabled, otherwise it is disabled. Enabling and disabling a
command enables and disables all the gadgets that are associated with
the command, such as menu items and tool bar buttons.

This function is useful when manipulating the disabled commands in
*frame*. For example, it is common to disable the *Save* menu command
immediately after saving a file, enabling it again only when the file
has been modified.

See also
        

`command-enabled?`_

command-function
----------------

Generic function
''''''''''''''''

Summary
       

Returns the function associated with the specified command.

Signature
         

command-function *command* => *function*

Arguments
         

-  *command* An instance of type `<command>`_.

Values
      

-  *function* An instance of type *<function>*.

Description
           

Returns the function associated with *command*. A command function is
the function that is called by a *<command>* object. Command functions
are similar to callbacks, in that they are user functions that are
invoked in order to perform some action. Command functions take at least
one argument: a `<frame>`_ object.

See also
        

`<command>`_

`execute-command`_

<command-table>
---------------

Open abstract instantiable class
''''''''''''''''''''''''''''''''

Summary
       

The class of command tables.

Superclasses
            

*<object>*

Init-keywords
             

-  *name:* An instance of type *<object>*. Required.
-  *inherit-from:* An instance of type *limited(<sequence>, of:
   <command-table>)*. Required.
-  *resource-id:* An instance of type *false-or(<object>)*. Default
   value: *#f*.

Description
           

The class of command tables. The command table for an application gives
a complete specification of the commands available to that application,
through its menus, tool bars, mnemonics, and accelerators.

The *name:* init-keyword is a symbol that names the current command
table.

The *inherit-from:* init-keyword is a sequence of command tables whose
behavior the current command table should inherit. All command tables
inherit the behavior of the command table specified by `See
\*global-command-table\*`_, and can also inherit the
behavior specified by `\*user-command-table\*`_.

You do not normally need to specify a unique *resource-id:* yourself. As
with most other DUIM classes, the *name:* init-keyword serves as a
sufficient unique identifier.

Operations
          

`add-command`_ `See
add-command-table-menu-item`_ `See
command-table-accelerators`_ `See
command-table-commands`_ `See
command-table-menu`_ `See
command-table-name`_ `See
frame-command-table-setter`_ `See
make`_ `See
make-menu-from-command-table-menu`_ `See
make-menus-from-command-table`_ `See
remove-command`_
 `remove-command-table`_ `See
remove-command-table-menu-item`_

Example
       

define command-table \*clipboard-command-table\*
                                                

=(\*global-command-table\*)

menu-item "Cut" = cut-selection,

documentation: $cut-doc;

menu-item "Copy" = copy-selection,

documentation: $copy-doc;

menu-item "Paste" = paste-from-clipboard,

documentation: $paste-doc;

menu-item "Delete" = delete-selection,

documentation: $delete-doc;

end command-table \*clipboard-command-table\*;
                                              

See also
        

`\*global-command-table\*`_

`\*user-command-table\*`_

command-table?
--------------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified object is a command table.

Signature
         

command-table? *object* => *command-table?*

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *command-table?* An instance of type *<boolean>*.

Description
           

Returns true if *object* is a command table.

See also
        

`<command-table>`_

command-table-accelerators
--------------------------

Generic function
''''''''''''''''

Summary
       

Returns the keyboard accelerators for the specified command table.

Signature
         

command-table-accelerators *command-table* => *accelerators*

Arguments
         

-  *command-table* An instance of type `See
   <command-table>`_.

Values
      

-  *accelerators* An instance of type *limited(<sequence>, of: `See
   <gesture> <silica.htm#76256>`_)*.

Description
           

Returns the keyboard accelerators for *command-table*.

See also
        

`command-table-commands`_

command-table-commands
----------------------

Generic function
''''''''''''''''

Summary
       

Returns the commands for the specified command table.

Signature
         

command-table-commands *command-table* => *commands*

Arguments
         

-  *command-table* An instance of type `See
   <command-table>`_.

Values
      

-  *commands* An instance of type *limited(<sequence>, of:* `See
   <command>`_*)*.

Description
           

Returns the commands defined for *command-table*.

See also
        

`command-table-accelerators`_

`command-table-menu`_

command-table-menu
------------------

Generic function
''''''''''''''''

Summary
       

Returns the menu items in the specified command table.

Signature
         

command-table-menu *command-table* => *menu-items*

Arguments
         

-  *command-table* An instance of type `See
   <command-table>`_.

Values
      

-  *menu-items* An instance of type *<stretchy-vector>*.

Description
           

Returns the menu items in *command-table*.

See also
        

`command-table-commands`_

`command-table-name`_

<command-table-menu-item>
-------------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of menu items in command tables.

Superclasses
            

*<object>*

Init-keywords
             

-  *name:* An instance of type *false-or(<string>)*. Default value:
   *#f*.
-  *image:* An instance of type *false-or(type-union(<string>, `See
   <image> <dcs.htm#51234>`_))*. Default value: *#f*
-  *type:* An instance of type *one-of(#"command", #"function", #"menu",
   #"divider")*.
-  *value:* An instance of type *<object>*. Default value: *#f*.
-  *options:* An instance of type *<sequence>*. Default value: *#()*.
-  *accelerator:* An instance of type *false-or(* `See
   <gesture> <silica.htm#76256>`_*)*. Default value: *#f*.
-  *mnemonic:* An instance of type *false-or(* `See
   <gesture> <silica.htm#76256>`_*)*. Default value: *#f*.

Description
           

The class of menu items in command tables. This class models menu items,
tool bar items, accelerators, and mnemonics for a command table entry.

The *type:* init-keyword denotes what type of menu item has been
created. This is either *#"command"*, *#"function"*, *#"menu"*, or
*#"divider"*.

When *type:* is *#"command"*, *value:* must be one of the following:

-  A command (a list consisting of a command name followed by a list of
   the arguments for the command).
-  A command name. In this case, *value* behaves as though a command
   with no arguments was supplied.

When all the required arguments for the command are supplied, clicking
on an item in the menu invokes the command immediately. Otherwise, the
user is prompted for the remaining required arguments.

When *type:* is *#"function"*, *value:* must be a function having
indefinite extent that, when called, returns a command. The function is
called with two arguments:

-  The gesture used to select the item (either a keyboard or button
   press event).
-  A "numeric argument".

When *type:* is *#"menu"*, this indicates that a sub-menu is required,
and *value* must be another command table or the name of another command
table.

When *type:* is *#"divider"*, some sort of a dividing line is displayed
in the menu at that point. If a string is supplied using the *options:*
init-keyword, it will be drawn as the divider instead of a line. If the
look and feel provided by the underlying window system has no
corresponding concept, *#"divider"* items may be ignored. When *type:*
is *#"divider"*, *value:* is ignored.

The *accelerator:* and *mnemonic:* init-keywords let you specify a
keyboard accelerator and mnemonic for the menu item.

Operations
          

`add-command-table-menu-item`_ `See
menu-item-accelerator`_ `See
menu-item-mnemonic`_ `See
menu-item-name`_ `See
menu-item-options`_ `See
menu-item-type`_ `See
menu-item-value`_

See also
        

`add-command-table-menu-item`_

command-table-name
------------------

Generic function
''''''''''''''''

Summary
       

Returns the name of the specified command table.

Signature
         

command-table-name *command-table* => *name*

Arguments
         

-  *command-table* An instance of type `See
   <command-table>`_.

Values
      

-  *name* An instance of type *<object>*.

Description
           

Returns the name of *command-table*, as defined by the *name:*
init-keyword for `<command-table>`_.

See also
        

`<command-table>`_

`command-table-menu`_

command-undoable?
-----------------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified command is undoable.

Signature
         

command-undoable? *command* => *undoable?*

Arguments
         

-  *command* An instance of type `<command>`_.

Values
      

-  *undoable?* An instance of type *<boolean>*.

Description
           

Returns true if *command* is undoable, that is, there is a specified
command that the user can choose (for instance, by choosing *Edit >
Undo*) that will reverse the effects of command.

See also
        

`undo-command`_

complete-from-generator
-----------------------

Generic function
''''''''''''''''

Summary
       

Completes a string based on a generated list of completions.

Signature
         

complete-from-generator *string generator delimiters*
 #key *action predicate
* => *string success object nmatches completions*

Arguments
         

-  *string* An instance of type *<string>*.
-  *generator* An instance of type *<function>*.
-  *delimiters* An instance of type *limited(<sequence>, of:
   <character>)*.
-  *action* An instance of type *one-of(#"complete",
   #"complete-limited", #"complete-maximal", #"completions",
   #"apropos-completions")*. Default value *#"complete"*.
-  *predicate* An instance of type *false-or(<function>)*. Default
   value *#f*.

Values
      

-  *string* An instance of type *false-or(<string>)*.
-  *success* An instance of type *<boolean>*.
-  *object* An instance of type *<object>*.
-  *nmatches* An instance of type *<integer>*.
-  *completions* An instance of type *<sequence>*.

Description
           

Completes *string* chunk-wise against a list of possibilities derived
from *generator*, using the specified *delimiters* to break both
*string* and the generated possibilities into chunks. This function is
identical to `complete-from-sequence`_, except
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
(if *action* is *#"completions"*) a sequence of possible completions.

The *action* argument can take any of the following values:

-  *#"complete"* Completes the input as much as possible, except that if
   the user’s input exactly matches one of the possibilities, the
   shorter possibility is returned as the result, even if it is a left
   substring of another possibility.
-  *#"complete-limited"* Completes the input up to the next partial
   delimiter.
-  *#"complete-maximal"* Completes the input as much as possible.

*#"completions"* or *#"apropos-completions"*
                                            

-  Returns a sequence of the possible completions.

Example
       

complete-from-generator
                       

("th", method (string, completer)

for (b in #["one", "two", "three", "four"])

completer(b, b)

end

end method, #[' ', '-'])
                        

See also
        

`complete-from-sequence`_

complete-from-sequence
----------------------

Generic function
''''''''''''''''

Summary
       

Completes a string based on a list of possible completions.

Signature
         

complete-from-sequence *string possibilities delimiters*
 #key *action predicate label-key value-key*
 => *string success object nmatches completions*

Arguments
         

-  *string* An instance of type *<string>*.
-  *possibilities* An instance of type *limited(<sequence>, of:
   <string>)*.
-  *delimiters* An instance of type *limited(<sequence>, of:
   <character>)*.
-  *action* An instance of type *one-of(#"complete",
   #"complete-limited", #"complete-maximal", #"completions",
   #"apropos-completions")*. Default value *#"complete"*.
-  *predicate* An instance of type *false-or(<function>)*. Default
   value *#f*.
-  *label-key* An instance of type *<function>*. Default value *first*
   .
-  *value-key* An instance of type *<function>*. Default value *second*
   .

Values
      

-  *string* An instance of type *false-or(<string>)*.
-  *success* An instance of type *<boolean>*.
-  *object* An instance of type *<object>*.
-  *nmatches* An instance of type *<integer>*.
-  *completions* An instance of type *<sequence>*.

Description
           

Completes *string* chunk-wise against the list of *possibilities*,
using the specified *delimiters* to break both *string* and the strings
in *possibilities* into chunks.

*The label-key* and *value-key* arguments are used to extract the
completion string and object from the entries in *possibilities*, and
*predicate* (if supplied) is applied to filter out unwanted objects.

The function returns five values: the completed string (if there is
one), whether or not the completion successfully matched, the object
associated with the completion, the number of things that matched, and
(if *action* is *#"completions"*) a sequence of possible completions.

The *action* argument can take any of the following values:

-  *#"complete"* Completes the input as much as possible, except that if
   the user’s input exactly matches one of the possibilities, the
   shorter possibility is returned as the result, even if it is a left
   substring of another possibility.
-  *#"complete-limited"* Completes the input up to the next partial
   delimiter.
-  *#"complete-maximal"* Completes the input as much as possible.

*#"completions"* or *#"apropos-completions"*
                                            

-  Returns a sequence of the possible completions.

Example
       

complete-from-sequence("s w ma",
                                

#["one fish two fish",

"red fish blue fish",

"single white male",

"on beyond zebra"],

#[' ', '-'],

label-key: identity,

value-key: identity)
                    

See also
        

`complete-from-generator`_

compute-next-page
-----------------

Generic function
''''''''''''''''

Summary
       

Returns the next page in the specified wizard frame.

Signature
         

compute-next-page *dialog* => *next-page*

Arguments
         

-  *dialog* An instance of type `See
   <wizard-frame>`_.

Values
      

-  *next-page* An instance of type *false-or(`See
   <sheet> <silica.htm#13118>`_)*.

Description
           

Returns the next page in *dialog*, which must be a wizard.

See also
        

`compute-previous-page`_

`<wizard-frame>`_

compute-previous-page
---------------------

Generic function
''''''''''''''''

Summary
       

Returns the previous page in the specified wizard frame.

Signature
         

compute-previous-page *dialog* => *prev-page*

Arguments
         

-  *dialog* An instance of type `See
   <wizard-frame>`_.

Values
      

-  *prev-page* An instance of type *false-or(`See
   <sheet> <silica.htm#13118>`_)*.

Description
           

Returns the previous page in *dialog*, which must be a wizard.

See also
        

`compute-next-page`_

`<wizard-frame>`_

contain
-------

Generic function
''''''''''''''''

Summary
       

Creates and returns a frame containing the specified object.

Signature
         

contain *object* #rest *initargs* #key *own-thread?* #all-keys =>
*sheet* *frame*

Arguments
         

-  *object* An instance of type *type-union(`See
   <sheet> <silica.htm#13118>`_, <class>, `See
   <frame>`_)*.
-  *initargs* Instances of type *<object>*.

Values
      

-  *sheet* An instance of type `<sheet> <silica.htm#13118>`_.
-  *frame* An instance of type `<frame>`_.

Description
           

Creates and returns a frame containing *object*. This function is
intended to be used as a convenience function when testing sections of
code in development; you are note recommended to use it in your final
source code. The function wraps a set of DUIM objects in a frame and
displays them on screen, without you needing to worry about the
creation, management, or display of frames on the computer screen. The
*contain* function is most useful when testing code interactively using
the Dylan Interactor.

If *own-thread?* is *#t*, then the window that is created by *contain*
runs in its own thread. If not supplied, *own-thread?* is *#f*.

Consider the following expression that calls *contain* :

contain(make(<button>));

This is equivalent to the fuller expression:

begin
     

let frame = make(<simple-frame>,

title: "container",

layout: make(<button>));

start-frame(frame);

end;
    

As can be seen, when testing short pieces of code interactively in the
environment, the former section of code is easier to use than the
latter.

Example
       

Assigning the result of a contain expression allows you to manipulate
the DUIM objects being contained interactively, as shown in the example
below.

You should assume the following code is typed into the Dylan Interactor,
and that each expression is evaluated by pressing the RETURN key at the
points indicated.

\*g\* := contain
                

(make

(<list-box>,

items: #(#"One", #"Two", #"Three"),

label-key:

method (symbol) as-lowercase

(as(<string>, symbol))

end));*RETURN*
              

gadget-items(\*g\*);*RETURN*

As you would expect, evaluating the call to `See
gadget-items <gadgets.htm#45083>`_ returns the following result:

#(#"one", #"two", #"three")

In a similar way, you can destructively modify the slot values of any
contained DUIM objects

current-frame
-------------

Function
''''''''

Summary
       

Returns the current frame

Signature
         

current-frame => *frame*

Arguments
         

None

Values
      

-  *frame* An instance of type `<frame>`_

Description
           

Returns the current frame.

define command-table
--------------------

Definition macro
''''''''''''''''

Summary
       

Defines a new class of command table with the specified name and
properties.

Macro call
          

define command-table *name* ({*supers* },\*) {*options* } end

Arguments
         

-  *name* A Dylan name*bnf*.
-  *supers* A Dylan name*bnf*.
-  *options* A Dylan body*bnf*.

Values
      

-  None.

Description
           

Defines a new class of command table with the specified name and
properties. This macro is equivalent to *define class*, but with
additional options.

The *supers* argument specifies a comma-separated list of command tables
from which the command table you are creating should inherit. If you are
not explicitly inheriting the behavior of other command tables, then
*supers* should have the value `See
\*global-command-table\*`_.

Each one of the *options* supplied describes a command for the command
table. This can be either a menu item, a separator, or another command
table to be included in the command table. You can supply any number of
options. Each option take one of the following forms:

menu-item *menu-item-descriptor* ;
                                  

include *command-table-name* ;

separator;
          

To add a menu item or menu to a command table, include an option of the
following form:

menu-item *label* = *command-function*
                                      

#key *accelerator documentation*
                                

-  *label* An instance of *<string>*. This is the label that appears in
   the menu.

*command-function*
                  

-  An instance of *type-union(`<command>`_, `See
   <command-table>`_, <function>)*. The command
   function is the callback that is invoked to perform the intended
   operation for the menu item. Note that this can itself be a command
   table.
-  *accelerator* An instance of *false-or(`See
   <gesture> <silica.htm#76256>`_)*. Default value: *#f*. This defines
   a keyboard accelerator that can be used to invoke *command-function*
   in preference to the menu item itself.
-  *documentation* An instance of *false-or(<string>)*. Default value:
   *#f*. This specifies a documentation string for the menu item that
   can be used to provide online help to the user. For menu items,
   documentation strings are usually displayed in the status bar of your
   application, when the mouse pointer is placed over the menu item
   itself.

To add a separator to a menu, just include the following option at the
point you want the separator to appear:

separator;

To include another command table in the current table, include the
following option at the point you want the command table to appear:

include *command-table-name* ;

The commands defined in *command-table-name* are added to the current
command table at the appropriate point.

Example
       

The following example shows how you might create a command table for the
standard Windows *File* menu, and how this could be integrated into the
menu bar for an application. The example assumes that the appropriate
command functions have already been defined for each command in the
command table.

define command-table
                    

\*file-menu-command-table\* (\*global-command-table\*)

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

include \*save-files-command-table\*;

separator;

menu-item "Exit"

= make(<command>,

function: exit-frame);

end command-table \*file-menu-command-table\*;
                                              

define command-table
                    

\*application-command-table\* (\*global-command-table\*)

menu-item "File" = \*file-menu-command-table\*;

menu-item "Edit" = \*edit-menu-command-table\*;

menu-item "View" = \*view-menu-command-table\*;

menu-item "Windows" = \*windows-menu-command-table\*;

menu-item "Help" = \*help-menu-command-table\*;

end command-table \*application-command-table\*;
                                                

See also
        

`\*global-command-table\*`_

define frame
------------

Definition macro
''''''''''''''''

Summary
       

Defines a new class of frame with the specified properties.

Macro call
          

define frame *name* ({*supers* },\*) {*slots-panes-options* } end

Arguments
         

-  *name* A Dylan name*bnf*.
-  *supers* A Dylan name*bnf*.
-  *slots-panes-options* A Dylan body*bnf*.

Values
      

-  None.

Description
           

Defines a new class of frame called *name* with the specified
properties. This macro is equivalent to *define class*, but with
additional options.

The *supers* argument lets you specify any classes from which the frame
you are creating should inherit. You must include at least one concrete
frame class, such as `<simple-frame>`_ or `See
<dialog-frame>`_.

The *slots-panes-options* supplied describe the state variables of the
frame class; that is, the total composition of the frame. This includes,
but is not necessarily limited to, any panes, layouts, tool bar, menus,
and status bar contained in the frame. You can specify arbitrary slots
in the definition of the frame. You may specify any of the following:

-  A number of slots for defining per-instance values of the frame
   state.
-  A number of named panes. Each pane defines a sheet of some sort.
-  A single layout.
-  A tool bar.
-  A status bar.
-  A menu bar.
-  A command table.
-  A number of sequential pages for inclusion in a multi-page frame such
   as a wizard or property dialog.

*Note:* If the frame has a menu bar, either define the menu bar and its
panes, or a command table, but not both. See the discussion below for
more details.

The syntax for each of these options is described below.

The *slot* option allows you to define any slot values that the new
frame class should allow. This option has the same syntax as slot
specifiers in *define class*, allowing you to define init-keywords,
required init-keywords, init-functions and so on for the frame class.

For each of the remaining options, the syntax is as follows:

*option* *name* (*owner*) *body* ;

The argument *option* is the name of the option used, taken from the
list described below, *name* is the name you assign to the option for
use within your code, *owner* is the owner of the option, usually the
frame itself, and *body* contains the definition of value returned by
the option.

*pane* specifies a single pane in the frame. The default is *#f*,
meaning that there is no single pane. This is the simplest way to define
a pane hierarchy.

*layout* specifies the layout of the frame. The default is to lay out
all of the named panes in horizontal strips. The value of this option
must evaluate to an instance of a layout.

*command-table* defines a command table for the frame. The default is to
create a command table with the same name as the frame. The value of
this option must evaluate to an instance of `See
<command-table>`_.

*menu-bar* is used to specify the commands that will in the menu bar of
the frame. The default is *#t*. If used, it typically specifies the
top-level commands of the frame. The value of this option can evaluate
to any of the following:

-  *#f* The frame has no menu bar.
-  *#t*, The menu bar for the frame is defined by the value of the
   *command-table* option.

A command table
               

-  The menu bar for the frame is defined by this command table.
-  A body of code This is interpreted the same way as the *menu-item*
   options to `define command-table`_.

*disabled-commands* is used to specify a list of command names that are
initially disabled in the application frame. The default is *#[]*. The
set of enabled and disabled commands can be modified via `See
command-enabled?-setter`_.

*tool-bar* is used to specify a tool bar for the frame. The default is
*#f*. The value of this option must evaluate to an instance of `See
<tool-bar> <gadgets.htm#58915>`_.

*top-level* specifies a function that executes the top level loop of the
frame. It has as its argument a list whose first element is the name of
a function to be called to execute the top-level loop. The function must
take at least one argument, which is the frame itself. The rest of the
list consists of additional arguments to be passed to the function.

*icon* specifies an `<image> <dcs.htm#51234>`_*<image>* to be used
in the window decoration for the frame. This icon may be used in the
title bar of the frame, or when the frame is iconized, for example.

*geometry* specifies the geometry for the frame.

*pages* is used to define the pages of a wizard or property frame. This
evaluates to a list of pages, each of which can be defined as panes
within the frame definition itself. For example:

define frame <wizard-type> (<wizard-frame>)
                                           

...

pages (frame)

vector(frame.page-1, frame.page-2, frame.page-3);

end frame <wizard-type>
                       

The *name*, *supers*, and slot arguments are not evaluated. The values
of each of the options are evaluated.

Example
       

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
                                   

See also
        

`<simple-frame>`_

`<wizard-frame>`_

deiconify-frame
---------------

Generic function
''''''''''''''''

Summary
       

Displays a frame that has previously been iconified on screen.

Signature
         

deiconify-frame *frame* => ()

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

None

Description
           

Displays a frame that has previously been iconified on screen.

Example
       

The following example creates and displays a simple frame, then
iconifies it and deiconifies it.

define variable \*frame\* =
                           

make(<simple-frame>, title: "A frame",

layout: make(<button>));
                        

start-frame(\*frame\*);

iconify-frame(\*frame\*);

deiconify-frame(\*frame\*);
                           

See also
        

`destroy-frame`_

`exit-frame`_

`frame-icon`_

`iconify-frame`_

destroy-frame
-------------

Generic function
''''''''''''''''

Summary
       

Unmaps the specified frame and destroys it.

Signature
         

destroy-frame *frame* => ()

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

None

Description
           

Unmaps *frame* from the screen and destroys it. Generally, you should
not need to call this function explicitly, since `See
exit-frame`_ performs all necessary operations in the
correct order, including calling *destroy-frame* if the *destroy?*
argument to `exit-frame`_ is true.

See also
        

`deiconify-frame`_

`exit-frame`_

`<frame-destroyed-event>`_

`iconify-frame`_

`lower-frame`_

`raise-frame`_

dialog-apply-button
-------------------

Generic function
''''''''''''''''

Summary
       

Returns the Apply button in the specified dialog.

Signature
         

dialog-apply-button *dialog* => *apply-button*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *apply-button* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*.

Description
           

Returns the Apply button in *dialog*. As well as having OK and Cancel
buttons, many dialogs also have an Apply button that lets the user apply
the changes that have been made in the dialog, without removing the
dialog from the screen itself.

See also
        

`dialog-cancel-button`_

`dialog-apply-button-setter`_

`dialog-apply-callback`_

`dialog-help-button`_

dialog-apply-button-setter
--------------------------

Generic function
''''''''''''''''

Summary
       

Specifies the Apply button in the specified dialog.

Signature
         

dialog-apply-button-setter *apply-button dialog* => *apply-button*

Arguments
         

-  *apply-button* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*.
-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *apply-button* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*.

Description
           

Specifies the Apply button in *dialog*. As well as having OK and Cancel
buttons, many dialogs also have an Apply button that lets the user apply
the changes that have been made in the dialog, without removing the
dialog from the screen itself.

See also
        

`dialog-cancel-button`_

`dialog-apply-button`_

`dialog-apply-callback`_

`dialog-help-button`_

dialog-apply-callback
---------------------

Generic function
''''''''''''''''

Summary
       

Returns the callback invoked when the Apply button is clicked in the
specified dialog.

Signature
         

dialog-apply-callback *dialog* => *callback*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *callback* An instance of type *false-or(type-nonfunctional>, `See
   <command>`_))*.

Description
           

Returns the callback invoked when the Apply button is clicked in
*dialog*. As well as having OK and Cancel buttons, many dialogs also
have an Apply button that lets the user apply the changes that have been
made in the dialog, without removing the dialog from the screen itself.

*Note:* If you supply *#f* as the callback, then the button does not
appear.

See also
        

`dialog-cancel-button`_

`dialog-apply-button`_

`dialog-apply-button-setter`_

`dialog-help-button`_

dialog-back-button
------------------

Generic function
''''''''''''''''

Summary
       

Returns the Back button in the specified multi-page dialog.

Signature
         

dialog-back-button *dialog* => *back-button*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *back-button* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*.

Description
           

Returns the Back button in *dialog*. This is most useful in multi-page
dialogs such as property frames and wizard frames, which typically have
Back and Next buttons that let the user navigate forward and backward
through the sequence of pages that comprise the dialog.

See also
        

`dialog-back-button-setter`_

`dialog-back-callback`_

`dialog-exit-button`_

`dialog-help-button`_

dialog-back-button-setter
-------------------------

Generic function
''''''''''''''''

Summary
       

Specifies the Back button in the specified multi-page dialog.

Signature
         

dialog-back-button-setter *back-button dialog* => *back-button*

Arguments
         

-  *back-button* An instance of type `See
   <button> <gadgets.htm#20680>`_.
-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *back-button* An instance of type `See
   <button> <gadgets.htm#20680>`_.

Description
           

Specifies the Back button in *dialog*. This is most useful in wizard
frames, which typically have Back and Next buttons that let the user
navigate forward and backward through the sequence of pages that
comprise the dialog.

See also
        

`dialog-back-button`_

`dialog-back-callback`_

`dialog-exit-button-setter`_

`dialog-help-button`_

dialog-back-callback
--------------------

Generic function
''''''''''''''''

Summary
       

Returns the callback invoked when the Back button is clicked in the
specified multi-page dialog.

Signature
         

dialog-apply-callback *dialog* => *callback*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *callback* An instance of type *false-or(type-nonfunctional>, `See
   <command>`_))*.

Description
           

Returns the callback invoked when the Back button is clicked in *dialog*
. This is most useful in wizard frames, which typically have Back and
Next buttons that let the user navigate forward and backward through the
sequence of pages that comprise the dialog.

*Note:* If you do not explicitly supply this callback, the previous page
in the sequence for the multi-page dialog is displayed when the Back
button is clicked. Specifying your own callback gives you flexibility in
describing how the user can navigate through the sequence of pages in
the dialog.

See also
        

`dialog-back-button`_

`dialog-back-button-setter`_

`dialog-exit-callback`_

`dialog-help-button`_

dialog-cancel-button
--------------------

Generic function
''''''''''''''''

Summary
       

Returns the Cancel button in the specified dialog.

Signature
         

dialog-cancel-button *dialog* => *cancel-button*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *cancel-button* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*.

Description
           

Returns the Cancel button in *dialog*.

See also
        

`dialog-cancel-button-setter`_

`dialog-cancel-callback`_

`dialog-exit-button`_

`dialog-help-button`_

dialog-cancel-button-setter
---------------------------

Generic function
''''''''''''''''

Summary
       

Specifies the Cancel button in the specified dialog.

Signature
         

dialog-cancel-button-setter *cancel-button* *dialog*
 => *cancel-button*

Arguments
         

-  *cancel-button* An instance of type `See
   <button> <gadgets.htm#20680>`_.
-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *cancel-button* An instance of type `See
   <button> <gadgets.htm#20680>`_.

Description
           

Specifies the Cancel button in *dialog*.

Example
       

In the following example, a simple dialog frame is created, and then its
cancel button is redefined before the dialog is displayed on screen.

define variable \*dialog\*
                          

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

max-width: $fill), \*dialog\*);
                               

start-frame(\*dialog\*);

See also
        

`dialog-cancel-button`_

`dialog-cancel-callback`_

`dialog-exit-button-setter`_

`dialog-help-button-setter`_

dialog-cancel-callback
----------------------

Generic function
''''''''''''''''

Summary
       

Returns the function invoked when the cancel button is clicked in the
specified dialog.

Signature
         

dialog-cancel-callback *dialog* => *callback*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *callback* An instance of type *false-or(type-union(`See
   <command>`_, <function>))*.

Library
       

duim-frames

Module
      

duim-frames

Description
           

Returns the function invoked when the cancel button is clicked in
*dialog*. This defaults to `cancel-dialog`_.

See also
        

`cancel-dialog`_

`dialog-cancel-button`_

`dialog-cancel-button-setter`_

`dialog-exit-callback`_

`dialog-help-callback`_

dialog-cancel-callback-setter
-----------------------------

Generic function
''''''''''''''''

Summary
       

Sets the function invoked when the cancel button is clicked in the
specified dialog.

Signature
         

dialog-cancel-callback-setter *callback* *dialog* => *callback*

Arguments
         

-  *callback* An instance of type *false-or(`See
   <command>`_, <function>)*. Default value: `See
   cancel-dialog`_.
-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *callback* An instance of type *false-or(`See
   <command>`_, <function>)*.

Library
       

duim-frames

Module
      

duim-frames

Description
           

Sets the function invoked when the cancel button is clicked in *dialog*
.

See also
        

`dialog-cancel-button`_

`dialog-cancel-button-setter`_

`dialog-exit-callback`_

`dialog-help-callback`_

dialog-current-page
-------------------

Generic function
''''''''''''''''

Summary
       

Returns the current page in the specified multi-page dialog.

Signature
         

dialog-current-page *dialog* => *page*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *page* An instance of type *false-or(`See
   <page> <gadgets.htm#93333>`_)*.

Library
       

duim-frames

Module
      

duim-frames

Description
           

Returns the current page in *dialog*.

See also
        

`dialog-current-page-setter`_

dialog-current-page-setter
--------------------------

Generic function
''''''''''''''''

Summary
       

Sets the current page in the specified multi-page dialog.

Signature
         

dialog-current-page-setter *page* *dialog* => *page*

Arguments
         

-  *page* An instance of type *`<page> <gadgets.htm#93333>`_*.
-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *page* An instance of type *`<page> <gadgets.htm#93333>`_*.

Library
       

duim-frames

Module
      

duim-frames

Description
           

Sets the current page in *dialog*.

See also
        

`dialog-current-page`_

dialog-exit-button
------------------

Generic function
''''''''''''''''

Summary
       

Returns the Exit button in the specified dialog.

Signature
         

dialog-exit-button *dialog* => *exit-button*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *exit-button* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*.

Description
           

Returns the Exit button in *dialog*. The Exit button is commonly found
in multi-page dialogs, where the user is given the option to exit the
sequence at any point (as well as navigate through the sequence using
Next and Back buttons).

See also
        

`dialog-cancel-button`_

`dialog-exit-button-setter`_

`dialog-exit-enabled?`_

`dialog-exit-callback`_

`dialog-help-button`_

dialog-exit-button-setter
-------------------------

Generic function
''''''''''''''''

Summary
       

Specifies the Exit button in the specified dialog.

Signature
         

dialog-exit-button-setter *exit-button* *dialog* => *exit-button*

Arguments
         

-  *exit-button* An instance of type `See
   <button> <gadgets.htm#20680>`_.
-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *exit-button* An instance of type `See
   <button> <gadgets.htm#20680>`_.

Description
           

Sets the Exit button in *dialog*. The Exit button is commonly found in
multi-page dialogs, where the user is given the option to exit the
sequence at any point (as well as navigate through the sequence using
Next and Back buttons).

Example
       

In the following example, a simple dialog frame is created, and then its
exit button is redefined before the dialog is displayed on screen.

define variable \*dialog\*
                          

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

max-width: $fill), \*dialog\*);
                               

start-frame(\*dialog\*);

See also
        

`dialog-cancel-button-setter`_

`dialog-exit-button`_

`dialog-exit-enabled?`_

`dialog-exit-callback`_

`dialog-help-button-setter`_

dialog-exit-callback
--------------------

Generic function
''''''''''''''''

Summary
       

Returns the callback invoked when the Exit button is clicked in the
specified dialog.

Signature
         

dialog-exit-callback *dialog* => *callback*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *callback* An instance of type *false-or(type-union(`See
   <command>`_, <function>))*. Default value: `See
   exit-dialog`_.

Library
       

duim-frames

Module
      

duim-frames

Description
           

Returns the callback invoked when the Exit button is clicked in *dialog*
. The Exit button is commonly found in multi-page dialogs, where the
user is given the option to exit the sequence at any point (as well as
navigate through the sequence using Next and Back buttons).

See also
        

`dialog-cancel-callback`_

`dialog-exit-button`_

`dialog-exit-button-setter`_

`dialog-exit-callback-setter`_

`dialog-help-callback`_

dialog-exit-callback-setter
---------------------------

Generic function
''''''''''''''''

Summary
       

Sets the callback invoked when the Exit button is clicked in the
specified dialog.

Signature
         

dialog-exit-callback *callback* *dialog* => *callback*

Arguments
         

-  *callback* An instance of type *false-or(type-union(`See
   <command>`_, <function>))*.
-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *callback* An instance of type *false-or(type-union(`See
   <command>`_, <function>))*.

Library
       

duim-frames

Module
      

duim-frames

Description
           

Sets the callback invoked when the Exit button is clicked in *dialog*.
The Exit button is commonly found in multi-page dialogs, where the user
is given the option to exit the sequence at any point (as well as
navigate through the sequence using Next and Back buttons).

If you do not supply this callback, then the default behavior is to quit
the dialog when the Exit button is clicked. This is normally the action
that you will want. Specifying your own callback gives you flexibility
in describing other actions to be performed when the dialog is exited.
In addition, supplying *#f* means that no Exit button is displayed at
all.

See also
        

`dialog-cancel-callback-setter`_

`dialog-exit-button`_

`dialog-exit-button-setter`_

`dialog-exit-callback`_

`dialog-help-callback`_

dialog-exit-enabled?
--------------------

Generic function
''''''''''''''''

Summary
       

Returns true if the Exit button has been enabled for the specified
dialog.

Signature
         

dialog-exit-enabled? *dialog* => *enabled?*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *enabled?* An instance of type *<boolean>*.

Description
           

Returns true if the Exit button has been enabled for *dialog*. The Exit
button is commonly found in multi-page dialogs, where the user is given
the option to exit the sequence at any point (as well as navigate
through the sequence using Next and Back buttons).

See also
        

`dialog-exit-button`_

`dialog-exit-button-setter`_

`dialog-exit-enabled?-setter`_

`dialog-exit-callback`_

dialog-exit-enabled?-setter
---------------------------

Generic function
''''''''''''''''

Summary
       

Enables or disables the Exit button for the specified dialog.

Signature
         

dialog-exit-enabled?-setter *enabled?* *dialog* => *enabled?*

Arguments
         

-  *enabled?* An instance of type *<boolean>*.
-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *enabled?* An instance of type *<boolean>*.

Description
           

Enables or disables the Exit button for *dialog*. The Exit button is
commonly found in multi-page dialogs, where the user is given the option
to exit the sequence at any point (as well as navigate through the
sequence using Next and Back buttons).

Example
       

In this example, a dialog is created, and then its exit button is
disabled. When displayed on the screen, the exit button is grayed out
and you cannot click on it.

define variable \*dialog\* =
                            

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
     

dialog-exit-enabled?-setter(#f, \*dialog\*);

start-frame(\*dialog\*);

See also
        

`dialog-exit-button`_

`dialog-exit-button-setter`_

`dialog-exit-enabled?`_

`dialog-exit-callback`_

<dialog-frame>
--------------

Open abstract instantiable class
''''''''''''''''''''''''''''''''

Summary
       

The class of dialog frames.

Superclasses
            

`<simple-frame>`_

Init-keywords
             

-  *mode:* An instance of type *one-of("modal", #"modeless",
   #"system-modal")*. Default value: *#"modal"*.
-  *exit-callback:* An instance of type *false-or(type-union(`See
   <command>`_, <function>))*. Default value: `See
   exit-dialog`_.
-  *exit-button:* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*. Default value: *#f*.
-  *exit-enabled?:* An instance of type *<boolean>*. Default value:
   *#t*.

*cancel-callback*
                 

-  An instance of type *false-or(type-union(`See
   <command>`_, <function>))*. Default value: `See
   cancel-dialog`_.
-  *cancel-button:* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*. Default value: *#f*.
-  *help-callback:* An instance of type *false-or(type-union(`See
   <command>`_, <function>))*. Default value: *#f*.
-  *help-button:* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*. Default value: *#f*.

*exit-buttons-position:*
                        

-  An instance of type *one-of(#"top", #"bottom", #"left", #"right")*.
   Default value: *#"bottom"*.
-  *pages:* An instance of type *false-or(<sequence>)*. Default value:
   *#f*.

*page-changed-callback:*
                        

-  An instance of type *false-or(<function>)*. Default value: *#f*.

Description
           

The class of dialog frames. These frames let you create dialog boxes for
use in your applications. All buttons in a dialog frame are
automatically made the same size, and are placed at the bottom of the
dialog by default. When at the bottom of the dialog, buttons are
right-aligned.

A typical dialog
                

.. figure:: images/frames-2.png
   :align: center
   :alt: 

.. figure:: images/frames-4.png
   :align: center
   :alt: 
By default, all dialogs are modal, that is, when displayed, they take
over the entire application thread, preventing the user from using any
other part of the application until the dialog has been removed from the
screen. To create a modeless dialog (that is, one that can remain
displayed on the screen while the user interacts with the application in
other ways) you should set the *mode:* keyword to *#"modeless"*. Note,
however, that you should not normally need to do this: if you need to
create a modeless dialog, then you should consider using a normal DUIM
frame, rather than a dialog frame.

The init-keywords *exit-button:*, and *cancel-button:* specify the exit
and cancel buttons in the dialog. The user clicks on the exit button to
dismiss the dialog and save any changes that have been made as a result
of editing the information in the dialog. The user clicks on the cancel
button in order to dismiss the dialog and discard any changes that have
been made.

In addition, the *exit-callback:* and *cancel-callback:* init-keywords
specify the callback that is invoked when the Exit or Cancel buttons in
the dialog are clicked on. These both default to the appropriate
function for each button, but you have the flexibility to specify an
alternative if you wish. If you do not require a Cancel button in your
dialog, specify *cancel-callback: #f*. Similarly, specify
*exit-callback: #f* if you do not require an Exit button.

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

The *exit-enabled?:* init-keyword is used to specify whether the exit
button on the dialog is enabled or not. If *#f*, then the exit button
is displayed on the dialog, but it is grayed out.

The *help-button:* init-keyword specifies the help button in the dialog.
Note that, in contrast to the exit and cancel buttons, specifying the
button gadget to use in a dialog determines its presence in the dialog:
it is not possible to define a help button and then only display it in
certain circumstances. You are strongly encouraged to provide a help
button in all but the most trivial dialogs.

The *help-callback:* init-keyword defines a callback function that is
invoked when the help button is clicked. This should normally display a
context-sensitive help topic from the help file supplied with the
application, although you might also choose to display an alert box with
the relevant information.

The *exit-buttons-position:* init-keyword defines the position in the
dialog that the exit and cancel buttons occupy (and any other standard
buttons, if they have been specified). By default, buttons are placed
where the interface guidelines for the platform recommend, and this
position is encouraged in most interface design guidelines. Usually,
this means that buttons are placed at the bottom of the dialog. Less
commonly, buttons may also be placed on the right side of the dialog.
Buttons are not normally placed at the top or on the left of the dialog,
though this is possible if desired.

The *pages:* init-keyword is used for multi-page dialogs such as
property frames and wizard frames. If used, it should be a sequence of
elements, each of which evaluates to an instance of a page.

The *page-changed-callback:* is a callback function that is invoked when
a different page in a multi-page dialog is displayed.

Operations
          

The following operations are exported from the *DUIM-Frames* module.

`cancel-dialog`_ `See
dialog-cancel-button`_
 `dialog-cancel-button-setter`_ `See
dialog-cancel-callback`_ `See
dialog-cancel-callback-setter`_ `See
dialog-exit-button`_
 `dialog-exit-button-setter`_ `See
dialog-exit-callback`_ `See
dialog-exit-callback-setter`_ `See
dialog-exit-enabled?`_ `See
dialog-exit-enabled?-setter`_ `See
dialog-exit-callback`_ `See
dialog-exit-callback-setter`_ `See
dialog-help-button`_ `See
dialog-help-button-setter`_ `See
dialog-help-callback`_ `See
exit-dialog`_ `start-dialog`_

Example
       

The following example creates and displays a simple dialog that contains
only an exit button, cancel button, and help button, and assigns a
callback to the help button.

define variable \*dialog\*
                          

= make(<dialog-frame>,

exit-button?: #t,

cancel-button?: #t,

help-callback:

method (gadget)

notify-user (format-to-string

("Here is some help",

gadget))

end);
     

start-frame(\*dialog\*);

See also
        

`cancel-dialog`_

`exit-dialog`_

`<property-frame>`_

`<simple-frame>`_

`<wizard-frame>`_

dialog-help-button
------------------

Generic function
''''''''''''''''

Summary
       

Returns the Help button in the specified dialog.

Signature
         

dialog-help-button *dialog* => *help-button*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *help-button* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*.

Description
           

Returns the Help button in *dialog*. Many dialogs contain a Help button
that, when clicked, displays a relevant topic from the online help
system for the application.

See also
        

`dialog-cancel-button`_

`dialog-exit-button`_

`dialog-help-button-setter`_

`dialog-help-callback`_

dialog-help-button-setter
-------------------------

Generic function
''''''''''''''''

Summary
       

Specifies the Help button in the specified dialog.

Signature
         

dialog-help-button-setter *help-button* *dialog* => *help-button*

Arguments
         

-  *help-button* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*.
-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *help-button* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*

Description
           

Specifies the Help button in *dialog*. Many dialogs contain a Help
button that, when clicked, displays a relevant topic from the online
help system for the application.

Example
       

In the following example, a simple dialog frame is created, and then its
help button is redefined before the dialog is displayed on screen.

define variable \*dialog\*
                          

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

max-width: $fill), \*dialog\*);
                               

start-frame(\*dialog\*);

See also
        

`dialog-cancel-button-setter`_

`dialog-exit-button-setter`_

`dialog-help-button`_

`dialog-help-callback`_

dialog-help-callback
--------------------

Generic function
''''''''''''''''

Summary
       

Returns the callback invoked when the Help button is clicked in the
specified dialog.

Signature
         

dialog-help-callback *dialog* => *help-callback*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *help-callback* An instance of type *false-or(type-union(`See
   <command>`_, <function>))*.

Library
       

duim-frames

Module
      

duim-frames

Description
           

Returns the callback invoked when the Help button is clicked in *dialog*
. Many dialogs contain a Help button that, when clicked, displays a
relevant topic from the online help system for the application.

*Note:* You must specify this callback in order to create a Help button
in any dialog. If the callback is *#f*, then there will be no Help
button present in the dialog.

See also
        

`dialog-cancel-callback`_

`dialog-exit-callback`_

`dialog-help-button`_

`dialog-help-button-setter`_

dialog-next-button
------------------

Generic function
''''''''''''''''

Summary
       

Returns the Next button in the specified multi-page dialog.

Signature
         

dialog-next-button *dialog* => *next-button*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *next-button* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*.

Description
           

Returns the Next button in *dialog*. This is most useful in multi-page
dialogs such as property frames and wizard frames, which typically have
Back and Next buttons that let the user navigate forward and backward
through the sequence of pages that comprise the dialog.

See also
        

`dialog-back-button`_

`dialog-exit-button`_

`dialog-next-button-setter`_

`dialog-next-callback`_

dialog-next-button-setter
-------------------------

Generic function
''''''''''''''''

Summary
       

Specifies the Next button in the specified multi-page dialog.

Signature
         

dialog-next-button-setter *next-button dialog* => *next-button*

Arguments
         

-  *next-button* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*.
-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *next-button* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*.

Description
           

Specifies the Next button in *dialog*. This is most useful in
multi-page dialogs such as property frames and wizard frames, which
typically have Back and Next buttons that let the user navigate forward
and backward through the sequence of pages that comprise the dialog.

See also
        

`dialog-back-button-setter`_

`dialog-exit-button`_

`dialog-next-button`_

`dialog-next-callback`_

dialog-next-callback
--------------------

Generic function
''''''''''''''''

Summary
       

Returns the callback invoked when the Next button is clicked in the
specified multi-page dialog.

Signature
         

dialog-apply-callback *dialog* => *callback*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *callback* An instance of type *false-or(type-union(`See
   <command>`_, <function>))*.

Description
           

Returns the callback invoked when the Next button is clicked in *dialog*
. This is most useful in multi-page dialogs such as property frames and
wizard frames, which typically have Back and Next buttons that let the
user navigate forward and backward through the sequence of pages that
comprise the dialog.

*Note:* If you do not explicitly supply this callback, the next page in
the sequence for the multi-page dialog is displayed when the Next button
is clicked. Specifying your own callback gives you flexibility in
describing how the user can navigate through the sequence of pages in
the dialog.

The default value for this callback is `See
move-to-next-page`_.

See also
        

`dialog-back-button`_

`dialog-exit-callback`_

`dialog-next-button`_

`dialog-next-button-setter`_

`move-to-next-page`_

dialog-next-enabled?
--------------------

Generic function
''''''''''''''''

Summary
       

Returns true if the Next button has been enabled for the specified
multi-page dialog.

Signature
         

dialog-next-enabled? *dialog* => *enabled?*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *enabled?* An instance of type *<boolean>*.

Description
           

Returns true if the Next button has been enabled for *dialog*. This
button is most useful in multi-page dialogs such as property frames and
wizard frames, which typically have Back and Next buttons that let the
user navigate forward and backward through the sequence of pages that
comprise the dialog.

See also
        

`<dialog-frame>`_

`dialog-next-button`_

`dialog-next-button-setter`_

`dialog-next-enabled?-setter`_

`dialog-next-callback`_

dialog-next-enabled?-setter
---------------------------

Generic function
''''''''''''''''

Summary
       

Enables or disables the Next button for the specified multi-page dialog.

Signature
         

dialog-next-enabled?-setter *enabled?* *dialog* => *enabled?*

Arguments
         

-  *enabled?* An instance of type *<boolean>*.
-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *enabled?* An instance of type *<boolean>*.

Description
           

Enables or disables the Next button for *dialog*. This button is most
useful in multi-page dialogs such as property frames and wizard frames,
which typically have Back and Next buttons that let the user navigate
forward and backward through the sequence of pages that comprise the
dialog.

It is useful to be able to enable and disable the Next button at any
point in order to ensure that the user supplies all necessary
information before proceeding to the next page of the dialog. You can do
this by testing to see if the information on the page has been specified
with `dialog-page-complete?`_, and then enabling
or disabling the Next button as appropriate.

Example
       

See also
        

`dialog-next-button`_

`dialog-next-button-setter`_

`dialog-next-callback`_

`dialog-next-enabled?`_

dialog-next-page
----------------

Generic function
''''''''''''''''

Summary
       

Returns the next page in sequence for the specified multi-page dialog.

Signature
         

dialog-next-page *dialog* => *next-page*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *next-page* An instance of type *false-or(`See
   <page> <gadgets.htm#93333>`_)*.

Description
           

Returns the next page in sequence for *dialog*. This is for use in
multi-page dialogs such as property frames and wizard frames, which
typically have Back and Next buttons that let the user navigate forward
and backward through the sequence of pages that comprise the dialog.

The default method for the Next button in *dialog* uses the value of
this function. When the Next button is clicked, the current page is set
to the next logical page in the sequence, but you are free to
dynamically change it as the state of the dialog changes.

See also
        

`dialog-next-button`_

`dialog-next-button-setter`_

`dialog-next-callback`_

`dialog-next-page-setter`_

`dialog-previous-page`_

dialog-next-page-setter
-----------------------

Generic function
''''''''''''''''

Summary
       

Specifies the next page in sequence for the specified multi-page dialog.

Signature
         

dialog-next-page-setter *next-page* *dialog* => *next-page*

Arguments
         

-  *next-page* An instance of type *false-or(`See
   <page> <gadgets.htm#93333>`_)*.
-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *next-page* An instance of type *false-or(`See
   <page> <gadgets.htm#93333>`_)*.

Description
           

Specifies the next page in sequence for *dialog*. This is for use in
multi-page dialogs such as property frames and wizard frames, which
typically have Back and Next buttons that let the user navigate forward
and backward through the sequence of pages that comprise the dialog.

The default method for the Next button in *dialog* uses the value of
this function. When the Next button is clicked, the current page is set
to the next logical page in the sequence, but you are free to
dynamically change it as the state of the dialog changes.

See also
        

`dialog-next-button`_

`dialog-next-button-setter`_

`dialog-next-callback`_

`dialog-next-page`_

`dialog-previous-page-setter`_

dialog-page-changed-callback
----------------------------

Generic function
''''''''''''''''

Summary
       

Returns the page-changed callback of the specified multi-page dialog.

Signature
         

dialog-page-changed-callback *dialog* => *callback*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *callback* An instance of type *false-or(type-union(`See
   <command>`_, <function>))*.

Description
           

Returns the page-changed-callback of *dialog*. This is the callback
function used to test whether the information in the current page of
*dialog* has changed. This callback is useful when using multi-page
dialogs, as a test that can be performed before the next page of the
dialog is displayed.

See also
        

`<dialog-frame>`_

`dialog-page-changed-callback-setter`_

`<property-frame>`_

`<wizard-frame>`_

dialog-page-changed-callback-setter
-----------------------------------

Generic function
''''''''''''''''

Summary
       

Sets the page-changed callback of the specified multi-page dialog.

Signature
         

dialog-page-changed-callback-setter *callback* *dialog
* => *callback*

Arguments
         

-  *callback* An instance of type *false-or(type-union(`See
   <command>`_, <function>))*.
-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *callback* An instance of type *false-or(type-union(`See
   <command>`_, <function>))*.

Description
           

Sets the page-changed-callback of *dialog*. This is the callback
function used to test whether the information in the current page of
*dialog* has changed. This callback is useful when using multi-page
dialogs, as a test that can be performed before the next page of the
dialog is displayed.

See also
        

`<dialog-frame>`_

`dialog-page-changed-callback`_

`<property-frame>`_

`<wizard-frame>`_

dialog-page-complete?
---------------------

Generic function
''''''''''''''''

Summary
       

Returns true if all the information required on the current page of the
specified multi-page dialog has been specified.

Signature
         

dialog-page-complete? *dialog* => *complete?*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *complete?* An instance of type *<boolean>*.

Description
           

Returns true if all the information required on the current page in
*dialog* has been specified by the user. This generic function has two
uses:

-  It can be used within wizards to test whether all the necessary
   information has been supplied, before moving on to the next page of
   the wizard.
-  It can be used within property pages to test whether all the
   necessary information has been supplied, before allowing the user to
   apply any changes.

See also
        

`dialog-page-complete?-setter`_

dialog-page-complete?-setter
----------------------------

Generic function
''''''''''''''''

Summary
       

Sets the slot that indicates all the information required on the current
page of the specified multi-page dialog has been specified.

Signature
         

dialog-page-complete? *complete?* *dialog* => *complete?*

Arguments
         

-  *complete?* An instance of type *<boolean>*.
-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *complete?* An instance of type *<boolean>*.

Description
           

Sets the slot that indicates all the information required on the current
page in *dialog* has been specified by the user. This generic function
has two uses:

-  It can be used within wizards to indicate that the necessary
   information has been supplied, so that the next page of the wizard
   can be displayed safely.
-  It can be used within property pages to indicate that the necessary
   information has been supplied, so that the user can apply any
   changes.

See also
        

`dialog-page-complete?`_

dialog-pages
------------

Generic function
''''''''''''''''

Summary
       

Returns the pages of the specified multi-page dialog.

Signature
         

dialog-pages *dialog* => *pages*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *pages* An instance of type *limited(<sequence>, of: `See
   <page> <gadgets.htm#93333>`_)*.

Description
           

Returns the pages of *dialog*. Each of the items in sequence is an
instance of `<page> <gadgets.htm#93333>`_.

See also
        

`<dialog-frame>`_

`dialog-pages-setter`_

`<property-frame>`_

`<wizard-frame>`_

dialog-pages-setter
-------------------

Generic function
''''''''''''''''

Summary
       

Sets the pages of the specified multi-page dialog.

Signature
         

dialog-pages-setter *pages* *dialog* => *pages*

Arguments
         

-  *pages* An instance of type *limited(<sequence>, of: `See
   <page> <gadgets.htm#93333>`_)*.
-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *pages* An instance of type *limited(<sequence>, of: `See
   <page> <gadgets.htm#93333>`_)*.

Description
           

Sets the pages of *dialog*. Each of the items in sequence must be an
instance of `<page> <gadgets.htm#93333>`_.

See also
        

`<dialog-frame>`_

`dialog-pages`_

`<property-frame>`_

`<wizard-frame>`_

dialog-previous-page
--------------------

Generic function
''''''''''''''''

Summary
       

Returns the previous page in sequence for the specified multi-page
dialog.

Signature
         

dialog-previous-page *dialog* => *previous-page*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *previous-page* An instance of type *false-or(`See
   <page> <gadgets.htm#93333>`_)*.

Description
           

Returns the previous page in sequence for *dialog*. This is for use in
multi-page dialogs such as property frames and wizard frames, which
typically have Back and Next buttons that let the user navigate forward
and backward through the sequence of pages that comprise the dialog.

The default method for the Back button in *dialog* uses the value of
this function. When the Back button is clicked, the current page is set
to the previous logical page in the sequence, but you are free to
dynamically change it as the state of the dialog changes.

See also
        

`dialog-back-button`_

`dialog-back-button-setter`_

`dialog-back-callback`_

`dialog-next-page`_

`dialog-previous-page-setter`_

dialog-previous-page-setter
---------------------------

Generic function
''''''''''''''''

Summary
       

Specifies the previous page in sequence for the specified multi-page
dialog.

Signature
         

dialog-previous-page-setter *previous-page* *dialog*
 => *previous-page*

Arguments
         

-  *previous-page* An instance of type *false-or(`See
   <page> <gadgets.htm#93333>`_)*.
-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *previous-page* An instance of type *false-or(`See
   <page> <gadgets.htm#93333>`_)*.

Description
           

Specifies the previous page in sequence for *dialog*. This is for use
in multi-page dialogs such as property frames and wizard frames, which
typically have Back and Next buttons that let the user navigate forward
and backward through the sequence of pages that comprise the dialog.

The default method for the Back button in *dialog* uses the value of
this function. When the Back button is clicked, the current page is set
to the previous logical page in the sequence, but you are free to
dynamically change it as the state of the dialog changes.

See also
        

`dialog-back-button`_

`dialog-back-button-setter`_

`dialog-back-callback`_

`dialog-next-page-setter`_

`dialog-previous-page`_

display-progress-note
---------------------

Generic function
''''''''''''''''

Summary
       

Displays the specified progress note.

Signature
         

display-progress-note *framem* *progress-note* => ()

Arguments
         

-  *framem* An instance of type `See
   <frame-manager> <silica.htm#32466>`_.
-  *progress-note* An instance of type *<progress-note>*.

Values
      

None

Description
           

Displays the specified *progress-note* in the frame managed by *framem*
.

doc-command-table-menu-commmands
--------------------------------

event-destroy-frame?
--------------------

Generic function
''''''''''''''''

Summary
       

Returns information about the frame was destroyed in the specified
event.

Signature
         

event-destroy-frame? *event* => *destroyed?*

Arguments
         

-  *event* An instance of type *`See
   <frame-exit-event>`_*.

Values
      

-  *destroyed?* An instance of type *<boolean>*.

Description
           

Returns information about the frame was destroyed in *event*.

See also
        

*`<frame-exit-event>`_*

event-status-code
-----------------

Generic function
''''''''''''''''

Summary
       

Returns the status code of the specified event.

Signature
         

event-status-code *event* => *code*

Arguments
         

-  *event* An instance of type *`See
   <frame-exited-event>`_*.

Values
      

-  *code* An instance of type *false-or(<integer>)*.

Description
           

Returns the status code of *event*.

See also
        

*`<frame-exited-event>`_*

execute-command
---------------

Generic function
''''''''''''''''

Summary
       

Executes a command for the specified frame.

Signature
         

execute-command *command* *frame* => #rest *values*

Arguments
         

-  *command* An instance of type `<command>`_.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *values* Instances of type *<object>*.

Description
           

Executes *command* for *frame*. The values returned are those values
returned as a result of evaluating the command function of *command*.

exit-dialog
-----------

Generic function
''''''''''''''''

Summary
       

Exits the specified dialog.

Signature
         

exit-dialog *dialog* #key *destroy?* => ()

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.
-  *destroy?* An instance of type *<boolean>*. Default value: *#t*.

Values
      

None

Description
           

Exits *dialog*, recording any changes to the information displayed in
the dialog that have been made by the user.

This is the default callback used for the exit button in a dialog. This
is the button that is typically labeled *OK*.

If *destroy?* is *#t*, then dialog is destroyed.

Example
       

The following example defines a button, *\*yes-button\**, that calls
*exit-dialog* as its activate-callback. This button is then used in a
dialog that simply replaces the standard exit button for the newly
defined dialog. Note that the example assumes the existence of a similar
*\*no-button\** to replace the cancel button.

define variable \*yes-button\*
                              

= make(<push-button>, label: "Yes",

activate-callback: exit-dialog,

max-width: $fill);
                  

define variable \*dialog\*
                          

= make(<dialog-frame>,

exit-button?: #f,

cancel-button?: #f,

layout: vertically

(x-alignment: #"center",

y-spacing: 5)

make(<label>,

label: "Here is a label");

horizontally (x-spacing: 2)

\*yes-button\*;

\*no-button\*;

end

end);
     

start-frame(\*dialog\*);

See also
        

`cancel-dialog`_

`<dialog-frame>`_

`start-dialog`_

exit-frame
----------

Generic function
''''''''''''''''

Summary
       

Unmaps the specified frame destroying it required.

Signature
         

exit-frame *frame* #key *destroy?* => ()

Arguments
         

-  *frame* An instance of type `<frame>`_.
-  *destroy?* An instance of type *<boolean>*. Default value: *#t*.

Values
      

None

Description
           

Unmaps *frame*, removing the associated sheet and its children from the
screen. If *destroy?* is true, then the frame is destroyed completely,
via a call to `destroy-frame`_.

If *destroy?* is *#t*, then dialog is destroyed.

Example
       

The following example creates a simple frame, then displays it and exits
it. You should run this code in the interactor, pressing the RETURN key
at the points indicated.

define variable \*frame\* =
                           

make(<simple-frame>, title: "A frame",

layout: make(<button>));*RETURN*
                                

start-frame(\*frame\*);*RETURN*

exit-frame(\*frame\*);*RETURN*

See also
        

`destroy-frame`_

`frame-can-exit?`_

`<frame-exited-event>`_

`<frame-exit-event>`_

`frame-mapped?-setter`_

`start-frame`_

find-frame
----------

Function
''''''''

Summary
       

Returns a frame of the specified type, creating one if necessary.

Signature
         

find-frame *frame-class* #rest *initargs* #key *create?* *activate?*
*own-thread?* *port* *frame-manager* *test* #all-keys => *frame*

Arguments
         

-  *frame-class* An instance of type *<object>*.
-  *initargs* An instance of type *<object>*.
-  *create?* An instance of type *<boolean>*. Default value: *#t*.
-  *activate?* An instance of type *<boolean>*. Default value: *#t*.
-  *own-thread?* An instance of type *<boolean>*. Default value: *#t*.
-  *port* An instance of type `<port> <silica.htm#11606>`_.
-  *frame-manager* An instance of type `See
   <frame-manager> <silica.htm#32466>`_.
-  *test* An instance of type *<function>*. Default value: *identity*.

Values
      

-  *frame* An instance of type `<frame>`_.

Description
           

This function creates a frame of the specified type if one does not
already exist, and then runs it, possibly in its own thread. If one
already exists, then it is selected.

The *frame-class* argument specifies the class of frame that is being
searched for. By default, if a match is not found, then an instance of
this class will be created.

The *init-args* supplied are the slot values that should be passed to
the instance of frame-class. Either an existing frame must be found that
has the specified slot values, or a new one will be created.

If *create?* is *#f*, then a new frame will not be created if it does
not already exist.

If *own-thread?* is true, the frame will run in its own thread if one is
created.

The *port* and *frame-manager* arguments specify a port and frame
manager which control the frame being searched for, or under the control
of which a new frame should be created.

If desired, you can supply a *test* which must evaluate to true for a
frame to match successfully.

See also
        

`<frame>`_

<frame>
-------

Open abstract class
'''''''''''''''''''

Summary
       

The base class of all frames.

Superclasses
            

*<object>*

Init-keywords
             

-  *owner:* An instance of type *false-or(<frame>)*. Default value:
   *#f*.
-  *mode:* An instance of type *one-of(#"modeless", #"modal",
   #"system-modal")*.Default value: *#"modeless"*.

*default-button:*
                 

-  An instance of type *false-or(`<button> <gadgets.htm#20680>`_)*.
   Default value: *#f*.
-  *x:* An instance of type *<integer>*.
-  *y:* An instance of type *<integer>*.
-  *width:* An instance of type *<integer>*.
-  *height:* An instance of type *<integer>*.

*disabled-commands:*
                    

-  An instance of type *<sequence>*.

*top-level-sheet:*
                  

-  An instance of type *false-or(`<sheet> <silica.htm#13118>`_)*.
   Default value: *#f*.
-  *layout:* An instance of type `<layout> <layouts.htm#13344>`_.
-  *icon:* An instance of type *false-or(`See
   <image> <dcs.htm#51234>`_)*.
-  *title:* An instance of type *false-or(<string>)*. Default value:
   *#f*.
-  *calling-frame:* An instance of type *<frame>*.
-  *state:* An instance of type *one-of(#"detached", #"unmapped",
   #"mapped", #"iconified")*. Default value: *#"detached"*.
-  *thread:* An instance of type *false-or(<thread>)*. Default value:
   *#f*.
-  *event-queue:* An instance of type *false-or(<event-queue>)*.
   Default value: *#f*.
-  *input-focus:* An instance of type *false-or(`See
   <sheet> <silica.htm#13118>`_)*. Default value: *#f*.
-  *foreground:* An instance of type *false-or(* `See
   <ink> <dcs.htm#15007>`_*)*.
-  *background:* An instance of type *false-or(* `See
   <ink> <dcs.htm#15007>`_*)*.
-  *text-style:* An instance of type *false-or(* `See
   <text-style> <dcs.htm#85385>`_*)*.
-  *palette:* An instance of type *false-or(* `See
   <palette> <dcs.htm#11494>`_*)*. Default value: *#f*.
-  *document:* An instance of type An instance of type
   *false-or(<object>)*. Default value: *#f*.
-  *resource-id:* An instance of type *false-or(<integer>)*.
-  *resizable?:* An instance of type *<boolean>*. Default value: *#t*.
-  *fixed-width?:* An instance of type *<boolean>*. Default value: *#f*
   .
-  *fixed-height?:* An instance of type *<boolean>*. Default value:
   *#f*.

Description
           

The class of all frames.

The *owner:* init-keyword is the parent of the frame.

The *mode:* init-keyword lets you specify the mode for the frame. By
default, frames are modeless, that is, they do not take over control of
the whole application when they are mapped, and the user can interact
with other frames in the application normally. Modal frames, on the
other hand, behave like a `<dialog-frame>`_,
restricting the user’s interaction with other frames in the application
until the modal frame has been dismissed.

The *default-button:* init-keyword is used to specify which button is
the default action for the frame. The default button is usually the one
whose callback is invoked by pressing the RETURN key.

The *x:*, *y:*, *width:* and *height:* init-keywords lets you specify
the initial size and position of the frame. The position is specified
using *x:* and *y:*, which represent the number of pixels from the top
left corner of the screen, and the *width:* and *height:* init-keywords
specify the initial size of the frame.

The *title:* init-keyword is used to specify a title for the frame.

The *state:* init-keyword is used to specify the initial state of the
frame. This describes whether the frame is mapped, whether it is
iconified, and so on. By default, new frames are detached.

By default, new frames run in their own thread. If desired, a frame can
be run in an existing thread by setting the *thread:* init-keyword to
the thread object concerned. For more information about threads, see the
manual *Library Reference: Core Features*.

As with threads, new frame run in their own event-queue by default. To
run the frame in an existing event-queue, use the *event-queue:*
init-keyword.

You can specify which sheet in the frame initially has the input-focus
using the *input-focus:* init-keyword. The input-focus dictates where
information can be typed by default.

The *foreground*,*:* *background:*, and *text-style:* init-keywords
describes the colors and fonts used in the frame.

Specify a palette for the frame using the *palette:* init-keyword.

Specify a resource-id for the frame using the *resource-id:*
init-keyword. This is a platform-specific ID or determining which
resource to use to fill in a frame.

The *resizable?:*, *fixed-width?:*, and *fixed-height?:* init-keywords
let you specify whether or not the user can resize the frame. If
*resizable?:* is *#t*, then the frame can be resized in either
direction; if it is #f, then it cannot be resized at all. In addition,
if *resizable?:* is *#t*, and one of *fixed-width?:* or
*fixed-height?:* is also *#t*, then the frame is resizable, but is
fixed in the appropriate direction. For example, if *resizable?:* is *#t
and fixed-height?:* is also *#t*, then only the width of the frame can
be resized.

Operations
          

The following operations are exported from the *DUIM-Frames* module.

`apply-in-frame`_ `See
call-in-frame`_ `See
command-enabled?`_
 `command-enabled?-setter`_ `See
deiconify-frame`_ `See
destroy-frame`_ `See
execute-command`_ `See
exit-frame`_ `frame?`_
 `frame-accelerators`_ `See
frame-accelerators-setter`_ `See
frame-can-exit?`_ `See
frame-default-button`_ `See
frame-default-button-setter`_ `See
frame-event-queue`_ `See
frame-icon`_ `See
frame-icon-setter`_ `See
frame-input-focus`_ `See
frame-input-focus-setter`_ `See
frame-mapped?`_ `See
frame-mapped?-setter`_ `See
frame-mode`_ `frame-owner`_
`frame-palette`_ `See
frame-palette-setter`_ `See
frame-position`_ `frame-size`_
`frame-state`_ `See
frame-thread`_ `frame-title`_
`frame-title-setter`_ `See
iconify-frame`_ `lower-frame`_
`layout-frame`_ `See
raise-frame`_ `redo-command`_
`set-frame-position`_ `See
set-frame-size`_ `See
layout-frame`_ `undo-command`_

The following operations are exported from the *DUIM-Sheets* module.

`beep <silica.htm#60172>`_ `display <silica.htm#56987>`_ `See
force-display <silica.htm#98525>`_ `See
frame-manager <silica.htm#15759>`_ `See
handle-event <silica.htm#94892>`_

The following operations are exported from the *DUIM-DCs* module.

`default-background <dcs.htm#19900>`_ `See
default-foreground <dcs.htm#40602>`_ `See
default-text-style <dcs.htm#95321>`_ `find-color <dcs.htm#33969>`_
`port <silica.htm#51457>`_ `queue-event <silica.htm#94000>`_
`synchronize-display <silica.htm#16560>`_ `See
top-level-sheet <silica.htm#60884>`_

frame?
------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified object is a frame.

Signature
         

frame? *object* => *frame?*

Arguments
         

-  *object* An instance of type *<object>*.

Values
      

-  *frame?* An instance of type *<boolean>*.

Description
           

Returns true if *object* is a frame. Use this generic function to test
that an object is a frame before carrying out frame-related operations
on it.

See also
        

`current-frame`_

`<frame>`_

frame-accelerators
------------------

Generic function
''''''''''''''''

Summary
       

Returns the keyboard accelerators defined for the specified frame.

Signature
         

frame-accelerators *frame* => *accelerators*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *accelerators* An instance of type *false-or(limited(<sequence>, of:
   `<gesture> <silica.htm#76256>`_))*.

Description
           

Returns the keyboard accelerators defined for *frame*.

See also
        

`frame-accelerators-setter`_

frame-accelerators-setter
-------------------------

Generic function
''''''''''''''''

Summary
       

Defines the keyboard accelerators for the specified frame.

Signature
         

frame-accelerators *accelerators* *frame* => *accelerators*

Arguments
         

-  *accelerators* An instance of type *false-or(limited(<sequence>, of:
   `<gesture> <silica.htm#76256>`_))*.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *accelerators* An instance of type *false-or(limited(<sequence>, of:
   `<gesture> <silica.htm#76256>`_))*.

Description
           

Defines the keyboard accelerators for *frame*.

See also
        

`frame-accelerators`_

frame-can-exit?
---------------

Open generic function
'''''''''''''''''''''

Summary
       

Returns true if the specified frame can be exited dynamically.

Signature
         

frame-can-exit? *frame* => *can-exit?*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *can-exit?* An instance of type *<boolean>*.

Description
           

Returns true if *frame* can be exited dynamically. You can add methods
to this generic function in order to allow the user to make a dynamic
decision about whether a frame should exit.

Example
       

define method frame-can-exit?
                             

(frame :: <abstract-test-frame>) =>

(can-exit? :: <boolean>)

notify-user("Really exit?",

frame: frame,

style: #"question")

end method frame-can-exit?;
                           

See also
        

`exit-frame`_

frame-command-table
-------------------

Generic function
''''''''''''''''

Summary
       

Returns the command table associated with the specified frame.

Signature
         

frame-command-table *frame* => *command-table*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *command-table* An instance of type *false-or(* `See
   <command-table>`_*)*.

Description
           

Returns the command table associated with *frame*.

See also
        

`frame-command-table-setter`_

frame-command-table-setter
--------------------------

Generic function
''''''''''''''''

Summary
       

Specifies the command table associated with the specified frame.

Signature
         

frame-command-table-setter *command-table* *frame
* => *command-table*

Arguments
         

-  *command-table* An instance of type `See
   <command-table>`_.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *command-table* An instance of type `See
   <command-table>`_.

Description
           

Specifies the command table associated with *frame*.

See also
        

`frame-command-table`_

<frame-created-event>
---------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events that indicate a frame has been created.

Superclasses
            

`<frame-event> <silica.htm#96445>`_

Init-keywords
             

None.

Description
           

The class of events that indicate a frame has been created. An instance
of this class is distributed to the frame when it is created. Only one
of these events is passed during the lifetime of any frame.

Operations
          

-  None.

See also
        

`<frame-destroyed-event>`_

`<frame-exited-event>`_

<frame-destroyed-event>
-----------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events that indicate a frame has been destroyed.

Superclasses
            

`<frame-event> <silica.htm#96445>`_

Init-keywords
             

None.

Description
           

The class of events that indicate a frame has been destroyed. An
instance of this class is distributed to the frame when it is destroyed.
Only one of these events is passed during the lifetime of any frame.

Operations
          

-  None.

See also
        

`destroy-frame`_

`<frame-created-event>`_

`<frame-exited-event>`_

frame-default-button
--------------------

Generic function
''''''''''''''''

Summary
       

Returns the default button associated with the specified frame.

Signature
         

frame-default-button *frame* => *default-button*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *default-button* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*.

Description
           

Returns the default button associated with *frame*.

See also
        

`frame-default-button-setter`_

frame-default-button-setter
---------------------------

Generic function
''''''''''''''''

Summary
       

Sets the default button associated with the specified frame.

Signature
         

frame-default-button-setter *default-button* *frame* => *default-button*

Arguments
         

-  *default-button* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *default-button* An instance of type *false-or(`See
   <button> <gadgets.htm#20680>`_)*.

Description
           

Sets the default button associated with *frame*.

See also
        

`frame-default-button`_

frame-event-queue
-----------------

Generic function
''''''''''''''''

Summary
       

Returns the event queue that the specified frame is running in.

Signature
         

frame-event-queue *frame* => *event-queue*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *event-queue* An instance of type *<event-queue>*.

Description
           

Returns the event queue that *frame* is running in.

See also
        

`<frame>`_

<frame-exited-event>
--------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events that indicate a frame has been exited.

Superclasses
            

`<frame-event> <silica.htm#96445>`_

Init-keywords
             

-  *status-code:* An instance of type *false-or(<integer>)*.

This class also inherits the *frame:* init-keyword from its superclass.

Description
           

Operations
          

-  None.

Example
       

The class of events that indicate a frame has been exited. An instance
of this class is distributed to the frame when it is exited. Only one of
these events is passed during the lifetime of any frame.

The *status-code:* init-keyword is used to pass a status code, if
desired. This code can be used to pass the reason that the frame was
exited.

See also
        

`<application-exited-event>`_

`exit-frame`_

`<frame-created-event>`_

`<frame-destroyed-event>`_

<frame-exit-event>
------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events distributed when a frame is about to exit.

Superclasses
            

`<frame-event> <silica.htm#96445>`_

Init-keywords
             

*destroy-frame?:*
                 

-  An instance of type *<boolean>*. Default value: *#f*.

Description
           

The class of events distributed when a frame is about to exit. Contrast
this with `<frame-exited-event>`_, which is
passed after the frame is exited.

The default method uses `frame-can-exit?`_ to
decide whether or not to exit.

If *destroy-frame?:* is *#t*, then the frame is destroyed.

Operations
          

-  None.

See also
        

`event-destroy-frame?`_

`frame-can-exit?`_

`<frame-exited-event>`_

<frame-focus-event>
-------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events distributed when a frame receives focus.

Superclasses
            

`<frame-event> <silica.htm#96445>`_

Init-keywords
             

None.

Description
           

The class of events distributed when a frame receives the mouse focus.

Operations
          

-  None.

See also
        

`event-destroy-frame?`_

`frame-can-exit?`_

`<frame-exited-event>`_

frame-fixed-height?
-------------------

Generic function
''''''''''''''''

Summary
       

Returns true if the height of the specified frame is not resizable.

Signature
         

frame-fixed-width? *frame* => *fixed-height?*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *fixed-height?* An instance of type *<boolean>*.

Description
           

Returns true if the height of *frame* is not resizable.

Example
       

See also
        

`frame-fixed-width?`_

`frame-resizable?`_

frame-fixed-width?
------------------

Generic function
''''''''''''''''

Summary
       

Returns true if the width of the specified frame is not resizable.

Signature
         

frame-fixed-width? *frame* => *fixed-width?*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *fixed-width?* An instance of type *<boolean>*.

Description
           

Returns true if the width of *frame* is not resizable.

Example
       

See also
        

`frame-fixed-height?`_

`frame-resizable?`_

frame-icon
----------

Generic function
''''''''''''''''

Summary
       

Returns the icon associated with the specified frame.

Signature
         

frame-icon *frame* => *icon*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *icon* An instance of type *false-or(`<image> <dcs.htm#51234>`_)*
   .

Description
           

Returns the icon associated with *frame*. This is the icon used to
represent the frame when it has been iconized. In Windows 95 and Windows
NT 4.0, this icon is also visible in the left hand corner of the title
bar of the frame when it is not iconized.

See also
        

`deiconify-frame`_

`frame-icon-setter`_

`iconify-frame`_

frame-icon-setter
-----------------

Generic function
''''''''''''''''

Summary
       

Specifies the icon associated with the specified frame.

Signature
         

frame-icon-setter *icon* *frame* => *icon*

Arguments
         

-  *icon* An instance of type *false-or(* `See
   <image> <dcs.htm#51234>`_*)*.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *icon* An instance of type *false-or(* `See
   <image> <dcs.htm#51234>`_*)*.

Description
           

Specifies the icon associated with *frame*. This icon is used when the
frame is iconified, and in Windows 95 and Windows NT 4.0 is also visible
on the left hand side of the title bar of the frame.

See also
        

`frame-icon`_

frame-input-focus
-----------------

Generic function
''''''''''''''''

Summary
       

Returns the sheet in the specified frame that has the input focus.

Signature
         

frame-input-focus *frame* => *focus*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *focus* An instance of type *false-or(`See
   <sheet> <silica.htm#13118>`_)*.

Description
           

Returns the sheet in *frame* that has the input focus.

See also
        

`frame-input-focus-setter`_

frame-input-focus-setter
------------------------

Generic function
''''''''''''''''

Summary
       

Sets which sheet in the specified frame has the input focus.

Signature
         

frame-input-focus-setter *focus frame* => *focus*

Arguments
         

-  *focus* An instance of type *false-or(`See
   <sheet> <silica.htm#13118>`_)*.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *focus* An instance of type *false-or(`See
   <sheet> <silica.htm#13118>`_)*.

Description
           

Sets which sheet in *frame* has the input focus.

See also
        

`frame-input-focus`_

frame-layout
------------

Generic function
''''''''''''''''

Summary
       

Returns the layout used in the specified frame.

Signature
         

frame-layout *frame* => *layout*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *layout* An instance of type *false-or(`See
   <sheet> <silica.htm#13118>`_)*.

Description
           

Returns the layout used in *frame*.

See also
        

`frame-layout-setter`_

frame-layout-setter
-------------------

Generic function
''''''''''''''''

Summary
       

Specifies the layout used in the specified frame.

Signature
         

frame-layout-setter *layout* *frame* => *layout*

Arguments
         

-  *layout* An instance of type *false-or(`See
   <sheet> <silica.htm#13118>`_)*.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *layout* An instance of type *false-or(`See
   <sheet> <silica.htm#13118>`_)*.

Description
           

Specifies the layout used in *frame*.

See also
        

`frame-layout`_

frame-mapped?
-------------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified frame is mapped.

Signature
         

frame-mapped? *frame* => *mapped?*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *mapped?* An instance of type *<boolean>*.

Description
           

Returns true if *frame* is mapped, that is, is currently displayed
on-screen. Note that a frame is considered to be mapped if it is
anywhere on the screen, even if it is not completely visible because
other windows are covering it either partially or completely, or if it
is iconized.

Example
       

The following example creates a simple frame, then displays it and exits
it. In between starting and exiting the frame, *frame-mapped?* is
called. You should run this code in the interactor, pressing the RETURN
key at the points indicated.

define variable \*frame\* =
                           

make(<simple-frame>, title: "A frame",

layout: make(<button>));*RETURN*
                                

start-frame(\*frame\*);*RETURN*

frame-mapped?(\*frame\*);*RETURN*
                                 

=> #t
     

exit-frame(\*frame\*);*RETURN*

frame-mapped?(\*frame\*);*RETURN*
                                 

=> #f
     

See also
        

`frame-mapped?-setter`_

<frame-mapped-event>
--------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events that indicate a frame has been mapped.

Superclasses
            

`<frame-event> <silica.htm#96445>`_

Init-keywords
             

-  None.

Description
           

The class of events that indicate a frame has been mapped, that is,
displayed on screen. An instance of this class is distributed whenever a
frame is mapped.

Operations
          

-  None.

Example
       

The following example defines a method that can inform you when an
instance of a class of frame you have defined is mapped.

define method handle-event
                          

(frame :: <my-frame>,

event :: <frame-mapped-event>) => ()

notify-user

(format-to-string("Frame %= mapped", frame))

end method handle-event;
                        

See also
        

`<frame-unmapped-event>`_

frame-mapped?-setter
--------------------

Generic function
''''''''''''''''

Summary
       

Maps or unmaps the specified frame.

Signature
         

frame-mapped?-setter *mapped?* *frame* => *mapped?*

Arguments
         

-  *mapped?* An instance of type *<boolean>*.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *mapped?* An instance of type *<boolean>*.

Description
           

Maps or unmaps *frame*, that is, displays frame on the screen or
removes it from the screen, depending on whether *mapped?* is true or
false. Note that a frame is considered to be mapped if it is anywhere on
the screen, even if it is not completely visible because other windows
are covering it either partially or completely, or if it is iconized.

Example
       

The following example creates a simple frame, then displays it and
unmaps it using *frame-mapped?-setter* rather than `See
start-frame`_ and `See
exit-frame`_. You should run this code in the
interactor, pressing the RETURN key at the points indicated.

define variable \*frame\* =
                           

make(<simple-frame>, title: "A frame",

layout: make(<button>));*RETURN*
                                

frame-mapped?-setter(#t, \*frame\*);*RETURN*

frame-mapped?-setter(#f, \*frame\*);*RETURN*

See also
        

`exit-frame`_

`frame-mapped?`_

`start-frame`_

frame-menu-bar
--------------

Generic function
''''''''''''''''

Summary
       

Returns the menu bar used in the specified frame.

Signature
         

frame-menu-bar *frame* => *menu-bar*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *menu-bar* An instance of type *false-or(`See
   <menu-bar> <gadgets.htm#30138>`_)*.

Description
           

Returns the menu bar used in *frame*.

See also
        

`frame-menu-bar-setter`_

frame-menu-bar-setter
---------------------

Generic function
''''''''''''''''

Summary
       

Sets the menu bar used in the specified frame.

Signature
         

frame-menu-bar-setter *menu-bar* *frame* => *menu-bar*

Arguments
         

-  *menu-bar* An instance of type *false-or(`See
   <menu-bar> <gadgets.htm#30138>`_)*.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *menu-bar* An instance of type *false-or(`See
   <menu-bar> <gadgets.htm#30138>`_)*.

Description
           

Sets the menu bar used in *frame*.

See also
        

`frame-menu-bar`_

frame-mode
----------

Generic function
''''''''''''''''

Summary
       

Returns the mode of the specified frame.

Signature
         

frame-mode *frame* => *mode*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *mode* An instance of type *one-of(#"modeless", #"modal",
   #"system-modal")*.

Description
           

Returns the mode of *frame*. This is the same value as was specified
for the *mode:* init-keyword when the frame was created.

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

*Note:* You can only set the mode of a frame when it is first created.
The mode cannot subsequently be changed.

See also
        

`<frame>`_

frame-owner
-----------

Generic function
''''''''''''''''

Summary
       

Returns the controlling frame for the specified frame.

Signature
         

frame-owner *frame* => *owner*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *owner* An instance of type *false-or(`See
   <frame>`_)*.

Description
           

Returns the controlling frame for *frame*. The controlling frame for
any hierarchy of existing frames is the one that owns the thread in
which the frames are running. Thus, the controlling frame for *frame* is
not necessarily its direct owner: it may be the owner of *frame* ’s
owner, and so on, depending on the depth of the hierarchy.

frame-palette
-------------

Generic function
''''''''''''''''

Summary
       

Returns the palette used in the specified frame.

Signature
         

frame-palette *frame* => *palette*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *palette* An instance of type `<palette> <dcs.htm#11494>`_.

Description
           

Returns the palette used in *frame*.

See also
        

`frame-palette-setter`_

frame-palette-setter
--------------------

Generic function
''''''''''''''''

Summary
       

Sets the palette used in the specified frame.

Signature
         

frame-palette-setter *palette* *frame* => *palette*

Arguments
         

-  *palette* An instance of type `<palette> <dcs.htm#11494>`_.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *palette* An instance of type `<palette> <dcs.htm#11494>`_.

Description
           

Sets the palette used in *frame*.

See also
        

`frame-palette`_

frame-position
--------------

Generic function
''''''''''''''''

Summary
       

Returns the position on the screen of the specified frame.

Signature
         

frame-position *frame* => *x* *y*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *x* An instance of type *<integer>*.
-  *y* An instance of type *<integer>*.

Description
           

Returns the position on the screen of *frame*. Coordinates are
expressed relative to the top left corner of the screen, measured in
pixels.

Example
       

The following example creates a simple frame, then displays it and tests
its position. You should run this code in the interactor, pressing the
RETURN key at the points indicated.

define variable \*frame\* =
                           

make(<simple-frame>, title: "A frame",

layout: make(<button>));*RETURN*
                                

start-frame(\*frame\*);*RETURN*

frame-position(\*frame\*);*RETURN*

See also
        

`frame-size`_

`frame-state`_

`set-frame-position`_

frame-resizable?
----------------

Generic function
''''''''''''''''

Summary
       

Returns true if the specified frame is resizable.

Signature
         

frame-resizable? *frame* => *resizable?*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *resizable?* An instance of type *<boolean>*.

Description
           

Returns true if *frame* is resizable, that is can have one or both of
its width and height modified by the user.

Example
       

See also
        

`frame-fixed-height?`_

`frame-fixed-width?`_

frame-size
----------

Generic function
''''''''''''''''

Summary
       

Returns the size of the specified frame.

Signature
         

frame-size *frame* => *width* *height*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *width* An instance of type *<integer>*.
-  *height* An instance of type *<integer>*.

Description
           

Returns the size of *frame*, measured in pixels.

Example
       

The following example creates a simple frame, then displays it and tests
its size. You should run this code in the interactor, pressing the
RETURN key at the points indicated.

define variable \*frame\* =
                           

make(<simple-frame>, title: "A frame",

layout: make(<button>));*RETURN*
                                

start-frame(\*frame\*);*RETURN*

frame-size(\*frame\*);*RETURN*

See also
        

`frame-position`_

`frame-state`_

`set-frame-size`_

frame-state
-----------

Generic function
''''''''''''''''

Summary
       

Returns the visible state of the specified frame.

Signature
         

frame-state *frame* => *state*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *state* An instance of type *one-of(#"detached", #"unmapped",
   #"mapped", #"iconified", #"destroyed")*.

Description
           

Returns the visible state of the specified frame. The return value from
this function indicates whether frame is currently iconified, whether it
is mapped or unmapped, whether it has been destroyed, or whether it has
become detached from the thread of which it was a part.

Example
       

The following example creates a simple frame, then displays it and tests
its position. You should run this code in the interactor, pressing the
RETURN key at the points indicated.

define variable \*frame\* =
                           

make(<simple-frame>, title: "A frame",

layout: make(<button>));*RETURN*
                                

start-frame(\*frame\*);*RETURN*

frame-state(\*frame\*);*RETURN*
                               

=> #"mapped"
            

See also
        

`frame-position`_

`frame-size`_

frame-status-bar
----------------

Generic function
''''''''''''''''

Summary
       

Returns the status bar used in the specified frame.

Signature
         

frame-status-bar *frame* => *status-bar*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *status-bar* An instance of type *false-or(`See
   <status-bar> <gadgets.htm#50900>`_)*.

Description
           

Returns the status bar used in *frame*.

See also
        

`frame-status-bar-setter`_

frame-status-bar-setter
-----------------------

Generic function
''''''''''''''''

Summary
       

Sets the status bar used in the specified frame.

Signature
         

frame-status-bar-setter *status-bar* *frame* => *status-bar*

Arguments
         

-  *status-bar* An instance of type *`See
   <status-bar> <gadgets.htm#50900>`_*.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *status-bar* An instance of type *false-or(`See
   <status-bar> <gadgets.htm#50900>`_)*.

Description
           

Sets the status bar used in *frame*.

See also
        

`frame-status-bar`_

frame-status-message
--------------------

Open generic function
'''''''''''''''''''''

Summary
       

Returns the status message for the specified frame.

Signature
         

frame-status-message *frame* => *status-message*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *status-message* An instance of type *false-or(<string>)*.

Description
           

Returns the status message for *frame*. This is the label in the status
bar for the frame. If the frame has no status bar, or if the label is
not set, this this function returns false.

See also
        

`frame-status-bar`_

`frame-status-message-setter`_

`<status-bar> <gadgets.htm#50900>`_

frame-status-message-setter
---------------------------

Generic function
''''''''''''''''

Summary
       

Sets the status message for the specified frame.

Signature
         

frame-status-message *status-message* *frame* => *status-message*

Arguments
         

-  *status-message* An instance of type *false-or(<string>)*.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *status-message* An instance of type *false-or(<string>)*.

Description
           

Sets the status message for *frame*. This is the label in the status
bar for the frame. If the frame has no status bar, then attempting to
set the label fails silently.

See also
        

`frame-status-bar-setter`_

`frame-status-message`_

`<status-bar> <gadgets.htm#50900>`_

frame-thread
------------

Generic function
''''''''''''''''

Summary
       

Returns the thread with which the specified frame is associated.

Signature
         

frame-thread *frame* => *thread*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *thread* An instance of type *<thread>*.

Description
           

Returns the thread with which *frame* is associated.

For more information about threads, refer to the manual *Library
Reference: Core Features*.

frame-title
-----------

Generic function
''''''''''''''''

Summary
       

Returns the title of the specified frame.

Signature
         

frame-title *frame* => *title*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *title* An instance of type *false-or(<string>)*.

Description
           

Returns the title of *frame*. If this is *#f*, then the title bar is
removed from the frame, if this is possible. If this is not possible,
then a default message is displayed. Whether the title bar can be
removed from the frame or not is platform dependent.

See also
        

`frame-title-setter`_

frame-title-setter
------------------

Generic function
''''''''''''''''

Summary
       

Sets the title of the specified frame.

Signature
         

frame-title-setter *title* *frame* => *title*

Arguments
         

-  *title* An instance of type *false-or(<string>)*.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *title* An instance of type *false-or(<string>)*.

Description
           

Sets the title of *frame*. The title of a frame is displayed in the
title bar of the frame. If *title* is *#f*, then the platform attempts
to remove the title bar from the frame, if possible.

See also
        

`frame-title`_

frame-tool-bar
--------------

Generic function
''''''''''''''''

Summary
       

Returns the tool bar used in the specified frame.

Signature
         

frame-tool-bar *frame* => *tool-bar*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *tool-bar* An instance of type *false-or(`See
   <tool-bar> <gadgets.htm#58915>`_)*.

Description
           

Returns the tool bar used in *frame*.

See also
        

`frame-tool-bar-setter`_

frame-tool-bar-setter
---------------------

Generic function
''''''''''''''''

Summary
       

Sets the tool bar used in the specified frame.

Signature
         

frame-tool-bar-setter *tool-bar* *frame* => *tool-bar*

Arguments
         

-  *tool-bar* An instance of type *false-or(`See
   <tool-bar> <gadgets.htm#58915>`_)*.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *tool-bar* An instance of type *false-or(`See
   <tool-bar> <gadgets.htm#58915>`_)*.

Description
           

Sets the tool bar used in *frame*.

See also
        

`frame-tool-bar`_

frame-top-level
---------------

Generic function
''''''''''''''''

Summary
       

Returns the top level loop function for the specified frame.

Signature
         

frame-top-level *frame* => *top-level*

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

-  *top-level* An instance of type *<function>*.

Description
           

Returns the top level loop function for *frame*. The top level loop
function for a frame is the "command loop" for the frame.

The default method for *frame-top-level* calls *read-event* and then
*`handle-event <silica.htm#94892>`_*.

See also
        

*`handle-event <silica.htm#94892>`_*

<frame-unmapped-event>
----------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of events that indicate a frame has been unmapped.

Superclasses
            

`<frame-event> <silica.htm#96445>`_

Init-keywords
             

-  None.

Description
           

The class of events that indicate a frame has been unmapped, that is,
removed from the screen. An instance of this class is distributed
whenever a frame is unmapped. A frame may be unmapped by either
iconifying it, or by exiting or destroying the frame completely, so that
it no longer exists.

Operations
          

-  None.

Example
       

The following example defines a method that can inform you when an
instance of a class of frame you have defined is unmapped.

define method handle-event
                          

(frame :: <my-frame>,

event :: <frame-unmapped-event>) => ()

notify-user

(format-to-string("Frame %= unmapped", frame))

end method handle-event;
                        

See also
        

`<frame-mapped-event>`_

\*global-command-table\*
------------------------

Variable
''''''''

Summary
       

The command table inherited by all new command tables.

Type
    

`<command-table>`_

Description
           

This is the command table from which all other command tables inherit by
default. You should not explicitly add anything to or remove anything
from this command table. DUIM can use this command to store internals or
system-wide commands. You should not casually install any commands or
translators into this command table.

See also
        

`<command-table>`_

`\*user-command-table\*`_

iconify-frame
-------------

Generic function
''''''''''''''''

Summary
       

Iconifies the specified frame.

Signature
         

iconify-frame *frame* => ()

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

None

Description
           

Iconifies *frame*. The appearance of the iconified frame depends on the
behavior of the operating system in which the application is running.
For instance, in Windows 95 or Windows NT 4.0, the icon is displayed in
the task bar at the bottom of the screen.

Example
       

The following example creates and displays a simple frame, then
iconifies it. You should run this code in the interactor, pressing the
RETURN key at the points indicated.

define variable \*frame\* =
                           

make(<simple-frame>, title: "A frame",

layout: make(<button>));*RETURN*
                                

start-frame(\*frame\*);*RETURN*
                               

iconify-frame(\*frame\*);*RETURN*
                                 

See also
        

`deiconify-frame`_

`destroy-frame`_

`exit-frame`_

`frame-icon`_

`lower-frame`_

`raise-frame`_

layout-frame
------------

Generic function
''''''''''''''''

Summary
       

Resizes the specified frame and lays out the current pane hierarchy
inside it.

Signature
         

layout-frame *frame* #key *width height* => ()

Arguments
         

-  *frame* An instance of type `<frame>`_.
-  *width* An instance of type *false-or(<integer>)*.
-  *height* An instance of type *false-or(<integer>)*.

Values
      

None

Description
           

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

lower-frame
-----------

Generic function
''''''''''''''''

Summary
       

Lowers the specified frame to the bottom of the stack of visible
windows.

Signature
         

lower-frame *frame* => ()

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

None

Description
           

Lowers *frame* to the bottom of the stack of visible windows. After
calling this function, *frame* will appear beneath any occluding windows
that may be on the screen.

Example
       

The following example creates and displays a simple frame, then lowers
it.

define variable \*frame\* =
                           

make(<simple-frame>, title: "A frame",

layout: make(<button>));
                        

start-frame(\*frame\*);
                       

lower-frame(\*frame\*);
                       

See also
        

`deiconify-frame`_

`destroy-frame`_

`exit-frame`_

`iconify-frame`_

`raise-frame`_

make
----

G.f. method
'''''''''''

Summary
       

Creates an instance of a *<frame>*.

Signature
         

make *(class* == <frame>*)*
 #key *top-level command-queue layout icon
* *pointer-documentation command-table menu-bar tool-bar
* *status-bar title calling-frame top-level-sheet state
* *geometry resizable? properties thread event-queue
* *foreground background text-style palette save-under?
* *drop-shadow? dialog-for*
 => *simple-frame*

Arguments
         

-  *class* The class *`<frame>`_*.
-  *top-level* An instance of type *false-or(`See
   <sheet> <silica.htm#13118>`_)*. Default value: *#f*.
-  *command-queue* An instance of type *false-or(<event-queue>)*.
   Default value: *#f*.
-  *layout* An instance of type *false-or(`See
   <sheet> <silica.htm#13118>`_)*. Default value: *#f*.
-  *icon* An instance of type *false-or(* `See
   <image> <dcs.htm#51234>`_*)*. Default value: *#f*.

*pointer-documentation*
                       

-  An instance of type *false-or(<string>)*. Default value: *#f*.
-  *command-table* An instance of type *false-or(* `See
   <command-table>`_*)*. Default value: *#f*.
-  *menu-bar* An instance of type *false-or(* `See
   <menu-bar> <gadgets.htm#30138>`_*)*. Default value: *#f*.
-  *tool-bar* An instance of type *false-or(* `See
   <tool-bar> <gadgets.htm#58915>`_*)*. Default value: *#f*.
-  *status-bar* An instance of type *false-or(* `See
   <status-bar> <gadgets.htm#50900>`_*)*. Default value: *#f*.
-  *title* An instance of type *false-or(<string>)*. Default value:
   *#f*.
-  *calling-frame* An instance of type *false-or(`See
   <frame>`_)*. Default value: *#f*.
-  *state* An instance of type *one-of(#"detached", #"unmapped",
   #"mapped", #"iconified")*. Default value: *#"detached"*.
-  *geometry* An instance of type *<vector>*. Default value:
   *vector(#f, #f, #f, #f)*.
-  *resizable?* An instance of type *<boolean>*. Default value: *#t*.
-  *properties* An instance of type *<stretchy-object-vector>*. Default
   value: *make(<stretchy-vector>)*.
-  *thread* An instance of type *false-or(<thread>)*. Default value:
   *#f*.
-  *event-queue* An instance of type *false-or(<event-queue>)*. Default
   value: *#f*.
-  *foreground* An instance of type *false-or(* `See
   <ink> <dcs.htm#15007>`_*)*. Default value: *#f*.
-  *background* An instance of type *false-or(* `See
   <ink> <dcs.htm#15007>`_*)*. Default value: *#f*.
-  *text-style* An instance of type *false-or(* `See
   <text-style> <dcs.htm#85385>`_*)*. Default value: *#f*.
-  *palette* An instance of type *false-or(* `See
   <palette> <dcs.htm#11494>`_*)*. Default value: *#f*.
-  *save-under?* An instance of type *<boolean>*. Default value: *#f*.
-  *drop-shadow?* An instance of type *<boolean>*. Default value: *#f*
   .
-  *dialog-for* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *simple-frame* An instance of type `<frame>`_.

Description
           

Creates and returns an instance of `<frame>`_ or
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

See also
        

`<frame>`_

make-menu-from-command-table-menu
---------------------------------

Generic function
''''''''''''''''

Summary
       

Returns a menu from the menu definition in the specified command table.

Signature
         

make-menu-from-command-table-menu
 *command-table-menu-items* *frame* *framem*
 #key *command-table* *label* *mnemonic* *item-callback*
 => *menu*

Arguments
         

*command-table-menu-items*
                          

-  An instance of type *<sequence>*.
-  *frame* An instance of type `<frame>`_.
-  *framem* An instance of type `See
   <frame-manager> <silica.htm#32466>`_.
-  *command-table* An instance of type `See
   <command-table>`_.
-  *label* An instance of type `<label> <gadgets.htm#68802>`_.
-  *mnemonic* An instance of type *false-or(* `See
   <gesture> <silica.htm#76256>`_*)*.
-  *item-callback* An instance of type *<function>*.

Values
      

-  *menu* An instance of type `<menu> <gadgets.htm#81833>`_.

Description
           

Returns a menu from the menu definition in the specified command table.
This function is used by `See
make-menus-from-command-table`_ to individually
create each menu defined in the command table. The function `See
make-menus-from-command-table`_ then puts each of the
menus created together in the appropriate way.

The *command-table-menu-items* argument defines the items that are to be
placed in the menu. It is a sequence of instances of `See
<command-table-menu-item>`_.

The *frame* and *framem* arguments define the frame and the frame
manager in which the menu created is to be placed.

The *command-table* argument specifies the command table in which the
definition of the menu created can be found.

The *label* argument defines a label for the menu created.

The *mnemonic* argument defines a keyboard mnemonic for the menu
created.

See also
        

`make-menus-from-command-table`_

make-menus-from-command-table
-----------------------------

Generic function
''''''''''''''''

Summary
       

Returns a set of menus from the menu definitions in the specified
command table.

Signature
         

make-menus-from-command-table *command-table* *frame* *framem* #key
*label* => *menus*

Arguments
         

-  *command-table* An instance of type `See
   <command-table>`_.
-  *frame* An instance of type `<frame>`_.
-  *framem* An instance of type `See
   <frame-manager> <silica.htm#32466>`_.
-  *label* An instance of type `<label> <gadgets.htm#68802>`_.

Values
      

-  *menus* An instance of type *limited(<sequence>, of:* `See
   <menu> <gadgets.htm#81833>`_*)*.

Description
           

Returns a set of menus from the menu definitions in *command-table*.

The *frame* and *framem* arguments specify the frame and frame manager
in which the menus are to be placed.

The *label* argument lets you specify a label for the set of menus.

See also
        

`make-menu-from-command-table-menu`_

menu-item-accelerator
---------------------

Generic function
''''''''''''''''

Summary
       

Returns the accelerator for the specified command table menu item.

Signature
         

menu-item-accelerator *menu-item* => *accelerator*

Arguments
         

-  *menu-item* An instance of type `See
   <command-table-menu-item>`_.

Values
      

-  *accelerator* An instance of type `See
   <gesture> <silica.htm#76256>`_.

Description
           

Returns the keyboard accelerator for *menu-item*. Note that *menu-item*
must be defined in a command table.

See also
        

`menu-item-mnemonic`_

menu-item-mnemonic
------------------

Generic function
''''''''''''''''

Summary
       

Returns the mnemonic for the specified menu item.

Signature
         

menu-item-mnemonic *menu-item* => *mnemonic*

Arguments
         

-  *menu-item* An instance of type `See
   <command-table-menu-item>`_.

Values
      

-  *mnemonic* An instance of type *false-or(* `See
   <gesture> <silica.htm#76256>`_*)*.

Description
           

Returns the keyboard mnemonic for *menu-item*.

See also
        

`menu-item-accelerator`_

menu-item-name
--------------

Generic function
''''''''''''''''

Summary
       

Returns the name of the specified menu item.

Signature
         

menu-item-name *menu-item* => *name*

Arguments
         

-  *menu-item* An instance of type `See
   <command-table-menu-item>`_.

Values
      

-  *name* An instance of type *<string>*.

Description
           

Returns the name of *menu-item*.

See also
        

`menu-item-options`_

`menu-item-type`_

`menu-item-value`_

menu-item-options
-----------------

Generic function
''''''''''''''''

Summary
       

Returns the options for the specified menu item.

Signature
         

menu-item-options *menu-item* => *options*

Arguments
         

-  *menu-item* An instance of type `See
   <command-table-menu-item>`_.

Values
      

-  *options* An instance of type *<object>*.

Description
           

Returns the options for *menu-item*.

See also
        

`menu-item-name`_

`menu-item-type`_

`menu-item-value`_

menu-item-type
--------------

Generic function
''''''''''''''''

Summary
       

Returns the type of the specified menu item.

Signature
         

menu-item-type *menu-item* => *type*

Arguments
         

-  *menu-item* An instance of type `See
   <command-table-menu-item>`_.

Values
      

-  *type* An instance of type *<object>*.

Description
           

Returns the type of *menu-item*.

See also
        

`menu-item-name`_

`menu-item-options`_

`menu-item-value`_

menu-item-value
---------------

Generic function
''''''''''''''''

Summary
       

Returns the value of the specified menu item.

Signature
         

menu-item-value *menu-item* => *value*

Arguments
         

-  *menu-item* An instance of type `See
   <command-table-menu-item>`_.

Values
      

-  *value* An instance of type *<object>*.

Description
           

Returns the value of *menu-item*.

See also
        

`menu-item-name`_

`menu-item-options`_

`menu-item-type`_

move-to-next-page
-----------------

Generic function
''''''''''''''''

Summary
       

Moves to the next page of the specified multi-page dialog.

Signature
         

move-to-next-page *wizard* => ()

Arguments
         

-  *wizard* An instance of type `See
   <wizard-frame>`_.

Values
      

None.

Description
           

Moves to the next page in sequence of *wizard*. This is the default
callback for the Next button in a wizard frame.

See also
        

`dialog-next-callback`_

`<wizard-frame>`_

move-to-previous-page
---------------------

Generic function
''''''''''''''''

Summary
       

Moves to the previous page of the specified multi-page dialog.

Signature
         

move-to-previous-page *wizard* => ()

Arguments
         

-  *wizard* An instance of type `See
   <wizard-frame>`_.

Values
      

None.

Description
           

Moves to the previous page in sequence of *wizard*. This is the default
callback for the Back button in a wizard frame.

See also
        

`dialog-back-callback`_

`<wizard-frame>`_

note-progress
-------------

Generic function
''''''''''''''''

Summary
       

Note the progress of an event in the specified progress note.

Signature
         

note-progress *numerator* *denominator*
 #key *note* *label* *pointer-cursor* => ()

Arguments
         

-  *numerator* An instance of type *<integer>*.
-  *denominator* An instance of type *<integer>*.
-  *note* An instance of type *<progress-note>*. Default value: `See
   \*progress-note\*`_.
-  *label* An instance of type `<label> <gadgets.htm#68802>`_.
-  *pointer-cursor* An instance of type `See
   <pointer> <silica.htm#85241>`_.

Values
      

None

Description
           

Note the progress of an event in *note*.

If a *numerator* and *denominator* are supplied, then the progress is
displayed in terms of those figures. For example, if *numerator* is 1,
and *denominator* is 10, then the progress is displayed in tenths.

If supplied, *pointer-cursor* is used as a cursor when the mouse pointer
is placed over the owner frame.

See also
        

`noting-progress`_

`\*progress-note\*`_

noting-progress
---------------

Statement macro
'''''''''''''''

Summary
       

Performs a body of code, noting its progress.

Macro call
          

noting-progress ({*sheet* }, {*label* }) {*body* } end

Arguments
         

-  *sheet* A Dylan expression*bnf*.
-  *label* A Dylan expression*bnf*.
-  body A Dylan body*bnf*.

Values
      

-  None.

Description
           

Performs a body of code, noting its progress, for the specified sheet.

The sheet argument is an expression that evaluates to an instance of
`<sheet> <silica.htm#13118>`_. The label argument is an expression
that evaluates to an instance of *<string>*.

See also
        

`note-progress`_

\*progress-note\*
-----------------

Fluid variable
''''''''''''''

Summary
       

Specifies a default progress note that can be used.

Type
    

*<object>*

Initial value
             

#f

Description
           

This variable is used to supply a default progress note to use if no
progress note is explicitly specified.

See also
        

`note-progress`_

<property-frame>
----------------

Open instantiable class
'''''''''''''''''''''''

Summary
       

The class of property frames.

Superclasses
            

`<dialog-frame>`_

Init-keywords
             

-  *pages:* An instance of type *false-or(limited(<sequence>, of: `See
   <page> <gadgets.htm#93333>`_))*. Default value: *#f*.

*apply-callback:*
                 

-  An instance of type *false-or(<function>)*. Default value: *#f*.
-  *apply-button:* An instance of type *false-or(* `See
   <button> <gadgets.htm#20680>`_*)*. Default value: *#f*.

*Note:* The following two useful init-keywords are inherited from `See
<dialog-frame>`_:

-  *pages:* An instance of type *false-or(<sequence>)*. Default value:
   *#f*.

*page-changed-callback:*
                        

-  An instance of type *false-or(<function>)*. Default value: *#f*.

Description
           

The class of property frames. These are dialogs that can contain
property sheets of some description. This is the class of dialogs with
several pages, each presented as a label in a tab control.

A property frame
                

.. figure:: images/frames-2.png
   :align: center
   :alt: 

.. figure:: images/frames-5.png
   :align: center
   :alt: 
The *pages:* init-keyword defines the pages available for the property
frame.

The apply callback and button define an additional Apply button
available in property frames. The Apply button applies any changes made
in the current page of the dialog, but does not dismiss the dialog from
the screen. By default, there is no Apply button defined.

The page-changed callback lets you specified a callback that should be
invoked if the current page in the property frame is changed by clicking
on a different page tab.

Operations
          

The following operations are exported from the *DUIM-Frames* module.

`dialog-apply-button`_ `See
dialog-apply-button-setter`_
 `dialog-apply-callback`_ `See
dialog-current-page`_
 `dialog-current-page-setter`_
 `dialog-page-changed-callback`_ `See
dialog-page-changed-callback-setter`_ `See
dialog-page-complete?`_ `See
dialog-page-complete?-setter`_ `See
dialog-pages`_ `See
dialog-pages-setter`_

See also
        

`dialog-apply-button`_

`dialog-apply-callback`_

`<dialog-frame>`_

`<property-page>`_

`<wizard-frame>`_

<property-page>
---------------

Open instantiable class
'''''''''''''''''''''''

Summary
       

The class of property pages.

Superclasses
            

`<page> <gadgets.htm#93333>`_

Init-keywords
             

None.

Description
           

The class of property pages. These are pages that can be displayed in an
instance of `<property-frame>`_.

A property page
               

.. figure:: images/frames-2.png
   :align: center
   :alt: 

.. figure:: images/frames-6.png
   :align: center
   :alt: 
Internally, this class maps into the Windows property page control.

Operations
          

-  None.

See also
        

`<page> <gadgets.htm#93333>`_

`<property-frame>`_

`<property-page>`_

`<tab-control-page> <gadgets.htm#47108>`_

`<wizard-page>`_

raise-frame
-----------

Generic function
''''''''''''''''

Summary
       

Raises the specified frame to the top of the stack of visible windows.

Signature
         

raise-frame *frame* => ()

Arguments
         

-  *frame* An instance of type `<frame>`_.

Values
      

None

Description
           

Raises *frame* to the top of the stack of visible windows. After calling
this function, *frame* will appear above any occluding windows that may
be on the screen.

Example
       

The following example creates and displays a simple frame, then lowers
and raises it. You should run this code in the interactor, pressing the
RETURN key at the points indicated.

define variable \*frame\* =
                           

make(<simple-frame>, title: "A frame",

layout: make(<button>));*RETURN*
                                

start-frame(\*frame\*);*RETURN*
                               

lower-frame(\*frame\*);*RETURN*

raise-frame(\*frame\*);*RETURN*
                               

See also
        

`deiconify-frame`_

`destroy-frame`_

`exit-frame`_

`iconify-frame`_

`lower-frame`_

redo-command
------------

Generic function
''''''''''''''''

Summary
       

Performs the last performed command again.

Signature
         

redo-command *command* *frame* => #rest *values*

Arguments
         

-  *command* An instance of type `<command>`_.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *values* Instances of type *<object>*.

Description
           

Performs *command* again. The command is the command that was last
executed using `execute-command`_.

Note that the command described by *command* must be undoable.

You can both specialize this function and call it directly in your code.

See also
        

`execute-command`_

remove-command
--------------

Generic function
''''''''''''''''

Summary
       

Removes a command from the specified command table.

Signature
         

remove-command *command-table* *command* => ()

Arguments
         

-  *command-table* An instance of type `See
   <command-table>`_.
-  *command* An instance of type `<command>`_.

Values
      

None

Description
           

Removes *command* from *command-table*.

See also
        

`add-command`_

remove-command-table
--------------------

Function
''''''''

Summary
       

Removes the specified command table.

Signature
         

remove-command-table *command-table* => ()

Arguments
         

-  *command-table* An instance of type `See
   <command-table>`_.

Values
      

None

Description
           

Removes *command-table*.

remove-command-table-menu-item
------------------------------

Generic function
''''''''''''''''

Summary
       

Removes a menu item from the specified command table.

Signature
         

remove-command-table-menu-item *command-table* *string* => ()

Arguments
         

-  *command-table* An instance of type `See
   <command-table>`_.
-  *string* An instance of type *<string>*.

Values
      

None

Description
           

Removes the menu item identified by *string* from *command-table*.

See also
        

`add-command-table-menu-item`_

set-frame-position
------------------

Generic function
''''''''''''''''

Summary
       

Sets the position of the specified frame.

Signature
         

set-frame-position *frame* *x* *y* => ()

Arguments
         

-  *frame* An instance of type `<frame>`_.
-  *x* An instance of type *<integer>*.
-  *y* An instance of type *<integer>*.

Values
      

None

Description
           

Sets the position of *frame*. The coordinates *x* and *y* are measured
from the top left of the screen, measured in pixels.

See also
        

`frame-position`_

`set-frame-size`_

set-frame-size
--------------

Generic function
''''''''''''''''

Summary
       

Sets the size of the specified frame.

Signature
         

set-frame-size *frame* *width* *height* => ()

Arguments
         

-  *frame* An instance of type `<frame>`_.
-  *width* An instance of type *<integer>*.
-  *height* An instance of type *<integer>*.

Values
      

None

Description
           

Sets the size of *frame*.

Example
       

The following example creates and displays a simple frame, then resizes
it. You should run this code in the interactor, pressing the RETURN key
at the points indicated.

define variable \*frame\* =
                           

make(<simple-frame>, title: "A frame",

layout: make(<button>, label: "Button"));*RETURN*
                                                 

set-frame-size(\*frame\*, 100, 500);*RETURN*

See also
        

`frame-size`_

`set-frame-position`_

<simple-command>
----------------

Open abstract instantiable class
''''''''''''''''''''''''''''''''

Summary
       

The class of simple commands.

Superclasses
            

*<object>*

Init-keywords
             

-  *function:* An instance of type *<function>*. Required.
-  *arguments:* An instance of type *<sequence>*. Default value *#[]*.

Description
           

The class of simple commands. A simple command has an associated
function and some arguments. Simple commands are not undoable.

The first argument to the function is always the frame.

Operations
          

None.

See also
        

`<command>`_

<simple-frame>
--------------

Open abstract instantiable class
''''''''''''''''''''''''''''''''

Summary
       

The class of simple frames.

Superclasses
            

`<frame>`_

Init-keywords
             

-  *command-queue:* An instance of type *false-or(<event-queue)*.
   Default value: *make(<event-queue>)*.
-  *layout:* An instance of type *false-or(* `See
   <sheet> <silica.htm#13118>`_*)*. Default value: *#f*.
-  *command-table:* An instance of type *false-or(* `See
   <command-table>`_*)*. Default value: *#f*.
-  *menu-bar:* An instance of type *false-or(* `See
   <menu-bar> <gadgets.htm#30138>`_*)*. Default value: *#f*.
-  *tool-bar:* An instance of type *false-or(* `See
   <tool-bar> <gadgets.htm#58915>`_*)*. Default value: *#f*.
-  *status-bar:* An instance of type *false-or(* `See
   <status-bar> <gadgets.htm#50900>`_*)*. Default value: *#f*.

Description
           

The class of simple frames.

The *command-queue:* init-keyword specifies a command-queue for the
frame.

The *layout:* init-keyword specifies a layout for the frame.

The *command-table:* init-keyword specifies a command table for the
frame.

The *menu-bar:* init-keyword specifies a menu bar for the frame.

The *tool-bar:* init-keyword specifies a tool bar for the frame.

The *status-bar:* init-keyword specifies a status bar for the frame.

Operations
          

The following operations are exported from the *DUIM-Frames* module.

`frame-command-table`_ `See
frame-command-table-setter`_ `See
frame-layout`_ `See
frame-layout-setter`_ `See
frame-menu-bar`_ `See
frame-menu-bar-setter`_ `See
frame-status-bar`_ `See
frame-status-bar-setter`_ `See
frame-status-message`_ `See
frame-status-message-setter`_ `See
frame-tool-bar`_ `See
frame-tool-bar-setter`_ `See
frame-top-level`_ `See
start-frame`_

<simple-undoable-command>
-------------------------

Open abstract instantiable class
''''''''''''''''''''''''''''''''

Summary
       

The class of simple commands that can contain an undo action.

Superclasses
            

*<object>*

Init-keywords
             

-  *undo-command:* An instance of type `See
   <command>`_.

Description
           

The class of simple commands that can contain an undo action. A simple
undoable command is like a simple command, except that it points to a
command that can undo it, represented by the *undo-command:*
init-keyword.

Operations
          

-  None.

See also
        

`<simple-command>`_

start-dialog
------------

Generic function
''''''''''''''''

Summary
       

Displays a DUIM frame as a dialog box.

Signature
         

start-dialog *dialog* => #rest *values*

Arguments
         

-  *dialog* An instance of type `See
   <dialog-frame>`_.

Values
      

-  *values* Instances of type *<object>*.

Description
           

Displays a DUIM frame as a dialog box.

The function *start-dialog* dynamically binds an *<abort>* restart
around the event loop for the dialog that is started. The restart allows
the event loop to be re-entered, and enables any callbacks run from the
dialog to signal an *<abort>* (via the *abort* function, for instance),
in order to terminate execution of the current callback and return to
event processing. This facility is useful for implementing operations
that cancel gestures and for debugging DUIM applications from Dylan
debuggers.

See also
        

`cancel-dialog`_

`<dialog-frame>`_

`exit-dialog`_

`start-frame`_

start-frame
-----------

Generic function
''''''''''''''''

Summary
       

Starts the specified frame.

Signature
         

start-frame *frame* #key *owner* *mode* => *status-code*

Arguments
         

-  *frame* An instance of type `<frame>`_.
-  *owner* An instance of type *false-or(`See
   <frame>`_)*. Default value: *#f*.
-  *mode* An instance of type *one-of("modal", #"modeless",
   #"system-modal")*. Default value: *#f*.

Values
      

-  *status-code* An instance of type *<integer>*.

Description
           

Starts *frame*, optionally setting the *owner* of the frame and the
*mode* in which it will run.

The function *start-frame* dynamically binds an *<abort>* restart around
the event loop for the frame that is started. The restart allows the
event loop to be re-entered, and enables any callbacks run from the
frame to signal an *<abort>* (via the *abort* function, for instance),
in order to terminate execution of the current callback and return to
event processing. This facility is useful for implementing operations
that cancel gestures and for debugging DUIM applications from Dylan
debuggers.

Example
       

The following example creates a simple frame, then displays it. You
should run this code in the interactor, pressing the RETURN key at the
points indicated.

define variable \*frame\* =
                           

make(<simple-frame>, title: "A frame",

layout: make(<button>));*RETURN*
                                

start-frame(\*frame\*);*RETURN*

See also
        

`exit-frame`_

`frame-mapped?-setter`_

`start-dialog`_

undo-command
------------

Generic function
''''''''''''''''

Summary
       

Calls the undo command for the specified command.

Signature
         

undo-command *command* *frame* => #rest *values*

Arguments
         

-  *command* An instance of type `<command>`_.
-  *frame* An instance of type `<frame>`_.

Values
      

-  *values* Instances of type *<object>*.

Description
           

Calls the undo command for *command*, undoing the effects of calling
*command*. Note that *command* must be undoable.

You can call this command directly in your own code, as well as
specialize it.

See also
        

`command-undoable?`_

\*user-command-table\*
----------------------

Variable
''''''''

Summary
       

A user-defined command table that can be inherited by other command
tables.

Type
    

`<command-table>`_

Description
           

This is a command table that can be used by the programmer for any
purpose. DUIM does not use it for anything, and its contents are
completely undefined.

If desired, all new command tables can inherit the command table
specified by this variable.

See also
        

`<command-table>`_

`\*global-command-table\*`_

<wizard-frame>
--------------

Open instantiable class
'''''''''''''''''''''''

Summary
       

The class of wizard frames.

Superclasses
            

`<dialog-frame>`_

Init-keywords
             

-  *page:* An instance of type `<page> <gadgets.htm#93333>`_.
-  *pages:* An instance of type *false-or(limited(<sequence>, of:* `See
   <page> <gadgets.htm#93333>`_*)*. Default value: *#f*.

*apply-callback:*
                 

-  An instance of type *false-or(<function>)*. Default value: *#f*.
-  *apply-button:* An instance of type *false-or(* `See
   <button> <gadgets.htm#20680>`_*)*. Default value: *#f*.

Note that the following two useful init-keywords are inherited from `See
<dialog-frame>`_:

-  *pages:* An instance of type *false-or(<sequence>)*. Default value:
   *#f*.

*page-changed-callback:*
                        

-  An instance of type *false-or(<function>)*. Default value: *#f*.

Description
           

The class of wizard frames. These are frames that are used to create
wizards (series of connected dialogs) that are used to guide the user
through a structured task, such as installing an application.

A wizard frame
              

.. figure:: images/frames-2.png
   :align: center
   :alt: 

.. figure:: images/frames-7.png
   :align: center
   :alt: 
A wizard frame is a multi-page dialog, in which the user specifies
requested information before proceeding to the next page in the
sequence. At the end of the sequence, the user exits the dialog to send
the relevant information back to the controlling application.

When a wizard frame is created, each page in the frame automatically has
a Next and Back button to let the user navigate forward and backward
through the sequence of pages.

In addition, if *apply-button:* is specified, an Apply button is
displayed in the frame. By default, clicking on this button lets the
user apply the changes made so far without dismissing the frame from the
screen. If specified, the *apply-callback:* function is invoked when the
Apply button is clicked.

The layout of a wizard frame is controlled using a `See
<stack-layout> <layouts.htm#99774>`_.

Operations
          

The following operations are exported from the *DUIM-Frames* module.

`compute-next-page`_ `See
compute-previous-page`_ `See
dialog-back-button`_ `See
dialog-back-button-setter`_ `See
dialog-back-callback`_ `See
dialog-current-page`_ `See
dialog-current-page-setter`_ `See
dialog-next-button`_ `See
dialog-next-button-setter`_ `See
dialog-next-callback`_ `See
dialog-next-enabled?`_
 `dialog-next-enabled?-setter`_ `See
dialog-next-page`_ `See
dialog-next-page-setter`_ `See
dialog-page-changed-callback`_ `See
dialog-page-changed-callback-setter`_ `See
dialog-page-complete?`_ `See
dialog-page-complete?-setter`_ `See
dialog-pages`_ `See
dialog-pages-setter`_ `See
dialog-previous-page`_ `See
dialog-previous-page-setter`_ `See
move-to-next-page`_ `See
move-to-previous-page`_

Example
       

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
                      

See also
        

`<dialog-frame>`_

`<property-frame>`_

`<wizard-page>`_

<wizard-page>
-------------

Open instantiable class
'''''''''''''''''''''''

Summary
       

The class of wizard pages.

Superclasses
            

`<page> <gadgets.htm#93333>`_

Init-keywords
             

None.

Description
           

The class of wizard pages. These are pages that can be displayed in an
instance of `<wizard-frame>`_, and are used for a
single dialog in the structured task that the wizard guides the user
through.

A wizard page
             

.. figure:: images/frames-2.png
   :align: center
   :alt: 

.. figure:: images/frames-8.png
   :align: center
   :alt: 
Operations
          

-  None.

See also
        

`<page> <gadgets.htm#93333>`_

`<property-page>`_

`<tab-control-page> <gadgets.htm#47108>`_

`<wizard-frame>`_


