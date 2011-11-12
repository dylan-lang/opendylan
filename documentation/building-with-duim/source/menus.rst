*******************************
Adding Menus To The Application
*******************************

Now it is time to consider adding some menus to your application. There
are two basic ways that you can create a system of menus for your
application:

-  Design a hierarchical series of panes using the ``<menu-bar>``,
   ``<menu>``, and various menu buttons classes, and glue the elements of
   this design together in the correct order.
-  Use a command table.

In this chapter, the first of these methods is demonstrated. For
information about command tables, refer to `Using Command
Tables <commands.htm#99799>`_. Before discussing the first method listed
above, the overall design of the menu system for the task list manager
is discussed.

A description of the menu system
--------------------------------

Before implementing the menus for the task list manager, it is worth
describing what you are going to implement. The menu system of the task
list manager comprises four menus: a *File* menu, *Edit* menu, *Task*
menu, and *Help*. Each of these menus contains a number of commands, as
follows:

-  *File* menu The *File* menu contains four commands that operate on
   the files loaded into the task list manager. The *Open* command opens
   a new file. The *Save* command saves the currently loaded file to
   disk. The *Save As* command saves the currently loaded file to disk
   under a new name. The *Exit* command quits the task application
   completely.
-  *Edit* menu The *Edit* menu contains the standard clipboard commands:
   *Cut*, *Copy*, and *Paste*.
-  *Task* menu The *Task* menu contains two commands that operate on
   individual tasks. The *Add* command adds a new task to the list. The
   *Remove* command removes the selected task from the list.
-  *Help* menu In a full-blown application, you would use commands in
   the *Help* menu as one hook into your online help system (other hooks
   being provided by buttons in dialog boxes and the F1 key). In this
   application, the *Help* menu contains a single command that simply
   displays a simple About dialog for the application.

Creating a menu hierarchy
-------------------------

As you might expect, creating a menu hierarchy in a frame definition is
a matter of defining a series of panes for the frame. At the top-most
level in the menu hierarchy is the menu bar itself. The menu bar
contains each menu defined for the application and each menu contains
the menu commands that themselves perform operations. Once the panes
have been defined, the menu bar needs to be included in the frame using
the ``menu-bar`` clause.

First of all, you can create a pane that defines the menu bar itself as
follows:

.. code-block:: dylan

    pane task-menu-bar (frame)
      make(<menu-bar>,
           children: vector(frame.file-menu,
                            frame.edit-menu,
                            frame.task-menu,
                            frame.help-menu));

Next, define the File and Tasks menus themselves:

.. code-block:: dylan

    pane file-menu (frame)
      make(<menu>, label: "File",
           children: vector(frame.open-menu-button,
                            frame.save-menu-button,
                            frame.save-as-menu-button,
                            frame.exit-menu-button));
    pane edit-menu (frame)
      make(<menu>, label: "Edit",
           children: vector(frame.cut-menu-button,
                            frame.copy-menu-button,
                            frame.paste-menu-button));
    pane task-menu (frame)
      make(<menu>, label: "Task",
           children: vector(frame.add-menu-button,
                            frame.remove-menu-button));
    pane help-menu (frame)
      make(<menu>, label: "Help",
           children: vector(frame.about-menu-button));

Finally, you need to define the menu commands themselves. A command that
appears on a menu is defined as an instance of ``<menu-button>``, and so
there is a strong similarity between these buttons and some of the
buttons already defined. DUIM also generates mnemonics for each menu
item; thus, the items appear as *File* and *Edit*, and so forth. (Note
that the ``make-keyboard-gesture`` function that appears below is defined
in `Keyboard accelerators <menus.htm#34519>`_.)

.. code-block:: dylan

    // Commands in the File menu
    pane open-menu-button (frame)
      make(<menu-button>, label: "Open...",
           activate-callback: not-yet-implemented,
           accelerator: make-keyboard-gesture(#"o", #"control"),
           documentation: "Opens an existing file.");
    pane save-menu-button (frame)
      make(<menu-button>, label: "Save",
           activate-callback: not-yet-implemented,
           accelerator: make-keyboard-gesture(#"s", #"control"),
           documentation: "Saves the current file to disk.");
    pane save-as-menu-button (frame)
      make(<menu-button>, label: "Save As...",
           activate-callback: save-as-file,
           documentation: "Saves the current file with a new name.");
    pane exit-menu-button (frame)
      make(<menu-button>, label: "Exit",
           activate-callback: not-yet-implemented,
           accelerator: make-keyboard-gesture(#"f4", #"alt"),
           documentation: "Exits the application.");

    //Commands in the Edit menu
    pane cut-menu-button (frame)
      make(<menu-button>, label: "Cut",
           activate-callback: not-yet-implemented,
           accelerator: make-keyboard-gesture(#"x", #"control"),
           documentation: "Cut the selection to the clipboard.");
    pane copy-menu-button (frame)
      make(<menu-button>, label: "Copy",
           activate-callback: not-yet-implemented,
           accelerator: make-keyboard-gesture(#"c", #"control"),
           documentation: "Copy the selection to the clipboard.");
    pane paste-menu-button (frame)
      make(<menu-button>, label: "Paste",
           activate-callback: not-yet-implemented,
           accelerator: make-keyboard-gesture(#"v", #"control"),
           documentation: "Paste the selection in the clipboard at the current position.");

    //Commands in the Task menu
    pane add-menu-button (frame)
      make(<menu-button>, label: "Add...",
           activate-callback: not-yet-implemented,
           accelerator: make-keyboard-gesture
                         (#"a", #"control", #"shift"),
           documentation: "Add a new task.");
    pane remove-menu-button (frame)
      make(<menu-button>, label: "Remove",
           activate-callback: not-yet-implemented,
           accelerator: make-keyboard-gesture
                         (#"d", #"control", #"shift"),
           documentation: "Remove the selected task from the list.");

    //Commands in the Help menu
    pane about-menu-button (frame)
      make(<menu-button>, label: "About",
           activate-callback: not-yet-implemented,
           accelerator: make-keyboard-gesture(#"f1"),
           documentation:
             "Display information about the application.");

Once you have defined the menu bar and all the children that it is to
contain, you need to activate the menu bar in the frame by including the
following towards the end of the frame definition.

.. code-block:: dylan

    menu-bar (frame) frame.task-menu-bar;

The definitions of these menu buttons demonstrate two interesting new
features: the use of keyboard accelerators, and the use of documentation
strings.

Documentation strings
~~~~~~~~~~~~~~~~~~~~~

Documentation strings let you provide brief online help for gadgets such
as menu buttons. You can specify a documentation string for any gadget
using the ``documentation:`` init-keyword. Although you can make whatever
use you want of these strings, using the ``gadget-documentation`` and
``gadget-documentation-setter`` methods, documentation strings for menu
buttons are used in status bars without any need for special action on
your part. If you display a menu and move the mouse pointer over the
items in the menu, then the documentation string defined for each item
is displayed in the status bar of the frame for as long as the mouse
pointer is over the menu item. It is generally good practice to supply
documentation strings for all the menu items in a frame. Documentation
strings for other gadgets become tooltips in Windows.

Keyboard accelerators
~~~~~~~~~~~~~~~~~~~~~

Keyboard accelerators let you define a combination of keys that can be
pressed in order to invoke the activate callback of a gadget. This means
that you can access the functionality of an application without having
to choose commands from menus using the mouse, and can make it much
quicker to use an application you are familiar with.

To specify a keyboard accelerator, you need to specify an alphanumeric
character, or a function key, together with any modifier keys (such as
the CONTROL or ALT keys) that should be held down while the alphanumeric
character is pressed. You actually create a keyboard accelerator by
calling the ``make`` method on ``<keyboard-gesture>``, though to make it a
little easier, define the function below, which is used in the
definition of each menu button.

.. code-block:: dylan

    define function make-keyboard-gesture
      (keysym :: <symbol>, #rest modifiers)
    => (gesture :: <keyboard-gesture>)
      make(<keyboard-gesture>, keysym: keysym, modifiers: modifiers)
    end function make-keyboard-gesture;

Add this definition to the file *frame.dylan*.

The keyboard accelerators defined demonstrate the several useful points
about keyboard accelerators:

-  Whenever possible, use standard keyboard accelerators for standard
   application commands on your platform. Here, you use CONTROL+O to
   open a file, CONTROL+S to save a file, and CONTROL+X, CONTROL+C, and
   CONTROL+V respectively for *Cut*, *Copy*, and *Paste*.
-  As well as standard alphanumeric characters, you can use function
   keys as keyboard accelerators.
-  As well as the more common CONTROL key, you can use the ALT and SHIFT
   keys as modifiers, though you should not use the SHIFT key as the
   sole modifier.
-  You can use more than one modifier key at once.
-  If you wish, you need not use any modifier keys at all, as is the
   case with the (slightly non-standard) keyboard accelerator for the
   *About* command.

Gluing the final design together
--------------------------------

You can now add the definitions of the menu bar, menus, and menu
buttons, to the definition of the ``<task-frame>`` class, to give the code
shown below. At this stage, the only thing missing from the final
application are real callback functions. Callbacks are dealt with in
`Adding Callbacks to the Application <callbacks.htm#15598>`_.

Note that the final definition of ``<task-frame>`` includes the definition
of a slot: ``frame-task-list``. This takes an instance of the class
``<task-list>`` as a value, the default value being an empty ``<task-list>``.
Although it has not been referred to so far, this class will be used
as the basic data structure in which task lists are stored, and a more
complete description of these data structures is given in `Defining
the underlying data structures for tasks <callbacks.htm#71186>`_. It
transpires that defining the ``frame-task-list`` slot is essential for
some of the file handling routines that are described in `Handling
files in the task list manager <callbacks.htm#78540>`_.

.. code-block:: dylan

    define frame <task-frame> (<simple-frame>)
      slot frame-task-list :: <task-list> = make(<task-list>);

      // definition of menu bar
      pane task-menu-bar (frame)
        make(<menu-bar>,
             children: vector(frame.file-menu,
      frame.edit-menu,
      frame.task-menu,
      frame.help-menu));

      // definition of menus
      pane file-menu (frame)
        make(<menu>, label: "File",
             children: vector(frame.open-menu-button,
      frame.save-menu-button,
      frame.save-as-menu-button,
      frame.exit-menu-button));
      pane edit-menu (frame)
        make(<menu>, label: "Edit",
             children: vector(frame.cut-menu-button,
      frame.copy-menu-button,
      frame.paste-menu-button));
      pane task-menu (frame)
        make(<menu>, label: "Task",
             children: vector(frame.add-menu-button,
      frame.remove-menu-button));

      pane help-menu (frame)
        make(<menu>, label: "Help",
             children: vector(frame.about-menu-button));

      // definition of menu buttons

      // Commands in the File menu
      pane open-menu-button (frame)
        make(<menu-button>, label: "Open...",
             activate-callback: not-yet-implemented,
             accelerator: make-keyboard-gesture(#"o", #"control"),
             documentation: "Opens an existing file.");
      pane save-menu-button (frame)
        make(<menu-button>, label: "Save",
             activate-callback: not-yet-implemented,
             accelerator: make-keyboard-gesture(#"s", #"control"),
             documentation: "Saves the current file to disk.");
      pane save-as-menu-button (frame)
        make(<menu-button>, label: "Save As...",
             activate-callback: save-as-file,
             documentation:
               "Saves the current file with a new name.");
      pane exit-menu-button (frame)
        make(<menu-button>, label: "Exit",
             activate-callback: not-yet-implemented,
             accelerator: make-keyboard-gesture(#"f4", #"alt"),
             documentation: "Exits the application.");

      //Commands in the Edit menu
      pane cut-menu-button (frame)
        make(<menu-button>, label: "Cut",
             activate-callback: not-yet-implemented,
             accelerator: make-keyboard-gesture(#"x", #"control"),
             documentation: "Cut the selection to the clipboard.");
      pane copy-menu-button (frame)
        make(<menu-button>, label: "Copy",
             activate-callback: not-yet-implemented,
             accelerator: make-keyboard-gesture(#"c", #"control"),
             documentation: "Copy the selection to the clipboard.");
      pane paste-menu-button (frame)
        make(<menu-button>, label: "Paste",
             activate-callback: not-yet-implemented,
             accelerator: make-keyboard-gesture(#"v", #"control"),
             documentation:
               "Paste the selection in the clipboard at the current position.");

      //Commands in the Task menu
      pane add-menu-button (frame)
        make(<menu-button>, label: "Add...",
             activate-callback: not-yet-implemented,
             accelerator: make-keyboard-gesture
                           (#"a", #"control", #"shift"),
             documentation: "Add a new task.");
      pane remove-menu-button (frame)
        make(<menu-button>, label: "Remove",
             activate-callback: not-yet-implemented,
             accelerator: make-keyboard-gesture
                           (#"d", #"control", #"shift"),
             documentation:
               "Remove the selected task from the list.");

      //Commands in the Help menu
      pane about-menu-button (frame)
        make(<menu-button>, label: "About",
             activate-callback: not-yet-implemented,
             accelerator: make-keyboard-gesture(#"f1"),
             documentation:
               "Display information about the application.");

      // definition of buttons
      pane add-button (frame)
        make(<push-button>, label: "Add task",
             activate-callback: not-yet-implemented);
      pane remove-button (frame)
        make(<push-button>, label: "Remove task",
             activate-callback: not-yet-implemented);
      pane open-button (frame)
        make(<push-button>, label: "Open file",
             activate-callback: not-yet-implemented);
      pane save-button (frame)
        make(<push-button>, label: "Save file",
             activate-callback: not-yet-implemented);

      // definition of radio box
      pane priority-box (frame)
        make (<radio-box>,
             items: $priority-items,
             orientation: #"horizontal",
             label-key: first,
             value-key: second,
             value: #"medium",
             activate-callback: not-yet-implemented);

      // definition of tool bar
      pane task-tool-bar (frame)
        make(<tool-bar>,
             child: horizontally ()
               frame.open-button;
               frame.save-button;
               frame.add-button;
               frame.remove-button
             end);

      // definition of status bar
      pane task-status-bar (frame)
        make(<status-bar>, label: "Task Manager");

      // definition of list
      pane task-list (frame)
        make (<list-box>, items: #(), lines: 15,
              activate-callback: not-yet-implemented);

      // main layout
      pane task-layout (frame)
        vertically ()
          frame.task-list;
          frame.priority-box;
        end;

      // activation of frame elements
      layout (frame) frame.task-layout;
      tool-bar (frame) frame.task-tool-bar;
      status-bar (frame) frame.task-status-bar;
      menu-bar (frame) frame.task-menu-bar;

      // frame title
      keyword title: = "Task List Manager";
    end frame <task-frame>;

