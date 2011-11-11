********************
Using Command Tables
********************

Introduction
------------

Another way that you can define a set of menus is by defining a *command
table*. A command table lets you create the complete set of commands
for an application in a more compact and reusable way than the standard
menus you have seen so far. As well as making the definition of each
command in a menu shorter and easier to code, it lets you handle effects
such as the disabling of menu commands more elegantly, by removing the
need to use *gadget-enabled*. You can include a command table in the
definition of a frame in the same way that you can include a tool bar,
or a status bar, and because of this, and the fact that you can include
command tables within other command tables, it is easy to reuse the same
command table across different frames in your application.

Command tables are best used in the following situations:

-  If your menu commands do not use check or radio buttons.
-  If the menu bar in your application is not context sensitive (that
   is, the available commands on the menu remain consistent as the
   application state changes).

In other cases, you should define your menu hierarchy by defining panes
that combine specific gadgets, as demonstrated in `See Adding Menus To
The Application <menus.htm#81811>`_. Using a combination of command
tables and standard menu definitions in a GUI design is not recommended.

The task list manager application does not use check or radio buttons in
any of its menu commands, and the menu bar is not context sensitive.
This means that, if you wish, you can define the commands in the task
list manager using command tables, rather than standard menus.

This chapter provides an introduction to command tables by showing you
how to re-implement the menu system of the task list manager as a set of
command tables. It does not provide a complete copy of all the source
code necessary to implement the task list manager. For a complete copy
of the code, please refer to `See Source Code For The Task List
Manager <source.htm#77017>`_. To load the code into the environment,
choose *Tools > Open Example Project* from any window in the
environment, and load the Task List 2 project from the Documentation
category of the Open Example Project dialog. *Please note that this
project, like the Task List 1 project, is called* *task-list* *within
the source code, and you should not load them both into the environment
at the same time.*

Implementing a command table
----------------------------

You use *define* *command-table* to define a new command table. Consider
the following command table defined for the *File* menu in the task list
manager:

.. code-block:: dylan

    define command-table *file-command-table*
        (*global-command-table*)
      menu-item "Open" = open-file,
        accelerator: make-keyboard-gesture(#"o", #"control"),
        documentation: "Opens an existing file.";
      menu-item "Save" = save-file,
        accelerator: make-keyboard-gesture(#"s", #"control"),
        documentation: "Saves the current file to disk.";
      menu-item "Save As..." = save-as-file,
        documentation: "Saves the current file with a new name.";
      menu-item "Exit" = exit-task,
        accelerator: make-keyboard-gesture(#"f4", #"alt"),
        documentation: "Exits the application.";
    end command-table *file-command-table*;

This defines a command table, called **file-command-table**, that
contains all the menu commands required in the *File* menu of the task
list manager. It replaces the definition of each menu button, as well as
the definition of the *File* menu itself, in the original implementation
of the task list manager application that was given in `See Adding Menus
To The Application <menus.htm#81811>`_. As you can see, this definition
is considerably shorter than the individual definitions of the menu and
menu buttons previously required,

When defining a command table, you should provide a list of other
command tables from which the command table you are defining inherits.
This is done in the clause

.. code-block:: dylan

    define command-table *file-command-table*
        (*global-command-table*)

above. This is analogous to the way that the superclasses of any frame
class are listed in the frame’s definition.

Any items defined by the command tables which are to be inherited are
automatically added to the command table being defined.

In the example above, **file-command-table** inherits from only one
command table: **global-command-table**. This is defined globally for
the whole Dylan environment, and every command table that does not
explicitly inherit from other command tables must inherit from this
command table.

Each menu item is introduced using the *menu-item* option, and a command
is specified for each menu item immediately after the *=* sign. Each
command is just the activate callback that was defined for the
equivalent menu button gadget in `See Adding Callbacks to the
Application <callbacks.htm#15598>`_.

Notice that you can use the *accelerator:* and *documentation:*
init-keywords to specify a keyboard accelerator and a documentation
string for each menu item in the command table, just like you can when
you define each menu button in a menu using a specific gadget. In the
same way, you can specify the value of any init-keyword that can be
specified for an instance of *<menu-button>*.

Re-implementing the menus of the task list manager
--------------------------------------------------

The code below provides definitions for the entire menu hierarchy of the
task list manager, using the same activate callbacks that are described
and implemented in `See Adding Callbacks to the
Application <callbacks.htm#15598>`_. Note that the labels, documentation
strings, and keyboard accelerators for each menu item are identical to
the ones used in the original implementation of the task list manager.
For completeness, the definition of **file-command-table**, described
in `See Implementing a command table <commands.htm#97241>`_, is repeated
below.

.. code-block:: dylan

    define command-table *file-command-table* (*global-command-table*)
      menu-item "Open" = open-file,
        accelerator: make-keyboard-gesture(#"o", #"control"),
        documentation: "Opens an existing file.";
      menu-item "Save" = save-file,
        accelerator: make-keyboard-gesture(#"s", #"control"),
        documentation: "Saves the current file to disk.";
      menu-item "Save As..." = save-as-file,
        documentation: "Saves the current file with a new name.";
      menu-item "Exit" = exit-task,
        accelerator: make-keyboard-gesture(#"f4", #"alt"),
        documentation: "Exits the application.";
    end command-table *file-command-table*;

    define command-table *edit-command-table* (*global-command-table*)
      menu-item "Cut" = cut-command,
        accelerator: make-keyboard-gesture(#"x", #"control"),
        documentation: "Cut the selection to the clipboard.";
      menu-item "Copy" = copy-command,
        accelerator: make-keyboard-gesture(#"c", #"control"),
        documentation: "Copy the selection to the clipboard.";
      menu-item "Paste" = paste-command,
        accelerator: make-keyboard-gesture(#"v", #"control"),
        documentation: "Paste the selection in the clipboard at the current position.";
    end command-table *edit-command-table*;

    define command-table *task-command-table*
        (*global-command-table*)
      menu-item "Add..." = frame-add-task,
        accelerator: make-keyboard-gesture(#"a", #"control", #"shift"),
        documentation: "Add a new task.";
      menu-item "Remove" = frame-remove-task,
        accelerator: make-keyboard-gesture(#"d", #"control", #"shift"),
        documentation: "Remove the selected task from the list.";
    end command-table *task-command-table*;

    define command-table *help-command-table* (*global-command-table*)
      menu-item "About" = about-task,
        accelerator: make-keyboard-gesture(#"f1"),
        documentation: "Display information about the application.";
    end command-table *help-command-table*;

The definitions above can be used in place of the definition of each
menu and menu button in the original implementation of the task list
manager. You must place the command table definitions provided above
after the callback definitions themselves, to avoid forward references.

Including command tables in frame definitions
---------------------------------------------

In the previous section, you defined four command tables: one for each
menu in the task list manager. Next, you need to combine these command
tables and include them in the definition of the *<task-frame>*. The
way to do this is to define an additional command table which has each
of the other command tables as its components, and then supply this
command table as an option in the definition of *<task-frame>*.

.. code-block:: dylan

    define command-table *task-list-command-table*
        (*global-command-table*)
      menu-item "File" = *file-command-table*;
      menu-item "Edit" = *edit-command-table*;
      menu-item "Task" = *task-command-table*;
      menu-item "Help" = *help-command-table*;
    end command-table *task-list-command-table*

Just like the menu commands in each menu, every menu in the menu bar is
defined as a menu item in the definition of the command table.

You can add a command table to the definition of a frame class in much
the same way as you add a layout, tool bar, status bar, or menu bar,
using the *command-table* option. In the definition of *<task-frame>*,
replace the line that reads:

.. code-block:: dylan

   menu-bar (frame) frame.task-menu-bar;

with

.. code-block:: dylan

    command-table (frame) *task-list-command-table*;

A complete listing of the implementation of *<task-frame>* using command
tables is given in `See Source Code For The Task List
Manager <source.htm#77017>`_.

Changes required to run Task List 2
-----------------------------------

In order for the Task List 2 project to run properly, you must modify
some of the definitions you constructed in `See Adding Callbacks to the
Application <callbacks.htm#15598>`_. This section outlines the required
changes. For your convenience, the complete source code for both of the
Task List projects is provided in `See Source Code For The Task List
Manager <source.htm#77017>`_.

Changes to button definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The definition of each button in the definition of *<task-frame>* needs
to be modified compared to their definition in the Task List 1 project,
as described in `See Gluing the new design
together <improve.htm#70170>`_.

Broadly speaking, you need to update the *command:* keyword/argument
pair for each button gadget, and you need to redefine the activate
callback to allow for the fact that the callbacks now take frames as
arguments.

Thus, for a button that is defined as:

.. code-block:: dylan

    pane add-button (frame)
      make (<push-button>, label: "Add task",
            activate-callback: frame-add-task);

the new definition is:

.. code-block:: dylan

    pane add-button (frame)
      make(<push-button>, label: "Add task",
           command: frame-add-task,
           activate-callback: method (gadget)
             frame-add-task(frame)
           end);

This change must also be made for the definition of radio box, which
then becomes:

.. code-block:: dylan

    // Definition of radio box
    pane priority-box (frame)
      make(<radio-box>,
           items: $priority-items,
           orientation: #"horizontal",
           label-key: first,
           value-key: second,
           value: #"medium",
           command: not-yet-implemented
           activate-callback: method (gadget)
             not-yet-implemented(frame)
           end);

For complete definitions, you should refer to the source code available
in Appendix A or from the Open Example Project dialog in the
environment.

Changes to callback definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following callbacks should be redefined so as to take an instance of
*<task-frame>* as an argument, rather than an instance of *<gadget>*.

- frame-add-task
- frame-remove-task
- open-file
- save-file
- save-as-file
- about-task
- exit-task

For complete definitions of these callbacks, you should refer to the
source code available in Appendix A or from the Open Example Project
dialog in the environment.

Changes to method definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The definitions for the methods given in Chapter 5 must be redefined so
as to take an instance of *<frame>* as an argument, rather than an
instance of *<gadget>*. This change results in these new definitions:

.. code-block:: dylan

    define method open-file
        (frame :: <task-frame>) => ()
      let task-list = frame-task-list(frame);
      let filename
        = choose-file(frame: frame,
                      default: task-list.task-list-filename,
                      direction: #"input");
      if (filename)
        let task-list = load-task-list(filename);
        if (task-list)
          frame.frame-task-list := task-list;
          refresh-task-frame(frame)
        else
          notify-user(format-to-string("Failed to open file %s", filename),
                      owner: frame)
        end
      end
    end method open-file;

    define method save-file
        (frame :: <task-frame>) => ()
      let task-list = frame-task-list(frame);
      if (task-list.task-list-modified?)
        save-as-file(frame, filename: task-list.task-list-filename)
      end
    end method save-file;

    define method save-as-file
        (frame :: <task-frame>, #key filename) => ()
      let task-list = frame-task-list(frame);
      let filename
        = filename | choose-file(frame: frame,
                                 default: task-list.task-list-filename,
                                 direction: #"output");
      if (filename)
        if (save-task-list(task-list, filename: filename))
          frame.frame-task-list := task-list;
          refresh-task-frame(frame)
        else
          notify-user(format-to-string
                       ("Failed to save file %s", filename),
                      owner: frame)
        end
      end
    end method save-as-file;

    define method frame-add-task (frame :: <task-frame>) => ()
      let task-list = frame-task-list(frame);
      let (name, priority) = prompt-for-task(owner: frame);
      if (name & priority)
        let new-task = make(<task>, name: name, priority: priority);
        add-task(task-list, new-task);
        refresh-task-frame(frame);
        frame-selected-task(frame) := new-task
      end
    end method frame-add-task;

    define method frame-remove-task (frame :: <task-frame>) => ()
      let task = frame-selected-task(frame);
      let task-list = frame-task-list(frame);
      if (notify-user(format-to-string
                       ("Really remove task %s", task.task-name),
                      owner: frame, style: #"question"))
        frame-selected-task(frame) := #f;
        remove-task(task-list, task);
        refresh-task-frame(frame)
      end
    end method frame-remove-task;

    define method note-task-selection-change
        (frame :: <task-frame>) => ()
      let task = frame-selected-task(frame);
      if (task)
        frame.priority-box.gadget-value := task.task-priority;
      end;
      command-enabled?(frame-remove-task, frame) := task ~= #f;
    end method note-task-selection-change;

For details about *note-task-selection-change*, see `See Enabling and
disabling buttons in the interface <callbacks.htm#42654>`_. See `See A
task list manager using command tables <source.htm#52969>`_ for the
complete source code for the Task List 2 project.


