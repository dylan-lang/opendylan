***********************************
Adding Callbacks to the Application
***********************************

At this point, the task list manager still does very little. If you try
running the code (as described in `See Starting the
application <improve.htm#17910>`_), and interacting with any of the
elements in the GUI (clicking on a button, choosing a menu command, and
so on), then only the “not yet implemented” message is displayed. This
section shows you how to remedy this situation, by adding callback
functions to the task list manager.

Getting the application to respond to mouse events such as clicking on a
button or choosing a menu command consists of two things:

For each gadget in the GUI, you need to specify which callbacks to use.
There are several different types of callback, depending on the type of
event for which you want to define behavior.

You need to define the callback functions themselves. These are the
functions that are invoked when a particular callback type is
detected, and are the functions you use to define the correct behavior
for your application.

In addition, you need to set up the basic data structures that allow you
to work with tasks in your application.

At this point, you may be wondering exactly what a callback is, and why
they are used to respond to application events, rather than event
handlers. If you have developed GUI applications using other development
environments, you may be more used to writing event handlers that work
for a whole class of objects, and discriminating on which instance of a
class to work on at any one time by means of case statements.

Writing event handlers in this way can be cumbersome. It turns out to be
much simpler to define a function that works only for a particular
instance of a class, and then refer to this function when defining the
class instance. This function is what is referred to as a callback. This
makes the source code for your application much clearer and easier to
write, and the only price you pay is that you have to specify a callback
for each gadget when you define the gadget itself.

In fact, DUIM provides a complete protocol for defining and handling
events of all descriptions. However, you only need to use this protocol
if you are creating new classes of gadgets, for which you need to define
the event behavior, or new classes of events (for example, support for
different input devices or notification of low resources). If you are
just using gadgets, then you only ever need to use callbacks.

Defining the underlying data structures for tasks
=================================================

Before defining any real callbacks, it is time to consider how you can
represent task lists, and the information contained in them. This is
essential, not just for handling tasks within the application, but for
saving task lists to disk, and loading them back into the application.

Add the code described in this section to ``task-list.dylan``.

There are two basic kinds of object that you need to model: task lists
and tasks. A task list is a collection of one or more tasks. The best
way to represent these is by defining a ``<task-list>`` class and a
``<task>`` class.

The definition of ``<task-list>``, below, contains three slots:

``task-list-tasks``

   This slot specifies a sequence of tasks that are contained in the
   task list. Each object in the sequence will be an instance of
   ``<task>``. The default for new task lists is an empty stretchy
   vector. An init-keyword has been specified so that this slot can be
   set when an instance of the class is initialized.

``task-list-filename``

   This slot specifies the file on disk to which the task list has been
   saved, if it has been saved at all. The default for new task lists is
   ``#f``, since the task list has not yet been saved to disk. An
   init-keyword has been specified so that this slot can be set when an
   instance of the class is initialized.

``task-list-modified?``

   The purpose for this slot is less obvious. It is useful to flag
   whether or not a task list has been modified so that, for instance,
   the *Save* command in the application can be disabled if the task
   list is unmodified. There is no init-keyword defined for this class,
   because you only ever want to use the supplied default value for new
   instances of ``<task-list>``.

.. code-block:: dylan

    define class <task-list> (<object>)
      constant slot task-list-tasks = make(<stretchy-vector>),
        init-keyword: tasks:;
      slot task-list-filename :: false-or(<string>) = #f,
        init-keyword: filename:;
      slot task-list-modified? :: <boolean> = #f;
    end class <task-list>;

Next, consider the information that needs to be encoded in each
individual task. There are two pieces of information that need to be
recorded:

-  The text of the task, which should be a string.
-  The priority, which should be one of high, medium, or low.

Priorities can be recorded using a constant, as shown below:

.. code-block:: dylan

    define constant <priority> = one-of(#"low", #"medium", #"high");

Notice that it is most straightforward to encode each priority as a
symbol. Later on, you will see how you can use ``as`` to convert each
symbol to a format that can be saved to disk and read back into the
application as a symbol.

The ``<task>`` class can then be defined as having two slots: one for the
task text itself, and another for the priority. Both have init-keywords
so that they can be specified when a new instance is created, and both
init-keywords are required; they must be specified whenever a task is
created.

.. code-block:: dylan

    define class <task> (<object>)
      slot task-name :: <string>,
        required-init-keyword: name:;
      slot task-priority :: <priority>,
        required-init-keyword: priority:;
    end class <task>;

These three definitions are all that is needed to be able to represent
tasks and task lists within the task list application.

In order to handle tasks effectively in the GUI of the task list
manager, some changes are necessary to the definition of the ``task-list``
pane in the definition of ``<task-frame>``. These changes are needed to
ensure that information about tasks is passed to the ``task-list`` pane
correctly. Make these changes to the existing definition in the file
``frame.dylan``.

In `See Gluing the final design together <menus.htm#70705>`_, the
definition of ``task-list`` was given as:

.. code-block:: dylan

    // definition of list
    pane task-list (frame)
      make (<list-box>, items: #(), lines: 15,
            activate-callback: not-yet-implemented);

First, you need to ensure that the items passed to ``task-list`` are the
tasks in the ``<task-list>`` associated with the frame. Recall that a
``frame-task-list`` slot was specified in the definition of ``<task-frame>``
; this slot is used to hold the instance of ``<task-list>`` that is
associated with the ``<task-frame>``. The sequence of tasks contained in
the associated ``frame-task-list`` can then be found using the
``frame-task-list.task-list-tasks`` accessor. To display these tasks in
the ``task-list`` pane, the ``items:`` init-keyword needs to be set to the
value of this accessor:

.. code-block:: dylan

    items: frame.frame-task-list.task-list-tasks,

Next, you need to ensure that the label for each task in the ``task-list``
pane is the text of the task itself. As described above, the text of any
task is stored in its ``task-name`` slot. In order to display this text as
the label for every item in the list box, you need to specify the
``task-name`` slot as the ``gadget-label-key`` of the list box. A label key
is a function that is used to calculate the label of each item in a
gadget, and it can be specified using the ``label-key:`` init-keyword:

.. code-block:: dylan

    label-key: task-name,

This gives the following new definition for the ``task-list`` pane:

.. code-block:: dylan

    // definition of list
    pane task-list (frame)
      make (<list-box>,
            items: frame.frame-task-list.task-list-tasks,
            label-key: task-name,
            lines: 15,
            activate-callback: not-yet-implemented);

There is one final change that still needs to be made to this pane
definition. This is described in `See Updating the user
interface <callbacks.htm#94307>`_.

Specifying a callback in the definition of each gadget
======================================================

As you have already seen when using the ``not-yet-implemented`` callback,
providing a callback for a gadget is just a matter of specifying another
keyword-value pair in the definition of the gadget. There are two ways
that you can specify the callback function to use.

If you wish, you can define the callback function inline, making the
definition itself the value part of the keyword-value pair.

This can be useful for a simple callback function that you only need to
invoke from a single callback type in a single pane. However, if several
panes, or several types of callback, need to invoke the same callback
function, you need to define the function explicitly in each gadget that
uses it.

Alternatively, you can define a callback function explicitly in your
application code, and then refer to it by name in the keyword-value
pair.

This method is best for portability and reusability of your code, since
the same callback function can be referred to by name in as many gadgets
as you need to use it in, without having to redefine the callback
function in each gadget. It can also lead to more readable source code.
This technique is the one used throughout this example application.

As already mentioned, there are a number of different kinds of callback
available, depending on the behavior that you want to specify, and the
gadget for which you are defining a callback. When defining different
callbacks for a gadget, you need to use a different init-keyword for
each callback.

As you have already seen, by far the most common callback is the
activate callback. This type of callback is invoked when you activate
any instance of ``<action-gadget>``. For buttons, the activate callback
is invoked when you click on the button. For menu commands, the activate
callback is invoked when you choose the command from the menu. The
activate callback is the callback that is used most in the task list
manager. You can specify an activate callback for any gadget using the
``activate-callback:`` init-keyword. In addition, you have seen the
value-changed callback, which is invoked when the gadget-value has been
changed. You can specify this callback using the
``value-changed-callback:`` init-keyword.

You have already defined a callback for all the gadgets in the GUI. All
you need to do now is replace the reference to ``not-yet-implemented``
with the real function name that should get called when each gadget is
activated. Thus, to specify an activate callback for the *Add task*
button in the tool bar, redefine the button as follows in the definition
of the ``<task-frame>`` class:

.. code-block:: dylan

    pane add-button (frame)
      make(<push-button>, label: "Add task",
           activate-callback: frame-add-task);

You can use exactly the same callback in the new definition of
``add-menu-button`` :

.. code-block:: dylan

    pane add-menu-button (frame)
      make(<menu-button>, label: "Add...",
           activate-callback: frame-add-task,
           accelerator: make-keyboard-gesture
                          (#"a", #"control", #"shift"),
           documentation: "Add a new task.");

Notice how both of these gadgets specify the same activate callback.
This is because the *Add* command in the menu should perform exactly the
same action as the *Add task* button in the tool bar.

At this point, redefine the callback for each gadget listed in the table
below, making sure that you supply the same callback to those gadgets
that perform the same functions.

The callback functions used in the Task List Manager

Gadget

Callback

open-menu-button

open-file

save-menu-button

save-file

save-as-menu-button

save-as-file

exit-menu-button

exit-task

add-menu-button

frame-add-task

remove-menu-button

frame-remove-task

about-menu-button

about-task

add-button

frame-add-task

remove-button

frame-remove-task

open-button

open-file

save-button

save-file

The following sections show you how to define the callbacks themselves.
You will need to define other functions and methods, as well as the
callback functions listed above. These other functions and methods are
called by some of the callbacks themselves.

Defining the callbacks
======================

This section shows you how to define the callbacks that are necessary in
the task list manager, as well as any other associated functions and
methods.

-  First, you will look at methods and functions that enable file
   handling in the task list manager; that is, functions and methods
   that let you save and load files into the application.
-  Next, you will look at methods and functions for adding and removing
   tasks from the task list.
-  Last, you will define a few additional methods that are necessary to
   update the GUI elegantly, when other operations are performed.

All the code discussed in this chapter is structured so that callbacks
which affect the GUI do not also perform other tasks that are not
related to the GUI. This helps to keep the design of the application
clean, so that you can follow the code more easily, and is recommended
for all GUI design. Separating GUI code and non-GUI code also lets you
produce code that is more easily reusable, either in other parts of a
developing application, or in completely different applications.

Handling files in the task list manager
---------------------------------------

To begin with, you will define the functions and methods that let you
save files to disk and load them back into the task list manager. Once
you have added these to your code, you will be able to save and reload
your task lists into the application; this type of functionality is
essential in even the most trivial application.

There are three methods and two functions necessary for handling files.
The methods handle GUI-specific operations involved in loading and
saving files. The functions deal with the basic task of saving data
structures to disk, and loading them from disk. Add the definitions of
the methods to ``frame.dylan``, and the definitions of the functions to
``task-list.dylan``.

Each method is invoked as a callback in the definition of the
``<task-frame>`` class:

-  ``open-file`` This method prompts the user to choose a filename, and
   then loads that file into the task list manager by calling the
   function ``load-task-list``. It is used as the activate callback for
   both ``open-button`` (on the application tool bar) and
   ``open-menu-button`` (in the *File* menu of the application).
-  ``save-file`` This method saves the task list currently loaded into the
   application to disk. It is used as the activate callback for both
   ``save-button`` (on the application tool bar) and ``save-menu-button``
   (in the *File* menu of the application).
-  ``save-as-file`` This method saves the task list currently loaded into
   the application to disk, and prompts the user to supply a name. It is
   used as the activate callback for ``save-as-menu-button`` (in the
   *File* menu of the application).

The following functions are called by the methods described above:

-  ``save-task-list`` This function saves an instance of ``<task-list>`` to
   a named file. It is called by ``save-as-file``.
-  ``load-task-list`` This function takes the contents of a file on disk
   and converts it into an instance of ``<task-list>``. It is called by
   ``open-as-file``.

The following sections present and explain the code for each of these
methods and functions in turn.

The open-file method
--------------------

The code for open-file is shown below. Add this code to ``frame.dylan``.

.. code-block:: dylan

    define method open-file
        (gadget :: <gadget>) => ()
      let frame = sheet-frame(gadget);
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

The method takes a gadget as an argument and returns no values. The
argument is the gadget that is used to invoke it, which in the case of
the task list manager means either ``open-menu-button`` (in the *File*
menu of the application) or ``open-button`` (on the tool bar). The
``open-file`` method then sets three local variables:

-  ``frame`` This contains the frame of which the gadget argument is a
   part. This is a simple way of identifying the main application frame.
-  ``task-list`` This contains the value of the ``frame-task-list`` slot for
   frame. This identifies the instance of ``<task-list>`` that is being
   used to hold the task list information currently loaded into the task
   list manager.
-  ``filename`` This is the name of the file that is to be loaded into the
   task list manager, and the user is always prompted to supply it.

The method ``choose-file`` (a method provided by DUIM) is used to prompt
for a file to load. The portion of code that performs this task is
repeated here:

.. code-block:: dylan

    choose-file(frame: frame,
                default: task-list.task-list-filename,
                direction: #"input");

This method displays a standard file dialog box so that the user can
select a file on any disk connected to the host computer. For
``open-file``, you need to supply three arguments to ``choose-file`` : the
frame that owns the dialog, a default value to supply to the user, and
the direction of the interaction.

You need to supply a frame so that the system knows how to treat the
frame correctly, with respect to the dialog box. Thus, while the dialog
is displayed, the frame that owns it cannot be minimized, resized, or
interacted with in any way; this is standard behavior for modal dialog
boxes.

In this case, supplying a default value is useful in that it lets us
supply the filename for the currently loaded task list as a default
value. It determines this by examining the ``task-list-filename`` slot of
``task-list`` (which, remember, is defined as a local variable and
represents the instance of ``<task-list>`` in use). If this slot has a
value, then it is offered as a default. (Note that if the currently
loaded task list has never been saved to disk, then this slot is ``#f``,
and so no default is offered.)

The direction of interaction should also be specified when calling
``choose-file``, since the same generic function can be used to prompt
for a filename using a standard Open File dialog or a standard Save File
dialog. In this case, the direction is ``#"input"``, which indicates that
data is being read in (that is, Open File is used).

The rest of the ``open-file`` method deals with loading in the task list
information safely. It consists of two nested ``if`` statements as shown
below.

.. code-block:: dylan

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

The clause

.. code-block:: dylan

    if (filename)
      ...
    end

is necessary to handle the case where the user cancels the Open file
dialog: on cancelling the dialog, the ``open-file`` method should return
silently with no side effects.

If a filename is supplied, then it is read from disk and converted into
a format that is readable by the application, in the line that reads

.. code-block:: dylan

    let task-list = load-task-list(filename);

The function ``load-task-list`` is described in `See The load-task-list
function <callbacks.htm#37927>`_.

The clause

.. code-block:: dylan

    if (task-list)
      ...
    else
      ...
    end

is necessary to handle the case where the filename specified does not
contain data that can be interpreted by ``load-task-list``. If
``task-list`` cannot be assigned, then the ``else`` code is run. This calls
the function ``notify-user``, which is a simple way to display a short
message to the user in a message box.

If ``task-list`` can be assigned (that is, the contents of the specified
file have been successfully read by ``load-task-list`` ), then two lines
of code are run. The line

.. code-block:: dylan

    frame.frame-task-list := task-list;

assigns the ``frame-task-list`` slot of frame to the value of ``task-list``
.

The line

.. code-block:: dylan

    refresh-task-frame(frame)

calls a method that refreshes the list of tasks displayed in the task
list manager, so that the contents of the newly loaded file are
correctly displayed on the screen. The method ``refresh-task-frame`` is
described in `See Updating the user interface <callbacks.htm#94307>`_.

The save-file method
--------------------

The code for ``save-file`` is as follows:

.. code-block:: dylan

    define method save-file
        (gadget :: <gadget>) => ()
      let frame = sheet-frame(gadget);
      let task-list = frame-task-list(frame);
      save-as-file(gadget, filename: task-list.task-list-filename)
    end method save-file;

Add this code to ``frame.dylan``.

This method is very simple, in that it just calls the method
``save-as-file``, passing it a filename as an argument. The
``save-as-file`` method then does the real work of updating the GUI and
calling the relevant code to save information to disk.

Just like the ``open-file`` method, ``save-file`` takes the gadget used to
invoke it as an argument and returns no values. In the case of the task
list manager the gadget is either ``open-menu-button`` (in the *File* menu
of the application) or ``open-button`` (on the tool bar). The ``save-file``
method sets the following two local variables:

-  ``frame`` The frame of which the gadget argument is a part, so that the
   main application frame can be identified.
-  ``task-list`` This contains the value of the ``frame-task-list`` slot for
   ``frame``. This identifies the instance of ``<task-list>`` that needs to
   be saved to disk.

Note that similar local variables are used in the definition of
``open-file``.

The ``save-file`` method then calls ``save-as-file``, passing it the
following two arguments:

-  The gadget that invoked ``save-file``.
-  The filename associated with the instance of ``<task-list>`` that needs
   to be saved to disk.

Notice that the second of these arguments may be ``#f``, if the task list
has not previously been saved to disk.

The save-as-file method
-----------------------

The code for ``save-as-file`` is as follows:

.. code-block:: dylan

    define method save-as-file
        (gadget :: <gadget>, #key filename) => ()
      let frame = sheet-frame(gadget);
      let task-list = frame-task-list(frame);
      let filename
        = filename
          | choose-file(frame: frame,
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

Add this code to ``frame.dylan``.

Like ``open-file`` and ``save-file``, this method takes a gadget as an
argument and returns no values. This argument is the gadget which is
used to invoke it. In addition, an optional keyword argument, a
filename, can be passed.

This method is a little unusual; as well as being the activate callback
for the ``save-as-menu-button`` (the command *File > Save As* ), it is
also called by the ``save-file`` method.

-  When directly invoked as an activate callback, the ``filename``
   argument is not passed to ``save-as-file``. Instead, the user is
   prompted to supply it. In addition, the ``gadget`` is
   ``save-as-menu-button``.
-  When invoked by ``save-file``, a ``filename`` may be passed, if the
   associated task list has been saved before. In addition, the gadget
   is either ``save-button`` or ``save-menu-button``.

As with ``open-file``, ``save-as-file`` sets three local variables:

-  ``frame`` This is the frame containing the gadget passed as an
   argument.
-  ``task-list`` This contains the value of the ``frame-task-list`` slot for
   ``frame``, and identifies the instance of ``<task-list>`` to be saved.
-  ``filename`` The filename to which the instance of ``<task-list>`` is
   saved.

Unless ``filename`` is passed as an optional argument, the user is
prompted to supply a filename in which the task list information is to
be saved. As with ``open-file``, the ``choose-file`` method is used to do
this. In fact, the call to ``choose-file`` here is identical to the call
to ``choose-file`` in ``open-file``, with the exception of the direction
argument, which is set to ``#"output"``.

The rest of the ``save-as-file`` method deals with saving the task list
information safely. It is similar to the equivalent code in ``open-file``,
and consists of two nested ``if`` statements as shown below.

.. code-block:: dylan

    if (filename)
      if (save-task-list(task-list, filename: filename))
        frame.frame-task-list := task-list;
        refresh-task-frame(frame)
      else
        notify-user(format-to-string("Failed to save file %s", filename),
                    owner: frame)
      end
    end

As with ``open-file``, the clause

.. code-block:: dylan

    if (filename)
      ...
    end

is necessary in case the user cancels the Save file dialog: on
cancelling the dialog, ``save-as-file`` should fail silently with no side
effects.

The second ``if`` statement is more interesting. The body of the ``if``
statement is like the body of the equivalent ``if`` statement in
``open-file`` :

.. code-block:: dylan

    frame.frame-task-list := task-list;
    refresh-task-frame(frame)

This sets the ``frame-task-list`` slot of ``frame`` and then calls
``refresh-task-frame`` to ensure that the correct information is shown on
the screen.

Similarly, the body of the ``else`` clause warns that the task list could
not be saved, when the ``if`` condition does not return true:

.. code-block:: dylan

    notify-user(format-to-string("Failed to save file %s", filename),
                owner: frame)

The interesting part of this ``if`` statement is the ``if`` condition
itself:

.. code-block:: dylan

    save-task-list(task-list, filename: filename)

As well as providing a test for whether the task list frame should be
updated, it actually performs the save operation, by calling the
function ``save-task-list`` with the required arguments.

The function ``save-task-list`` is described in `See The save-task-list
function <callbacks.htm#88460>`_ and the method ``refresh-task-frame`` is
described in `See Updating the user interface <callbacks.htm#94307>`_.

The load-task-list function
---------------------------

The code for ``load-task-list`` is shown below. Because this function does
not use any DUIM code, it is described only briefly.

.. code-block:: dylan

    define function load-task-list
        (filename :: <string>) => (task-list :: false-or(<task-list>))
      let tasks = make(<stretchy-vector>);
      block (return)
        with-open-file (stream = filename, direction: #"input")
          while (#t)
            let name = read-line(stream, on-end-of-stream: #f);
            unless (name) return() end;
            let priority = read-line(stream, on-end-of-stream: #f);
            unless (priority)
              error("Unexpectedly missing priority!")
            end;
            let task = make(<task>, name: name,
                            priority: as(<symbol>, priority));
            add!(tasks, task)
          end
        end
      end;
      make(<task-list>, tasks: tasks, filename: filename)
    end function load-task-list;

Add this code to ``task-list.dylan``.

The function ``load-task-list`` reads a file from disk and attempts to
convert its contents into an instance of ``<task-list>``, which itself
contains any number of instances of ``<task>``. It takes one argument,
the filename, and returns one value, the instance of ``<task-list>``.

This function uses a generic function and a macro from the Streams
library to read information from the file. For full information about
this library, please refer to the *I/O and Networks Library Reference*.

The file format used by the task list manager is very simple, with each
element of a task occupying a single line in the file. Suppose
``load-task-list`` is called on a file containing the following
information:

Wash the dog

medium

Video Men Behaving Badly

high

This would create an instance of ``<task-list>`` whose ``task-list-tasks``
slot was a sequence of two instances of ``<task>``.

-  The first ``<task>`` would have a ``task-name`` of *"Wash the dog"* and a
   ``task-priority`` of ``#"medium"``.
-  The second ``<task>`` would have a ``task-name`` of *"Video Men Behaving
   Badly"* and a ``task-priority`` of ``#"high"``.

The ``task-list-filename`` slot of the ``<task-list>`` is the filename
itself. Note that the ``task-list-modified?`` slot of the ``<task-list>`` is
set to ``#f``, reflecting the fact that the task list is loaded, but
unchanged. This does not have to be done explicitly by ``load-task-list``,
since ``#f`` is the default value of this slot, as you can see from its
definition in `See Defining the underlying data structures for
tasks <callbacks.htm#71186>`_.

The file is opened for reading using the ``with-open-file`` macro. It is
then read a line at a time, setting the local variables ``name`` and
``priority`` with each alternate line. After successfully setting both
``name`` and ``priority``, an instance of ``<task>`` is created, and added to
the stretchy vector tasks using ``add!``. When the end of the file is
reached, ``#f`` is returned and an instance of ``<task-list>`` is created
from ``tasks`` and returned by the function.

Note how the ``as`` method is used to convert a string value such as
``"medium"`` into a symbol such as ``#"medium"``. This is a useful
technique to use when you wish to save and load symbol information in an
application.

The save-task-list function
---------------------------

The code for ``save-task-list`` is shown below. Because this function does
not use any DUIM code, it is described only briefly.

.. code-block:: dylan

    define function save-task-list
        (task-list :: <task-list>, #key filename)
    => (saved? :: <boolean>)
      let filename = filename | task-list-filename(task-list);
      with-open-file (stream = filename, direction: #"output")
        for (task in task-list.task-list-tasks)
          format(stream, "%s\\n%s\\n",
                 task.task-name, as(<string>, task.task-priority))
        end
      end;
      task-list.task-list-modified? := #f;
      task-list.task-list-filename := filename;
      #t
    end function save-task-list;

Add this code to ``task-list.dylan``.

The function ``save-task-list`` takes an instance of ``<task-list>`` as an
argument, and optionally a ``filename``. It then attempts to save the
instance of ``<task-list>`` to the file specified by ``filename``. It
returns a boolean value that indicates whether the file was successfully
saved or not. If filename is not passed as an argument to
``save-task-list`` (in the case where the user has chosen *File > Save* or
clicked the *Save* button when working with a task list file that has
previously been saved), then the ``task-list-filename`` slot of the
``<task-list>`` is used instead.

Like ``load-task-list``, this function uses the Streams library to save
information to a file. For full information about this library, please
refer to the *I/O and Networks Library Reference*. It also uses the
``format`` function from the Format library, which is described in the
same reference.

The file is opened for saving using the ``with-open-file`` macro (just
like ``load-task-list``, but in the opposite direction), A ``for`` loop is
used to save each element in each task to the file. The ``format``
function then writes each element to the file, separated by a newline
character. Note how the ``as`` method is used to convert the
``task-priority`` symbol to a string when saving each priority value: this
is the reverse situation to ``load-task-list``, where a method for ``as``
was used to convert the string to a symbol.

Once every element in the file has been saved, the ``task-list-modified``
slot of the ``<task-list>`` is reset to ``#f``, and the
``task-list-filename`` slot of the ``<task-list>`` is set to the filename
used by ``save-task-list``. This last step is necessary to allow for the
case where the user has chosen the *File > Save As* command to save the
file under a different name.

Finally, ``save-task-list`` returns ``#t`` to indicate that the file has
been successfully saved.

Adding and removing tasks from the task list
--------------------------------------------

This section describes the functions and methods necessary for adding to
the task list and removing tasks from the task list. A total of two
methods and two functions are necessary.

``frame-add-task``

   This prompts the user for the details of a new task and adds it to
   the list.

``frame-remove-task``

   This removes the currently selected task from the list, prompting the
   user before removing it completely.

``add-task``

   This adds an instance of ``<task>`` to an instance of ``<task-list>``.

``remove-task``

   This removes an instance of ``<task>`` from an instance of
   ``<task-list>``.

As with the file handling code, DUIM code and non-DUIM code has been
separated. The methods beginning with ``frame-`` deal with the GUI-related
issues of adding and removing tasks, and the functions deal with the
underlying data structures.

Add the definitions of the methods to ``frame.dylan``, and the
definitions of the functions to ``task-list.dylan``.

DUIM support for adding and removing tasks
------------------------------------------

This section describes the methods necessary to provide support in the
task list manager GUI for adding and removing tasks.

Add the code described in this section to ``frame.dylan``.

The code for ``frame-add-task`` is as follows:

.. code-block:: dylan

    define method frame-add-task (gadget :: <gadget>) => ()
      let frame = sheet-frame(gadget);
      let task-list = frame-task-list(frame);
      let (name, priority) = prompt-for-task(owner: frame);
      if (name & priority)
        let new-task = make(<task>, name: name, priority: priority);
        add-task(task-list, new-task);
        refresh-task-frame(frame);
        frame-selected-task(frame) := new-task
      end
    end method frame-add-task;

The method takes a gadget as an argument and returns no values. The
argument is the gadget which is used to invoke it, which in the case of
the task list manager means either ``add-menu-button`` (in the *Task* menu
of the application) or ``add-button`` (on the tool bar). The
``frame-add-task`` method then sets a number of local variables:

-  ``frame`` The frame containing the gadget passed as an argument.
-  ``task-list`` The value of the ``frame-task-list`` slot for ``frame``.
   This identifies the instance of ``<task-list>`` to which a task is to
   be added.
-  ``name`` The text of the task to be added.
-  ``priority`` The priority of the task to be added.

As with other DUIM methods you have seen, ``frame`` and ``task-list`` are
specified using known slot values about the gadget supplied to
``frame-add-task``, and the frame that contains the gadget. The ``name``
and ``priority`` values are specified by calling the ``prompt-for-task``
method defined in `See Creating a dialog for adding new
items <improve.htm#89811>`_. This method displays a dialog into which
the user types the text for the new task and chooses the priority, both
of which values are returned from ``prompt-for-task``.

Once all the local variables have been specified, the main body of code
for the method, repeated below, is executed.

.. code-block:: dylan

    if (name & priority)
      let new-task = make(<task>, name: name, priority: priority);
      add-task(task-list, new-task);
      refresh-task-frame(frame);
      frame-selected-task(frame) := new-task
    end

This consists of four expressions around which is wrapped an ``if``
statement.

#. The first expression creates a new task from the values of the ``name``
   and ``priority`` local variables.
#. The second expression adds the new task to task list, by calling the
   ``add-task`` function.
#. The third expression refreshes the display of the task list in the
   task list manager, so that the new task is displayed on the screen
   once it has been added.
#. The fourth expression ensures that the new task is selected in the
   task list manager. The frame-selected--task method is described in
   `See Updating the user interface <callbacks.htm#94307>`_.

The ``if`` statement ensures that all the information needed to construct
the new task is specified before the new task is created.

The ``add-task`` function is described in `See Non-DUIM support for adding
and removing tasks <callbacks.htm#59071>`_.

The code for ``frame-remove-task`` is as follows:

.. code-block:: dylan

    define method frame-remove-task (gadget :: <gadget>) => ()
      let frame = sheet-frame(gadget);
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

As with ``frame-add-task``, this method takes the gadget that is used to
invoke it as an argument and returns no values. In the case of the task
list manager, the gadget is either ``remove-menu-button`` (in the *Task*
menu of the application) or ``remove-button`` (on the tool bar). The
``frame-remove-task`` method then sets a number of local variables:

-  ``frame`` The frame containing the gadget passed as an argument.
-  ``task`` The task that is to be removed. The task to be removed is the
   one selected in the list of tasks on screen. The method
   ``frame-selected-task`` is called to determine which task this is.
-  ``task-list`` The value of the ``frame-task-list`` slot for ``frame``.
   This identifies the instance of ``<task-list>`` from which a task is to
   be removed.

The method ``frame-selected-task`` is described in `See Updating the user
interface <callbacks.htm#94307>`_.

Once these local variables have been set, the rest of the code goes
about removing the task. The code consists of three expressions around
which is wrapped an ``if`` statement, as shown below.

.. code-block:: dylan

    if (notify-user(format-to-string
                      ("Really remove task %s", task.task-name),
        owner: frame, style: #"question"))
      frame-selected-task(frame) := #f;
      remove-task(task-list, task);
      refresh-task-frame(frame)
    end

Notice here that the method ``notify-user`` is used as the condition in
the ``if`` statement: if the call to ``notify-user`` returns ``#t``, then the
subsequent expressions are executed. This use of ``notify-user``
illustrates how you can use the method to generate a yes-no question for
the user to answer, by using the ``style:`` init-keyword. You might like
to compare the user of ``notify-user`` in this method with its use in
``open-file`` or ``save-as-file`` ; essentially, the only difference is in
the use of the ``style:`` init-keyword.

If the call to ``notify-user`` returns ``#t``, then three expressions are
executed:

#. The first calls the setter for ``frame-selected-task``, to ensure that no
   items in the task list are selected.
#. The second calls the function ``remove-task``, which removes task from
   ``task-list``.
#. Then, ``refresh-task-frame`` is called to ensure that the task that has
   been removed is no longer displayed in the list of tasks on the
   screen.

The methods defined for ``frame-selected-task`` are described in `See
Updating the user interface <callbacks.htm#94307>`_. The function
``remove-task`` is described in `See Non-DUIM support for adding and
removing tasks <callbacks.htm#59071>`_. The ``refresh-task-frame`` method
is described in `See Updating the user
interface <callbacks.htm#94307>`_.

Non-DUIM support for adding and removing tasks
----------------------------------------------

This section describes the functions necessary for adding an instance of
``<task>`` to a ``<task-list>``, and removing a ``<task>`` from a
``<task-list>``. These functions are called by the callback functions
``frame-add-task`` and ``frame-remove-task``, respectively. Because these
functions do not use any DUIM code, they are described only briefly.

Add the code described in this section to ``task-list.dylan``.

The code for ``add-task`` is as follows:

.. code-block:: dylan

    define function add-task
        (task-list :: <task-list>, task :: <task>) => ()
      add!(task-list.task-list-tasks, task);
      task-list.task-list-modified? := #t
    end function add-task;

This function takes two arguments, a ``<task-list>`` and the ``<task>`` that
is to be added to it, and returns no values. The ``add-task`` function
first adds the ``<task>`` to the end of the sequence bound to the
``task-list-tasks`` slot of the ``<task-list>``, and then sets the
``task-list-modified?`` slot of the ``<task-list>`` to ``#t``, to indicate
that a change in the ``<task-list>`` has occurred.

The code for ``remove-task`` is as follows:

.. code-block:: dylan

    define function remove-task
        (task-list :: <task-list>, task :: <task>) => ()
      remove!(task-list.task-list-tasks, task);
      task-list.task-list-modified? := #t
    end function remove-task;

This function is analogous to ``add-task``. It takes the same arguments,
and returns no values. The function first removes the ``<task>`` from the
``task-list-tasks`` slot of the ``<task-list>``, and then sets the
``task-list-modified?`` slot of the ``<task-list>`` to ``#t``, to indicate
that a change in the ``<task-list>`` has occurred.

Updating the user interface
---------------------------

This section describes a number of miscellaneous methods that are
required for smooth operation of the task list manager. Each of the
methods defined here ensures that the task list manager displays the
correct information and gives the user access to appropriate commands in
any given situation. Here is a list of the methods defined in this
section, together with a brief description of each one:

-  ``initialize`` An ``initialize`` method is provided for ``<task-frame>``
   that ensures information is displayed correctly when the task list
   manager is first displayed. This method is described in `See
   Initializing a new instance of <task-frame> <callbacks.htm#84277>`_.

``frame-selected-task``

   This method returns the task that is currently selected in the task
   list manager. This method is described in `See Determining and
   setting the selected task <callbacks.htm#98481>`_.

``frame-selected-task-setter``

   This is a setter method for frame-selected-task, and is used to
   select or deselect item in the task list manager. This method is
   described in `See Determining and setting the selected
   task <callbacks.htm#98481>`_.

``note-task-selection-change``

   Two methods are defined that deal with updating the GUI whenever a
   change is made to the task selection state. This method is described
   in `See Enabling and disabling buttons in the
   interface <callbacks.htm#42654>`_.

``refresh-task-frame``

   This method can be called to refresh the task frame at any time. This
   method is described in `See Refreshing the list of
   tasks <callbacks.htm#28478>`_.

Each of these methods should be added to the file ``frame.dylan``.

Initializing a new instance of <task-frame>
-------------------------------------------

The code below provides an ``initialize`` method for the class
``<task-frame>``. This simply ensures that the display in a
``<task-frame>`` is refreshed as soon as it is created, and calls any
subsequent methods that may be defined for it (although, in the case of
the task list manager, there are none). While not strictly necessary,
this ``initialize`` method illustrates general good practice when defining
your own classes of frame. If the application was associated with files
of a particular type on disk, then the ``initialize`` method would be
necessary to ensure that tasks were displayed correctly after starting
the task list manager by double-clicking on a file of tasks.

.. code-block:: dylan

    define method initialize
        (frame :: <task-frame>, #key) => ()
      next-method();
      refresh-task-frame(frame);
    end method initialize;

Add the code for this method to ``frame.dylan``.

Determining and setting the selected task
-----------------------------------------

Two methods are used to determine which task is selected in the task
list manager, and to set a specific task in the task list manager:
``frame-selected-task`` and ``frame-selected-task-setter``.

The ``frame-selected-task`` method returns the task that is currently
selected in the task list manager, or ``#f`` if no task is selected. This
method is used by ``frame-remove-task`` to determine which task should be
deleted from the task list. It is also used by
``note-task-selection-change`` to determine whether or not a task is
selected.

.. code-block:: dylan

    define method frame-selected-task
        (frame :: <task-frame>) => (task :: false-or(<task>))
      let list-box = task-list(frame);
      gadget-value(list-box)
    end method frame-selected-task;

The ``frame-selected-task`` method works by determining the ``gadget-value``
of the list box that displays the tasks in the task list manager. The
``gadget-value`` of a collection such as a list box is the selected item.
Notice how you can access the value of a pane in a frame instance in
exactly the same way that you can access the value of a slot in a class
instance; the definition of the pane creates an accessor that is just
like a slot accessor. Recall that the name of the list box in the
definition of the ``<task-frame>`` class is ``task-list``.

A setter method is also defined for ``frame-selected-task``, as shown
below:

.. code-block:: dylan

    define method frame-selected-task-setter
        (task :: false-or(<task>), frame :: <task-frame>)
    => (task :: false-or(<task>))
      let list-box = task-list(frame);
      gadget-value(list-box) := task;
      note-task-selection-change(frame);
      task
    end method frame-selected-task-setter;

This method takes two arguments: the ``task`` to select in the task list
manager, and the ``frame`` to which the task belongs. It returns the task.
The method determines the list box used to display tasks in ``frame``,
and then sets the ``gadget-value`` of that list box to ``task``. Finally,
it calls ``note-task-selection-change``, described below, to update other
parts of the user interface appropriately, such as buttons on the tool
bar.

As with most setter methods, ``frame-selected-task-setter`` is not called
directly. Instead, it is called implicitly by setting a value using
``frame-selected-task``. For example,

.. code-block:: dylan

    frame-selected-task(frame) := #f;

ensures that no tasks are selected in ``frame``.

The ``frame-selected-task-setter`` method is called by two other methods:
``frame-add-task`` (to ensure that the task added is subsequently
selected) and ``frame-remove-task`` (to ensure that no tasks are selected
once a task has been removed from the list). These methods are described
in `See DUIM support for adding and removing
tasks <callbacks.htm#29566>`_.

Add the code for these methods to ``frame.dylan``.

Enabling and disabling buttons in the interface
-----------------------------------------------

The two methods for ``note-task-selection-change`` make a number of
changes to the GUI of the task list manager, to ensure that the correct
information is displayed to the user. In particular, they perform any
changes necessary after an item in the task list has been selected or
deselected. They ensure that the correct priority is displayed in the
radio box, depending on whether there is a task currently selected, and
they also enable or disable the *Remove task* button and its equivalent
command in the *Task* menu, depending on whether there is a task
selected or not (if there is no task selected, then the button and menu
command should both be disabled).

There are two methods defined, one on an instance of ``<task-frame>``,
and one on an instance of ``<gadget>``. The Task List 1 project requires
both of these methods. For the Task List 2 project, however, the first
method requires a slightly different definition, and the second method
is not required at all.

The ``note-task-selection-change`` method defined on ``<task-frame>`` is
called by ``refresh-task-frame``, described on page `See The
refresh-task-frame method is called whenever the list of tasks needs to
be refreshed for whatever reason. This happens most commonly after
adding or removing a task from the list, or loading in a new task list
from a file on disk. The method refresh-task-frame takes an instance of
<task-frame> as an argument and returns no values. For the Task List 1
project the definition is: <callbacks.htm#52283>`_. For the Task List 1
project, the ``note-task-selection-change`` method is defined:

.. code-block:: dylan

    define method note-task-selection-change
        (frame :: <task-frame>) => ()
      let task = frame-selected-task(frame);
      if (task)
        frame.priority-box.gadget-value := task.task-priority;
      end;
      let selection? = (task ~= #f);
      frame.remove-button.gadget-enabled? := selection?;
      frame.remove-menu-button.gadget-enabled? := selection?;
    end method note-task-selection-change;

For the Task List 2 project the ``note-task-selection-change`` method is
defined:

.. code-block:: dylan

    define method note-task-selection-change
        (frame :: <task-frame>) => ()
      let task = frame-selected-task(frame);
      if (task)
        frame.priority-box.gadget-value := task.task-priority;
      end;
      command-enabled?(frame-remove-task, frame) := task ~= #f;
    end method note-task-selection-change;

The method takes an instance of ``<task-frame>`` as an argument, and
returns no values. It works by calling ``frame-selected-task`` to
determine which, if any, task is currently selected, and sets that to a
local variable, ``task``.

The expression

.. code-block:: dylan

    if (task)
      frame.priority-box.gadget-value := task.task-priority;
    end;

sets the gadget value of the ``priority-box`` pane in the task list
manager to the value of the ``task-priority`` slot of the selected task,
if a task is selected. This ensures that if a task is selected, its
priority is displayed correctly beneath the list of tasks. Note that
``priority-box`` may take the same set of values as the ``task-priority``
slot, namely ``#"low"``, ``#"medium"``, and ``#"high"``, so it is
straightforward to make this kind of assignment.

The rest of the method deals with enabling or disabling gadgets that let
the user remove a task from the task list. If there is no task selected,
then ``remove-button`` and ``remove-menu-button`` need to be disabled. If
there is a task selected, then they need to be enabled. This behavior is
achieved by converting the value of the variable ``task``, which can take
a value of ``false-or(<task>)``, into a boolean value, called
``selection?``. This is done in the expression

.. code-block:: dylan

    let selection? = (task ~= #f);

This sets ``selection?`` to the result of performing an inequality
comparison on ``task`` and ``#f``. Thus, if ``task`` is ``#f`` (there is no
task selected), then ``selection?`` is ``#f``, but if ``task`` is an instance
of ``<task>`` (there is a task selected), then ``selection?`` is ``#t``.

The two calls to ``gadget-enabled?`` then set the ``gadget-enabled`` slot of
the appropriate gadgets to the value of ``selection?``, enabling or
disabling each gadget as appropriate.

The second method for ``note-task-selection-change`` is defined for an
instance of ``<gadget>``, as follows:

.. code-block:: dylan

    define method note-task-selection-change
        (gadget :: <gadget>) => ()
      let frame = gadget.sheet-frame;
      note-task-selection-change(frame)
    end method note-task-selection-change;

This takes a gadget as an argument. It simply finds the frame that the
gadget belongs to, and calls the other method for
``note-task-selection-change`` on that frame.

The second method for ``note-task-selection-change`` needs to be used as
the value-changed callback of the ``task-list`` pane in the definition of
``<task-frame>`` ; a value-changed callback is invoked whenever the
``gadget-value`` of a gadget changes. Because the ``gadget-value`` of a list
box is the currently selected item, whenever a different item is
selected in the list box, ``note-task-selection-change`` is called.

In order to achieve this, a small change is needed to the definition of
the task-list pane in ``frame.dylan``. In this definition for the Task
List 1 project, change the line that reads:

.. code-block:: dylan

    activate-callback: not-yet-implemented);

to

.. code-block:: dylan

    value-changed-callback: note-task-selection-change);

and for the Task List 2 project change the line to

.. code-block:: dylan

    value-changed-callback: method (gadget)
      note-task-selection-change(frame) end);

to give a final definition for this pane as follows:

.. code-block:: dylan

    // definition of list
    pane task-list (frame)
      make (<list-box>,
            items: frame.frame-task-list.task-list-tasks,
            label-key: task-name,
            lines: 15,
            value-changed-callback: note-task-selection-change);

Add the code for these methods to ``frame.dylan``.

Refreshing the list of tasks
----------------------------

The ``refresh-task-frame`` method is called whenever the list of tasks
needs to be refreshed for whatever reason. This happens most commonly
after adding or removing a task from the list, or loading in a new task
list from a file on disk. The method ``refresh-task-frame`` takes an
instance of ``<task-frame>`` as an argument and returns no values. For the
Task List 1 project the definition is:

.. code-block:: dylan

    define method refresh-task-frame
        (frame :: <task-frame>) => ()
      let list-box = frame.task-list;
      let task-list = frame.frame-task-list;
      let modified? = task-list.task-list-modified?;
      let tasks = task-list.task-list-tasks;
      if (gadget-items(list-box) == tasks)
        update-gadget(list-box)
      else
        gadget-items(list-box) := tasks
      end;
      gadget-enabled?(frame.save-button) := modified?;
      gadget-enabled?(frame.save-menu-button) := modified?;
      note-task-selection-change(frame);
    end method refresh-task-frame;

However, the Task List 2 project requires a call to ``command-enabled?``,
so the definition is:

.. code-block:: dylan

    define method refresh-task-frame
        (frame :: <task-frame>) => ()
      let list-box = frame.task-list;
      let task-list = frame.frame-task-list;
      let modified? = task-list.task-list-modified?;
      let tasks = task-list.task-list-tasks;
      if (gadget-items(list-box) == tasks)
        update-gadget(list-box)
      else
        gadget-items(list-box) := tasks
      end;
      command-enabled?(save-file, frame) := modified?;
      note-task-selection-change(frame);
    end method refresh-task-frame;

To begin, ``refresh-task-frame`` sets a number of local variables:

-  ``list-box`` The list box used to display the list of tasks in task
   list manager.
-  ``task-list`` The task list currently loaded in the task list manager.
-  ``modified?`` The value of the ``task-list-modified?`` slot of
   ``task-list``.
-  ``tasks`` The sequence of tasks stored in ``task-list``.

Next, the following code is executed:

.. code-block:: dylan

    if (gadget-items(list-box) == tasks)
      update-gadget(list-box)
    else
      gadget-items(list-box) := tasks
    end;

This code ensures that if the items in the list box are the same as the
sequence of tasks in the task list, then the display in the list box is
updated to ensure all the items are displayed correctly. If the items in
the list box are not the same as the sequence of tasks, then the items
in the list box are updated to reflect the current task list. The items
in the list box could be different if a task had been added or removed
from the list, or if a completely new set of tasks had been loaded into
the task list manager.

Lastly, the following three lines

.. code-block:: dylan

    gadget-enabled?(frame.save-button) := modified?;
    gadget-enabled?(frame.save-menu-button) := modified?;
    note-task-selection-change(frame);

ensure that the *Save* button and *File > Save* menu command are enabled
if the task list has been modified, and then any changes that need to be
made to the GUI as a result of changing the selected item are performed,
by calling ``note-task-selection-change``.

Add the code for this method to ``frame.dylan``.

Creating an information dialog
------------------------------

The following function displays a simple dialog box that provides
information about the application. This dialog is displayed when you
choose the *Help > About* menu command.

.. code-block:: dylan

    define function about-task (gadget :: <gadget>) => ()
      notify-user("Task List Manager", owner: sheet-frame(gadget))
    end function about-task;

Exiting the task list manager
-----------------------------

The ``exit-task`` method allows you to exit the task list manager. It is
invoked by choosing *File > Exit*. The definition of this method is
quite simple.

.. code-block:: dylan

    define method exit-task (gadget :: <gadget>) => ()
      let frame = sheet-frame(gadget);
      let task-list = frame-task-list(frame);
      save-file (gadget);
      exit-frame(frame)
    end method exit-task;

Add this method to the file ``frame.dylan``.

The method takes the gadget used to invoke it and returns no values. In
this case, ``exit-task`` is only ever invoked by the ``exit-menu-button``
gadget.

As with many other callbacks in this example, ``exit-task`` sets a number
of local variables:

-  ``frame`` The frame that the gadget argument belongs to.
-  ``task-list`` The task list associated with ``frame``.

The method begins by calling the ``save-file`` method (defined in `See The
save-file method <callbacks.htm#98742>`_) to save the current task list
to disk. This ensures that the user does not lose any work. Next, the
``exit-frame`` generic function is invoked to exit the task list manager
window.

Enhancing the task list manager
===============================

This concludes the tutorial on building application with DUIM. At this
point, you can build and run a functional task list manager, but it is a
very basic application. `See Using Command
Tables <commands.htm#99799>`_, introduces command tables as a way of
defining hierarchies of menu commands. To do this, it re-implements the
menu hierarchy you defined in `See Adding Menus To The
Application <menus.htm#81811>`_, but does not add any new functionality
to the application.

There are many ways that the task list manager could be extended, and
you might like to try experimenting with the code. To begin with, very
little error checking has been written into the application, and you
might like to add some in order to make the task list manager more
robust. For example, it is currently possible to exit the task list
manager and lose any changes in an unsaved list of tasks.

In addition to error checking, there is a wide range of new
functionality you might like to add. A few ideas are listed below:

-  Re-implement the list box and radio box in the main window of the
   task list manager as a table control, so that the priority of each
   task is displayed next to the text for the task.
-  Implement the facility to define categories, so that tasks could be
   assigned categories such as “Home” and “Business”. Categories could
   be listed in the table control alongside priorities.
-  Allow sorting the list of tasks according to a key. Tasks could then
   be sorted by priority or category.
-  Implement the ability to mark tasks as complete.
-  Allow users to add text memos to any task.

This is only a very limited list of ideas. After learning about command
tables in `See Using Command Tables <commands.htm#99799>`_, read through
`See A Tour of the DUIM Libraries <tour.htm#93265>`_ to learn more about
the features that DUIM provides. Then, using the *DUIM Reference Manual*
as your reference source, get coding!
