********************
Improving The Design
********************

The simple layout hierarchy described in `See Creating the basic sheet
hierarchy <design.htm#23252>`_ has a number of problems associated with
it, all of which revolve around the fact that the task list manager does
not yet look very much like a standard Windows application. Although it
is a simple design that does not warrant a complicated user interface,
the design you have already seen looks more like a dialog box than an
application window.

This section shows you how to improve on the basic design, adding a menu
bar and replacing the buttons with a proper tool bar. It also shows you
how to move the text field into a separate dialog that pops up when you
click the *Add task* button in the tool bar.

From this point on, the interface is defined more formally, using
frames. Up to now, the layout hierarchy has been presented informally,
and you have used *contain* to display the layout interactively. This is
fine for code that you want to evaluate once only, perhaps using the
interactor, but for permanent code, a more rigorous framework is
preferable.

Defining a project
------------------

From this point on, you should put the code for the task list manager
into a project, rather than evaluating pieces of code using the
interactor. Please refer to the *Getting Started with Open Dylan*
for complete details on how to manage projects using the environment;
this section provides brief details that tell you how to create a
project specifically for the task list manager application. Use the New
Project wizard to create a GUI application, and call the project
*task-list* when prompted. The New Project wizard offers the option of
generating template source code to help you get started. For this of
this example, you must ensure that this option is switched *off* (this
is the default setting).

Two versions of the task list manager are included with Open Dylan,
so that you can load the code straight into the environment if you wish.
These are available in the Open Example Project dialog, in the
Documentation category. You can display this dialog by choosing *Tools >
Open Example Project* from the environment. The two versions included
represent the state of the task list manager at the end of `See Adding
Callbacks to the Application <callbacks.htm#15598>`_, and at the end of
`See Using Command Tables <commands.htm#99799>`_. *Please note that both
projects have the same name within the source code—* *task-list* *—and
you should not load them both into the environment at the same time.*

The number of source code files in a given project, and the names you
give them, is entirely up to you. For the purposes of this example, you
will use the files suggested by the New Project wizard. When you use the
New Project wizard, Open Dylan will create a number of files for a
project named *task-list*.

*module.dylan*, *library.dylan*
''''''''''''''''''''''''''''''''

These files define the library and modules for the project. For the
purposes of this application, you can ignore these files.

*task-list.dylan*
'''''''''''''''''

Add non-GUI-specific code to this file.

Finally, you need to create the following new file using *File > New*,
and add it to the project using the *Project > Insert File* command.
Make sure that this file is the last one listed in the project window.

*frame.dylan* Add the GUI-specific code to this file.

Starting the application
------------------------

As you add source code to the files in your project, there will be times
when you want to build the project to test it. This section defines some
methods that let you run the application in a clean way. Add these
methods to *frame.dylan*.

The frame class that is used to implement the task list manager is
called *<task-frame>*. This class will be introduced in `See Defining a
new frame class <improve.htm#66956>`_. You can define a method to create
an instance of *<task-frame>* as follows:

define method start-task () => ()

let frame

= make(<task-frame>);

start-frame(frame);

end method start-task;

This method is provided as a convenient way to create the frame and then
start its event loop. It returns when the event loop shuts down.

.. note:: Obviously, you should not call this method until you have
   defined a frame class called *<task-frame>*.

Finally, you can start the application with the following method, and
its subsequent call:

define method main (arguments :: <sequence>) => ()

// handle the arguments

start-task();

end method main;

begin

main(application-arguments()) // Start the application!

end;

Make sure that this is the very last definition in the file
*frame.dylan*, and remember that *frame.dylan* should itself be the
last file listed in the project window.

Once you have added these methods to your code, you can compile and link
the code, and run the application to test it, using the appropriate
commands in the Dylan environment.

Note that, unlike languages such as C, Dylan does not insist on a single
entrance point to an application such as the one given here. All the
same, it is still good practice to define one if you can. The main
difference between the use of the method *main* here, and the use of the
*main* function in C, is in the arguments that need to be passed. In C,
you need to pass two generic arguments: *argc*, which specifies the
number of arguments you are passing, and *argv*, an array of strings
that define the arguments themselves. In Dylan, however, you only need
to pass the second of these arguments; since any Dylan collection
already knows its own size, you do not need to pass the number of
arguments as an additional parameter.

Adding a default callback
-------------------------

Nothing is more frustrating than designing a user interface that does
not respond to user input. Although, in the early stages at least, the
user interface does nothing particularly useful, you can at least define
a “not yet implemented” message that can be used until you define real
behavior for the application.

The definition of the function that gives you this default behavior is
as follows:

define function not-yet-implemented (gadget :: <gadget>) => ()

notify-user("Not yet implemented!", owner: sheet-frame(gadget))

end function not-yet-implemented;

Add this function to *frame.dylan*.

You can call this function from any gadget in the task list manager by
defining it as the activate callback for each gadget. There are several
types of callback, and this is the type that is used most in the task
list manager. You can define the activate callback for any gadget using
the *activate-callback:* init-keyword. More information about callbacks
is given in `See Adding Callbacks to the
Application <callbacks.htm#15598>`_, in which some real callbacks are
defined, to make the task list manager do something more substantial.

Defining a new frame class
--------------------------

To begin with, define a frame class using the layout hierarchy you have
already created. Although it might seem redundant to implement an
inelegant layout again, it is easier to illustrate the basic techniques
using a design you are already familiar with. In addition, there are
several elements in the design that can be reused successfully.

Add the code described in this section to *frame.dylan*.

Defining a new class of frame is just like defining any Dylan class,
except that there are several extra options available beyond the slot
options normally available to *define class*. Each of these extra
options lets you describe a particular aspect of the user interface. To
define the new frame class, use the following structure:

define frame <task-frame> (<simple-frame>)

// definitions of frame slots and options go here

end frame <task-frame>;

In this case, *<task-frame>* is the name of the new class of frame,
and*<simple-frame>* is its superclass. Like ordinary Dylan classes,
frame classes can have any number of superclasses, with multiple
superclasses separated by commas. The superclass of any “standard” frame
is usually *<simple-frame>*. If you were designing a dialog box, its
superclass would be *<dialog-frame>*. If you were designing a wizard,
its superclass would be *<wizard-frame>*.

Adding slots to a frame class is exactly the same as adding slots to a
standard Dylan class. You can define slot names, init-keywords,
init-functions, default values, and so on. For this example, you are not
defining any slots.

Each user interface element in the new class of frame is specified as a
pane with a name and a definition. A pane is a sheet within a layout,
and you can think of panes as sheets that represent concrete classes in
an interface (as opposed to abstract classes). In effect, specifying a
pane allows you to group together existing gadgets into some meaningful
relationship that effectively creates a new gadget, without actually
defining a gadget class.

The name is used to refer to the pane, both from within the frame
definition itself, and from other code. The pane definition includes
code to create the interface element. A pane specification also includes
a place to declare a local variable that can be used within the pane’s
definition to refer to the surrounding frame.

The following code fragment defines the two buttons, the text field, the
radio box, and the list box from the initial design:

pane add-button (frame)

make(<push-button>, label: "Add task",

activate-callback: not-yet-implemented);

pane remove-button (frame)

make(<push-button>, label: "Remove task",

activate-callback: not-yet-implemented);

pane task-text (frame)

make(<text-field>, label: "Task text:",

activate-callback: not-yet-implemented);

pane priority-box (frame)

make (<radio-box>, label: "Priority:",

items: #("High", "Medium", "Low"),

orientation: #"vertical",

activate-callback: not-yet-implemented);

pane task-list (frame)

make(<list-box>, items: #(), lines: 15,

activate-callback: not-yet-implemented);

Note that the definition of each element is identical to the definitions
included in the original layout described in `See Creating the basic
sheet hierarchy <design.htm#23252>`_ (except that activate callbacks
have been added to the code). Adding *(frame)* immediately after the
name of each pane lets you refer to the frame itself within the frame
definition using a local variable. This means that you can refer to any
pane within the frame using normal slot syntax; that is, a pane called
*my-pane* can be referred to as *frame.my-pane* throughout all of the
definition of the frame class. This ability is essential when you come
to layout each pane in the frame itself.

In addition, you need to define the layout in which to place these
panes. This is itself just another pane, and its definition is again
identical to the original layout described in `See Creating the basic
sheet hierarchy <design.htm#23252>`_, with one exception; rather than
defining each element explicitly, you just include a reference to the
relevant pane that you have already defined using normal slot syntax,
thus:

pane task-layout (frame)

horizontally ()

frame.task-list;

vertically ()

horizontally ()

frame.task-text;

frame.add-button;

end;

frame.remove-button;

frame.priority-box;

end;

end;

To describe the top-level layout for the frame, you need to refer to
this pane using the *layout* option, as follows:

layout (frame) frame.task-layout;

You actually have a certain amount of freedom when choosing what to
define as a pane in the definition of a frame class. For example, the
layout in the *task-layout* pane actually contains a number of
sub-layouts. If you wanted, each one of these sub-layouts could be
defined as a separate pane within the frame definition. Note, however,
that you only have to “activate” the top-most layout; there should only
be one use of the *layout* option.

Similarly, you are free to use whatever programming constructs you like
when defining elements in your code. Just as in the earlier examples,
you could define the layouts with explicit calls to *make*, rather than
by using the *horizontally* and *vertically* macros. Thus, the following
definition of *task-layout* is just as valid as the one above:

pane task-layout (frame)

make(<row-layout>,

children: vector(frame.task-list,

make(<column-layout>,

children:

vector(make(<row-layout>,

children:

vector

(frame.task-text,

frame.add-button)

)))));

Notice that this construct is rather more complicated than the one using
macros!

Throughout this section, you may have noticed that you can identify a
sequence of steps that need to occur inside the definition of a frame.
It is good practice to keep this sequence in mind when writing your own
frame-based code:

Define the content panes
''''''''''''''''''''''''

Define the layout panes
'''''''''''''''''''''''

Use the *layout* option
'''''''''''''''''''''''

If you glue all the code defined in this section together, you end up
with the following complete definition of a frame class.

define frame <task-frame> (<simple-frame>)

pane add-button (frame)

make(<push-button>, label: "Add task",

activate-callback: not-yet-implemented);

pane remove-button (frame)

make(<push-button>, label: "Remove task",

activate-callback: not-yet-implemented);

pane task-text (frame)

make(<text-field>, label: "Task text:",

activate-callback: not-yet-implemented);

pane priority-box (frame)

make(<radio-box>, label: "Priority:",

items: #("High", "Medium", "Low"),

orientation: #"vertical",

activate-callback: not-yet-implemented);

pane task-list (frame)

make (<list-box>, items: #(), lines: 15,

activate-callback: not-yet-implemented);

pane task-layout (frame)

horizontally ()

frame.task-list;

vertically ()

horizontally ()

frame.task-text;

frame.add-button;

end;

frame.remove-button;

frame.priority-box;

end;

end;

layout (frame) frame.task-layout;

keyword title: = "Task List Manager";

end frame <task-frame>;

Note the addition of a *title:* keyword at the end of the definition.
This can be used to give any instance of the frame class a title that is
displayed in the title bar of the frame’s window when it is mapped to
the screen.

At this stage, the application still has no real functionality, and
there is no improvement in the interface compared to the initial design,
but by defining a frame class, the implementation is inherently more
robust, making it easier to modify and, eventually, maintain.

If you want to try running your code, remember that you need to define
some additional methods to create a frame instance and exit it cleanly.
Methods for doing this were provided in `See Starting the
application <improve.htm#17910>`_. If you define these methods now, you
can create running versions of each successive generation of the
application as it is developed.

Adding a tool bar
-----------------

So far, you have seen how to experiment interactively to create an
initial interface design. You have also seen how you can take that
initial design and turn it into a more rigorous definition, for use
within project source code, using a frame class. However, the design of
the interface still leaves a lot to be desired, and the application
still does not do anything. In this section, you start to look at
improving the overall design of the interface.

To begin with, add a tool bar to the interface of the application. Most
modern applications have a tool bar that runs along the top edge of the
main application window, beneath the application menu bar. Tool bars
contain a number of buttons that give you quick access to some of the
most common commands in the application. Each button has a label that
designates its use, or, more often, a small icon. Although you have
already added buttons to the interface that perform important tasks,
they have the appearance of buttons in a dialog box, rather than buttons
in the main window of an application. The solution is to use a tool bar.

Adding a tool bar to the definition of a frame class is very similar to
defining the overall layout of the panes in a frame class. You need to
create the tool bar as a pane in the frame definition, and then
incorporate it using the *tool-bar* clause, as shown below:

pane task-tool-bar (frame)

make(<tool-bar>, child: ...);

// more definitions here

tool-bar (frame) frame.task-tool-bar;

A tool bar has a layout as its child, and each button in the tool bar is
defined as a child of that layout. You can either define each button
within the definition of the tool bar itself, or, more appropriately,
define each button as a pane in the frame, and then refer to the names
of these panes in the tool bar definition.

In fact, the buttons you defined in the earlier interface design can be
used just as easily in a tool bar as they can within the main layout of
the application itself. However, first you must remove the buttons from
the task-layout pane of the definition of *<task-frame>*. (If you fail
to do this, DUIM attempts to use the same buttons in two different parts
of the interface, with undefined results.) A complete definition of a
simple tool bar containing two buttons is as follows:

pane task-tool-bar (frame)

make(<tool-bar>, child: horizontally ()

frame.add-button;

frame.remove-button

end);

// more definitions here

tool-bar (frame) frame.task-tool-bar;

A tool bar that only contains two buttons is on the lean side, however,
so let’s add two more buttons to open a file and save a file to disk.

pane open-button (frame)

make(<push-button>, label: "Open file",

activate-callback: not-yet-implemented);

pane save-button (frame)

make(<push-button>, label: "Save file",

activate-callback: not-yet-implemented);

// more definitions here

pane task-tool-bar (frame)

make(<tool-bar>,

child: horizontally ()

frame.open-button;

frame.save-button;

frame.add-button;

frame.remove-button

end);

// more definitions here

tool-bar (frame) frame.task-tool-bar;

More commonly, an icon is used to label buttons in a tool bar, rather
than a text label. You can do this by supplying an instance of *<image>*
to the *label:* init-keyword when you define the button, rather than an
instance of *<string>*.

So now the application has a tool bar. Somewhat oddly, it does not yet
have a menu bar or a system of menus — most tool bars represent a subset
of the commands already available from the application’s menu system. A
menu system is added to the task list manager in `See Adding Menus To
The Application <menus.htm#81811>`_.

Adding a status bar
-------------------

As well as a tool bar, most applications have a status bar. This is a
bar that runs along the bottom edge of the main application window, and
displays information about the current status of the application. At its
most basic, a status bar provides a label that displays text of some
sort. In many applications, status bars contain a number of different
fields, providing a wide range of functionality. At their most complex,
a status bar may have several different labels that display information
about the current state of the application, and labels that display help
for the currently selected menu command.

It is worth adding a very simple status bar to the task list
application. This contains a label that could eventually be used to
display the name of the file currently loaded into the application.
Adding a status bar to the definition of a frame class is very similar
to adding a tool bar: you need to define a pane that describes the
status bar, and then you need to incorporate it using the *status-bar*
clause.

pane task-status-bar (frame)

make(<status-bar>, label: "Task Manager");

// more definitions here

status-bar (frame) frame.task-status-bar;

Now you have added a status bar to the application. The next step is to
glue all the pieces together once again to create your modified frame
design.

Gluing the new design together
------------------------------

In improving the initial design of the application, you have added a
tool bar and a status bar. Adding a tool bar, in particular, has
obviated the need for some of the elements that you added to the earlier
version of the frame design. In this section, you throw away those
elements that are no longer needed, and add in the new elements, to
create a new, improved design for the frame class.

One part of the initial design you have not yet improved on is the radio
box that shows the priority of any task in the list. Ideally, rather
than using a radio box, you would display the priority of each task
alongside the task itself, within the list box. For now, however, keep
the radio box.

pane priority-box (frame)

make (<radio-box>,

items: $priority-items,

orientation: #"horizontal",

label-key: first,

value-key: second,

value: #"medium",

activate-callback: not-yet-implemented);

Notice that the orientation is no longer constrained to be vertical. In
the new design, a horizontal radio box looks better. By default, the
orientation of a radio box is horizontal, so you could just completely
remove the initialization of the *orientation:* init-keyword from the
definition of the pane. In general, though, if you care about the
orientation of a gadget, you should specify it explicitly, so leave the
init-keyword in the pane definition, and change its value, as shown
above.

Next, notice that the items are now specified using a named constant,
rather than by embedding literals in the pane definition. The definition
of this constant is as follows:

define constant $priority-items

= #(#("Low", #"low"),

#("Medium", #"medium"),

#("High", #"high"));

Add the definition for this constant to *frame.dylan*.

Using lists of string and symbol values in this constant lets you assign
values to the individual components of the radio box elegantly, in
conjunction with the other improvements to the definition of
*priority-box*.

The *label key* is a function which is passed an entry from the sequence
and returns a string to use as the label.
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Assigning *first* to the label key of *priority-box* ensures that the
first element from each sub-list of *$priority-items* (the string) is
used as the label for the appropriate item. Thus, the first button in
priority box is labeled “Low”.

The *value key* is a function which is passed an entry and returns the
logical value of the entry.
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Assigning *second* to the value key of *priority-box* ensures that the
second element from each sub-list of *$priority-items* (the symbol) is
used as the value for the appropriate item. Thus, the first button in
priority box has the value *#"low"*.

Lastly, *priority-box* is given a default value: *#"medium"*. This
ensures that the button labeled “Medium” is selected by default whenever
*priority-box* is first created.

The definitions for *add-button*, *remove-button*, and *task-list*
remain unchanged from the initial design. In addition, you need to add
the definitions for *open-button* and *save-button* described in `See
Adding a tool bar <improve.htm#32725>`_.

You also need to add in the definitions for the tool bar and status bar
themselves, as described in `See Adding a tool bar <improve.htm#32725>`_
and `See Adding a status bar <improve.htm#26367>`_.

The definition for *task-layout* has become much simpler. Because you
have added buttons to the tool bar, the main layout for the application
has reduced to a single column layout whose children are *task-list* and
*priority-box*.

The definition for the new design of the frame class now looks as
follows (button definitions vary slightly for the Task List 2 project,
see `See A task list manager using command tables <source.htm#52969>`_):

define frame <task-frame> (<simple-frame>)

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

// frame title

keyword title: = "Task List Manager";

end frame <task-frame>;

Note that this definition does not incorporate the original *task-text*
pane defined in `See Defining a new frame class <improve.htm#66956>`_.
In fact, this part of the original interface is handled differently in
the final design, and is re-implemented in `See Creating a dialog for
adding new items <improve.htm#89811>`_.

Creating a dialog for adding new items
--------------------------------------

You may be wondering what has happened to *task-text*, the text field
in which you typed the text of each new task. In the new design, this is
moved to a new dialog box that is popped up whenever you choose a
command to add a new task to the list. This section shows you how to
define this dialog.

The method *prompt-for-task* below creates and displays a dialog that
asks the user to type the text for a new task. The definition of
*task-text* is very similar to the definition you provided in the
initial design, with the exception that the activate callback exits the
dialog, rather than calling the *not-yet-implemented* function.

The dialog box created by the prompt-for-task method

.. figure:: ../images/new-task.png
   :align: center

The method takes two keyword arguments: a title, which is assigned a
value by default, and an owner, which is used as the owner for the
dialog that is displayed by *prompt-for-task*. Note that the title is
never explicitly set by any calls to *prompt-for-task* in the task list
manager; it is provided here as an illustration of how you can provide a
default value for a keyword argument, rather than requiring that it
either always be passed in the call to the method, or that it be
hard-wired into the code.

The method returns two values: the name of the new task, that is, the
text that the user types into the text field, and the priority of the
new task.

Add this method to *frame.dylan*.

.. note: The definition of the *prompt-for-task* method uses the
   *<priority>* type. Note that this type is defined in `See Defining the
   underlying data structures for tasks <callbacks.htm#71186>`_. Until the
   relevant code in `See Defining the underlying data structures for
   tasks <callbacks.htm#71186>`_ is added to your project, any attempt to
   build it will generate a serious warning.

define method prompt-for-task

(#key title = "Type text of new task", owner)

=> (name :: false-or(<string>),

priority :: false-or(<priority>))

let task-text

= make(<text-field>,

label: "Task text:",

activate-callback: exit-dialog);

let priority-field

= make(<radio-box>,

items: $priority-items,

label-key: first,

value-key: second,

value: #"medium");

let frame-add-task-dialog

= make(<dialog-frame>,

title: title,

owner: owner,

layout: vertically ()

task-text;

priority-field

end,

input-focus: task-text);

if (start-dialog(frame-add-task-dialog))

values(gadget-value(task-text), gadget-value(priority-field))

end

end method prompt-for-task;

Notice that the dialog used in the *prompt-for-task* method is created
inline within the method definition. In this particular case, the dialog
is only ever needed within the context of *prompt-for-task* and so it is
not necessary to use *define frame* to create a distinct class of frame
specifically for this dialog.

Note also that *OK* and *Cancel* buttons are generated automatically for
a dialog box; you do not need to define them explicitly.

Later on, the activate callback you define for the *add-button* pane
calls this method, then inserts the return value into the list
*task-list*.
