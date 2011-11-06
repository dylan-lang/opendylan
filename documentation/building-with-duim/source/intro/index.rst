************
Introduction
************

Overview of the DUIM libraries
------------------------------

The Dylan User Interface Manager (DUIM—pronounced “dwim”) is a
Dylan-based programming interface that provides a layered set of
portable facilities for constructing user interfaces.

While DUIM provides an API to user interface facilities for the Dylan
application programmer, it is not itself a window system toolkit. DUIM
uses the service of the underlying window system and UI toolkits as much
as possible. DUIM’s API is intended to insulate the programmer from most
of the complexities of portability, since the DUIM application need only
deal with DUIM objects and functions regardless of their operating
platform (that is, the combination of Dylan, the host computer, and the
host window environment).

DUIM is a high level library that allows you to concentrate on how the
interface looks and behaves rather than how to implement it on a
particular platform. It abstracts out many of the concepts common to all
window environments. The programmer is encouraged to think in terms of
these abstractions, rather than in the specific capabilities of a
particular host system. For example, using DUIM, you can specify the
appearance of output in high-level terms and those high-level
descriptions are turned into the appropriate appearance for the given
host. Thus, the application has the same fundamental interface across
multiple environments, although the details will differ from system to
system.

The DUIM programming model
--------------------------

The Dylan User Interface Manager (DUIM) provides a complete functional
interface so that you can use Open Dylan to develop and build
graphical user interfaces (GUIs) for your applications. It comprises a
suite of libraries, each of which provides a specific set of components
necessary for developing a GUI.

DUIM has a simple overall design, ensuring that developers who are
relatively new to Dylan can produce results quickly and effectively. At
the same time, the design is robust enough to allow more experienced
developers to extend and use DUIM in non-standard ways when required, in
order to produce specific behavior.

Because it is completely written in Dylan, DUIM is able to harness all
the power of the Dylan language. This means not only the clean
object-oriented design of Dylan, but also the power of functionality
such as macros and collections, together with the concise nature of the
language syntax. This makes it easy to implement quite complicated GUI
designs from the ground up, using small, clear pieces of code. This is
in contrast to other GUI design libraries that have to rely on a much
more verbose underlying language, such as C, which in turn leads to more
complex GUI code that is harder to improve upon and maintain.

In the functionality that it provides, DUIM has a number of goals:

It should be as easy to use as possible.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As well as providing the minimum feature set necessary to build a GUI,
DUIM provides functionality that lets you use common GUI features
easily.

It should be as compact as possible.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

DUIM does not provide *so much* functionality that either you, or the
environment, is swamped in complexity.

It should be as portable as possible.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It should be relatively easy to compile code in, and for, as many
different hardware and software configurations as possible.

DUIM provides support for all the controls available in every modern GUI
environment, and also allows you to develop your own controls as
required. As far as possible, DUIM code is not specific to any
particular platform, and whenever possible, controls native to the
target environment are used in the resulting executable. This has two
important consequences for your code:

By using controls native to the target environment, it is easy to
develop an application that has the correct look and feel for your
platform.

It enables DUIM code to be compiled and run on any platform for which a
DUIM backend has been implemented.

A DUIM interface is built from *frames* ; each window in your
application is represented by a frame. Each frame contains a hierarchy
of *sheets*, in which each sheet represents a unique piece of your
window (the menu bar, buttons, and so on). DUIM also handles the event
loop for you, allowing you to write methods to handle just the events
you wish to treat specially.

The components of the sheet structure itself consist of three types of
DUIM object:

- *Gadgets*, which are discrete GUI controls such as buttons, panes, and
  menus. These are the basic behavioral element of a GUI, and provide
  methods to handle events such as mouse clicks.

- *Layouts*, which are controls that, rather than having a physical
  appearance on screen, describe the arrangement of the sheets that are
  their children.

- *User-defined sheets*, which are implemented by you rather than by DUIM
  itself.

To implement a user-defined sheet, you create a new class and write
methods to handle the different events that it receives, such as
repainting itself, supporting mouse events, or handling the clipboard.

To develop an application using DUIM, you typically have to define a
number of classes of frame (one for every kind of window or dialog in
your application). The definition of each frame class contains a
description of the sheet hierarchy that describes the contents of the
frame, together with any slots and initial values that are required by
the frame class. Once the frame classes are defined, you need to define
callback functions that are invoked when certain events occur within the
scope of the sheet hierarchy, such as mouse button clicks or textual
input. These callback functions encapsulate the behavior of the
application.

` <design.htm#77027>`_, to `See Using Command
Tables <commands.htm#99799>`_, provide an extended tutorial that
illustrates the basic and most common principles involved in building a
GUI for a simple application.

As well as a rich set of GUI controls, DUIM provides support for the
following features that are required in GUI design:

-  *Dialogs* You can build your own dialogs, wizards, and property
   frames using pre-supplied DUIM classes. In addition, a number of
   convenience functions are provided which let you add common dialogs
   (such as file requesters) to your GUI without having to design the
   dialog from scratch.
-  *Graphics* DUIM provides portable models for colors, fonts, images,
   and generic drawing operations.
-  *Events* DUIM provides portable models for keyboard handling and
   mouse handling, to simplify the process of writing your own event
   handling routines.
-  *Layouts* DUIM makes it easy to lay out groups of controls in a
   variety of standard ways, letting you arrange controls in columns,
   rows, or tables. DUIM takes care of any necessary calculations,
   ensuring that the size of each control, and the spacing between
   controls, is correct, without the need for any explicit layout
   calculation on your part.


