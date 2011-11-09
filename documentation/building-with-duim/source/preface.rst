*******
Preface
*******

About this manual
-----------------

This manual, *Building Applications using DUIM*, provides an
introduction to developing your own windowed applications using
Open Dylan and, in particular, the interface-building functionality
provided by the DUIM library suite. It is designed to complement
*Getting Started with Open Dylan*, which provides information on
using the Open Dylan development environment, and the *DUIM
Reference Manual*, which provides a complete reference to the DUIM
library suite. You are advised to look at *Getting Started with
Open Dylan* before reading this manual in any depth.

This manual is divided into several parts:

`See Introduction <intro.htm#92880>`_, provides an introduction to the
concepts behind the DUIM libraries, and their intended use.

` <design.htm#77027>`_, through to `See Adding Callbacks to the
Application <callbacks.htm#15598>`_, provide an extended example of how
to use DUIM to design the user interface to an application. A simple
working application is developed from first principles, and this is used
as an illustration of some of the most useful features provided by the
DUIM libraries. ` <design.htm#77027>`_ provides an initial design for
the application, `See Improving The Design <improve.htm#63710>`_
improves on this initial design. `See Adding Menus To The
Application <menus.htm#81811>`_ shows you how you can add a menu system
to an application and `See Adding Callbacks to the
Application <callbacks.htm#15598>`_ demonstrates how to give the
application some useful functionality. `See Using Command
Tables <commands.htm#99799>`_, introduces the concept of command tables,
by re-implementing some of the functionality already described in `See
Adding Menus To The Application <menus.htm#81811>`_. For reference, the
full source code of the application described in these chapters is
provided in `See Source Code For The Task List
Manager <source.htm#77017>`_.

`See A Tour of the DUIM Libraries <tour.htm#93265>`_, provides an
overall tour of what is available in the suite of DUIM libraries. It
provides much less detail than the chapters covering application
development, but covers a broader spectrum of functionality. This
chapter can be seen as a general introduction to the material covered in
the *DUIM Reference Manual*.

The material provided in `See A Tour of the DUIM
Libraries <tour.htm#93265>`_ is reasonably independent from the material
provided in Chapters ` <design.htm#77027>`_ to `See Adding Callbacks to
the Application <callbacks.htm#15598>`_, and if you wish, you can read
through the tour before looking at the example application. Whichever
order you approach them in, you should expect some repetition of subject
matter, however.

Running examples in this manual
-------------------------------

Naturally, when developing your own DUIM applications, you create, edit,
and compile files of source code, and organize them as projects based on
Dylan libraries and modules, just as you would when developing Dylan
code that uses any other library. When developing your application, you
can also take advantage of the development environment to make this
process smoother, and to execute sections of code using the interactor.
Many of the examples in this manual can be run directly from the
interactor. Furthermore, this manual assumes that you are reasonably
familiar with the development environment provided by Open Dylan.
If you are not, please refer to the *Getting Started with Open
Dylan* manual.

When developing your own projects using the New Project wizard, new
modules that use the DUIM library, and any other relevant libraries are
created for you. You may also like to use the Dylan Playground to
experiment safely with your development code while keeping your
project-specific modules clean. You can open the Dylan Playground by
choosing *Tools > Open Playground* from the Dylan the main window.

The full source code for both versions of the application is provided as
part of the Open Dylan installation. To load them into the
environment, choose *Tools > Open Example Project* and look in the
Documentation category, at the examples labeled Task List.

Further reading
---------------

For more information about DUIM, you should refer to the *DUIM Reference
Manual*. This provides complete reference material on all the libraries
and modules provided by DUIM. A wide variety of examples are also
provided as part of the standard installation. These can be loaded into
the environment by choosing *Tools > Open Example Project* from the main
window.
