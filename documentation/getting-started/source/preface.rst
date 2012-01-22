*******
Preface
*******

This preface describes the *Getting Started with Open Dylan*
manual. It discusses the purpose of the manual and the conventions it
uses. It also gives a brief description of each chapter in the manual.

About this guide
================

This guide explains how to use Open Dylan to develop and deliver Dylan
applications.  There are three relatively independent sections
covering command-line usage, Dylan Interaction Mode for Emacs (DIME),
and then the IDE (Windows only) which makes up the bulk of this
document.

Conventions in this guide
=========================

This guide uses only a few special conventions.

Menus and menu items are printed in bold type as follows:

    **Save As**

Instructions involving menu selection are given like this:

    Choose **Build > Run**.

This means “Choose **Run** from the **Build** menu”.

Where submenus are involved, the convention is the same:

    Choose **Start > Programs > Open Dylan**

This means “Choose the Start menu, then the Programs submenu, and then
choose Open Dylan.”

Instructions involving buttons are given like this:

    Click **Cancel**.

Instructions involving radio buttons or check boxes are given like this:

    Check the “Make a personal copy” box.

    Select the “Maximize interactor” radio button.

The Dylan Reference Manual
==========================

The *Dylan Reference Manual* (Shalit, 1996) is published by Addison
Wesley Developers’ Press, ISBN 0-201-44211-6. In this guide, we refer to
the *Dylan Reference Manual* as “the DRM”.

Chapters in this guide
======================

The first two chapters in this guide cover how to use Open Dylan from
the command-line and how to use DIME (Dylan Interaction Mode for
Emacs).  The remainder of the guide is dedicated to doing development
with the IDE (on Windows only).

The early chapters on the IDE give you a quick tour of the
application development cycle under Open Dylan, using the Reversi
example for illustration. In these early chapters we come across most of
the Open Dylan development tools. Later chapters examine those
tools more directly, and provide a broader view of the development
process in Open Dylan.

:doc:`console`, shows how to use Open Dylan from the command-line.

:doc:`quick-start`, shows how to build a
standalone executable application in Open Dylan. It discusses the
Open Dylan main window and the project window.

:doc:`expanding`, shows how to make changes to your application
sources and how to debug compile-time and run-time errors. It discusses
the project window, the debugger, the editor, and the browser.

:doc:`model`, gives an overview of the programming model in
Open Dylan.

:doc:`projects`, discusses the New Project wizard, and looks at
the project window in more detail.

:doc:`browsing`, shows how you can examine the sources of a
project, and run-time values in an application, using the browser.

:doc:`debug`, returns to the debugger and studies it in more
detail.

:doc:`remotedbg`, describes how to debug a Dylan application
running on another machine.

:doc:`coloring`, describes the editor’s facility for coloring
source code to show the degree to which it has been optimized.

:doc:`delivery`, shows how you can package an application with
everything necessary to deliver it to customers as a stand-alone
product.
