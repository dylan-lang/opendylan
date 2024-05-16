*************************
Open Dylan Hacker's Guide
*************************

Welcome, Dylan hacker!

This document is about how to make changes to the ``opendylan`` repository. For
libraries (including ones that are pulled into ``opendylan`` as submodules)
just make your changes, run the test suite, and submit a pull request to the
appropriate repository.

Aside from `dylan-lang <https://github.com/dylan-lang>`_ there are two
other GitHub organizations that may be of interest:

* `dylan-foundry <https://github.com/dylan-foundry>`_ is Bruce
  Mitchener's large collection of Dylan libraries.

* `dylan-hackers <https://github.com/dylan-hackers>`_ contains
  repositories of historical interest, such as old Harlequin Dylan
  documents and `gwydion <https://github.com/dylan-hackers/gwydion>`_,
  the CMU implementation of Dylan which is no longer maintained.

Looking for Ideas?
==================

If you're looking for ideas on how you can contribute to Open Dylan or
what others might find useful, please see our `list of projects in the
Open Dylan wiki <https://github.com/dylan-lang/opendylan/wiki>`_ or
check out the list of `bugs labeled Easy
<https://github.com/dylan-lang/opendylan/labels/Easy>`_.  If you have
something in mind that isn't there, feel free to :doc:`talk with us
</community/index>` or add it to the list.

Making Changes
==============

The `Open Dylan sources <https://github.com/dylan-lang/opendylan>`_ are hosted
on GitHub, along with sources for the `opendylan.org web site
<https://github.com/dylan-lang/website>`_ and many other repositories.  If you
don't yet have a GitHub account and ssh keys, now is a good time to get them.

To checkout the main "opendylan" repository::

    git clone --recursive https://github.com/dylan-lang/opendylan
    cd opendylan
    git config blame.ignoreRevsFile .git-blame-ignore-revs  # (optional)


You'll want to fork this repository so you can push changes to your
fork and then submit pull requests.

In general, when making changes to the compiler and core libraries you'll use
the ``Makefile`` to build and test your changes. See ``BUILDING.rst`` in the
top-level directory of your checkout for details on how to install required
software and build the compiler.

* Fork the repository. You will need to have a GitHub account to do this.
* Create a topic branch with ``git checkout -t -b my-branch``.
* Build and test with ``make`` and ``make check``.
* Commit changes to your branch, putting each distinct fix in a separate
  commit.
* Push your changes to your fork on GitHub.
* Submit a pull request with your changes.

Guidelines
==========

* You almost always want to branch from the master branch.
* Update documentation as necessary. Also, if appropriate, update the
  release notes, samples and other supporting materials.
* We suggest following `this note about git commit messages
  <https://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html>`_.
* Follow the :doc:`style guide </style-guide/index>` for new code. When
  working in existing code, follow the existing style.
* Do not make a lot of style or whitespace fixes in the same commit as
  other changes.
* Use 2 spaces for indentation, **never** tabs.  If you use emacs,
  `dylan-mode <https://github.com/dylan-lang/dylan-emacs-support>`_ does a
  decent job of indenting code.

Licensing
=========

All materials in this repository are licensed under the terms
expressed in the current `License.txt
<https://github.com/dylan-lang/opendylan/blob/master/License.txt>`_
file.

We are not interested in incorporating any source code using
proprietary or copyleft-style licenses.

Open Dylan is under the collective ownership of the Dylan
Hackers.

.. toctree::
   :hidden:
   :titlesonly:
   :maxdepth: 2

   documentation/index
   build-system
   compiler/index
   runtime/index
   runtime-manager/index
   duim/index
   topics/index
   glossary
   copyright
