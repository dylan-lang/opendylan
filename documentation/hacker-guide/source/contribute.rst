*******************************
How to contribute to Open Dylan
*******************************

The first thing you'll need is a source checkout of the Git
repository.  The `Open Dylan sources
<https://github.com/dylan-lang/opendylan>`_ are hosted on GitHub,
along with sources for the `opendylan.org web site
<https://github.com/dylan-lang/website>`_ and many other repositories.
If you don't yet have a GitHub account and ssh keys, now is a good
time to get them.

To checkout the main "opendylan" repository::

    git clone --recursive git@github.com:dylan-lang/opendylan

You'll want to fork this repository so you can push changes to your
fork and then submit pull requests.

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
something in mind that isn't there, feel free to `talk with us
<http://opendylan.org/community/>`_ or add it to the list.

Making Changes
==============

* Fork the repository. You will need to have a GitHub account to do
  this.
* Create a topic branch with ``git checkout -t -b your-contribution``.
* Commit your changes to your branch, putting each distinct fix in
  a separate commit.
* Push your changes to your fork on GitHub.
* Submit a pull request with your changes.

Guidelines
==========

* You almost always want to branch from the master branch.
* Update documentation as necessary. Also, if appropriate, update the
  release notes, samples and other supporting materials.
* We suggest following `this note about git commit messages
  <http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html>`_.
* Follow the `style guide
  <http://opendylan.org/documentation/style-guide/index.html>`_ for
  new code. When working in existing code, follow the existing style.
* Do not make a lot of style or whitespace fixes in the same commit as
  other changes.
* Use 2 spaces for indentation, **never** tabs.  If you use emacs,
  `dylan-mode <https://github.com/dylan-lang/dylan-mode>`_ does a
  decent job of indenting code.

Testing
=======

In the near future, we will have our test suites working more
reliably. We will also document the processes involved with updating
and running tests. In the meantime, each library usually has a "tests"
subdirectory with a test suite library named "\*-tests".

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
