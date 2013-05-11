How to Contribute
=================

If you are looking for ideas on how you can contribute
to Open Dylan or what others might find useful, please
see our `list of projects in the Open Dylan wiki
<https://github.com/dylan-lang/opendylan/wiki>`_. If you
have something in mind that isn't there, feel free
to `talk with us <http://opendylan.org/community/>`_.

Making Changes
--------------

* Fork the repository. You will need to have a GitHub
  account to do this.
* Create a topic branch.
  (``git checkout -t -b your-contribution``)
* Commit your changes to your branch with separate fixes
  in separate commits.
* Push your changes to your fork on GitHub.
* Submit a pull request with your changes.

Guidelines
----------

* You almost always want to branch from the master
  branch.
* Update documentation as necessary. Also, if appropriate,
  update the release notes, samples and other supporting
  materials.
* We suggest following `this note about git commit messages
  <http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html>`_.

Code Style
----------

* Follow the `style guide
  <http://opendylan.org/documentation/style-guide/index.html>`_
  for new code. When working in existing code, follow the
  existing style.
* Do not do style or whitespace fixes in the same commit
  as other changes.
* Be sure that your changes haven't introduced new
  unnecessary whitespace by checking with ``git diff --check``.
* We prefer spaces to tabs. We prefer 2 space indents.

Testing
-------

In the near future, we will have our test suites working more
reliably. We will also document the processes involved with
updating and running tests.

Licensing
---------

All materials in this repository are licensed under the terms
expressed in the current ``License.txt`` file.

We are not interested in incorporating any sourcecode using
proprietary or copyleft-style licenses.

Open Dylan is under the collective ownership of the Dylan
Hackers.
