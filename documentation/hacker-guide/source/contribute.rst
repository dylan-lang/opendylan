*******************************
How to contribute to Open Dylan
*******************************

Getting the Sources
===================

The first thing you'll need to start contributing is a source checkout
of the Git repository.  The `Open Dylan sources
<https://github.com/dylan-lang/opendylan>`_ are hosted on GitHub,
along with sources for the `opendylan.org web site
<https://github.com/dylan-lang/website>`_ and various other
repositories.  If you don't yet have a GitHub account and ssh keys,
now is a good time to get them.

To checkout the main "opendylan" repository::

    git clone git@github.com:dylan-lang/opendylan.git

You will want to fork this repository so you can push changes to your
fork and then submit pull requests.

Before you commit
=================

- Please read the `Dylan style guide <../style-guide/index.html>`_

- We use `Vincent Driessen's branching model
  <http://nvie.com/posts/a-successful-git-branching-model/>`_ (see
  `gitflow <https://github.com/nvie/gitflow/>`_ for tool integration)
  but we don't separate master from develop branch. Instead we develop
  in the master branch.

- We also emphasize `this note about git commit messages
  <http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html>`_

- Open Dylan is distributed under the MIT license.  We expect
  contributions to be the same.

- Open Dylan is of collective authorship of the "Dylan Hackers".
