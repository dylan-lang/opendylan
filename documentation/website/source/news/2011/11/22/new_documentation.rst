:Author: Bruce Mitchener
:Date: 2011-11-22 12:00:00

New Documentation
=================

For a long time, our documentation has been in poor condition. Much of it
was originally in Framemaker in the 1990s and then automatically converted
to HTML.  This had left it in a form that was very difficult to maintain
and we haven't been able to provide our documentation in all of the formats
that we'd like (HTML Help on Windows, ePub, PDF, etc).

After some research, we're in the process of moving our documentation to
`Sphinx`_.  Sphinx is used very widely in the Python world and uses
ReStructured Text as the input format.  This has proven pretty easy to
work with and should greatly reduce the barrier to contributing.

As part of this, the documentation has moved back within the Open Dylan
repository in the `documentation`_ directory where it can be built
right along side the compiler and runtime.

We're also writing some new extensions to Sphinx to handle Dylan specific
constructs, make it easier to document Dylan code and link to the DRM. These
extensions can be found in the `sphinx-extensions`_ repo on GitHub.

There will still be a fair bit of work to update and improve the
documentation, but this should prove much easier now that we have
it in a modern format that we all can work with!

.. _Sphinx: http://sphinx-doc.org/
.. _documentation: https://github.com/dylan-lang/opendylan/tree/master/documentation
.. _sphinx-extensions: https://github.com/dylan-lang/sphinx-extensions

