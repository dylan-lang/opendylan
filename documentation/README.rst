This is the documentation for Open Dylan.

It is built using `Sphinx <http://sphinx.pocoo.org>`_. All content is written
using ReStructured Text with Sphinx extensions.

Preparation
===========

Before building this documentation, you will need a copy of Sphinx installed.
The easiest way to do this is to get it from the `Python Package Index
<http://pypi.python.org/pypi/Sphinx>`_ or to use ``easy_install``::

    sudo easy_install -U Sphinx

Building
========

Building the documentation is easy on a system with ``make``::

    make html

If you are on Windows, there is a ``make.bat`` as well::

    make.bat html

The generated documentation will be in ``build/html``.

You can build other formats as well. Run ``make`` or ``make.bat`` without
arguments to see which formats are available.

Link Validation
---------------

Sphinx also makes it easy to check that all of the links to external sites
are valid.  You can run the link checker by::

    make linkcheck


Section Header Markup
=====================

We are following the `Sphinx suggestions
<http://sphinx.pocoo.org/rest.html#sections>`_ for header markup, which
are as follows:
    
    * # with overline, for parts
    * \* with overline, for chapters
    * =, for sections
    * -, for subsections
    * ^, for subsubsections
    * ", for paragraphs

Note that most files will correspond to a chapter and hence will start
with "`*`".  By convention we use over *and* under markup at this level.


Dylan Language Markup
=====================

We have created a Dylan language Sphinx domain to make it easier to document and
refer to Dylan language entities. This domain is documented in the
"dylandomain/reference.rst" file in the "sphinx-extensions" repository.
