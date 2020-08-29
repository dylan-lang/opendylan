This is the documentation for Open Dylan.

It is built using `Sphinx <http://sphinx.pocoo.org>`_. All content is written
using ReStructured Text with Sphinx extensions.

Preparation
===========

Before building this documentation, you will need a copy of Sphinx
installed. The latest versions of Sphinx use Python 3 so we assume Python 3 is
in use here.  The easiest way to install Sphinx is to get it from the `Python
Package Index <http://pypi.python.org/pypi/Sphinx>`_ or to use ``pip3``::
 
  sudo pip3 install -U Sphinx

On some systems it might be called just ``pip`` even when installing Python 3
packages.

You also need to have the git submodule for our Sphinx extension populated.  If
this is not present for you, do this at the top level of the opendylan
repository::

  git submodule init && git submodule update

Building
========

Building the documentation is easy on a system with ``make``. For example::

  cd library-reference && make html

On Windows, use ``make.bat`` instead::

  make.bat html

The generated documentation will be in ``build/html``.  Just viewing the HTML
locally may not correctly load the CSS files.  A workaround is to cd into
``build/html`` and run a web server.  For example::

  python3 -m http.server

or::

  python2 -m SimpleHTTPServer

The pages can then be accessed via::

  http://localhost:8000/index.html

You can build other formats as well. Run ``make`` or ``make.bat`` without
arguments to see the available formats.

Note that to build the PDF files with ``make latexpdf`` it may be necessary (at
least on Debian) to install these packages:

*  latexmk
*  texlive-latex-recommended
*  texlive-fonts-recommended
*  texlive-latex-extra
 

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

There is a `Dylan language Sphinx domain
<https://github.com/dylan-lang/sphinx-extensions/blob/master/sphinxcontrib/dylan/domain/reference.rst>`_
to make it easier to document and refer to Dylan language entities.
