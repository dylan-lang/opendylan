This is the Open Dylan website.

It is built using `Sphinx <https://www.sphinx-doc.org/>`_.  All content is
written using ReStructured Text with Sphinx extensions.

Preparation
===========

Installing dependencies
-----------------------

Some system dependencies need to be satisfied first:

- Python 3 and its package manager pip3.
- Git
- Make

On a Debian-derivative, they're quite easy to fetch::

    sudo apt install python3 python3-pip git make

Now you need the Python dependencies. The easiest way to do this is to use
``pip3``::

    sudo pip3 install -U Sphinx furo

You may also need ``python-dateutil``.

    sudo pip3 install python-dateutil

Building
========

Get the source if you haven't already::

    git clone --recursive https://github.com/dylan-lang/opendylan

Building the website::

    cd opendylan/documentation
    mkdir -p /tmp/opendylan.org
    ./update.sh /tmp/opendylan.org      # for testing
    ./update.sh /var/www/opendylan.org  # for realz

The generated site will be in ``_build/html`` and then copied to the output
directory you specified, along with package docs and the DRM files.

If you are tweaking non-package doc pages you can test your changes by running
``make html`` again, but the output will only be in ``_build/html``. **To build
the full website you must use ``update.sh``.**

Test your changes by running a web server::

    python3 -m http.server --directory /tmp/opendylan.org   # or _build/html

or you can eat our own Dylan dogfood and run our HTTP server! ::

    git clone --recursive https://github.com/dylan-lang/http
    cd http
    make install
    cd ...back to website dir...
    http-server --directory /tmp/opendylan.org   # or _build/html

Link Validation
---------------

Sphinx also makes it easy to check that all of the links to external sites
are valid.  You can run the link checker by::

    make linkcheck

Site Maintenance
================

#. Update the appropriate info on the download page when a new version of Open
   Dylan is released.

#. The :file:`update-opendylan.org.sh` script is run periodically via a systemd
   timer to update the documentation in various repositories by essentially
   doing a git pull and make html.

   Note that the script copies package docs into
   ``/var/www/opendylan.org/package/`` and copies the DRM to
   ``/var/www/opendylan.org/books/drm/`` so it assumes there are no top-level
   URLs starting with ``/books/drm`` or ``/package``.

Section Header Markup
=====================

We are following the `Sphinx suggestions
<https://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html#sections>`_
for header markup, which are as follows:

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
