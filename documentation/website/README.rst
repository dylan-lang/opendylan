This is the Open Dylan website.

It is built using `Sphinx <http://sphinx.pocoo.org>`_.  All content is written using
ReStructured Text with Sphinx extensions.

Preparation
===========

Before building this website, you will need a copy of Sphinx installed.
The easiest way to do this is to get it from the `Python Package Index
<http://pypi.python.org/pypi/Sphinx>`_ or to use ``easy_install``::

    sudo easy_install -U Sphinx

You will also need ``html5lib`` installed so that the RSS feed generator
can work::

    sudo easy_install html5lib

You may also need ``python-dateutil``. If so, make sure you install version
1.5 if you are using Python 2.x::

    sudo easy_install python-dateutil==1.5

Building
========

Building the website is easy on a system with ``make``::

    make

If you are on Windows, there is a ``make.bat`` as well. It currently requires
that you run it with an argument::

    make.bat html

The generated site will be in ``build/html``.

Link Validation
---------------

Sphinx also makes it easy to check that all of the links to external sites
are valid.  You can run the link checker by::

    make linkcheck

Site Maintenance
================

New Binary Release
------------------

#. Update the appropriate info on the download page.
#. Add a recent news entry. (See below for notes on that.)

New News Entry
--------------

#. Create an article about it (even if very short) in the appropriate date
   hierarchy in ``source/news``. The hierarchy should be ``year/month/day``.
   This article must have 2 metadata fields in it::

       :Author: Hannes Mehnert
       :Date: 2001-08-11 09:00:00

#. Add it to ``source/news/recent.rst.inc``
#. If there are too many entries in that file, move some
   to ``source/news/index.rst``.
#. Confirm that the site's main page and news page both
   look good / correct.

