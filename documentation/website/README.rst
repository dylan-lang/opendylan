This is the Open Dylan website.

It is built using `Sphinx <http://sphinx.pocoo.org>`_.  All content is written using
ReStructured Text with Sphinx extensions.

Preparation
===========

Installing system dependencies
------------------------------

Some system dependencies need to be satisfied first:

- Python 2.7.x and its package manager pip (Python 3.x is not supported)
- Git
- Make

On a Debian-derivative, they're quite easy to fetch::

    sudo apt install python2.7 python-pip git make

Getting the source
------------------

The next step is fetching the repository and its submodule::

    git clone https://github.com/dylan-lang/website.git  # or your fork
    git submodule update --init --recursive


Installing Sphinx
-----------------

Now you need the Python dependencies. The easiest way to do this is to use ``pip``::

    sudo pip install -U Sphinx

You will also need ``html5lib`` installed so that the RSS feed generator
can work::

    sudo pip install html5lib

You may also need ``python-dateutil``. If so, make sure you install version
1.5 as that is the lastest version to support Python 2.x::

    sudo pip install python-dateutil==1.5

Building
========

Building the website is easy on a system with ``make``::

    make

If you are on Windows, there is a ``make.bat`` as well. It currently requires
that you run it with an argument::

    make.bat html

The generated site will be in ``build/html``. For the stylesheets and
JavaScript to load correctly, we suggest running a local webserver
pointing to this directory::

    cd build/html
    python -m SimpleHTTPServer

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

Updating Documentation
----------------------

The `update-opendylan.org.sh` script is run by cron to update the
documentation in various repositories by essentially doing a git pull
and make html. These are the docs linked from the main Documentation
page.

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

