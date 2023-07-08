This is the Open Dylan website.

It is built using `Sphinx <https://www.sphinx-doc.org/>`_.  All content is
written using ReStructured Text with Sphinx extensions.

Preparation
===========

Installing system dependencies
------------------------------

Some system dependencies need to be satisfied first:

- Python 3 and its package manager pip3. (It may also be installed as just
  "pip", but check whether that installs Python 2 packages.)
- Git
- Make

On a Debian-derivative, they're quite easy to fetch::

    sudo apt install python3 python3-pip git make

Getting the source
------------------

The next step is fetching the repository and its submodule::

    git clone --recursive https://github.com/dylan-lang/website.git  # or your fork

Note that for now the "opendylan" repository is a submodule and it (along with
its own submodules) is rather large.

Installing Sphinx and the Furo Theme
------------------------------------

Now you need the Python dependencies. The easiest way to do this is to use
``pip3``::

    sudo pip3 install -U Sphinx furo

You may also need ``python-dateutil``.

    sudo pip3 install python-dateutil

Building
========

Building the website is easy on a system with ``make``::

    make html

If you are on Windows, there is a ``make.bat`` as well. It currently requires
that you run it with an argument::

    make.bat html  # Best of luck! This hasn't been tested in ages.

The generated site will be in ``build/html``. For the stylesheets and
JavaScript to load correctly, we suggest running a local webserver
pointing to this directory::

    python3 -m http.server --directory build/html

or you can eat our own Dylan dogfood and run our HTTP server! ::

    git clone --recursive https://github.com/dylan-lang/http
    cd http
    make install
    cd ...back to website dir...
    http-server --directory build/html

Link Validation
---------------

Sphinx also makes it easy to check that all of the links to external sites
are valid.  You can run the link checker by::

    make linkcheck

Site Maintenance
================

New Binary Release of Open Dylan
--------------------------------

#. Update the appropriate info on the download page.


Updating Documentation
----------------------

The `update-opendylan.org.sh` script is run by cron to update the
documentation in various repositories by essentially doing a git pull
and make html.

*TODO:* more info here.
