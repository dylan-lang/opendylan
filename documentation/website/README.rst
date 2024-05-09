This is the Open Dylan website.

It is built using `Sphinx <https://www.sphinx-doc.org/>`_.  All content is
written using ReStructured Text with Sphinx extensions.

Installation
============

Some system dependencies need to be satisfied first.  On a Debian-derivative
this should work::

    sudo apt install git graphviz make python3

Install a Python3 virtual environment and use ``pip`` rather than the ``apt``
Python packages, which are sometimes very out-of-date. ::

    python3 -m venv /opt/python3-venv
    export PATH=/opt/python3-venv/bin:${PATH}
    pip install Sphinx furo

The next step is fetching the repository and its dependencies::

    git clone https://github.com/dylan-lang/website.git
    cd website
    deft update    # Install Dylan package dependencies

Building the site
=================

Simply run the :file:`update.sh` script, specifying where you want the HTML
files to be generated::

    update.sh /tmp/opendylan.org

The first time you run :file:`update.sh`, it will build the `gendoc
<https://github.com/dylan-lang/gendoc>`_ executable, which takes a bit longer.

.. note:: Currently the downloads directory is still maintained by hand. When
          building the live site the first time, copy the files from the old
          location.

Testing
=======

The generated site will be in the output directory you specified. Run a local
server using that directory as a static site. For example::

    python3 -m http.server --directory /tmp/opendylan.org

For bonus points, you can eat our own dogfood and run the Dylan HTTP server
instead::

    git clone --recursive https://github.com/dylan-lang/http
    cd http
    make install
    http-server --directory /tmp/opendylan.org

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
