*********************
Writing Documentation
*********************

We are working on a tool to automatically generate skeletal documentation from
source code, but until then, we are documenting the Open Dylan libraries
manually using `Sphinx <http://sphinx-doc.org/>`_ to build the HTML pages.
Sphinx uses reStructuredText markup with some extensions of its own, and we have
created additional extensions to document Dylan language entities.

The documentation — a number of RST files — is in the ``documentation`` directory
in the ``opendylan`` repository. Consult the Sphinx web-site for details about
reStructuredText markup and Sphinx extensions to it, and see the
``dylandomain/reference.rst`` file in the ``sphinx-extensions`` repository for
details about the Dylan language extensions. (You may use the ``rst2html`` tool
to generate an HTML page from an ``.rst`` file.)

.. toctree::
   :titlesonly:

   guidelines
   generating
   doctower
   example
