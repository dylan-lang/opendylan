=========================
Dylan Documentation Tools
=========================

The purpose of this project is to improve our documentation tools
in interesting ways.

Searchable, Browsable API documentation
=======================================

The purpose of this project is to provide searchable, browsable API
documentation based on the reference materials. The idea would be to
extract documentation into JSON by writing an extension to Sphinx, put
that JSON into something like ElasticSearch and building a web-based
user interface on top of that.  This information could also be exposed
via an API for use in an IDE or other editing tools.

Interactive Graphs
==================

A class hierarchy diagram need not just be a static image.

We have a lot of information encoded in our documentation markup including
superclasses, related ("see also") items, available operations and 
example code.  This provides a pretty rich basis for a graphical visualization
of a library or module.  We could include generated SVG, HTML or JS using
the Canvas into the documentation to let people interact with this data.
