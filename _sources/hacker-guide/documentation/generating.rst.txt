************************
Generating Documentation
************************

``dylan-compiler`` can output skeletal documentation for a library once
it has been compiled. Prior to compiling the project, it will not output
anything.

To generate documentation, once you've loaded and compiled your project,
simply::

   export -format rst interface-reference

The generated documention will need cleaning up and will not include
anything from comments in the source.

We hope to improve this tool in the future.


