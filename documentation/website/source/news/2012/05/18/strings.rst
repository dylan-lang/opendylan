:Author: Carl Gay
:Date: 2012-05-18 00:00:00

New Strings Library
===================

A new library of basic string operations is now available as a
standard library.  The new `strings library
<https://github.com/dylan-lang/strings>`_ replaces most of the code in
the old ``string-extensions`` library and provides a simpler and more
consistent API.  ``string-extensions`` will eventually be retired when
the remainder of the code there is moved to the
``regular-expressions`` library, which is its only client.
