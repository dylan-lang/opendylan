Strings cleanup
***************

There are some copies of string code in various places that can be removed
in favor of using the official ``strings`` library.

* ``sources/lib/cl/cl-strings.dylan``
* ``sources/duim/utilities/strings.dylan``
* Parts of ``sources/runtime-manager/devel-dbg-ui/new-lexer.dylan``
* Parts of ``sources/app/news-app/string-utils.dylan``
