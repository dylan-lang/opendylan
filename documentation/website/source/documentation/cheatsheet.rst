*****************
Dylan Cheat Sheet
*****************

* `Literals`_
* `Naming Conventions`_
* `Operators`_
* `String Formatting`_

Literals
========

+----------------+----------------------------------------+---------------------------+
| Type           | Dylan Syntax                           | Learn More...             |
+================+========================================+===========================+
| Booleans       | .. code-block:: dylan                  | * :drm:`<boolean>`        |
|                |                                        |                           |
|                |  #t // true                            |                           |
|                |  #f // false                           |                           |
+----------------+----------------------------------------+---------------------------+
| Numbers        | .. code-block:: dylan                  | * :drm:`Numbers`          |
|                |                                        | * :drm:`<number>`         |
|                |  42       // A decimal integer         |                           |
|                |  #b101010 // A binary integer          |                           |
|                |  #o52     // An octal integer          |                           |
|                |  #x2A     // A hexadecimal integer     |                           |
|                |  -42.5    // A floating point number   |                           |
|                |  6.02E23  // A floating point number   |                           |
+----------------+----------------------------------------+---------------------------+
| Strings        | .. code-block:: dylan                  |                           |
|                |                                        | * :drm:`<string>`         |
|                |  'a'       // A character              | * :drm:`<character>`      |
|                |  "Hello"   // A simple string          |                           |
|                |  "Hello\n" // An escape sequence       |                           |
+----------------+----------------------------------------+---------------------------+
| Symbols        | .. code-block:: dylan                  | * :drm:`<symbol>`         |
|                |                                        |                           |
|                |  #"hello"  // A symbol                 |                           |
|                |  #"HELLO"  // The same symbol          |                           |
|                |  hello:    // Again, in keyword syntax |                           |
+----------------+----------------------------------------+---------------------------+
| Collections    | .. code-block:: dylan                  | * :drm:`<pair>`           |
|                |                                        | * :drm:`<list>`           |
|                |  #(1 . "one") // A literal <pair>      | * :drm:`<vector>`         |
|                |  #(1, 2, 3)   // A literal <list>      |                           |
|                |  #[1, 2, 3]   // A literal <vector>    |                           |
+----------------+----------------------------------------+---------------------------+

Naming Conventions
==================

These are *conventions* only; they have no semantic value to the compiler.

+-------------+-------------------------+--------------------------+
| Classes     | Class names begin / end | .. code-block:: dylan    |
|             | with angle brackets     |                          |
|             | (``<`` and ``>``).      |  <float>                 |
|             |                         |  <stretchy-vector>       |
+-------------+-------------------------+--------------------------+
| Constants   | Constants begin with    | .. code-block:: dylan    |
|             | ``$``.                  |                          |
|             |                         |  $word-size              |
|             |                         |  $tag-bits               |
+-------------+-------------------------+--------------------------+
| Module      | Module variable names   | .. code-block:: dylan    |
| Variables   | begin / end with ``*``. |                          |
|             |                         |  *news*                  |
|             | This does not apply to  |  *command-dispatcher*    |
|             | local variables that    |                          |
|             | have been declared with |                          |
|             | ``let``.                |                          |
+-------------+-------------------------+--------------------------+
| Predicate   | Predicate functions     | .. code-block:: dylan    |
| Functions   | return #t or #f.  They  |                          |
|             | end in ``?``.           |  even?                   |
|             |                         |  instance?               |
+-------------+-------------------------+--------------------------+
| Getters &   | Getters read a value    | .. code-block:: dylan    |
| Setters     | while setters write a   |                          |
|             | value.  Setter functions|  window.size := 3        |
|             | end in ``-setter``.     |  size-setter(3, window)  |
|             | The compiler uses this  |                          |
|             | convention to find the  |                          |
|             | function to call for    |                          |
|             | ``:=``.                 |                          |
+-------------+-------------------------+--------------------------+

Operators
=========

+--------------------+---------------------------------------+--------------------------+
| Class              | Dylan Syntax                          | Learn More...            |
+====================+=======================================+==========================+
| Equality &         | .. code-block:: dylan                 | * :drm:`<`               |
| Comparison         |                                       | * :drm:`>`               |
|                    |   a < b    // a less than b?          | * :drm:`=`               |
|                    |   a > b    // a greater than b?       | * :drm:`~=`              |
|                    |   a = b    // a equal to b?           | * :drm:`==`              |
|                    |   a ~= b   // a not equal b           | * :drm:`~==`             |
|                    |   a == b   // a identical to b        |                          |
|                    |   a ~== b  // a not identical to b    |                          |
|                    |  ~a        // logical negation        |                          |
+--------------------+---------------------------------------+--------------------------+
| Arithmetic         | .. code-block:: dylan                 | * :drm:`+`               |
|                    |                                       | * :drm:`*`               |
|                    |  a + b        // add a and b          | * :drm:`-`               |
|                    |  a * b        // mulitply a and b     | * :drm:`/`               |
|                    |  a - b        // subtract b from a    | * :drm:`modulo`          |
|                    |  a / b        // divide a by b        | * :drm:`negative`        |
|                    |  modulo(a, b) // modulus of a by b    |                          |
|                    |  negative(a)  // negative of a        |                          |
+--------------------+---------------------------------------+--------------------------+
| :drm:`Collections` | .. code-block:: dylan                 | * :drm:`<collection>`    |
|                    |                                       |                          |
|                    |  c[k]       // elem. k of col. c      | * :drm:`element`         |
|                    |  c[k] := x  // set elem. k of col. c  | * :drm:`element-setter`  |
|                    |  c.empty?   // is c empty?            | * :drm:`empty?`          |
|                    |  c.size     // how big is c?          | * :drm:`size`            |
+--------------------+---------------------------------------+--------------------------+
| Sequence           | .. code-block:: dylan                 | * :drm:`<sequence>`      |
|                    |                                       |                          |
|                    |  add(c, x)    // add x to copy of c   | * :drm:`add`             |
|                    |  remove(c, x) // rem x from copy of c | * :drm:`remove`          |
|                    |  sort(c)      // copy of c, sorted    | * :drm:`sort`            |
|                    |  reverse(c)   // copy of c, reversed  | * :drm:`reverse`         |
+--------------------+---------------------------------------+--------------------------+


String Formatting
=================

Example: ``format(stream, "%s:%d", host, port)``

+-------------+------------------+-----------------------+
| Directive   | Argument Type    | Description           |
+=============+==================+=======================+
| %d          | <integer>        | decimal number        |
+-------------+------------------+-----------------------+
| %b          | <integer>        | binary number         |
+-------------+------------------+-----------------------+
| %o          | <integer>        | octal number          |
+-------------+------------------+-----------------------+
| %x          | <integer>        | hexadecimal number    |
+-------------+------------------+-----------------------+
| %c          | <character>      | character, no quotes  |
+-------------+------------------+-----------------------+
| %s          | <object>         | "pretty" format       |
+-------------+------------------+-----------------------+
| %=          | <object>         | "unique" format       |
+-------------+------------------+-----------------------+
| %%          | None             | literal %             |
+-------------+------------------+-----------------------+
