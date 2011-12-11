*****************
Dylan Cheat Sheet
*****************

* `Literals`_
* `Naming Conventions`_
* `Operators`_

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
|                |  'a'       // A character              | * :drm:`<symbol>`         |
|                |  "Hello"   // A simple string          |                           |
|                |  "Hello\n" // An escape sequence       |                           |
|                |  #"hello"  // A symbol, unique string  |                           |
|                |  hello:    // A symbol, keyword        |                           |
+----------------+----------------------------------------+---------------------------+
| Collections    | .. code-block:: dylan                  | * :drm:`<pair>`           |
|                |                                        | * :drm:`<list>`           |
|                |  #(1 . "one") // A literal <pair>      | * :drm:`<vector>`         |
|                |  #(1, 2, 3)   // A literal <list>      |                           |
|                |  #[1, 2, 3]   // A literal <vector>    |                           |
+----------------+----------------------------------------+---------------------------+

Naming Conventions
==================

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
| Functions   | return true / false     |                          |
|             | values. They end in     |  even?                   |
|             | ``?``.                  |  instance?               |
+-------------+-------------------------+--------------------------+
| Getters &   | Getters return a value  | .. code-block:: dylan    |
| Setters     | while setters allow you |                          |
|             | modify that value. The  |  window.size := 3        |
|             | compiler uses this      |  size-setter(3, window)  |
|             | convention to find the  |                          |
|             | function to call for    |                          |
|             | ``:=``.                 |                          |
|             |                         |                          |
|             | Setter functions end    |                          |
|             | in ``-setter``.         |                          |
+-------------+-------------------------+--------------------------+

Operators
=========

+-------------+------------------------------------+--------------------+
| Class       | Dylan Syntax                       | Learn More...      |
+=============+====================================+====================+
| Equality &  | .. code-block:: dylan              | * :drm:`<`         |
| Comparison  |                                    | * :drm:`>`         |
|             |   a < b    // a less than b?       | * :drm:`=`         |
|             |   a > b    // a greater than b?    | * :drm:`~=`        |
|             |   a = b    // a equal to b?        | * :drm:`==`        |
|             |   a ~= b   // a not equal b        | * :drm:`~==`       |
|             |   a == b   // a identical to b     |                    |
|             |   a ~== b  // a not identical to b |                    |
|             |  ~a        // logical negation     |                    |
+-------------+------------------------------------+--------------------+
| Arithmetic  | .. code-block:: dylan              | * :drm:`+`         |
|             |                                    | * :drm:`*`         |
|             |  a + b        // add a and b       | * :drm:`-`         |
|             |  a * b        // mulitply a and b  | * :drm:`/`         |
|             |  a - b        // subtract b from a | * :drm:`modulo`    |
|             |  a / b        // divide a by b     | * :drm:`negative`  |
|             |  modulo(a, b) // modulus of a by b |                    |
|             |  negative(a)  // negative of a     |                    |
+-------------+------------------------------------+--------------------+
| Collection  | .. code-block:: dylan              | * :drm:`empty?`    |
|             |                                    |                    |
|             |  my-list.empty? // is this empty?  |                    |
+-------------+------------------------------------+--------------------+
