A Dylan Primer for Python Programmers
=====================================

.. note:: If one column contains N/A, then that language has no corresponding
          direct way to express what is in the other column.

Literals
--------

+------------------------+-----------------------------------------------------+
| Python                 | Dylan                                               |
+========================+=====================================================+
| .. code-block:: python | .. code-block:: dylan                               |
|                        |                                                     |
|    True                |    #t                                               |
+------------------------+-----------------------------------------------------+
| .. code-block:: python | .. code-block:: dylan                               |
|                        |                                                     |
|    False               |    #f                                               |
+------------------------+-----------------------------------------------------+
| .. code-block:: python | .. code-block:: dylan                               |
|                        |                                                     |
|    23                  |    23                                               |
+------------------------+-----------------------------------------------------+
| .. code-block:: python | .. code-block:: dylan                               |
|                        |                                                     |
|    0b1011              |    #b1011                                           |
+------------------------+-----------------------------------------------------+
| .. code-block:: python | .. code-block:: dylan                               |
|                        |                                                     |
|    0o644               |    #o644                                            |
+------------------------+-----------------------------------------------------+
| .. code-block:: python | .. code-block:: dylan                               |
|                        |                                                     |
|    0x2A5F              |    #x2A5F                                           |
+------------------------+-----------------------------------------------------+
| .. code-block:: python | .. code-block:: dylan                               |
|                        |                                                     |
|    6.02e+23            |    6.02e+23 // double float                         |
|                        |    6.02s+23 // single float                         |
|                        |    6.02d+23 // double float                         |
+------------------------+-----------------------------------------------------+
| N/A                    | .. code-block:: dylan                               |
|                        |                                                     |
| No character type,     |                                                     |
| only integer character |    'a'                                              |
| codes.                 |    '\n'                                             |
+------------------------+-----------------------------------------------------+
| .. code-block:: python | .. code-block:: dylan                               |
|                        |                                                     |
|    'Hello'             |    "Hello"                                          |
|    'Hello\n'           |    "Hello\n"                                        |
+------------------------+-----------------------------------------------------+
| .. code-block:: python | .. code-block:: dylan                               |
|                        |                                                     |
|    s = '''Hi, from     |    let s = """                                      |
|    python'''           |    Hi, from                                         |
|                        |    Dylan                                            |
|                        |    """;                                             |
|    # No Rectangle rule |    // Rectangle rule support                        |
|    s = '''             |    let s = """                                      |
|        Hi, from        |            Hi, from                                 |
|        Python          |            Dylan                                    |
|        '''             |            """;                                     |
|                        |                                                     |
|                        | (See `rectangle rule`_.)                            |
+------------------------+-----------------------------------------------------+
| N/A                    | .. code-block:: dylan                               |
|                        |                                                     |
| Not real symbols but   |    #"apple"                                         |
| strings are interned   |    apple:                                           |
+------------------------+-----------------------------------------------------+
| N/A                    | .. code-block:: dylan                               |
|                        |                                                     |
| No symbols             |    #"two words"                                     |
+------------------------+-----------------------------------------------------+
| .. code-block:: python | N/A                                                 |
|                        |                                                     |
|    (1, 'a', 'dog')     | No tuples (see `multiple values`_)                  |
+------------------------+-----------------------------------------------------+
| .. code-block:: python | .. code-block:: dylan                               |
|                        |                                                     |
|    [5, 10, 15]         |   #[5, 10, 15]                                      |
+------------------------+-----------------------------------------------------+

.. _`multiple values`: https://opendylan.org/about/examples/multiple_return_values.html

Syntax
------

.. note::

   In Dylan, any words after an ``end`` (e.g.  ``end method``) are
   optional but, if present, must match the corresponding "begin"
   word.

+----------------------------------+------------------------------------------+
| Python                           | Dylan                                    |
+==================================+==========================================+
| .. code-block:: python           | .. code-block:: dylan                    |
|                                  |                                          |
|    var = exp                     |    let var = exp;                        |
|                                  |    var := exp;                           |
|                                  |    define variable var = exp;            |
|                                  |    define variable var :: type = exp;    |
|    VAR = exp # Convention        |    define constant var = exp;            |
+----------------------------------+------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                    |
|                                  |                                          |
|    f(x, y, z)                    |    f(x, y, z)                            |
+----------------------------------+------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                    |
|                                  |                                          |
|    1                             |    begin 1; 2; 3; end                    |
|    2                             |    begin 1; 2; 3 end                     |
|    3                             |                                          |
+----------------------------------+------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                    |
|                                  |                                          |
|    lambda x, y, *z:              |    method (x, y, #rest z)                |
|      print('hello')              |      format-out("hello");                |
|      return f(x, y, z)           |      f(x, y, z)                          |
|                                  |    end method                            |
+----------------------------------+------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                    |
|                                  |                                          |
|    x, y = exp                    |    let (x, y) = exp                      |
+----------------------------------+------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                    |
|                                  |                                          |
|    x, y = (5, 6)                 |    let (x, y) = values(5, 6);            |
|    f(x, y)                       |    f(x, y)                               |
+----------------------------------+------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                    |
|                                  |                                          |
|    def foo():                    |    define function foo ()                |
|      def f(x):                   |      local method f (x)                  |
|        f-body                    |              f-body                      |
|                                  |            end,                          |
|      def g(y, z):                |            method g (y, z)               |
|        g-body                    |              g-body                      |
|                                  |            end;                          |
|      body                        |      body                                |
|                                  |    end;                                  |
+----------------------------------+------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                    |
|                                  |                                          |
|    if test:                      |    if (test)                             |
|      then1                       |      then1;                              |
|      then2                       |      then2;                              |
|    else:                         |    else                                  |
|      else1                       |      else1;                              |
|      else2                       |      else2;                              |
|                                  |    end if                                |
+----------------------------------+------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                    |
|                                  |                                          |
|    a and b and c                 |    a & b & c                             |
+----------------------------------+------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                    |
|                                  |                                          |
|    a or b or c                   |    a | b | c                             |
+----------------------------------+------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                    |
|                                  |                                          |
|    if test1:                     |    case                                  |
|      result1                     |      test1 => result1;                   |
|    elif test2:                   |      test2 => result2;                   |
|      result2                     |      otherwise => result                 |
|    else:                         |    end case                              |
|      result                      |                                          |
+----------------------------------+------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                    |
|                                  |                                          |
|    if exp in (1, 2):             |    select (exp)                          |
|      result1                     |      1, 2 => result1;                    |
|    elif exp in ('a', 'b'):       |      'a', 'b' => result2;                |
|      result2                     |      otherwise => result                 |
|    else:                         |    end select                            |
|      result                      |                                          |
+----------------------------------+------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                    |
|                                  |                                          |
|    c = comparison-func(exp)      |    select (exp by comparison-func)       |
|    if c == 'foo':                |      "foo" => result1;                   |
|      result1                     |      "bar" => result2;                   |
|    elif c == 'bar':              |      otherwise => result                 |
|      result2                     |    end select                            |  
|    else:                         |                                          |
|      result                      |                                          |
+----------------------------------+------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                    |
|                                  |                                          |
|    var1 = init1                  |    for (var1 = init1 then step1,         |
|    var2 = init2                  |         var2 = init2 then step2,         |
|    while not test:               |         until: test)                     |
|      body                        |      body                                |
|      var1 = step1                |    finally var1                          |
|      var2 = step2                |    end for                               |
|    return var1                   |                                          |
+----------------------------------+------------------------------------------+

Predefined functions
--------------------

Boolean functions
^^^^^^^^^^^^^^^^^
+--------------------------------+-----------------------------------------------+
| Python                         | Dylan                                         |
+================================+===============================================+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    not obj                     |    ~ obj                                      |
|                                |    ~obj                                       |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    isinstance(obj, bool)       |    instance?(obj, <boolean>)                  |
+--------------------------------+-----------------------------------------------+

Equivalence predicates
^^^^^^^^^^^^^^^^^^^^^^
+--------------------------------+-----------------------------------------------+
| Python                         | Dylan                                         |
+================================+===============================================+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    x == y                      |    x = y                                      |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    x is y                      |    x == y                                     |
+--------------------------------+-----------------------------------------------+

Symbols
^^^^^^^
+--------------------------------+-----------------------------------------------+
| Python                         | Dylan                                         |
+================================+===============================================+
| N/A                            | .. code-block:: dylan                         |
|                                |                                               |
|                                |    instance?(obj, <symbol>)                   |
+--------------------------------+-----------------------------------------------+
| N/A                            | .. code-block:: dylan                         |
|                                |                                               |
|                                |    as(<string>, sym)                          |
+--------------------------------+-----------------------------------------------+
|                                | .. code-block:: dylan                         |
|                                |                                               |
|                                |    as(<symbol>, str)                          |
+--------------------------------+-----------------------------------------------+

Numerical operations
^^^^^^^^^^^^^^^^^^^^
+----------------------------------+-----------------------------------------------+
| Python                           | Dylan                                         |
+==================================+===============================================+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    isinstance(obj,               |    instance?(obj, <number>)                   |
|      (int, float, complex))      |                                               |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    isinstance(obj, complex)      |    instance?(obj, <complex>)                  |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    isinstance(obj,               |    instance?(obj, <real>)                     |
|      (int, float))               |                                               |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    isinstance(obj, int) or       |    instance?(obj, <rational>)                 |
|      (isinstance(obj, float) and |                                               |
|        obj == int(obj))          |                                               |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    isinstance(obj, int)          |    instance?(obj, <integer>)                  |
|                                  |    integral?(num)                             |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    n1 == n2                      |    n1 == n2                                   |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    n1 < n2                       |    n1 < n2                                    |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    n1 > n2                       |    n1 > n2                                    |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    n1 <= n2                      |    n1 <= n2                                   |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    n1 >= n2                      |    n1 >= n2                                   |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    n == 0                        |    zero?(n)                                   |
|    0 == 0.0 # True               |    0 == 0.0 // #f                             |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    n > 0                         |    positive?(n)                               |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    n < 0                         |    negative?(n)                               |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    i % 2 != 0                    |    odd?(i)                                    |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    i % 2 == 0                    |    even?(i)                                   |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    1 + 2 + 3                     |    1 + 2 + 3                                  |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    1 * 2 * 3                     |    1 * 2 * 3                                  |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    5 - 3                         |    5 - 3                                      |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    1 / 2 # 0.5                   |    1 / 2           // error                   |
|                                  |    1.0 / 2         // 0.5                     |
|                                  |    truncate/(1, 2) // 0                       |
|                                  |    ceiling/(1, 2)  // 1                       |
|                                  |    floor/(1,2)     // 0                       |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    -x                            |    - x                                        |
|                                  |    -x                                         |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    2 ** 16                       |    2 ^ 16                                     |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    max(1, 2, 3)                  |    max(1, 2, 3)                               |
|    max([1, 2, 3])                |    apply(max, #(1, 2, 3))                     |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    5 % 2                         |    remainder(5, 2)                            |
+----------------------------------+-----------------------------------------------+

Characters
^^^^^^^^^^
+--------------------------------+-----------------------------------------------+
| Python                         | Dylan                                         |
+================================+===============================================+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    isinstance(obj, str) and    |    instance?(obj, <character>)                |
|      len(obj) == 1             |                                               |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    char1 == char2              |    char1 == char2                             |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    char1 < char2               |    char1 < char2                              |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    char1 > char2               |    char1 > char2                              |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    char1 <= char2              |    char1 <= char2                             |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    char1 >= char2              |    char1 >= char2                             |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    ord(char)                   |    as(<integer>, char)                        |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    chr(n)                      |    as(<character>, n)                         |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    char.upper()                |    as-uppercase(char)                         |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    char.lower()                |    as-lowercase(char)                         |
+--------------------------------+-----------------------------------------------+

Strings
^^^^^^^
+--------------------------------+-----------------------------------------------+
| Python                         | Dylan                                         |
+================================+===============================================+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    isinstance(obj, str)        |    instance?(obj, <string>)                   |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    'x' * k                     |    make(<string>, size: k, fill: char)        |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    "".join('a', 'b', 'c'])     |    as(<string>, #['a', 'b', 'c'])             |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    len(str)                    |    size(str)                                  |
|                                |    str.size                                   |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    str[k]                      |    element(str, k)                            |
|                                |    str[k]                                     |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    # Strings are immutable     |    element-setter(char, str, k)               |
|                                |    str[k] := char                             |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    str1 == str2                |    str1 = str2                                |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    str1 < str2                 |    str1 < str2                                |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    str[start:end]              |    copy-sequence(str, start: start, end: end) |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    str1 + str2                 |    concatenate(str1, str2)                    |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    list(str)                   |    as(<list>, str)                            |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    list(str)                   |    as(<string>, chars)                        |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    copy(str)                   |    shallow-copy(str)                          |
|                                |    copy-sequence(str)                         |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    # Strings are immutable     |    fill!(str, char)                           |
+--------------------------------+-----------------------------------------------+

Vectors
^^^^^^^
+--------------------------------+-----------------------------------------------+
| Python                         | Dylan                                         |
+================================+===============================================+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    isinstance(obj, list)       |    instance?(obj, <vector>)                   |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    [fill] * k                  |    make(<vector>, size: k, fill: fill)        |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    [obj, ...]                  |    vector(obj, ...)                           |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    len(vec)                    |    size(vec)                                  |
|                                |    vec.size                                   |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    vec[k]                      |    element(vec, k)                            |
|                                |    vec[k]                                     |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    vec[k] = obj                |    element-setter(obj, vec, k)                |
|                                |    vec[k] := obj                              |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    list(vec)                   |    as(<list>, vec)                            |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    # Uses lists for both       |    as(<vector>, list)                         |
|    list(list)                  |                                               |
+--------------------------------+-----------------------------------------------+
| .. code-block:: python         | .. code-block:: dylan                         |
|                                |                                               |
|    for i in range(len(vec)):   |    fill!(vec, obj)                            |
|      vec[i] = obj              |                                               |
+--------------------------------+-----------------------------------------------+

Control Features
----------------
+----------------------------------+-----------------------------------------------+
| Python                           | Dylan                                         |
+==================================+===============================================+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    callable(obj)                 |    instance?(obj, <function>)                 |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    proc(arg1, arg2, *args)       |    apply(proc, arg1, arg2, args)              |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    map(proc, list1, list2, ...)  |    map(proc, list1, list2, ...)               |
|    # returns iterator in         |                                               |
|    # Python 3                    |                                               |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    map(proc, vec1, vec2, ...)    |    map(proc, vec1, vec2, ...)                 |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    map(proc, str1, str2)         |    map(proc, str1, str2)                      |
+----------------------------------+-----------------------------------------------+
| .. code-block:: python           | .. code-block:: dylan                         |
|                                  |                                               |
|    for a,b in zip(list1, list2): |    do(proc, list1, list2)                     |
|      proc(a, b)                  |                                               |
+----------------------------------+-----------------------------------------------+

Continuations
-------------

Python doesn’t have first-class continuations like Scheme’s
``call/cc``, and Dylan’s ``block`` syntax is more like structured
exception handling or generators.

Here are approximate equivalents:

+-------------------------------------+-------------------------------------------------+
| Python                              | Dylan                                           |
+=====================================+=================================================+
| .. code-block:: python              | .. code-block:: dylan                           |
|                                     |                                                 |
|    def example():                   |    block (k)                                    |
|      try:                           |      body                                       |
|        body()                       |    exception (e :: <error>)                     |
|      except Error as e:             |      handle-error()                             |
|        handle_error()               |    cleanup                                      |
|      finally:                       |      cleanup-stuff                              |
|        cleanup_stuff()              |    end block                                    |
+-------------------------------------+-------------------------------------------------+
| N/A                                 | .. code-block:: dylan                           |
|                                     |                                                 |
|                                     |    define function top ()                       |
|                                     |      block (exit-block)                         |
|                                     |        bar(exit-block);                         |
|                                     |        format-out("You won't see this.\n");     |
|                                     |      end;                                       |
|                                     |      format-out("You WILL see this.\n");        |
|                                     |    end;                                         |
|                                     |                                                 |
|                                     |    define function bar (thunk)                  |
|                                     |      thunk()                                    |
|                                     |    end;                                         | 
|                                     |                                                 |
|                                     |    top();                                       |
+-------------------------------------+-------------------------------------------------+

Keyword arguments
-----------------

+--------------------------------------------------+---------------------------------------------------+
| Python                                           | Dylan                                             |
+==================================================+===================================================+
| .. code-block:: python                           | .. code-block:: dylan                             |
|                                                  |                                                   |
|    def wrapper_fn (a, b, c, **keys):             |    define method wrapper-fn (a, b, c, #rest keys) |
|      do_stuff(a, b, c)                           |      do-stuff(a, b, c);                           |
|    return wrapped_fn(**keys)                     |      apply(wrapped-fn, keys)                      |
|                                                  |    end;                                           |
|    def wrapped_fn (one = 1, two = 2, three = 3): |                                                   |
|      return [one, two, three]                    |    define method wrapped-fn                       |
|                                                  |      (#key one = 1, two = 2, three = 3)           |
|                                                  |      list(one, two, three)                        |
|                                                  |    end;                                           |
+--------------------------------------------------+---------------------------------------------------+
   
Python collects any number of keyword arguments into a dictionary.  In
Dylan, ``#rest keys`` collects the remaining arguments, not into a
dictionary, but into a vector (like a property list or plist):
alternating symbol-value pairs.

Loops
-----

for
^^^

+--------------------------------------------------+---------------------------------------------------+
| Python                                           | Dylan                                             |
+==================================================+===================================================+
| .. code-block:: python                           | .. code-block:: dylan                             |
|                                                  |                                                   |
|    words = ['one', 'two']                        |    let words = #("one", "two")                    |
|    for w in words:                               |    for (w in words)                               |
|      print(w, len(w))                            |      format-out("%s %d\n", w, w.size)             |
|                                                  |    end;                                           |
+--------------------------------------------------+---------------------------------------------------+
| N/A Tail recursion                               | .. code-block:: dylan                             |
|                                                  |                                                   |
|                                                  |    iterate loop (words = #("one", "two"))         |
|                                                  |      if (~empty?(words))                          |
|                                                  |        let word = head(words);                    |
|                                                  |        format-out("%s %d\n", word, word.size);    |
|                                                  |        loop(tail(words))                          |
|                                                  |      end                                          |
|                                                  |    end                                            |
+--------------------------------------------------+---------------------------------------------------+

while
^^^^^

+--------------------------------------------------+---------------------------------------------------+
| Python                                           | Dylan                                             |
+==================================================+===================================================+
| .. code-block:: python                           | .. code-block:: dylan                             |
|                                                  |                                                   |
|    a, b = 0, 1                                   |    let (a, b) = values(0, 1);                     |
|    while (a < 10):                               |    while (a < 10)                                 |
|      print(a)                                    |      format-out("%d\n", a);                       |
|      a, b = b, a+b                               |      let temp = a;                                |
|                                                  |      a := b; b := temp + b;                       |
|                                                  |    end                                            |
+--------------------------------------------------+---------------------------------------------------+
| N/A Tail recursion                               | .. code-block:: dylan                             |
|                                                  |                                                   |
|                                                  |    iterate loop (a = 0, b = 1)                    |
|                                                  |      if (a < 10)                                  |
|                                                  |         format-out("%d\n", a);                    |
|                                                  |         loop(b, a + b)                            |
|                                                  |      end                                          |
|                                                  |    end                                            |
+--------------------------------------------------+---------------------------------------------------+

until
^^^^^

+--------------------------------------------------+---------------------------------------------------+
| Python                                           | Dylan                                             |
+==================================================+===================================================+
| N/A                                              | .. code-block:: dylan                             |
|                                                  |                                                   |
|                                                  |    let count = 5;                                 |
|                                                  |    until (count = 0)                              |
|                                                  |      format-out("Count is %d\n", count);          |
|                                                  |      count := count - 1;                          |
|                                                  |    end;                                           |
+--------------------------------------------------+---------------------------------------------------+
| N/A                                              | .. code-block:: dylan                             |
|                                                  |                                                   |
|                                                  |    // tail recursion                              |
|                                                  |    iterate loop (count = 5)                       |
|                                                  |      if (count > 0)                               |
|                                                  |        format-out("Count is %d\n", count);        |
|                                                  |        loop(count - 1)                            |
|                                                  |      end                                          |
|                                                  |    end                                            |
+--------------------------------------------------+---------------------------------------------------+

break
^^^^^

+--------------------------------------------------+---------------------------------------------------+
| Python                                           | Dylan                                             |
+==================================================+===================================================+
| .. code-block:: python                           | .. code-block:: dylan                             |
|                                                  |                                                   |
|    for i in range(10):                           |    block(break)                                   |
|      if i == 5:                                  |      for (i from 0 below 10)                      |
|        break                                     |        if (i = 5) break() end;                    |
|      print(i)                                    |        format-out("%d\n", i);                     |
|                                                  |      end;                                         |
|                                                  |    end                                            |
+--------------------------------------------------+---------------------------------------------------+

continue
^^^^^^^^

+--------------------------------------------------+---------------------------------------------------+
| Python                                           | Dylan                                             |
+==================================================+===================================================+
| .. code-block:: python                           | .. code-block:: dylan                             |
|                                                  |                                                   |
|    for i in range(5):                            |    for (i from 0 below 5)                         |
|      if i == 2:                                  |      block(continue)                              |
|        continue                                  |        if (i = 2) continue() end;                 |
|      print(i)                                    |        format-out("%d\n", i);                     |
|                                                  |      end;                                         |
|                                                  |    end                                            |
+--------------------------------------------------+---------------------------------------------------+

pass
^^^^

+--------------------------------------------------+---------------------------------------------------+
| Python                                           | Dylan                                             |
+==================================================+===================================================+
| .. code-block:: python                           | .. code-block:: dylan                             |
|                                                  |                                                   |
|    if some_condition:                            |    if (some-condition)                            |
|      pass                                        |      // do nothing                                |
|                                                  |    end                                            |
+--------------------------------------------------+---------------------------------------------------+

match
^^^^^

+--------------------------------------------------+---------------------------------------------------+
| Python                                           | Dylan                                             |
+==================================================+===================================================+
| .. code-block:: python                           | .. code-block:: dylan                             |
|                                                  |                                                   |
|    def http_error(status):                       |    define function http-error(status)=>(msg)      |
|      match status:                               |      select (status)                              |
|        case 400:                                 |        400 => "Bad request";                      |
|          return "Bad request"                    |        404 => "Not found";                        |
|        case 404:                                 |        otherwise => "Something went wrong";       |
|          return "Not found"                      |      end                                          |
|        case _:                                   |    end                                            |
|          return "Something went wrong"           |                                                   |
+--------------------------------------------------+---------------------------------------------------+

+--------------------------------------------------+---------------------------------------------------+
| Python                                           | Dylan                                             |
+==================================================+===================================================+
| .. code-block:: python                           | Dylan has no built-in pattern matching but        |
|                                                  | *match* can be implemented in a library as a      |
|    # point is an (x, y) tuple                    | macro.                                            |
|    def say(point):                               |                                                   |
|      match point:                                |                                                   |
|        case (0, 0):                              |                                                   |
|          print("Origin")                         |                                                   |
|        case (0, y):                              |                                                   |
|          print(f"Y={y}")                         |                                                   |
|        case (x, 0):                              |                                                   |
|          print(f"X={x}")                         |                                                   |
|        case (x, y):                              |                                                   |
|          print(f"X={x}, Y={y}")                  |                                                   |
|        case _:                                   |                                                   |
|          raise ValueError("Not a point")         |                                                   |
+--------------------------------------------------+---------------------------------------------------+

.. _rectangle rule: https://opendylan.org/proposals/dep-0012-string-literals.html#the-rectangle-rule
