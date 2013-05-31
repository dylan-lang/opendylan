A Dylan Primer for Scheme Programmers
=====================================

This document was originally authored by Jonathan Sobel.

Almost everything you already do in Scheme can be translated easily into
Dylan. In fact, with one exception, Dylan is a proper superset of Scheme.
The one exception is that continuations have indefinite extent in Scheme
and dynamic extent in Dylan. Some individual implementations of Dylan
might provide a mechanism for producing continuations with indefinite
extent, making all Scheme programs be Dylan programs, too. (Of course,
you won't be taking full advantage of the power of Dylan if you only
write Scheme programs in it.)

This document is really just a large table in two columns. On the left,
you will see a Scheme expression, and on the right, its Dylan counterpart.
If one column contains N/A, then that language has no corresponding
direct way to express what is in the other column.

Please remember, there is much more to Dylan than what you'll find here!
For example, this document doesn't show you how to define new classes or
create generic functions (functions which support ad hoc polymorphic
behavior). This document is only intended to help ease your transition
from Scheme to Dylan.

Literals
--------

+------------------------+-----------------------+
| Scheme                 | Dylan                 |
+========================+=======================+
| .. code-block:: scheme | .. code-block:: dylan |
|                        |                       |
|    #t                  |    #t                 |
+------------------------+-----------------------+
| .. code-block:: scheme | .. code-block:: dylan |
|                        |                       |
|    #f                  |    #f                 |
+------------------------+-----------------------+
| .. code-block:: scheme | .. code-block:: dylan |
|                        |                       |
|    23                  |    23                 |
+------------------------+-----------------------+
| .. code-block:: scheme | .. code-block:: dylan |
|                        |                       |
|    #b1011              |    #b1011             |
+------------------------+-----------------------+
| .. code-block:: scheme | .. code-block:: dylan |
|                        |                       |
|    #o644               |    #o644              |
+------------------------+-----------------------+
| .. code-block:: scheme | .. code-block:: dylan |
|                        |                       |
|    #x2A5F              |    #x2A5F             |
+------------------------+-----------------------+
| .. code-block:: scheme | N/A                   |
|                        |                       |
|    -4/5                |                       |
+------------------------+-----------------------+
| .. code-block:: scheme | .. code-block:: dylan |
|                        |                       |
|    6.02E23             |    6.02E23            |
+------------------------+-----------------------+
| .. code-block:: scheme | .. code-block:: dylan |
|                        |                       |
|    #\a                 |    'a'                |
+------------------------+-----------------------+
| .. code-block:: scheme | .. code-block:: dylan |
|                        |                       |
|    #\newline           |    '\n'               |
+------------------------+-----------------------+
| .. code-block:: scheme | .. code-block:: dylan |
|                        |                       |
|    "Hello"             |    "Hello"            |
+------------------------+-----------------------+
| N/A                    | .. code-block:: dylan |
|                        |                       |
|                        |    "Hello\n"          |
+------------------------+-----------------------+
| .. code-block:: scheme | .. code-block:: dylan |
|                        |                       |
|    'apple              |    #"apple"           |
|                        |    apple:             |
+------------------------+-----------------------+
| N/A                    | .. code-block:: dylan |
|                        |                       |
|                        |    #"two words"       |
+------------------------+-----------------------+
| .. code-block:: scheme | .. code-block:: dylan |
|                        |                       |
|    '(1 #\a dog)        |    #(1, 'a', #"dog")  |
+------------------------+-----------------------+
| .. code-block:: scheme | .. code-block:: dylan |
|                        |                       |
|    '#(5 10 15)         |    #[5, 10, 15]       |
+------------------------+-----------------------+
| .. code-block:: scheme | N/A                   |
|                        |                       |
|    `(1 2 ,x ,@y)       |                       |
+------------------------+-----------------------+

Syntax
------

Note that, in Dylan, any words after an ``end`` (e.g.
``end method``) are optional.

+----------------------------------+---------------------------------------+
| Scheme                           | Dylan                                 |
+==================================+=======================================+
| .. code-block:: scheme           | .. code-block:: dylan                 |
|                                  |                                       |
|    (define var exp)              |    define variable var = exp          |
|                                  |    define constant var = exp          |
+----------------------------------+---------------------------------------+
| .. code-block:: scheme           | .. code-block:: dylan                 |
|                                  |                                       |
|    (f x y z)                     |    f(x, y, z)                         |
+----------------------------------+---------------------------------------+
| .. code-block:: scheme           | .. code-block:: dylan                 |
|                                  |                                       |
|    (begin 1 2 3)                 |    begin 1; 2; 3; end                 |
|                                  |    begin 1; 2; 3 end                  |
+----------------------------------+---------------------------------------+
| .. code-block:: scheme           | N/A                                   |
|                                  |                                       |
|    (quote datum)                 |                                       |
+----------------------------------+---------------------------------------+
| .. code-block:: scheme           | .. code-block:: dylan                 |
|                                  |                                       |
|    (lambda (x y . z)             |    method (x, y, #rest z)             |
|      (say "hello")               |      say("hello");                    |
|      (f x y z)                   |      f(x, y, z);                      |
|    )                             |    end method                         |
+----------------------------------+---------------------------------------+
| .. code-block:: scheme           | .. code-block:: dylan                 |
|                                  |                                       |
|    (let ((x 5))                  |    let x = 5;                         |
|      body)                       |    body                               |
|                                  |    // (Scope ends at next             |
|                                  |    // "body-ender.")                  |
+----------------------------------+---------------------------------------+
| N/A                              | .. code-block:: dylan                 |
|                                  |                                       |
|                                  |    let (x, y) = exp;                  |
|                                  |    // (Binds multiple values          |
|                                  |    // returned by exp.)               |
+----------------------------------+---------------------------------------+
| .. code-block:: scheme           | .. code-block:: dylan                 |
|                                  |                                       |
|    (let ((x 5) (y 6))            |    let (x, y) = values(5, 6);         |
|      (f x y))                    |    f(x, y)                            |
+----------------------------------+---------------------------------------+
| .. code-block:: scheme           | .. code-block:: dylan                 |
|                                  |                                       |
|    (letrec ((f (lambda (x)       |    local method f (x)                 |
|            f-body)               |      f-body                           |
|       )                          |    end method f,                      |
|       (g (lambda (y z)           |    method g (y, z)                    |
|            g-body)               |      g-body                           |
|       ))                         |    end method g;                      |
|      body)                       |    body                               |
+----------------------------------+---------------------------------------+
| .. code-block:: scheme           | .. code-block:: dylan                 |
|                                  |                                       |
|    (if test                      |    if (test)                          |
|        (begin then1              |      then1;                           |
|         then2)                   |      then2;                           |
|        (begin else1              |    else                               |
|         else2)                   |      else1;                           |
|    )                             |      else2;                           |
|                                  |    end if                             |
+----------------------------------+---------------------------------------+
| .. code-block:: scheme           | .. code-block:: dylan                 |
|                                  |                                       |
|    (set! var value)              |    var := value                       |
+----------------------------------+---------------------------------------+
| .. code-block:: scheme           | .. code-block:: dylan                 |
|                                  |                                       |
|    (and a b c)                   |    a & b & c                          |
+----------------------------------+---------------------------------------+
| .. code-block:: scheme           | .. code-block:: dylan                 |
|                                  |                                       |
|    (or a b c)                    |    a | b | c                          |
+----------------------------------+---------------------------------------+
| .. code-block:: scheme           | .. code-block:: dylan                 |
|                                  |                                       |
|    (cond                         |    case                               |
|     (test1 result1)              |      test1 => result1;                |
|     (test2 result2)              |      test2 => result2;                |
|     (else result)                |      otherwise => result;             |
|    )                             |    end case                           |
+----------------------------------+---------------------------------------+
| .. code-block:: scheme           | .. code-block:: dylan                 |
|                                  |                                       |
|    (case exp                     |    select (exp)                       |
|     ((a 2) result1)              |      #"a", 2 => result1;              |
|     (('a' 'b') result2)          |      'a', 'b' => result2;             |
|     (else result)                |      otherwise => result;             |
|    )                             |    end select                         |
+----------------------------------+---------------------------------------+
| N/A                              | .. code-block:: dylan                 |
|                                  |                                       |
|                                  |    select (exp by comparison-func)    |
|                                  |      f(x) => result1;                 |
|                                  |      g(y), h(z) => result2;           |
|                                  |      otherwise => result;             |
|                                  |    end select                         |
+----------------------------------+---------------------------------------+
| .. code-block:: scheme           | .. code-block:: dylan                 |
|                                  |                                       |
|    (do ((var1 init1 step1)       |    for (var1 = init1 then step1,      |
|         (var2 init2 step2))      |         var2 = init2 then step2,      |
|        (test result)             |         until: test)                  |
|      body                        |      body                             |
|    )                             |    finally result                     |
|                                  |    end for                            |
+----------------------------------+---------------------------------------+

Predefined functions
--------------------

These are organized based on the "Standard Procedures" section of R4RS.

+--------------------------------+-----------------------------------------------+
| **Boolean functions**                                                          |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (not obj)                   |    ~ obj                                      |
|                                |    ~obj                                       |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (boolean? obj)              |    instance?(obj, <boolean>)                  |
+--------------------------------+-----------------------------------------------+
| **Equivalence predicates**                                                     |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (eqv? x y)                  |    x == y                                     |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | N/A                                           |
|                                |                                               |
|    (eq? x y)                   |                                               |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (equal? x y)                |    x = y                                      |
+--------------------------------+-----------------------------------------------+
| **Pairs and Lists**                                                            |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (pair? obj)                 |    instance?(obj, <pair>)                     |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (cons x y)                  |    pair(x, y)                                 |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (car ls)                    |    head(ls)                                   |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (cdr ls)                    |    tail(ls)                                   |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (set-car! ls val)           |    head-setter(val, ls)                       |
|                                |    head(ls) := val                            |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (set-cdr! ls val)           |    tail-setter(val, ls)                       |
|                                |    tail(ls) := val                            |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (cadadr ls)                 |    N/A                                        |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (null? obj)                 |    instance?(obj, <empty-list>)               |
|                                |    obj = #()                                  |
|                                |    empty?(ls) // most common                  |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (list? obj)                 |    instance?(obj, <list>)                     |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (list x y z)                |    list(x, y, z)                              |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (length ls)                 |    size(ls)                                   |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (append ls1 ls2 ls3)        |    concatenate(ls1, ls2, ls3)                 |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (reverse ls)                |    reverse(ls)                                |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (list-ref ls n)             |    element(ls, n)                             |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (member obj ls)             |    member?(obj, ls)                           |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (memv obj ls)               |    member?(obj, ls, test: \==)                |
+--------------------------------+-----------------------------------------------+
| **Symbols**                                                                    |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (symbol? obj)               |    instance?(obj, <symbol>)                   |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (symbol->string sym)        |    as(<string>, sym)                          |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (string->symbol str)        |    as(<symbol>, str)                          |
+--------------------------------+-----------------------------------------------+
| **Numerical operations**                                                       |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (number? obj)               |    instance?(obj, <number>)                   |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (complex? obj)              |    instance?(obj, <complex>)                  |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (real? obj)                 |    instance?(obj, <real>)                     |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (rational? obj)             |    instance?(obj, <rational>)                 |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (integer? obj)              |    instance?(obj, <integer>)                  |
|                                |    integral?(num)                             |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (= n1 n2)                   |    n1 = n2                                    |
|                                |    n1 == n2                                   |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (< n1 n2)                   |    n1 < n2                                    |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (> n1 n2)                   |    n1 > n2                                    |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (<= n1 n2)                  |    n1 <= n2                                   |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (>= n1 n2)                  |    n1 >= n2                                   |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (zero? n)                   |    zero?(n)                                   |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (positive? n)               |    positive?(n)                               |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (negative? n)               |    negative?(n)                               |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (odd? i)                    |    odd?(i)                                    |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (even? i)                   |    even?(i)                                   |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (+ 1 2 3)                   |    1 + 2 + 3                                  |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (* 1 2 3)                   |    1 * 2 * 3                                  |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (- 5 3)                     |    5 - 3                                      |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (/ 2.3 1.7)                 |    2.3 / 1.7                                  |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (- x)                       |    - x                                        |
|                                |    -x                                         |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (expt 2 16)                 |    2 ^ 16                                     |
+--------------------------------+-----------------------------------------------+
| *[Most of the standard Scheme numeric functions (e.g.  max, remainder) are     |
| defined similarly in Dylan.  No need  to list them all here.]*                 |
+--------------------------------+-----------------------------------------------+
| ** Characters**                                                                |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (char? obj)                 |    instance?(obj, <character>)                |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (char=? char1 char2)        |    char1 = char2                              |
|                                |    char1 == char2                             |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (char<? char1 char2)        |    char1 < char2                              |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (char>? char1 char2)        |    char1 > char2                              |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (char<=? char1 char2)       |    char1 <= char2                             |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (char>=? char1 char2)       |    char1 >= char2                             |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (char->integer char)        |    as(<integer>, char)                        |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (integer->char n)           |    as(<character>, n)                         |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (char-upcase char)          |    as-uppercase(char)                         |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (char-downcase char)        |    as-lowercase(char)                         |
+--------------------------------+-----------------------------------------------+
| **Strings**                                                                    |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (string? obj)               |    instance?(obj, <string>)                   |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (make-string k char)        |    make(<string>, size: k, fill: char)        |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | N/A                                           |
|                                |                                               |
|    (string char ...)           |                                               |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (string-length str)         |    size(str)                                  |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (string-ref str k)          |    element(str, k)                            |
|                                |    str[k]                                     |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (string-set! str k char)    |    element-setter(char, str, k)               |
|                                |    str[k] := char                             |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (string=? str1 str2)        |    str1 = str2                                |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (string<? str1 str2)        |    str1 < str2                                |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (substring str start end)   |    copy-sequence(str, start: start, end: end) |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (string-append str1 str2)   |    concatenate(str1, str2)                    |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (string->list str)          |    as(<list>, str)                            |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (list->string chars)        |    as(<string>, chars)                        |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (string-copy str)           |    shallow-copy(str)                          |
|                                |    copy-sequence(str)                         |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (string-fill! str char)     |    fill!(str, char)                           |
+--------------------------------+-----------------------------------------------+
| **Vectors**                                                                    |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (vector? obj)               |    instance?(obj, <vector>)                   |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (make-vector k fill)        |    make(<vector>, size: k, fill: fill)        |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (vector obj ...)            |    vector(obj, ...);                          |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (vector-length vec)         |    size(vec)                                  |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (vector-ref vec k)          |    element(vec, k)                            |
|                                |    vec[k]                                     |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (vector-set! vec k obj)     |    element-setter(obj, vec, k)                |
|                                |    vec[k] := obj                              |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (vector->list vec)          |    as(<list>, vec)                            |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (list>vector list)          |    as(<vector>, list)                         |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (vector-fill! vec obj)      |    fill!(vec, obj)                            |
+--------------------------------+-----------------------------------------------+
| **Control Features**                                                           |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (procedure? obj)            |    instance?(obj, <function>)                 |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (apply proc arg1 arg2 args) |    apply(proc, arg1, arg2, args)              |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (map proc list1 list2)      |    map(proc, list1, list2)                    |
+--------------------------------+-----------------------------------------------+
| N/A                            | .. code-block:: dylan                         |
|                                |                                               |
|                                |    map(proc, vec1, vec2)                      |
+--------------------------------+-----------------------------------------------+
| N/A                            | .. code-block:: dylan                         |
|                                |                                               |
|                                |    map(proc, string1, string2)                |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (for-each proc list1 list2) |    do(proc, list1, list2)                     |
+--------------------------------+-----------------------------------------------+
| **Continuations**                                                              |
|                                                                                |
| As mentioned before, continuations have dynamic extent in Dylan. Also, whereas |
| ``call/cc`` is a function, Dylan uses a syntax form to grab a continuation.    |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (call/cc                    |    block (k)                                  |
|      (lambda (k)               |      body                                     |
|        body))                  |    end block                                  |
+--------------------------------+-----------------------------------------------+
| .. code-block:: scheme         | .. code-block:: dylan                         |
|                                |                                               |
|    (call/cc                    |    block (k)                                  |
|      (lambda (k)               |      body                                     |
|        (dynamic-wind           |    cleanup                                    |
|          (lambda () #f)        |      cleanup-stuff                            |
|          (lambda () body)      |    end block                                  |
|          (lambda ()            |                                               |
|             cleanup-stuff))))  |                                               |
+--------------------------------+-----------------------------------------------+
