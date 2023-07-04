************************
An IDL Binding for Dylan
************************

Introduction
==============

This chapter proposes an IDL binding for Dylan. It is based on the
draft OMG Request For Comment submission.

Document conventions
--------------------

The requirements of this specification are indicated by the verb
“shall”. All other statements are either explanatory, amplifying, or
relaxing. All implementation notes are so labeled.

Dylan names within the text and all example Dylan code appears in
fixed-point font, ``like this``.

Bibliography
--------------

This specification mentions the following documents.

[DP 97]
   Neal Feinberg, S. E. Keene, R.O. Mathews, P. T. Withington,
   :title-reference:`Dylan Programming`, Addison Wesley, 1997.

[DRM 96]
   \L. M. Shalit, :title-reference:`The Dylan Reference Manual`,
   Addison Wesley, 1996.

[OMG 94.3.11]
   \T. J. Mowbray, and K. L. White, OMG IDL Mapping for Common Lisp,
   OMG 94.3.11, 1994.

[OMG 98.07.01]
   The Common Object Request Broker: Architecture and Specification,
   Revision 2.2, formal/98-07-01,
   <https://www.omg.org/spec/CORBA/2.2>, July 1998.

Design rationale
================

Glossary of terms
-----------------

This document uses terms from both the CORBA 2.2 specification [OMG
98.7.1] and the Dylan Reference Manual [DRM 96]. Any additional
terminology is described below.

Design philosophy
-----------------

Linguistic requirements
^^^^^^^^^^^^^^^^^^^^^^^

The design should be:

Conformant
   Provide a mapping that conforms to the CORBA 2.0 specification.

Complete
   Provide a complete language mapping between IDL and Dylan.

Correct
   Provide a mapping that correctly maps legal IDL definitions to
   equivalent Dylan definitions.

Consistent
   Provide a mapping that is consistent in its translation of IDL
   constructs to Dylan constructs.

Natural
   Provide a mapping that produces Dylan definitions that would be
   judged to be expressed in “natural Dylan”.

Stable
   Small changes in an IDL description should not lead to
   disproportionately large changes in the mapping to Dylan.

Concise
   Common constructs should be mapped in as simple and direct a manner
   as possible. If necessary to achieve this goal, rare pieces of
   syntax can be traded off and absorb the linguistic fallout.

Engineering requirements
^^^^^^^^^^^^^^^^^^^^^^^^

The design should be:

Extension-free
   Do not require extensions to CORBA.

Implementation-Free
   Do not provide implementation descriptions, except as explanatory notes.

Reliable
   The mapping should not adversely impact the reliability of clients
   or implementations of services built on an ORB.

Efficient
   The mapping should allow for as efficient an implementation as is
   provided by other language bindings.

Portable
   The mapping should use standard Dylan constructs.

Encapsulated
   The mapping should hide and not constrain the implementation details.

Miscellaneous requirements
^^^^^^^^^^^^^^^^^^^^^^^^^^

The design should also be:

Rationalized
   Provide a rationale for key design decisions.

Mapping summary
---------------

The following table summarizes the mapping of IDL constructs to Dylan constructs.

IDL constructs mapped to Dylan constructs.

+---------------------------------+-----------------------------------------------------+
| This IDL construct ...          | ... Maps to this Dylan construct                    |
+=================================+=====================================================+
| ``Foo_Bar``                     | ``Foo-Bar``                                         |
+---------------------------------+-----------------------------------------------------+
| ``Foo::Bar``                    | ``Foo/Bar``                                         |
+---------------------------------+-----------------------------------------------------+
| ``Foo + Bar``                   | ``Foo + Bar``                                       |
+---------------------------------+-----------------------------------------------------+
| :file:`filename.idl`            | ``define library filename-protocol ...``            |
|                                 |                                                     |
|                                 | ``define library filename-stubs ...``               |
|                                 |                                                     |
|                                 | ``define library filename-skeletons ...``           |
+---------------------------------+-----------------------------------------------------+
| ``module Foo { ... Bar ... }``  | ``Foo/Bar``                                         |
+---------------------------------+-----------------------------------------------------+
| ``interface Foo { ... }``       | ``define open abstract class <foo>``                |
|                                 |                                                     |
|                                 | ``(<object>) ...``                                  |
+---------------------------------+-----------------------------------------------------+
| ``interface Foo : Bar { ... }`` | ``define open abstract class <foo>``                |
|                                 | ``(<bar>) ...``                                     |
+---------------------------------+-----------------------------------------------------+
| ``const long FOO ...``          | ``define constant $FOO ...``                        |
+---------------------------------+-----------------------------------------------------+
| ``long``                        | ``CORBA/<long>``                                    |
+---------------------------------+-----------------------------------------------------+
| ``typedef Foo ...``             | ``define constant <Foo> ...``                       |
+---------------------------------+-----------------------------------------------------+
| ``enum Foo {Bar ... }``         | ``define constant <Foo> = one-of(#"bar", ...)``     |
+---------------------------------+-----------------------------------------------------+
| ``struct Foo { ... }``          | ``define class <Foo> (CORBA/<struct>) ...``         |
+---------------------------------+-----------------------------------------------------+
| ``union Foo { ... }``           | ``define class <Foo> (CORBA/<union>) ...``          |
+---------------------------------+-----------------------------------------------------+
| ``sequence``                    | ``CORBA/<sequence>``                                |
+---------------------------------+-----------------------------------------------------+
| ``string``                      | ``CORBA/<string>``                                  |
+---------------------------------+-----------------------------------------------------+
| ``array``                       | ``CORBA/<array>``                                   |
+---------------------------------+-----------------------------------------------------+
| ``exception Foo { ... }``       | ``define class <Foo> (CORBA/<user-exception>) ...`` |
+---------------------------------+-----------------------------------------------------+
| ``void Foo ( ... )``            | ``define open generic Foo ( ... ) => ()``           |
+---------------------------------+-----------------------------------------------------+
| ``any``                         | ``CORBA/<any>``                                     |
+---------------------------------+-----------------------------------------------------+

Lexical mapping
===============

This section specifies the mapping of IDL identifiers, literals, and constant expressions.

Identifiers
-----------

Background
^^^^^^^^^^

IDL identifiers are defined as follows [OMG 98.7.1]:

   An identifier is an arbitrarily long sequence of alphabetic, digit,
   and underscore (``_``) characters. The first character must be an
   alphabetic character. All characters are significant.

   Identifiers that differ only in case collide and yield a
   compilation error. An identifier for a definition must be spelled
   consistently (with respect to case) throughout a specification.

   ...

   There is only one namespace for OMG IDL identifiers. Using the same
   identifier for a constant and an interface, for example, produces a
   compilation error.

Dylan identifiers defined are as follows [DRM 96]:

   A name is one of the following four possibilities:

      - An alphabetic character followed by zero or more name characters.
      - A numeric character followed by two or more name characters
        including at least two alphabetic characters in a row.

      - A graphic character followed by one or more name characters
        including at least one alphabetic character.

      - A ``\`` (backslash) followed by a function operator.

   where:

      - Alphabetic case is not significant except within character and string literals. ...

      - An alphabetic character is any of the 26 letters of the Roman
        alphabet in upper and lower case.

      - A numeric character is any of the 10 digits.

      - A graphic character is one of the following: ! & * < = > | ^ $ % @ _

      - A name character is an alphabetic character, a numeric character, a graphic character, or one of the following: - + ~ ? /

Specification
^^^^^^^^^^^^^

From Section A.3.1.1 it can be seen that Dylan identifiers are a
superset of IDL identifiers, and therefore they shall be left
unmodified in the mapping except as follows.

IDL provides only underscores to separate individual words, while
Dylan identifiers conventionally use hyphens to separate individual
words. A mapping shall therefore translate underscores to hyphens.

There are some reserved words in Dylan:

A reserved word is a syntactic token that has the form of a name but
is reserved by the Dylan language and so cannot be given a binding and
cannot be used as a named value reference. There are seven reserved
words in Dylan: ``define``, ``end``, ``handler``, ``let``, ``local``,
``macro``, and ``otherwise``.

When an IDL identifier collides with a reserved Dylan word, the string
“``-%``” shall be appended to the end of the identifier. The “``%``”
character is used instead of, say, the string “``idl``”, because it
cannot occur in an IDL identifier, and so we avoid having to deal with
cases where the Dylan identifier together with an existing “``-idl``”
suffix also appears in the IDL description.

This “``-%``” suffix shall also be added to IDL identifiers ending in
“``-setter``” in order to prevent potential collisions with setter
functions mapped from IDL attributes.

There are further grammar-driven modifications of identifiers due to
scope and convention described `The mapping of IDL to Dylan`_.

Examples
^^^^^^^^

Some example mappings of IDL identifiers to Dylan identifiers:

IDL identifiers mapped to Dylan identifiers.

+------------------------+-----------------------------------+
| This IDL identifier... | ... maps to this Dylan identifier |
+========================+===================================+
| ``fusion``             | ``fusion``                        |
+------------------------+-----------------------------------+
| ``Fusion``             | ``Fusion``                        |
+------------------------+-----------------------------------+
| ``cold_fusion``        | ``cold-fusion``                   |
+------------------------+-----------------------------------+
| ``let``                | ``let-%``                         |
+------------------------+-----------------------------------+
| ``RED_SETTER``         | ``RED-SETTER-%``                  |
+------------------------+-----------------------------------+
| ``isExothermic``       | ``isExothermic``                  |
+------------------------+-----------------------------------+

.. note::
   The case of all characters is not lowered in order to avoid
   modifying acronyms like: ``do_TLA`` → ``do-tla``.

.. note::
   Similarly hyphens are not inserted at lower-to-upper case
   boundaries in order to avoid mistaken translations like
   ``LaTex_Parser`` → ``la-te-x-parser``.

Literals
--------

IDL literals shall be mapped to lexically equivalent Dylan literals or
semantically equivalent Dylan expressions. The following subsections
describe the mapping for integers, floating-point numbers, and
characters.

Integers
^^^^^^^^

Background
""""""""""
Integer literals are defined as follows in IDL [OMG 98.7.1]:

   An integer literal consisting of a sequence of digits is taken to
   be decimal (base ten) unless it begins with 0 (digit zero). A
   sequence of digits starting with 0 is taken to be an octal integer
   (base eight). The digits 8 and 9 are not octal digits. A sequence
   of digits preceded by ``0x`` or ``0X`` is taken to be a hexadecimal
   integer (base sixteen). The hexadecimal digits include ``a`` or
   ``A`` through ``f`` or ``F`` with decimal values ten through
   fifteen, respectively.

The corresponding integer literals are defined as follows in Dylan:

   A sequence of decimal digits denote a decimal number.

   The characters “``#o``” followed by a sequence of octal digits
   denote an octal number.

   The characters “``#x``” followed by a sequence of hexadecimal
   digits denote a hexadecimal number.

Specification
"""""""""""""

A mapping shall therefore prepend the characters “``#o``” to the
beginning of an octal literal. For a hexadecimal literal a mapping
shall therefore remove the characters “``0x``” or “``0X``” from the
beginning and prepend the characters “``#x``” in their place.

Floating point numbers
^^^^^^^^^^^^^^^^^^^^^^

Background
""""""""""

Floating point literals are defined as follows in IDL [OMG 98.7.1]:

   A floating-point literal consists of an integer part, a decimal
   point, a fraction part, an ``e`` or ``E``, and an optionally signed
   integer exponent. The integer and fraction parts both consist of a
   sequence of decimal (base ten) digits. Either the integer part or
   the fraction part (but not both) may be missing; either the decimal
   point or the letter ``e`` (or ``E`` ) and the exponent (but not
   both) may be missing.

The corresponding floating point literals are defined similarly in
Dylan; see `Numbers
<https://opendylan.org/books/drm/Lexical_Grammar#XREF-2105>`_ in the
:title-reference:`Dylan Reference Manual`.

Specification
"""""""""""""

No modification shall be made to floating point literals during
translation.

Character literals
^^^^^^^^^^^^^^^^^^

Background
""""""""""

IDL character literals are single printing characters, or escape
sequences, enclosed by single quotes. The escape sequences are as
follows:

IDL character literal escape sequences.

+-----------------+-----------------+
| Description     | Escape Sequence |
+=================+=================+
| newline         | ``\n``          |
+-----------------+-----------------+
| horizontal tab  | ``\t``          |
+-----------------+-----------------+
| vertical tab    | ``\v``          |
+-----------------+-----------------+
| backspace       | ``\b``          |
+-----------------+-----------------+
| carriage return | ``\r``          |
+-----------------+-----------------+
| form feed       | ``\f``          |
+-----------------+-----------------+
| alert           | ``\a``          |
+-----------------+-----------------+
| backslash       | ``\\``          |
+-----------------+-----------------+
| question mark   | ``\?``          |
+-----------------+-----------------+
| single quote    | ``\'``          |
+-----------------+-----------------+
| double quote    | ``\"``          |
+-----------------+-----------------+
| octal           | ``\ooo``        |
+-----------------+-----------------+
| hexadecimal     | ``\xhh``        |
+-----------------+-----------------+

Dylan character literals are defined in `Character and String Literals
<https://opendylan.org/books/drm/Lexical_Grammar#HEADING-117-38>`_ in
the :title-reference:`Dylan Reference Manual`.

Specification
"""""""""""""

A mapping shall leave a single printing character unmodified during
translation. A mapping shall leave escape sequences unmodified except
as follows:

IDL character-literal escape sequences mapped to Dylan.

+---------------+---------------------+-------------------+
| Description   | IDL Escape Sequence | Dylan Translation |
+===============+=====================+===================+
| vertical tab  | ``\v``              | ``\<0B>``         |
+---------------+---------------------+-------------------+
| question mark | ``\?``              | ``?``             |
+---------------+---------------------+-------------------+
| double quote  | ``\"``              | ``"``             |
+---------------+---------------------+-------------------+
| octal         | ``\ooo``            | ``\<hh>``         |
+---------------+---------------------+-------------------+
| hexadecimal   | ``\xhh``            | ``\<hh>``         |
+---------------+---------------------+-------------------+

String Literals
^^^^^^^^^^^^^^^

Background
""""""""""

IDL defines a string literal as follows:

   A string literal is a sequence of characters (as defined in
   “Character Literals” ...) surrounded by double quotes, as in
   ``"`` ... ``"``.  Adjacent string literals are concatenated. Characters
   in concatenated strings are kept distinct.

Dylan defines a string literal as follows:


Specification
"""""""""""""

A mapping shall leave string literals unmodified during translation
except as follows. Escape sequences shall be modified in accordance
with the specification for character literals, with one exception:
``\"`` is left unmodified.

Fixed point decimals
----------------------

Background
^^^^^^^^^^

IDL defines a fixed point decimal literal as follows:

   A fixed point decimal literal consists of an integer part, a
   decimal part, a fraction part, and a ``d`` or a ``D``. The integer
   and fraction parts both consist of a sequence of decimal (base 10)
   digits. Either the integer part of the fractional part (but not
   both) may be missing; the decimal point (but not the letter ``d``
   (or ``D``)) may be missing.

Dylan has no defined fixed point decimal literal format.

Specification
^^^^^^^^^^^^^

A fixed point decimal literal shall be mapped to any available Dylan
representation of the value.

Constant expressions
--------------------

A mapping shall either interpret the IDL constant expression yielding
an equivalent Dylan literal or build a Dylan constant expression that
will yield the same value.

Operators
^^^^^^^^^

The IDL operators shall be interpreted as, or translated to, Dylan as
defined by the following table. Note that the Dylan expressions will
necessarily have whitespace around the operators even if the IDL
expressions do not.

IDL operators mapped to Dylan.

+------------------+------------+---------------------+
| Operation        | IDL        | Dylan               |
+==================+============+=====================+
| Bitwise Or       | ``x | y``  | ``logior(x, y)``    |
+------------------+------------+---------------------+
| Bitwise Xor      | ``x ^ y``  | ``logxor(x, y)``    |
+------------------+------------+---------------------+
| Bitwise And      | ``x & y``  | ``logand(x, y)``    |
+------------------+------------+---------------------+
| Bitwise Not      | ``~ x``    | ``lognot(x)``       |
+------------------+------------+---------------------+
| Shift Left       | ``x << y`` | ``ash(x, y)``       |
+------------------+------------+---------------------+
| Shift Right      | ``x >> y`` | ``ash(x, -y)``      |
+------------------+------------+---------------------+
| Add              | ``x + y``  | ``x + y``           |
+------------------+------------+---------------------+
| Subtract         | ``x - y``  | ``x - y``           |
+------------------+------------+---------------------+
| Multiply         | ``x * y``  | ``x * y``           |
+------------------+------------+---------------------+
| Divide (integer) | ``x / y``  | ``truncate/(x, y)`` |
+------------------+------------+---------------------+
| Divide (float)   | ``x / y``  | ``x / y``           |
+------------------+------------+---------------------+
| Remainder        | ``x % y``  | ``modulo(x, y)``    |
+------------------+------------+---------------------+
| Plus             | ``+ x``    | ``+ x``             |
+------------------+------------+---------------------+
| Minus            | ``- y``    | ``- y``             |
+------------------+------------+---------------------+

The mapping of IDL to Dylan
===========================

This section specifies the syntactic and semantic mapping of OMG IDL
to Dylan. Unless otherwise noted, the mapping is applicable to both
client-side and server-side interfaces. Issues specific to the
server-side only are covered in clearly marked subsections.

Names
-----

Identifiers
^^^^^^^^^^^

The lexical mapping of identifiers shall be as specified in Section
A.3.1.2, “Specification”

Scoped names
^^^^^^^^^^^^

Specification
"""""""""""""

Dylan has very different scoping rules from IDL. In particular, Dylan
is not able to introduce new subordinate namespaces at all the
linguistic points that IDL allows: files, modules, interfaces,
structures, unions, operations, and exceptions. Except for files, the
mapping shall handle this by appending together all the enclosing
scope identifiers and the scoped identifier, separating them by
forward slashes. See Section A.4.2, “IDL Files” for the mapping of IDL
files to Dylan.

Rationale
"""""""""

This is basically what several other languages have done [OMG
98.7.1]. Dylan has the concept of modules, but these are more
linguistically heavyweight than the nested scopes they would be trying
to model. Modules would also not allow out-of-scope references in the
way that IDL does through its scope delimiter “::”.

The slash character is used instead of the hyphen character so in
examples like the following the two IDL identifiers below do not clash
after translation::

  Moorcock::Michael
  Moorcock_Michael

Examples
""""""""
.. code-block:: idl

   // IDL
   eco::umberto
   SOCIETIES::Secret::knights_templar

.. code-block:: dylan

   // Dylan
   eco/umberto
   SOCIETIES/Secret/knights-templar

IDL Files
---------

Specification
^^^^^^^^^^^^^

An IDL file shall be mapped to three Dylan libraries each exporting a
single module with the same name as its respective library.


The three libraries shall be given the same name as the original IDL
file minus its ``.idl`` extension, but adding the suffixes
``-protocol``, ``-stubs``, and ``-skeletons`` respectively.

The protocol library shall minimally use the Dylan library, or a
library that uses it an re-exports its bindings. The stubs and
skeletons libraries shall similarly minimally use the Dylan library,
the Dylan-ORB library (see Section A.4.3), and the protocol library;
and shall re-export the latter’s bindings.

Unless otherwise specified, Dylan constructs introduced as part of the
IDL mapping shall be created in the protocol library.

Rationale
^^^^^^^^^

The advantages of a mandatory mapping of a complete IDL description to
a particular structure of Dylan libraries is:

- Libraries are the natural large-scale unit of reuse in Dylan, and
  will match expectations.

- Dylan programmers, applications, and tools will be able to rely on
  the Dylan “signature” of a service defined by an IDL description.

- Enforcing the creation of a libraries allows any required runtime
  support to be naturally separated into and used from subordinate
  libraries.

If multiple IDL files are required to be combined into a single trio
of libraries, then a single top level file can be used to include them
together with any extra IDL module declarations required to prevent
name clashes.

For example, an application library wishing to invoke the operations
described in the IDL file :file:`http.idl` should use the
``http-stubs`` library. Similarly, an application library wishing to
implement the operations described in the same IDL file should use the
``http-skeletons`` library.

The protocol library is available separately for applications wishing
to make use of the Dylan framework generated from the IDL without
necessarily using an ORB for communication. For example, an
application team may wish to introduce the discipline of an IDL
description early on in the project lifecycle so that development work
on the clients and servers can proceed in parallel using dummy local
implementations of the other components.

Implementation notes
^^^^^^^^^^^^^^^^^^^^

An implementation is free to map an IDL file to as many Dylan source
files as is convenient.

An implementation is encouraged to transfer comments from IDL source
files to the generated Dylan files.


The DYLAN-ORB library
---------------------

The Dylan mapping relies on some runtime support, for example the
built in types like ``corba/<short>``, and this shall be provided in a
Dylan library called Dylan-ORB that shall be used by a Dylan library
generated from an IDL file. Similarly, the Dylan-ORB library shall
define and export a Dylan module called Dylan-ORB that shall be used
by a Dylan module generated from an IDL file.

The Dylan-ORB library can be used independently of libraries generated
from IDL files to build generic applications without specific
knowledge of particular services.

Mapping modules
---------------

Background
^^^^^^^^^^

IDL modules define a name scope for other declarations including subordinate modules.

Specification
^^^^^^^^^^^^^

An IDL module shall be mapped to a Dylan identifier prefix for
identifiers declared in the scope of the module declaration as defined
by Section A.4.1.2, “Scoped names”.

Rationale
^^^^^^^^^

Although mapping an IDL module to a Dylan module would seem to be more
natural, Dylan modules do not support out-of-scope references to
identifiers, for example, in IDL, ``foo::bar``.

Examples
^^^^^^^^
.. code-block:: idl

   // IDL
   module physics {
     module quantum_mechanics {
       interface schroedinger {};
     };
   };

.. code-block:: dylan

   //  Dylan
   define open class physics/quantum-mechanics/<schroedinger> (<object>)
   end class;

Mapping for interfaces
----------------------

Background
^^^^^^^^^^

The CORBA standard [OMG 98.7.1] states:

   An interface is a description of a set of possible operations that
   a client may request of an object. An object satisfies an interface
   if it can be specified as the target object in each potential
   request described by the interface.

   In practice, an interface declaration introduces a name scope and
   defines a set of operations on, and attributes of, the interface.

Specification
^^^^^^^^^^^^^

An IDL interface shall be mapped to an open, abstract, Dylan class
with no superclasses other than :drm:`<object>` and classes generated
from inherited IDL interfaces.

The implementation dependent classes used to represent object
references (see Section A.5.2.1, “Object references” and servants (see
section Section A.5.5.1, “Servants”) shall be subclasses of the open
abstract classes, and shall be defined in the stubs and skeletons
libraries respectively.

An IDL interface shall also be mapped to a Dylan identifier prefix for
identifiers declared in the scope of the class declaration as defined
by Section A.4.1.2. Angle brackets shall be added to the start and end
of the Dylan class name in accordance with Dylan programming
conventions.

A forward declaration of an IDL interface shall not be mapped to
anything.

Rationale
^^^^^^^^^

Dylan classes are the natural focus of protocol definition and also
allow IDL interface inheritance to be modeled by Dylan class
inheritance (see below).

The class is abstract because it is an interface to an
implementation. On the client side the class is uninstantiable since
it is meaningless for Dylan client code to call the :drm:`make`
generic function on an arbitrary remote class. Instead the client must
acquire object references by invoking operations on factory objects as
defined in the IDL description of the particular service concerned.

The class is open to allow users of the server module to implement the
interface by subclassing.

The class has no superclasses other than :drm:`<object>` or those that
might be mapped from inherited IDL interfaces in order that the
protocol library, generated from the IDL interface, may be used
independently of any runtimes, stubs, or skeletons, as an abstract
Dylan protocol.

IDL allows name-only “forward declarations” of interfaces in order to
allow interfaces to refer to one another. Two Dylan classes can refer
to one another, but there is no special forward reference declaration
form. Furthermore, the definition of the language only encourages
implementations to support forward references, repeated definitions
are not allowed, and bindings are not created in any prescribed order.

It therefore appears that there is nothing particular which the
definition of the mapping can do concerning ordering or extra
definitions, to ensure that mapped Dylan classes can be compiled by
all Dylan implementations.

Examples
^^^^^^^^
.. code-block:: idl

   // IDL
   interface T34 {};

.. code-block:: dylan

   // Dylan
   define open abstract class <T34> (<object>)
   end class;

Mapping for interface inheritance
---------------------------------

Background
^^^^^^^^^^

The CORBA standard [OMG 98.7.1] states:

Interface inheritance provides the composition mechanism for
permitting an object to support multiple interfaces. The principal
interface is simply the most-specific interface that the object
supports, and consists of all operations in the transitive closure of
the interface inheritance graph.

CORBA interfaces can inherit from multiple other interfaces, and Dylan
also supports multiple inheritance. However, Dylan’s multiple
inheritance is more constrained. The class precedence order must be
consistent. That is, any pair of classes must be in the same order
with respect to each other wherever they occur together.

Specification
^^^^^^^^^^^^^

Interface inheritance shall be mapped to class inheritance in
Dylan. The superclass list for the resulting Dylan class shall be
canonicalized using lexicographic order. That is, the order shall be
alphabetic on the character set used for Dylan identifiers.

Rationale
^^^^^^^^^

Class inheritance is the natural means of sharing protocols in Dylan.

The superclass list needs to be canonicalized to avoid legal IDL
interface inheritance lists mapping to illegal Dylan class precedence
lists. Reordering cannot affect method selection for IDL operations
because IDL explicitly prohibits this kind of overloading. An
alphabetic order is as good as any.

Examples
^^^^^^^^

.. code-block:: idl

   // IDL
   interface T34 : tank soviet_made {};
   interface T48 : soviet_made tank {};
   interface T1000 : T48 T34 {};

.. code-block:: dylan

   // Dylan
   define open abstract class <T34> (<soviet-made>, <tank>)
   end class;

   define open abstract class <T48> (<soviet-made>, <tank>)
   end class;

   define open abstract class <T1000> (<T34>, <T48>)
   end class;

Mapping for constants
---------------------

Specification
^^^^^^^^^^^^^

IDL constant declarations shall be mapped to Dylan constant
definitions. IDL constant expressions are mapped as defined in Section
A.2.3, “Mapping summary”. In addition, a dollar character shall be
added to front of the main identifier, but after the scope prefix, in
accordance with Dylan programming conventions.

Examples
^^^^^^^^

.. code-block:: idl

   // IDL
   module time {
     const unsigned long SECS_IN_100_YRS
       = 100 * 365 * 24 * 60 * 60;
   };

.. code-block:: dylan

   // Dylan
   define constant time/$SECS-IN-100-YRS :: CORBA/<unsigned-long>
     = 100 * 365 * 24 * 60 * 60;

   // Or alternatively
   define constant time/$SECS-IN-100-YRS :: CORBA/<unsigned-long>
     = 3153600000;

Mapping for basic types
-----------------------

Overall Background
^^^^^^^^^^^^^^^^^^

IDL and Dylan both provide a number of basic built in types and the
means of constructing and naming new types.

Specification
^^^^^^^^^^^^^

The mapping shall introduce new Dylan types for each CORBA type. The
mapping of the new Dylan types to the built-in Dylan types shall be
constrained as specified in the following sections, but within these
constraints the mapping is implementation dependent.

A generic constraint shall be that legal Dylan literals within the
ranges allowed by the type are allowed as values for these new Dylan
types. That is, it shall not be necessary to call a constructor
function on these values.

Rationale
^^^^^^^^^

This allows implementations of the mapping some latitude, but also
allows CORBA applications written in Dylan to succinctly and portably
declare their data types and gain such efficiency as is provided by
the combination of the mapping implementation and the Dylan
compiler. The CORBA-specific types also protect application source
code against underlying changes, and ensure that the code
automatically benefits from any improvements.

The literals constraint means that programmers can expect to be able
to use, say, the literal “2” where a CORBA ``short`` is expected.

Integers
^^^^^^^^

Background
""""""""""

IDL defines six types of integer with the following ranges:

+------------------------+-----------------------------------+
| IDL integer type       | Range                             |
+========================+===================================+
| ``short``              | -2\ :sup:`15` .. 2\ :sup:`15` - 1 |
+------------------------+-----------------------------------+
| ``long``               | -2\ :sup:`31` .. 2\ :sup:`31` - 1 |
+------------------------+-----------------------------------+
| ``long long``          | -2\ :sup:`63` .. 2\ :sup:`63` - 1 |
+------------------------+-----------------------------------+
| ``unsigned short``     | 0 .. 2\ :sup:`16` - 1             |
+------------------------+-----------------------------------+
| ``unsigned long``      | 0 .. 2\ :sup:`32` - 1             |
+------------------------+-----------------------------------+
| ``unsigned long long`` | 0 .. 2\ :sup:`64` - 1             |
+------------------------+-----------------------------------+

Dylan has the class :drm:`<integer>` which is required to be at least
28 bits of precision. Overflow behavior is implementation defined.

Specification
"""""""""""""

All IDL integer types shall be mapped to the following Dylan classes.

IDL and Dylan integer types.

+------------------------+--------------------------------+
| IDL integer type       | Dylan CORBA integer type       |
+========================+================================+
| ``short``              | ``CORBA/<short>``              |
+------------------------+--------------------------------+
| ``long``               | ``CORBA/<long>``               |
+------------------------+--------------------------------+
| ``long long``          | ``CORBA/<long-long>``          |
+------------------------+--------------------------------+
| ``unsigned short``     | ``CORBA/<unsigned-short>``     |
+------------------------+--------------------------------+
| ``unsigned long``      | ``CORBA/<unsigned-long>``      |
+------------------------+--------------------------------+
| ``unsigned long long`` | ``CORBA/<unsigned-long-long>`` |
+------------------------+--------------------------------+

These classes, in turn, shall be defined as aliases for, or subtypes
of, some Dylan implementation’s integer classes, and shall be capable
of representing the specified range of values.

Rationale
"""""""""

The rationale is as given for the general case above. In this
particular instance, although an individual Dylan compiler could
convert a :drm:`limited` expression to the best concrete class that the
runtime supports, this is not guaranteed. The runtime may have a good
class for implementing a CORBA class, but the compiler may not be
capable of translating a :drm:`limited` expression into it.

Even if the translation of the :drm:`limited` expression to the best
runtime class was guaranteed, the expressions are quite long an
cumbersome to use repeatedly in code, and an alias is convenient.

Examples
""""""""

.. code-block:: idl

   // IDL
   const long DIM_OF_UNIV = 11;

.. code-block:: dylan

   // Dylan
   define constant $DIM_OF_UNIV :: CORBA/<long> = 11;

   // Some alternative binding implementations
   define constant CORBA/<long> = <integer>;
   define constant CORBA/<long> = <machine-word>;
   define constant CORBA/<long> =
     limited(<integer>, min: -(2 ^ 31), max: (2 ^ 31) -1);

Floating-point numbers
^^^^^^^^^^^^^^^^^^^^^^

Background
""""""""""

IDL defines three types of floating-point numbers:

IDL floating point number types.

+-----------------+----------------------------------------------+
| IDL float type  | Range                                        |
+=================+==============================================+
| ``float``       | ANSI/IEEE 754-1985 single precision          |
+-----------------+----------------------------------------------+
| ``double``      | ANSI/IEEE 754-1985 double precision          |
+-----------------+----------------------------------------------+
| ``long double`` | ANSI/IEEE 754-1985 double-extended precision |
+-----------------+----------------------------------------------+

The Dylan types :drm:`<single-float>`, :drm:`<double-float>`, and
:drm:`<extended-float>` are intended to correspond to the IEEE types
but may not.

Specification
"""""""""""""
The IDL floating-point types shall be mapped to Dylan as follows:

IDL floating point types mapped to Dylan.

+-----------------+-------------------------+
| IDL float type  | Dylan float type        |
+=================+=========================+
| ``float``       | ``CORBA/<float>``       |
+-----------------+-------------------------+
| ``double``      | ``CORBA/<double>``      |
+-----------------+-------------------------+
| ``long double`` | ``CORBA/<long-double>`` |
+-----------------+-------------------------+

These classes, in turn, shall be aliases for or subclasses of the
Dylan :drm:`<float>` class and shall be capable of representing the specified
range of values.

Rationale
"""""""""

As above.

Examples
""""""""

.. code-block:: idl

   // IDL
   const double E = 2.71828182845904523536;
   const float LYRS_TO_ALPHA_CENTAURI = 4.35;

.. code-block:: dylan

   // Dylan
   define constant $E :: CORBA/<double> = 2.71828182845904523536;
   define constant $LYRS-TO-ALPHA-CENTAURI :: CORBA/<float> = 4.35;


Fixed-point decimals
^^^^^^^^^^^^^^^^^^^^

Background
""""""""""

IDL fixed-point decimals are defined as follows:

   The ``fixed`` data type represents a fixed-point decimal number of
   up to 31 significant digits. The scale factor is normally a
   non-negative integer less than or equal to the total number of
   digits (note that constants with effectively negative scale, such
   as 10000, are always permitted.). However, some languages and
   environments may be able to accommodate types that have a negative
   scale or scale greater then than the number of digits.

Dylan has no defined fixed-point decimal type, but does have a rational type.

Specification
"""""""""""""

The IDL ``fixed`` type shall be mapped to the Dylan class
``CORBA/<fixed>``, which shall be a subtype of the Dylan type
``<rational>``.

Subtypes of the IDL ``fixed`` type of the form ``fixed<d,s>``, where d
is the number of digits and s is the scale, shall be mapped to Dylan
types of the form

.. code-block:: dylan

   limited(CORBA/<fixed>, digits: d, scale: s)

The Dylan language operators and functions on rationals shall have
methods defined on instances of ``CORBA/<fixed>``.

In addition, instances of ``CORBA/<fixed>`` shall support the
following functions:

.. code-block:: dylan

   CORBA/Fixed/digits(x :: CORBA/<Fixed>)
     => (digits :: CORBA/<unsigned-short>)

   CORBA/Fixed/scale(x :: CORBA/<Fixed>)
     => (scale :: CORBA/<short>)

   as(class == CORBA/<long-double>, x :: CORBA/<Fixed>)
     => (value :: CORBA/<long-double>)

   as(class :: subclass(CORBA/<Fixed>), x :: CORBA/<Fixed>)
     => (fixed :: CORBA/<Fixed>)

   as(class :: subclass(CORBA/<Fixed>), x :: CORBA/<long-double>)
     => (fixed :: CORBA/<Fixed>)

   as(class :: subclass(CORBA/<Fixed>), x :: CORBA/<long>)
     => (fixed :: CORBA/<Fixed>)

Rationale
"""""""""

This seems to be the natural mapping to Dylan and approximately
mirrors the C++ mapping.

Examples
""""""""

.. code-block:: idl

   // IDL
   const fixed<6,2> salary_increment = 0100.50d;

.. code-block:: dylan

   // Dylan
   define constant $foo
       :: limited(CORBA/<Fixed>, digits: 6, scale: 2)
     = make(limited(CORBA/<Fixed>, digits: 6, scale: 2),
            digits: "100.5");

Characters
^^^^^^^^^^

Background
""""""""""

IDL characters are elements of the 8 bit ISO Latin-1 (8859.1)
character set. Dylan’s characters are unspecified. Dylan has a
:drm:`<character>` class and has three string classes:
:drm:`<string>`, :drm:`<byte-string>`, and
:drm:`<unicode-string>`. Objects of these string types have elements
that are subtypes of :drm:`<character>`.

Specification
"""""""""""""

The IDL ``char`` type shall be mapped to the Dylan class
``CORBA/<char>``, which will be an alias for or a subclass of the
Dylan class ``<character>``.

Rationale
"""""""""

As above.

Examples
""""""""

.. code-block:: idl

   // IDL
   const char ALEPH = 'a';

.. code-block:: dylan

   // Dylan
   define constant $ALEPH :: CORBA/<char> = 'a';

Wide characters
^^^^^^^^^^^^^^^

Background
""""""""""

IDL wide characters are implementation defined.

Dylan defines a :drm:`<unicode-string>` type.

Specification
"""""""""""""

The IDL ``wchar`` type shall be mapped to ``CORBA/<wchar>``, which
shall be a subclass of ``<character>``. Instances of ``CORBA/<wchar>``
shall be allowed as elements of instances of ``<unicode-string>``.

Rationale
"""""""""

The natural mapping to Dylan.

Examples
""""""""

.. code-block:: idl

   // IDL
   const wchar ALEPH = 'a';

.. code-block:: dylan

   // Dylan
   define constant $ALEPH :: CORBA/<wchar> = 'a';

Boolean values
^^^^^^^^^^^^^^

Background
""""""""""

IDL ``boolean`` type can take the values ``TRUE`` or ``FALSE``.

Dylan’s :drm:`<boolean>` class similarly can take the values ``#t``,
and ``#f``.

Specification
"""""""""""""

The IDL ``boolean`` type shall be mapped onto the Dylan
``CORBA/<boolean>`` class which shall be an alias for the Dylan
:drm:`<boolean>` class.

Rationale
"""""""""

The extra CORBA prefix class is introduced for completeness and
consistency, but there is no need to allow it to be a subclass of the
built-in class.

Examples
""""""""

.. code-block:: idl

   // IDL
   const boolean CANTORS_HYPOTHESIS = TRUE;

.. code-block:: dylan

   // Dylan
   define constant $CANTORS-HYPOTHESIS :: CORBA/<boolean> = #t;

Octets
^^^^^^

Background
""""""""""

An IDL octet is an 8-bit quantity that undergoes no conversion of
representation during transmission.

Specification
"""""""""""""

The IDL ``octet`` type shall be mapped to the Dylan class
``CORBA/<octet>`` which shall be an alias for or a subclass of some
Dylan implementation’s integer class allowing values in the range 0
to 255.

Rationale
"""""""""

As above.

Examples
""""""""

.. code-block:: idl

   // IDL
   const octet BOND_ID = 007;

.. code-block:: dylan

   // Dylan
   define constant $BOND-ID :: CORBA/<octet> = #o007;

The “any” type
^^^^^^^^^^^^^^

Background
""""""""""

The IDL ``any`` type permits the specification of values that can
express any OMG IDL type.

Dylan is a dynamic language with runtime type information.

Specification
"""""""""""""

The IDL ``any`` type shall be mapped to a sealed Dylan class
``CORBA/<any>`` with sealed getter and setter generic functions and
initialization keywords for the underlying value and the associated
typecode:

.. code-block:: dylan

   define generic CORBA/any/type
       (any :: CORBA/<any>) 
    => (typecode :: CORBA/<typecode>);

   define generic CORBA/any/type-setter
       (typecode :: CORBA/<typecode>, any :: CORBA/<any>)
    => (typecode :: CORBA/<typecode>);

   define generic CORBA/any/value
       (any :: CORBA/<any>)
    => (value :: <object>);

   define generic CORBA/any/value-setter
       (value :: <object>, any :: CORBA/<any>)
    => (value :: <object>);

In addition, anywhere that an object of type ``Any`` is required the
Dylan programmer can supply objects that are instances of any mapped
IDL type.

At least one of the ``value:`` initialization and ``typecode:``
keywords shall be required. If the latter is not supplied then it is
coerced from the value in an implementation defined manner.

Explicit coercion to and from objects of type ``Any`` shall be
provided by sealed methods on the Dylan generic function :drm:`as`.

The function ``CORBA/any/value`` shall signal an error if the value
cannot be coerced to a native Dylan type corresponding to a mapped IDL
type.

Rationale
"""""""""

Although it is awkward for the Dylan programmer to have to deal with
an explicit ``Any`` type, it allows the typecode to be preserved
across requests and replies in cases where it matters.

In some cases, for example where the ``Any`` contains a structure
whose Dylan type is unknown (to the current program) it is not
possible for ``CORBA/any/value`` to return a meaningful value. In
these cases the ``DynAny`` interface should be used to navigate the
data inside the ``Any``.

Examples
""""""""

.. code-block:: idl

   // IDL
   long goedel_number(in any thing);

.. code-block:: dylan

   // Dylan
   define open generic goedel-number (thing :: CORBA/<any>)
     => (result :: CORBA/<long>);

.. note::
   The mapping of operations is described in more detail later.

Mapping for constructed types
-----------------------------

Mapping for typedefs
^^^^^^^^^^^^^^^^^^^^

Background
""""""""""

An IDL ``typedef`` declaration introduces aliases for a given type.

Dylan has a single namespace for identifiers and so no separate
defining form is needed to introduce a new alias for a class.

Specification
"""""""""""""

An IDL ``typedef`` declaration shall be mapped to as many Dylan
``define constant`` definitions as there are declarators being
introduced by the IDL declaration.

Examples
""""""""

.. code-block:: idl

   // IDL
   typedef short mozart_symphony_no, layston_park_house_no;

.. code-block:: dylan

   // Dylan
   define constant <mozart-symphony-no> = CORBA/<short>;
   define constant <layston-park-house-no> = CORBA/<short>;


Mapping for enumeration types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Background
""""""""""

An IDL enumerated type consists of ordered lists of identifiers.

Specification
"""""""""""""

An IDL enumerated type shall be mapped to a type union of singleton
symbol types. In addition, four sealed generic functions shall be
defined on the enumerated type (as if in its scope) for traversing and
comparing the enumerated values: ``successor``, ``predecessor``,
``<``, and ``>``.

It shall be an error to call these functions on symbols outside the
enumeration.

Rationale
"""""""""

This is the straightforward implementation of enumerated types
described in [DP 97]. We retain this basic format with a view to
benefiting from the compiler optimizations encouraged in [DP
97]. However, we also specify successor, predecessor, and comparison
functions for convenience.

Examples
""""""""

.. code-block:: idl

   // IDL
   enum planet {Mercury, Venus, Earth, Mars,
                Jupiter, Saturn, Uranus, Neptune, Pluto};

.. code-block:: dylan

   // Dylan
   define constant <planet>
     = apply(type-union,
   	  map(singleton, #(#"Mercury", #"Venus",
   			   #"Earth", #"Mars", #"Jupiter", #"Saturn",
   			   #"Uranus", #"Neptune", #"Pluto")));

   define generic planet/successor
       (value :: <planet>) => (succ :: <planet>);

   define generic planet/predecessor (value :: <planet>)
    => (pred :: <planet>);

   define generic planet/<(lesser :: <planet>, greater :: <planet>)
    => (lesser? :: <boolean>);

   define generic planet/> (greater :: <planet>, lesser :: <planet>)
    => (greater? :: <boolean>);

Mapping for structure types
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Background
""""""""""

IDL defines a ``structure`` type that aggregates together multiple
pieces of data of potentially heterogeneous types.

Dylan programmers define new classes for this purpose.

Specification
"""""""""""""

An IDL structure shall be mapped to a sealed, concrete, Dylan subclass
of ``CORBA/<struct>`` together with pairs of sealed getter and setter
generic functions and a required initialization keyword for each
struct member. The initialization keywords shall be Dylan symbols
mapped using the normal identifier mapping rules, but without any
scope prefixes. It shall be an error to call the getter and setter
functions on instances of types other than those mapped from the IDL
structure. Furthermore the Dylan protocol functions :drm:`make` and
:drm:`initialize` shall be sealed over the domain of the mapped class.

Rationale
"""""""""

The Dylan class, the getters, the setters, and initializers, are
defined to be sealed in the anticipation that operations on structures
are expected to be as efficient as possible without any need for
extension.

There is no need to specify whether the getter and setter generic
functions are defined as Dylan slots. The data may in fact be
maintained in a foreign internal format convenient for network
transmission.

The initialization keywords are required so as not to introduce
complicated defaulting rules.

The superclass ``CORBA/<struct>`` is made explicit to allow
``instance?`` tests.

Examples
""""""""

.. code-block:: idl

   // IDL
   struct meeting {
     string topic, venue, convenor;
     long date, duration;
     sequence<string> attendees, agenda, hidden_agenda, minutes;
   };

.. code-block:: dylan

  // Dylan (using slots)
  define class <meeting> (CORBA/<struct>)
    slot meeting/topic :: CORBA/<string>,
      required-init-keyword: topic:;

    slot meeting/venue :: CORBA/<string>,
      required-init-keyword: venue:;

    slot meeting/convenor :: CORBA/<string>,
      required-init-keyword: convenor:;

    slot meeting/date :: CORBA/<long>,
      required-init-keyword: date:;

    slot meeting/duration :: CORBA/<long>,
      required-init-keyword: duration:;

    slot meeting/attendees ::
       limited(CORBA/<sequence>, of: CORBA/<string>)>,
       required-init-keyword: attendees:; 

     slot meeting/agenda ::
        limited(CORBA/<sequence>, of: CORBA/<string>)>,
        required-init-keyword: agenda:; 

     slot meeting/hidden-agenda :: 
       limited(CORBA/<sequence>, of: CORBA/<string>)>,
       required-init-keyword: hidden-agenda:; 

     slot meeting/minutes ::
       limited(CORBA/<sequence>, of: CORBA/<string>),
       required-init-keyword: minutes:; 
   end class;

   define sealed domain make (singleton(<meeting>));

   define sealed domain initialize (<meeting>);

Mapping for discriminated union type
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Background
""""""""""

IDL defines a ``union`` type that allows data of heterogeneous types
used interchangeably in places like parameters, results, arrays, and
sequences. An explicit tag called a discriminator is used to determine
the type of the data in a given object that is of the union type.

Dylan is a dynamic language with runtime type information and has no
explicit tagging mechanism.

Specification
"""""""""""""

An IDL union type shall be mapped to a sealed, concrete, Dylan
subclass of ``CORBA/<union>`` with pairs of sealed getter and setter
functions and an initialization keyword for each union branch. Every
mapped union shall also have the following sealed getter and setter
functions::

   corba/union/discriminator
   corba/union/discriminator-setter 
   corba/union/value
   corba/union/value-setter

and the following initialization keywords::

   discriminator:
   value:

It is an error to call these functions on instances of types other
than those mapped from the IDL union definition. Furthermore the Dylan
protocol functions:drm:`make` and :drm:`initialize` shall be sealed
over the domain of the mapped class.

The initialization keywords shall be mapped as for structs. However,
they are not required in the same manner. Instead, either the caller
shall supply the ``discriminator:`` and the ``value:`` or an
initialization keyword mapped from one of the branches.

In addition, wherever a union is required (for example, in the
parameter of an operation) the Dylan programmer shall be able to give
any Dylan object that is an instance of one of the types of the
branches of the union.

Explicit coercion to and from a union shall also be available as
sealed methods on the Dylan :drm:`as` generic function. It is
undefined which discriminator is used in ambiguous cases.

Rationale
"""""""""

Although it is unnatural for a Dylan programmer to have to manipulate
explicit union discriminators, there are ambiguous cases that require
this explicit treatment. By reifying the union the Dylan programmer is
given as much direct control as a static language provides, and yet
can also use the implicit coercion and value getter to ignore the
details if so desired.

It is not necessary to state whether the getter and setter functions
are implemented by slots.

The superclass ``CORBA/<union>`` is made explicit to allow
``instance?`` tests.

Examples
""""""""

.. code-block:: idl

   // IDL
   union RLE_entity switch (short) {
     case 1: long length;
     case 2: char character;
   };

.. code-block:: dylan

   // Dylan (sample)
   define class <RLE-entity> (CORBA/<union>)
   end class;

   define sealed domain make (singleton(<RLE-entity>));
   define sealed domain initialize (<RLE-entity>);

   define sealed method as
       (class == <RLE-entity>, length :: CORBA/<long>)
    => (object :: <RLE-entity>)
     make(<RLE-entity>, length: length);
   end method;

   define sealed method as
       (class == CORBA/<long>, object :: <RLE-entity>)
    => (length :: CORBA/<long>)
     RLE-entity/length(object);
   end method;

   define method RLE-entity/length (union :: <RLE-entity>)
    => (length :: CORBA/<long>)
     select (corba/union/discriminator(union))
       1 => corba/union/value(union);
       otherwise => error(...);
     end select;
   end method;

   define method RLE-entity/length-setter
       (length :: CORBA/<long>, union :: <RLE-entity>)
    => (length :: CORBA/<long>)
     corba/union/value(union) := length;
     corba/union/discriminator(union) := 1;
   end method;

   // ...

Mapping for sequence type
^^^^^^^^^^^^^^^^^^^^^^^^^

Background
""""""""""

IDL defines a ``sequence`` type. A sequence is a one-dimensional array
with an element type, an optional maximum size (fixed at compile
time), and a current length (determined at run time).

Dylan defines several sequence-like classes including :drm:`<sequence>`
itself.

Specification
"""""""""""""

The IDL ``sequence`` type shall be mapped onto the a new Dylan
``CORBA/<sequence>`` class that shall be an alias for or a subclass of
the Dylan :drm:`<stretchy-vector>` class. An element type shall be
mapped to a ``limited`` type of ``CORBA/<sequence>``. The maximum
size is not modeled in Dylan and must be checked on marshalling.

Rationale
"""""""""

Dylan’s :drm:`<stretchy-vector>` class appears closest in intent to
IDL’s ``sequence`` type.

Examples
""""""""

.. code-block:: idl

   // IDL
   typedef sequence<long, CHAIN-MAX> chromosomes;

.. code-block:: dylan

   // Dylan
   define constant <chromosomes> =
     limited(CORBA/<sequence>, of: CORBA/<long>);

Mapping for string type
^^^^^^^^^^^^^^^^^^^^^^^

Background
""""""""""

IDL defines a ``string`` type. A string is a one-dimensional array of
all possible 8-bit quantities except NUL, with an optional maximum
size.

A string is similar to a sequence of ``char``.

Dylan defines :drm:`<string>`, :drm:`<byte-string>`, and
:drm:`<unicode-string>` classes.

Specification
"""""""""""""

The IDL ``string`` type shall be mapped onto a Dylan
``CORBA/<string>`` class that shall be an alias for or a subclass of
the Dylan ``<string>`` class.

Rationale
"""""""""

The :drm:`<byte-string>` class is not mandated in the mapping to
retain the flexibility and efficiency of the underlying Dylan
implementation. The effect of storing a NUL in a ``CORBA/<string>``
that is passed as an argument to a request is undefined.

Examples
""""""""

.. code-block:: idl

   // IDL
   typedef string constellation;

.. code-block:: dylan

   // Dylan
   define constant <constellation> = CORBA/<string>;

Mapping for wide string type
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Background
""""""""""

The IDL ``wstring`` data type represents a sequence of ``wchar`` elements.

Dylan defines a :drm:`<unicode-string>` type.

Specification
"""""""""""""

The ``wstring`` type shall be mapped to the Dylan type
``CORBA/<wstring>`` which shall be an alias for
:drm:`<unicode-string>`.

Rationale
"""""""""

The natural mapping to Dylan.

Examples
""""""""

.. code-block:: idl

   // IDL
   typedef wstring local_name;

.. code-block:: dylan

   // Dylan
   define constant <local-name> = CORBA/<wstring>;

Mapping for array type
^^^^^^^^^^^^^^^^^^^^^^

Background
""""""""""

IDL defines an ``array`` type for multidimensional fixed-size arrays,
with explicit sizes for each dimension.

Dylan has a similar :drm:`<array>` class.

Specification
"""""""""""""

The IDL array type shall be mapped onto the Dylan ``CORBA/<array>``
class. The new class shall be an alias for or a subclass of the Dylan
:drm:`<array>` class. An element type shall be mapped to a limited
type of ``CORBA/<array>``.

Rationale
"""""""""

This is the straightforward, natural mapping, albeit hidden behind a
CORBA-specific class for portability across implementations and
versions.

Examples
""""""""

.. code-block:: idl

   // IDL
   typedef long tensor[3][3][3];

.. code-block:: dylan

   // Dylan
   define constant <tensor> =
     limited(CORBA/<array>, of: CORBA/<long>,
             dimensions: #(3,3,3));

Mapping for exceptions
----------------------

Background
^^^^^^^^^^

IDL defines exceptions as:

   ... struct-like data structures which may be returned to indicate
   that an exceptional situation has occurred during the performance
   of a request.

Dylan defines a rich, object-oriented, condition signalling and
handling facility.

Specification
^^^^^^^^^^^^^

IDL exceptions shall be mapped onto sealed Dylan conditions that are
subclasses of ``CORBA/<user-exception>``, which shall be a subtype of
``CORBA/<exception>``, which itself shall be a subtype of the Dylan
:drm:`<condition>` class. As with IDL structures, any members shall be
mapped to pairs of sealed setter and getter generic functions and
corresponding initialization keywords. It shall be an error to call
these functions on instances of types other than those mapped from the
IDL exception definition. Furthermore the Dylan protocol functions
:drm:`make` and :drm:`initialize` shall be sealed over the domain of
the mapped class.

Conditions shall be signalled in the standard Dylan manner by the
CORBA runtime and not returned or passed as arguments.

Standard system exceptions shall be direct or indirect subclasses of
``CORBA/<system-exception>`` which shall be a subtype of
``CORBA/<exception>``.

Rationale
^^^^^^^^^

This is the natural mapping of IDL exceptions into the Dylan language.

Examples
^^^^^^^^

.. code-block:: idl

   // IDL
   exception melt_down {
     short seconds_remaining;
   };

.. code-block:: dylan

   // Dylan (using slots)
   define class <melt-down> (CORBA/<user-exception>)
     slot melt-down/seconds-remaining :: CORBA/<short>,
       required-init-keyword: seconds-remaining:;
   end class;

  define sealed domain make (singleton(<melt-down>));
  define sealed domain initialize (<melt-down>);

Mapping for operations
----------------------

Background
^^^^^^^^^^

IDL uses operations as the basic means by which CORBA-compliant
programs communicate with each other. Operation declarations are akin
to C function declarations, but they also have to deal with parameter
directions, exceptions, and client contexts. All operations are
defined within the scope of an interface.

Dylan programs call generic functions to communicate with other Dylan
programs. The generic functions are implemented by methods.

Specification
^^^^^^^^^^^^^

An IDL operation shall be mapped to an open Dylan generic
function. The generic function name is subject to the usual identifier
translation specified earlier. It shall be an error to call the
function on instances of types not mapped from the IDL operation
definition.

An IDL operation declared as ``oneway`` shall be mapped on to a
generic function that returns zero results.

The IDL interface object shall become the first parameter to the Dylan
generic function. A IDL operation parameter declared as ``in`` shall
become a parameter of the Dylan generic function. A parameter declared
as ``out`` shall become a result of the Dylan generic function. A
parameter declared as ``inout`` shall become both a parameter and a
result of the Dylan generic function. The Dylan parameters and results
shall maintain the order of the original parameters, with the
interface object and operation return value coming before any further
parameters and results defined by the IDL parameters.

An IDL ``raises`` declaration describes the additional, non-standard,
exceptions that may be raised by invocation of the operation. This is
not mapped to any visible feature of the generic function that is
mapped from the operation declaration. If any exceptions are raised,
however, they shall be signalled and not returned from an operation
request.

An IDL ``context`` declaration describes which additional pieces of
client state the service is passed. When a ``context`` clause is
present this shall be mapped to a ``context:`` keyword argument. When
invoking an operation, if a context is passed in then this shall be
used instead of the ORB’s default context for looking up the property
names listed in the operation’s ``context`` clause. When being
invoked, an operation’s context keyword argument shall be filled by
applying the proper names to the given client context.

Rationale
^^^^^^^^^

This is the natural mapping. The generic function is open to allow the
server module to implement the function by adding methods.

Contexts are mapped are keyword arguments so that client code does not
have to worry about them by default.

Examples
^^^^^^^^

In Parameters
"""""""""""""

.. code-block:: idl

   // IDL
   interface stealth {
     exception power_failure {};
     void engage_cloak (in long power)
       raises (power_failure);
   };

.. code-block:: dylan

   // Dylan
   define open abstract class <stealth> (<object>)
   end class;

   define open generic stealth/engage-cloak
       (stealth :: <stealth>, power :: CORBA/<long>)
    => ();

   define class stealth/<power-failure> (CORBA/<user-exception>)
   end class;

   define sealed domain make (singleton(stealth/<power-failure>));
   define sealed domain initialize (stealth/<power-failure>);

Out Parameters
""""""""""""""

.. code-block:: idl

   // IDL
   interface fuel_cell : power_source {
     short burn_hydrogen
       (in long burn_rate, out sequence<short,7> emissions);
   };

.. code-block:: dylan

   // Dylan
   define open abstract class <fuel-cell> (<power-source>)
   end class;

   define open generic fuel-cell/burn-hydrogen
       (fuel-cell :: <fuel-cell>, burn-rate :: CORBA/<long>)
    => (result :: CORBA/<short>,
        emissions ::limited(CORBA/<sequence>, of: CORBA/<short>));

InOut Parameters
""""""""""""""""

.. code-block:: idl

   // IDL
   interface frame {
     void request-sizes (inout long width, inout long height);
   };

.. code-block:: dylan

   // Dylan
   define open abstract class <frame> (<object>)
   end class;

   define open generic frame/request-sizes
       (frame :: <frame>,
        width :: CORBA/<long>,
        height :: CORBA/<height>)
    => (width :: CORBA/<long>,
        height :: CORBA/<height>);

Mapping for attributes
------------------------

Background
^^^^^^^^^^

IDL attributes implicitly define a pair of accessor functions, one for
retrieving the value of the attribute, and another for setting its
value. The names of the accessor functions are language-mapping
specific, but must not collide with legal operation names specifiable
in IDL. Attributes may be defined as ``readonly``.

Dylan’s slots provide a similar mechanism, including ``constant``
slots.

Specification
^^^^^^^^^^^^^

IDL attributes shall be mapped to a pair of open generic functions,
one getter and one setter. The names of the functions shall be derived
by the usual identifier translation rules, with the addition that the
setter function has the suffix ``-setter``. Attributes declared as
``readonly`` will only have a getter function. It shall be an error to
call these functions on instances of types other than those mapped
from the IDL attribute definition.

If a mapped attribute name would clash with a mapped operation name
then the mapped attribute name shall have ``%`` prepended.

Rationale
^^^^^^^^^

It is not necessary to specify whether the generic functions are
defined by virtual slots.

The identifier translation rules prevent potential name collisions
with the setter functions by appending ``-%`` to the end of an
existing identifier that ends with ``-setter``.

In the main, attribute names are unlikely to clash with operation
names so it seems harsh to punish the normal case with some additional
name mangling. Instead, the penalty is put on defining a clashing
operation name for an existing attribute. Existing source code would
probably then have to be rewritten in order to continue to get at the
old attribute.

Examples
^^^^^^^^

.. code-block:: idl

   // IDL
   interface prisoners_dilemma {
     attribute short mutual_cooperation_reward;
     attribute short mutual_defection_punishment;
     attribute short defectors_temptation;
     attribute short suckers_payoff;
   };

.. code-block:: dylan

   // Dylan
   define open abstract class <prisoners-dilemma> (<object>)
   end class;

   define open generic prisoners-dilemma/mutual-cooperation-reward
       (object :: <prisoners-dilemma>) => (value :: CORBA/<short>);

   define open generic
     prisoners-dilemma/mutual-cooperation-reward-setter
       (value :: CORBA/<short>, object :: <prisoners-dilemma>)
    => (value :: CORBA/<short>);

   // ...

Memory management considerations
--------------------------------

Dylan is a garbage-collected language, however a Dylan program may be
interacting, via an ORB, with remote data in another address space,
and/or with local data not built using the same Dylan implementation
or even the same language. In each case, explicit freeing of some data
may be necessary.

By default, the ORB shall provide copying semantics for arguments and
results and the Dylan garbage collector shall take care of reclaiming
the storage occupied by provably unreferenced data. This includes data
of basic types, constructed types, and object references.

However, a Dylan ORB may optionally provide non-copying semantics
data. That is, Dylan programs may have to deal with pointers to shared
data, because the ORB is acting as an efficient in-process
interoperability layer. In this case, it is expected that the memory
management protocol will be explicitly described in IDL as part of the
contract between the interoperating programs.

Multi-threading considerations
------------------------------

It should be assumed that invocation of a CORBA operation from Dylan
is not thread-safe. That is, if two threads in a Dylan program invoke
the same operation it is not guaranteed that they will be properly
serialized.

An implementation of the IDL-to-Dylan mapping should document whether
it is thread-safe, and if it is should document whether the whole
program or just the thread is blocked.

The mapping of pseudo-objects to Dylan
======================================

Introduction
------------

Background
^^^^^^^^^^

In addition to defining how application objects and data are accessed,
a language mapping must describe how to access some services
implemented directly by the ORB. These services are concerned with
operations like converting an object reference to a string, making a
request through the Dynamic Invocation Interface (DII), building a
``Context``, and so on.

All that is required is that there be some defined mechanism for doing
these things in each language-binding specification. However, most
language-binding specifications take the pseudo-objects approach in
which these ORB services are accessed by applying the binding’s normal
mapping rules to OMG’s IDL descriptions of the interfaces to the
services.

The advantage of this is that programmers can read the OMG’s
descriptions of the interfaces and know how to access them from their
preferred language without learning any additional language-specific
access methods.

The disadvantage is that some of the interfaces turn out to be
particularly clumsy for a given programming language.

Specification
^^^^^^^^^^^^^

The IDL pseudo-object interfaces shall be mapped according to the
Dylan language binding as specified in the earlier parts of this
document, except where it explicitly deviates. In some cases, also
where explicitly specified, there will be an additional access path
that is more convenient for Dylan programmers. A conforming Dylan
language mapping shall support both interfaces.

Rationale
^^^^^^^^^

The pseudo object approach is taken so that experienced CORBA
developers can leverage more of their knowledge and so that any new
pseudo-object services defined by the OMG will automatically be
covered.

However, some of the potential pseudo-object IDL descriptions lead to
awkward interfaces from the Dylan programmer’s point of view and
incompatible abstractions might arise on top of them; built by
different binding. Therefore, in order to provide more natural Dylan
abstractions, there are some additional Dylan-friendly interfaces in
the following sections.

ORB Interface
-------------

Object references
^^^^^^^^^^^^^^^^^

Background
""""""""""

All CORBA interfaces inherit from the ``CORBA::Object`` interface.

Specification
"""""""""""""

An IDL interface shall be mapped to a reference class that is a direct
or indirect subclass of both the interface class itself and
``corba/<object>``.

Rationale
"""""""""

This allows object references and servants to inherit from the same
protocol class and yet retain their own separate behaviors.

Example
"""""""

.. code-block:: idl

   // IDL
   interface ion {};

.. code-block:: dylan

   // Dylan (protocol)
   define open abstract class <ion> (<object>)
   end class;

   // Dylan (possible reference implementation)
   define class <ion-reference> (<ion>, corba/<object>)
   end class;

Object reference equality
^^^^^^^^^^^^^^^^^^^^^^^^^

Background
""""""""""

CORBA allows for (imprecise) equality testing via
``corba::object::is_equivalent``.

Specification
"""""""""""""

In addition to mapping the PIDL function above to
``corba/object/is-equivalent``, a Dylan ORB shall provide a method on
``=`` that performs the same function.

Rationale
"""""""""

It is natural for Dylan programmers to want to use ``=`` on object
references.

Example
"""""""

.. code-block:: dylan

   // Dylan
   // ...
   if (corba/object/is-equivalent(grid1, grid2))
     // ...
   end if;

   // versus
   // ...
   if (grid1 = grid2)
     // ...
   end if;

Nil object references
^^^^^^^^^^^^^^^^^^^^^

Background
""""""""""

CORBA allows for nil object references. These are returned from some
operations to indicate situations where, for example, no real object
reference could be found.

Dylan programmers normally indicate such a situation by returning the
rogue value ``#f`` and by declaring the type of the return value to be
``false-or(<some-type>)``.

Specification
"""""""""""""

NIL object references shall be mapped to interface-specific values
that can be tested with ``CORBA::is_nil``. A nil object reference for
a particular interface shall be obtainable by calling the function
``make-nil`` on the associated protocol class.

Rationale
"""""""""

Using ``#f`` to represent nil object references would be natural and
convenient in some respects, notably during testing. However, all
object reference type declarations would have to be wrapped with
``false-or`` which would be very awkward. Even if we did this then we
would create ambiguous, unorderable methods which all accepted ``#f``
for some generic functions. A less pervasive approach where
``false-or`` was only used for return values would be better, but then
it would be harder to glue operations together. The glue code would
have to check for ``#f`` output from one operation before calling the
next. This seems an inappropriate place to check this, and should be
left to the receiving operation.

There are also advantages in terms of type safety. A spurious ``#f``
is more easily passed to, or returned from, an operation than a more
explicit nil reference.

Example
"""""""

.. code-block:: dylan

   // Dylan
   let philosopher = make-nil(<philosopher>);
   corba/object/is-nil(philosopher); // returns #t

Dynamic Invocation Interface
------------------------------

NVList
^^^^^^

Background
""""""""""

CORBA NVLists are partially opaque in that they can normally only be
created by the ORB ``create_list`` operation and added to by the
``add_item`` operation.

Specification
"""""""""""""

NVLists shall be mapped to CORBA sequences of NamedValues.

Rationale
"""""""""

Since CORBA sequences shall be mapped to a type that supports the
``<stretchy-vector>`` protocol, they can be created and added to using
the normal Dylan calls.

This is in addition to the pseudo IDL interfaces ``create_list`` and
``add_item``.

Example
"""""""

.. code-block:: dylan

   // Dylan
   let args = make(corba/<nvlist>);
   args := add!(args, make(corba/<namedvalue>,
                           name: "foo",
                           argument: as(corba/<any>, 0),
                           len: 0,
                           arg-modes: 0));

Dynamic Skeleton Interface
--------------------------

Dynamic Implementation Routine
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Background
""""""""""

The Dynamic Implementation Routine (DIR) is intended to support a
variety of uses, including dynamic programming/scripting languages,
bridge building, debugging, monitoring, and so on.

The idea is that a particular kind of registered servant is invoked
via the DIR for all operations instead of invoking particular
skeletons and thence particular implementation methods.

An auxiliary function is required for the application to inform the
POA of the repository-ID of the most derived interface supported by
the dynamic servant.

Specification
"""""""""""""

There shall be a subclass of ``portableserver/<servant>`` called
``portableserver/<dynamic-servant>``. Instances of subclasses of this
class registered with the adapter as servants for objects shall have
operations invoked via the DIR function
``corba/serverrequest/invoke``.

The shall be an open generic function
``portableserver/servant/primary-interface`` which shall be called by
the POA to determine the repository-ID of the most derived interface
supported by the dynamic servant.

The pseudo IDL of the above is:

.. code-block:: idl

   // IDL
   module CORBA {
     interface PortableServer::Dynamic_Servant :
       PortableServer::Servant {};
     interface ServerRequest {
       // ...
       void invoke (in PortableServer::Dynamic_Servant servant);
     };
   };

   module PortableServer {
     interface Servant {
     // ...
       RepositoryID primary_interface
         (in ObjectID id, in POA poa)
     };
   };

Rationale
"""""""""

We just need to specify the function that is called on the registered
servant for all operations, plus the auxiliary function for
determining the repository-ID.

Example
"""""""

.. code-block:: dylan

   // Dylan
   define class <buckstop> (portableserver/<dynamic-servant>)
   end class;

   define method corba/serverrequest/invoke
       (request :: corba/<serverrequest>, servant :: <buckstop>)
    => ()
     // ...
   end method;

   define method portableserver/servant/primary-interface
       (servant :: <buckstop>,
        id :: <string>,
        poa :: portableserver/<poa>)
    => (repositoryid :: <string>)
     "LOCAL:buckstop"
  end method;

The Portable Object Adapter
---------------------------

Servants
^^^^^^^^

Background
""""""""""

The ``PortableServer`` module for the Portable Object Adapter (POA)
defines the native ``Servant`` type.

Specification
"""""""""""""

The Dylan mapping for the Servant type shall be the open abstract
class ``portableserver/<servant>``.

An IDL interface shall be mapped to a skeleton class that is a direct
or indirect subclass of both the interface class itself and
``portableserver/<servant>``.

The name of the skeleton class shall be formed by appending
``-servant`` to the interface name and applying the normal identifier
mapping rules.

The skeleton class shall be exported from the skeletons library
generated from the IDL.

Rationale
"""""""""

Only instances of subclasses of ``portableserver/<servant>`` should be
 created.

Examples
""""""""

.. code-block:: dylan

   // Dylan Skeleton (generated from IDL)
   define class <grid-servant>
       (<grid>, portableserver/<servant>)
   end class;

   // Dylan Implementation
   define class <grid-implementation> (<grid-servant>)
   end class;
