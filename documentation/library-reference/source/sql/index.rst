***************
The SQL library
***************

.. current-library:: sql
.. current-module:: sql

.. contents::
   :local:

Introduction
============

Open Dylan's SQL-ODBC library provides a generic Dylan protocol for interfacing
applications to any database management system (DBMS) supporting Microsoft's
Open Database ConnectivityTM (ODBC) interface and the industry-standard database
query language, SQL. The SQL-OBDC library supports the full SQL language defined
in the ANSI SQL-89 and ANSI SQL-92 specifications, as well as any extensions
defined by a DBMS.

A low-level interface to the Microsoft ODBC API is also available in the ODBC-FFI
library. Functional Objects built the ODBC-FFI library using the C-FFI library
and the same C-to-Dylan name mapping scheme as described in the Win32 API FFI
library documentation. See the C FFI and Win32 library reference for details
of that scheme. The ODBC-FFI library is otherwise undocumented.

Implementation
--------------

The SQL-ODBC library is built on top of a generic SQL library. This SQL library
does not include the low-level code necessary to communicate with any particular
DBMS. In itself, the SQL library simply provides a convenient high-level mechanism
for integrating database operations into Dylan applications. It is designed to
form the high-level part of "implementation libraries" that contain lower-level
code to supporting a particular DBMS protocol, such as ODBC. The SQL-ODBC
library is, then, one such implementation library.

Our intention is that the SQL library will provide a common high-level Dylan
interface to any DBMS. Applications written using the SQL-ODBC library will
therefore be simple to port to any future DBMSes for which implementation
libraries are written.

Using the SQL-ODBC library in applications
------------------------------------------

The SQL-ODBC library is available to applications as the SQL-ODBC library,
which exports the modules SQL-ODBC and SQL. (You should not need to use the
SQL module, but it will be visible during debugging sessions.)


Object-oriented languages and relational databases
--------------------------------------------------

The SQL-ODBC library does not provide the means to "objectify" a relational
database or an SQL statement. That is, it does not treat a databases or
statements in an object-oriented fashion or provide support for doing so.

This is because the object-oriented programming model is very
different from the relational database model. The two most significant
differences follow.

First, the relational database model has only simple datatypes
(string, integer, floating point number, and so on) and does not
provide a means of defining new types, as object-oriented programming
languages do.

Second, objects in an object-oriented program have unique identities
that allow two objects of the same value to be distinguished from one
another. By contrast, SQL rows (the SQL notion nearest to the notion
of an object) do not have unique identities: if two rows in a given
table have identical values, they are indistinguishable.

Result-retrieval protocol
-------------------------

The SQL-ODBC library provides an abstract Dylan protocol for handling
SQL result sets, the means by which SQL SELECT statement results are
retrieved. The library allows result sets to be processed as Dylan
collections. The various Dylan collection protocols and functions work
as you would expect on a result set.

Processing results
------------------

SQL SELECT statements return database records. You process the results
of an SQL SELECT statement using a result set. Result sets are the
focal point of the SQL-ODBC library's encapsulation of the protocol
for retrieving database records. Using result sets allows you to
concentrate on the logic of your application instead of the logic of
record retrieval.

Result sets retrieve their records from the database synchronously. As
result sets retrieve their records, you can specify conversion of
records to application-specific objects which are added to the result
set in place of the record. Result sets retrieve their records one at
a time.

Bridging the object-relational gap
----------------------------------

Relational DBMSes do not in general deal with objects or classes.
Since Dylan is an object-oriented language, this creates a gap between
Dylan and the DBMS.

The SQL-ODBC library bridges this gap by allowing you to specify a
liaison function for results. A liaison function acts as an
interpreter for results, taking the records retrieved from the
relational DBMS and converting each into suitable Dylan objects. A
default liaison method exists for use in situations where your
application does not know the appropriate conversion, for example when
processing SQL SELECT statements typed in by the application user. The
default method transforms each record retrieved into a Dylan
collection, where each element of the collection corresponds to a
column of the record. See Section 1.5.4 on page 36 for more on liaison
functions.

Error handling
--------------

As in any application, errors at run time can occur when applications
talk to databases. The SQL-ODBC library captures the errors and
warnings that a DBMS generates and signals a corresponding Dylan error
or warning condition. Your application can then process the condition
using the Dylan condition system.

Examples used in this document
------------------------------

The following tables depict example database tables to which this
document's code examples refer.

.. table:: Table 1.1 Table "Book" used in this document's code examples.

    +-------------------------------------------------+-------------------+---------------+
    | Title                                           | Publisher         | ISBN          |
    +=================================================+===================+===============+
    | An Introduction to Database Systems             | Addison Wesley    | 0-201-14201-5 |
    +-------------------------------------------------+-------------------+---------------+
    | Transaction Processing: Concepts and Techniques | Morgan Kaufmann   | 1-55860-190-2 |
    +-------------------------------------------------+-------------------+---------------+
    | Fundamentals of Database Systems                | Benjamin/Cummings | 0-8053-1748-1 |
    +-------------------------------------------------+-------------------+---------------+
    | Relational Database Writings, 1991-1994         | Addison-Wesley    | 0-201-82459-0 |
    +-------------------------------------------------+-------------------+---------------+

.. table:: Table 1.2 Table "Author" used in this document's code examples.

    +-----------+-----------+------------+
    | Author ID | Last Name | First Name |
    +===========+===========+============+
    | 1         | Date      | Chris      |
    +-----------+-----------+------------+
    | 2         | Gray      | Jim        |
    +-----------+-----------+------------+
    | 3         | Reuter    | Andreas    |
    +-----------+-----------+------------+
    | 4         | Elmasri   | Ramez      |
    +-----------+-----------+------------+
    | 5         | Navathe   | Shamkant   |
    +-----------+-----------+------------+

.. table:: Table 1.3 Table "Book_author" used in this document's code examples.

    +-----------+---------------+
    | Author_ID | ISBN          |
    +===========+===============+
    | 1         | 0-201-14201-5 |
    +-----------+---------------+
    | 2         | 1-55860-190-2 |
    +-----------+---------------+
    | 3         | 1-55860-190-2 |
    +-----------+---------------+
    | 4         | 0-8053-1748-1 |
    +-----------+---------------+
    | 5         | 0-8053-1748-1 |
    +-----------+---------------+
    | 1         | 0-201-82459-0 |
    +-----------+---------------+

Connecting to a database
========================

Before it can query a database, your application must connect to it.
Most DBMSes operate a form of login procedure to verify connections,
using a user name and password for the purpose. The popular DBMSes
each have different protocols for identifying themselves, their users,
their databases, and connections to those databases.

The SQL-ODBC library provides a general-purpose connection protocol
that is not specific to any DBMS, and represents DBMSes, databases,
database connections, user names and passwords with generic Dylan
classes, thereby hiding the idiosyncrasies of the various DBMSes from
Dylan applications. The classes that the SQL-ODBC library defines are
shown in Table 1.4.

.. table:: Table 1.4 Dylan DBMS classes.

    +------------------------+-----------------------+-----------------------+
    | Entity                 | Abstract Dylan class  | SQL-ODBC class        |
    +========================+=======================+=======================+
    | DBMS                   | :class:`<dbms>`       | ``<odbc-dbms>``       |
    +------------------------+-----------------------+-----------------------+
    | Database               | :class:`<database>`   | ``<odbc-database>``   |
    +------------------------+-----------------------+-----------------------+
    | User name and password | :class:`<user>`       | ``<odbc-user>``       |
    +------------------------+-----------------------+-----------------------+
    | Active connection      | :class:`<connection>` | ``<odbc-connection>`` |
    +------------------------+-----------------------+-----------------------+

You should create DBMS-specific instances of these classes to connect
to a database.

See also :macro:`with-database`.

Connection protocol functions, methods, and macros
--------------------------------------------------

* :macro:`with-dbms`
* :gf:`dbms`
* :gf:`database`
* :gf:`user`

Connecting and disconnecting
----------------------------

The SQL-ODBC library provides DBMS-independent functions to connect to
and disconnect from databases. Connecting to a database establishes a
context (an instance of :class:`<connection>`) in which SQL statements
may be executed within an application. You can make connections by
calling the :gf:`connect` function on a DBMS-specific instance of
:class:`<database>` and :class:`<user>`.

An application can connect to multiple databases served by a DBMS if
the DBMS supports the feature. Multiple-connection support can be
determined by calling the :gf:`multiple-connections?` function
on the DBMS object.

Keeping connections open requires system resources. An application can
disconnect from connections that it no longer needs in order to reduce
its use of system resources. When the application terminates, the
SQL-ODBC library disconnects all open connections. If a connection is
not explicitly terminated using the :gf:`disconnect` generic function,
and a client application has no references to it, the connection is
terminated when the garbage collector notices that the object can be
reclaimed. After a connection has been disconnected, the
:class:`<connection>` object cannot be reused, and so references to it
should be dropped.

* :gf:`connect`
* :gf:`connections`
* :gf:`default-connection`
* :gf:`disconnect`
* :gf:`disconnect-all`
* :macro:`with-connection`

Executing SQL statements
========================

The SQL-ODBC library provides a way of processing SQL statements: the
:gf:`execute` function, which you must apply to instances of the
:class:`<sql-statement>` class.

* :class:`<database-statement>`
* :gf:`execute`
* :class:`<sql-statement>`
* :gf:`coercion-policy`
* :gf:`coercion-policy-setter``
* :gf:`datatype-hints`
* :gf:`datatype-hints-setter`
* :gf:`execute`
* :gf:`input-indicator`
* :gf:`input-indicator-setter`
* :gf:`output-indicator`
* :gf:`output-indicator-setter`
* :gf:`text`
* :gf:`text-setter`

The null value
--------------

SQL offers the null value to represent missing information, or
information that is not applicable in a particular context. All
columns of a table can accept the null value -- unless prohibited by
integrity constraints -- regardless of the domain of the column.
Hence, the null value is included in all domains of a relational
database and can be viewed as an out-of-band value.

Relational database theory adopted a three-valued logic system --
"true", "false", and "null" (or "unknown") -- in order to process
expressions involving the null value. This system has interesting (and
sometimes frustrating) consequences when evaluating arithmetic and
comparison expressions. If an operand of an arithmetic expression is
the null value, the expression evaluates to the null value. If a
comparand of a comparison expression is the null value, the expression
may evaluate to the null/unknown truth-value.

For example:

* ``a + b``, where a contains the null value or b contains the null
  value, evaluates to the null value
* ``a + b``, where a contains the null value and b contains the null
  value, evaluates to the null value
* ``a = b``, where a contains the null value or b contains the null
  value, evaluates to unknown
* ``a = b``, where a contains the null value and b contains the null
  value, evaluates to unknown
* ``a | b``, where a is true and b contains the null value, evaluates
  to true
* ``a & b``, where a is false and b contains the null value, evaluates
  to false

The SQL ``SELECT`` statements return records for which the ``WHERE``
clause (or ``WHERE`` predicate) evaluates to true (not to false and
not to the null value). In order to test for the presence or absence
of the null value, SQL provides a special predicate of the form::

    column-name is [not] null

The null value is effectively a universal value that is difficult to
use efficiently in Dylan. To identify when null values are returned
from or need to be sent to a DBMS server, the SQL-ODBC library
supports indicator objects. Indicator objects indicate when a column
of a record retrieved from a database contains the null value, or when
a client application wishes to set a column to the null value.

* :class:`<null-value>`
* :const:`$null-value`

Input indicators and output indicators
--------------------------------------

It is difficult for database applications written in traditional
programming languages to represent the semantics of the null value,
because it is a universal value which is in the domain of all types,
and the three-valued logic system which accompanies null values does
not easily translate to the two-value logic system in traditional
programming languages.

In Dylan, a universal value can be achieved if we ignore type
specialization, but this inhibits optimization and method dispatching.
Even if we were to forgo type specialization, the evaluation of
arithmetic and comparison expressions is a problem since Dylan's logic
system is boolean and not three-valued. Therefore, the SQL-ODBC
library has a goal of identifying null values and translating them
into Dylan values that can be recognized as representing null values.

In order to identify null values during SQL statement processing, the
:class:`<sql-statement>` class supports an input indicator and output
indicator. An input indicator is a marker value or values which
identifies an input host variable as containing the null value. An
output indicator is a substitution value which semantically identifies
columns of a retrieved record as containing the null value.

If the SQL-ODBC library encounters a null value when retrieving
records from a database, and there is no appropriate indicator object,
it signals a :class:`<data-exception>` condition. The condition is
signaled from result-set functions (including the collection protocol)
and not the execute function.

During the execution of an SQL statement to which an input indicator
value was supplied, each input host variable is compared (with the
function ``\==``) to the input indicator and, if it holds the input
indicator value, the null value is substituted for it.

The input indicator may be a single value or a sequence of values. A
single value is useful when it is in the domain of all input host
variables; if the host variables have not been specialized, any newly
created value will do. Otherwise, a sequence of values must be used.
Input indicators that are general instances of :drm:`<sequence>` use
their positional occurrence within the SQL statement as the key for
the sequence.

The SQL SELECT statement is the only SQL statement that returns non-
status results back to the client application. During the retrieval of
these results, the SQL-ODBC library substitutes the output indicator,
if supplied, for null values found in the columns of each record.

The output indicator may be a single value or a sequence of values. If
the output indicator is a general instance of :drm:`<sequence>`, the
element of the sequence whose key corresponds to the column index is
used as the substitution value. Otherwise, the output indicator value
itself is used as the substitution value.

Data retrieval using result-set collection
==========================================

Executing an SQL SELECT statement by invoking the execute function on
the instance of :class:`<sql-statement>` that represents the statement
yields a result set.

A result set is a Dylan collection which encapsulates the protocol
necessary to retrieve data from a database. The SQL-ODBC library
defines two subclasses of :class:`<result-set>` that provide different
behaviors and performance characteristics. The type of the result set
returned by the execute function is determined by the result-set
policy supplied to the function or macro.

There are two subclasses of :class:`<result-set>`:
:class:`<forward-only-result-set>` and :class:`<scrollable-result-set>`.

The :class:`<forward-only-result-set>` class provides an efficient
means of accessing the elements of a result set. Efficiency is
achieved by performing minimal processing on each record retrieved and
by maintaining in memory only the current record. Implicit in this
behavior is that records you have accessed previously are no longer
available to your application; if you maintain references to previous
records behavior is unpredictable. The key for each access must always
be greater than or equal to the previous access's key; otherwise, a
condition is signaled.

The :class:`<scrollable-result-set>` class allows your application to
access elements of the result-set collection in any order, meaning
that records you have accessed previously can be revisited. Scrollable
result sets retrieve records synchronously.

Example:

This example returns a list of authors who have published two or more books.

.. code-block:: dylan

    (result-set-policy: make(<scrollable-result-set-policy>))
        select last_name, first_name, count(*)
        from author, book_author
        where book_author.author_id = author.author_id
        group by last_name, first_name
        having count(*) > 2
      end;
    => #(#("Date", "Chris", 2))

    let query = make(<sql-statement>,
                    text: "select last_name, first_name, count(*)"
                          "from author, book_author"
                          "where book_author.author_id"
                             "= author.author_id"
                          "group by last_name, first_name having"
                              "count(*) >= 2");
    execute(query, result-set-policy: $scrollable-result-set-policy);

Result-set collections
======================

A result-set collection, in spirit, contains the result of an SQL
``SELECT`` statement. To provide these results, result-set collections
and their methods control the retrieval of elements from the database.
Each element of a result set is a record and each element of a record
is a value. The SQL-ODBC library does not provide any classes to
represent columns; the elements of a record are just Dylan objects.

Result-set classes, in conjunction with the methods defined on them,
provide a protocol to retrieve data from a database. Result-sets do
not necessarily contain the records from the database. A result set
could cache a small subset of the records retrieved for performance
reasons. The logic for retrieving a record from a result set (from the
database) is as follows:

1. Perform an internal fetch: values are stored into bindings
   established during SQL statement preparation. A record object is
   created during the preparation of the SQL statement which represents
   the values of the record (collection of values).

2. Invoke the liaison method on the record object. The result of the
   liaison function is the result of the collection access.

The columns of a record are processed when the columns are retrieved
from the record object. This includes checking for null values and
performing data coercion if a :gf:`coercion-policy` is supplied.

Record class
------------

An instance of the :class:`<record>` class is a placeholder for
records retrieved from the database. The record class is a collection
whose elements are the columns of the records retrieved from the
database. If the record object has a coercion policy (obtained through
the :gf:`result-set-policy`), datatype coercion is performed on the
elements of the record object as they are retrieved from the
collection.

The elements of a record collection are ephemeral under the result-set
retrieval protocol: the values for the elements of the collection can
change when the next record of the result set is accessed. A result
set may maintain more than one record object to improve performance.

Record collections support the forward- and backward-iteration
protocols. The result of calling :drm:`type-for-copy` on the
:class:`<record>` class is :class:`<simple-object-vector>`.

Applications cannot instantiate the :class:`<record>` class. However,
the functions returned by the forward- and backward-iteration protocol
methods on the result-set classes return instances of this class.

The values in a record object have a short lifespan: they are only
valid until the next fetch is performed.

Result-set policy class
-----------------------

Applications use result-set policy classes to specify the behavior and
performance characteristics of a result set, as well as its type. The
type of the result set is determined by the result-set policy object.
The type of the record object is determined by the :gf:`coercion-policy`
slot of :class:`<sql-statement>`.

If ``result-set-policy.scrollable?`` is ``#t``, the result set will be an
instance of :class:`<scrollable-result-set>` otherwise it will be an instance
of :class:`<forward-only-result-set>`. If ``statement.coercion-policy ~=
$no-coercion`` then the record will be an instance of :class:`<coercion-record>`;
otherwise, it will be an instance of :class:`<record>`.

.. table:: Table 1.5 Result set policies and classes.

    +-------------+-----------------+------------------------------------+
    | Scrollable? | Coercion policy | Result set class                   |
    +=============+=================+====================================+
    | ``#f``      | ``#f``          | :class:`<forward-only-result-set>` |
    +-------------+-----------------+------------------------------------+
    | ``#t``      | -               | :class:`<scrollable-result-set>`   |
    +-------------+-----------------+------------------------------------+

Result-set classes
------------------

Result-sets are the focal point for the encapsulation of the protocol
required to retrieve records from a database. The SQL-ODBC library
provides three result-set classes with different performance and
behavioral characteristics. These classes are :class:`<result-set>`,
:class:`<forward-only-result-set>`, and :class:`<scrollable-result-set>`.

Liaison functions
-----------------

Liaison functions convert records retrieved from a database query to
Dylan objects. These functions bridge the conceptual gap between
relational databases and Dylan's object-orientation.

To create a Dylan object from a retrieved record, the liaison function
must understand the form of the records coming from the database and
the mappings of records to Dylan objects. These Dylan objects make up
the elements of the result set: the results of the liaison function
are added to the result set each time it is called. As your
application iterates over a result set, the liaison function provides
the objects that the application processes.

If you do not provide a liaison function for a result set, the SQL-
ODBC library supplies a ``default-liaison`` function to perform the
conversion. If a coercion policy is provided, the ``default-liaison``
function is :drm:`copy-sequence`. The new sequence is safe in that it is a
normal Dylan collection with no relationship to databases, SQL
statements, or result sets. If a coercion policy is not provided, the
``default-liaison`` is the identity function.

You can specify the identity function as the liaison function to
process the actual record objects. If no type coercion is performed by
the functions on the record class, this function will have the lowest
overhead, but there are some restrictions: the values retrieved from
the record may become invalid when the state of the iteration protocol
changes.

The liaison function can, potentially, cause the greatest number of
problems for an application using SQL-ODBC since there is no type
safety between the liaison function, the record class and the SQL
``SELECT`` statement. You must ensure that the liaison function is in sync
with the SQL ``SELECT`` statement since there is no support in SQL-ODBC
for this.

Example:

.. code-block:: dylan

    define class <book> (<object>)
      slot title :: <string>, init-keyword: title:;
      slot publisher :: <string>, init-keyword: publisher:;
      slot isbn :: <string>, init-keyword: isbn:;
      slot author :: <string>, init-keyword: author:;
    end class;

    begin
      let booker =
        method (record :: <record>) => (book :: <book>)
          let (title, publisher, isbn, last_name, first_name) =
            apply(values, record);

          make(<book>, title: title, publisher: publisher,
               isbn: isbn, author: concatenate(last_name, ", ",
               first_name));
        end method;
    let query = make(<sql-statement>,
                     statement: "select title, publisher, isbn,
                                   last_name, first_name
                                 from book, author, book_author
                                 where book.isbn = book_author.isbn
                                   and book_author.author_id =
                                         author.author_id
                                 order by author.last_name,
                                          author.first_name");
    execute(query, liaison: booker
            result-set-policy:
              make(<forward-only-result-set-policy>));
    end;

Coercion policies
-----------------

In the SQL-ODBC library, the element method on the record class
encapsulates all coercions of data retrieved from a database. This
method can return columns with or without coercion: as low-level SQL
data-types (no conversion), as Dylan data-types, or as user-defined
types. The ``coercion-policy:`` init-keyword of the
:class:`<sql-statement>` class determines this behavior.

If the ``coercion-policy:`` init-keyword is :const:`$no-coercion`,
coercions are not performed. Hence, your application will be
processing objects with low-level SQL datatypes. This option has the
lowest overhead but the most restrictions: the values returned from
the element method may not be valid (values may change as storage may
be reused) after the next call to the ``next-state`` method returned
by ``forward-iteration-protocol``.

The value of :const:`$default-coercion` for the ``coercion-policy:``
init-keyword (the default value) indicates that default coercion
should be performed: the data retrieved from the database is coerced
to the corresponding Dylan objects.

A sequence for the ``coercion-policy:`` init-keyword instructs the SQL
library to perform specific data coercion on the data retrieved from
the database. Essentially, each element of the limited sequence is a
data coercion function which will be invoked using the objects
returned from the database as the argument.

When there is a one-to-one correspondence between an SQL datatype and
a built-in or user-defined Dylan datatype, use the :class:`<record>`
class to perform the conversion. When multiple columns define a Dylan
object or one column defines multiple Dylan objects, use the liaison
function to perform the conversion.

Data types and conversions
==========================

The datatypes that relational DBMSes use are different from those
Dylan uses. The SQL-ODBC library provides classes that represent
these low-level relational datatypes, along with a table that
defines the mapping from these datatypes to Dylan datatypes
(Table 1.6). The methods on the record class consult this mapping
when performing data coercion.

The datatypes of host variables are limited to the Dylan datatypes
that appear in Table 1.6. Host variables come in two flavors: read
and write. Host variables appearing in an into clause of an SQL
``SELECT`` statement are write parameters, and all other host
variables are read parameters.

.. table:: Table 1.6 Mapping from DBMS to Dylan datatypes

   +-------------------+------------------------------+--------------------------+
   | DBMS type         | SQL type                     | Dylan type               |
   +===================+==============================+==========================+
   | sql_char          | :class:`<sql-char>`          | :drm:`<character>`       |
   +-------------------+------------------------------+--------------------------+
   | sql_varchar       | :class:`<sql-varchar>`       | :drm:`<string>`          |
   +-------------------+------------------------------+--------------------------+
   | sql_longvarchar   | :class:`<sql-longvarchar>`   | :drm:`<string>`          |
   +-------------------+------------------------------+--------------------------+
   | sql_decimal       | :class:`<sql-decimal>`       | :drm:`<string>`          |
   +-------------------+------------------------------+--------------------------+
   | sql_numeric       | :class:`<sql-numeric>`       | :drm:`<string>`          |
   +-------------------+------------------------------+--------------------------+
   | sql_bit           | :class:`<sql-bit>`           | :drm:`<integer>`         |
   +-------------------+------------------------------+--------------------------+
   | sql_tinyint       | :class:`<sql-tinyint>`       | :drm:`<integer>`         |
   +-------------------+------------------------------+--------------------------+
   | sql_smallint      | :class:`<sql-smallint>`      | :drm:`<integer>`         |
   +-------------------+------------------------------+--------------------------+
   | sql_integer       | :class:`<sql-integer>`       | :drm:`<integer>`         |
   +-------------------+------------------------------+--------------------------+
   | sql_bigint        | :class:`<sql-bigint>`        | :drm:`<integer>`         |
   +-------------------+------------------------------+--------------------------+
   | sql_real          | :class:`<sql-real>`          | :drm:`<single-float>`    |
   +-------------------+------------------------------+--------------------------+
   | sql_float         | :class:`<sql-float>`         | :drm:`<single-float>`,   |
   |                   |                              | :drm:`<double-float>` or |
   |                   |                              | :drm:`<extended-float>`  |
   +-------------------+------------------------------+--------------------------+
   | sql_double        | :class:`<sql-double>`        | :drm:`<double-float>`    |
   +-------------------+------------------------------+--------------------------+
   | sql_binary        | :class:`<sql-binary>`        | :class:`<binary>`        |
   +-------------------+------------------------------+--------------------------+
   | sql_varbinary     | :class:`<sql-varbinary>`     | :class:`<binary>`        |
   +-------------------+------------------------------+--------------------------+
   | sql_longvarbinary | :class:`<sql-longvarbinary>` | :class:`<binary>`        |
   +-------------------+------------------------------+--------------------------+
   | sql_date          | :class:`<sql-date>`          | :class:`<date>`          |
   +-------------------+------------------------------+--------------------------+
   | sql_time          | :class:`<sql-time>`          | :class:`<time>`          |
   +-------------------+------------------------------+--------------------------+
   | sql_timestamp     | :class:`<sql-timestamp>`     | :class:`<timestamp>`     |
   +-------------------+------------------------------+--------------------------+

To retrieve integer elements from databases that may contain more than 30-bit data,
you must use the :lib:`generic-arithmetic` library or a run-time error will occur.
The Dylan SQL-ODBC library must also be prepared.

Example library and module definition:

.. code-block:: dylan

    define library sql-example
      use common-dylan;
      use generic-arithmetic;
      use sql-odbc;

      export sql-example;
    end library;

    define module sql-example
      use generic-arithmetic-common-dylan;
      use sql-odbc;
    end module;

Warning and error conditions
============================

The SQL-ODBC library defines condition classes for each category of
error and warning defined in SQL-92. (SQL-92 calls them classes
rather than categories.)

When an error or warning occurs, SQL-ODBC detects it, creates a
condition object, and signals it. You can then handle the condition
using the Dylan condition system.

Some DBMSes can detect and report multiple errors or warnings during
the execution of a single SQL statement. The DBMS reports these
errors and warnings to the SQL-ODBC library using SQL-92's concept of
diagnostics; the first error or warning in the diagnostic area is the
same error or warning indicated by the SQLSTATE status parameter.
The SQL-ODBC library signals a condition which corresponds to the
error or warning indicated by SQLSTATE.

While handling the first condition, your application can process
any additional errors or warnings that may have occurred by signaling
the next DBMS condition; to obtain the next DBMS condition, call
:func:`next-dbms-condition` on the condition being handled.

Database introspection
======================

The SQL-ODBC library offers introspection features to allow you to
determine the structure of a database at run time. A database
structure is a hierarchy comprising catalogs, schemas, tables and
columns. A catalog is a named collection of schemas, a schema is a
named collection of tables, and a table is a named collection of
columns. For security reasons, the SQL-ODBC library does not provide
any means of obtaining a list of databases available from a particular
DBMS; your application must provide access to a particular database
via a connection object.

For DBMSes which do not support catalogs or schemas, the SQL-ODBC
library uses a default value that your application can use to perform
introspection.

Database objects and integrity constraints
------------------------------------------

You can interrogate schema and table database objects for a collection
of constraints defined against them. A constraint is a data integrity
rule which the DBMS enforces at all times. These constraints are
unique, primary key, referential and check.

The unique constraint specifies that one or more columns within a
table must have a unique value or set of values for each record in the
table (however, the set of columns are not necessarily a key). The
primary key constraint is similar to the unique constraint, except the
set of columns must uniquely identify records within the table.

The referential constraint specifies the relationship between a column
or a group of columns in one table to another table; this constraint
also specifies the action to take when records within the table are
updated or deleted.

Finally, the check constraint is a general constraint on a table which
must never be false and, due to three-valued logic, an unknown or null
value will satisfy the constraint.

An assertion is a constraint on a schema. It is similar to the check
constraint but it normally involves more than one table. The
significant difference between an assertion and a check is that an
assertion must always be true, whereas a check must never be false.

The nullability of a column is a column constraint which can be
determined by introspection on the desired column.

Syntactically, SQL-92 supports table and column constraints;
semantically, however, all constraints are enforced at the table
level.

The SQL module
==============

.. constant:: $default-coercion

.. constant:: $default-result-set-policy

.. constant:: $diagnostic-table

.. constant:: $no-coercion

.. constant:: $no-indicator

.. constant:: $null-value

   :description:

     References the canonical null value. It is an instance of
     :class:`<null-value>`.

.. constant:: $read-committed

.. constant:: $read-only

.. constant:: $read-uncommitted

.. constant:: $read-write

.. constant:: $repeatable-read

.. constant:: $scrollable-result-set-policy

.. constant:: $serializable

.. constant:: *all-connections*

.. constant:: *all-connections-lock*

.. class:: <ambiguous-cursor-name>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <assertion-constraint>
   :abstract:

   :superclasses: :class:`<constraint>`


.. class:: <cardinality-violation>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <catalog-not-found>

   :superclasses: :class:`<database-object-not-found>`

   :keyword catalog-name:

.. class:: <catalog>
   :open:
   :abstract:

   :superclasses: :class:`<database-object>`, :class:`<result-set>`

   :keyword connection:

.. class:: <character-not-in-repertoire>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <check-constraint>
   :abstract:

   :superclasses: :class:`<constraint>`


.. constant:: <coercion-policy>

   Determines what data coercion is to be performed on a result set.

.. class:: <coercion-record>
   :open:
   :abstract:

   :superclasses: :class:`<record>`

   :keyword record-coercion-policy:

.. class:: <column>
   :open:
   :abstract:

   :superclasses: :class:`<database-object>`

   :keyword default-value:
   :keyword domain:
   :keyword nullable?:

.. class:: <connection-does-not-exist>
   :open:

   :superclasses: :class:`<connection-exception>`

   :keyword subclass-code:

.. class:: <connection-exception>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <connection-failure>
   :open:

   :superclasses: :class:`<connection-exception>`

   :keyword subclass-code:

.. class:: <connection-name-in-use>
   :open:

   :superclasses: :class:`<connection-exception>`

   :keyword subclass-code:

.. class:: <connection-not-specified>
   :open:

   :superclasses: ``<error>``


.. class:: <connection>
   :open:
   :abstract:

   :superclasses: ``<object>``

   :keyword dbms:

   :description:

     The ``<connection>`` class represents a database connection. More
     formally, we can say that it identifies a context in which a
     client application can execute SQL statements. The exact
     composition of a connection depends on the DBMS and the client
     platform. Implementation libraries like SQL-ODBC define a
     subclass of ``<connection>`` that implements the necessary
     requirements to identify the execution context to the client
     application.

.. class:: <constraint>
   :abstract:

   :superclasses: :class:`<database-object>`


.. class:: <cursor-operation-conflict>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <cursor-specification-cannot-be-executed>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <data-exception>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <data-not-available>
   :open:

   :superclasses: ``<error>``


.. class:: <database-collection>
   :open:
   :abstract:

   :superclasses: ``<sequence>``


.. class:: <database-error>
   :open:
   :abstract:

   :superclasses: ``<error>``


.. class:: <database-object-not-found>
   :abstract:

   :superclasses: :class:`<diagnostic>`


.. class:: <database-object>
   :abstract:

   :superclasses: ``<object>``

   :keyword name:

.. class:: <database-statement>
   :open:
   :abstract:

   :superclasses: ``<object>``

   :description:

     This class represents statements which can be executed by a DBMS
     server.

.. class:: <database>
   :open:
   :abstract:

   :superclasses: ``<object>``

   :description:

     The ``<database>`` class identifies a database to a DBMS. Exactly what a
     database is depends on the DBMS in use. Implementation libraries
     like SQL-ODBC supply an instantiable subclass of ``<database>`` to provide
     whatever implementation is necessary for identifying a database to a
     specific DBMS.

.. class:: <datetime-field-overflow>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <dbms-not-specified>
   :open:

   :superclasses: ``<error>``


.. class:: <dbms>
   :open:
   :abstract:

   :superclasses: ``<object>``

   :description:

     The ``<dbms>`` class identifies a database management system (DBMS) to a
     client application. Implementation libraries like SQL-ODBC supply an
     instantiable subclass of ``<dbms>`` to provide whatever implementation is
     necessary for identifying a DBMS to an application.

.. class:: <dependent-privilege-descriptors-still-exist>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <diagnostic-table>

   :superclasses: ``<object>``

   :keyword general-key:

.. class:: <diagnostic>
   :open:
   :abstract:

   :superclasses: ``<condition>``

   :keyword class-code:
   :keyword condition-number:
   :keyword subclass-code:

.. class:: <disconnect-error>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <division-by-zero>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <dynamic-sql-error>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <empty-result-set>
   :open:

   :superclasses: :class:`<result-set>`

   :keyword liaison:

.. class:: <error-in-assignment>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <feature-not-supported>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <forward-only-result-set>
   :open:
   :abstract:

   :superclasses: :class:`<result-set>`


.. class:: <implicit-zero-bit-padding>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <index>
   :open:
   :abstract:

   :superclasses: :class:`<database-object>`

   :keyword indexed-table:
   :keyword unique-index?:

.. class:: <indicator-overflow>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. constant:: <indicator-policy>

.. class:: <insufficient-item-descriptor-areas>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <integrity-constraint-violation>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <interval-field-overflow>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <invalid-argument>
   :open:

   :superclasses: ``<error>``


.. class:: <invalid-authorization-specification>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-catalog-name>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-character-set-name>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-character-value-for-cast>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <invalid-condition-number>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-cursor-name>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-datatype-hint>
   :open:

   :superclasses: ``<warning>``

   :keyword datatype-hint:

.. class:: <invalid-datetime-format>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <invalid-descriptor-count>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <invalid-descriptor-index>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <invalid-escape-character>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <invalid-escape-sequence>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <invalid-fetch-sequence>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <invalid-parameter-value>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <invalid-schema-name>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-sql-descriptor-name>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-sql-statement-name>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-time-zone-displacement-value>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <invalid-transaction-state>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-transaction-termination>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. constant:: <isolation-level>

.. class:: <multiple-server-transaction>
   :open:

   :superclasses: :class:`<feature-not-supported>`

   :keyword subclass-code:

.. class:: <no-data>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <null-value-eliminated-in-set-function>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <null-value-no-indicator-parameter>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <null-value>
   :open:

   :superclasses: ``<object>``

   :description:

     Instances of this class represent the canonical null value. This
     class is the root class for all null-value classes.

.. class:: <numeric-value-out-of-range>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <prepared-statement-not-a-cursor-specification>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <privilege-not-granted>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <privilege-not-revoked>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <query-expression-too-long-for-information-schema>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <record>
   :open:
   :abstract:

   :superclasses: :class:`<database-collection>`

   :keyword indicator-policy:

.. class:: <referential-constraint>
   :abstract:

   :superclasses: :class:`<constraint>`


.. class:: <remote-database-access>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <restricted-data-type-attribute-violation>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <result-set-mutation-error>
   :open:

   :superclasses: ``<error>``


.. class:: <result-set-policy>
   :open:

   :superclasses: ``<object>``

   :keyword asynchronous:
   :keyword rowset-size:
   :keyword scroll-window:
   :keyword scrollable:

.. class:: <result-set>
   :open:
   :abstract:

   :superclasses: :class:`<database-collection>`

   :keyword liaison:

.. class:: <schema-not-found>

   :superclasses: :class:`<database-object-not-found>`

   :keyword schema-name:

.. class:: <schema>
   :open:
   :abstract:

   :superclasses: :class:`<database-object>`, :class:`<result-set>`


.. class:: <scrollable-result-set>
   :open:
   :abstract:

   :superclasses: :class:`<result-set>`


.. class:: <search-condition-too-long-for-information-schema>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <sql-bigint>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-binary>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-bit-varying>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-bit>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-character-varying>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-character>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-client-unable-to-establish-connection>
   :open:

   :superclasses: :class:`<connection-exception>`

   :keyword subclass-code:

.. class:: <sql-datatype>
   :open:
   :abstract:

   :superclasses: ``<object>``


.. class:: <sql-date>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-day-time-interval>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-decimal>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-double-precision>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-double>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-error>
   :open:
   :abstract:

   :superclasses: :class:`<database-error>`


.. class:: <sql-float>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-integer>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-longvarbinary>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-longvarchar>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-national-character-varying>
   :open:

   :superclasses: :class:`<sql-character-varying>`


.. class:: <sql-national-character>
   :open:

   :superclasses: :class:`<sql-character>`


.. class:: <sql-numeric>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-real>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-server-rejected-establishment-of-connection>
   :open:

   :superclasses: :class:`<connection-exception>`

   :keyword subclass-code:

.. class:: <sql-smallint>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-statement>
   :open:
   :abstract:

   :superclasses: :class:`<database-statement>`

   :keyword coercion-policy: An instance of
      ``false-or(<coercion-policy>)``. The coercion policy
      is a sequence of functions, or the value :const:`$default-coercion`,
      or the value :const:`$no-coercion`, used to perform data coercion
      when the SQL statement to be executed is a ``SELECT`` statement.
   :keyword datatype-hints: An instance of ``false-or(<sequence>)``. This
      is a hint for parameter binding when the SQL statement to be executed
      is a ``SELECT`` statement.
   :keyword input-indicator: An instance of :drm:`<object>`. The
      input indicator is a marker value used to identify null
      values in host variables.
   :keyword output-indicator: An instance of :drm:`<object>`. The
      output indicator is a substitution value to be used whenever
      the column of a retrieved record contains the null value.
   :keyword text: An instance of :drm:`<string>`. Required. Contains the
      text of the SQL statement. If you want to include host variables,
      place a question mark (``?``) at the point in the string at which you
      want a host variable to be substituted.


   :description:

     The ``<sql-statement>`` class represents SQL statements and their
     indicator values and coercion policy. You can use this class to
     represent any SQL statement, be it static or dynamic. You can
     send SQL statements to the DBMS for execution by calling the
     :gf:`execute` function on an instance of ``<sql-statement>``. The
     :gf:`execute` function returns the results of executing the SQL
     statement, if there are any.

     In the :gf:`make` method on ``<sql-statement>``, you can specify that
     values should be substituted into the SQL statement when it is
     executed. You do not specify the values until calling :gf:`execute`
     on the statement, when you can pass the substitution values
     with the ``parameter:`` keyword.

     The values are substituted wherever a question mark (``?``) occurs in
     the SQL statement string. We call the question marks anonymous host
     variables because there is no Dylan variable name. Substitution occurs
     positionally: the first value replaces the first anonymous host variable,
     the second value replaces the second anonymous host variable, and so on.
     If the number of values is greater than the number of anonymous host
     variables, the extra parameters are ignored. If the number of anonymous
     host variables is greater than the number of parameters, a condition
     is signaled.

     When the SQL statement is ``SELECT``, you can also specify a result-set
     policy and a liaison function in the call to :gf:`execute`. A result-set
     policy describes behavioral and performance characteristics of the
     result-set object that the execute function returns. A liaison function
     creates Dylan objects from the records retrieved from the database. These
     objects become the elements of the result set instead of the record object.

.. class:: <sql-table>
   :open:
   :abstract:

   :superclasses: :class:`<database-object>`, :class:`<result-set>`


.. class:: <sql-time-with-time-zone>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-time>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-timestamp-with-time-zone>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-timestamp>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-tinyint>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-type-timestamp>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-unknown-type>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-unsupported-type>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-varbinary>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-warning>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <sql-year-month-interval>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <statement-completion-unknown>
   :open:

   :superclasses: :class:`<transaction-rollback>`

   :keyword subclass-code:

.. class:: <string-data-length-mismatch>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <string-data-right-truncation>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <substring-error>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <successful-completion>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <syntax-error-or-access-rule-violation-in-direct-sql-statement>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <syntax-error-or-access-rule-violation-in-dynamic-sql-statement>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <syntax-error-or-access-rule-violation>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <table-not-found>

   :superclasses: :class:`<database-object-not-found>`

   :keyword table-name:

.. constant:: <transaction-mode>

.. class:: <transaction-resolution-unknown>
   :open:

   :superclasses: :class:`<connection-exception>`

   :keyword subclass-code:

.. class:: <transaction-rollback-due-to-integrity-constraint-violation>
   :open:

   :superclasses: :class:`<transaction-rollback>`

   :keyword subclass-code:

.. class:: <transaction-rollback-due-to-serialization-failure>
   :open:

   :superclasses: :class:`<transaction-rollback>`

   :keyword subclass-code:

.. class:: <transaction-rollback>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <transaction>
   :open:

   :superclasses: ``<object>``

   :keyword diagnostics-size:
   :keyword isolation-level:
   :keyword transaction-mode:

.. class:: <triggered-data-change-violation>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <trim-error>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <unhandled-diagnostic>
   :open:

   :superclasses: :class:`<sql-error>`

   :keyword diagnostic:

.. class:: <unique-constraint>
   :abstract:

   :superclasses: :class:`<constraint>`


.. class:: <unknown-sqlstate>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:
   :keyword sqlstate:
   :keyword subclass-code:

.. class:: <unterminated-c-string>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <user>
   :open:
   :abstract:

   :superclasses: ``<object>``

   :description:

     The ``<user>`` class identifies a user to a DBMS. Exactly what a "user"
     means depends on the DBMS. Implementation libraries like SQL-ODBC
     supply an instantiable subclass of ``<user>`` to provide whatever
     implementation is necessary for identifying a user to a specific DBMS.

     When connecting to a DBMS that did not have any users per se,
     instances of ``<user>`` would merely satisfy the API protocol,
     and would not identify a specific user -- any instance of ``<user>``
     would identify all users to the DBMS. However, most DBMSes do
     require a user name and password to identify a specific user.
     Indeed, some DBMSes require stringent authorization information
     in order to identify a user, such as multiple passwords.

.. class:: <using-clause-does-not-match-dynamic-parameter-specification>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <using-clause-does-not-match-target-specification>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <using-clause-required-for-dynamic-parameters>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <using-clause-required-for-result-fields>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <warning-cursor-operation-conflict>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <warning-string-data-right-truncation>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <with-check-option-violation>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. generic-function:: acquire-null-value

   :signature: acquire-null-value (indicator index) => (null-value)

   :parameter indicator: An instance of ``<object>``.
   :parameter index: An instance of ``<integer>``.
   :value null-value: An instance of ``<object>``.

.. generic-function:: asynchronous

   :signature: asynchronous (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: catalog-from-name
   :open:

   :signature: catalog-from-name (connection name) => (catalog)

   :parameter connection: An instance of :class:`<connection>`.
   :parameter name: An instance of ``<string>``.
   :value catalog: An instance of ``<catalog>``.

.. generic-function:: catalog-name
   :open:

   :signature: catalog-name (diag) => (catalog-name)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value catalog-name: An instance of ``<string>``.

.. generic-function:: catalogs
   :open:

   :signature: catalogs (#key connection) => (result-set)

   :parameter #key connection: An instance of :class:`<connection>`.
   :value result-set: An instance of :class:`<result-set>`.

.. generic-function:: catalogs-assist
   :open:

   :signature: catalogs-assist (connection) => (result-set)

   :parameter connection: An instance of :class:`<connection>`.
   :value result-set: An instance of :class:`<result-set>`.

.. generic-function:: class-code

   :signature: class-code (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: class-origin
   :open:

   :signature: class-origin (diag) => (class-origin)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value class-origin: An instance of ``<string>``.

.. generic-function:: coercion-policy

   :signature: coercion-policy (sql-statement) => (coercion-policy)

   :parameter sql-statement: An instance of :class:`<sql-statement>`.
   :value coercion-policy: An instance of :class:`<coercion-policy>`.

   :description:

      Returns the coercion policy for sql-statement. This
      method is only relevant to SQL ``SELECT`` statements.

.. generic-function:: coercion-policy-setter

   :signature: coercion-policy-setter (new-coercion-policy sql-statement) => (new-coercion-policy)

   :parameter new-coercion-policy: An instance of :class:`<coercion-policy>`.
   :parameter sql-statement: An instance of :class:`<sql-statement>`.
   :value new-coercion-policy: An instance of :class:`<coercion-policy>`.

   :description:

     Sets the ``coercion-policy`` slot of ``sql-statement``
     to ``new-coercion-policy``.

.. generic-function:: column-name
   :open:

   :signature: column-name (diag) => (column-name)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value column-name: An instance of ``<string>``.

.. generic-function:: command-function
   :open:

   :signature: command-function (diag) => (command-function)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value command-function: An instance of ``<string>``.

.. generic-function:: commit-transaction
   :open:

   :signature: commit-transaction (transaction) => ()

   :parameter transaction: An instance of :class:`<transaction>`.

.. generic-function:: condition-number

   :signature: condition-number (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: conditions-not-recorded?
   :open:

   :signature: conditions-not-recorded? (diag) => (not-recorded-status)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value not-recorded-status: An instance of ``<boolean>``.

.. generic-function:: connect
   :open:

   :signature: connect (database user) => (connection)

   :parameter database: An instance of :class:`<database>`.
   :parameter user: An instance of :class:`<user>`.
   :value connection: An instance of :class:`<connection>`.

.. generic-function:: connect-with-prompt
   :open:

   :signature: connect-with-prompt (dbms #key database user) => (connection)

   :parameter dbms: An instance of :class:`<dbms>`.
   :parameter #key database: An instance of ``false-or(<database>)``.
   :parameter #key user: An instance of ``false-or(<user>)``.
   :value connection: An instance of :class:`<connection>`.

.. generic-function:: connect-with-prompt?
   :open:

   :signature: connect-with-prompt? (dbms) => (connect-with-prompt-status)

   :parameter dbms: An instance of :class:`<dbms>`.
   :value connect-with-prompt-status: An instance of ``<boolean>``.

.. generic-function:: connection
   :open:

   :signature: connection (o) => (result)

   :parameter o: An instance of ``<object>``.
   :value result: An instance of :class:`<connection>`.

.. generic-function:: connection-name
   :open:

   :signature: connection-name (diag) => (connection-name)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value connection-name: An instance of ``<string>``.

.. generic-function:: connection-setter
   :open:

   :signature: connection-setter (c o) => (result)

   :parameter c: An instance of :class:`<connection>`.
   :parameter o: An instance of ``<object>``.
   :value result: An instance of :class:`<connection>`.

.. generic-function:: connections
   :open:

   :signature: connections (#key dbms) => (connection-sequence)

   :parameter #key dbms: An instance of ``false-or(<dbms>)``.
   :value connection-sequence: An instance of ``<sequence>``.

.. generic-function:: constraint-catalog
   :open:

   :signature: constraint-catalog (diag) => (constraint-catalog)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value constraint-catalog: An instance of ``<string>``.

.. generic-function:: constraint-name
   :open:

   :signature: constraint-name (diag) => (constraint-name)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value constraint-name: An instance of ``<string>``.

.. generic-function:: constraint-schema
   :open:

   :signature: constraint-schema (diag) => (constraint-schema)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value constraint-schema: An instance of ``<string>``.

.. generic-function:: constraints
   :open:

   :signature: constraints (db-object) => (result)

   :parameter db-object: An instance of ``<database-object>``.
   :value result: An instance of :class:`<result-set>`.

.. generic-function:: convert-value

   :signature: convert-value (coercion-policy value key) => (converted-value)

   :parameter coercion-policy: An instance of :class:`<coercion-policy>`.
   :parameter value: An instance of ``<object>``.
   :parameter key: An instance of ``<integer>``.
   :value converted-value: An instance of ``<object>``.

.. generic-function:: cursor-name
   :open:

   :signature: cursor-name (diag) => (cursor-name)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value cursor-name: An instance of ``<string>``.

.. generic-function:: database
   :open:

   :signature: database (connection) => (database)

   :parameter connection: An instance of :class:`<connection>`.
   :value database: An instance of :class:`<database>`.

.. generic-function:: database-object-name

   :signature: database-object-name (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: database-object-name-setter

   :signature: database-object-name-setter (value object) => (#rest results)

   :parameter value: An instance of ``<object>``.
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: datatype-hints

   :signature: datatype-hints (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: datatype-hints-setter

   :signature: datatype-hints-setter (value object) => (#rest results)

   :parameter value: An instance of ``<object>``.
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: dbms
   :open:

   :signature: dbms (connection) => (dbms)

   :parameter connection: An instance of :class:`<connection>`.
   :value dbms: An instance of :class:`<dbms>`.

.. generic-function:: dbms-name
   :open:

   :signature: dbms-name (dbms #key connection) => (dbms-name)

   :parameter dbms: An instance of :class:`<dbms>`.
   :parameter #key connection: An instance of :class:`<connection>`.
   :value dbms-name: An instance of ``<string>``.

.. generic-function:: dbms-version
   :open:

   :signature: dbms-version (dbms #key connection) => (dbms-version)

   :parameter dbms: An instance of :class:`<dbms>`.
   :parameter #key connection: An instance of :class:`<connection>`.
   :value dbms-version: An instance of ``<string>``.

.. generic-function:: default-connection

   :signature: default-connection () => (connection)

   :value connection: An instance of :class:`<connection>`.

.. generic-function:: default-conversion
   :open:

   :signature: default-conversion (value) => (converted-value)

   :parameter value: An instance of ``<object>``.
   :value converted-value: An instance of ``<object>``.

.. generic-function:: default-dbms

   :signature: default-dbms () => (dbms)

   :value dbms: An instance of :class:`<dbms>`.

.. generic-function:: default-diagnostics-size
   :open:

   :signature: default-diagnostics-size (connection) => (diagnostics-size)

   :parameter connection: An instance of :class:`<connection>`.
   :value diagnostics-size: An instance of ``<integer>``.

.. generic-function:: default-isolation-level
   :open:

   :signature: default-isolation-level (connection) => (level)

   :parameter connection: An instance of :class:`<connection>`.
   :value level: An instance of :class:`<isolation-level>`.

.. generic-function:: default-transaction-mode
   :open:

   :signature: default-transaction-mode (connection) => (mode)

   :parameter connection: An instance of :class:`<connection>`.
   :value mode: An instance of :class:`<transaction-mode>`.

.. generic-function:: default-value
   :open:

   :signature: default-value (column) => (default)

   :parameter column: An instance of :class:`<column>`.
   :value default: An instance of ``<object>``.

.. generic-function:: diagnostic-to-string
   :open:

   :signature: diagnostic-to-string (diag) => (string)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value string: An instance of ``<string>``.

.. generic-function:: diagnostics-size

   :signature: diagnostics-size (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: diagnostics-size-setter

   :signature: diagnostics-size-setter (value object) => (#rest results)

   :parameter value: An instance of ``<object>``.
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: disconnect
   :open:

   :signature: disconnect (connection #key terminate-statements) => ()

   :parameter connection: An instance of :class:`<connection>`.
   :parameter #key terminate-statements: An instance of ``<boolean>``.

.. generic-function:: disconnect-all
   :open:

   :signature: disconnect-all (#key dbms) => ()

   :parameter #key dbms: An instance of ``false-or(<dbms>)``.

.. generic-function:: domain

   :signature: domain (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: dynamic-function
   :open:

   :signature: dynamic-function (diag) => (dynamic-function)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value dynamic-function: An instance of ``<string>``.

.. generic-function:: end-transaction
   :open:

   :signature: end-transaction (transaction) => ()

   :parameter transaction: An instance of :class:`<transaction>`.

.. generic-function:: environment-name
   :open:

   :signature: environment-name (diag) => (env-name)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value env-name: An instance of ``<string>``.

.. generic-function:: execute
   :open:

   Prepares an SQL statement for execution on the specified connection
   and then executes the statement.

   :signature: execute (database-statement #key #all-keys) => (result-set)

   :parameter database-statement: An instance of ``type-union(<database-statement>, <string>)``.
   :parameter #key connection: An instance of :class:`<connection>`.
   :parameter #key parameters: An instance of ``false-or(<sequence>)``.
   :parameter #key result-set-policy: An instance of ``false-or(<result-set-policy>)``.
   :parameter #key liaison: An instance of ``false-or(<function>)``
      whose signature is ``liaison(<record>) => <object>``. Default
      value: :gf:`default-liaison`.
   :value result-set: An instance of ``false-or(<result-set>)``.

   :description:

     Prepares the SQL statement represented by sql-statement for
     execution on the connection, then sends it to the DBMS for
     execution.

     If connection is not supplied, execute uses the connection
     returned by default-connection instead.

     The liaison function is invoked on each record as it is retrieved
     from the database. If a liaison function is not provided, a
     default value of :gf:`default-liaison` is used; each result-set
     class has its own :gf:`default-liaison`.

     In the SQL-ODBC library, the ``database-statement`` will be an
     instance of :class:`<sql-statement>`. If anonymous host
     variables--that is, question marks (``?``)--appear in
     ``database-statement``, pass suitable substitution
     values in the call to this function.

   :example:

     This example executes two SQL statements against the database
     represented by ``the-connection``. The first SQL statement
     inserts a new book record into the book table. The second SQL
     statement queries for the list of titles and their ISBN
     published by Addison Wesley.

     .. code-block:: dylan

        with-connection(the-connection)
          let insert-stmt :: <sql-statement> =
          make(<sql-statement>,
            text: "insert into book (title, publisher, isbn) 
                       values (?, ?, ?)",
            input-indicator: $null-value);
          execute(insert-stmt,
                  parameters: #("Large Databases", "Addison-Wesley",
                                $null-value));

        let query-stmt :: <sql-statement> =
          make(<sql-statement>, 
          text: "select title, isbn from book 
                     where publisher = ?",
                     output-indicator: $null-value);
          execute(query-stmt, parameters: #("Addison-Wesley"));
        end with-connection;

        => #(#("An Introduction to Database Systems", "0-201-14201-5"),
             #("Relational Database Writings, 1991-1994", "0-8053-1748-1), #("Large Databases", $null-value))


.. generic-function:: fields

   :signature: fields (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: fields-setter

   :signature: fields-setter (value object) => (#rest results)

   :parameter value: An instance of ``<object>``.
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. function:: find-diagnostic

   :signature: find-diagnostic (table diagnostic-set-key sqlstate) => (diagnostic-detail-class)

   :parameter table: An instance of :class:`<diagnostic-table>`.
   :parameter diagnostic-set-key: An instance of ``<object>``.
   :parameter sqlstate: An instance of ``<string>``.
   :value diagnostic-detail-class: An instance of ``<object>``.

.. generic-function:: indexed-table

   :signature: indexed-table (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: indexed-table-setter

   :signature: indexed-table-setter (value object) => (#rest results)

   :parameter value: An instance of ``<object>``.
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: indexes
   :open:

   :signature: indexes (table) => (index-collection)

   :parameter table: An instance of :class:`<sql-table>`.
   :value index-collection: An instance of :class:`<result-set>`.

.. generic-function:: indicator-policy

   :signature: indicator-policy (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: input-indicator
   :open:

   :signature: input-indicator (sql-statement) => (input-indicator)

   :parameter sql-statement: An instance of :class:`<sql-statement>`.
   :value input-indicator: An instance of :class:`<indicator-policy>`.

.. generic-function:: input-indicator-setter
   :open:

   :signature: input-indicator-setter (new-input-indicator sql-statement) => (new-input-indicator)

   :parameter new-input-indicator: An instance of :class:`<indicator-policy>`.
   :parameter sql-statement: An instance of :class:`<sql-statement>`.
   :value new-input-indicator: An instance of :class:`<indicator-policy>`.

.. function:: install-diagnostic

   :signature: install-diagnostic (table class #key key) => ()

   :parameter table: An instance of :class:`<diagnostic-table>`.
   :parameter class: An instance of ``subclass(<diagnostic>)``.
   :parameter #key key: An instance of ``<symbol>``.

.. function:: install-diagnostic-key

   :signature: install-diagnostic-key (key) => ()

   :parameter key: An instance of ``<symbol>``.

.. generic-function:: installation-functions

   :signature: installation-functions (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: is-null?

   :signature: is-null? (record key) => (null-state)

   :parameter record: An instance of :class:`<record>`.
   :parameter key: An instance of ``<integer>``.
   :value null-state: An instance of ``<boolean>``.

.. generic-function:: isolation-level

   :signature: isolation-level (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: isolation-level-setter

   :signature: isolation-level-setter (value object) => (#rest results)

   :parameter value: An instance of ``<object>``.
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: liaison

   :signature: liaison (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: liaison-setter

   :signature: liaison-setter (value object) => (#rest results)

   :parameter value: An instance of ``<object>``.
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: make-dbms-specific
   :open:

   :signature: make-dbms-specific (type dbms #rest more-args) => (instance)

   :parameter type: An instance of ``<class>``.
   :parameter dbms: An instance of :class:`<dbms>`.
   :parameter #rest more-args: An instance of ``<object>``.
   :value instance: An instance of ``<object>``.

.. generic-function:: message-text
   :open:

   :signature: message-text (diag) => (message-text)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value message-text: An instance of ``<string>``.

.. generic-function:: multiple-connections?
   :open:

   :signature: multiple-connections? (dbms) => (multiple-connections-status)

   :parameter dbms: An instance of :class:`<dbms>`.
   :value multiple-connections-status: An instance of ``<boolean>``.

.. generic-function:: next-dbms-diagnostic
   :open:

   :signature: next-dbms-diagnostic (diag) => (next-diagnostic)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value next-diagnostic: An instance of ``false-or(<diagnostic>)``.

.. generic-function:: nullable?

   :signature: nullable? (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: output-indicator
   :open:

   :signature: output-indicator (sql-statement) => (output-indicator)

   :parameter sql-statement: An instance of :class:`<sql-statement>`.
   :value output-indicator: An instance of :class:`<indicator-policy>`.

.. generic-function:: output-indicator-setter
   :open:

   :signature: output-indicator-setter (new-output-indicator sql-statement) => (new-output-indicator)

   :parameter new-output-indicator: An instance of :class:`<indicator-policy>`.
   :parameter sql-statement: An instance of :class:`<sql-statement>`.
   :value new-output-indicator: An instance of :class:`<indicator-policy>`.

.. generic-function:: possible-explanation

   :signature: possible-explanation (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: record-available?
   :open:

   :signature: record-available? (result-set key) => (availability)

   :parameter result-set: An instance of :class:`<result-set>`.
   :parameter key: An instance of ``<integer>``.
   :value availability: An instance of ``<boolean>``.

.. generic-function:: record-coercion-policy

   :signature: record-coercion-policy (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. function:: register-diagnostic-installer

   :signature: register-diagnostic-installer (function) => ()

   :parameter function: An instance of ``<function>``.

.. generic-function:: returned-sqlstate
   :open:

   :signature: returned-sqlstate (diag) => (sqlstate)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value sqlstate: An instance of ``<string>``.

.. generic-function:: rollback-transaction
   :open:

   :signature: rollback-transaction (transaction) => ()

   :parameter transaction: An instance of :class:`<transaction>`.

.. generic-function:: row-count
   :open:

   :signature: row-count (diag) => (count)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value count: An instance of ``<integer>``.

.. generic-function:: rowset-size

   :signature: rowset-size (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: schema-from-name
   :open:

   :signature: schema-from-name (connection catalog-name schema-name) => (schema)

   :parameter connection: An instance of :class:`<connection>`.
   :parameter catalog-name: An instance of ``<string>``.
   :parameter schema-name: An instance of ``<string>``.
   :value schema: An instance of :class:`<schema>`.

.. generic-function:: schema-name
   :open:

   :signature: schema-name (diag) => (schema-name)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value schema-name: An instance of ``<string>``.

.. generic-function:: scroll-window

   :signature: scroll-window (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: scrollable?

   :signature: scrollable? (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. macro:: sql

.. generic-function:: start-transaction
   :open:

   :signature: start-transaction (connection transaction-mode isolation-level diagnostics-size) => (transaction)

   :parameter connection: An instance of :class:`<connection>`.
   :parameter transaction-mode: An instance of :class:`<transaction-mode>`.
   :parameter isolation-level: An instance of :class:`<isolation-level>`.
   :parameter diagnostics-size: An instance of ``<integer>``.
   :value transaction: An instance of :class:`<transaction>`.

.. generic-function:: statement-column-names
   :open:

   :signature: statement-column-names (statement) => (column-names)

   :parameter statement: An instance of :class:`<sql-statement>`.
   :value column-names: An instance of ``<sequence>``.

.. generic-function:: subclass-code

   :signature: subclass-code (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: subclass-origin
   :open:

   :signature: subclass-origin (diag) => (subclass-origin)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value subclass-origin: An instance of ``<string>``.

.. generic-function:: table-from-name
   :open:

   :signature: table-from-name (connection catalog-name schema-name table-name) => (table)

   :parameter connection: An instance of :class:`<connection>`.
   :parameter catalog-name: An instance of ``<string>``.
   :parameter schema-name: An instance of ``<string>``.
   :parameter table-name: An instance of ``<string>``.
   :value table: An instance of :class:`<sql-table>`.

.. generic-function:: table-name
   :open:

   :signature: table-name (diag) => (table-name)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value table-name: An instance of ``<string>``.

.. generic-function:: text
   :open:

   :signature: text (sql-statement) => (sql-statement-text)

   :parameter sql-statement: An instance of :class:`<sql-statement>`.
   :value sql-statement-text: An instance of ``<string>``.

.. generic-function:: text-setter
   :open:

   :signature: text-setter (new-text sql-statement) => (new-text)

   :parameter new-text: An instance of ``<string>``.
   :parameter sql-statement: An instance of :class:`<sql-statement>`.
   :value new-text: An instance of ``<string>``.

.. generic-function:: transaction-mode

   :signature: transaction-mode (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: transaction-mode-setter

   :signature: transaction-mode-setter (value object) => (#rest results)

   :parameter value: An instance of ``<object>``.
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: unique-index?

   :signature: unique-index? (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: user
   :open:

   :signature: user (connection) => (user)

   :parameter connection: An instance of :class:`<connection>`.
   :value user: An instance of :class:`<user>`.

.. macro:: with-connection

.. macro:: with-database

.. macro:: with-dbms

.. macro:: with-transaction
