***************
The date Module
***************

.. current-library:: system
.. current-module:: date

Introduction
------------

The ``date`` module is part of the System library and provides a
machine-independent facility for representing and manipulating dates and
date/time intervals.

This chapter describes the classes, types, and functions that the Date
module contains.

Representing dates and times
----------------------------

The ``date`` module contains a single class, ``<date>``, an instance of which
can represent any date between 1 Jan 1800 00:00:00 and 31 Dec 2199
23:59:59, Greenwich Mean Time. You can create a date object by calling
the function *encode-date* or using the *make* method for ``<date>``.

- :class:`<date>`
- :meth:`make <make(<date>)>`
- :func:`encode-date`
- :func:`current-date`
- :class:`<day-of-week>`

Each of the arguments to :func:`encode-date` and to the :meth:`make
<make(<date>)>` method on :class:`<date>` is an instance of
:drm:`<integer>` (except for the *iso8601-string* keyword for the *make*
method, which is a string) that is passed as an init-keyword value to
the ``<date>`` object. Each init-keyword takes an instance of
:drm:`<integer>`, limited to the natural range for that attribute. For
example, *month:* can only take values between 1 and 12.

You must specify values, via :func:`encode-date`, for at least the
*year:*, *month:*, and *day:* init-keywords. In addition, you can also
specify values for *hours:*, *minutes:*, *seconds:*, *microseconds:*,
and *time-zone-offset:*. If not supplied, the default value for each of
these init-keywords is 0.

The *time-zone-offset:* init-keyword is used to represent time zones in
the ``date`` module as :drm:`<integer>` values representing the offset in minutes
from Greenwich Mean Time (GMT). Positive values are used for time zones
East of Greenwich; negative values represent time zones to the west of
Greenwich.

For example, the value -300 (-5 hours) is U.S. Eastern Standard Time and
the value -240 (-4 hours) is U.S. Eastern Daylight Savings Time.

If you wish, a ``<date>`` can be specified completely by using the
*iso8601-string:* init-keyword. This init-keyword takes an instance of
:drm:`<string>`, which should be a valid ISO8601 format date. If you use the
*iso8601-string:* init-keyword, there is no need to specify any other
init-keywords to a call to *make* on ``<date>``.

Representing durations
----------------------

Date/time intervals, called durations, are modeled in a style quite
similar to that of SQL. There are two, effectively disjoint, classes of
duration: one with a resolution of months (for example, 3 years, 2
months) and the other with a resolution of microseconds (for example, 50
days, 6 hours, 23 minutes). The first is :class:`<year/month-duration>` and the
second :class:`<day/time-duration>`.

An important distinction between ``<day/time-duration>`` and
``<year/month-duration>`` is that a given instance of
``<day/time-duration>`` is always a fixed unit of a fixed length, whereas
a ``<year/month-duration>`` follows the vagaries of the calendar. So if
you have a ``<date>`` that represents, for example, the 5th of some month,
adding a ``<year/month-duration>`` of 1 month to that will always take you
to the 5th of the following month, whether that is an interval of 28,
29, 30, or 31 days.

- :class:`<duration>`
- :class:`<year/month-duration>`
- :class:`<day/time-duration>`
- :func:`encode-year/month-duration`
- :func:`encode-day/time-duration`
- :gf:`decode-duration`

Performing operations on dates and durations
--------------------------------------------

A number of interfaces are exported from the ``date`` module that let you
perform other operations on dates and durations, and extract
date-specific information from your local machine.

Comparing dates
^^^^^^^^^^^^^^^

The following operations are exported from the ``date`` module.

- :meth:`= <=(<date>)>`
- :meth:`< <<(<date>)>`

These methods let you perform arithmetic-like operations on dates to
test for equality, or to test whether one date occurred before another.

Comparing durations
^^^^^^^^^^^^^^^^^^^

The following operations are exported from the ``date`` module.

- :meth:`= <=(<duration>)>`
- :meth:`< <<(<duration>)>`

As with dates, you can perform arithmetic-like operations on durations
to test for equality, or to test whether one duration is shorter than
another.

Performing arithmetic operations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can add, subtract, multiply, and divide dates and durations in a
number of ways to produce a variety of date or duration information.
Methods are defined for any combination of date and duration, with any
operation that makes sense, and the return value is of the appropriate
type.

For example, a method is defined that subtracts one date from another,
and returns a duration, but there is no method for adding two dates
together, since dates cannot be summed in any sensible way. However,
there are methods for adding dates and durations which return dates.

Note that some addition and subtraction operations involving dates and
instances of :class:`<year/month-duration>` can cause
errors where the result is a date that does not exist in the calendar.
For example, adding one month to January 30th.

The table below summarizes the methods defined for each arithmetic
operation, for different combinations of date and duration arguments,
together with their return values.

Methods defined for arithmetic operations on dates and durations

+-------+---------------------------+---------------------------+---------------------------+
| Op    | Argument 1                | Argument 2                | Return value              |
+=======+===========================+===========================+===========================+
| ``+`` | ``<duration>``            | ``<duration>``            | ``<duration>``            |
+-------+---------------------------+---------------------------+---------------------------+
| ``+`` | ``<year/month-duration>`` | ``<year/month-duration>`` | ``<year/month-duration>`` |
+-------+---------------------------+---------------------------+---------------------------+
| ``+`` | ``<day/time-duration>``   | ``<day/time-duration>``   | ``<day/time-duration>``   |
+-------+---------------------------+---------------------------+---------------------------+
| ``+`` | ``<date>``                | ``<duration>``            | ``<date>``                |
+-------+---------------------------+---------------------------+---------------------------+
| ``+`` | ``<duration>``            | ``<date>``                | ``<date>``                |
+-------+---------------------------+---------------------------+---------------------------+
| ``-`` | ``<duration>``            | ``<duration>``            | ``<duration>``            |
+-------+---------------------------+---------------------------+---------------------------+
| ``-`` | ``<year/month-duration>`` | ``<year/month-duration>`` | ``<year/month-duration>`` |
+-------+---------------------------+---------------------------+---------------------------+
| ``-`` | ``<day/time-duration>``   | ``<day/time-duration>``   | ``<day/time-duration>``   |
+-------+---------------------------+---------------------------+---------------------------+
| ``-`` | ``<date>``                | ``<duration>``            | ``<date>``                |
+-------+---------------------------+---------------------------+---------------------------+
| ``-`` | ``<date>``                | ``<date>``                | ``<day/time-duration>``   |
+-------+---------------------------+---------------------------+---------------------------+
| ``*`` | ``<duration>``            | :drm:`<real>`             | ``<duration>``            |
+-------+---------------------------+---------------------------+---------------------------+
| ``*`` | :drm:`<real>`             | ``<duration>``            | ``<duration>``            |
+-------+---------------------------+---------------------------+---------------------------+
| ``/`` | ``<duration>``            | :drm:`<real>`             | ``<duration>``            |
+-------+---------------------------+---------------------------+---------------------------+

Dealing with time-zones
^^^^^^^^^^^^^^^^^^^^^^^

The following functions return information about the time-zone that the
host machine is in.

- :func:`local-time-zone-name`
- :func:`local-time-zone-offset`
- :func:`local-daylight-savings-time?`

Extracting information from dates
---------------------------------

A number of functions are available to return discrete pieces of
information from a specified ``<date>`` object. These are useful to allow
you to deconstruct a given date in order to retrieve useful information
from it.

The most basic way to extract information from a date is to use the
function *decode-date*.

- :func:`decode-date`

  Decodes a ``<date>`` into its constituent parts. This function is the
  companion of *encode-date*, in that it takes a ``<date>`` object and
  returns all of its constituent parts. Note, however, that in contrast to
  *encode-date*, it does not return any millisecond component to the
  date, but it does return the day of the week of the specified date.

A number of other functions exist to extract individual components from
a ``<date>`` object. Each of these functions is listed below. Each
function takes a single argument, a ``<date>`` object, and returns the
component of the date referred to in the function name. For example,
*date-month* takes a ``<date>`` object as an argument, and returns the
month that the date refers to.

- :func:`date-year`
- :func:`date-month`
- :func:`date-day`
- :func:`date-day-of-week`
- :func:`date-hours`
- :func:`date-minutes`
- :func:`date-seconds`
- :func:`date-microseconds`
- :func:`date-time-zone-offset`

For each function except *date-day-of-week*, the value returned is an
instance of :drm:`<integer>`. The *date-day-of-week* function returns an
object of type ``<day-of-week>``. For more information, please refer to
the reference entries of each function. See also the function
:func:`date-time-zone-offset-setter`, which allows you to set
the time-zone offset of a ``<date>`` explicitly.

Formatting Dates
----------------

To return an ISO 8601 format date from a ``<date>`` object, use the
function :func:`as-iso8601-string`.

Dates can also be returned in RFC-822 and RFC-1123 formats with the
:func:`as-rfc822-string` and :func:`as-rfc1123-string` functions.

More flexible date formatting is available with :func:`format-date`.

The date module
---------------

This section contains a reference entry for each item exported from the
Date module.

.. method:: =
   :specializer: <date>
   :sealed:

   Compares two dates for equality.

   :signature: *date1* = *date2* => *equal?*
   :parameter date1: An instance of :class:`<date>`.
   :parameter date2: An instance of :class:`<date>`.
   :value equal?: An instance of :drm:`<boolean>`.

   :description:

     This method lets you compare two dates to see if they are equal.
     Any differences in microseconds between *date1* and *date2* are
     ignored.

   See also

   - :meth:`< <<(<date>)>`

.. method:: =
   :specializer: <duration>
   :sealed:

   Compares two durations for equality.

   :signature: *duration1* = *duration2* => *equal?*

   :parameter duration1: An instance of :class:`<duration>`.
   :parameter duration2: An instance of :class:`<duration>`.
   :value equal?: An instance of :drm:`<boolean>`.

   :description:

     This method lets you compare two durations to see if they are
     equal. If the durations are actually instances of
     :class:`<day/time-duration>`, any differences in microseconds
     between *duration1* and *duration2* are ignored.

   See also

   - :meth:`< <<(<duration>)>`

.. method:: <
   :sealed:
   :specializer: <date>

   Determines whether one date is earlier than another.

   :signature: *date1* < *date2* => *before?*

   :parameter date1: An instance of :class:`<date>`.
   :parameter date2: An instance of :class:`<date>`.
   :value before?: An instance of :drm:`<boolean>`.

   :description:

     This method determines if *date1* is earlier than *date2*. Any
     differences in microseconds between *date1* and *date2* are
     ignored.

   See also

   - :meth:`= <=(<date>)>`

.. method:: <
   :sealed:
   :specializer: <duration>

   Determines whether one duration is less than another.

   :signature: *duration1* < *duration2* => *less-than?*

   :parameter duration1: An instance of :class:`<duration>`.
   :parameter duration2: An instance of :class:`<duration>`.
   :value less-than?: An instance of :drm:`<boolean>`.

   :description:

     This method determines if *duration1* is less than *duration2*. If
     the durations are actually instances of :class:`<day/time-duration>`, any
     differences in microseconds between *duration1* and *duration2* are
     ignored.

   See also

   - :meth:`= <=(<duration>)>`

\+
^^

Sealed methods

   Performs addition on specific combinations of dates and durations.

   :signature: + *arg1* *arg2* => *sum*

Arguments

-  *arg1* An instance of :class:`<date>` or
   :class:`<duration>`. See description for details.
-  *arg2* An instance of :class:`<date>` or
   :class:`<duration>`. See description for details.

Values

-  *sum* An instance of :class:`<date>` or
   :class:`<duration>`. See description for details.

   :description:

A number of methods are defined for the *+* generic function to allow
summing of various combinations of dates and durations. Note that there
is not a method defined for every possible combination of date and
duration. Specifically, you cannot sum different types of duration, and
you cannot sum two dates. The return value can be either a date or a
duration, depending on the arguments supplied. The table below lists the
methods that are defined on *+*.

Methods defined for addition of dates and durations

*arg1*

*arg2*

*sum*

#. ``<duration>``

#. ``<duration>``

#. ``<duration>``

#. ``<year/month-duration>``

#. ``<year/month-duration>``

#. ``<year/month-duration>``

#. ``<day/time-duration>``

#. ``<day/time-duration>``

#. ``<day/time-duration>``

#. ``<date>``

#. ``<duration>``

#. ``<date>``

#. ``<duration>``

#. ``<date>``

#. ``<date>``

   See also: `-`_, `\*`_, `/`_

\-
^^

Sealed methods

   Performs subtraction on specific combinations of dates and durations.

   :signature: - *arg1* *arg2* => *diff*

Arguments

-  *arg1* An instance of :class:`<date>` or
   :class:`<duration>`. See description for details.
-  *arg2* An instance of :class:`<duration>`, or an
   instance of :class:`<date>` if *arg1* is a ``<date>``.
   See description for details.

Values

-  *diff* An instance of :class:`<date>` or
   :class:`<duration>`. See description for details.

   :description:

A number of methods are defined for the *-* generic function to allow
subtraction of various combinations of dates and durations. Note that
there is not a method defined for every possible combination of date and
duration. Specifically, you cannot subtract a date from a duration, and
you cannot subtract different types of duration. The return value can be
either a date or a duration, depending on the arguments supplied. The
table below lists the methods that are defined on *-*.

Methods defined for subtraction of dates and durations
                                                      
*arg1*

*arg2*

*diff*

#. ``<year/month-duration>``

#. ``<year/month-duration>``

#. ``<year/month-duration>``

#. ``<day/time-duration>``

#. ``<day/time-duration>``

#. ``<day/time-duration>``

#. ``<date>``

#. ``<duration>``

#. ``<date>``

#. ``<date>``

#. ``<date>``

#. ``<day/time-duration>``

   See also: `+`_ `\*`_ `/`_

\*
^^

Sealed methods

   Multiplies a duration by a scale factor.

   :signature: \* *duration* *scale* => *new-duration*
   :signature: \* *scale* *duration* => *new-duration*

Arguments

-  *duration* An instance of :class:`<duration>`.
-  *scale* An instance of :drm:`<real>`.

*Note:* These arguments can be expressed in any order.

Values

-  *new-duration* An instance of :class:`<date>` or
   :class:`<duration>`. See description for details.

   :description:

Multiples a duration by a scale factor and returns the result. Note that
the arguments can be expressed in any order: methods are defined such
that the duration can be placed first or second in the list of
arguments.

   See also: `+`_, `-`_, `/`_

/
^

Sealed methods

   Divides a duration by a scale factor

   :signature: / *duration* *scale* => *new-duration*

Arguments

-  *duration* An instance of :class:`<duration>`.
-  *scale* An instance of :drm:`<real>`.

Values

-  *new-duration* An instance of :class:`<date>` or
   :class:`<duration>`. See description for details.

   :description:

A number of methods are defined for the + generic function to allow
summing of various combinations of dates and durations. Note that there
is not a method defined for every possible combination of date and
duration. Specifically, you cannot sum different types of duration, and
you cannot sum two dates. The return value can be either a date or a
duration, depending on the arguments supplied. The table below lists the
methods that are defined on +.

   See also: `+`_, `-`_, `\*`_

.. function:: as-iso8601-string

   Returns a string representation of a date, conforming to the ISO 8601
   standard.

   :signature: as-iso8601-string *date* #key *precision* => *iso8601-string*

   :parameter date: An instance of :class:`<date>`.
   :parameter precision: An instance of :drm:`<integer>`. Default value: 0.
   :value iso8601-string: An instance of :drm:`<string>`.

   :description:

     Returns a string representation of *date* using the format
     identified by International Standard ISO 8601 (for example,
     ``"19960418T210634Z"``). If *precision* is non-zero, the specified
     number of digits of a fraction of a second are included in the
     string (for example, ``"19960418T210634.0034Z"``).

     The returned string always expresses the time in Greenwich Mean
     Time. The *iso8601-string* init-keyword for :class:`<date>`,
     however, accepts ISO 8601 strings with other time zone
     specifications.

     This is the same as calling:

     .. code-block:: dylan

        format-date("%Y-%m-%dT%H:%M:%S%:z", date);

   See also

   - :class:`<date>`
   - :func:`as-rfc822-string`
   - :func:`as-rfc1123-string`
   - :func:`format-date`

.. function:: as-rfc822-string

   Returns a string representation of a date, conforming to
   `RFC 822 <http://www.w3.org/Protocols/rfc822/#z28>`_.

   :signature: as-rfc822-string *date* => *rfc822-string*

   :parameter date: An instance of :class:`<date>`.
   :value rfc822-string: An instance of :drm:`<string>`.

   :description:

     An example in this format is::

        Sun, 01 Sep 13 17:00:00 GMT

     This is the same as calling:

     .. code-block:: dylan

        format-date("%a, %d %b %y %H:%M:%S %z", date);

   See also

   - :class:`<date>`
   - :func:`as-rfc1123-string`
   - :func:`as-iso8601-string`
   - :func:`format-date`

.. function:: as-rfc1123-string

   Returns a string representation of a date, conforming to
   `RFC 1123 <http://www.faqs.org/rfcs/rfc1123.html>`_.

   :signature: as-rfc1123-string *date* => *rfc1123-string*

   :parameter date: An instance of :class:`<date>`.
   :value rfc1123-string: An instance of :drm:`<string>`.

   :description:

     The date format for RFC-1123 is the same as for RFC-822
     except that the year must be 4 digits rather than 2::

        Sun, 01 Sep 2013 17:00:00 GMT

     This is the same as calling:

     .. code-block:: dylan

        format-date("%a, %d %b %Y %H:%M:%S %z", date);

     This format is commonly used in email, HTTP headers,
     RSS feeds and other protocols where date representations
     are used.

   See also

   - :class:`<date>`
   - :func:`as-rfc822-string`
   - :func:`as-iso8601-string`
   - :func:`format-date`

.. function:: current-date

   Returns a date object representing the current date and time.

   :signature: current-date () => *date*

   :value date: An instance of :class:`<date>`.

   :description:

     Returns *date* for the current date and time.

.. class:: <date>
   :sealed:

   The class of objects representing dates.

   :superclasses: :drm:`<number>`

   :keyword iso8601-string: An instance of ``false-or(<string>)``.
     Default value: ``#f``.
   :keyword year: An instance of ``limited(<integer>, min: 1800, max:
     2199)``.
   :keyword month: An instance of ``limited(<integer>, min: 1, max:
     12)``.
   :keyword day: An instance of ``limited(<integer>, min: 1, max: 31)``.
   :keyword hours: An instance of ``limited(<integer>, min: 0, max:
     23)``. Default value: 0.
   :keyword minutes: An instance of ``limited(<integer>, min: 0, max:
     59)``. Default value: 0.
   :keyword seconds: An instance of ``limited(<integer>, min: 0, max:
     59)``. Default value: 0.
   :keyword microseconds: An instance of ``limited(<integer>, min: 0,
     max: 999999)``. Default value: 0.
   :keyword time-zone-offset: An instance of :drm:`<integer>`. Default
     value: 0.

   :description:

     Represents a date and time between 1 Jan 1800 00:00:00 and 31 Dec
     2199 23:59:59, Greenwich Mean Time (GMT).

     A ``<date>`` can be specified to microsecond precision and includes
     a time zone indication.

     If supplied, the *iso8601-string:* init-keyword completely
     specifies the value of the ``<date>``. Otherwise, the *year:*,
     *month:*, and *day:* init-keywords must be supplied. Note that,
     although you can supply ISO 8601 strings that represent any time
     zone specification, the related function :func:`as-iso8601-string`
     always returns an ISO 8601 string representing a time in Greenwich
     Mean Time.

     For the *time-zone-offset* init-keyword, a positive number
     represents an offset ahead of GMT, in minutes, and a negative
     number represents an offset behind GMT. The value returned is an
     instance of :drm:`<integer>` (for example, -300 represents the offset
     for EST, which is 5 hours behind GMT).

   :operations:

     - :meth:`= <=(<date>)>`
     - :meth:`< <<(<date>)>`
     - :meth:`+ <+(<date>)>`
     - :meth:`- <-(<date>)>`
     - :func:`as-iso8601-string`
     - :func:`as-rfc822-string`
     - :func:`as-rfc1123-string`
     - :func:`current-date`
     - :func:`date-day`
     - :func:`date-day-of-week`
     - :func:`date-hours`
     - :func:`date-microseconds`
     - :func:`date-minutes`
     - :func:`date-month`
     - :func:`date-seconds`
     - :func:`date-time-zone-offset`
     - :func:`date-time-zone-offset-setter`
     - :func:`date-year`
     - :func:`decode-date`

   See also

   - :func:`as-iso8601-string`
   - :func:`as-rfc822-string`
   - :func:`as-rfc1123-string`
   - :class:`<day-of-week>`

.. function:: date-day

   Returns the day of the month component of a specified date.

   :signature: date-day *date* => *day*

   :parameter date: An instance of :class:`<date>`.
   :value day: An instance of :drm:`<integer>`.

   :description:

     Returns the day of the month component of the specified *date*. For
     example, if passed a :class:`<date>` that represented 16:36 on the
     20th June, 1997, *date-day* returns the value 20.

   See also

   - :func:`decode-date`
   - :func:`date-month`
   - :func:`date-year`
   - :func:`date-hours`
   - :func:`date-minutes`
   - :func:`date-seconds`
   - :func:`date-microseconds`
   - :func:`date-time-zone-offset`
   - :func:`date-day-of-week`

.. function:: date-day-of-week

   Returns the day of the week of a specified date.

   :signature: date-day-of-week *date* => *day-of-week*

   :parameter date: An instance of :class:`<date>`.
   :value day-of-week: An object of type ``<day-of-week>``.

   :description:

     Returns the day of the week of the specified *date*.

   See also

   - :func:`decode-date`
   - :func:`date-month`
   - :func:`date-year`
   - :func:`date-hours`
   - :func:`date-minutes`
   - :func:`date-seconds`
   - :func:`date-microseconds`
   - :func:`date-time-zone-offset`
   - :func:`date-day`
   - :class:`<day-of-week>`

.. function:: date-hours

   Returns the hour component of a specified date.

   :signature: date-hours *date* => *hour*

   :parameter date: An instance of :class:`<date>`.
   :value hour: An instance of :drm:`<integer>`.

   :description:

     Returns the hour component of the specified *date*. This component is
     always expressed in 24 hour format.

   See also

   - :func:`decode-date`
   - :func:`date-month`
   - :func:`date-day`
   - :func:`date-year`
   - :func:`date-minutes`
   - :func:`date-seconds`
   - :func:`date-microseconds`
   - :func:`date-time-zone-offset`
   - :func:`date-day-of-week`

.. function:: date-microseconds

   Returns the microseconds component of a specified date.

   :signature: date-microseconds *date* => *microseconds*

   :parameter date: An instance of :class:`<date>`.
   :value microseconds: An instance of :drm:`<integer>`.

   :description:

     Returns the microseconds component of the specified *date*. Note
     that this does *not* return the entire date object, represented as
     a number of microseconds; it returns any value assigned to the
     *microseconds:* init-keyword when the :class:`<date>` object was
     created.

   See also

   - :func:`decode-date`
   - :func:`date-month`
   - :func:`date-day`
   - :func:`date-hours`
   - :func:`date-minutes`
   - :func:`date-seconds`
   - :func:`date-year`
   - :func:`date-time-zone-offset`
   - :func:`date-day-of-week`

.. function:: date-minutes

   Returns the minutes component of a specified date.

   :signature: date-minutes *date* => *minutes*

   :parameter date: An instance of :class:`<date>`.
   :value minutes: An instance of :drm:`<integer>`.

   :description:

     Returns the minutes component of the specified *date*.

   See also

   - :func:`decode-date`
   - :func:`date-month`
   - :func:`date-day`
   - :func:`date-hours`
   - :func:`date-year`
   - :func:`date-seconds`
   - :func:`date-microseconds`
   - :func:`date-time-zone-offset`
   - :func:`date-day-of-week`

.. function:: date-month

   Returns the month of a specified date.

   :signature: date-month *date* => *month*

   :parameter date: An instance of :class:`<date>`.
   :value month: An instance of :drm:`<integer>`.

   :description:

     Returns the month of the specified *date*.

   See also

   - :func:`decode-date`
   - :func:`date-year`
   - :func:`date-day`
   - :func:`date-hours`
   - :func:`date-minutes`
   - :func:`date-seconds`
   - :func:`date-microseconds`
   - :func:`date-time-zone-offset`
   - :func:`date-day-of-week`

.. function:: date-seconds

   Returns the seconds component of a specified date.

   :signature: date-seconds *date* => *seconds*

   :parameter date: An instance of :class:`<date>`.
   :value seconds: An instance of :drm:`<integer>`.

   :description:

     Returns the seconds component of the specified *date*. Note that
     this does *not* return the entire date object, represented as a
     number of seconds; it returns any value assigned to the *seconds:*
     init-keyword when the :class:`<date>` object was created.

   See also

   - :func:`decode-date`
   - :func:`date-month`
   - :func:`date-day`
   - :func:`date-hours`
   - :func:`date-minutes`
   - :func:`date-year`
   - :func:`date-microseconds`
   - :func:`date-time-zone-offset`
   - :func:`date-day-of-week`

.. function:: date-time-zone-offset

   Returns the time zone offset of a specified date.

   :signature: date-time-zone-offset *date* => *time-zone-offset*

   :parameter date: An instance of :class:`<date>`.
   :value time-zone-offset: An instance of :drm:`<integer>`.

   :description:

     Returns the time zone offset of the specified *date*. The values of
     the other components of *date* reflect this time zone.

     A positive number represents an offset ahead of GMT, in minutes,
     and a negative number represents an offset behind GMT. The value
     returned is an instance of :drm:`<integer>` (for example, -300
     represents the offset for EST, which is 5 hours behind GMT).

   See also

   - :func:`decode-date`
   - :func:`date-month`
   - :func:`date-day`
   - :func:`date-hours`
   - :func:`date-minutes`
   - :func:`date-seconds`
   - :func:`date-year`
   - :func:`date-microseconds`
   - :func:`date-time-zone-offset-setter`
   - :func:`date-day-of-week`

.. function:: date-time-zone-offset-setter

   Change the time zone offset of a specified date, while maintaining
   the same point in time.

   :signature: date-time-zone-offset-setter *new-time-zone-offset* *date*  => *new-time-zone-offset*

   :parameter new-time-zone-offset: An instance of :drm:`<integer>`.
   :parameter date: An instance of :class:`<date>`.
   :value new-time-zone-offset: An instance of :drm:`<integer>`.

   :description:

     Changes the time zone offset of *date* without changing the actual
     point in time identified by the *date*. The values of the other
     components of *date* are adjusted to reflect the new time zone.

     The *new-time-zone-offset* argument should represent the offset
     from GMT, in minutes. Thus, if you wish to specify a new offset
     representing EST, which is 5 hours behind GMT,
     *new-time-zone-offset* should have the value -300.

   See also

   - :func:`date-time-zone-offset`

.. function:: date-year

   Returns the year of a specified date.

   :signature: date-year *date* => *year*

   :parameter date: An instance of :class:`<date>`.
   :value year: An instance of :drm:`<integer>`.

   :description:

     Returns the year of the specified *date*.

   See also

   - :func:`decode-date`
   - :func:`date-month`
   - :func:`date-day`
   - :func:`date-hours`
   - :func:`date-minutes`
   - :func:`date-seconds`
   - :func:`date-microseconds`
   - :func:`date-time-zone-offset`
   - :func:`date-day-of-week`

:: todo Make this a type.

.. class:: <day-of-week>

   The days of the week.

   :description:

     The days of the week. This is the type of the return value of the
     :func:`date-day-of-week` function.

     .. code-block:: dylan

        one-of(#"Sunday", #"Monday", #"Tuesday", #"Wednesday", #"Thursday", #"Friday", #"Saturday")

   :operations:

     - :func:`date-day-of-week`

   See also

   - :func:`date-day-of-week`

.. class:: <day/time-duration>
   :sealed:

   The class of objects representing durations in units of microseconds.

   :superclasses: :class:`<duration>`

   :keyword days: An instance of :drm:`<integer>`.
   :keyword hours: An instance of :drm:`<integer>`. Default value: 0.
   :keyword minutes: An instance of :drm:`<integer>`. Default value: 0.
   :keyword seconds: An instance of :drm:`<integer>`. Default value: 0.
   :keyword microseconds: An instance of :drm:`<integer>`. Default value: 0.

   :description:

     The class of objects representing durations in units of
     microseconds. It is a subclass of :class:`<duration>`.

     Use this class to represent a number of days and fractions thereof.
     If you need to represent durations in calendar units of months or
     years, use :class:`<year/month-duration>` instead.

   :operations:

     - :meth:`< <<(<duration>)>`
     - :meth:`+ <+(<duration>)>`
     - :meth:`- <-(<duration>)>`
     - :gf:`decode-duration`
     - :func:`encode-day/time-duration`

   See also

   - :class:`<duration>`
   - :class:`<year/month-duration>`

.. function:: decode-date

   Returns the date and time stored in a date object.

   :signature: decode-date *date* => *year month day hours minutes seconds day-of-week time-zone-offset*

   :parameter date: An instance of :class:`<date>`.

   :value year: An instance of :drm:`<integer>`.
   :value month: An instance of :drm:`<integer>`.
   :value day: An instance of :drm:`<integer>`.
   :value hours: An instance of :drm:`<integer>`.
   :value minutes: An instance of :drm:`<integer>`.
   :value seconds: An instance of :drm:`<integer>`.
   :value day-of-week: An instance of ``<day-of-week>``.
   :value time-zone-offset: An instance of :drm:`<integer>`.

   :description:

     Returns the date and time stored in *date*.

     Note that it does not return the millisecond component of a
     :class:`<date>`, but it does return the appropriate
     ``<day-of-week>``.

   See also

   - :func:`encode-date`

.. generic-function:: decode-duration
   :sealed:

   Decodes a duration into its constituent parts.

   :signature: decode-duration *duration* => #rest *components*

   :parameter duration: An instance of :class:`<duration>`.
   :value #rest components: Instances of :drm:`<integer>`.

   :description:

     Decodes an instance of :class:`<duration>` into its constituent
     parts. There are methods for this generic function that specialize
     on :class:`<year/month-duration>` and :class:`<day/time-duration>`
     respectively, as described below.

   See also

   - :meth:`decode-duration <decode-duration(<day/time-duration>)>`
   - :meth:`decode-duration <decode-duration(<year/month-duration>)>`

.. method:: decode-duration
   :specializer: <day/time-duration>
   :sealed:

   Decodes a day/time duration into its constituent parts.

   :signature: decode-duration *duration* => *days* *hours* *minutes* *seconds* *microseconds*

   :parameter duration: An instance of :class:`<day/time-duration>`.
   :value days: An instance of :drm:`<integer>`.
   :value hours: An instance of :drm:`<integer>`.
   :value minutes: An instance of :drm:`<integer>`.
   :value seconds: An instance of :drm:`<integer>`.
   :value microseconds: An instance of :drm:`<integer>`.

   :description:

     Decodes an instance of :class:`<day/time-duration>` into its
     constituent parts.

   See also

   - :gf:`decode-duration`
   - :meth:`decode-duration <decode-duration(<year/month-duration>)>`
   - :func:`encode-day/time-duration`

.. method:: decode-duration
   :specializer: <year/month-duration>
   :sealed:

   Decodes a year/month duration into its constituent parts.

   :signature: decode-duration *duration* => *years* *months*

   :parameter duration: An instance of :class:`<year/month-duration>`.
   :value years: An instance of :drm:`<integer>`.
   :value months: An instance of :drm:`<integer>`.

   :description:

     Decodes an instance of :class:`<year/month-duration>` into its
     constituent parts.

   See also

   - :gf:`decode-duration`
   - :meth:`decode-duration <decode-duration(<day/time-duration>)>`
   - :func:`encode-year/month-duration`

.. class:: <duration>
   :sealed:
   :abstract:
   :instantiable:

   The class of objects representing durations.

   :superclasses: :drm:`<number>`

   :keyword iso8601-string: An instance of ``false-or(<string>)``.
     Default value: ``#f``.
   :keyword year: An instance of ``limited(<integer>, min: 1800, max:
     2199)``.
   :keyword month: An instance of ``limited(<integer>, min: 1, max:
     12)``.
   :keyword day: An instance of ``limited(<integer>, min: 1, max: 31)``.
   :keyword hours: An instance of ``limited(<integer>, min: 0, max:
     23)``. Default value: 0.
   :keyword minutes: An instance of ``limited(<integer>, min: 0, max:
     59)``. Default value: 0.
   :keyword seconds: An instance of ``limited(<integer>, min: 0, max:
     59)``. Default value: 0.
   :keyword microseconds: An instance of ``limited(<integer>, min: 0,
     max: 999999)``. Default value: 0.
   :keyword time-zone-offset: An instance of :drm:`<integer>`. Default
     value: 0.

   :description:

     This class is the used to represent durations. It is a subclass of
     :drm:`<number>`, and it has two subclasses.

   :operations:

     - :meth:`= <=(<duration>)>`
     - :meth:`< <<(<duration>)>`
     - :meth:`+ <+(<duration>)>`
     - :meth:`- <-(<duration>)>`
     - :meth:`\* <*(<duration>)>`
     - :meth:`/ </(<duration>)>`

   See also

   - :class:`<day/time-duration>`
   - :class:`<year/month-duration>`

.. function:: encode-date

   Creates a date object for the specified date and time.

   :signature: encode-date *year month day hours minutes seconds* #key*microseconds time-zone-offset* => *date*

   :parameter year: An instance of :drm:`<integer>`.
   :parameter month: An instance of :drm:`<integer>`.
   :parameter day: An instance of :drm:`<integer>`.
   :parameter hours: An instance of :drm:`<integer>`.
   :parameter minutes: An instance of :drm:`<integer>`.
   :parameter seconds: An instance of :drm:`<integer>`.
   :parameter microseconds: An instance of :drm:`<integer>`. Default value:
     0.
   :parameter time-zone-offset: An instance of :drm:`<integer>`. Default
     value: :func:`local-time-zone-offset()`.
   :value date: An instance of :class:`<date>`.

   :description:

     Creates a :class:`<date>` object for the specified date and time.

   See also

   - :gf:`decode-date`
   - :func:`local-time-zone-offset`
   - :meth:`make <make(<date>)>`

.. function:: encode-day/time-duration

   Creates a day/time duration from a set of integer values.

   :signature: encode-day/time-duration *days* *hours* *minutes* *seconds* *microseconds* => *duration*

   :parameter days: An instance of :drm:`<integer>`.
   :parameter hours: An instance of :drm:`<integer>`.
   :parameter minutes: An instance of :drm:`<integer>`.
   :parameter seconds: An instance of :drm:`<integer>`.
   :parameter microseconds: An instance of :drm:`<integer>`.
   :value duration: An instance of :class:`<day/time-duration>`.

   :description:

     Creates an instance of :class:`<day/time-duration>`.

   See also

   - :gf:`decode-duration`
   - :func:`encode-year/month-duration`

.. function:: encode-year/month-duration

   Creates a year/month duration from a set of integer values.

   :signature: encode-year/month-duration *years* *months* => *duration*

   :parameter years: An instance of :drm:`<integer>`.
   :parameter months: An instance of :drm:`<integer>`.
   :value duration: An instance of :class:`<year/month-duration>`.

   :description:

     Creates an instance of :class:`<year/month-duration>`.

   See also

   - :gf:`decode-duration`
   - :func:`encode-day/time-duration`

.. function:: format-date

   Formats a date according to a format string.

   :signature: format-date *format* *date* => *formatted-date*

   :parameter format: An instance of :drm:`<string>`.
   :parameter date: An instance of :class:`<date>`.
   :value formatted-date: An instance of :drm:`<string>`.

   :description:

     ``format-date`` interprets a control string, ``format``,
     to create a string representing the ``date``.

     The control string can contain these directives:

     - ``%Y`` - The year.
     - ``%y`` - The year, in 2 digit form.
     - ``%H`` - Hours, zero padded.
     - ``%k`` - Hours, space padded.
     - ``%M`` - Minutes, zero padded.
     - ``%S`` - Seconds, zero padded.
     - ``%f`` - Microseconds, 6 digits.
     - ``%F`` - Milliseconds, 3 digits.
     - ``%T`` - Time, each component zero padded.
     - ``%m`` - Month in numeric form, zero padded.
     - ``%d`` - Day of the month, zero padded.
     - ``%e`` - Day of the month, space padded.
     - ``%A`` - Name of the day of the week.
     - ``%a`` - Short name of the day of the week.
     - ``%B`` - Name of the month.
     - ``%b`` - Short name of the month.
     - ``%z`` - Time zone offset.
     - ``%:z`` - Time zone offset, but using ``:`` between hours and minutes.
     - ``%n`` - A new line.
     - ``%%`` - A % character.

   See also

   - :func:`as-rfc822-string`
   - :func:`as-rfc1123-string`
   - :func:`as-iso8601-string`

.. function:: local-daylight-savings-time?

   Checks whether the local machine is using Daylight Savings Time.

   :signature: local-daylight-savings-time? () => *dst?*

   :value dst?: An instance of :drm:`<boolean>`.

   :description:

     Returns ``#t`` if the local machine is using Daylight Savings Time,
     and ``#f`` otherwise.

.. function:: local-time-zone-name

   Returns the time zone name in use by the local machine.

   :signature: local-time-zone-name () => *time-zone-name*

   :value time-zone-name: An instance of :drm:`<string>`.

   :description:

     Returns the time zone name in use by the local machine, if
     available, or a string of the form ``+/-HHMM`` if the time zone
     name is unknown.

.. function:: local-time-zone-offset

   Returns the offset of the time-zone from Greenwich Mean Time,
   expressed as a number of minutes.

   :signature: local-time-zone-offset () => *time-zone-offset*

   :value time-zone-offset: An instance of :drm:`<integer>`.

   :description:

     Returns the offset of the time-zone from Greenwich Mean Time,
     expressed as a number of minutes. A positive number represents an
     offset ahead of GMT, and a negative number represents an offset
     behind GMT. The return value is an instance of :drm:`<integer>` (for
     example, -300 represents the offset for EST, which is 5 hours
     behind GMT). The return value incorporates daylight savings time
     when necessary.

.. method:: make
   :specializer: <date>

   Creates an instance of the :class:`<date>` class.

   :signature: make *date-class* #key *iso8601-string year month day hours minutes seconds microseconds time-zone-offset* => *date-instance*

   :parameter date-class: The class :class:`<date>`.
   :parameter #key iso8601-string: An instance of
     ``false-or(<string>)``. Default value: ``#f``.
   :parameter #key year: An instance of ``limited(<integer>, min: 1800,
     max: 2199)``.
   :parameter #key month: An instance of ``limited(<integer>, min: 1,
     max: 12)``.
   :parameter #key day: An instance of ``limited(<integer>, min: 1, max:
     31)``.
   :parameter #key hours: An instance of ``limited(<integer>, min: 0,
     max: 23)``. Default value: 0.
   :parameter #key minutes: An instance of ``limited(<integer>, min: 0,
     max: 59)``. Default value: 0.
   :parameter #key seconds: An instance of ``limited(<integer>, min: 0,
     max: 59)``. Default value: 0.
   :parameter #key microseconds: An instance of ``limited(<integer>,
     min: 0, max: 999999)``. Default value: 0.
   :parameter #key time-zone-offset: An instance of :drm:`<integer>`.
     Default value: 0.
   :value date-instance: An instance of :class:`<date>`.

   :description:

     Creates an instance of :class:`<date>`.

     The make method on :class:`<date>` takes the same
     keywords as the :class:`<date>` class.

     **Note**: The iso8601-string keyword accepts a richer subset of
     the ISO 8601 specification than is produced by the
     :func:`as-iso8601-string` function.

   :example:

     .. code-block:: dylan

       make (<date>, iso8601-string: "19970717T1148-0400")

   See also

   - :class:`<date>`
   - :func:`encode-date`

.. class:: <year/month-duration>
   :sealed:

   The class of objects representing durations with a coarse resolution.

   :superclasses: :class:`<duration>`

   :keyword year: An instance of :drm:`<integer>`.
   :keyword month: An instance of :drm:`<integer>`.

   :description:

     The class of objects representing durations in units of calendar
     years and months. It is a subclass of :class:`<duration>`.

     Use this class to represent a number of calendar years and months.
     If you need to represent durations in units of days or fractions
     thereof (to microsecond resolution), use
     :class:`<day/time-duration>` instead.

   :operations:

     - :meth:`< <<(duration>)>`
     - :meth:`+ <+(<year/month-duration>)>`
     - :meth:`- <-(<year/month-duration>)>`
     - :func:`decode-duration`
     - :func:`encode-year/month-duration`

   See also

   - :class:`<day/time-duration>`
   - :class:`<duration>`
