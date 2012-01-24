***************
The Date Module
***************

.. current-library:: system
.. current-module:: date

Introduction
------------

The Date module is part of the System library and provides a
machine-independent facility for representing and manipulating dates and
date/time intervals.

This chapter describes the classes, types, and functions that the Date
module contains.

Representing dates and times
----------------------------

The Date module contains a single class, ``<date>``, an instance of which
can represent any date between 1 Jan 1800 00:00:00 and 31 Dec 2199
23:59:59, Greenwich Mean Time. You can create a date object by calling
the function *encode-date* or using the *make* method for ``<date>``.

<date>

Sealed class

The class of all date objects. It is a subclass of ``<number>``. Note
that there is no method for *make* defined on ``<date>``. The function
*encode-date* should be used instead.

make *date-class*

G.f. method

make *date-class* #key *iso8601-string year month day hours minutes
* *seconds microseconds time-zone-offset*
=> *date-instance*

Creates an instance of the `<date>`_ class.

encode-date

Function

encode-date *year month day hours minutes seconds
* #key*microseconds time-zone-offset* => *date*

Creates a ``<date>`` object from a set of ``<integer>`` values.

Each of the arguments to *encode-date* and to the *make* method on
``<date>`` is an instance of ``<integer>`` (except for the *iso8601-string*
keyword for the *make* method, which is a string) that is passed as an
init-keyword value to the ``<date>`` object. Each init-keyword takes an
instance of ``<integer>``, limited to the natural range for that
attribute. For example, *month:* can only take values between 1 and 12.

You must specify values, via *encode-date*, for at least the *year:*,
*month:*, and *day:* init-keywords. In addition, you can also specify
values for *hours:*, *minutes:*, *seconds:*, *microseconds:*, and
*time-zone-offset:*. If not supplied, the default value for each of
these init-keywords is 0.

The *time-zone-offset:* init-keyword is used to represent time zones in
the Date module as ``<integer>`` values representing the offset in minutes
from Greenwich Mean Time (GMT). Positive values are used for time zones
East of Greenwich; negative values represent time zones to the west of
Greenwich.

For example, the value -300 (-5 hours) is U.S. Eastern Standard Time and
the value -240 (-4 hours) is U.S. Eastern Daylight Savings Time.

If you wish, a ``<date>`` can be specified completely by using the
*iso8601-string:* init-keyword. This init-keyword takes an instance of
``<string>``, which should be a valid ISO8601 format date. If you use the
*iso8601-string:* init-keyword, there is no need to specify any other
init-keywords to a call to *make* on ``<date>``.

current-date

Function

current-date () => *date*

Returns the current date on your local machine as a ``<date>`` object.

<day-of-week>

Type

one-of(#"Sunday", #"Monday", #"Tuesday", #"Wednesday",
 #"Thursday", #"Friday", #"Saturday")

Days of the week can be represented using the ``<day-of-week>`` type.

You can extract the day of the week of a specified date using
*date-day-of-week*. See `Extracting information from
dates`_ for details.

Representing durations
----------------------

Date/time intervals, called durations, are modeled in a style quite
similar to that of SQL. There are two, effectively disjoint, classes of
duration: one with a resolution of months (for example, 3 years, 2
months) and the other with a resolution of microseconds (for example, 50
days, 6 hours, 23 minutes). The first is ``<year/month-duration>`` and the
second ``<day/time-duration>``.

An important distinction between ``<day/time-duration>`` and
``<year/month-duration>`` is that a given instance of
``<day/time-duration>`` is always a fixed unit of a fixed length, whereas
a ``<year/month-duration>`` follows the vagaries of the calendar. So if
you have a ``<date>`` that represents, for example, the 5th of some month,
adding a ``<year/month-duration>`` of 1 month to that will always take you
to the 5th of the following month, whether that is an interval of 28,
29, 30, or 31 days.

<duration>

Sealed abstract instantiable class

This class is the used to represent durations. It is a subclass of
``<number>``, and it has two subclasses, described below.

<year/month-duration>

Sealed class

Represents durations in units of calendar months. It is a subclass of
``<duration>``.

<day/time-duration>

Sealed class

Represents durations in units of microseconds. It is a subclass of
``<duration>``.

The following functions and methods are available for creating
durations, and decoding them into their constituent integer parts.

encode-year/month-duration

Function

encode-year/month-duration *years* *months* => *duration*

Creates an instance of ``<year/month-duration>``.

encode-day/time-duration

Function

encode-day/time-duration *days* *hours* *minutes* *seconds*
*microseconds*
=> *duration*

Creates an instance of ``<day/time-duration>``.

decode-duration

Sealed generic function

decode-duration *duration* => #rest *components*

Decodes an instance of ``<duration>`` into its constituent parts. There
are methods for this generic function that specialize on
``<year/month-duration>`` and ``<day/time-duration>`` respectively, as
described below.

decode-duration

Sealed method

decode-duration *duration* => *years* *months*

Decodes an instance of ``<year/month-duration>`` into its constituent
parts.

decode-duration

Sealed method

decode-duration *duration* => *days* *hours* *minutes* *seconds*
*microseconds*

Decodes an instance of ``<day/time-duration>`` into its constituent parts.

Performing operations on dates and durations
--------------------------------------------

A number of interfaces are exported from the Date module that let you
perform other operations on dates and durations, and extract
date-specific information from your local machine.

Comparing dates
^^^^^^^^^^^^^^^

The following operations are exported from the Date module.

=

Sealed method

<

Sealed method

*date1* = *date2* => *equal?*

*date1* < *date2* => *before?*

These methods let you perform arithmetic-like operations on dates to
test for equality, or to test whether one date occurred before another.

Comparing durations
^^^^^^^^^^^^^^^^^^^

The following operations are exported from the Date module.

=

Sealed method

<

Sealed method

*duration1* = *duration2* => *equal?*

*duration1* < *duration2* => *less-than?*

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
instances of `<year/month-duration>`_ can cause
errors where the result is a date that does not exist in the calendar.
For example, adding one month to January 30th.

The table below summarizes the methods defined for each arithmetic
operation, for different combinations of date and duration arguments,
together with their return values.

Methods defined for arithmetic operations on dates and durations

Op

Argument 1

Argument 2

Return value

#. *+*

#. ``<duration>``

#. ``<duration>``

#. ``<duration>``

#. *+*

#. ``<year/month-duration>``

#. ``<year/month-duration>``

#. ``<year/month-duration>``

#. *+*

#. ``<day/time-duration>``

#. ``<day/time-duration>``

#. ``<day/time-duration>``

#. *+*

#. ``<date>``

#. ``<duration>``

#. ``<date>``

#. *+*

#. ``<duration>``

#. ``<date>``

#. ``<date>``

#. *-*

#. ``<duration>``

#. ``<duration>``

#. ``<duration>``

#. *-*

#. ``<year/month-duration>``

#. ``<year/month-duration>``

#. ``<year/month-duration>``

#. *-*

#. ``<day/time-duration>``

#. ``<day/time-duration>``

#. ``<day/time-duration>``

#. *-*

#. ``<date>``

#. ``<duration>``

#. ``<date>``

#. *-*

#. ``<date>``

#. ``<date>``

#. ``<day/time-duration>``

#. *\**

#. ``<duration>``

#. ``<real>``

#. ``<duration>``

#. *\**

#. ``<real>``

#. ``<duration>``

#. ``<duration>``

#. */*

#. ``<duration>``

#. ``<real>``

#. ``<duration>``

Dealing with time-zones
^^^^^^^^^^^^^^^^^^^^^^^

The following functions return information about the time-zone that the
host machine is in.

local-time-zone-name

Function

local-time-zone-name () => *time-zone-name*

Returns the name of the time-zone that the local computer is in. The
name is returned as a string (for example, *"EST"*).

local-time-zone-offset

Function

local-time-zone-offset () => *time-zone-offset*

Returns the offset of the time-zone from Greenwich Mean Time, expressed
as a number of minutes. A positive number represents an offset ahead of
GMT, and a negative number represents an offset behind GMT. The return
value is an instance of ``<integer>`` (for example, -300 represents the
offset for EST, which is 5 hours behind GMT). The return value
incorporates daylight savings time when necessary.

local-daylight-savings-time?

Function

local-daylight-savings-time? () => *dst?*

Returns ``#t`` if the local computer is using Daylight Savings Time.

Extracting information from dates
---------------------------------

A number of functions are available to return discrete pieces of
information from a specified ``<date>`` object. These are useful to allow
you to deconstruct a given date in order to retrieve useful information
from it.

The most basic way to extract information from a date is to use the
function *decode-date*.

decode-date

Function

decode-date *date* => *year month day hours minutes seconds
* *day-of-week time-zone-offset*

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

`date-year`_

`date-month`_

`date-day`_

`date-day-of-week`_

`date-hours`_

`date-minutes`_

`date-seconds`_

`date-microseconds`_

`date-time-zone-offset`_

For each function except *date-day-of-week*, the value returned is an
instance of ``<integer>``. The *date-day-of-week* function returns an
object of type ``<day-of-week>``. For more information, please refer to
the reference entries of each function. See also the function
`date-time-zone-offset-setter`_, which allows you to set
the time-zone offset of a ``<date>`` explicitly.

To return an ISO 8601 format date from a ``<date>`` object, use the
function *as-iso8601-string*.

as-iso8601-string

Function

as-iso8601-string *date* #key *precision* => *iso8601-string*

Returns an instance of ``<string>`` representing a date in ISO 8601
format. The *precision* keyword, if present, is an integer representing
the number of decimal places to which the second should be specified in
the result.

The DATE module
---------------

This section contains a reference entry for each item exported from the
Date module.

=
^^

Sealed method

Summary

Compares two dates for equality.

Signature

*date1* = *date2* => *equal?*

Arguments

-  *date1* An instance of `<date>`_.
-  *date2* An instance of `<date>`_.

Values

-  *equal?* An instance of ``<boolean>``.

Description

This method lets you compare two dates to see if they are equal. Any
differences in microseconds between *date1* and *date2* are ignored.

See also

`<`_

=
^^

Sealed method

Summary

Compares two durations for equality.

Signature

*duration1* = *duration2* => *equal?*

Arguments

-  *duration1* An instance of `<duration>`_.
-  *duration2* An instance of `<duration>`_.

Values

-  *equal?* An instance of ``<boolean>``.

Description

This method lets you compare two durations to see if they are equal. If
the durations are actually instances of
`<day/time-duration>`_, any differences in microseconds
between *duration1* and *duration2* are ignored.

See also

`<`_

<
^^

Sealed method

Summary

Determines whether one date is earlier than another.

Signature

*date1* < *date2* => *before?*

Arguments

-  *date1* An instance of `<date>`_.
-  *date2* An instance of `<date>`_.

Values

-  *before?* An instance of ``<boolean>``.

Description

This method determines if *date1* is earlier than *date2*. Any
differences in microseconds between *date1* and *date2* are ignored.

See also

`=`_

<
^^

Sealed method

Summary

Determines whether one duration is less than another.

Signature

*duration1* < *duration2* => *less-than?*

Arguments

-  *duration1* An instance of `<duration>`_.
-  *duration2* An instance of `<duration>`_.

Values

-  *less-than?* An instance of ``<boolean>``.

Description

This method determines if *duration1* is less than *duration2*. If
the durations are actually instances of `<day/time-duration>`_, any
differences in microseconds between *duration1* and *duration2* are
ignored.

See also

`=`_

+
^^

Sealed methods

Summary

Performs addition on specific combinations of dates and durations.

Signature

+ *arg1* *arg2* => *sum*

Arguments

-  *arg1* An instance of `<date>`_ or
   `<duration>`_. See description for details.
-  *arg2* An instance of `<date>`_ or
   `<duration>`_. See description for details.

Values

-  *sum* An instance of `<date>`_ or
   `<duration>`_. See description for details.

Description

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

See also

`-`_

`\*`_

`/`_

-
^^

Sealed methods

Summary

Performs subtraction on specific combinations of dates and durations.

Signature

- *arg1* *arg2* => *diff*

Arguments

-  *arg1* An instance of `<date>`_ or
   `<duration>`_. See description for details.
-  *arg2* An instance of `<duration>`_, or an
   instance of `<date>`_ if *arg1* is a ``<date>``.
   See description for details.

Values

-  *diff* An instance of `<date>`_ or
   `<duration>`_. See description for details.

Description

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

See also

`+`_

`\*`_

`/`_

\*
^^

Sealed methods

Summary

Multiplies a duration by a scale factor.

Signature

\* *duration* *scale* => *new-duration
* \* *scale* *duration* => *new-duration*

Arguments

-  *duration* An instance of `<duration>`_.
-  *scale* An instance of ``<real>``.

*Note:* These arguments can be expressed in any order.

Values

-  *new-duration* An instance of `<date>`_ or
   `<duration>`_. See description for details.

Description

Multiples a duration by a scale factor and returns the result. Note that
the arguments can be expressed in any order: methods are defined such
that the duration can be placed first or second in the list of
arguments.

See also

`+`_

`-`_

`/`_

/
^^

Sealed methods

Summary

Divides a duration by a scale factor

Signature

/ *duration* *scale* => *new-duration*

Arguments

-  *duration* An instance of `<duration>`_.
-  *scale* An instance of ``<real>``.

Values

-  *new-duration* An instance of `<date>`_ or
   `<duration>`_. See description for details.

Description

A number of methods are defined for the + generic function to allow
summing of various combinations of dates and durations. Note that there
is not a method defined for every possible combination of date and
duration. Specifically, you cannot sum different types of duration, and
you cannot sum two dates. The return value can be either a date or a
duration, depending on the arguments supplied. The table below lists the
methods that are defined on +.

See also

`+`_

`-`_

`\*`_

as-iso8601-string
^^^^^^^^^^^^^^^^~

Function

Summary

Returns a string representation of a date, conforming to the ISO 8601
standard.

Signature

as-iso8601-string *date* #key *precision* => *iso8601-string*

Arguments

-  *date* An instance of ``<date>``.
-  *precision* An instance of ``<integer>``. Default value: 0.

Values

-  *iso8601-string* An instance of ``<string>``.

Description

Returns a string representation of *date* using the format identified by
International Standard ISO 8601 (for example, *"19960418T210634Z"*). If
*precision* is non-zero, the specified number of digits of a fraction of
a second are included in the string (for example,
*"19960418T210634.0034Z"*).

The returned string always expresses the time in Greenwich Mean Time.
The *iso8601-string* init-keyword for `<date>`_,
however, accepts ISO 8601 strings with other time zone specifications.

See also

`<date>`_

current-date
^^^^^^^^^^^^

Function

Summary

Returns a date object representing the current date and time.

Signature

current-date () => *date*

Arguments

-  None.

Values

-  *date* An instance of ``<date>``.

Description

Returns *date* for the current date and time.

<date>
^^^^^^

Sealed class

Summary

The class of objects representing dates.

Superclasses

``<number>``

Init-keywords

-  *iso8601-string* An instance of *false-or(<string>)*. Default value:
   ``#f``.
-  *year* An instance of *limited(<integer>, min: 1800, max: 2199)*.
-  *month* An instance of *limited(<integer>, min: 1, max: 12)*.
-  *day* An instance of *limited(<integer>, min: 1, max: 31)*.
-  *hours* An instance of *limited(<integer>, min: 0, max: 23)*.
   Default value: 0.
-  *minutes* An instance of *limited(<integer>, min: 0, max: 59)*.
   Default value: 0.
-  *seconds* An instance of *limited(<integer>, min: 0, max: 59)*.
   Default value: 0.
-  *microseconds* An instance of *limited(<integer>, min: 0, max:
   999999)*. Default value: 0.
-  *time-zone-offset* An instance of ``<integer>``. Default value: 0.

Description

Represents a date and time between 1 Jan 1800 00:00:00 and 31 Dec 2199
23:59:59, Greenwich Mean Time (GMT).

A ``<date>`` can be specified to microsecond precision and includes a time
zone indication.

If supplied, the *iso8601-string:* init-keyword completely specifies the
value of the ``<date>``. Otherwise, the *year:*, *month:*, and *day:*
init-keywords must be supplied. Note that, although you can supply ISO
8601 strings that represent any time zone specification, the related
function `as-iso8601-string`_ always returns an ISO
8601 string representing a time in Greenwich Mean Time.

For the *time-zone-offset* init-keyword, a positive number represents an
offset ahead of GMT, in minutes, and a negative number represents an
offset behind GMT. The value returned is an instance of ``<integer>`` (for
example, -300 represents the offset for EST, which is 5 hours behind
GMT).

Operations

`=`_ `<`_
`+`_ `-`_
`as-iso8601-string`_
`current-date`_ `date-day`_
`date-day-of-week`_
`date-hours`_ `date-microseconds`_
`date-minutes`_ `date-month`_
`date-seconds`_
`date-time-zone-offset`_
`date-time-zone-offset-setter`_
`date-year`_ `decode-date`_

See also

`as-iso8601-string`_

`<day-of-week>`_

date-day
^^^^^^^^

Function

Summary

Returns the day of the month component of a specified date.

Signature

date-day *date* => *day*

Arguments

-  *date* An instance of ``<date>``.

Values

-  *day* An instance of ``<integer>``.

Description

Returns the day of the month component of the specified *date*. For
example, if passed a `<date>`_ that represented
16:36 on the 20th June, 1997, *date-day* returns the value 20.

See also

`decode-date`_

`date-month`_

`date-year`_

`date-hours`_

`date-minutes`_

`date-seconds`_

`date-microseconds`_

`date-time-zone-offset`_

`date-day-of-week`_

date-day-of-week
^^^^^^^^^^^^^^^^

Function

Summary

Returns the day of the week of a specified date.

Signature

date-day-of-week *date* => *day-of-week*

Arguments

-  *date* An instance of ``<date>``.

Values

-  *day-of-week* An object of type ``<day-of-week>``.

Description

Returns the day of the week of the specified *date*.

See also

`decode-date`_

`date-month`_

`date-year`_

`date-hours`_

`date-minutes`_

`date-seconds`_

`date-microseconds`_

`date-time-zone-offset`_

`date-day`_

`<day-of-week>`_

date-hours
^^^^^^^^^^

Function

Summary

Returns the hour component of a specified date.

Signature

date-hours *date* => *hour*

Arguments

-  *date* An instance of ``<date>``.

Values

-  *hour* An instance of ``<integer>``.

Description

Returns the hour component of the specified *date*. This component is
always expressed in 24 hour format.

See also

`decode-date`_

`date-month`_

`date-day`_

`date-year`_

`date-minutes`_

`date-seconds`_

`date-microseconds`_

`date-time-zone-offset`_

`date-day-of-week`_

date-microseconds
^^^^^^^^^^^^^^^^~

Function

Summary

Returns the microseconds component of a specified date.

Signature

date-microseconds *date* => *microseconds*

Arguments

-  *date* An instance of ``<date>``.

Values

-  *microseconds* An instance of ``<integer>``.

Description

Returns the microseconds component of the specified *date*. Note that
this does *not* return the entire date object, represented as a number
of microseconds; it returns any value assigned to the *microseconds:*
init-keyword when the `<date>`_ object was created.

See also

`decode-date`_

`date-month`_

`date-day`_

`date-hours`_

`date-minutes`_

`date-seconds`_

`date-year`_

`date-time-zone-offset`_

`date-day-of-week`_

date-minutes
^^^^^^^^^^^^

Function

Summary

Returns the minutes component of a specified date.

Signature

date-minutes *date* => *minutes*

Arguments

-  *date* An instance of ``<date>``.

Values

-  *minutes* An instance of ``<integer>``.

Description

Returns the minutes component of the specified *date*.

See also

`decode-date`_

`date-month`_

`date-day`_

`date-hours`_

`date-year`_

`date-seconds`_

`date-microseconds`_

`date-time-zone-offset`_

`date-day-of-week`_

date-month
^^^^^^^^^^

Function

Summary

Returns the month of a specified date.

Signature

date-month *date* => *month*

Arguments

-  *date* An instance of ``<date>``.

Values

-  *month* An instance of ``<integer>``.

Description

Returns the month of the specified *date*.

See also

`decode-date`_

`date-year`_

`date-day`_

`date-hours`_

`date-minutes`_

`date-seconds`_

`date-microseconds`_

`date-time-zone-offset`_

`date-day-of-week`_

date-seconds
^^^^^^^^^^^^

Function

Summary

Returns the seconds component of a specified date.

Signature

date-seconds *date* => *seconds*

Arguments

-  *date* An instance of ``<date>``.

Values

-  *seconds* An instance of ``<integer>``.

Description

Returns the seconds component of the specified *date*. Note that this
does *not* return the entire date object, represented as a number of
seconds; it returns any value assigned to the *seconds:* init-keyword
when the `<date>`_ object was created.

See also

`decode-date`_

`date-month`_

`date-day`_

`date-hours`_

`date-minutes`_

`date-year`_

`date-microseconds`_

`date-time-zone-offset`_

`date-day-of-week`_

date-time-zone-offset
^^^^^^^^^^^^^^^^^^^^~

Function

Summary

Returns the time zone offset of a specified date.

Signature

date-time-zone-offset *date* => *time-zone-offset*

Arguments

-  *date* An instance of ``<date>``.

Values

-  *time-zone-offset* An instance of ``<integer>``.

Description

Returns the time zone offset of the specified *date*. The values of the
other components of *date* reflect this time zone.

A positive number represents an offset ahead of GMT, in minutes, and a
negative number represents an offset behind GMT. The value returned is
an instance of ``<integer>`` (for example, -300 represents the offset for
EST, which is 5 hours behind GMT).

See also

`decode-date`_

`date-month`_

`date-day`_

`date-hours`_

`date-minutes`_

`date-seconds`_

`date-year`_

`date-microseconds`_

`date-time-zone-offset-setter`_

`date-day-of-week`_

date-time-zone-offset-setter
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Function

Summary

Change the time zone offset of a specified date, while maintaining the
same point in time.

Signature

date-time-zone-offset-setter *new-time-zone-offset* *date*
 => *new-time-zone-offset*

Arguments

*new-time-zone-offset*

An instance of ``<integer>``.

-  *date* An instance of ``<date>``.

Values

*new-time-zone-offset*

An instance of ``<integer>``.

Description

Changes the time zone offset of *date* without changing the actual point
in time identified by the *date*. The values of the other components of
*date* are adjusted to reflect the new time zone.

The *new-time-zone-offset* argument should represent the offset from
GMT, in minutes. Thus, if you wish to specify a new offset representing
EST, which is 5 hours behind GMT, *new-time-zone-offset* should have the
value -300.

See also

`date-time-zone-offset`_

date-year
^^^^^^^^~

Function

Summary

Returns the year of a specified date.

Signature

date-year *date* => *year*

Arguments

-  *date* An instance of ``<date>``.

Values

-  *year* An instance of ``<integer>``.

Description

Returns the year of the specified *date*.

See also

`decode-date`_

`date-month`_

`date-day`_

`date-hours`_

`date-minutes`_

`date-seconds`_

`date-microseconds`_

`date-time-zone-offset`_

`date-day-of-week`_

<day-of-week>
^^^^^^^^^^^^~

Type

Summary

The days of the week.

Equivalent

one-of(#"Sunday", #"Monday", #"Tuesday", #"Wednesday",
 #"Thursday", #"Friday", #"Saturday")

Supertypes

None.

Init-keywords

-  None.

Description

The days of the week. This is the type of the return value of the
`date-day-of-week`_ function.

Operations

`date-day-of-week`_

See also

`date-day-of-week`_

<day/time-duration>
^^^^^^^^^^^^^^^^^^~

Sealed class

Summary

The class of objects representing durations in units of microseconds.

Superclasses

`<duration>`_

Init-keywords

-  *days* An instance of ``<integer>``.
-  *hours* An instance of ``<integer>``. Default value: 0.
-  *minutes* An instance of ``<integer>``. Default value: 0.
-  *seconds* An instance of ``<integer>``. Default value: 0.
-  *microseconds* An instance of ``<integer>``. Default value: 0.

Description

The class of objects representing durations in units of microseconds. It
is a subclass of `<duration>`_.

Use this class to represent a number of days and fractions thereof. If
you need to represent durations in calendar units of months or years,
use `<year/month-duration>`_ instead.

Operations

`<`_ `+`_
`-`_ `decode-duration`_
`encode-day/time-duration`_

See also

`<duration>`_

`<year/month-duration>`_

decode-date
^^^^^^^^^^~

Function

Summary

Returns the date and time stored in a date object.

Signature

decode-date *date*
 => *year month day hours minutes seconds day-of-week time-zone-offset*

Arguments

-  *date* An instance of ``<date>``.

Values

-  *year* An instance of ``<integer>``.
-  *month* An instance of ``<integer>``.
-  *day* An instance of ``<integer>``.
-  *hours* An instance of ``<integer>``.
-  *minutes* An instance of ``<integer>``.
-  *seconds* An instance of ``<integer>``.
-  *day-of-week* An instance of ``<day-of-week>``.
-  *time-zone-offset* An instance of ``<integer>``.

Description

Returns the date and time stored in *date*. Note that it does not
return the millisecond component of a ``<date>``, but it does return the
appropriate ``<day-of-week>``.

See also

`encode-date`_

decode-duration
^^^^^^^^^^^^^^~

Sealed generic function

Summary

Decodes a duration into its constituent parts.

Signature

decode-duration *duration* => #rest *components*

Arguments

-  *duration* An instance of `<duration>`_.

Values

-  *components* Instances of ``<integer>``.

Description

Decodes an instance of ``<duration>`` into its constituent parts. There
are methods for this generic function that specialize on
``<year/month-duration>`` and ``<day/time-duration>`` respectively, as
described below.

See also

`decode-duration`_

`decode-duration`_

decode-duration
^^^^^^^^^^^^^^~

Sealed method

Summary

Decodes a day/time duration into its constituent parts.

Signature

decode-duration *duration*
 => *days* *hours* *minutes* *seconds* *microseconds*

Arguments

-  *duration* An instance of `<day/time-duration>`_.

Values

-  *days* An instance of ``<integer>``.
-  *hours* An instance of ``<integer>``.
-  *minutes* An instance of ``<integer>``.
-  *seconds* An instance of ``<integer>``.
-  *microseconds* An instance of ``<integer>``.

Description

Decodes an instance of ``<day/time-duration>`` into its constituent parts.

See also

`decode-duration`_

`decode-duration`_

`encode-day/time-duration`_

decode-duration
^^^^^^^^^^^^^^~

Sealed method

Summary

Decodes a year/month duration into its constituent parts.

Signature

decode-duration *duration* => *years* *months*

Arguments

-  *duration* An instance of `<year/month-duration>`_.

Values

-  *years* An instance of ``<integer>``.
-  *months* An instance of ``<integer>``.

Description

Decodes an instance of ``<year/month-duration>`` into its constituent
parts.

See also

`decode-duration`_

`decode-duration`_

`encode-year/month-duration`_

<duration>
^^^^^^^^^^

Sealed abstract instantiable class

Summary

The class of objects representing durations.

Superclasses

``<number>``

Init-keywords

-  *iso8601-string* An instance of *false-or(<string>)*. Default value: ``#f``.
-  *year* An instance of *limited(<integer>, min: 1800, max: 2199)*.
-  *month* An instance of *limited(<integer>, min: 1, max: 12)*.
-  *day* An instance of *limited(<integer>, min: 1, max: 31)*.
-  *hours* An instance of *limited(<integer>, min: 0, max: 23)*.
   Default value: 0.
-  *minutes* An instance of *limited(<integer>, min: 0, max: 59)*.
   Default value: 0.
-  *seconds* An instance of *limited(<integer>, min: 0, max: 59)*.
   Default value: 0.
-  *microseconds* An instance of *limited(<integer>, min: 0, max:
   999999)*. Default value: 0.
-  *time-zone-offset* An instance of ``<integer>``. Default value: 0.

Description

This class is the used to represent durations. It is a subclass of
``<number>``, and it has two subclasses.

Operations

`=`_ `<`_
`+`_ `-`_
`\*`_ `/`_

See also

`<day/time-duration>`_

`<year/month-duration>`_

encode-date
^^^^^^^^^^~

Function

Summary

Creates a date object for the specified date and time.

Signature

encode-date *year month day hours minutes seconds
* #key*microseconds time-zone-offset* => *date*

Arguments

-  *year* An instance of ``<integer>``.
-  *month* An instance of ``<integer>``.
-  *day* An instance of ``<integer>``.
-  *hours* An instance of ``<integer>``.
-  *minutes* An instance of ``<integer>``.
-  *seconds* An instance of ``<integer>``.
-  *microseconds* An instance of ``<integer>``. Default value: 0.
-  *time-zone-offset* An instance of ``<integer>``. Default value:
   *local-time-zone-offset()*.

Values

-  *date* An instance of ``<date>``.

Description

Creates a ``<date>`` object for the specified date and time.

See also

`decode-date`_

`local-time-zone-offset`_

`make date-class`_

encode-day/time-duration
^^^^^^^^^^^^^^^^^^^^^^^^

Function

Summary

Creates a day/time duration from a set of integer values.

Signature

encode-day/time-duration *days* *hours* *minutes* *seconds*
*microseconds* => *duration*

Arguments

-  *days* An instance of ``<integer>``.
-  *hours* An instance of ``<integer>``.
-  *minutes* An instance of ``<integer>``.
-  *seconds* An instance of ``<integer>``.
-  *microseconds* An instance of ``<integer>``.

Values

-  *duration* An instance of `<day/time-duration>`_.

Description

Creates an instance of ``<day/time-duration>``.

See also

`decode-duration`_

`encode-year/month-duration`_

encode-year/month-duration
^^^^^^^^^^^^^^^^^^^^^^^^^^

Function

Summary

Creates a year/month duration from a set of integer values.

Signature

encode-year/month-duration *years* *months* => *duration*

Arguments

-  *years* An instance of ``<integer>``.
-  *months* An instance of ``<integer>``.

Values

-  *duration* An instance of `<year/month-duration>`_.

Description

Creates an instance of ``<year/month-duration>``.

See also

`decode-duration`_

`encode-day/time-duration`_

local-daylight-savings-time?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Function

Summary

Checks whether the local machine is using Daylight Savings Time.

Signature

local-daylight-savings-time? () => *dst?*

Arguments

-  None.

Values

-  *dst?* An instance of ``<boolean>``.

Description

Returns ``#t`` if the local machine is using Daylight Savings Time, and
``#f`` otherwise.

local-time-zone-name
^^^^^^^^^^^^^^^^^^^^

Function

Summary

Returns the time zone name in use by the local machine.

Signature

local-time-zone-name () => *time-zone-name*

Arguments

-  None.

Values

-  *time-zone-name* An instance of ``<string>``.

Description

Returns the time zone name in use by the local machine, if available, or
a string of the form *+/-HHMM* if the time zone name is unknown.

local-time-zone-offset
^^^^^^^^^^^^^^^^^^^^^^

Function

Summary

Returns the offset of the time-zone from Greenwich Mean Time, expressed
as a number of minutes.

Signature

local-time-zone-offset () => *time-zone-offset*

Arguments

-  None.

Values

-  *time-zone-offset* An instance of ``<integer>``.

Description

Returns the offset of the time-zone from Greenwich Mean Time, expressed
as a number of minutes. A positive number represents an offset ahead of
GMT, and a negative number represents an offset behind GMT. The return
value is an instance of ``<integer>`` (for example, -300 represents the
offset for EST, which is 5 hours behind GMT). The return value
incorporates daylight savings time when necessary.

make *date-class*
^^^^^^^^^^^^^^^^~

G.f. method

Summary

Creates an instance of the `<date>`_ class.

Signature

make *date-class* #key *iso8601-string year month day hours minutes
* *seconds microseconds time-zone-offset*
=> *date-instance*

Arguments

-  *date-class* The class `<date>`_.
-  *iso8601-string* An instance of *false-or(<string>)*. Default value:
   ``#f``.
-  *year* An instance of *limited(<integer>, min: 1800, max: 2199)*.
-  *month* An instance of *limited(<integer>, min: 1, max: 12)*.
-  *day* An instance of *limited(<integer>, min: 1, max: 31)*.
-  *hours* An instance of *limited(<integer>, min: 0, max: 23)*.
   Default value: 0.
-  *minutes* An instance of *limited(<integer>, min: 0, max: 59)*.
   Default value: 0.
-  *seconds* An instance of *limited(<integer>, min: 0, max: 59)*.
   Default value: 0.
-  *microseconds* An instance of *limited(<integer>, min: 0, max:
   999999)*. Default value: 0.
-  *time-zone-offset* An instance of ``<integer>``. Default value: 0.

Values

-  *date-instance* An instance of `<date>`_.

Description

Creates an instance of `<date>`_.

The make method on `<date>`_ takes the same
keywords as the `<date>`_ class.

*Note:* The iso8601-string keyword accepts a richer subset of the ISO
8601 specification than is produced by the `as-iso8601-string`_ function.

Example

.. code-block:: dylan

    make (<date>, iso8601-string: "19970717T1148-0400")

See also

`<date>`_

`encode-date`_

<year/month-duration>
^^^^^^^^^^^^^^^^^^^^~

Sealed class

Summary

The class of objects representing durations with a coarse resolution.

Superclasses

`<duration>`_

Init-keywords

-  *year* An instance of ``<integer>``.
-  *month* An instance of ``<integer>``.

Description

The class of objects representing durations in units of calendar years
and months. It is a subclass of `<duration>`_.

Use this class to represent a number of calendar years and months. If
you need to represent durations in units of days or fractions thereof
(to microsecond resolution), use `<day/time-duration>`_ instead.

Operations

`<`_ `+`_
`-`_ `decode-duration`_
`encode-year/month-duration`_

See also

`<day/time-duration>`_

`<duration>`_


