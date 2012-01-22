************************
The Simple Random module
************************

.. current-library:: common-dylan
.. current-module:: simple-random

Common Dylan provides a simple facility for generating sequences of
pseudo-random integers via the *simple-random* module.

Instances of the sealed class `\<random\>`_
generate pseudo-random integers. Given an instance of ``<random>``, the
function `random`_ will return a
pseudo-random integer.

<random>
--------

Sealed instantiable class
'''''''''''''''''''''''''

Summary

The class of random number generators.

Superclasses

<object>

Init-keywords

*seed* An instance of ``<integer>``. Default value: computed to be
random.

Description

The class of random number generators.

The seed value from which to start the sequence of integers. Default
value: computed to be random.

Example

random
------

Function
''''''''

Summary

Returns a pseudorandomly generated number greater than or equal to zero
and less than a specified value.

Signature

random *upperbound* #key *random* => *random-integer*

Arguments

*range* An instance of ``<integer>``.

*random* An instance of ``<random>``.

Values

*random-integer*

An instance of ``<integer>``.

Description

Returns a pseudorandomly generated number greater than or equal to zero
and less than *range*.

