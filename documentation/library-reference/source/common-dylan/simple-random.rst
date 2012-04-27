************************
The simple-random Module
************************

.. current-library:: common-dylan
.. current-module:: simple-random

Common Dylan provides a simple facility for generating sequences of
pseudo-random integers via the *simple-random* module.

Instances of the sealed class :class:`<random\>` generate pseudo-random
integers. Given an instance of :class:`<random>`, the function
:func:`random` will return a pseudo-random integer.

.. class:: <random>
   :sealed:
   :instantiable:

   The class of random number generators.

   :superclasses: <object>

   :keyword seed: An instance of ``<integer>``. Default value: computed
     to be random.

   :description:

     The class of random number generators.

     The seed value from which to start the sequence of integers. Default
     value: computed to be random.

.. function:: random

   Returns a pseudorandomly generated number greater than or equal to
   zero and less than a specified value.

   :signature: random *upperbound* #key *random* => *random-integer*

   :parameter range: An instance of ``<integer>``.
   :parameter #key random: An instance of :class:`<random>`.
   :value random-integer: An instance of ``<integer>``.

   :description:

     Returns a pseudorandomly generated number greater than or equal to zero
     and less than *range*.
