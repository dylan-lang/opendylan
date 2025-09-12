.. index:: pair: for; keyed-by
.. index:: pair: for; using

Extensions to the FOR iteration macro
-------------------------------------

We have also made two extensions to the ``for`` iteration construct: a
``keyed-by`` clause and ``in ... using`` clauses.

The ``keyed-by`` clause allows binding a variable to the collection key associated with
each element.  This is particularly useful for iteration over tables or other explicit
key collections:

.. code-block:: dylan

    for (v keyed-by k in collection)
      ...
    end;

The ``in ... using`` clause allows you to specify a iteration protocol
other than the default (:drm:`forward-iteration-protocol`):

.. code-block:: dylan

    for (element in my-sequence using backward-iteration-protocol)
      ...
    end;
