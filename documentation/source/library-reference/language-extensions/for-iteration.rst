Extensions to the FOR iteration construct
-----------------------------------------

We have also made two extensions to the ``for`` iteration construct: a
``keyed-by`` clause and ``in … using`` clauses.

The ``keyed-by`` clause allows iteration over table elements:

.. code-block:: dylan

    for (my-element keyed-by my-key in my-table)
      ...
    end;

The ``in … using`` clause allows you to specify a iteration protocol
other than the default (:drm:`forward-iteration-protocol`):

.. code-block:: dylan

    for (element in my-sequence using backward-iteration-protocol)
      ...
    end;
