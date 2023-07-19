*******************
The t-lists Library
*******************

.. current-library:: t-lists

The t-lists Module
******************

.. current-module:: t-lists

.. class:: <t-list>
   :open:
   :abstract:
   :primary:

   :superclasses: :drm:`<deque>`

   :keyword first-pair: An instance of :drm:`<list>`.
   :keyword last-pair: An instance of :drm:`<list>`.

   :description:

     The `t-lists` library is an implementation of a tail concatenate
     list that supports efficient append operations.

     The `<t-list>` maintains a reference to the last object in the
     list. This allows appends to happen in O(1) time.

     In the Lisp world, this data structure is known as `tconc`.

   :operations:

     * :gf:`concatenate!`
     * :drm:`empty?`
     * :drm:`last`
     * :drm:`pop`
     * :drm:`pop-last`
     * :drm:`push`
     * :drm:`push-last`

