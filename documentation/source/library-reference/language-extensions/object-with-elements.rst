object-with-elements
--------------------

.. current-library:: dylan
.. current-module:: dylan-extensions

In the DRM, the generics :drm:`element` and :drm:`element-setter`
are specified to operate on instances of :drm:`<collection>`.

In the Open Dylan C-FFI, :class:`<C-statically-typed-pointer>` would
be much more useful if :drm:`element` and :drm:`element-setter`
could be used with it, especially via the sugar syntax using ``[]``,
but they needn't support any other collection operations such as
:drm:`do` or :drm:`map`.

To accommodate this, Open Dylan provides 2 additional classes in
the hierarchy provided by the standard library:

- :class:`<object-with-elements>`
- :class:`<mutable-object-with-elements>`

.. class:: <object-with-elements>
   :open:
   :abstract:

   An invented superclass of collections and any other object to
   which :drm:`element` is applicable.

   :superclasses: :drm:`<object>`

   :operations:

     - :drm:`element`

   :seealso:

     - :class:`<mutable-object-with-elements>`

.. class:: <mutable-object-with-elements>
   :open:
   :abstract:

   An invented superclass of mutable collections and any other
   object to which :drm:`element-setter` is applicable.

   :superclasses: :class:`<object-with-elements>`

   :operations:

     - :drm:`element-setter`

   :seealso:

     - :class:`<object-with-elements>`
