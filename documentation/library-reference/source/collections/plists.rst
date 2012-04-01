*****************
The PLISTS module
*****************

.. current-library:: collections
.. current-module:: plists

.. generic-function:: do-put-property!

   :signature: do-put-property! *plist* *indicator* *value* => *plist*

   :parameter plist: An instance of ``<sequence>``.
   :parameter indicator: An instance of ``<object>``.
   :parameter value: An instance of ``<object>``.
   :value plist: An instance of ``<sequence>``.

.. generic-function:: do-remove-property!

   :signature: do-remove-property! *plist* *indicator* => *value* *plist*

   :parameter plist: An instance of ``<sequence>``.
   :parameter indicator: An instance of ``<object>``.
   :value value: An instance of ``<object>``.
   :value plist: An instance of ``<sequence>``.

.. generic-function:: get-property

   :signature: get-property *plist* *indicator* #key *default* => *property*

   :parameter plist: An instance of ``<sequence>``.
   :parameter indicator: An instance of ``<object>``.
   :parameter #key default: An instance of ``<object>``.
   :value property: An instance of ``<object>``.

.. generic-function:: keyword-sequence

   :signature: keyword-sequence *plist* => *keywords*

   :parameter plist: An instance of ``<sequence>``.
   :value keywords: An instance of ``<sequence>``.

.. macro:: put-property!

.. generic-function:: remove-keywords

   :signature: remove-keywords *plist* *keywords* => *plist*

   :parameter plist: An instance of ``<sequence>``.
   :parameter keywords: An instance of ``<sequence>``.
   :value plist: An instance of ``<sequence>``.

.. macro:: remove-property!

.. generic-function:: value-sequence

   :signature: value-sequence *plist* => *values*

   :parameter plist: An instance of ``<sequence>``.
   :value values: An instance of ``<sequence>``.

.. macro:: with-keywords-removed
