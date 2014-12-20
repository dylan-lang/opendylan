*******************
The locators Module
*******************

.. current-library:: system
.. current-module:: locators

Introduction
------------

The LOCATORS module
-------------------

.. class:: <directory-locator>
   :open:
   :abstract:

   :superclasses: <physical-locator>


.. class:: <file-locator>
   :open:
   :abstract:

   :superclasses: <physical-locator>


.. class:: <locator-error>

   :superclasses: :class:`<format-string-condition>`, :drm:`<error>`


.. class:: <locator>
   :open:
   :abstract:

   :superclasses: <object>


.. constant:: <native-directory-locator>

.. constant:: <native-file-locator>

.. class:: <physical-locator>
   :open:
   :abstract:

   :superclasses: <locator>


.. class:: <server-locator>
   :open:
   :abstract:

   :superclasses: <locator>


.. generic-function:: list-locator
   :open:

   :signature: list-locator (locator) => (locators)

   :parameter locator: An instance of :class:`<locator>`.
   :value locators: An instance of ``<sequence>``.

.. generic-function:: locator-address

   :signature: locator-address (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: locator-as-string
   :open:

   :signature: locator-as-string (class locator) => (string)

   :parameter class: An instance of ``subclass(<string>)``.
   :parameter locator: An instance of :class:`<locator>`.
   :value string: An instance of :drm:`<string>`.

.. generic-function:: locator-base
   :open:

   :signature: locator-base (locator) => (base)

   :parameter locator: An instance of :class:`<locator>`.
   :value base: An instance of ``false-or(<string>)``.

.. generic-function:: locator-directory
   :open:

   :signature: locator-directory (locator) => (directory)

   :parameter locator: An instance of :class:`<locator>`.
   :value directory: An instance of ``false-or(<directory-locator>)``.

.. function:: locator-error

   :signature: locator-error (format-string #rest format-arguments) => (#rest results)

   :parameter format-string: An instance of :drm:`<string>`.
   :parameter #rest format-arguments: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: locator-extension
   :open:

   :signature: locator-extension (locator) => (extension)

   :parameter locator: An instance of :class:`<locator>`.
   :value extension: An instance of ``false-or(<string>)``.

.. generic-function:: locator-file

   :signature: locator-file (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: locator-host
   :open:

   :signature: locator-host (locator) => (host)

   :parameter locator: An instance of :class:`<locator>`.
   :value host: An instance of ``false-or(<string>)``.

.. generic-function:: locator-name

   :signature: locator-name (locator) => (#rest results)

   :parameter locator: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: locator-path
   :open:

   :signature: locator-path (locator) => (path)

   :parameter locator: An instance of :class:`<locator>`.
   :value path: An instance of ``<sequence>``.

.. generic-function:: locator-relative?
   :open:

   :signature: locator-relative? (locator) => (relative?)

   :parameter locator: An instance of :class:`<locator>`.
   :value relative?: An instance of :drm:`<boolean>`.

.. generic-function:: locator-server
   :open:

   :signature: locator-server (locator) => (server)

   :parameter locator: An instance of :class:`<locator>`.
   :value server: An instance of ``false-or(<server-locator>)``.

.. generic-function:: locator-volume
   :open:

   :signature: locator-volume (locator) => (volume)

   :parameter locator: An instance of :class:`<locator>`.
   :value volume: An instance of ``false-or(<string>)``.

.. generic-function:: merge-locators
   :open:

   :signature: merge-locators (locator from-locator) => (merged-locator)

   :parameter locator: An instance of :class:`<physical-locator>`.
   :parameter from-locator: An instance of :class:`<physical-locator>`.
   :value merged-locator: An instance of :class:`<physical-locator>`.

.. generic-function:: open-locator
   :open:

   :signature: open-locator (locator #key #all-keys) => (stream)

   :parameter locator: An instance of :class:`<locator>`.
   :value stream: An instance of :class:`<stream>`.

.. generic-function:: relative-locator
   :open:

   :signature: relative-locator (locator from-locator) => (relative-locator)

   :parameter locator: An instance of :class:`<physical-locator>`.
   :parameter from-locator: An instance of :class:`<physical-locator>`.
   :value relative-locator: An instance of :class:`<physical-locator>`.

.. generic-function:: simplify-locator
   :open:

   :signature: simplify-locator (locator) => (simplified-locator)

   :parameter locator: An instance of :class:`<physical-locator>`.
   :value simplified-locator: An instance of :class:`<physical-locator>`.

.. generic-function:: string-as-locator
   :open:

   :signature: string-as-locator (class string) => (locator)

   :parameter class: An instance of ``subclass(<locator>)``.
   :parameter string: An instance of :drm:`<string>`.
   :value locator: An instance of :class:`<locator>`.

.. generic-function:: subdirectory-locator
   :open:

   :signature: subdirectory-locator (locator #rest sub-path) => (subdirectory)

   :parameter locator: An instance of :class:`<directory-locator>`.
   :parameter #rest sub-path: An instance of ``<object>``.
   :value subdirectory: An instance of :class:`<directory-locator>`.

.. generic-function:: supports-list-locator?
   :open:

   :signature: supports-list-locator? (locator) => (listable?)

   :parameter locator: An instance of :class:`<locator>`.
   :value listable?: An instance of :drm:`<boolean>`.

.. generic-function:: supports-open-locator?
   :open:

   :signature: supports-open-locator? (locator) => (openable?)

   :parameter locator: An instance of :class:`<locator>`.
   :value openable?: An instance of :drm:`<boolean>`.

