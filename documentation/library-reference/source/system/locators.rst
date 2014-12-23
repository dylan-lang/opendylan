*******************
The locators Module
*******************

.. current-library:: system
.. current-module:: locators

Introduction
------------

The Locators module provides Dylan programs with a portable, flexible,
and uniform facility for locating files.

The LOCATORS module
-------------------

.. class:: <locator>
   :open:
   :abstract:

   :superclasses: :drm:`<object>`

   This is the base class for all locators. This is the usual locator
   class for coercion (using ``as``) or instantiation (using ``make``)
   of new locators. Situations where this class is not appropriate
   are ones where there is not enough information provided to select
   the appropriate concrete class. For example, it is not appropriate
   to coerce a string representing a portion of a URL without a scheme,
   such as ``as(<locator>, "toothpaste.html")``, because this would
   likely result in the instantiation of a native locator instead of
   the desired URL locator class.

.. class:: <physical-locator>
   :open:
   :abstract:

   :superclasses: :class:`<locator>`

   A physical locator is a locator which refers to an object (such as
   a file or directory) in a physical file system. This locator class
   is useful for coercing an abstract locator into its corresponding
   physical counterpart.

.. class:: <file-system-locator>
   :open:
   :abstract:

   :superclasses: :class:`<physical-locator>`

   A file system locator is a locator that refers to either a directory
   or a file within the file system.

.. class:: <directory-locator>
   :open:
   :abstract:

   :superclasses: :class:`<physical-locator>`

   A directory locator is a locator that refers to a directory as
   distinct from a file. This is important in file systems which can
   view a directory as either a file or a directory. This locator
   class is useful for coercing a file locator into a form where it
   can be manipulated as a directory (e.g. for constructing a locator
   to a file in a directory).

.. constant:: <native-directory-locator>

   This is bound to the native directory locator type for the host
   platform. On Windows, this is typically ``<microsoft-directory-locator>``
   while on POSIX platforms, it is ``<posix-directory-locator>``.

.. class:: <file-locator>
   :open:
   :abstract:

   :superclasses: :class:`<physical-locator>`

   A file locator is a locator which refers to a file as distinct from
   a directory. This is important in file systems which can view a
   directory as either a file or a directory. This locator class is
   useful for coercing a directory locator into a form where it can be
   manipulated as a file.

.. constant:: <native-file-locator>

   This is bound to the native file locator type for the host
   platform. On Windows, this is typically ``<microsoft-file-locator>``
   while on POSIX platforms, it is ``<posix-file-locator>``.

.. class:: <locator-error>

   :superclasses: :class:`<format-string-condition>`, :drm:`<error>`


.. class:: <server-locator>
   :open:
   :abstract:

   :superclasses: :class:`<locator>`


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

   Returns a directory locator for a subdirectory of a given directory.

   :signature: subdirectory-locator (locator #rest sub-path) => (subdirectory)

   :parameter locator: An instance of :class:`<directory-locator>`.
   :parameter #rest sub-path: An instance of ``<object>``.
   :value subdirectory: An instance of :class:`<directory-locator>`.

   :example:

     .. code-block:: dylan

       let build-dir = subdirectory-locator(working-directory(), "_build");

.. generic-function:: supports-list-locator?
   :open:

   Returns whether or not a given locator supports the :gf:`list-locator`
   operation.

   :signature: supports-list-locator? (locator) => (listable?)

   :parameter locator: An instance of :class:`<locator>`.
   :value listable?: An instance of :drm:`<boolean>`.

.. generic-function:: supports-open-locator?
   :open:

   Returns whether or not a given locator supports the :gf:`open-locator`
   operation.

   :signature: supports-open-locator? (locator) => (openable?)

   :parameter locator: An instance of :class:`<locator>`.
   :value openable?: An instance of :drm:`<boolean>`.
