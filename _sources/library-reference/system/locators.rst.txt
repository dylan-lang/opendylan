*******************
The locators Module
*******************

.. current-library:: system
.. current-module:: locators

The Locators module provides Dylan programs with a portable, flexible, and
uniform facility for locating files and other objects on the internet.

The locators Module
-------------------

.. class:: <locator>
   :open:
   :abstract:

   :superclasses: :drm:`<object>`

   This is the base class for all locators. This is the usual locator
   class for coercion (using :drm:`as`) or instantiation (using :drm:`make`)
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

   .. note:: Locator classes representing file system objects are documented
             in :doc:`file-system`.

.. class:: <directory-locator>
   :open:
   :abstract:

   :superclasses: :class:`<physical-locator>`

   :slot locator-relative?: #t if the locator is relative, #f if it is absolute.
   :slot locator-path: the path to the directory.

   A directory locator is a locator that refers to a directory as
   distinct from a file. This is important in file systems which can
   view a directory as either a file or a directory. This locator
   class is useful for coercing a file locator into a form where it
   can be manipulated as a directory (e.g. for constructing a locator
   to a file in a directory).

.. constant:: <native-directory-locator>

   This is bound to the native directory locator type for the host
   platform. On Windows, this is typically :class:`<microsoft-directory-locator>`
   while on POSIX platforms, it is :class:`<posix-directory-locator>`.

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
   platform. On Windows, this is typically :class:`<microsoft-file-locator>`
   while on POSIX platforms, it is :class:`<posix-file-locator>`.

.. class:: <locator-error>

   All errors raised by the locator system should be instances of
   this error.

   :superclasses: :drm:`<simple-error>`


.. class:: <server-locator>
   :open:
   :abstract:

   The abstract superclass of  locators for servers.

   :superclasses: :class:`<locator>`

   :seealso:

      - :class:`<server-url>`
      - :class:`<microsoft-server-locator>`

.. generic-function:: list-locator
   :open:

   Return a sequence of locators that are children of the given
   locator.

   :signature: list-locator (locator) => (locators)

   :parameter locator: An instance of :class:`<locator>`.
   :value locators: An instance of :drm:`<sequence>`.

   :description:

     Return a sequence of locators that are children of the given
     locator.

     Note that this should only be called on a locator for which
     :gf:`supports-list-locator?` returns true.

   :seealso:

     - :gf:`supports-list-locator?`

.. method:: list-locator
   :specializer: <file-system-directory-locator>

   Returns a sequence of locators for the files and directories within
   the directory specified by the directory locator.

   :parameter locator: An instance of :class:`<file-system-directory-locator>`.
   :value locators: An instance of :drm:`<sequence>`.

   :description:

     Returns a sequence of locators for the files and directories within
     the directory specified by the directory locator.

     Instances of :class:`<file-system-file-locator>` for files and symbolic
     links. :gf:`subdirectory-locator` will be called to create locators for
     any directories.

   :seealso:

     - :meth:`supports-list-locator?(<file-system-directory-locator>)`
     - :gf:`do-directory`

.. generic-function:: locator-address

   :signature: locator-address (mailto) => (address)

   :parameter mailto: An instance of :class:`<mail-to-locator>`.
   :value address: An instance of :drm:`<string>`.

   :description:

    Returns the email address specified by the mailto locator.

.. generic-function:: locator-as-string
   :open:

   :signature: locator-as-string (class locator) => (string)

   :parameter class: A subclass of :drm:`<string>`.
   :parameter locator: An instance of :class:`<locator>`.
   :value string: An instance of :drm:`<string>`.

.. generic-function:: locator-base
   :open:

   :signature: locator-base (locator) => (base)

   :parameter locator: An instance of :class:`<locator>`.
   :value base: :drm:`#f` or an instance of :drm:`<string>`.

   :description:
    Returns the locator name without extension. For example, if a file locator's
    path was ``a/b/c.txt``, the locator-base would be ``c``.

.. generic-function:: locator-directory
   :open:

   :signature: locator-directory (locator) => (directory)

   :parameter locator: An instance of :class:`<locator>`.
   :value directory: :drm:`#f` or an instance of :class:`<directory-locator>`.

   :description:
    Returns the enclosing directory of a locator, or :drm:`#f` if it
    is not in a directory.

.. function:: locator-error

   :signature: locator-error (format-string #rest format-arguments) => (#rest results)

   :parameter format-string: An instance of :drm:`<string>`.
   :parameter #rest format-arguments: An instance of :drm:`<object>`.
   :value #rest results: An instance of :drm:`<object>`.

.. generic-function:: locator-extension
   :open:

   :signature: locator-extension (locator) => (extension)

   :parameter locator: An instance of :class:`<locator>`.
   :value extension: :drm:`#f` or an instance of :drm:`<string>`.

   :description:
    Returns the extension part of the locator name. For example, if a file locator's
    path was ``a/b/c.txt``, the locator-extension would be ``txt``.
    Returns :drm:`#f` if the locator does not have an extension.

.. generic-function:: locator-file

   :signature: locator-file (url) => (file)

   :parameter url: An instance of :class:`<file-index-url>` or :class:`<cgi-url>`.
   :value file: An instance of :class:`<file-url>`.

   :description:
    Returns the file URL of a file index or CGI URL. For example, the locator-file
    of ``http://example.com/index.html#tag`` or ``http://example.com/index.html?q=text``
    would be ``http://example.com/index.html``

.. generic-function:: locator-host
   :open:

   Returns the host name.

   :signature: locator-host (locator) => (host)

   :parameter locator: An instance of :class:`<locator>`.
   :value host: :drm:`#f` or an instance of :drm:`<string>`.

   :description:

     Returns the computer host name of a :class:`<server-url>` or
     :class:`<microsoft-unc-locator>`.

.. generic-function:: locator-name

   Returns the name of this locator.

   :signature: locator-name (locator) => (name)

   :parameter locator: An instance of :class:`<locator>`.
   :value name: :drm:`#f` or an instance of :drm:`<string>`.

   :description:

     This is typically the last component of the locator's path but can be
     different for some specializations.

.. method:: locator-name
   :specializer: <mailto-locator>

   Returns the email address of this locator.

   :parameter locator: an instance of :class:`<mailto-locator>`
   :value name: An instance of :drm:`<string>`


.. method:: locator-name
   :specializer: <mailto-locator>

   Returns the email address of this locator.

   :parameter locator: an instance of :class:`<mailto-locator>`
   :value name: An instance of :drm:`<string>`

.. method:: locator-name
   :specializer:  <microsoft-volume-locator>

   Returns the drive letter of this locator.

   :parameter locator: an instance of :class:`<microsoft-volume-locator>`
   :value name: An instance of :drm:`<string>`

   :description:
      The drive is returned as a single letter, for example, 'A'

.. method:: locator-name
   :specializer:  <microsoft-unc-locator>

   Returns the server name of this locator.

   :parameter locator: an instance of :class:`<microsoft-unc-locator>`
   :value name: An instance of :drm:`<string>`

.. generic-function:: locator-path
   :open:

   Returns the directory path of a locator.

   :signature: locator-path (locator) => (path)

   :parameter locator: An instance of :class:`<directory-locator>`.
   :value path: An instance of :drm:`<sequence>`.

   :description:

     Returns the directory path as a sequence of strings, each being the name
     of a path element.

   :example:

     .. code-block:: dylan

        locator-path(as(<file-locator>, "/a/b/c.d")) => #["a", "b"]

.. generic-function:: locator-relative?
   :open:

   :signature: locator-relative? (locator) => (relative?)

   :parameter locator: An instance of :class:`<locator>`.
   :value relative?: An instance of :drm:`<boolean>`.

.. generic-function:: locator-server
   :open:

   :signature: locator-server (locator) => (server)

   :parameter locator: An instance of :class:`<locator>`.
   :value server: :drm:`#f` or an instance of :class:`<server-locator>`.

.. generic-function:: locator-volume
   :open:

   :signature: locator-volume (locator) => (volume)

   :parameter locator: An instance of :class:`<locator>`.
   :value volume: :drm:`#f` or an instance of :drm:`<string>`.

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

   Returns a locator relative to another locator which references the
   same file as this locator.

   :signature: relative-locator (locator from-locator) => (relative-locator)

   :parameter locator: An instance of :class:`<physical-locator>`.
   :parameter from-locator: An instance of :class:`<physical-locator>`.
   :value relative-locator: An instance of :class:`<physical-locator>`.

   :example:
      If self is '/a/b/c/d.txt' and root is '/a/b'

      .. code-block:: dylan

        let rel = relative-locator(self, root);

      Then rel is 'c/d.txt'

.. generic-function:: simplify-locator
   :open:

   Simplifies a locator by removing redundant elements from its
   path.

   :signature: simplify-locator (locator) => (simplified-locator)

   :parameter locator: An instance of :class:`<physical-locator>`.
   :value simplified-locator: An instance of :class:`<physical-locator>`.

.. generic-function:: resolve-locator
   :open:

   Resolves all links, parent references (``..``), self references (``.``), and
   removes unnecessary path separators. Similar to :func:`simplify-locator`
   except that it consults the file system to resolve links. A
   :class:`<file-system-error>` is signaled if for any reason the path can't be
   resolved. Examples include non-existent directory components, access denied,
   I/O error, etc.  In short, this function follows the semantics of POSIX
   ``realpath(3)``.

   :signature: resolve-locator (locator) => (resolved-locator)

   :parameter locator: An instance of :class:`<physical-locator>`.
   :value simplified-locator: An instance of :class:`<physical-locator>`.


.. generic-function:: string-as-locator
   :open:

   Parse a string and create a locator.

   :signature: string-as-locator (class string) => (locator)

   :parameter class: A subclass of :class:`<locator>`.
   :parameter string: An instance of :drm:`<string>`.
   :value locator: An instance of :class:`<locator>`.

   :description:
      This method should be specialized for each new locator class. It
      should return an instance of ``class``, or
      raise a condition of type :class:`<locator-error>`.


.. generic-function:: subdirectory-locator
   :open:

   Returns a directory locator for a subdirectory of a given directory.

   :signature: subdirectory-locator (locator #rest sub-path) => (subdirectory)

   :parameter locator: An instance of :class:`<directory-locator>`.
   :parameter #rest sub-path: An instance of :drm:`<object>`.
   :value subdirectory: An instance of :class:`<directory-locator>`.

   :example:

     .. code-block:: dylan

       let build-dir = subdirectory-locator(working-directory(), "_build");

.. generic-function:: file-locator
   :open:

   Returns a file locator for a file in a subdirectory of the given directory.

   :signature: file-locator (directory, name, #rest more-names) => (file)

   :parameter directory: An instance of :class:`<directory-locator>`.
   :parameter name: An instance of :drm:`<string>`.
   :parameter #rest more-names: Instances of :drm:`<string>`.
   :value file: An instance of :class:`<file-locator>`.

   :example:

     .. code-block:: dylan

       let temp = file-locator(temp-directory(), "my-subdir", "my-test.json");
       ensure-directories-exist(temp);  // Create "my-subdir" directory.

.. generic-function:: supports-list-locator?
   :open:

   Returns whether or not a given locator supports the :gf:`list-locator`
   operation.

   :signature: supports-list-locator? (locator) => (listable?)

   :parameter locator: An instance of :class:`<locator>`.
   :value listable?: An instance of :drm:`<boolean>`.

   :seealso:

     - :gf:`list-locator`

.. method:: supports-list-locator?
   :specializer: <file-system-directory-locator>

   Returns true if the directory locator is not relative.

   :parameter locator: An instance of :class:`<file-system-directory-locator>`.
   :value listable?: An instance of :drm:`<boolean>`.

   :seealso:

     - :meth:`list-locator(<file-system-directory-locator>)`

.. generic-function:: supports-open-locator?
   :open:

   Returns whether or not a given locator supports the :gf:`open-locator`
   operation.

   :signature: supports-open-locator? (locator) => (openable?)

   :parameter locator: An instance of :class:`<locator>`.
   :value openable?: An instance of :drm:`<boolean>`.

.. class:: <web-locator>
   :abstract:

   :superclasses: :class:`<locator>`

   The abstract superclass of locators that access a resource via
   web protocols, such as ftp or http.

.. class:: <url>
   :abstract:
   :sealed:

   :superclasses: :class:`<web-locator>`, :class:`<physical-locator>`

   The abstract superclass of web locators that reference a physical object.
   Use ``as(<url>, "...")`` to create an appropriate concrete subclass.

   :seealso:
      :class:`<file-url>`
      :class:`<directory-url>`
      :class:`<cgi-url>`
      :class:`<file-index-url>`

.. class:: <directory-url>

   :superclasses: :class:`<url>`, :class:`<directory-locator>`

   Represents directories that are accessible via web protocols.

.. class:: <file-url>

   :superclasses: :class:`<url>`, :class:`<file-locator>`

   Represents files that are accessible via web protocols.

.. class:: <file-index-url>

   :superclasses: :class:`<url>`

   Represents a URL that has a fragment part, for
   example ``http://www.example.com/path/file.txt#fragment``.

.. class:: <cgi-url>

   :superclasses: :class:`<url>`

   Represents a URL that has a query part, for example
   ``http://www.example.com/path/file.txt?query=text``.

.. function:: locator-cgi-string

   Return the query part of a ``<cgi-url>``.

   :signature: locator-cgi-string(locator) => (string)

   :parameter locator: an instance of :class:`<cgi-url>`
   :value string: an instance of :drm:`<string>`

.. function:: locator-index

   Return the fragment part of a :class:``<file-index-url>``

   :signature: locator-index(locator) => (string)

   :parameter locator: an instance of :class:`<file-index-url>`
   :value string: an instance of :drm:`<string>`

.. class:: <mail-to-locator>

   :superclasses: :class:`<url>`

   Represents a locator which is an email address.

.. class:: <server-url>
   :abstract:

   Represents a locator which is a machine accessible via web
   protocols.

   :superclasses: :class:`<url>`, :class:`<server-locator>`

   :slot locator-host: The computer host
   :slot locator-username: The user identifier
   :slot locator-password: The user password
   :operations: :gf:`locator-port`,
		:gf:`locator-default-port`

   The locator includes information on the protocol, host-name, port, user and password of the machine.

   :seealso:
      :class:`<http-server>`
      :class:`<https-server>`
      :class:`<ftp-server>`
      :class:`<file-server>`

.. class:: <http-server>
   :sealed:

   A server for the http protocol.

   :superclasses: :class:`<server-url>`

.. class:: <https-server>
   :sealed:

   A server for the https protocol.

   :superclasses: :class:`<server-url>`

.. class:: <ftp-server>
   :sealed:

   A server for the ftp protocol.

   :superclasses: :class:`<server-url>`

.. class:: <file-server>
   :sealed:

   A locator using the file protocol.

   :superclasses: :class:`<server-url>`

.. generic-function:: locator-default-port

   Return the default port associated with the locator's protocol.

   :signature: locator-default-port(locator) => (port)

   :param locator: An instance of :class:`<server-url>`
   :value port: :drm:`#f` or an instance of :drm:`<integer>`

   :example:

   .. code-block:: dylan

      let locator = as(<server-url>, "http://www.example.com");
      let default-port = locator-default-port(locator);
      // Result: default-port = 80
