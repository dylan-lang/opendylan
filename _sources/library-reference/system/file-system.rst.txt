**********************
The file-system Module
**********************

.. current-library:: system
.. current-module:: file-system

The File-System module is part of the System library and provides a
generic interface to the file system of the local machine. Remotely
mounted file systems are also accessible using this module.

Types specific to file system operations
----------------------------------------

The File-System module contains a number of types specifically designed
for use by interfaces in the module.

- :type:`<file-type>`
- :type:`<pathname>`
- :type:`<copy/rename-disposition>`

Manipulating files
------------------

The File-System module contains a number of interfaces that let you
perform standard file management operations on files already resident on
the filesystem. You can rename, copy, or delete any file, and you can
set any available properties for the file.

- :func:`copy-file`
- :func:`delete-file`
- :func:`rename-file`
- :func:`file-property-setter`
- :gf:`expand-pathname`
- :gf:`shorten-pathname`
- :func:`create-symbolic-link`
- :func:`create-hard-link`
- :macro:`with-open-file`

Manipulating directories
------------------------

The File-System module contains a number of interfaces that let you
create and delete directories. These can be used in conjunction with the
file manipulation operations described in `Manipulating files`_ to
perform file management tasks at any position in the file system.

- :func:`create-directory`
- :func:`delete-directory`
- :func:`directory-contents`
- :func:`ensure-directories-exist`
- :func:`do-directory`
- :func:`working-directory-setter`
- :gf:`directory-empty?`

Finding out file system information
-----------------------------------

A number of functions return environment information regarding the
directory structure of the file system. Each function takes no
arguments, and returns a pathname or list of pathnames. The return
values can be use in conjunction with other functions to perform
file-based operations relative to the directories involved.

- :func:`home-directory`
- :func:`root-directories`
- :func:`temp-directory`
- :func:`working-directory`

Finding out file information
----------------------------

Several interfaces in the File-System module allow you to interrogate
files for information. You can find out whether a file exists, what its
name is, or which directory it resides in, and you can find the current
properties of the file.

- :func:`file-exists?`
- :func:`file-properties`
- :func:`file-property`
- :func:`file-type`
- :func:`link-target`

File system locators
--------------------

The module offers multiple classes that reference either a directory
or a file within the file system.

- :class:`<file-system-locator>`
- :class:`<file-system-file-locator>`
- :class:`<file-system-directory-locator>`

The graphic below shows the file system locator hierarchy
(dashed boxes are classes from module `locators`).

.. graphviz::
  :align: center
  :class: only-light

  digraph G {

    fontname="Helvetica,Arial,sans-serif";
    node [shape=box, color=black];

    physical_locator              [label="<physical-locator>\noa", style=dashed];
    file_locator                  [label="<file-locator>\noa", style=dashed];
    directory_locator             [label="<directory-locator>\noa",style=dashed];
    file_system_locator           [label="<file-system-locator>\noa"];
    file_system_file_locator      [label="<file-system-file-locator>"];
    file_system_directory_locator [label="<file-system-directory-locator>"];

    physical_locator              -> file_system_locator;
    physical_locator              -> directory_locator;
    physical_locator              -> file_locator;
    file_system_locator           -> file_system_file_locator;
    file_locator                  -> file_system_file_locator;
    file_system_locator           -> file_system_directory_locator;
    directory_locator             -> file_system_directory_locator;
  }

.. graphviz::
  :align: center
  :class: only-dark

  digraph G {

    bgcolor="#131416";
    fontname="Arial,Helvetica,sans-serif";

    node [
      fontcolor = "#e6e6e6",
      style = filled,
      shape=box,
      color = "#e6e6e6",
      fillcolor = "#333333"
    ]

    edge [
      color = "#e6e6e6",
      fontcolor = "#e6e6e6"
    ]

    physical_locator              [label="<physical-locator>\noa", style=dashed];
    file_locator                  [label="<file-locator>\noa", style=dashed];
    directory_locator             [label="<directory-locator>\noa",style=dashed];
    file_system_locator           [label="<file-system-locator>\noa"];
    file_system_file_locator      [label="<file-system-file-locator>"];
    file_system_directory_locator [label="<file-system-directory-locator>"];

    physical_locator              -> file_system_locator;
    physical_locator              -> directory_locator;
    physical_locator              -> file_locator;
    file_system_locator           -> file_system_file_locator;
    file_locator                  -> file_system_file_locator;
    file_system_locator           -> file_system_directory_locator;
    directory_locator             -> file_system_directory_locator;
  }

On Posix systems:

- :class:`<posix-file-system-locator>`
- :class:`<posix-directory-locator>`
- :class:`<posix-file-locator>`

The graphic below shows the Posix file system hierarchy
(dashed boxes are classes from module `locators`).

.. graphviz::
  :align: center
  :class: only-light

  digraph G {
    fontname="Helvetica,Arial,sans-serif";
    node  [shape=box, color=black];

    physical_locator              [label="<physical-locator>\noa", style=dashed] ;
    directory_locator             [label="<directory-locator>\noa",style=dashed];
    file_locator                  [label="<file-locator>\noa", style=dashed];
    file_system_locator           [label="<file-system-locator>\noa"];
    file_system_file_locator      [label="<file-system-file-locator>"];
    file_system_directory_locator [label="<file-system-directory-locator>"];

    posix_file_system_locator     [label="<posix-file-system-locator>\noas"];
    posix_directory_locator       [label="<posix-directory-locator>\ns"] ;
    posix_file_locator            [label="<posix-file-locator>\ns"]

    physical_locator              -> file_locator;
    physical_locator              -> file_system_locator;
    physical_locator              -> directory_locator;
    directory_locator             -> file_system_directory_locator;
    file_locator                  -> file_system_file_locator;
    file_system_locator           -> file_system_file_locator;
    file_system_locator           -> file_system_directory_locator;
    file_system_locator           -> posix_file_system_locator;
    file_system_directory_locator -> posix_directory_locator;
    file_system_file_locator      -> posix_file_locator;
    posix_file_system_locator     -> posix_directory_locator;
    posix_file_system_locator     -> posix_file_locator;
  }

.. graphviz::
  :align: center
  :class: only-dark

  digraph G {

    bgcolor="#131416";
    fontname="Helvetica,Arial,sans-serif";

    node [
      fontcolor = "#e6e6e6",
      style = filled,
      shape=box,
      color = "#e6e6e6",
      fillcolor = "#333333"
    ]

    edge [
      color = "#e6e6e6",
      fontcolor = "#e6e6e6"
    ]

    physical_locator              [label="<physical-locator>\noa", style=dashed] ;
    directory_locator             [label="<directory-locator>\noa",style=dashed];
    file_locator                  [label="<file-locator>\noa", style=dashed];
    file_system_locator           [label="<file-system-locator>\noa"];
    file_system_file_locator      [label="<file-system-file-locator>"];
    file_system_directory_locator [label="<file-system-directory-locator>"] ;

    posix_file_system_locator     [label="<posix-file-system-locator>\noas"];
    posix_directory_locator       [label="<posix-directory-locator>\ns"] ;
    posix_file_locator            [label="<posix-file-locator>\ns"]

    physical_locator              -> file_locator;
    physical_locator              -> file_system_locator;
    physical_locator              -> directory_locator;
    directory_locator             -> file_system_directory_locator;
    file_locator                  -> file_system_file_locator;
    file_system_locator           -> file_system_file_locator;
    file_system_locator           -> file_system_directory_locator;
    file_system_locator           -> posix_file_system_locator;
    file_system_directory_locator -> posix_directory_locator;
    file_system_file_locator      -> posix_file_locator;
    posix_file_system_locator     -> posix_directory_locator;
    posix_file_system_locator     -> posix_file_locator;
  }

On Microsoft systems:

- :class:`<microsoft-server-locator>`
- :class:`<microsoft-unc-locator>`
- :class:`<microsoft-volume-locator>`
- :class:`<microsoft-file-system-locator>`
- :class:`<microsoft-directory-locator>`
- :class:`<microsoft-file-locator>`

The graphic below shows the Microsoft file system hierarchy
(dashed boxes are classes from module `locators`).

.. graphviz::
  :align: center
  :class: only-light

  digraph G {
    fontname="Helvetica,Arial,sans-serif";
    node  [shape=box, color=black];

    locator                       [label="<locator>\noa",style=dashed];
    physical_locator              [label="<physical-locator>\noa", style=dashed] ;
    directory_locator             [label="<directory-locator>\noa",style=dashed];
    server_locator                [label="<server-locator>\noa",style=dashed];

    file_locator                  [label="<file-locator>\noa", style=dashed];
    file_system_locator           [label="<file-system-locator>\noa"];

    microsoft_file_system_locator [label="<microsoft-file-system-locator>\na"];
    microsoft_server_locator      [label="<microsoft-server-locator>\nas"];
    microsoft_unc_locator         [label="<microsoft-unc-locator>\ns"];
    microsoft_volume_locator      [label="<microsoft-volume-locator>\ns"];
    microsoft_directory_locator   [label="<microsoft-directory-locator>"];
    microsoft_file_locator        [label="<microsoft-file-locator>"];

    locator                       -> server_locator;
    locator                       -> physical_locator;
    physical_locator              -> file_locator;
    physical_locator              -> file_system_locator;
    physical_locator              -> directory_locator;
    server_locator                -> microsoft_server_locator;
    directory_locator             -> microsoft_directory_locator;
    file_locator                  -> microsoft_file_locator;
    file_system_locator           -> microsoft_file_system_locator;
    microsoft_file_system_locator -> microsoft_directory_locator;
    microsoft_file_system_locator -> microsoft_file_locator;
    microsoft_server_locator      -> microsoft_unc_locator;
    microsoft_server_locator      -> microsoft_volume_locator;
  }

.. graphviz::
  :align: center
  :class: only-dark

  digraph G {

    bgcolor="#131416";
    fontname="Helvetica,Arial,sans-serif";

    node [
      fontcolor = "#e6e6e6",
      style = filled,
      shape=box,
      color = "#e6e6e6",
      fillcolor = "#333333"
    ]

    edge [
      color = "#e6e6e6",
      fontcolor = "#e6e6e6"
    ]

    locator                       [label="<locator>\noa",style=dashed];
    physical_locator              [label="<physical-locator>\noa", style=dashed] ;
    directory_locator             [label="<directory-locator>\noa",style=dashed];
    server_locator                [label="<server-locator>\noa",style=dashed];

    file_locator                  [label="<file-locator>\noa", style=dashed];
    file_system_locator           [label="<file-system-locator>\noa"];

    microsoft_file_system_locator [label="<microsoft-file-system-locator>\na"];
    microsoft_server_locator      [label="<microsoft-server-locator>\nas"];
    microsoft_unc_locator         [label="<microsoft-unc-locator>\ns"];
    microsoft_volume_locator      [label="<microsoft-volume-locator>\ns"];
    microsoft_directory_locator   [label="<microsoft-directory-locator>"];
    microsoft_file_locator        [label="<microsoft-file-locator>"];

    locator                       -> server_locator;
    locator                       -> physical_locator;
    physical_locator              -> file_locator;
    physical_locator              -> file_system_locator;
    physical_locator              -> directory_locator;
    server_locator                -> microsoft_server_locator;
    directory_locator             -> microsoft_directory_locator;
    file_locator                  -> microsoft_file_locator;
    file_system_locator           -> microsoft_file_system_locator;
    microsoft_file_system_locator -> microsoft_directory_locator;
    microsoft_file_system_locator -> microsoft_file_locator;
    microsoft_server_locator      -> microsoft_unc_locator;
    microsoft_server_locator      -> microsoft_volume_locator;
  }

Native locators, which are bound to the host platform:

- :const:`<native-file-system-locator>`

File streams
------------

File streams are intended only for accessing the contents of
files. More general file handling facilities, such as renaming,
deleting, moving, and parsing directory names, are provided by this
module.

The :drm:`make` method on :class:`<file-stream>` does not create
direct instances of :class:`<file-stream>`, but instead an instance of
a subclass determined by :gf:`type-for-file-stream`. See
`make`_ and `Options when creating file streams`_ below.

Options when creating file streams
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When creating file streams, you can supply the following init-keywords
to *make* in addition to those described in `File streams`_:

- ``if-exists:`` An action to take if the file already exists.
- ``if-does-not-exist:`` An action to take if the file does not already exist.
- ``element-type:`` How the elements of the underlying file are accessed.
- ``asynchronous?:`` Allows asynchronous writing of stream data to disk.
- ``share-mode:`` How the file can be accessed while the stream is
  operating on it.

The ``if-exists:`` init-keyword allows you to specify an action to take if
the file named by *filename* already exists. The options are:

- :drm:`#f` The file is opened with the stream position at the beginning.
  This is the default when the stream's direction is ``#"input"`` or
  ``#"input-output"``.
- ``#"new-version"`` If the underlying file system supports file versioning,
  a new version of the file is created. This is the default when the stream's
  direction is ``#"output"``.
  If the file system does not support file versioning, the default is
  ``#"replace"`` when the direction of the stream is ``#"output"``.
- ``#"overwrite"`` Set the stream's position to the beginning of the
  file, but preserve the current contents of the file. This is useful
  when the direction is ``#"input-output"`` or ``#"output"`` and you want
  to overwrite an existing file.
- ``#"replace"`` Delete the existing file and create a new file.
- ``#"append"`` Set the stream's initial position to the end of the
  existing file so that all new output occurs at the end of the file.
  This option is only useful if the file is writeable.
- ``#"truncate"`` If the file exists, it is truncated, setting the size
  of the file to 0. If the file does not exist, create a new file.
- ``#"signal"`` Signal a :class:`<file-exists-error>` condition.

The ``if-does-not-exist:`` init-keyword allows you to specify an action to
take if the file named by *filename* does not exist. The options are:

- :drm:`#f` No action.
- ``#"signal"`` Signal a :class:`<file-does-not-exist-error>` condition. This is
  the default when the stream's direction is ``#"input"``.
- ``#"create"`` Create a new zero-length file. This is the default when
  the stream's direction is ``#"output"`` or ``#"input-output"``.

Because creating a file stream *always* involves an attempt to open the
underlying file, the aforementioned error conditions will occur during
file stream instance initialization.

File permissions are checked when creating and opening file streams, and
if the user attempts to open a file for input, and has no read
permission, or to open a file for output, and has no write permission,
then an :class:`<invalid-file-permissions-error>`
condition is signalled at the time the file stream is created.

The ``element-type:`` init-keyword controls how the elements of the
underlying file are accessed. The three possible element types
are:

- :type:`<byte-character>`
  The file is accessed as a sequence of 8-bit characters.

- :type:`<unicode-character>`
  The file is accessed as a sequence of 16-bit Unicode characters.

- :type:`<byte>`
  The file is accessed as a sequence of unsigned 8-bit integers.

The ``asynchronous?:`` init-keyword allows asynchronous writing of stream
data to disk. If :drm:`#f`, whenever the stream has to write a buffer to
disk, the thread which triggered the write must wait for the write to
complete. If ``asynchronous?`` is :drm:`#t`, the write proceeds in parallel
with the subsequent actions of the thread.

Note that asynchronous writes complicate error handling a bit. Any write
error which occurs most likely occurs after the call which triggered the
write. If this happens, the error is stored in a queue, and the next
operation on that stream signals the error. If you *close* the stream
with the *wait?* flag :drm:`#f`, the close happens asynchronously (after all
queued writes complete) and errors may occur after *close* has returned.
A method :gf:`wait-for-io-completion` is provided to catch any errors that
may occur after *close* is called.

The ``share-mode:`` keyword determines how a file can be accessed by other
streams while the stream has it open. The possible values are:

- ``#"share-read"`` Allow other streams to be opened to the file for
  reading but not for writing.
- ``#"share-write"`` Allow other streams to be opened for writing but not
  for reading.
- ``#"share-read-write"`` Allow other streams to be opened for writing
  or reading.
- ``#"exclusive"`` Do not allow other streams to be opened to this file.

Conditions
----------

The conditions signaled by this module are:

- :class:`<file-system-error>`
- :class:`<file-error>`
- :class:`<file-exists-error>`
- :class:`<file-does-not-exist-error>`
- :class:`<invalid-file-permissions-error>`

All errors directly signaled by this module are subclasses of
:class:`<file-system-error>`.

The :gf:`file-error-locator` provides extra details about the file
locator that signals the condition. This function can be used on the
class :class:`<file-error>` and its subclasses.


The FILE-SYSTEM module reference
--------------------------------

This section contains a reference entry for each item included in the
File-System module.

.. function:: copy-file

   Creates a copy of a file.

   :signature: copy-file *old-file* *new-file* #key *if-exists* => ()

   :parameter old-file: An instance of :type:`<pathname>`.
   :parameter new-file: An instance of :type:`<pathname>`.
   :parameter #key if-exists: An instance of
     :type:`<copy/rename-disposition>`. Default value: ``#"signal"``.

   :description:

     Copies *old-file* to *new-file*. If *new-file* already exists, the
     action of this function is controlled by the value of *if-exists*. The
     default is to prompt you before overwriting an existing file.

   :seealso:

     - :type:`<copy/rename-disposition>`
     - :class:`rename-file`

.. type:: <copy/rename-disposition>

   The type that represents possible actions when overwriting existing
   files.

   :equivalent: ``one-of(#"signal", #"replace")``

   :description:

     This type represents the acceptable values for the *if-exists:*
     argument to the :func:`copy-file` and :func:`rename-file`
     functions. Only two values are acceptable:

     -  If ``#"signal"`` is used, then you are warned before a file is
        overwritten during a copy or move operation.
     -  If ``#"replace"`` is used, then you are not warned before a file is
        overwritten during a copy or move operation.

   :operations:

     - :func:`copy-file`
     - :func:`rename-file`

   :seealso:

     - :func:`copy-file`
     - :func:`rename-file`

.. function:: create-directory

   Creates a new directory in the specified parent directory.

   :signature: create-directory *parent* *name* => *directory*

   :parameter parent: An instance of :type:`<pathname>`.
   :parameter name: An instance of :drm:`<string>`.
   :value directory: An instance of :type:`<pathname>`.

   :description:

     Creates *directory* in the specified *parent* directory. The return
     value of this function can be used with :drm:`concatenate` to
     create pathnames of entities in the new directory.

   :seealso:

     - :func:`delete-directory`

.. function:: create-symbolic-link

   Creates a symbolic link

   :signature: create-symbolic-link *target* *link* => ()

   :param target: An instance of :class:`<pathname>`.
   :param link:   An instance of :class:`<pathname>`.

   :description:

     Creates a symbolic link to *target*.
     Not supported in Win32.

   :example:

     Creates a symbolic link named *whattimeislove* to the *date* 
     utility.

     .. code-block:: dylan

        create-symbolic-link("/usr/bin/date", "/tmp/whattimeislove")

    :seealso:

      - :func:`create-hard-link`

.. function:: create-hard-link

   Creates a hard link

   :signature: create-hard-link *target* *link* => ()

   :param target: An instance of :class:`<pathname>`.
   :param link:   An instance of :class:`<pathname>`.

   :description:

     Creates a directory entry that associates *link* with *target*.
     Not yet supported in Win32.

   :example:

     Creates a hard link named *my-hard-link* to *myfile.txt*.

     .. code-block:: dylan

        create-hard-link("myfile.txt", "my-hard-link")

    :seealso:

      - :func:`create-symbolic-link`

.. function:: delete-directory

   Deletes the specified directory.

   :signature: delete-directory *directory* #key *recursive?* => ()

   :parameter directory: An instance of :type:`<pathname>`.
   :parameter #key recursive?: An instance of :type:`<boolean>`.
                               Default value: :drm:`#f`

   :description:

     Deletes the specified directory. By default the directory may
     only be deleted if it is empty. Pass ``recursive?: #t`` to delete
     the directory and its contents recursively.

   :seealso:

     - :func:`create-directory`
     - :func:`delete-file`

.. function:: delete-file

   Deletes the specified file system entity.

   :signature: delete-file *file* => ()

   :parameter file: An instance of :type:`<pathname>`.

   :description:

     Deletes the file system entity specified by *file*. If *file*
     refers to a link, the link is removed, but the actual file that the
     link points to is not removed.

.. function:: directory-contents

   Returns a sequence of files and subdirectories contained in a directory.

   :signature: directory-contents *directory* => *locators*

   :parameter directory: An instance of :type:`<pathname>`.
   :value locators: A :drm:`<sequence>` of :class:`<locator>`.

   :description:

      In the result, each file is represented by a :class:`<file-locator>` and
      each directory is represented by a :class:`<directory-locator>`. The "."
      and ".." directories are not included in the result.

.. generic-function:: directory-empty?

   Checks whether a directory is empty or not.

   :signature: directory-empty? *directory* => *empty?*

   :param directory: An instance of :class:`<pathname>`,
   :value empty?: An instance of :class:`<boolean>`.

.. method:: directory-empty?
   :specializer: <file-system-directory-locator>

   :param directory: An instance of :class:`<file-system-directory>`.
   :value empty?: An instance of :class:`<boolean>`.

.. function:: do-directory

   Executes the supplied function once for each entity in the specified
   directory.

   :signature: do-directory *function* *directory* => ()

   :parameter function: An instance of :drm:`<function>`.
   :parameter directory: An instance of :type:`<pathname>`.

   :description:

     Executes *function* once for each entity in *directory*.

     The signature of *function* is::

       *function* *directory* *name* *type* => ()

     where *directory* is an instance of :type:`<pathname>`, *name* is
     an instance of :drm:`<byte-string>`, and *type* is an instance of
     :type:`<file-type>`.

     Within *function*, the values of *directory* and *name* can be
     concatenated to generate a :type:`<pathname>` suitable for use by
     the other functions in the module.

     The following calls are equivalent:

     .. code-block:: dylan

       do-directory(my-function, "C:\\USERS\\JOHN\\FOO.TEXT")

       do-directory(my-function, "C:\\USERS\\JOHN\\")

     as they both operate on the contents of ``C:\\USERS\\JOHN``. The call:

     .. code-block:: dylan

       do-directory(my-function, "C:\\USERS\\JOHN")

     is not equivalent as it will operate on the contents of ``C:\\USERS``.

.. function:: ensure-directories-exist

   Ensures that all the directories in the pathname leading to a file
   exist, creating any that do not, as needed.

   :signature: ensure-directories-exist *file* => *created?*

   :parameter file: An instance of :type:`<pathname>`.
   :value created?: An instance of :drm:`<boolean>`.

   :description:

     Ensures that all the directories in the pathname leading to a file
     exist, creating any that do not, as needed. The return value
     indicates whether or not any directory was created.

     The following calls are equivalent:

     .. code-block:: dylan

       ensure-directories-exist("C:\\USERS\\JOHN\\FOO.TEXT")
       ensure-directories-exist("C:\\USERS\\JOHN\\")

     as they will both create the directories *USERS* and *JOHN* if needed.
     The call:

     .. code-block:: dylan

       ensure-directories-exist("C:\\USERS\\JOHN")

     is not equivalent as it will only create *USERS* if needed.

   :example:

     .. code-block:: dylan

       ensure-directories-exist("C:\\USERS\\JOHN\\FOO.TEXT")

   :seealso:

     - :func:`create-directory`

.. generic-function:: expand-pathname

   Given a pathname, returns its fully expanded form.

   :signature: expand-pathname *path* => *expanded-path*

   :param path: An instance of :class:`<pathname>`.
   :value expanded-path: An instance of :class:`<pathname>`.

.. method:: expand-pathname
   :specializer: <file-system-locator>

   Expand a file path to its fully expanded form.

   :param path: An instance of :class:`<file-system-locator>`.

.. method:: expand-pathname
   :specializer: <string>

   Expands a pathname given as a string.

   :param path: An instance of :class:`<string>`.

.. generic-function:: file-error-locator

   :signature: file-error-locator *error* => (locator)

   :param error: An instance of :class:`<file-error>`.
   :value locator: An instance of :class:`<file-system-file-locator>`.

   :description:

      Returns the file locator associated with the error.

.. class:: <file-does-not-exist-error>

   Error type signaled accessing a file that do not exist.

   :superclasses: :class:`<file-error>`

   :description:

      Signaled when trying to open a file and the file does not
      already exist.

.. class:: <file-error>

   Error type signaled for all failed file operations.

   :superclasses: :class:`<file-system-error>`

   :keyword locator: An instance of
     :class:`<file-system-file-locator>`. Specifies the file locator
     related with the error.

   :description:

     Signaled when one of the file system functions triggers an error,
     such as a permissions error when trying to delete or rename a file.
     It provides information about the file locator.

   :seealso:

      - :class:`<file-system-error>`
      - :class:`<file-system-file-locator>`
      - :class:`<locator>`

.. class:: <file-exists-error>

   Error type signaled when a file already exists.

   :superclasses: :class:`<file-error>`

   :description:

      Signaled when an attempt is made to create a file and it
      already exists.

.. function:: file-exists?

   Returns :drm:`#t` if the specified file exists.

   :signature: file-exists? *file* #key *follow-links?* => *exists?*

   :parameter file: An instance of :type:`<pathname>`.
   :parameter follow-links?: An instance of :drm:`<boolean>`. Defaults to
      :drm:`#t`.
   :value exists?: An instance of :drm:`<boolean>`.

   :description:

     Returns :drm:`#t` if *file* exists. If the file refers to a symbolic link,
     the behavior depends on the value of *follow-links?*. If *follow-links?*
     is true (the default) the target of the link is checked; otherwise the
     link itself is checked.

.. function:: file-properties

   Returns all the properties of a file system entity.

   :signature: file-properties *file* => *properties*

   :parameter file: An instance of :type:`<pathname>`.
   :value properties: An instance of a concrete subclass of
     :drm:`<explicit-key-collection>`.

   :description:

     Returns all the properties of *file*. The keys to the properties
     collection are the same as those use by :gf:`file-property`, above.

   :example:

     .. code-block:: dylan

       file-properties() [#"size"]

   :seealso:

     - :gf:`file-property`
     - :func:`file-property-setter`

.. generic-function:: file-property
   :sealed:

   Returns the specified property of a file system entity.

   :signature: file-property *file* #key *key* => *property*

   :parameter file: An instance of :type:`<pathname>`.
   :parameter #key key: One of ``#"author"``, ``#"size"``,
     ``#"creation-date"``, ``#"access-date"``, ``#"modification-date"``,
     ``#"readable?"``, ``#"writeable?"``, ``#"executable?"``.
   :value property: The value of the property specified by *key*. The
     type of the value returned depends on the value of *key*: see the
     description for details.

   :description:

     Returns the property of *file* specified by *key*. The value
     returned depends on the value of *key*, as shown in Table :ref:`Return
     value types of file-property <file-property-return-value-types>`.

     .. _file-property-return-value-types:
     .. table:: Return value types of ``file-property``

       +--------------------------+-------------------------------+
       | Value of *key*           | Type of return value          |
       +==========================+===============================+
       | ``#"author"``            | ``false-or(<string>)``        |
       +--------------------------+-------------------------------+
       | ``#"size"``              | :drm:`<integer>`              |
       +--------------------------+-------------------------------+
       | ``#"creation-date"``     | :class:`<date>`               |
       +--------------------------+-------------------------------+
       | ``#"access-date"``       | :class:`<date>`               |
       +--------------------------+-------------------------------+
       | ``#"modification-date"`` | :class:`<date>`               |
       +--------------------------+-------------------------------+
       | ``#"readable?"``         | :drm:`<boolean>`              |
       +--------------------------+-------------------------------+
       | ``#"writeable?"``        | :drm:`<boolean>`              |
       +--------------------------+-------------------------------+
       | ``#"executable?"``       | :drm:`<boolean>`              |
       +--------------------------+-------------------------------+

     Not all platforms implement all of the above keys. Some platforms
     may support additional keys. The ``#"author"`` key is supported on
     all platforms but may return :drm:`#f` if it is not meaningful on a
     given platform. Use of an unsupported key signals an error.

     All keys listed above are implemented by Win32, though note that
     ``#"author"`` always returns :drm:`#f`.

   :seealso:

     - :gf:`file-property-setter`
     - :func:`file-properties`

.. generic-function:: file-property-setter
   :sealed:

   Sets the specified property of a file system entity to a given value.

   :signature: file-property-setter *new-value* *file* *key* => *new-value*

   :parameter new-value: The type of this depends on the value of *key*.
     See the description for details.
   :parameter file: An instance of :type:`<pathname>`.
   :parameter key: One of ``#"author"``, ``#"size"``,
     ``#"creation-date"``, ``#"access-date"``, ``#"modification-date"``,
     ``#"readable?"``, ``#"writeable?"``, ``#"executable?"``.
   :value new-value: The type of this depends on the value of *key*. See
     the description for details.

   :description:

     Sets the property of *file* specified by *key* to *new-value*. The type
     of *new-value* depends on the property specified by key, as shown in
     Table :ref:`New value types of file-property-setter
     <file-property-setter-return-value-types>` below.

     .. _file-property-setter-return-value-types:
     .. table:: New value types of *file-property-setter*

       +--------------------------+-------------------------------+
       | Value of *key*           | Type of *new-value*           |
       +==========================+===============================+
       | ``#"author"``            | ``false-or(<string>)``        |
       +--------------------------+-------------------------------+
       | ``#"size"``              | :drm:`<integer>`              |
       +--------------------------+-------------------------------+
       | ``#"creation-date"``     | :class:`<date>`               |
       +--------------------------+-------------------------------+
       | ``#"access-date"``       | :class:`<date>`               |
       +--------------------------+-------------------------------+
       | ``#"modification-date"`` | :class:`<date>`               |
       +--------------------------+-------------------------------+
       | ``#"readable?"``         | :drm:`<boolean>`              |
       +--------------------------+-------------------------------+
       | ``#"writeable?"``        | :drm:`<boolean>`              |
       +--------------------------+-------------------------------+
       | ``#"executable?"``       | :drm:`<boolean>`              |
       +--------------------------+-------------------------------+

     Note that *file-property-setter* returns the value that was set, and so
     return values have the same types as specified values, depending on the
     value of *key*.

     Not all platforms implement all of the above keys. Some platforms may
     support additional keys. Use of an unsupported key signals an error.

     The only property that can be set on Win32 is ``#"writeable?"``.

   :seealso:

     - :gf:`file-property`
     - :func:`file-properties`

.. class:: <file-system-error>

   Error type signaled when any of the functions in the File-System
   module signal an error.

   :superclasses: :drm:`<simple-error>`

   :description:

     Signaled when one of the file system functions triggers an error,
     such as a permissions error when trying to delete or rename a file.

.. class:: <file-system-locator>
   :open:
   :abstract:

   :superclasses: :class:`<physical-locator>`

   A file system locator is a locator that refers to either a directory
   or a file within the file system.

.. class:: <file-system-file-locator>

   :superclasses: :class:`<file-system-locator>`, :class:`<file-locator>`

   This locator refers to a non-directory file within a file system.

.. class:: <file-system-directory-locator>

   :superclasses: :class:`<file-system-locator>`, :class:`<directory-locator>`

   This locator refers to a directory within a file system.

.. function:: file-system-separator

   Returns the character used to separate the directory components in
   a file path.

   :signature: file-system-separator => separator

   :value separator: An instance of :class:`<character>`.

   :description:

   The character separator used in a file system is determined by the
   specific file system and operating system. Open Dylan offers
   modules that transparently provide the appropriate separator for
   Posix and Microsoft systems.

.. function:: file-type

   Returns the type of the specified file system entity.

   :signature: file-type *file* => *file-type*

   :parameter file: An instance of :type:`<pathname>`.
   :value file-type: An instance of :type:`<file-type>`.

   :description:

     Returns the type of *file*, the specified file system entity. A
     file system entity can either be a file, a directory, or a link to
     another file or directory.

.. type:: <file-type>

   The type representing all possible types of a file system entity.

   :equivalent: ``one-of(#"file", #"directory", #"link")``

   :description:

     The type representing all possible types of a file system entity.
     An entity on the file system can either be a file, a directory or
     folder, or a link to another file or directory. The precise
     terminology used to refer to these different types of entity
     depends on the operating system you are working in.

   :operations:

     - :func:`do-directory`

.. function:: home-directory

   Returns the current value of the home directory.

   :signature: home-directory () => *home-directory*

   :value home-directory: An instance of :type:`<pathname>`.

   :description:

     Returns the current value of the home directory. The return value
     of this function can be used with concatenate to create pathnames
     of entities in the home directory.

.. class:: <invalid-file-permissions-error>

   Signals an error when the user has no permission to create, delete,
   read or write a file.

   :superclasses: :class:`<file-error>`

   :description:

     Signals an error when you attempt to perform an operation on a
     file or directory that requires certain permissions, but the
     permissions set on the file are incorrect or insufficient for
     your operation.

.. function:: link-target

   Returns the target of a symbolic link.

   :signature: link-target *file* => *target*
   :parameter file: An instance of type :type:`<pathname>`.
   :value target: An instance of type :type:`<pathname>`.
   :description:

      Repeatedly follows symbolic links starting with *file* until it finds a
      non-link file or directory, or a non-existent link target.

   :seealso:

     - :func:`create-symbolic-link`

.. _make:

.. method:: make
   :specializer: <file-stream>

   Creates and opens a stream over a file.

   :signature: make *file-stream-class* #key *filename* *direction* *if-exists* *if-does-not-exist* *buffer-size* *element-type* => *file-stream-instance*

   :parameter file-stream-class: The class :class:`<file-stream>`.
   :parameter #key locator: An instance of :drm:`<object>`.
   :parameter #key direction: One of ``#"input"``, ``#"output"``, or
     ``#"input-output"``. The default is ``#"input"``.
   :parameter #key if-exists: One of :drm:`#f`, ``#"new-version"``,
     ``#"overwrite"``, ``#"replace"``, ``#"append"``, ``#"truncate"``,
     ``#"signal"``. Default value: :drm:`#f`.
   :parameter #key if-does-not-exist: One of :drm:`#f`, ``#"signal"``, or
     ``#"create"``. Default value: depends on the value of *direction*.
   :parameter #key buffer-size: An instance of :drm:`<integer>`.
   :parameter #key element-type: One of :type:`<byte-character>`,
     :type:`<unicode-character>`, or :type:`<byte>`, or :drm:`#f`.
   :value file-stream-instance: An instance of :class:`<file-stream>`.

   :description:

     Creates and opens a stream over a file.

     Returns a new instance of a concrete subclass of
     :class:`<file-stream>` that streams over the contents of the file
     referenced by *filename*. To determine the concrete subclass to be
     instantiated, this method calls the generic function
     :gf:`type-for-file-stream`.

     The *locator* init-keyword should be a :class:`<file-locator>` or
     a :drm:`<string>` that can be coerced to one.

     The *direction* init-keyword specifies the direction of the
     stream.  This can be one of ``#"input"``, ``#"output"``, or
     ``#"input-output"``.  The default is ``#"input"``.

     The *if-exists* and *if-does-not-exist* init-keywords specify
     actions to take if the file named by *filename* does or does not
     already exist when the stream is created. These init-keywords are
     discussed in more detail in `Options when creating file streams`_.

     The *buffer-size* init-keyword can be used to suggest the size of
     a stream's buffer. See :class:`<buffered-stream>`.

     The *element-type* init-keyword specifies the type of the elements
     in the file named by *filename*. This allows file elements to be
     represented abstractly; for instance, contiguous elements could be
     treated as a single database record. This init-keyword defaults to
     something useful, potentially based on the properties of the file;
     :type:`<byte-character>` and :type:`<unicode-character>` are likely choices.
     See `Options when creating file streams`_.

   :seealso:

     - :class:`<buffered-stream>`
     - :class:`<file-stream>`
     - :gf:`type-for-file-stream`

.. class:: <microsoft-server-locator>
   :sealed:
   :abstract:

   The abstract superclass of all servers using Microsoft protocols.

   :superclasses: :class:`<server-locator>`

   :seealso: :class:`<microsoft-unc-locator>`
	     :class:`<microsoft-volume-locator>`

.. class:: <microsoft-unc-locator>
   :sealed:

   A server located using Microsoft's Universal Naming Convention,
   for example ``\\ComputerName\Share``

   :superclasses: :class:`<microsoft-server-locator>`

.. class:: <microsoft-volume-locator>
   :sealed:

   A server located using a volume name (drive letter) on a Microsoft
   system, for example ``C``.

   :superclasses: :class:`<microsoft-server-locator>`

.. class:: <microsoft-file-system-locator>
   :abstract:

   The abstract superclass of files and directories on Microsoft file systems.

   :superclasses: :class:`<file-system-locator>`

.. class:: <microsoft-directory-locator>

   A directory on a Microsoft file system.

   :superclasses: :class:`<microsoft-file-system-locator>`, :class:`<directory-locator>`

   :slot locator-server: the server which holds this directory.

.. class:: <microsoft-file-locator>

   A file on a Microsoft file system.

   :superclasses: :class:`<microsoft-file-system-locator>`, :class:`<file-locator>`

   :slot locator-directory: the directory that holds this file.
   :slot locator-base: the file name without extension.
   :slot locator-extension: the file extension.

.. constant:: <native-file-system-locator>

   File system locator bound to the host system locator.

   :description:

     A native file system locator is specific to the host system it is running
     on. For example, if the host system is Posix, the file locator is bound to
     :class:`<posix-file-system-locator>`, and if the host system is Microsoft,
     it is bound to :class:`<microsoft-file-system-locator>`.

   :seealso:

     - :class:`<posix-file-system-locator>`
     - :class:`<microsoft-file-system-locator>`

.. type:: <pathname>

   The type representing a file system entity.

   :equivalent: ``type-union(<string>, <file-system-locator>)``

   :description:

     A type that identifies a file system entity. This can be either a
     :drm:`<string>` or a :class:`<file-system-locator>`.

   :operations:

     - :func:`copy-file`
     - :func:`create-directory`
     - :func:`delete-directory`
     - :func:`delete-file`
     - :func:`do-directory`
     - :func:`ensure-directories-exist`
     - :func:`file-exists?`
     - :func:`file-properties`
     - :func:`file-property`
     - :func:`file-property-setter`
     - :func:`file-type`
     - :func:`home-directory`
     - :func:`link-target`
     - :func:`rename-file`
     - :func:`create-symbolic-link`

.. class:: <posix-file-system-locator>
   :abstract:
   :sealed:

   The abstract superclass of files and directories on a posix-like
   file system.

   :superclasses: :class:`<file-system-locator>`

.. class:: <posix-directory-locator>
   :sealed:

   A directory on a posix-like file system.

   :superclasses: :class:`<file-system-directory-locator>`, :class:`<posix-file-system-locator>`

.. class:: <posix-file-locator>
   :sealed:

   A file on a posix-like file system.

   :superclasses: :class:`<file-system-file-locator>`, :class:`<posix-file-system-locator>`

   :slot locator-directory: the directory that holds this file.
   :slot locator-base: the file name without extension.
   :slot locator-extension: the file extension.

.. function:: rename-file

   Renames a specified file.

   :signature: rename-file *old-file* *new-file* #key *if-exists* => ()

   :parameter old-file: An instance of :type:`<pathname>`.
   :parameter new-file: An instance of :type:`<pathname>`.
   :parameter if-exists: An instance of
     :type:`<copy/rename-disposition>`. Default value: ``#"signal"``.

   :description:

     Renames *old-file* to *new-file*. If *new-file* already exists, the
     action of this function is controlled by the value of *if-exists*.
     The default is to prompt you before overwriting an existing file.

     This operation may fail if the source and destination are not on
     the same file system.

   :seealso:

     - :func:`copy-file`
     - :type:`<copy/rename-disposition>`

.. function:: root-directories

   Returns a sequence containing the pathnames of the root directories of
   the file systems on the local machine.

   :signature: root-directories () => *roots*

   :value roots: An instance of :drm:`<sequence>`.

   :description:

     Returns a sequence containing the pathnames of the root directories
     of the file systems on the local machine.

.. generic-function:: shorten-pathname

   Given a pathname, returns the shortest equivalent form.

   :signature: shorten-pathname *path* => *shortened-path*

   :param path: An instance of :class:`<pathname>`.
   :value shorten-pathname: An instance of :class:`<pathname>`.

   :description:

   Given a pathname, returns the shortest equivalent form. For instance a DOS 
   pathname on Windows.

.. method:: shorten-pathname
   :specializer: <file-system-locator>

   A specialization of :gf:`shorten-pathname`.

   :param path: An instance of :class:`<file-system-locator>`

.. function:: temp-directory

   Returns the pathname of the temporary directory in use.

   :signature: temp-directory () => *temp-directory*

   :value temp-directory: An instance of :type:`<pathname>`, or false.

   :description:

     Returns the pathname of the temporary directory in use. The return
     value of this function can be used with :drm:`concatenate` to
     create pathnames of entities in the temporary directory. If no
     temporary directory is defined, ``temp-directory`` returns :drm:`#f`.
     On Windows the temporary directory is specified by the ``TMP``
     environment variable.

.. macro:: with-open-file
   :statement:

   Runs a body of code within the context of a file stream.

   :macrocall:
     .. parsed-literal:: 
        with-open-file (`stream-var` = `filename`, #rest `keys`)
          `body`
        end => `values`

   :parameter stream-var: An Dylan variable-name *bnf*.
   :parameter filename: An instance of :drm:`<string>`.
   :parameter keys: Instances of :drm:`<object>`.
   :parameter body: A Dylan body *bnf*.
   :value values: Instances of :drm:`<object>`.

   :description:

     Provides a safe mechanism for working with file streams. The macro
     creates a file stream and binds it to *stream-var*, evaluates a
     *body* of code within the context of this binding, and then closes
     the stream. The macro calls :gf:`close` upon exiting *body*.

     The values of the last expression in *body* are returned.

     Any *keys* are passed to the :meth:`make <make(<file-stream>)>`
     method on :class:`<file-stream>`.

   :example:

     The following expression yields the contents of file *foo.text* as
     a :class:`<byte-vector>`:

     .. code-block:: dylan

       with-open-file (fs = "foo.text", element-type: <byte>)
         read-to-end(fs)
       end;

     It is roughly equivalent to:

     .. code-block:: dylan

       begin
         let hidden-fs = #f; // In case the user bashes fs variable
         block ()
           hidden-fs := make(<file-stream>,
                             locator: "foo.text", element-type: <byte>);
           let fs = hidden-fs;
           read-to-end(fs);
         cleanup
           if (hidden-fs) close(hidden-fs) end;
         end block;
       end;

   :seealso:

     - :meth:`close(<file-stream>)`
     - :class:`<file-stream>`
     - :meth:`make(<file-stream>)`

.. function:: working-directory

   Returns the working directory for the current process.

   :signature: working-directory () => *working-directory*

   :value working-directory: An instance of :type:`<pathname>`.

   :description:

     Returns the :type:`<pathname>` of the current working directory in
     the current process on the local machine. You can use the return
     value of ``working-directory`` in conjunction with
     :drm:`concatenate` to specify pathnames of entities in the working
     directory.

   :seealso:

     - :func:`working-directory-setter`

.. function:: working-directory-setter

   Sets the working directory for the current process.

   :signature: working-directory-setter *directory* => *directory*

   :parameter directory: An instance of :type:`<pathname>`.
   :value directory: An instance of :type:`<pathname>`.

   :description:

     Sets the working directory for the current process.

     Note that the following calls are equivalent

     .. code-block:: dylan

       working-directory() := "C:\\USERS\\JOHN\\FOO.TEXT";
       working-directory() := "C:\\USERS\\JOHN\\";

     as they will both set the working directory to *C:\\USERS\\JOHN*. The
     call

     .. code-block:: dylan

       working-directory() := "C:\\USERS\\JOHN";

     is not equivalent as it sets the working directory to *C:\\USERS*.

   :example:

     .. code-block:: dylan

       working-directory() := "C:\\USERS\\JOHN\\";

   :seealso:

     - :func:`working-directory`
