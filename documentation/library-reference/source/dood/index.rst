****************
The DOOD library
****************

.. current-library:: dood

The Dylan Object Oriented Database
==================================

The Dylan Object Oriented Database (DOOD) is a simple mechanism for
storing arbitrary objects and lazily loading them. During dump time,
DOOD traverses a graph of objects and encodes the objects as a
sequence of bytes. These bytes are later interpreted by DOOD during
load time reconstructing an isomorphic object-graph in which cyclic
structures and shared references are preserved. DOOD provides ways to
control what slots in objects should be stored, and to decide what
objects should be stored by proxies.

DOOD was meant to be a very simple Dylan object store that supported:

- pay as you go
- incremental loading,
- flexible proxies, and
- non-corrupting commits

DOOD was not meant to provide:

- multiuser support,
- full blown transaction support,
- incremental writes,
- schema evolution, nor
- client/server support

In order to add persistence to a program, the easiest thing is to
just save and load the data completely:

.. code-block:: dylan

   define method dump-data (data, locator)
     let dood = make(<dood>, locator: locator, direction: #"output",
                     if-exists: #"replace");
     dood-root(dood) := data;
     dood-commit(dood);
     dood-close(dood);
   end method;

   define method load-data (locator) => (data)
     let dood = make(<dood>, locator: locator, direction: #"input");
     let data = dood-root(dood);
     dood-close(dood);
     data
   end method;

This works great for simple applications. More complicated
applications potentially require support for data compression, special
reinitialization, lazy loading of data, and multiple databases. Data
can be compressed by being able to specify that only a subset of an
object's slots are saved (for example, an object may cache information
in a slot) or more generally by dumping the object's information in a
completely different format on disk. The former technique is described
in the schema section below and the latter technique is described in
the proxy section below.

The DOOD module
---------------

.. current-module:: dood

.. class:: <dood>
   :open:
   :primary:

   :superclasses: :class:`<object>`

   :keyword if-exists: An instance of :class:`<object>`.  Specifies
                       the actions to take during creation of a file
                       stream when the ``locator:`` init-keyword is
                       specified. If ``if-exists:``
                       init-keyword specifies
                       ``#"replace"`` then the database
                       is considered empty.

   :keyword backups?: An instance of :class:`<boolean>`. Specifies
                      whether a new file is used during a
                      commit. ``backups?`` is always false when the
                      ``stream:`` init-keyword is specified.

   :keyword batch-mode?: An instance of :class:`<boolean>`.

   :keyword default-segment: An instance of :class:`<dood-segment>`.

   :keyword locator: An instance of :class:`<object>`. Should be a
                     object valid for opening a stream with a
                     ``locator:`` init-keyword.

   :keyword name: An instance of :class:`<object>`. Usually a string
                  or symbol but user-controlled.
   :keyword read-only?: An instance of :class:`<boolean>`.
   :keyword segments: An instance of :class:`<simple-object-vector>`.
   :keyword stream: An instance of ``false-or(<stream>)``.

   :keyword version: An instance of :class:`<integer>`. Specifies a
                     version that can be used for version
                     control. Upon opening of an existing database,
                     the specified version is compared against the
                     stored version, and if they are different a
                     :class:`<dood-user-version-warning>` condition is
                     signaled.

   :keyword world: An instance of :class:`<dood-world>`.

   This class can be user subclassed and used as the basis for specialized
   loading and dumping behavior.

.. class:: <dood-opening-warning>

   :superclasses: :class:`<dood-warning>`

   :keyword required dood: An instance of :class:`<dood>`.

   Superclass of all dood warnings.

.. class:: <dood-corruption-warning>

   :superclasses: :class:`<dood-opening-warning>`

   Signaled if DOOD data is found to be corrupted.

.. class:: <dood-version-warning>

   :superclasses: :class:`<dood-opening-warning>`

   Signaled if the current DOOD version is different from the saved
   DOOD version.

.. class:: <dood-user-version-warning>

   :superclasses: :class:`<dood-version-warning>`

   Signaled if the specified user version is different from the saved
   user version.

.. method:: dood-name
   :specializer: <dood>

   Returns the name of the specified dood.

.. generic-function:: dood-root

   :signature: dood-root (object) => (value)

   :parameter object: An instance of :class:`<dood>`
   :value value: An instance of :class:`<object>`.

   Returns the one distinguished root of the specified dood. It is
   defaulted to be :drm:`#f` when a new dood is created.

.. generic-function:: dood-root-setter

   :signature: dood-root-setter (value object) => (value)

   :parameter value: An instance of :class:`<object>`.
   :parameter object: An instance of ``{<dood> in dood}``.
   :value value: An instance of :class:`<object>`.

   Sets the one distinguished root of the specified dood.

.. generic-function:: dood-commit

   :signature: dood-commit (dood #key flush? dump? break? parents? clear? stats? size) => ()

   :parameter dood: An instance of :class:`<dood>`.
   :parameter #key flush?: An instance of :class:`<object>`.
   :parameter #key dump?: An instance of :class:`<object>`.
   :parameter #key break?: An instance of :class:`<object>`.
   :parameter #key parents?: An instance of :class:`<object>`.
   :parameter #key clear?: An instance of :class:`<object>`.
   :parameter #key stats?: An instance of :class:`<object>`.
   :parameter #key size: An instance of :class:`<object>`.

   Saves the data reachable from :func:`dood-root`. When the ``backups?:``
   init-keyword is specified to be true, the data is first written to
   a new file. The new file is named the same as the specified locator
   but with its suffix changed to “new”. Upon success, the original
   data file is replaced with the new file. Upon failure the new file
   is just removed and the original data file is untouched.

.. generic-function:: dood-size

   :signature: dood-size (dood) => (res)

   :parameter dood: An instance of :class:`<dood>`.
   :value res: An instance of :class:`<integer>`.

   Returns the size in bytes of the data file.

.. generic-function:: dood-close

   :signature: dood-close (dood #rest all-keys #key abort?) => ()

   :parameter dood: An instance of :class:`<dood>`.
   :parameter #rest all-keys: An instance of :class:`<object>`.
   :parameter #key abort?: An instance of :class:`<object>`.

   Closes the specified database and underlying file stream if created
   with ``locator:`` init-keyword. If the ``stream:`` init-keyword was
   specified then it is the user's responsibility to close this
   stream. Abort is passed through to close an underlying file stream.

Schemas
-------

Schemas are declarative descriptions of how objects are persistently
dumped and loaded. In the current version of DOOD, schemas are not
themselves persistently stored. User versions can be used to manually
ensure compatible schemas.

.. macro:: dood-class-definer

   The ``dood-class-definer`` macro defines a dylan class with extra
   slot adjectives specifying the dumping and loading behavior of the
   corresponding slot. The default DOOD treatment of a slot, called
   ``deep``, is that its contents is recursively dumped and eagerly
   loaded. There are three dood slot adjectives that modify this
   behavior: ``lazy``, ``disk``, and ``weak.`` A ``lazy`` slot's
   contents is recursively dumped and lazily loaded, that is, loaded
   from disk upon first access. A ``disk`` slot's contents is
   recursively dumped and is always loaded from disk when and only
   when explicitly accessed and is never written back to the slot. A
   ``weak`` slot's contents is never dumped and a user can specify a
   ``reinit-expression`` to be used instead during loading. A
   ``reinit-expression`` must be specified even if an
   ``init-expression`` is the same, otherwise reinitialization will
   not occur and the slot will be unbound. In the current version of
   DOOD, the ``reinit-expression`` must appear as the first slot
   keyword parameter if at all. Accessing ``lazy`` slot values in a
   closed database will signal a ``dood-proxy-error`` (see below).

   :example:

.. code-block:: dylan

   define dood-class <computation> (<object>)
     lazy slot computation-source-location :: false-or(<source-location>) = #f,
       init-keyword: source-location:;
     slot computation-previous :: <compution>,
       required-init-keyword: previous:;
     slot computation-next :: <computation>,
       required-init-keyword: previous:;
     weak slot computation-type :: false-or(<type-estimate>) = #f,
       reinit-expression: #f;
   end dood-class;

Reading
-------

Internally DOOD loads objects by instantiation and slot assignment. An
object is instantiated via the internal system allocator, which
returns an uninitialized instance, and then initialized by applying
the setters of an object's class.

.. generic-function:: dood-reinitialize
   :open:

   :signature: dood-reinitialize (dood object) => ()

   :parameter dood: An instance of :class:`<dood>`.
   :parameter object: An instance of :class:`<object>`.

   For some objects the simple instantiation and slot assignment
   approach will not produce a well-formed object. :gf:`dood-reinitialize`
   gives objects a chance to correct any reconstruction problems. This
   function is called on an object immediately after the object has
   been loaded from disk.

   :example:

.. code-block:: dylan

   define dood-class <rectangle> (<object>)
     slot rectangle-height :: <integer>,
       required-init-keyword: height:;
     slot rectangle-width :: <integer>,
       required-init-keyword: width:;
     weak rectangle-area :: <integer>;
   end dood-class;

   define method dood-reinitialize (dood :: <dood>, object :: <rectangle>)
     next-method();
     rectangle-area(object)
       := rectangle-height(object) * rectangle-width(object);
   end method;

Tables
------

.. class:: <dood-lazy-symbol-table>

   :superclasses: :class:`<dood-lazy-key-table>`

   Provide a mechanism for indexes. The keys are symbols and are
   loaded lazily using a binary search. This is known to be an
   inferior layout strategy and will be replaced by b\*-trees in the
   future.

.. generic-function:: dood-lazy-forward-iteration-protocol

   :signature: dood-lazy-forward-iteration-protocol (table) => (#rest results)

   :parameter table: An instance of :class:`<object>`.
   :value #rest results: An instance of :class:`<object>`.

   Used for walking only keys presently loaded. The standard
   :drm:`forward-iteration-protocol` will load all keys and values
   into memory.

Proxies
-------

Sometimes users need more control over how objects are dumped to
disk. DOOD provide a general mechanism called a proxy, which provides
both a disk representation of an object and a reconstruction
policy. The basic idea is that during the dumping process each memory
object is given a chance to provide a disk object (a proxy) to be used
for dumping and then upon loading, a loaded disk object is given a
chance to map back to its original memory object. Proxies can be used
for mapping objects back to unique runtime objects, for compressing
objects, for looking up objects in external databases, etc.

.. class:: <dood-proxy>
   :open:

   :superclasses: :class:`<dood-mapped-and-owned-object>`

   This is the superclass of all proxy objects. Users must subclass
   this class in order to define a new kind of proxy.

.. generic-function:: dood-disk-object
   :open:

   :signature: dood-disk-object (dood object) => (disk-object)

   :parameter dood: An instance of :class:`<dood>`.
   :parameter object: An instance of :class:`<object>`.
   :value disk-object: An instance of :class:`<object>`.

   Users write methods on this generic when they want an object to
   have a proxy. It returns a disk-object which is dumped in lieu of
   the memory-object.

.. method:: dood-disk-object
   :specializer: <dood>, <object>

.. method:: dood-disk-object
   :specializer: <dood>, <dood-mapped-and-owned-object>

.. method:: dood-disk-object
   :specializer: <dood>, <generic-function>

.. method:: dood-disk-object
   :specializer: <dood>, <function>

.. method:: dood-disk-object
   :specializer: <dood>, <class>

.. method:: dood-disk-object
   :specializer: <dood>, <integer>

.. generic-function:: dood-restore-proxy
   :open:

   :signature: dood-restore-proxy (dood proxy) => (memory-object)

   :parameter dood: An instance of :class:`<dood>`.
   :parameter proxy: An instance of :class:`<dood-proxy>`.
   :value memory-object: An instance of :class:`<object>`.

   This function is called immediately after a proxy is reconstructed
   with instantiation and slot assignment. Its job is to map from a
   disk-object back to its memory-object.

.. method:: dood-restore-proxy
   :specializer: <dood>, <dood-program-module-proxy>

.. method:: dood-restore-proxy
   :specializer: <dood>, <dood-program-binding-proxy>

.. method:: dood-restore-proxy
   :specializer: <dood>, <dood-class-program-binding-proxy>

.. class:: <dood-proxy-error>
   :open:

   :superclasses: :class:`<error>`

   Signaled when proxy is restored from closed database.

Proxy examples
--------------

Dump by Reference
^^^^^^^^^^^^^^^^^

The first example shows how proxies can be used to dump objects by
reference back to objects in a user's program. This is necessary when
the data can not be dumped by DOOD (e.g., functions).

.. code-block:: dylan

   define constant $boot-objects = make(<table>);
   define class <boot-object> (<object>)
     slot boot-id :: <integer>, required-init-keyword: id:;
     slot boot-function :: <function>, required-init-keyword: function:;
   end class;

   define method initialize (object :: <boot-object>, #key id, #all-keys)
     next-method();
     $boot-objects[id] := object;
   end method;

   define class <boot-proxy> (<dood-proxy>)
     slot boot-id :: <integer>, required-init-keyword: id:;
   end class;

   define method dood-disk-object
       (dood :: <dood>, object :: <boot-object>) => (proxy :: <boot-proxy>)
     make(<boot-proxy>, id: boot-id(object))
   end method;

   define method dood-restore-proxy
       (dood :: <dood>, proxy :: <boot-proxy>) => (object :: <boot-object>)
     $boot-objects[boot-id(proxy)]
   end method;

Compression
^^^^^^^^^^^

The second example shows how proxies can be used to compress an object's
disk representation.

.. code-block:: dylan

   define class <person> (<object>)
     slot person-gender :: one-of(#"male", #"female"), 
       required-init-keyword: gender:;
     slot person-height :: <integer>, 
       required-init-keyword: height:;
   end class;

   define constant <person-code> = <integer>;

   define method encode-person (person :: <person>) => (code :: <person-code>)
     if (person-gender(person) == #"male") 0 else 1 end
         + (person-height(person) * 2)
   end method;

   define method decode-person (code :: <person-code>) => (person :: <person>)
     person-gender(person) := if (even?(code)) #"male" else #"female";
     person-height(person) := truncate/(code, 2);
   end method;

   define class <person-proxy> (<dood-proxy>)
     slot person-code :: <person-code>, required-init-keyword: code:;
   end class;

   define method dood-disk-object 
       (dood :: <dood>, object :: <person>) => (proxy :: <person-proxy>)
     make(<person-proxy>, code: encode-person(object))
   end method;

   define method dood-restore-proxy 
       (dood :: <dood>, proxy :: <person-proxy>) => (object :: <person>)
     make(<person>, decode-person(person-code(object)))
   end method;

Multiple Databases
^^^^^^^^^^^^^^^^^^

The third example demonstrates how to use proxies for interdatabase
references. Suppose that each database is registered in a symbol-table
of databases and that each object stored in these databases knows both
its name and to which database it belongs. Furthermore, suppose that
each database has a lazy symbol-table stored as its distinguished
root.

.. code-block:: dylan

   define class <dooded-object> (<object>)
     slot object-dood-name,    required-init-keyword: dood-name:;
     slot object-binding-name, required-init-keyword: binding-name:;
     // ...
   end class;

   define class <dood-cross-binding-proxy> (<dood-proxy>)
     slot proxy-dood-name,    required-init-keyword: dood-name:;
     slot proxy-binding-name, required-init-keyword: binding-name:;
   end class;

   define method dood-external-object (dood :: <dood>, name :: <symbol>)
     let symbol-table = dood-root(dood);
     element(symbol-table, name, default: #f)
   end method;

   define constant $doods = make(<table>);

   define method lookup-dood (name :: <symbol>) => (dood :: <dood>)
     element($doods, name, default: #f)
       | (element($doods, name)
          := make(<dood>, locator: as(<string>, name), direction: #"input"))
   end method;

   define method dood-restore-proxy
       (dood :: <dood>, proxy :: <dood-cross-binding-proxy>) => (object)
     let external-dood = lookup-dood(proxy-dood-name(proxy);
     dood-external-object(external-dood, proxy-binding-name(proxy))
   end method;

   define method dood-disk-object
       (dood :: <dood>, object :: <dooded-object>) => (disk-object)
     if (dood-name(dood) == object-dood-name(object)) // local?
       object
     else
       make(<dood-cross-binding-proxy>,
         dood-name:    dood-name(dood),
         binding-name: object-binding-name(object))
     end if;
   end method;
