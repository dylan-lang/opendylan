***************************
The hash-algorithms Library
***************************

.. current-library:: hash-algorithms
.. current-module:: hash-algorithms

The hash-algorithms library provides consistent access to a variety of
hash algorithms:

* md5
* sha1
* sha224
* sha256
* sha384
* sha512

Hashing an entire string at once can be done via the functions named
after each hash:

.. code-block:: dylan

    let digest = sha1("Some text");

If you want a printable digest, use :meth:`hexdigest(<byte-vector>)`:

.. code-block:: dylan

    let hexdigest = hexdigest(sha1("Some text"));

If you want to hash multiple strings into a single digest (useful when streaming),
you can use the :gf:`update-hash` and :gf:`digest` functions:

.. code-block:: dylan

    let hash = make(<sha1>);
    update-hash(hash, "Some");
    update-hash(hash, " ");
    update-hash(hash, "text");
    let digest = digest(hash);
    // hexdigest works on hashes as well:
    let hexdigest = hexdigest(hash);

The hash-algorithms Module
==========================

.. class:: <hash>

   :superclasses: :drm:`<object>`

.. generic-function:: digest-size

   Returns the digest size of the hash algorithm.

   :signature: digest-size (hash) => (digest-size)

   :parameter hash: An instance of :class:`<hash>`.
   :value digest-size: An instance of :drm:`<integer>`.

.. generic-function:: block-size

   Returns the block size of the hash algorithm.

   :signature: block-size (hash) => (block-size)

   :parameter hash: An instance of :class:`<hash>`.
   :value block-size: An instance of :drm:`<integer>`.

.. generic-function:: update-hash

   Add more data to the hash.

   :signature: update-hash (hash, input) => ()

   :parameter hash: An instance of :class:`<hash>`.
   :parameter input: An instance of :drm:`<byte-string>`, :class:`<buffer>` or :class:`collections:byte-vector:<byte-vector>`.

   :description:

      Add more data to the hash. This is useful when streaming data or the data is
      available in multiple strings and you wish to avoid the overhead of concatenation.

      Calling ``update-hash`` multiple times is equivalent to calling it once with
      a concatenation of the arguments:

      .. code-block:: dylan

        let hash-separate = make(<sha1>);
        update-hash(hash-separate, "Some");
        update-hash(hash-separate, " ");
        update-hash(hash-separate, "text");
        let digest-separate = digest(hash-separate);

        let hash-combined = make(<sha1>);
        update-hash(hash-combined, "Some text");
        let digest-combined = digest(hash-combined);

        // digest-separate and digest-combined will be the same

   :seealso:

     - :gf:`digest`
     - :meth:`hexdigest(<hash>)`
     - :meth:`hexdigest(<byte-vector>)`

.. generic-function:: digest

   :signature: digest (hash) => (digest)

   :parameter hash: An instance of :class:`<hash>`.
   :value digest: An instance of :class:`collections:byte-vector:<byte-vector>`.

   :description:

      The return value *digest* is binary data and may include null bytes. To display
      this result in text form, use :meth:`hexdigest(<hash>)` or
      :meth:`hexdigest(<byte-vector>)`.

      Use :gf:`update-hash` to add data to the hash.

   :seealso:

     - :gf:`update-hash`
     - :meth:`hexdigest(<hash>)`
     - :meth:`hexdigest(<byte-vector>)`

.. method:: hexdigest
   :specializer: <hash>

   Returns the digest for the given hash as a hexadecimal string.

   :signature: hexdigest (hash) => (hexdigest)

   :parameter hash: An instance of :class:`<hash>`.
   :value hexdigest: An instance of :drm:`<byte-string>`.

   :seealso:

     - :gf:`digest`
     - :meth:`hexdigest(<byte-vector>)`

.. method:: hexdigest
   :specializer: <byte-vector>

   Returns the digest given as a hexadecimal string.

   :signature: hexdigest (digest) => (hexdigest)

   :parameter digest: An instance of :class:`collections:byte-vector:<byte-vector>`.
   :value hexdigest: An instance of :drm:`<byte-string>`.

   :seealso:

     - :gf:`digest`
     - :meth:`hexdigest(<hash>)`

MD5
---

.. class:: <md5>

   :superclasses: :class:`<hash>`

.. function:: md5

   :signature: md5 (input) => (digest)

   :parameter input: An instance of :drm:`<byte-string>`, :class:`<buffer>` or :class:`collections:byte-vector:<byte-vector>`.
   :value digest: An instance of :class:`collections:byte-vector:<byte-vector>`.

SHA-1
-----

.. class:: <sha1>

   :superclasses: :class:`<hash>`

.. function:: sha1

   :signature: sha1 (input) => (digest)

   :parameter input: An instance of :drm:`<byte-string>`, :class:`<buffer>` or :class:`collections:byte-vector:<byte-vector>`.
   :value digest: An instance of :class:`collections:byte-vector:<byte-vector>`.

SHA-2
-----

.. class:: <sha256>

   :superclasses: :class:`<hash>`

.. function:: sha256

   :signature: sha256 (input) => (digest)

   :parameter input: An instance of :drm:`<byte-string>`, :class:`<buffer>` or :class:`collections:byte-vector:<byte-vector>`.
   :value digest: An instance of :class:`collections:byte-vector:<byte-vector>`.

.. class:: <sha224>

   :superclasses: :class:`<hash>`

.. function:: sha224

   :signature: sha224 (input) => (digest)

   :parameter input: An instance of :drm:`<byte-string>`, :class:`<buffer>` or :class:`collections:byte-vector:<byte-vector>`.
   :value digest: An instance of :class:`collections:byte-vector:<byte-vector>`.

.. class:: <sha384>

   :superclasses: :class:`<hash>`

.. function:: sha384

   :signature: sha384 (input) => (digest)

   :parameter input: An instance of :drm:`<byte-string>`, :class:`<buffer>` or :class:`collections:byte-vector:<byte-vector>`.
   :value digest: An instance of :class:`collections:byte-vector:<byte-vector>`.

.. class:: <sha512>

   :superclasses: :class:`<hash>`

.. function:: sha512

   :signature: sha512 (input) => (digest)

   :parameter input: An instance of :drm:`<byte-string>`, :class:`<buffer>` or :class:`collections:byte-vector:<byte-vector>`.
   :value digest: An instance of :class:`collections:byte-vector:<byte-vector>`.
