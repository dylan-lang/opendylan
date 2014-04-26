Weak tables
-----------

We have extended ``define table`` to incorporate *weak references*
through keys and values.

A weak reference is an reference that the garbage collector treats as
irrelevant to establishing whether the object referred to is live. If an
object has only weak references to it, the garbage collector can delete
the reference and recycle the object’s memory. We call a normal
reference a *strong reference*.

Weak references are a useful tool for building data structures where you
do not want the garbage collector to preserve objects in the structure
on account of certain references merely used to build up the structure.

Typically, this level of control is not required in a language like
Dylan, which does not expose memory references to programs. But without
the ability to tell the garbage collector to disregard certain kinds of
reference, data structures such as tables could be bloated unnecessarily
by the garbage collector preserving entries (a key/value pair) solely
because the table object itself has a reference to the entry’s key or
value.

Common Dylan provides weakness options for instances of :drm:`<table>`. A
table can have *weak keys* or *weak values*:

.. code-block:: dylan

    make(<table>, weak: #"key"); // makes a weak-key table

    make(<table>, weak: #"value"); // makes a weak-value table

In a weak-keyed table, if a key is no longer referenced from anywhere
else in the program (apart from weak references, including from the same
table), then the entry (key and value) can be deleted from the table.
After that, the key object will be recycled. The value will also be
recycled unless it has strong references from elsewhere in the program.

Weak-valued tables are much the same, except that the focus is values
and not keys. In a weak-valued table, if a value is no longer referenced
from anywhere else in the program (apart from weak references, including
from the same table), then the entry (value and key) can be deleted from
the table. After that, the value object will be recycled. The key will
also be recycled unless it has strong references from elsewhere in the
program.

Weak tables are useful for implementing many sorts of cache, where the
cached data is recomputable and yet both expensive to compute and also
expensive to keep for a long time. For example, consider something like
a font cache for an X Window System server, or a printer. Fonts might be
looked up by name, so the strings would be the keys of the table. The
values would be the bitmaps for the font. While the X server is using a
font, the cache will be kept alive — so any further requests to select
the font will find the data already present. However, if the font is not
used then you would eventually expect the garbage collector to clean it
out. Any future request would then have to re-load all the bitmaps.


