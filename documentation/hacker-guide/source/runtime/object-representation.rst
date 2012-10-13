Object Representation
=====================

Harlequin’s implementation achieves dynamic typing of Dylan objects by
associating the type with an object based on tagging.

In many circumstances, the Dylan compiler can statically determine the
type of an object. This knowledge can be used to select an alternative
representation which is more efficient than the canonical
representation. For example, the canonical representation of a double
float object in Dylan is as a pointer to heap-allocated storage which
contains the IEEE bit pattern of the double float in addition to a
reference to the Dylan class object ``<double-float>``. The compiler may
choose to represent the value as a direct bit pattern, wherever this
does not violate the semantics of the program.

Tagging Scheme
--------------

All Dylan values are represented as data of the same size, the size of a
pointer. The bit pattern of these values contains tag bits which
indicate whether the value is actually a pointer, or whether it is a
direct value. Since there are three major groups (integers, characters
and everything else), the representation for all platforms is to use two
bits.

+----------+-----------------+
| Tag Bits | Type            |
+==========+=================+
| 00       | Heap Allocated  |
+----------+-----------------+
| 01       | Integers        |
+----------+-----------------+
| 10       | Characters      |
+----------+-----------------+
| 11       | Unused          |
+----------+-----------------+

Integers and Characters
-----------------------

Integers and characters are represented as direct values, using the tag
bits as the only indication of type. The tagging scheme uses the least
significant two bits. With this scheme, a character or integer is
converted to its untagged representation by arithmetic right shifting by
two bits. Similarly the conversion from an untagged to a tagged
representation is to shift left and add in the tag bits.

Operations on these values (e.g., addition, or other arithmetic
operations) are always performed on the untagged representation. This is
sub-optimal, because it is possible to perform arithmetic operations
directly on the tagged values. It is planned to improve this mechanism
at a later date, along with a revision of the tagging scheme.

Boxed Objects
-------------

Apart from integers and characters, all Dylan objects are indirectly
represented as *boxed* values (that is, they are pointers to heap
allocated boxes). The runtime system is responsible for ensuring that
these boxed values are appropriately tagged, because the runtime system
provides the allocation service, and must ensure appropriate alignment.

Boxed objects are dynamically identified by their first slot, which is
an identification wrapper. This identification wrapper (itself a boxed
Dylan object) contains a pointer to the class of the object it is
wrapping, as well as some encoded information for the garbage collector
about which slots should be traced.

.. figure:: ../images/runtime-2.png
   :align: center

Boxed Objects
-------------

Note that two indirections are necessary to find the class of an object.
In practice, this is a rare operation, because almost all dynamic class
testing within Dylan is implicit, and the implementation can use the
wrapper for these implicit tests. Note that there is potentially a
many-to-one correspondence between wrapper objects and class objects.

The Dylan compiler builds literal boxed objects statically whenever it
can. In practice, this will include most function objects apart from
closures, virtually all wrappers, and most class objects, as well as
strings, symbols, and literal vectors and lists.

Variably Sized Objects
----------------------

Variably sized objects, such as strings, vectors and arrays, are boxed
objects which contain a *repeated slot*. The repeated slot is
implemented as a variably sized data area preceded by a normal slot
containing the size of the variably sized data represented as a tagged
integer. The size slot is used at the Dylan language level to determine
the size of the array. There is also a special encoding for it in the
tracing data of the wrapper so that the memory manager knows how to
trace the repeated data. For example, an instance of the ``<byte-string>``
*"foo"* is represented as in:.

.. figure:: ../images/runtime-3.png
   :align: center

   An Instance of ``<byte-string>``

Function Objects
----------------

Dylan provides two built-in classes of functions: ``<generic-function>``
and ``<method>``. These both obey the same general purpose calling
convention, but also support specialized calling conventions (described
below) which the compiler may use depending on the detail of its
knowledge about the function being called, and the circumstances. Slots
in the function object point to the code which implements each
convention.

All functions also have a slot which encodes the number of required
parameters the function accepts, and whether the function accepts
optional or keyword parameters. Another slot in each function object
contains a vector of the types which are acceptable for each required
parameter. These slots are used for consistency checking of the
arguments.

Generic functions have further slots which support the method
dispatching process — including a slot which contains a vector of all
the methods belonging to the generic function, and a slot which contains
a cache of sorted applicable methods for combinations of arguments which
have been processed before.

Methods may be closures, in which case a slot in the method object
contains the environment for the method, which is represented as a
vector of closed-over variables. If the variable is known actually to be
constant, then the constant value is stored directly in the vector.
Alternatively, if there is any possibility of an assignment to the
variable, then the value is stored with an extra indirection to a *value
cell*, which may be shared between many closures with related
environments.
