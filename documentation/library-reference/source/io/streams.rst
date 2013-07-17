******************
The streams Module
******************

.. current-library:: io
.. current-module:: streams

Introduction
------------

This chapter describes the Streams module, which allows you to establish
and control input to and output from aggregates of data, such as files
on disk, or sequences. This module, together with the Standard-IO
module, provides similar functionality to the *Java.io* package in Java.
See :doc:`standard-io`, for details about the Standard-IO module in Dylan.

`Concepts`_ discusses the basic concepts involved in streaming over data.
`Stream classes`_ describes the different classes of stream available, and
how to create them, and `Reading from and writing to streams`_ describes
how to read from and write to them.

More specialized subjects are covered next: `Locking streams`_ discusses
locking streams while they are in use; `Using buffered streams`_ describes
using buffered streams; `Wrapper streams`_ describes wrapper streams;
`Conditions`_ the different stream-specific error conditions that can be
raised.For the most part, you do not have to worry about the information
in these later sections when using streams.

Finally, `The streams Module Reference`_ gives complete details on all
interfaces in the Streams module. Each entry in this section is
arranged in alphabetical order.

Discussing error conditions
---------------------------

This chapter uses two special terms in discussions of error conditions.

When it notes that something *is an error*, this means that the result
is undefined. In particular, it does not*necessarily* mean that an error
condition will be signalled. So, for instance, the following example
text means only that the result of using *pull-stream-element* in the
case described is undefined:

It is an error to apply *pull-stream-element* to an element that has
already been read from the stream.

A given function is only guaranteed to raise an exception in response to
an error if the documentation for that function specifically states that
it will signal an error. Note that the specific error condition that is
signaled may depend on the program state; in such situations, the
specific error condition is not stated in the documentation. Consider
the following hypothetical example, which states that an implementation
must signal an error, but does not say what error must be signaled:

When *index* is a ``<stream-index>``, if it is invalid for some reason,
this function signals an error.

By contrast, the following example names the class of which the
condition signaled is guaranteed to be a general instance:

If the end of the stream is encountered and no value was supplied for
*on-end-of-stream*, *read-last-element* signals an
:class:`<end-of-stream-error>` condition.

If the name of the condition class is given, applications are permitted
to specialize error handlers on that class.

Goals of the module
-------------------

The Streams module provides:

-  A generic, easy-to-use interface for streaming over sequences and
   files. The same high-level interface for consuming or producing is
   available irrespective of the type of stream, or the types of the
   elements being streamed over.
-  Efficiency, especially for the common case of file I/O.
-  Access to an underlying buffer management protocol.

The Streams module does not address a number of related issues,
including:

-  A standard object-printing package such as Smalltalk’s *printOn:* or
   Lisp’s *print-object*, or a formatted printing facility such as
   Lisp’s *format*. These facilities are provided by the Print, Format,
   and Format-out libraries. For convenience, the Common Dylan
   library also provides simple formatting capabilities.
-  General object dumping and loading.
-  A comprehensive range of I/O facilities for using memory-mapped
   files, network connections, and so on.
-  An interface for naming files. The Locators module provides such an
   interface.
-  An interface to operating system functionality, such as file renaming
   or deleting operations. The File-System module provides such an
   interface.

Concepts
--------

A *stream* provides sequential access to an aggregate of data, such as a
Dylan sequence or a disk file. Streams grant this access according to a
metaphor of *reading* and *writing*: elements can be read from streams
or written to them.

Streams are represented as Dylan objects, and all are general instances
of the class :class:`<stream>`, which the Streams module defines.

It is usual to say that a stream is established *over* the data
aggregate. Hence, a stream providing access to the string *"hello
world"* is said to be a stream over the string *"hello world"*.

Streams permitting reading operations are called *input* streams. Input
streams allow elements from the underlying data aggregate to be
consumed. Conversely, streams permitting writing operations are called
*output* streams. Output streams allow elements to be written to the
underlying data aggregate. Streams permitting both kinds of operations
are called *input-output* streams.

The Streams module provides a set of functions for reading elements from
an input stream. These functions hide the details of indexing,
buffering, and so on. For instance, the function :gf:`read-element`
reads a single data element from an input stream.

The following expression binds *stream* to an input stream over the
string *"hello world"*:

.. code-block:: dylan

    let stream = make(<string-stream>, contents: "hello world");

The first invocation of :gf:`read-element` on *stream* returns the
character "h", the next invocation "e", and so on. Once a stream has
been used to consume all the elements of the data, the stream is said to
be at its end. This condition can be tested with the function
:gf:`stream-at-end?`. The following code fragment applies *my-function*
to all elements of the sequence:

.. code-block:: dylan

    let stream = make(<sequence-stream>, contents: seq);
    while (~stream-at-end?(stream))
      my-function(read-element(stream));
    end;

When all elements of a stream have been read, further calls to
:gf:`read-element` result in the :class:`<end-of-stream-error>`
condition being signaled. An alternative end-of-stream behavior is to
have a distinguished end-of-stream value returned. You can supply such
an end-of-stream value as a keyword argument to the various read
functions; the value can be any object. Supplying an end-of-stream value
to a read function is more concise than asking whether a stream is at
its end on every iteration of a loop.

The Streams module also provides a set of functions for writing data
elements to an output stream. Like the functions that operate upon input
streams, these functions hide the details of indexing, growing an
underlying sequence, buffering for a file, and so on. For instance, the
function :gf:`write-element` writes a single data element to an output
stream.

The following forms bind *stream* to an output stream over an empty
string and create the string "I see!", using the function
:gf:`stream-contents` to access all of the stream’s elements.

.. code-block:: dylan

    let stream = make(<byte-string-stream>, direction: #"output");
    write(stream, "I see!");
    stream-contents(stream);

Calling :gf:`write` on a sequence has the same effect as calling
:gf:`write-element` on all the elements of the sequence. For more
information about writing to streams, see `Writing to streams`_.

Some streams are *positionable*; that is, any element of the stream can
be accessed at any time. Positionable streams allow you to set the
position at which the stream is accessed by the next operation. The
following example uses positioning to return the character "w" from a
stream over the string *"hello world"*:

.. code-block:: dylan

    let stream = make(<string-stream>, contents: "hello world");
    stream-position(stream) := 6;
    read-element(stream);

The following example returns a string. The first ten characters are the
fill characters for the underlying sequence of the stream. The fill
character for ``<string>`` is " " (the space character), so in the
example below, the first ten characters are spaces.

.. code-block:: dylan

    let stream = make(<string-stream>, direction: #"output");
    adjust-stream-position(stream, 10);
    write(stream, "whoa!");
    stream-contents(stream);

You can request a sequence containing all of the elements of a
positionable stream by calling :gf:`stream-contents` on it. If the
positionable stream is a :class:`<file-stream>`, then it must be
readable. Otherwise, it must be a sequence stream. The sequence returned
never shares structure with any underlying sequence that might be used
in the future by the stream. For instance, the string returned by
calling :gf:`stream-contents` on an output :class:`<string-stream>` will
not be the same string as that being used to represent the string
stream.

When making an input :class:`<string-stream>`, you can cause the stream
to produce elements from any subsequence of the supplied string. For
example:

.. code-block:: dylan

    read-to-end(make(<string-stream>,
               contents: "hello there, world",
               start: 6,
               end: 11));

This example evaluates to *"there"*. The interval (*start*, *end*)
includes the index *start* but excludes the index *end*. This is
consistent with standard Dylan functions over sequences, such as
:drm:`copy-sequence`. The :gf:`read-to-end` function is one of a number
of convenient utility functions for operating on streams and returns all
the elements up to the end of the stream from the stream’s current
position.

Streams, growing sequences, and object identity
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When writing to output streams over sequences, Dylan may from time to
time need to grow the underlying sequence that it is using to represent
the stream data.

Consider the example of an output stream instantiated over an empty
string. As soon as a write operation is performed on the stream, it is
necessary to replace the string object used in the representation of the
string stream. As well as incurring the cost of creating a new string,
references to the string within the program after the replacement
operation has occurred will still refer to the *original* string, and
this may not be what the user intended.

To guarantee that other references to a sequence used in an output
:class:`<sequence-stream>` will have access to any elements written to
the sequence via the stream, supply a stretchy collection (such as a
``<stretchy-vector>``) to :drm:`make`. A stream over a stretchy vector
will use the same stretchy vector throughout the stream’s existence.

For example:

.. code-block:: dylan

    let sv = make(<stretchy-vector>);
    let stream = make(<sequence-stream>,
                      contents: sv,
                      direction: #"output");
    write(stream,#(1, 2, 3, 4, 5, 6, 7, 8, 9));
    write(stream,"ABCDEF");
    values(sv, stream-contents(stream));

The example returns two values. Each value is the same (``==``) stretchy
vector:

.. code-block:: dylan

    (1, 2, 3, 4, 5, 6, 7, 8, 9, ’A’, ’B’, ’C’, ’D’, ’E’, ’F’)

If a stretchy vector is not supplied, the result is different:

.. code-block:: dylan

    let v = make(<vector>, size: 5);
    let stream = make(<sequence-stream>,
                      contents: v,
                      direction: #"output");
    write(stream,#(1, 2, 3, 4, 5, 6, 7, 8, 9));
    write(stream,"ABCDEF");
    values(v, stream-contents(stream));

This example returns as its first value the original vector, whose
contents are unchanged, but the second value is a new vector:

.. code-block:: dylan

    (1, 2, 3, 4, 5, 6, 7, 8, 9, ’A’, ’B’, ’C’, ’D’, ’E’, ’F’)

This difference arises because the output stream in the second example
does not use a stretchy vector to hold the stream data. A vector of at
least 15 elements is necessary to accommodate the elements written to
the stream, but the vector supplied, *v*, can hold only 5. Since the
stream cannot change *v* ’s size, it must allocate a new vector each
time it grows.

Stream classes
--------------

The exported streams class heterarchy includes the classes shown in
`Streams module classes`_. Classes shown in bold are all instantiable.

.. figure:: ../images/streams.png
   :align: center

* s - sealed  | o - open
* p - primary | f - free
* c - concrete | a - abstract
* u - uninstantiable | i - instantiable

Streams module classes
^^^^^^^^^^^^^^^^^^^^^^

- :class:`<stream>`
- :class:`<positionable-stream>`
- :class:`<buffered-stream>`
- :class:`<file-stream>`
- :class:`<sequence-stream>`

Creating streams
^^^^^^^^^^^^^^^^

This section describes how to create and manage different types of file
stream and sequence stream.

File streams
^^^^^^^^^^^^

File streams are intended only for accessing the contents of files. More
general file handling facilities, such as renaming, deleting, moving,
and parsing directory names, are provided by the File-System module: see
` <file-system.htm#46956>`_ for details. The make method on
:class:`<file-stream>` does not create direct instances of
:class:`<file-stream>`, but instead an instance of a subclass determined
by :gf:`type-for-file-stream`.

make *file-stream-class*

G.f method

make <file-stream> #key locator: direction: if-exists:
 if-does-not-exist: buffer-size: element-type:
 asynchronous?: share-mode => *file-stream-instance*

Creates and opens a stream over a file, and returns a new instance of a
concrete subclass of :class:`<file-stream>` that streams over the
contents of the file referenced by *filename*. To determine the concrete
subclass to be instantiated, this method calls the generic function
:gf:`type-for-file-stream`.

The *locator:* init-keyword should be a string naming a file. If the
Locators library is in use, *filename* should be an instance of
``<locator>`` or a string that can be coerced to one.

The *direction:* init-keyword specifies the direction of the stream.
This can be one of *#"input"*, *#"output"*, or *#"input-output"*. The
default is *#"input"*.

The *if-exists:* and *if-does-not-exist:* init-keywords specify actions
to take if the file named by *filename* does or does not already exist
when the stream is created. These init-keywords are discussed in more
detail in `Options when creating file streams`_.

The *buffer-size:* init-keyword can be used to suggest the size of a
stream’s buffer. See :class:`<buffered-stream>`.

The *element-type:* init-keyword specifies the type of the elements in
the file named by *filename*. See `Options when creating file
streams`_ for more details.

Options when creating file streams
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When creating file streams, you can supply the following init-keywords
to *make* in addition to those described in `File streams`_:

-  *if-exists:* An action to take if the file already exists.
-  *if-does-not-exist*: An action to take if the file does not already exist.
-  *element-type:* How the elements of the underlying file are accessed.
-  *asynchronous?:* Allows asynchronous writing of stream data to disk.
-  *share-mode:* How the file can be accessed while the stream is
   operating on it.

The *if-exists:* init-keyword allows you to specify an action to take if
the file named by *filename* already exists. The options are:

-  ``#f`` The file is opened with the stream position at the beginning.
   This is the default when the stream’s direction is *#"input"* or
   *#"input-output"*.
-  *#"new-version"* If the underlying file system supports file versioning,
   a new version of the file is created. This is the default when the stream’s
   direction is *#"output"*.
   If the file system does not support file versioning, the default is
   *#"replace"* when the direction of the stream is *#"output"*.
-  *#"overwrite"* Set the stream’s position to the beginning of the
   file, but preserve the current contents of the file. This is useful
   when the direction is *#"input-output"* or *#"output"* and you want
   to overwrite an existing file.
-  *#"replace"* Delete the existing file and create a new file.
-  *#"append"* Set the stream’s initial position to the end of the
   existing file so that all new output occurs at the end of the file.
   This option is only useful if the file is writeable.
-  *#"truncate"* If the file exists, it is truncated, setting the size
   of the file to 0. If the file does not exist, create a new file.
-  *#"signal"* Signal a :class:`<file-exists-error>`
   condition.

The *if-does-not-exist:* init-keyword allows you to specify an action to
take if the file named by *filename* does not exist. The options are:

-  ``#f`` No action.
-  *#"signal"* ** Signal a :class:`<file-does-not-exist-error>` condition. This is
   the default when the stream’s direction is *#"input"*.
-  *#"create"* Create a new zero-length file. This is the default when
   the stream’s direction is *#"output"* or *#"input-output"*.

Because creating a file stream *always* involves an attempt to open the
underlying file, the aforementioned error conditions will occur during
file stream instance initialization.

File permissions are checked when creating and opening file streams, and
if the user attempts to open a file for input, and has no read
permission, or to open a file for output, and has no write permission,
then an :class:`<invalid-file-permissions-error>`
condition is signalled at the time the file stream is created.

The *element-type:* init-keyword controls how the elements of the
underlying file are accessed. This allows file elements to be
represented abstractly; for instance, contiguous elements could be
treated as a single database record. The three possible element types
are:

- `<byte-character>`_
  The file is accessed as a sequence of 8-bit characters.

- `<unicode-character>`_
  The file is accessed as a sequence of 16-bit Unicode characters.

- `<byte>`_
  The file is accessed as a sequence of unsigned 8-bit integers.

The *asynchronous?:* init-keyword allows asynchronous writing of stream
data to disk. If ``#f``, whenever the stream has to write a buffer to
disk, the thread which triggered the write must wait for the write to
complete. If *asynchronous?* is ``#t``, the write proceeds in parallel
with the subsequent actions of the thread.

Note that asynchronous writes complicate error handling a bit. Any write
error which occurs most likely occurs after the call which triggered the
write. If this happens, the error is stored in a queue, and the next
operation on that stream signals the error. If you *close* the stream
with the *wait?* flag ``#f``, the close happens asynchronously (after all
queued writes complete) and errors may occur after *close* has returned.
A method *wait-for-io-completion* is provided to catch any errors that
may occur after *close* is called.

The *share-mode:* keyword determines how a file can be accessed by other
streams while the stream has it open. The possible values are:

-  *#"share-read"* Allow other streams to be opened to the file for
   reading but not for writing.
-  *#"share-write"* Allow other streams to be opened for writing but not
   for reading.
-  *#"share-read-write"* Allow other streams to be opened for writing
   or reading.
-  *#"exclusive"* Do not allow other streams to be opened to this file.

Sequence streams
^^^^^^^^^^^^^^^^

There are *make* methods on the following stream classes:

- :class:`<sequence-stream>`
- :class:`<string-stream>`
- :class:`<byte-string-stream>`
- :class:`<unicode-string-stream>`

Rather than creating direct instances of :class:`<sequence-stream>` or
:class:`<string-stream>`, the :drm:`make` methods for those classes
might create an instance of a subclass determined by
:gf:`type-for-sequence-stream`.

- :meth:`make(<sequence-stream>)`
- :meth:`make(<string-stream>)`
- :meth:`make(<byte-string-stream>)`
- :meth:`make(<unicode-string-stream>)`

Closing streams
^^^^^^^^^^^^^^^

It is important to call :gf:`close` on streams when you have finished with
them. Typically, external streams such as ``<file-stream>`` and
``<console-stream>`` allocate underlying system resources when they are
created, and these resources are not recovered until the stream is
closed. The total number of such streams that can be open at one time
may be system dependent. It may be possible to add reasonable
finalization methods to close streams when they are no longer referenced
but these are not added by default. See the
:doc:`../common-dylan/finalization` for full details about finalization.

Locking streams
^^^^^^^^^^^^^^^

In an application where more than one control thread may access a common
stream, it is important to match the granularity of locking to the
transaction model of the application. Ideally, an application should
lock a stream which is potentially accessed by multiple threads, only
once per transaction. Repeated and unnecessary locking and unlocking can
seriously degrade the performance of the Streams module. Thus an
application which wishes to write a complex message to a stream that
needs to be thread safe should lock the stream, write the message and
then unlock the stream after the entire message is written. Locking and
unlocking the stream for each character in the message would be a poor
match of locking to transaction model. The time required for the lock
manipulation would dominate the time required for the stream
transactions. Unfortunately this means that there is no way for the
Streams module to choose a default locking scheme without the likelihood
of seriously degrading streams performance for all applications whose
transaction models are different from the model implied by the chosen
default locking scheme. Instead, the Streams module provides the user
with a single, per instance slot, *stream-lock:*, which is inherited by
all subclasses of ``<stream>``. You should use the generic functions
:gf:`stream-lock` and :gf:`stream-lock-setter`, together with other
appropriate functions and macros from the Threads library, to implement
a locking strategy appropriate to your application and its stream
transaction model. The functions in the Streams module are not of
themselves thread safe, and make no guarantees about the atomicity of
read and write operations.

Reading from and writing to streams
-----------------------------------

This section describes how you can read from or write to a stream. Note
that it is an error to call any of these functions on a buffered stream
while its buffer is held by another thread; see `Using buffered
streams`_ for details about buffered streams.

Reading from streams
^^^^^^^^^^^^^^^^^^^^

The following are the basic functions for reading from streams.

- :gf:`read-element`
- :gf:`read`

A number of other functions are available for reading from streams. See
:gf:`peek`, :gf:`read-into!`, :gf:`discard-input`, and
:gf:`stream-input-available?`.

Convenience functions for reading from streams
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following is a small set of reading functions that search for
particular elements in a stream. These functions behave as though they
were implemented in terms of the more primitive functions described in
`Reading from streams`_.

- :gf:`read-to`
- :gf:`read-to-end`
- :gf:`skip-through`

Writing to streams
^^^^^^^^^^^^^^^^^^

This section describes the basic functions for writing to streams.

- :gf:`write-element`
- :gf:`write`

See :gf:`force-output`, :gf:`synchronize-output`, and
:gf:`discard-output`.

Reading and writing by lines
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following functions provide line-based input and output operations.

The newline sequence for string streams is a sequence comprising the
single newline character ``\n``. For character file streams, the newline
sequence is whatever sequence of characters the underlying platform uses
to represent a newline. For example, on MS-DOS platforms, the sequence
comprises two characters: a carriage return followed by a linefeed.

.. note:: No other functions in the Streams module do anything to
   manage the encoding of newlines; calling :gf:`write-element` on the
   character ``\n`` does not cause the ``\n`` character to be written as
   the native newline sequence, unless ``\n`` happens to *be* the native
   newline sequence.

- :gf:`read-line`
- :gf:`write-line`
- :gf:`new-line`

See also :gf:`read-line-into!`.

Querying streams
^^^^^^^^^^^^^^^^

The following functions can be used to determine various properties of a
stream.

- :gf:`stream-open?`
- :gf:`stream-element-type`
- :gf:`stream-at-end?`
- :gf:`stream-is-tty?`

For output streams, note that you can determine if a stream is one place
past the last written element by comparing :gf:`stream-position` to
:gf:`stream-size`.

Using file streams
^^^^^^^^^^^^^^^^^^

The following operations can be performed on file streams.

- :meth:`close(<file-stream>)`
- :gf:`wait-for-io-completion`
- :macro:`with-open-file`

Using buffered streams
----------------------

The Streams module provides efficient support for general use of
buffered I/O. Most ordinary programmers using the module do not need to
be concerned with buffering in most cases. When using buffered streams,
the buffering is transparent, but programs requiring more control can
access buffering functionality when appropriate. This section describes
the available buffering functionality.

Overview
^^^^^^^^

A buffered stream maintains some sort of buffer. All buffered streams
use the sealed class :class:`<buffer>` for their buffers. You can
suggest a buffer size when creating buffered streams, but normally you
do not need to do so, because a buffer size that is appropriate for the
stream’s source or destination is chosen for you.

Instances of the class :class:`<buffer>` also contain some state
information. This state information includes an index where reading or
writing should begin, and an index that is the end of input to be read,
or the end of space available for writing.

Buffered streams also maintain a *held* state, indicating whether the
application has taken the buffer for a stream and has not released it
yet. When a thread already holds the buffer for a stream, it is an error
to get the buffer again (or any other buffer for the same stream).

Useful types when using buffers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following types are used in operations that involve buffers.

<byte>
   A type representing limited integers in the range 0 to 255 inclusive.

<byte-character>
   A type representing 8-bit characters that instances of
   ``<byte-string>`` can contain.

<unicode-character>
   A type representing Unicode characters that instances of
   ``<unicode-string>`` can contain.

<byte-vector>
   A subtype of ``<vector>`` whose element-type is `<byte>`_.

Wrapper streams
---------------

Sometimes stream data requires conversion before an application can use
it: you might have a stream over a file of EBCDIC characters which you
would prefer to handle as their ASCII equivalents, or you might need to
encrypt or decrypt file data.

Wrapper streams provide a mechanism for working with streams which
require such conversion. Wrapper streams hold on to an underlying
stream, delegating to it most streams operations. The wrapper stream
carries out appropriate processing in its own implementations of the
streaming protocol.

The Streams module includes a base class called
:class:`<wrapper-stream>` upon which other wrapping streams can be
implemented.

A subclass of :class:`<wrapper-stream>` can "pass on" functions such as
:gf:`read-element` and :gf:`write-element` by simply delegating these
operations to the inner stream, as shown below:

.. code-block:: dylan

    define method read-element (ws :: <io-wrapper-stream>,
      #key on-end-of-stream)
     => (element)
      read-element(ws.inner-stream,
                   on-end-of-stream: on-end-of-stream)
      end method;

    define method write-element (ws :: <io-wrapper-stream>, element)
     => ()
      write-element(ws.inner-stream, element)
    end method;

Assuming that ``<io-wrapper-stream>`` delegates all other operations to
its inner stream, the following would suffice to implement a 16-bit
Unicode character stream wrapping an 8-bit character stream.

.. code-block:: dylan

    define class <unicode-stream> (<io-wrapper-stream>) end class;

    define method read-element (s :: <unicode-stream>,
      #key on-end-of-stream)
     => (ch :: <unicode-character>)
      with-stream-locked (s)
        let first-char = read-element(s.inner-stream,
                                      on-end-of-stream);
        let second-char = read-element(s.inner-stream,
                                       on-end-of-stream)
      end;
      convert-byte-pair-to-unicode(first-char, second-char)
    end method;

    define method write-element (s :: <unicode-stream>,
      c :: <character>)
     => ()
      let (first-char, second-char) =
        convert-unicode-to-byte-pair(c);
      with-stream-locked (s)
        write-element(s.inner-stream, first-char);
        write-element(s.inner-stream, second-char)
      end;
      c
    end method;

    define method stream-position (s :: <unicode-stream>)
     => p :: <integer>;
      truncate/(stream-position(s.inner-stream), 2)
    end method;

    define method stream-position-setter (p :: <integer>,
        s :: <unicode-stream>);
      stream-position(s.inner-stream) := p * 2
    end method;

Wrapper streams and delegation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

One problem with wrapper streams is the need for a wrapper stream to
intercept methods invoked by its inner stream. For example, consider two
hypothetical streams, ``<interactive-stream>`` and ``<dialog-stream>``,
the latter a subclass of :class:`<wrapper-stream>`. Both of these
classes have a method called *prompt*. The ``<interactive-stream>``
class specializes :gf:`read` thus:

.. code-block:: dylan

    define method read (s :: <interactive-stream>,
        n :: <integer>,
        #key on-end-of-stream);
      prompt(s);
      next-method()
    end method;

If a ``<dialog-stream>`` is used to wrap an ``<interactive-stream>`` then
an invocation of :gf:`read` on the ``<dialog-stream>`` will call ``prompt`` on
the inner ``<interactive-stream>``, not on the ``<dialog-stream>``, as desired.
The problem is that the ``<dialog-stream>`` delegates some tasks to its inner
stream, but handles some other tasks itself.

Delegation by inner-streams to outer-streams is implemented by the use
of the :gf:`outer-stream` function. The :gf:`outer-stream` function is used
instead of the stream itself whenever a stream invokes one of its
other protocol methods.

A correct implementation of the :gf:`read` method in the example above
would be as follows:

.. code-block:: dylan

    define method read (stream :: <interactive-stream>,
        n :: <integer>,
        #key on-end-of-stream)
      prompt(s.outer-stream);
      next-method()
    end method;

The *initialize* method on :class:`<stream>` is defined to set the
:gf:`outer-stream` slot to be the stream itself. The *initialize* method
on :class:`<wrapper-stream>` is specialized to set the
:gf:`outer-stream` slot to be the "parent" stream:

.. code-block:: dylan

    define method initialize (stream :: <wrapper-stream>,
        #key on, #rest all-keys);
      an-inner-stream.outer-stream := stream;
      next-method()
    end method;

Conditions
----------

The following classes are available for error conditions on streams.

- :class:`<end-of-stream-error>`
- :class:`<incomplete-read-error>`
- :class:`<file-error>`
- :class:`<file-exists-error>`
- :class:`<file-does-not-exist-error>`
- :class:`<invalid-file-permissions-error>`

There is no recovery protocol defined for any of these errors. Every
condition that takes an init-keyword has a slot accessor for the value
supplied. The name of this accessor function takes the form *class* *-*
*key*, where *class* is the name of the condition class (without the
angle brackets) and *key* is the name of the init-keyword. For example,
the accessor function for the *locator:* init-keyword for
:class:`<file-error>` is *file-error-locator*.

For more information, please refer to the reference entry for the
individual conditions.

Streams protocols
-----------------

This section describes the protocols for different classes of stream.

Positionable stream protocol
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This section describes the protocol for positionable streams.

A stream position can be thought of as a natural number that indicates
how many elements into the stream the stream’s current location is.
However, it is not always the case that a single integer contains enough
information to reposition a stream. Consider the case of an
"uncompressing" file stream that requires additional state beyond simply
the file position to be able to get the next input character from the
compressed file.

The Streams module addresses this problem by introducing the class
:class:`<stream-position>`, which is subclassed by various kinds of
stream implementations that need to maintain additional state. A stream
can be repositioned as efficiently as possible when
:gf:`stream-position-setter` is given a value previously returned by
:gf:`stream-position` on that stream.

It is also legal to set the position of a stream to an integer position.
However, for some types of streams, to do so might be slow, perhaps
requiring the entire contents of the stream up to that point to be read.

- :class:`<position-type>`
- :class:`<stream-position>`
- :gf:`stream-position`
- :gf:`stream-position-setter`
- :gf:`adjust-stream-position`
- :meth:`as(<integer>, <stream-position>)`
- :gf:`stream-size`
- :gf:`stream-contents`
- :gf:`unread-element`

Wrapper stream protocol
^^^^^^^^^^^^^^^^^^^^^^^

This section describes the protocol for implementing wrapper streams.
For information on using wrapper streams, see `Wrapper streams`_.

- :class:`<wrapper-stream>`
- :gf:`inner-stream`
- :gf:`inner-stream-setter`
- :gf:`outer-stream`
- :gf:`outer-stream-setter`

The streams Module Reference
----------------------------

This section includes complete reference entries for all interfaces that
are exported from the *streams* module.

.. generic-function:: adjust-stream-position
   :open:

   Moves the position of a positionable stream by a specified amount.

   :signature: adjust-stream-position *positionable-stream* *delta* #key *from* => *new-position*

   :parameter positionable-stream: An instance of :class:`<positionable-stream>`.
   :parameter delta: An instance of ``<integer>``.
   :parameter #key from: One of ``#"current"``, ``#"start"``, or
     ``#"end"``. Default value: ``#"current"``.
   :value new-position: An instance of :class:`<stream-position>`.

   :description:

     Moves the position of *positionable-stream* to be offset *delta*
     elements from the position indicated by *from*. The new position is
     returned.

     When *from* is ``#"start"``, the stream is positioned relative to
     the beginning of the stream. When *from* is ``#"end"``, the stream
     is positioned relative to its end. When *from* is ``#"current"``,
     the current position is used.

     Using *adjust-stream-position* to set the position of a stream to
     be beyond its current last element causes the underlying aggregate
     to be grown to a new size. When extending the underlying aggregate
     for a stream, the contents of the unwritten elements are the fill
     character for the underlying sequence.

   :example:

     The following example returns a string, the first ten characters of
     which are the space character, which is the fill character for the
     sequence ``<string>``.

     .. code-block:: dylan

       let stream = make(<string-stream>,
                         direction: #"output");
       adjust-stream-position(stream, 10);
       write(stream, "whoa!");
       stream-contents(stream);

   See also

   - :gf:`stream-position-setter`

.. method:: as
   :specializer: <integer>, <stream-position>

   Coerces a :class:`<stream-position>` to an integer.

   :signature: as *integer-class* *stream-position* => *integer*

   :parameter integer-class: The class ``<integer>``.
   :parameter stream-position: An instance of :class:`<stream-position>`.
   :value integer: An instance of ``<integer>``.

   :description:

     Coerces a :class:`<stream-position>` to an integer. The *integer-class*
     argument is the class ``<integer>``.

   See also

   - :drm:`as`

.. class:: <buffer>
   :sealed:
   :instantiable:

   A subclass of ``<vector>`` whose *element-type* is `<byte>`_.

   :superclasses: <vector>

   :keyword size: An instance of ``<integer>`` specifying the size of
     the buffer. Default value: 0.
   :keyword next: An instance of ``<integer>``. For an input buffer,
     this is where the next input byte can be found. For an output buffer,
     this is where the next output byte should be written to. Default
     value: 0.
   :keyword end: An instance of ``<integer>``. The value of this is one
     more than the last valid index in a buffer. For an input buffer, this
     represents the number of bytes read.

   :description:

     A subclass of ``<vector>`` whose *element-type* is `<byte>`_.

     Instances of ``<buffer>`` contain a data vector and two indices:
     the inclusive start and the exclusive end of valid data in the
     buffer. The accessors for these indexes are called ``buffer-next``
     and ``buffer-end``.

     Note that *size:* is not taken as a suggestion of the size the user
     would like, as with the value passed with *buffer-size:* to *make*
     on :class:`<buffered-stream>`; if you supply a value with the
     *size:* init-keyword, that size is allocated, or, if that is not
     possible, an error is signalled, as with making any vector.

.. class:: <buffered-stream>
   :open:
   :abstract:

   A subclass of :class:`<stream>` supporting the Stream Extension and
   Buffer Access protocols.

   :superclasses: :class:`<stream>`

   :keyword buffer-size: An instance of ``<integer>``. This is the size
     of the buffer in bytes.

   :description:

     A subclass of :class:`<stream>` supporting the Stream Extension
     Protocol and the Buffer Access Protocol. It is not instantiable.

     Streams of this class support the *buffer-size:* init-keyword,
     which can be used to suggest the size of the stream’s buffer.
     However, the instantiated stream might not use this value: it is
     taken purely as a suggested value. For example, a stream that uses
     a specific device’s hardware buffer might use a fixed buffer size
     regardless of the value passed with the *buffer-size:*
     init-keyword.

     In general, it should not be necessary to supply a value for the
     *buffer-size:* init-keyword.

<byte>
^^^^^^

   :type:    A type representing limited integers in the range 0 to 255 inclusive.

Supertypes

``<integer>``

Init-keywords

None.

   :description:

A type representing limited integers in the range 0 to 255 inclusive.

Operations

- :gf:`type-for-file-stream`

<byte-character>
^^^^^^^^^^^^^^^^

   :type:    A type representing 8-bit characters that instances of ``<byte-string>`` can contain.

Supertypes

``<character>``

Init-keywords

None.

   :description:

A type representing 8-bit characters that instances of ``<byte-string>``
can contain.

Operations

- :gf:`type-for-file-stream`

.. class:: <byte-string-stream>
   :open:
   :instantiable:

   The class of streams over byte strings.

   :superclasses: :class:`<string-stream>`

   :keyword contents: A general instance of ``<sequence>``.
   :keyword direction: Specifies the direction of the stream. It must
     be one of ``#"input"``, ``#"output"``, or ``#"input-output"``.
     Default value: ``#"input"``.
   :keyword start: An instance of ``<integer>``. This specifies the
     start position of the sequence to be streamed over. Only valid when
     *direction:* is ``#"input"``. Default value: 0.
   :keyword end: An instance of ``<integer>``. This specifies the
     sequence position immediately after the portion of the sequence to
     stream over. Only valid when *direction:* is ``#"input"``. Default
     value: *contents.size*.

   :description:

     The class of streams over byte strings. It is a subclass of
     :class:`<string-stream>`.

     The class supports the same init-keywords as
     :class:`<sequence-stream>`.

     The *contents:* init-keyword is used as the input for an input
     stream, and as the initial storage for an output stream.

     The *start:* and *end:* init-keywords specify the portion of the
     byte string to create the stream over: *start:* is inclusive and
     *end:* is exclusive. The default is to stream over the entire byte
     string.

   :operations:

     - :meth:`make(<byte-string-stream-class>)`

   See also

   - :meth:`make(<byte-string-stream-class>)`
   - :class:`<sequence-stream>`

.. class:: <byte-vector>
   :sealed:

   A subtype of ``<vector>`` whose element-type is `<byte>`_.

   :superclasses: <vector>

   :keyword: See Superclasses.

   :description:

     A subclass of ``<vector>`` whose element-type is `<byte>`_.

   See also

   - `<byte>`_

.. generic-function:: close
   :open:

   Closes a stream.

   :signature: close *stream* #key #all-keys => ()

   :parameter stream: An instance of :class:`<stream>`.

   :description:

     Closes *stream*, an instance of :class:`<stream>`.

.. method:: close
   :specializer: <file-stream>

   Closes a file stream.

   :signature: close *file-stream* #key *abort?* *wait?* => ()

   :parameter file-stream: An instance of :class:`<file-stream>`.
   :parameter #key abort?: An instance of ``<boolean>``. Default value: ``#f``.
   :parameter #key wait?: An instance of ``<boolean>``.

   :description:

     Closes a file stream. This method frees whatever it can of any
     underlying system resources held on behalf of the stream.

     If *abort* is false, any pending data is forced out and
     synchronized with the file’s destination. If *abort* is true, then
     any errors caused by closing the file are ignored.

   See also

   - :gf:`close`

.. generic-function:: discard-input
   :open:

   Discards input from an input stream.

   :signature: discard-input *input-stream* => ()

   :parameter input-stream: An instance of :class:`<stream>`.

   :description:

     Discards any pending input from *input-stream*, both buffered input
     and, if possible, any input that might be at the stream’s source.

     This operation is principally useful for "interactive" streams,
     such as TTY streams, to discard unwanted input after an error
     condition arises. There is a default method on :class:`<stream>` so
     that applications can call this function on any kind of stream. The
     default method does nothing.

   See also

   - :gf:`discard-output`

.. generic-function:: discard-output
   :open:

   Discards output to an output stream.

   :signature: discard-output *output-stream* => ()

   :parameter output-stream: An instance of :class:`<stream>`.

   :description:

     Attempts to abort any pending output for *output-stream*.

     A default method on :class:`<stream>` is defined, so that
     applications can call this function on any sort of stream. The
     default method does nothing.

   See also

   - :gf:`discard-input`

.. class:: <end-of-stream-error>

   Error type signaled on reaching the end of an input stream.

   :superclasses: <error>

   :keyword stream: An instance of :class:`<stream>`.

   :description:

     Signalled when one of the read functions reaches the end of an
     input stream. It is a subclass of ``<error>``.

     The *stream:* init-keyword has the value of the stream that caused
     the error to be signaled. Its accessor is
     ``end-of-stream-error-stream``.

   See also

   - :class:`<file-does-not-exist-error>`
   - :class:`<file-error>`
   - :class:`<file-exists-error>`
   - :class:`<incomplete-read-error>`
   - :class:`<invalid-file-permissions-error>`

.. class:: <file-does-not-exist-error>

   Error type signaled when attempting to read a file that does not exist.

   :superclasses: :class:`<file-error>`

   :keyword: See Superclasses.

   :description:

     Signaled when an input file stream creation function tries to read
     a file that does not exist. It is a subclass of
     :class:`<file-error>`.

   See also

   - :class:`<end-of-stream-error>`
   - :class:`<file-error>`
   - :class:`<file-exists-error>`
   - :class:`<incomplete-read-error>`
   - :class:`<invalid-file-permissions-error>`

.. class:: <file-error>

   The base class for all errors related to file I/O.

   :superclasses: <error>

   :keyword locator: An instance of ``<locator>``.

   :description:

     The base class for all errors related to file I/O. It is a subclass
     of ``<error>``.

     The *locator:* init-keyword indicates the locator of the file that
     caused the error to be signalled. Its accessor is
     ``file-error-locator``.

   See also

   - :class:`<end-of-stream-error>`
   - :class:`<file-does-not-exist-error>`
   - :class:`<file-exists-error>`
   - :class:`<incomplete-read-error>`
   - :class:`<invalid-file-permissions-error>`

.. class:: <file-exists-error>

   Error type signaled when trying to create a file that already exists.

   :superclasses: :class:`<file-error>`

   :keyword: See Superclasses.

   :description:

     Signalled when an output file stream creation function tries to
     create a file that already exists. It is a subclass of
     :class:`<file-error>`.

   See also

   - :class:`<end-of-stream-error>`
   - :class:`<file-does-not-exist-error>`
   - :class:`<file-error>`
   - :class:`<incomplete-read-error>`
   - :class:`<invalid-file-permissions-error>`

.. class:: <file-stream>
   :open:
   :abstract:
   :instantiable:

   The class of single-buffered streams over disk files.

   :superclasses: :class:`<buffered-stream>`, :class:`<positionable-stream>`

   :keyword locator: An instance of ``<string>`` or ``<locator>``. This
     specifies the file over which to stream.
   :keyword direction: Specifies the direction of the stream. It must be one of
     ``#"input"``, ``#"output"``, or ``#"input-output"``. Default value:
     ``#"input"``.
   :keyword if-exists: One of ``#f``, ``#"new-version"``,
     ``#"overwrite"``, ``#"replace"``, ``#"append"``, ``#"truncate"``,
     ``#"signal"``. Default value: ``#f``.
   :keyword if-does-not-exist: One of ``#f``, ``#"signal"``, or
     ``#"create"``. Default value: depends on the value of *direction:*.
   :keyword asynchronous?: If ``#t``, all writes on this stream are
     performed asynchronously. Default value:``#f``.

   :description:

     The class of single-buffered streams over disk files. It is a
     subclass of :class:`<positionable-stream>` and
     :class:`<buffered-stream>`.

     When you instantiate this class, an indirect instance of it is
     created. The file being streamed over is opened immediately upon
     creating the stream.

     The class supports several init-keywords: *locator:*, *direction:*,
     *if-exists:*, and *if-does-not-exist:*.

   :operations:

     - :meth:`close(<file-stream>)`
     - :meth:`make(<file-stream>)`

   See also

   - :meth:`make(<file-stream>)`

.. generic-function:: force-output
   :open:

   Forces pending output from an output stream buffer to its destination.

   :signature: force-output *output-stream* #key *synchronize?* => ()

   :parameter output-stream: An instance of :class:`<stream>`.
   :parameter synchronize?: An instance of ``<boolean>``. Default value: ``#f``.

   :description:

     Forces any pending output from *output-stream* ’s buffers to its
     destination. Even if the stream is asynchronous, this call waits
     for all writes to complete. If *synchronize?* is true, also flushes
     the operating system’s write cache for the file so that all data is
     physically written to disk. This should only be needed if you’re
     concerned about system failure causing loss of data.

   See also

   - :gf:`synchronize-output`

.. class:: <incomplete-read-error>

   Error type signaled on encountering the end of a stream before
   reading the required number of elements.

   :superclasses: :class:`<end-of-stream-error>`

   :keyword sequence: An instance of ``<sequence>``.
   :keyword count: An instance of ``<integer>``.

   :description:

     This error is signaled when input functions are reading a required
     number of elements, but the end of the stream is read before
     completing the required read.

     The *sequence:* init-keyword contains the input that was read
     before reaching the end of the stream. Its accessor is
     ``incomplete-read-error-sequence``.

     The *count:* init-keyword contains the number of elements that were
     requested to be read. Its accessor is
     ``incomplete-read-error-count``.

   See also

   - :class:`<end-of-stream-error>`
   - :class:`<file-does-not-exist-error>`
   - :class:`<file-error>`
   - :class:`<file-exists-error>`
   - :class:`<invalid-file-permissions-error>`

.. generic-function:: inner-stream
   :open:

   Returns the stream being wrapped.

   :signature: inner-stream *wrapper-stream* => *wrapped-stream*

   :parameter wrapper-stream: An instance of :class:`<wrapper-stream>`.
   :value wrapped-stream: An instance of :class:`<stream>`.

   :description:

     Returns the stream wrapped by *wrapper-stream*.

   See also

   - :gf:`inner-stream-setter`
   - :gf:`outer-stream`
   - :class:`<wrapper-stream>`

.. generic-function:: inner-stream-setter
   :open:

   Wraps a stream with a wrapper stream.

   :signature: inner-stream-setter *stream* *wrapper-stream* => *stream*

   :parameter stream: An instance of :class:`<stream>`.
   :parameter wrapper-stream: An instance of :class:`<wrapper-stream>`.
   :value stream: An instance of :class:`<stream>`.

   :description:

     Wraps *stream* with *wrapper-stream*. It does so by setting the
     :gf:`inner-stream` slot of *wrapper-stream* to *stream*, and the
     :gf:`outer-stream` slot of *stream* to *wrapper-stream*.

     .. note:: Applications should not set ``inner-stream`` and
        ``outer-stream`` slots directly. The ``inner-stream-setter``
        function is for use only when implementing stream classes.

   See also

   - :gf:`inner-stream`
   - :gf:`outer-stream-setter`

.. class:: <invalid-file-permissions-error>

   Error type signalled when accessing a file in a way that conflicts
   with the permissions of the file.

   :superclasses: :class:`<file-error>`

   :keyword: See Superclasses.

   :description:

     Signalled when one of the file stream creation functions tries to access
     a file in a manner for which the user does not have permission. It is a
     subclass of :class:`<file-error>`.

   See also

   - :class:`<end-of-stream-error>`
   - :class:`<file-does-not-exist-error>`
   - :class:`<file-error>`
   - :class:`<file-exists-error>`
   - :class:`<incomplete-read-error>`

.. method:: make
   :specializer: <byte-string-stream>

   Creates and opens a stream over a byte string.

   :signature: make *byte-string-stream-class* #key *contents* *direction* *start* *end* => *byte-string-stream-instance*

   :parameter byte-string-stream-class: The class :class:`<byte-string-stream>`.
   :parameter #key contents: An instance of ``<string>``.
   :parameter #key direction: One of ``#"input"``, ``#"output"``, or
     ``#"input-output"``. Default value: ``#"input"``.
   :parameter #key start: An instance of ``<integer>``. Default value: 0.
   :parameter #key end: An instance of ``<integer>``. Default value: *contents.size*.
   :value byte-string-stream-instance: An instance of :class:`<byte-string-stream>`.

   :description:

     Creates and opens a stream over a byte string.

     This method returns an instance of :class:`<byte-string-stream>`.
     If supplied, *contents* describes the contents of the stream. The
     *direction*, *start*, and *end* init-keywords are as for
     :meth:`make <make(<sequence-stream>)>` on
     :class:`<sequence-stream>`.

   :example:

     .. code-block:: dylan

       let stream = make(<byte-string-stream>,
                         direction: #"output");

   See also

   - :class:`<byte-string-stream>`
   - :meth:`make(<sequence-stream>)`

.. method:: make
   :specializer: <file-stream>

   Creates and opens a stream over a file.

   :signature: make *file-stream-class* #key *filename* *direction* *if-exists* *if-does-not-exist* *buffer-size* *element-type* => *file-stream-instance*

   :parameter file-stream-class: The class :class:`<file-stream>`.
   :parameter #key filename: An instance of ``<object>``.
   :parameter #key direction: One of ``#"input"``, ``#"output"``, or
     ``#"input-output"``. The default is ``#"input"``.
   :parameter #key if-exists: One of ``#f``, ``#"new-version"``,
     ``#"overwrite"``, ``#"replace"``, ``#"append"``, ``#"truncate"``,
     ``#"signal"``. Default value: ``#f``.
   :parameter #key if-does-not-exist: One of ``#f``, ``#"signal"``, or
     ``#"create"``. Default value: depends on the value of *direction*.
   :parameter #key buffer-size: An instance of ``<integer>``.
   :parameter #key element-type: One of `<byte-character>`_,
     `<unicode-character>`_, or `<byte>`_, or ``#f``.
   :value file-stream-instance: An instance of :class:`<file-stream>`.

   :description:

     Creates and opens a stream over a file.

     Returns a new instance of a concrete subclass of
     :class:`<file-stream>` that streams over the contents of the file
     referenced by *filename*. To determine the concrete subclass to be
     instantiated, this method calls the generic function
     :gf:`type-for-file-stream`.

     The *filename* init-keyword should be a string naming a file. If
     the Locators library is in use, *filename* should be an instance of
     ``<locator>`` or a string that can be coerced to one.

     The *direction* init-keyword specifies the direction of the stream.

     The *if-exists* and *if-does-not-exist* init-keywords specify
     actions to take if the file named by *filename* does or does not
     already exist when the stream is created. These init-keywords are
     discussed in more detail in `Options when creating file streams`_.

     The *buffer-size* init-keyword is explained in :class:`<buffered-stream>`.

     The *element-type* init-keyword specifies the type of the elements
     in the file named by *filename*. This allows file elements to be
     represented abstractly; for instance, contiguous elements could be
     treated as a single database record. This init-keyword defaults to
     something useful, potentially based on the properties of the file;
     `<byte-character>`_ and `<unicode-character>`_ are likely choices.
     See `Options when creating file streams`_.

   See also

   - :class:`<buffered-stream>`
   - :class:`<file-stream>`
   - :gf:`type-for-file-stream`

.. method:: make
   :specializer: <sequence-stream>

   Creates and opens a stream over a sequence.

   :signature: make *sequence-stream-class* #key *contents* *direction* *start* *end* => *sequence-stream-instance*

   :parameter sequence-stream-class: The class :class:`<sequence-stream>`.
   :parameter #key contents: An instance of ``<string>``.
   :parameter #key direction: One of ``#"input"``, ``#"output"``, or
     ``#"input-output"``. Default value: ``#"input"``.
   :parameter #key start: An instance of ``<integer>``. Default value: 0.
   :parameter #key end: An instance of ``<integer>``. Default value: *contents.size*.
   :value sequence-stream-instance: An instance of :class:`<sequence-stream>`.

   :description:

     Creates and opens a stream over a sequence.

     This method returns a general instance of
     :class:`<sequence-stream>`. To determine the concrete subclass to
     be instantiated, this method calls the generic function
     :gf:`type-for-sequence-stream`.

     The *contents* init-keyword is a general instance of ``<sequence>``
     which is used as the input for input streams, and as the initial
     storage for an output stream. If *contents* is a stretchy vector,
     then it is the only storage used by the stream.

     The *direction* init-keyword specifies the direction of the stream.

     The *start* and *end* init-keywords are only valid when *direction*
     is ``#"input"``. They specify the portion of the sequence to create
     the stream over: *start* is inclusive and *end* is exclusive. The
     default is to stream over the entire sequence.

   :example:

     .. code-block:: dylan

       let sv = make(<stretchy-vector>);
       let stream = make(<sequence-stream>,
                         contents: sv,
                         direction: #"output");
       write(stream,#(1, 2, 3, 4, 5, 6, 7, 8, 9));
       write(stream,"ABCDEF");
       values(sv, stream-contents(stream));

   See also

   - :class:`<sequence-stream>`
   - :gf:`type-for-sequence-stream`

.. method:: make
   :specializer: <string-stream>

   Creates and opens a stream over a string.

   :signature: make *string-stream-class* #key *contents* *direction* *start* *end* => *string-stream-instance*

   :parameter string-stream-class: The class :class:`<string-stream>`.
   :parameter #key contents: An instance of ``<string>``.
   :parameter #key direction: One of ``#"input"``, ``#"output"``, or
     ``#"input-output"``. Default value: ``#"input"``.
   :parameter #key start: An instance of ``<integer>``. Default value: 0.
   :parameter #key end: An instance of ``<integer>``. Default value: *contents.size*.
   :value string-stream-instance: An instance of :class:`<string-stream>`.

   :description:

     Creates and opens a stream over a string.

     This method returns an instance of :class:`<string-stream>`. If
     supplied, *contents* describes the contents of the stream. The
     *direction*, *start*, and *end* init-keywords are as for
     :meth:`make <make(<sequence-stream>)>` on
     :class:`<sequence-stream>`.

   :example:

     .. code-block:: dylan

       let stream = make(<string-stream>,
                         contents: "here is a sequence");

   See also

   - :meth:`make(<sequence-stream>)`
   - :class:`<string-stream>`

.. method:: make
   :specializer: <unicode-string-stream>

   Creates and opens a stream over a Unicode string.

   :signature: make *unicode-string-stream-class* #key *contents* *direction* *start* *end* => *unicode-string-stream-instance*

   :parameter unicode-string-stream-class: The class :class:`<unicode-string-stream>`.
   :parameter #key contents: An instance of ``<unicode-string>``.
   :parameter #key direction: One of ``#"input"``, ``#"output"``, or
     ``#"input-output"``. Default value: ``#"input"``.
   :parameter #key start: An instance of ``<integer>``. Default value: 0.
   :parameter #key end: An instance of ``<integer>``. Default value: *contents.size*.
   :value unicode-string-stream-instance: An instance of :class:`<unicode-string-stream>`.

   :description:

     Creates and opens a stream over a Unicode string.

     This method returns a new instance of
     :class:`<unicode-string-stream>`. If supplied, *contents* describes
     the contents of the stream, and must be an instance of
     ``<unicode-string>``. The *direction*, *start*, and *end*
     init-keywords are as for :meth:`make <make(<sequence-stream>)>` on
     :class:`<sequence-stream>`.

   See also

   - :meth:`make(<sequence-stream>)`
   - :class:`<unicode-string-stream>`

.. generic-function:: new-line
   :open:

   Writes a newline sequence to an output stream.

   :signature: new-line *output-stream* => ()

   :parameter output-stream: An instance of :class:`<stream>`.

   :description:

     Writes a newline sequence to *output-stream*.

     A method for ``new-line`` is defined on :class:`<string-stream>`
     that writes the character ``\n`` to the string stream.

.. generic-function:: outer-stream
   :open:

   Returns a stream’s wrapper stream.

   :signature: outer-stream *stream* => *wrapping-stream*

   :parameter stream: An instance of :class:`<stream>`.
   :value wrapping-stream: An instance of :class:`<wrapper-stream>`.

   :description:

     Returns the stream that is wrapping *stream*.

   See also

   - :gf:`inner-stream`
   - :gf:`outer-stream-setter`
   - :class:`<wrapper-stream>`

.. generic-function:: outer-stream-setter
   :open:

   Sets a stream’s wrapper stream.

   :signature: outer-stream-setter *wrapper-stream* *stream* => *wrapper-stream*

   :parameter wrapper-stream: An instance of :class:`<wrapper-stream>`.
   :parameter stream: An instance of :class:`<stream>`.
   :value wrapper-stream: An instance of :class:`<wrapper-stream>`.

   :description:

     Sets the :gf:`outer-stream` slot of *stream* to *wrapper-stream*.

     .. note:: Applications should not set ``inner-stream`` and
        ``outer-stream`` slots directly. The ``outer-stream-setter``
        function is for use only when implementing stream classes.

   See also

   - :gf:`inner-stream-setter`
   - :gf:`outer-stream`

.. generic-function:: peek
   :open:

   Returns the next element of a stream without advancing the stream
   position.

   :signature: peek *input-stream* #key *on-end-of-stream* => *element-or-eof*

   :parameter input-stream: An instance of :class:`<stream>`.
   :parameter #key on-end-of-stream: An instance of ``<object>``.
   :value element-or-eof: An instance of ``<object>``, or ``#f``.

   :description:

     This function behaves as :gf:`read-element` does, but the stream
     position is not advanced.

   See also

   - :gf:`read-element`

.. class:: <positionable-stream>
   :open:
   :abstract:

   The class of positionable streams.

   :superclasses: :class:`<stream>`

   :keyword: See Superclasses.

   :description:

     A subclass of :class:`<stream>` supporting the Positionable Stream
     Protocol. It is not instantiable.

   :operations:

     - :gf:`adjust-stream-position`
     - :gf:`stream-contents`
     - :gf:`stream-position`
     - :gf:`stream-position-setter`
     - :gf:`unread-element`

<position-type>
^^^^^^^^^^^^^^~

   :type:    A type representing positions in a stream.

Equivalent

type-union(<stream-position>, <integer>)

Supertypes

None.

Init-keywords

None.

   :description:

A type used to represent a position in a stream. In practice, positions
within a stream are defined as instances of ``<integer>``, but this type,
together with the :class:`<stream-position>` class, allows for cases where
this might not be possible.

See also

- :class:`<stream-position>`

.. generic-function:: read
   :open:

   Reads a number of elements from an input stream.

   :signature: read *input-stream* *n* #key *on-end-of-stream* => *sequence-or-eof*

   :parameter input-stream: An instance of :class:`<stream>`.
   :parameter n: An instance of ``<integer>``.
   :parameter #key on-end-of-stream: An instance of ``<object>``.
   :value sequence-or-eof: An instance of ``<sequence>``, or an instance
     of ``<object>`` if the end of stream is reached.

   :description:

     Returns a sequence of the next *n* elements from *input-stream*.

     The type of the sequence returned depends on the type of the
     stream’s underlying aggregate. For instances of
     :class:`<sequence-stream>`, the type of the result is given by
     :drm:`type-for-copy` of the underlying aggregate. For instances of
     :class:`<file-stream>`, the result is a vector that can contain
     elements of the type returned by calling :gf:`stream-element-type`
     on the stream.

     The stream position is advanced so that subsequent reads start
     after the *n* elements.

     If the stream is not at its end, *read* waits until input becomes
     available.

     If the end of the stream is reached before all *n* elements have
     been read, the behavior is as follows.

     - If a value for the *on-end-of-stream* argument was supplied, it
       is returned as the value of *read*.
     - If a value for the *on-end-of-stream* argument was not supplied,
       and at least one element was read from the stream, then an
       :class:`<incomplete-read-error>` condition is signaled. When
       signaling this condition, *read* supplies two values: a sequence
       of the elements that were read successfully, and *n*.
     - If the *on-end-of-stream* argument was not supplied, and no
       elements were read from the stream, an
       :class:`<end-of-stream-error>` condition is signalled.

   See also

   - :class:`<end-of-stream-error>`
   - :class:`<incomplete-read-error>`
   - :gf:`stream-element-type`

.. generic-function:: read-element
   :open:

   Reads the next element in a stream.

   :signature: read-element *input-stream* #key *on-end-of-stream* => *element-or-eof*

   :parameter input-stream: An instance of :class:`<stream>`.
   :parameter #key on-end-of-stream: An instance of ``<object>``.
   :value element-or-eof: An instance of ``<object>``.

   :description:

     Returns the next element in the stream. If the stream is not at its
     end, the stream is advanced so that the next call to
     ``read-element`` returns the next element along in *input-stream*.

     The *on-end-of-stream* keyword allows you to specify a value to be
     returned if the stream is at its end. If the stream is at its end
     and no value was supplied for *on-end-of-stream*, ``read-element``
     signals an :class:`<end-of-stream-error>` condition.

     If no input is available and the stream is not at its end,
     ``read-element`` waits until input becomes available.

   :example:

     The following piece of code applies *function* to all the elements
     of a sequence:

     .. code-block:: dylan

       let stream = make(<sequence-stream>, contents: seq);
       while (~stream-at-end?(stream))
         function(read-element(stream));
       end;

   See also

   - :class:`<end-of-stream-error>`
   - :gf:`peek`
   - :gf:`unread-element`

.. generic-function:: read-into!
   :open:

   Reads a number of elements from a stream into a sequence.

   :signature: read-into! *input-stream* *n* *sequence* #key *start* *on-end-of-stream* => *count-or-eof*

   :parameter input-stream: An instance of :class:`<stream>`.
   :parameter n: An instance of ``<integer>``.
   :parameter sequence: An instance of ``<mutable-sequence>``.
   :parameter #key start: An instance of ``<integer>``.
   :parameter #key on-end-of-stream: An instance of ``<object>``.
   :value count-or-eof: An instance of ``<integer>``, or an instance of
     ``<object>`` if the end of stream is reached..

   :description:

     Reads the next *n* elements from *input-stream*, and inserts them
     into a mutable sequence starting at the position *start*. Returns
     the number of elements actually inserted into *sequence* unless the
     end of the stream is reached, in which case the *on-end-of-stream*
     behavior is as for :gf:`read`.

     If the sum of *start* and *n* is greater than the size of
     *sequence*, ``read-into!`` reads only enough elements to fill
     sequence up to the end. If *sequence* is a stretchy vector, no
     attempt is made to grow it.

     If the stream is not at its end, ``read-into!`` waits until input
     becomes available.

   See also

   - :gf:`read`

.. generic-function:: read-line
   :open:

   Reads a stream up to the next newline.

   :signature: read-line *input-stream* #key *on-end-of-stream* => *string-or-eof* *newline?*

   :parameter input-stream: An instance of :class:`<stream>`.
   :parameter #key on-end-of-stream: An instance of ``<object>``.
   :value string-or-eof: An instance of ``<string>``, or an instance of
     ``<object>`` if the end of the stream is reached.
   :value newline?: An instance of ``<boolean>``.

   :description:

     Returns a new string containing all the input in *input-stream* up
     to the next newline sequence.

     The resulting string does not contain the newline sequence. The
     second value returned is ``#t`` if the read terminated with a
     newline or ``#f`` if the read terminated because it came to the end
     of the stream.

     The type of the result string is chosen so that the string can
     contain characters of *input-stream* ’s element type. For example,
     if the element type is `<byte-character>`_, the string will be a
     ``<byte-string>``.

     If *input-stream* is at its end immediately upon calling
     ``read-line`` (that is, the end of stream appears to be at the end
     of an empty line), then the end-of-stream behavior and the
     interpretation of *on-end-of-stream* is as for :gf:`read-element`.

   See also

   - :gf:`read-element`

.. generic-function:: read-line-into!
   :open:

   Reads a stream up to the next newline into a string.

   :signature: read-line-into! *input-stream* *string* #key *start* *on-end-of-stream* *grow?* => *string-or-eof* *newline?*

   :parameter input-stream: An instance of :class:`<stream>`.
   :parameter string: An instance of ``<string>``.
   :parameter #key start: An instance of ``<integer>``. Default value: 0.
   :parameter #key on-end-of-stream: An instance of ``<object>``.
   :parameter #key grow?: An instance of ``<boolean>``. Default value: ``#f``.
   :value string-or-eof: An instance of ``<string>``, or an instance of
     ``<object>`` if the end of the stream is reached.
   :value newline?: An instance of ``<boolean>``.

   :description:

     Fills *string* with all the input from *input-stream* up to the
     next newline sequence. The *string* must be a general instance of
     ``<string>`` that can hold elements of the stream’s element type.

     The input is written into *string* starting at the position
     *start*. By default, *start* is the start of the stream.

     The second return value is ``#t`` if the read terminated with a
     newline, or ``#f`` if the read completed by getting to the end of
     the input stream.

     If *grow?* is ``#t``, and *string* is not large enough to hold all
     of the input, ``read-line-into!`` creates a new string which it
     writes to and returns instead. The resulting string holds all the
     original elements of *string*, except where ``read-line-into!``
     overwrites them with input from *input-stream*.

     In a manner consistent with the intended semantics of *grow?*, when
     *grow?* is ``#t`` and *start* is greater than or equal to
     *string.size*, ``read-line-into!`` grows *string* to accommodate
     the *start* index and the new input.

     If *grow?* is ``#f`` and *string* is not large enough to hold the
     input, the function signals an error.

     The end-of-stream behavior and the interpretation of
     *on-end-of-stream* is the same as that of :gf:`read-line`.

   See also

   - :gf:`read-line`

.. generic-function:: read-through

   Returns a sequence containing the elements of the stream up to, and
   including, the first occurrence of a given element.

   :signature: read-through *input-stream* *element* #key *on-end-of-stream* *test* => *sequence-or-eof* *found?*

   :parameter input-stream: An instance of :class:`<stream>`.
   :parameter element: An instance of ``<object>``.
   :parameter #key on-end-of-stream: An instance of ``<object>``.
   :parameter #key test: An instance of ``<function>``. Default value: ``==``.
   :value sequence-or-eof: An instance of ``<sequence>``, or an instance of
     ``<object>`` if the end of the stream is reached.
   :value found?: An instance of ``<boolean>``.

   :description:

     This function is the same as :gf:`read-to`, except that *element*
     is included in the resulting sequence.

     If the *element* is not found, the result does not contain it. The
     stream is left positioned after *element*.

   See also

   - :gf:`read-to`

.. generic-function:: read-to

   Returns a sequence containing the elements of the stream up to, but
   not including, the first occurrence of a given element.

   :signature: read-to *input-stream* *element* #key *on-end-of-stream* *test* => *sequence-or-eof* *found?*

   :parameter input-stream: An instance of :class:`<stream>`.
   :parameter element: An instance of ``<object>``.
   :parameter #key on-end-of-stream: An instance of ``<object>``.
   :parameter #key test: An instance of ``<function>``. Default value: ``==``.
   :value sequence-or-eof: An instance of ``<sequence>``, or an instance of
     ``<object>`` if the end of the stream is reached.
   :value found?: An instance of ``<boolean>``.

   :description:

     Returns a new sequence containing the elements of *input-stream*
     from the stream’s current position to the first occurrence of
     *element*. The result does not contain *element*.

     The second return value is ``#t`` if the read terminated with
     *element*, or ``#f`` if the read terminated by reaching the end of
     the stream’s source. The "boundary" element is consumed, that is,
     the stream is left positioned after *element*.

     The ``read-to`` function determines whether the element occurred by
     calling the function *test*. This function must accept two
     arguments, the first of which is the element retrieved from the
     stream first and the second of which is *element*.

     The type of the sequence returned is the same that returned by
     :gf:`read`. The end-of-stream behavior is the same as that of
     :gf:`read-element`.

   See also

   - :gf:`read-element`

.. generic-function:: read-to-end

   Returns a sequence containing all the elements up to, and including,
   the last element of the stream.

   :signature: read-to-end *input-stream* => *sequence*

   :parameter input-stream: An instance of :class:`<stream>`.
   :value sequence: An instance of ``<sequence>``.

   :description:

     Returns a sequence of all the elements up to, and including, the
     last element of *input-stream*, starting from the stream’s current
     position.

     The type of the result sequence is as described for :gf:`read`.
     There is no special end-of-stream behavior; if the stream is
     already at its end, an empty collection is returned.

   :example:

     .. code-block:: dylan

       read-to-end(make(<string-stream>,
                        contents: "hello there, world",
                   start: 6,
                   end: 11));

   See also

   - :gf:`read`

.. class:: <sequence-stream>
   :open:

   The class of streams over sequences.

   :superclasses: :class:`<positionable-stream>`

   :keyword contents: A general instance of ``<sequence>`` which is used
     as the input for an input stream, and as the initial storage for an
     output stream.
   :keyword direction: Specifies the direction of the stream. It must
     be one of ``#"input"``, ``#"output"``, or ``#"input-output"``.
     Default value: ``#"input"``.
   :keyword start: An instance of ``<integer>``. This specifies the
     start position of the sequence to be streamed over. Only valid when
     *direction:* is ``#"input"``. Default value: 0.
   :keyword end: An instance of ``<integer>``. This specifies the
     sequence position immediately after the portion of the sequence to
     stream over. Only valid when *direction:* is ``#"input"``. Default
     value: *contents.size*.

   :description:

     The class of streams over sequences. It is a subclass of
     :class:`<positionable-stream>`.

     If *contents:* is a stretchy vector, then it is the only storage
     used by the stream.

     The ``<sequence-stream>`` class can be used for streaming over all
     sequences, but there are also subclasses :class:`<string-stream>`,
     :class:`<byte-string-stream>`, and
     :class:`<unicode-string-stream>`, which are specialized for
     streaming over strings.

     The *start:* and *end:* init-keywords specify the portion of the
     sequence to create the stream over: *start:* is inclusive and
     *end:* is exclusive. The default is to stream over the entire
     sequence.

   :operations:

     - :meth:`make(<sequence-stream>)`

   See also

   - :class:`<byte-string-stream>`
   - :meth:`make(<sequence-stream>)`
   - :class:`<string-stream>`
   - :class:`<unicode-string-stream>`

.. generic-function:: skip-through

   Skips through an input stream past the first occurrence of a given element.

   :signature: skip-through *input-stream* *element* #key *test* => *found?*

   :parameter input-stream: An instance of :class:`<stream>`.
   :parameter element: An instance of ``<object>``.
   :parameter #key test: An instance of ``<function>``. Default value: ``==``.
   :value found?: An instance of ``<boolean>``.

   :description:

     Positions *input-stream* after the first occurrence of *element*,
     starting from the stream’s current position. Returns ``#t`` if the
     element was found, or ``#f`` if the end of the stream was
     encountered. When ``skip-through`` does not find *element*, it
     leaves *input-stream* positioned at the end.

     The ``skip-through`` function determines whether the element
     occurred by calling the test function *test*. The test function
     must accept two arguments. The order of the arguments is the
     element retrieved from the stream first and element second.

.. class:: <stream>
   :open:
   :abstract:

   The superclass of all stream classes.

   :superclasses: <object>

   :keyword outer-stream: The name of the stream wrapping the stream.
     Default value: the stream itself (that is, the stream is not
     wrapped).

   :description:

     The superclass of all stream classes and a direct subclass of
     ``<object>``. It is not instantiable.

     The *outer-stream:* init-keyword should be used to delegate a task
     to its wrapper stream. See `Wrapper streams and delegation`_ for
     more information.

   :operations:

     - :gf:`close`
     - :gf:`discard-input`
     - :gf:`discard-output`
     - :gf:`force-output`
     - :gf:`new-line`
     - :gf:`outer-stream`
     - :gf:`outer-stream-setter`
     - :gf:`peek`
     - :gf:`read`
     - :gf:`read-element`
     - :gf:`read-into!`
     - :gf:`read-line`
     - :gf:`read-line-into!`
     - :gf:`read-through`
     - :gf:`read-to`
     - :gf:`read-to-end`
     - :gf:`skip-through`
     - :gf:`stream-at-end?`
     - :gf:`stream-element-type`
     - :gf:`stream-input-available?`
     - :gf:`stream-lock`
     - :gf:`stream-lock-setter`
     - :gf:`stream-open?`
     - :gf:`synchronize-output`
     - :gf:`write`
     - :gf:`write-element`

.. generic-function:: stream-at-end?
   :open:

   Tests whether a stream is at its end.

   :signature: stream-at-end? *stream* => *at-end?*

   :parameter stream: An instance of :class:`<stream>`.
   :value at-end?: An instance of ``<boolean>``.

   :description:

     Returns ``#t`` if the stream is at its end and ``#f`` if it is not.
     For input streams, it returns ``#t`` if a call to
     :gf:`read-element` with no supplied keyword arguments would signal
     an :class:`<end-of-stream-error>`.

     This function differs from :gf:`stream-input-available?`, which
     tests whether the stream can be read.

     For output-only streams, this function always returns ``#f``.

     For output streams, note that you can determine if a stream is one
     place past the last written element by comparing
     :gf:`stream-position` to :gf:`stream-size`.

   :example:

     The following piece of code applies *function* to all the elements of a
     sequence:

     .. code-block:: dylan

       let stream = make(<sequence-stream>, contents: seq);
       while (~stream-at-end?(stream))
         function(read-element(stream));
       end;

   See also

   - :class:`<end-of-stream-error>`
   - :gf:`read-element`
   - :gf:`stream-input-available?`

.. generic-function:: stream-contents
   :open:

   Returns a sequence containing all the elements of a positionable stream.

   :signature: stream-contents *positionable-stream* #key *clear-contents?*  => *sequence*

   :parameter positionable-stream: An instance of :class:`<positionable-stream>`.
   :parameter clear-contents?: An instance of ``<boolean>``. Default value: ``#t``.
   :value sequence: An instance of ``<sequence>``.

   :description:

     Returns a sequence that contains all of *positionable-stream* ’s
     elements from its start to its end, regardless of its current
     position. The type of the returned sequence is as for :gf:`read`.

     The *clear-contents?* argument is only applicable to writeable
     sequence streams, and is not defined for file-streams or any other
     external stream. It returns an error if applied to an input only
     stream. If clear-contents? is ``#t`` (the default for cases where
     the argument is defined), this function sets the size of the stream
     to zero, and the position to the stream’s start. Thus the next call
     to ``stream-contents`` will return only the elements written after
     the previous call to ``stream-contents``.

     Note that the sequence returned never shares structure with any
     underlying sequence that might be used in the future by the stream.
     For instance, the string returned by calling ``stream-contents`` on
     an output :class:`<string-stream>` will not be the same string as
     that being used to represent the string stream.

   :example:

     The following forms bind *stream* to an output stream over an empty
     string and create the string "I see!", using the function
     ``stream-contents`` to access all of the stream’s elements.

     .. code-block:: dylan

       let stream = make(<byte-string-stream>,
                         direction: #"output");
       write-element(stream, ’I’);
       write-element(stream, ’ ’);
       write(stream, "see");
       write-element(stream, ’!’);
       stream-contents(stream);

   See also

   - :gf:`read-to-end`

.. generic-function:: stream-element-type
   :open:

   Returns the element-type of a stream.

   :signature: stream-element-type *stream* => *element-type*

   :parameter stream: An instance of :class:`<stream>`.
   :value element-type: An instance of ``<type>``.

   :description:

     Returns the element type of *stream* as a Dylan ``<type>``.

.. generic-function:: stream-input-available?
   :open:

   Tests if an input stream can be read.

   :signature: stream-input-available? *input-stream* => *available?*

   :parameter input-stream: An instance of :class:`<stream>`.
   :value available?: An instance of ``<boolean>``.

   :description:

     Returns ``#t`` if *input-stream* would not block on
     :gf:`read-element`, otherwise it returns ``#f``.

     This function differs from :gf:`stream-at-end?`. When
     :gf:`stream-input-available?` returns ``#t``, :gf:`read-element`
     will not block, but it may detect that it is at the end of the
     stream’s source, and consequently inspect the *on-end-of-stream*
     argument to determine how to handle the end of stream.

   See also

   - :gf:`read-element`
   - :gf:`stream-at-end?`

.. generic-function:: stream-is-tty?
   :open:

   Tests whether a stream is directed to the console.

   :signature: stream-is-tty? *stream* => *is-tty?*

   :parameter stream: An instance of :class:`<file-stream>`.
   :value is-tty?: An instance of ``<boolean>``.

   :description:

     Returns ``#t`` if the stream is directed to the console and ``#f`` if it is not.

   :example:

     The following piece of code tests whether stdout has been directed to the
     console (./example), or to a file (./example > file):

     .. code-block:: dylan

       if (stream-is-tty?(*standard-output*))
    	 format-out("Output is directed to the console\n")
       else
	 format-out("Output is not directed to the console\n")
       end if;

.. generic-function:: stream-lock
   :open:

   Returns the lock for a stream.

   :signature: stream-lock *stream* => *lock*

   :parameter stream: An instance of :class:`<stream>`.
   :value lock: An instance of :class:`<lock>`, or ``#f``.

   :description:

     Returns *lock* for the specified *stream*. You can use this function,
     in conjunction with :gf:`stream-lock-setter` to
     implement a basic stream locking facility.

   See also

   - :gf:`stream-lock-setter`

.. generic-function:: stream-lock-setter
   :open:

   Sets a lock on a stream.

   :signature: stream-lock-setter *stream lock* => *lock*

   :parameter stream: An instance of :class:`<stream>`.
   :parameter lock: An instance of :class:`<lock>`, or ``#f``.
   :value lock: An instance of :class:`<lock>`, or ``#f``.

   :description:

     Sets *lock* for the specified *stream*. If *lock* is ``#f``, then
     the lock on *stream* is freed. You can use this function in
     conjunction with :gf:`stream-lock` to implement a basic stream
     locking facility.

   See also

   - :gf:`stream-lock`

.. generic-function:: stream-open?
   :open:

   Generic function for testing whether a stream is open.

   :signature: stream-open? *stream* => *open?*

   :parameter stream: An instance of :class:`<stream>`.
   :value open?: An instance of ``<boolean>``.

   :description:

     Returns ``#t`` if *stream* is open and ``#f`` if it is not.

   See also

   - :gf:`close`

.. generic-function:: stream-position
   :open:

   Finds the current position of a positionable stream.

   :signature: stream-position *positionable-stream* => *position*

   :parameter positionable-stream: An instance of :class:`<positionable-stream>`.
   :value position: An instance of `<position-type>`_.

   :description:

     Returns the current position of *positionable-stream* for reading
     or writing.

     The value returned can be either an instance of
     :class:`<stream-position>` or an integer. When the value is an
     integer, it is an offset from position zero, and is in terms of the
     stream’s element type. For instance, in a Unicode stream, a
     position of four means that four Unicode characters have been read.

   :example:

     The following example uses positioning to return the character "w"
     from a stream over the string *"hello world"*:

     .. code-block:: dylan

       let stream = make(<string-stream>,
                         contents: "hello world");
       stream-position(stream) := 6;
       read-element(stream);

   See also

   :class:`<position-type>`

.. class:: <stream-position>
   :abstract:

   The class representing non-integer stream positions.

   :superclasses: <object>

   :description:

     A direct subclass of ``<object>``. It is used in rare cases to
     represent positions within streams that cannot be represented by
     instances of ``<integer>``. For example, a stream that supports
     compression will have some state associated with each position in
     the stream that a single integer is not sufficient to represent.

     The ``<stream-position>`` class is disjoint from the class
     ``<integer>``.

   :operations:

     - :gf:`as`
     - :gf:`stream-position-setter`
     - :gf:`stream-size`

   See also

   - :class:`<position-type>`

.. generic-function:: stream-position-setter
   :open:

   Sets the position of a stream.

   :signature: stream-position-setter *position* *positionable-stream* => *new-position*

   :parameter position: An instance of :class:`<position-type>`.
   :parameter positionable-stream: An instance of :class:`<positionable-stream>`.
   :value new-position: An instance of :class:`<stream-position>`, or an
     instance of ``<integer>``.

   :description:

     Changes the stream’s position for reading or writing to *position*.

     When it is an integer, if it is less than zero or greater than
     *positionable-stream.stream-size* this function signals an error. For
     file streams, a :class:`<stream-position-error>` is signalled. For other types
     of stream, the error signalled is ``<simple-error>``.

     When *position* is a :class:`<stream-position>`, if it is invalid
     for some reason, this function signals an error. Streams are
     permitted to restrict the *position* to being a member of the set
     of values previously returned by calls to :gf:`stream-position` on
     the same stream.

     The *position* may also be ``#"start"``, meaning that the stream
     should be positioned at its start, or ``#"end"``, meaning that the
     stream should be positioned at its end.

     .. note:: You cannot use ``stream-position-setter`` to set the
       position past the current last element of the stream: use
       ``adjust-stream-position`` instead.

   See also

   - :gf:`adjust-stream-position`
   - :class:`<stream-position>`

.. generic-function:: stream-size
   :open:

   Finds the number of elements in a stream.

   :signature: stream-size *positionable-stream* => *size*

   :parameter positionable-stream: An instance of :class:`<positionable-stream>`.
   :value size: An instance of ``<integer>``, or ``#f``.

   :description:

     Returns the number of elements in *positionable-stream*.

     For input streams, this is the number of elements that were
     available when the stream was created. It is unaffected by any read
     operations that might have been performed on the stream.

     For output and input-output streams, this is the number of elements
     that were available when the stream was created (just as with input
     streams), added to the number of elements written past the end of
     the stream (regardless of any repositioning operations).

     It is assumed that:

     - There is no more than one stream open on the same source or
       destination at a time.
     - There are no shared references to files by other processes.
     - There are no alias references to underlying sequences, or any
       other such situations.

     In such situations, the behavior of ``stream-size`` is undefined.

.. class:: <string-stream>
   :open:
   :instantiable:

   The class of streams over strings.

   :superclasses: :class:`<sequence-stream>`

   :keyword contents: A general instance of ``<sequence>``.
   :keyword direction: Specifies the direction of the stream. It must
     be one of ``#"input"``, ``#"output"``, or ``#"input-output"``;
     Default value: ``#"input"``.
   :keyword start: An instance of ``<integer>``. Only valid when
     *direction:* is ``#"input"``. Default value: 0.
   :keyword end: An instance of ``<integer>``. This specifies the string
     position immediately after the portion of the string to stream over.
     Only valid when *direction:* is ``#"input"``. Default value:
     *contents.size*.

   :description:

     The class of streams over strings.

     The *contents:* init-keyword is used as the input for an input
     stream, and as the initial storage for an output stream.

     The *start:* init-keyword specifies the start position of the
     string to be streamed over.

     The class supports the same init-keywords as :class:`<sequence-stream>`.

     The *start:* and *end:* init-keywords specify the portion of the
     string to create the stream over: *start:* is inclusive and *end:*
     is exclusive. The default is to stream over the entire string.

   :operations:

     - :meth:`make(<string-stream>)`

   See also

   - :meth:`make(<string-stream>)`
   - :class:`<sequence-stream>`

.. generic-function:: synchronize-output
   :open:

   Synchronizes an output stream with the application state.

   :signature: synchronize-output *output-stream* => ()

   :parameter output-stream: An instance of :class:`<stream>`.

   :description:

     Forces any pending output from *output-stream*’s buffers to its
     destination. Before returning to its caller, ``synchronize-output``
     also attempts to ensure that the output reaches the stream’s
     destination before, thereby synchronizing the output destination
     with the application state.

     When creating new stream classes it may be necessary to add a
     method to the ``synchronize-output`` function, even though it is
     not part of the Stream Extension Protocol.

   See also

   - :gf:`force-output`

.. generic-function:: type-for-file-stream
   :open:

   Finds the type of file-stream class that needs to be instantiated for
   a given file.

   :signature: type-for-file-stream *filename* *element-type* #rest #all-keys => *file-stream-type*

   :parameter filename: An instance of ``<object>``.
   :parameter element-type: One of `<byte-character>`_,
     `<unicode-character>`_, or `<byte>`_, or ``#f``.
   :value file-stream-type: An instance of ``<type>``.

   :description:

     Returns the kind of file-stream class to instantiate for a given
     file. The method for :meth:`make(<file-stream>)` calls this function
     to determine the class of which it should create an instance.

   See also

   - :class:`<file-stream>`
   - :meth:`make(<file-stream>)`

.. generic-function:: type-for-sequence-stream
   :open:

   Finds the type of sequence-stream class that needs to be instantiated
   for a given sequence.

   :signature: type-for-sequence-stream *sequence* => *sequence-stream-type*

   :parameter sequence: An instance of ``<sequence>``.
   :value sequence-stream-type: An instance of ``<type>``.

   :description:

     Returns the sequence-stream class to instantiate over a given
     sequence object. The method for :meth:`make(<sequence-stream>)`
     calls this function to determine the concrete subclass of
     :class:`<sequence-stream>` that it should instantiate.

     There are ``type-for-sequence-stream`` methods for each of the
     string object classes. These methods return a stream class object
     that the Streams module considers appropriate.

   See also

   - :meth:`make(<sequence-stream>)`
   - :class:`<sequence-stream>`

<unicode-character>
^^^^^^^^^^^^^^^^^^~

   :type:    The type that represents Unicode characters.

Supertypes

``<character>``

Init-keywords

None.

   :description:

A type representing Unicode characters that instances of
``<unicode-string>`` can contain.

Operations

- :gf:`type-for-file-stream`

.. class:: <unicode-string-stream>
   :open:
   :instantiable:

   The class of streams over Unicode strings.

   :superclasses: :class:`<string-stream>`

   :keyword contents: A general instance of ``<sequence>``.
   :keyword direction: Specifies the direction of the stream. It must
     be one of ``#"input"``, ``#"output"``, or ``#"input-output"``.
     Default value: ``#"input"``.
   :keyword start: An instance of ``<integer>``. This specifies the
     start position of the sequence to be streamed over. Only valid when
     *direction:* is ``#"input"``. Default value: 0.
   :keyword end: An instance of ``<integer>``. This specifies the
     sequence position immediately after the portion of the sequence to
     stream over. Only valid when *direction:* is ``#"input"``. Default
     value: *contents.size*.

   :description:

     The class of streams over Unicode strings. It is a subclass of
     :class:`<string-stream>`.

     The *contents:* init-keyword is used as the input for an input
     stream, and as the initial storage for an output stream. If it is a
     stretchy vector, then it is the only storage used by the stream.

     The class supports the same init-keywords as
     :class:`<sequence-stream>`.

     The *start:* and *end:* init-keywords specify the portion of the
     Unicode string to create the stream over: *start:* is inclusive and
     *end:* is exclusive. The default is to stream over the entire
     Unicode string.

   :operations:

     - :meth:`make(<unicode-string-stream>)`

   See also

   - :meth:`make(<unicode-string-stream>)`
   - :class:`<sequence-stream>`

.. generic-function:: unread-element
   :open:

   Returns an element that has been read back to a positionable stream.

   :signature: unread-element *positionable-stream* *element* => *element*

   :parameter positionable-stream: An instance of :class:`<positionable-stream>`.
   :parameter element: An instance of ``<object>``.
   :value element: An instance of ``<object>``.

   :description:

     "Unreads" the last element from *positionable-stream*. That is, it
     returns *element* to the stream so that the next call to
     :gf:`read-element` will return *element*. The stream must be a
     :class:`<positionable-stream>`.

     It is an error to do any of the following:

     - To apply ``unread-element`` to an element that is not the element
       most recently read from the stream.
     - To call ``unread-element`` twice in succession.
     - To unread an element if the stream is at its initial position.
     - To unread an element after explicitly setting the stream’s position.

   See also

   - :gf:`read-element`

.. generic-function:: wait-for-io-completion

   Waits for all pending operations on a stream to complete.

   :signature: wait-for-io-completion *file-stream* => ()

   :parameter file-stream: An instance of :class:`<stream>`.

   :description:

     If *file-stream* is asynchronous, waits for all pending write or
     close operations to complete and signals any queued errors. If
     *file-stream* is not asynchronous, returns immediately.

.. macro:: with-open-file
   :statement:

   Runs a body of code within the context of a file stream.

   :macrocall:
     .. code-block:: dylan

       with-open-file (*stream-var* = *filename*, #rest *keys*)
         *body* end => *values*

   :parameter stream-var: An Dylan variable-name *bnf*.
   :parameter filename: An instance of ``<string>``.
   :parameter keys: Instances of ``<object>``.
   :parameter body: A Dylan body *bnf*.
   :value values: Instances of ``<object>``.

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

   See also

   - :meth:`close(<file-stream>)`
   - :class:`<file-stream>`
   - :meth:`make(<file-stream>)`

.. class:: <wrapper-stream>
   :open:
   :instantiable:

   The class of wrapper-streams.

   :superclasses: :class:`<stream>`

   :keyword inner-stream: An instance of :class:`<stream>`.

   :description:

     The class that implements the basic wrapper-stream functionality.

     It takes a required init-keyword *inner-stream:*, which is used to
     specify the wrapped stream.

     The ``<wrapper-stream>`` class implements default methods for all
     of the stream protocol functions described in this document. Each
     default method on ``<wrapper-stream>`` simply "trampolines" to its
     inner stream.

   :operations:

     - :gf:`inner-stream`
     - :gf:`inner-stream-setter`
     - :gf:`outer-stream-setter`

   :example:

     In the example below, ``<io-wrapper-stream>``, a subclass of
     ``<wrapper-stream>``, "passes on" functions such as
     :gf:`read-element` and :gf:`write-element` by simply delegating these
     operations to the inner stream:

     .. code-block:: dylan

       define method read-element (ws :: <io-wrapper-stream>,
         #key on-end-of-stream)
        => (element)
         read-element(ws.inner-stream)
       end method;

       define method write-element (ws :: <io-wrapper-stream>,
         element)
        => ()
         write-element(ws.inner-stream, element)
       end method;

     Assuming that ``<io-wrapper-stream>`` delegates all other
     operations to its inner stream, the following is sufficient to
     implement a 16-bit Unicode character stream wrapping an 8-bit
     character stream.

     .. code-block:: dylan

       define class <unicode-stream> (<io-wrapper-stream>)
       end class;

       define method read-element (s :: <unicode-stream>,
         #key on-end-of-stream)
        => (ch :: <unicode-character>)
         with-stream-locked (s)
           let first-char = read-element(s.inner-stream,
                                         on-end-of-stream);
           let second-char = read-element(s.inner-stream,
                                          on-end-of-stream);
         end;
         convert-byte-pair-to-unicode(first-char, second-char)
       end method;

       define method write-element (s :: <unicode-stream>,
         c :: <character>)
        => ()
         let (first-char, second-char)
           = convert-unicode-to-byte-pair(c);
         with-stream-locked (s)
           write-element(s.inner-stream, first-char);
           write-element(s.inner-stream, second-char)
         end;
         c
       end method;

       define method stream-position (s :: <unicode-stream>)
        => (p :: <integer>)
         truncate/(stream-position(s.inner-stream), 2)
       end method;

       define method stream-position-setter (p :: <integer>,
           s :: <unicode-stream>);
         stream-position(s.inner-stream) := p * 2
       end method;

.. generic-function:: write
   :open:

   Writes the elements of a sequence to an output stream.

   :signature: write *output-stream* *sequence* #key *start* *end* => ()

   :parameter output-stream: An instance of :class:`<stream>`.
   :parameter sequence: An instance of ``<sequence>``.
   :parameter #key start: An instance of ``<integer>``. Default value: 0.
   :parameter #key end: An instance of ``<integer>``. Default value: *sequence.size*.

   :description:

     Writes the elements of *sequence* to *output-stream*, starting at
     the stream’s current position.

     The elements in *sequence* are accessed in the order defined by the
     forward iteration protocol on ``<sequence>``. This is effectively
     the same as the following:

     .. code-block:: dylan

       do (method (elt) write-element(stream, elt)
           end, sequence);
       sequence;

     If supplied, *start* and *end* delimit the portion of *sequence* to
     write to the stream. The value of *start* is inclusive and that of
     *end* is exclusive.

     If the stream is positionable, and it is not positioned at its end,
     ``write`` overwrites elements in the stream and then advances the
     stream’s position to be beyond the last element written.

     *Implementation Note:* Buffered streams are intended to provide a
     very efficient implementation of ``write``, particularly when
     sequence is an instance of ``<byte-string>``, ``<unicode-string>``,
     :class:`<byte-vector>`, or :class:`<buffer>`, and the stream’s element type is
     the same as the element type of sequence.

   :example:

     The following forms bind *stream* to an output stream over an empty
     string and create the string "I see!", using the function
     :gf:`stream-contents` to access all of the stream’s
     elements.

     .. code-block:: dylan

       let stream = make(<byte-string-stream>,
                         direction: #"output");
       write-element(stream, ’I’);
       write-element(stream, ’ ’);
       write(stream, "see");
       write-element(stream, ’!’);
       stream-contents(stream);

   See also

   - :gf:`read`
   - :gf:`write-element`
   - :gf:`write-line`

.. generic-function:: write-element
   :open:

   Writes an element to an output stream.

   :signature: write-element *output-stream* *element* => ()

   :parameter output-stream: An instance of :class:`<stream>`.
   :parameter element: An instance of ``<object>``.

   :description:

     Writes *element* to *output-stream* at the stream’s current
     position. The output-stream must be either ``#"output"`` or
     ``#"input-output"``. It is an error if the type of *element* is
     inappropriate for the stream’s underlying aggregate.

     If the stream is positionable, and it is not positioned at its end,
     ``write-element`` overwrites the element at the current position and
     then advances the stream position.

   :example:

     The following forms bind *stream* to an output stream over an empty
     string and create the string "I do", using the function :gf:`stream-contents` to access all of the stream’s
     elements.

     .. code-block:: dylan

       let stream = make(<byte-string-stream>,
                         direction: #"output");
       write-element(stream, ’I’);
       write-element(stream, ’ ’);
       write-element(stream, ’d’);
       write-element(stream, ’o’);
       stream-contents(stream);

   See also

   - :gf:`read-element`
   - :gf:`write`
   - :gf:`write-line`

.. generic-function:: write-line
   :open:

   Writes a string followed by a newline to an output stream.

   :signature: write-line *output-stream* *string* #key *start* *end* => ()

   :parameter output-stream: An instance of :class:`<stream>`.
   :parameter string: An instance of ``<string>``.
   :parameter #key start: An instance of ``<integer>``. Default value: 0.
   :parameter #key end: An instance of ``<integer>``. Default value: *string.size*.

   :description:

     Writes *string* followed by a newline sequence to *output-stream*.

     The default method behaves as though it calls :gf:`write` on *string*
     and then calls :gf:`new-line`.

     If supplied, *start* and *end* delimit the portion of *string* to
     write to the stream.

   See also

   - :gf:`new-line`
   - :gf:`read-line`
   - :gf:`write`
   - :gf:`write-element`
