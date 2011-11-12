*************
Limited Types
*************

Limited types provide a way to define new, restricted types based on
existing types in a way that is different from subclassing.  For example,
there is no built-in ``<byte>`` type, but it could be defined as follows:

.. code-block:: dylan

    define constant <byte> = limited(<integer>, min: 0, max: 255);

Dylan compilers can often provide improved error checking and
optimizations for limited types.  For example, this code could
generate a compile time error:

.. code-block:: dylan

    define function add1 (b :: <byte>) => (b2 :: <byte>) b + 1 end;
    add1(255);

The set of limited types that are supported depends on the compiler.

Collection classes can be limited to containing a specific type of
element:

.. code-block:: dylan

    define constant <int-vector> = limited(<vector>, of: <integer>);

Open Dylan will optimize accesses into an ``<int-vector>`` defined
as above.

A Dylan compiler can avoid doing bounds checking or
type checking, and can use an efficient representation of the
vectors of floating point numbers for this code:

.. code-block:: dylan

    define constant $audio-buffer-size = 2048;
    define constant <audio-buffer> =
      limited(<vector>,
	      of: <single-float>,
	      size: $audio-buffer-size);

    define function mix-buffers
	(input1 :: <audio-buffer>, input2 :: <audio-buffer>,
	 output :: <audio-buffer>)
     => ()
      for (i from 0 below $audio-buffer-size)
	output[i] = 0.5 * input1[i] + 0.5 * input2[i];
      end;
    end;

