*****************
Generic Functions
*****************

Methods in Dylan aren't tied directly to a particular class like they are in many other languages.
Instead a method belongs to a generic function (GF).  Which method actually gets called is 
decided by the types of all of the required arguments passed to the GF.  Consider the built-in
:drm:`add` generic function:

.. code-block:: dylan

    define open generic add (seq :: <sequence>, new-element) => (new-seq :: <sequence>);

This says that any method defined on :drm:`add` must accept two required arguments.
The first argument must be a :drm:`<sequence>` and the second can be anything, since
no type was declared for it.  The methods must return a single value which is a :drm:`<sequence>`.

Also:

#. The methods on this GF may not accept any optional (keyword)  arguments.
#. The "open" adjective tells the compiler that other libraries may add methods to
   this GF.

If you try to define a method on ``add`` that breaks any of the above rules, you'll
get a warning from the compiler.

Note that the ``define generic`` form doesn't contain any actual code; it merely
defines an API to which all other methods with the same name must conform.  To
define a very general method on ``add`` we write this:

.. code-block:: dylan

    define method add (seq :: <sequence>,  new-element) => (new-seq :: <sequence>)
      concatenate-as(type-for-copy(seq), list(new-element), seq)
    end;

Now, let's say you decide it would be good to have a method specifically for
:drm:`<vector>` because you think it will be more efficient than the default implementation,
even though the default will do the right thing.  You could write this:

.. code-block:: dylan

    define method add (v :: <vector>, new-element) => (new-v :: <vector>)
      let new-vector = make(<vector>, size: v.size + 1);
      map-into!(new-vector, identity, v);
      new-vector[v.size] := new-element;
      new-vector
    end;

(*Note that the above is just an example and isn't intended to be super-efficient.*)

Now, if you use ``add(#[1, 2, 3], 4)`` the method for :drm:`<vector>` will be called
because the first argument is a ``<vector>``, and if you use ``add(#(1, 2, 3), 4)``
then the first method will be called because the first argument is a :drm:`<list>`, 
which is a :drm:`<sequence>` but is not a :drm:`<vector>`.

Very often, the compiler can figure out which method to call and so no run-time
overhead for dispatch is incurred in those cases.  (The Open Dylan IDE has a
tool to show whether method dispatch was optimized.)

Note that the second method above has a more specific return type.  This
can aid in compiler optimization also.
