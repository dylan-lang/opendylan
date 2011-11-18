********************************************
Dispatch Optimization Coloring in the Editor
********************************************

This chapter is about source-code coloring in the editor that shows
where and how optimizations have taken place.

.. note:: Optimization work is best done in Production mode.
   See :ref:`project-settings`.

.. index::
   single: applications; optimization
   single: optimization; coloring in the editor

About dispatch optimizations
============================

When you call a generic function in Dylan, the method that will be
executed has to be selected from the set of methods defined on that
generic function.

The method is selected by comparing the types of the arguments passed in
the generic function call to the parameter lists of the methods
available; the method whose parameter list is closest to the types of
the arguments passed in the call is the one that will be selected. (Note
that there may be situations where no one method is more applicable than
another, or even where there is no applicable method at all. These
situations, which may be detected either at compile time or at run time,
signal an error.)

The process of selecting the right method to call is known as *method
dispatch*. The algorithm for selecting a method is described in Chapter
6 of the DRM.

Method dispatch can, in principle, occur entirely at run time; but in
some circumstances, the Open Dylan compiler can work out at compile
time precisely which method needs to be called, and so optimize away the
need for run-time dispatch, making delivered applications faster.
Depending on the circumstances, these *dispatch optimizations* can
consist of replacing the generic function call with a direct call to the
correct method; replacing the generic function call with a class slot
access; or inlining the call completely.

.. index:: Color Dispatch Optimizations menu command

Optimization coloring
=====================

When you compile a project, the Open Dylan compiler records the
kinds of optimizations it performed for each source code file in the
project. It also records cases where compile-time optimization was for
one reason or another not possible.

The Open Dylan editor provides a way to see this information, by
choosing **View > Color Dispatch Optimizations**. This command colors a
source code file so that you can see where the compiler managed to
optimize method dispatch, and also places where you may be able to make
changes that will make dispatch optimizations possible next time you
compile the project.

For instance, you will see a generic function call colored in blue where
the compiler worked out the exact method to call and emitted a direct
call to that method in the compiled code. :ref:`dispatch-optimization-colors`
contains a full list of colors and their meanings.

Editing colored source code
---------------------------

Dispatch optimization coloring in a file shows only what the compiler
achieved the last time you compiled the project containing the file. If
you edit colored source code, the changes you make could affect the
optimizations the compiler can carry out upon it the next time you
compile the project.

For this reason, when **View > Color Dispatch Optimizations** is on and
you edit a colored line, the editor resets the entire line to black. All
the other lines in the file keep their color.

In this situation, you can re-color the line by doing **View > Refresh**.
Note that once you have edited the text, the coloring information may no
longer fit it, but you may still find it useful. Alternatively you may
prefer to turn off coloring altogether once the coloring information for
part of the file has been invalidated.

Effect of compilation mode on dispatch optimizations
----------------------------------------------------

The Open Dylan compiler tries to optimize method dispatch whether
compiling a project in Interactive Development mode or in Production
mode. Because the compiler does more optimization in Production mode,
you will see more coloring information after a Production-mode build.

.. _dispatch-optimization-colors:

Dispatch optimization colors and their meanings
-----------------------------------------------

The following table shows the colors used to indicate different kinds of
dispatch optimization.

+-------------+-------------------------------+--------------------------+
| Color       | Meaning                       | Recommended Action       |
+=============+===============================+==========================+
| Magenta     | Call not optimized because    | Happens when the generic |
|             | the compiler could not        | function is open. Where  |
|             | determine all the applicable  | possible, turn open      |
|             | methods.                      | protocols into sealed    |
|             |                               | protocols.               |
+-------------+-------------------------------+--------------------------+
| Red         | Call not optimized despite    | Where possible, add type |
|             | the compiler finding all the  | specializers to the      |
|             | applicable methods.           | bindings of the          |
|             |                               | arguments to the call.   |
|             | Can happen when the type of   |                          |
|             | an argument is not specific   |                          |
|             | enough.                       |                          |
+-------------+-------------------------------+--------------------------+
| Blue        | Call optimized. The compiler  | None required.           |
|             | found the appropriate method, |                          |
|             | and emitted a direct call to  |                          |
|             | it.                           |                          |
+-------------+-------------------------------+--------------------------+
| Green       | Call optimized. The compiler  | None required.           |
|             | found that this call was an   |                          |
|             | access to a slot whose        |                          |
|             | position in a class is fixed. |                          |
|             | It replaced the function call |                          |
|             | with (faster) code to access  |                          |
|             | that slot value.              |                          |
+-------------+-------------------------------+--------------------------+
| Dark Gray   | Call optimized. The compiler  | None required.           |
|             | inlined the code of the       |                          |
|             | appropriate method.           |                          |
+-------------+-------------------------------+--------------------------+
| Light Gray  | This code was eliminated      | None required. (Unless   |
|             | completely. The compiler      | the code should have     |
|             | either determined that this   | been called.)            |
|             | call would never be made or   |                          |
|             | that it would not make any    |                          |
|             | difference to the outcome of  |                          |
|             | other code with which it was  |                          |
|             | associated, or it managed to  |                          |
|             | evaluate the call directly.   |                          |
+-------------+-------------------------------+--------------------------+

Where possible, add type specializers to the bindings of the arguments
to the call.

Optimizing the Reversi application
==================================

In this section we look at the dispatch optimization color information
for part of the Reversi application and see what we can do to optimize
it.

Before doing that, we should build the Reversi application in Production
mode so we know that the application has been optimized as much as
possible.

Open the Reversi project.

#. Choose **Project > Settings** and, on the Compile page, set the
   compilation mode to “Production mode”.
#. Choose **Project > Clean Build**.
#. When the build is complete, go to the Sources page and open the file
   *game.dylan*.

An editor window showing *game.dylan* appears.

#. In the editor window, turn on the **View > Color Dispatch
   Optimizations** check item.

We can now see color information showing how dispatch optimizations were
or were not carried out during the last build.

#. Go to the definition of the method ``<reversi-game>``.

You can use **Edit > Find** or the “binoculars” toolbar button to do this.

This is the definition of ``<reversi-game>`` :

.. code-block:: dylan

    define class <reversi-game> (<object>)
      slot reversi-game-board :: <reversi-board> = make(<reversi-board>);
      slot %player :: <player> = #"black",
        init-keyword: player:;
      slot %players :: <integer> = 1,
        init-keyword: players:;
      slot black-algorithm :: <algorithm> = default-algorithm-for-player(#"black"),
        init-keyword: black-algorithm:;
      slot white-algorithm :: <algorithm> = default-algorithm-for-player(#"white"),
        init-keyword: white-algorithm:;
      slot reversi-game-update-callback :: <function> = always(#f),
        init-keyword: update-callback:;
      slot reversi-game-message-function :: false-or(<function>) = #f,
        init-keyword: message-function:;
    end class <reversi-game>;

There are three different colorings in this definition. The call to the
function *always*, a Dylan language built-in function, is in light
gray. That means the call has been eliminated completely from the
compiled application. A call to the function *always* is defined to
return a function object that always returns the value passed in the
call to *always*. So here, the function object would always return ``#f``.
Unsurprisingly, the compiler evaluated this call completely, avoiding
the need for run-time method dispatch.

The two calls to *default-algorithm-for-player*, a Reversi application
method from *algorithms.dylan*, are colored in blue, signifying that
the compiler managed to determine precisely which method to call, and
inserted a direct call to that method in the compiled application.
Again, the need for run-time method dispatch was averted.

Investigation shows that there is only one method on
*default-algorithm-for-player*, which makes blue optimization simple
here. The generic function for *default-algorithm-for-player* is defined
implicitly, in the single ``define method default-algorithm-for-player``
call. Recall from the DRM (chapter 6) that implicitly defined generic
functions are sealed by default. That fact allows the compiler to conclude
that this method is the only method there will ever be on
*default-algorithm-for-player*, making the optimization possible.

The third coloring is magenta, in the call to ``make`` on
``<reversi-board>``, in the *reversi-game-board* slot definition. Here,
then, is a generic function call that was not optimized. Magenta
coloring means that for this call to ``make``, the compiler could not
determine the complete set of methods from which it could attempt to
select the appropriate method to call. We will now make changes to the
Reversi sources to optimize this call.

The problem here is that the compiler cannot be sure that additional
methods on ``make`` might not be added at run time. By defining a sealed
domain on make for ``<reversi-board>``, we can clear this up.

#. Add the following to *game.dylan* :

.. code-block:: dylan

    define sealed domain make(subclass(<reversi-board>));

With this information, the compiler knows it has access to the complete
set of methods on ``make`` for this class, and therefore can attempt to do
the method selection itself.

We can recompile the application to see what effect our change has had.

#. Save *game.dylan* with **File > Save**.
#. Rebuild the application, and refresh the color information for
   *game.dylan* with **View > Refresh**.

The refreshed coloring shows the call to ``make`` on ``<reversi-board>`` in
the *reversi-game-board* slot definition in light gray. This coloring
means that the compiler determined which ``make`` method to call, computed
the result of the call—a ``<reversi-board>`` object—and inlined the
object.

Looking further down *game.dylan*, notice that the definition of
*reversi-game-size-setter* also calls ``make`` on ``<reversi-board>``, a
call that is also colored light gray.

We can now look at other possible optimizations in *game.dylan*.

#. Go to the definition of the method *initialize-board*.

The definition of *initialize-board* is:

.. code-block:: dylan

    define method initialize-board (board :: <reversi-board>) => ()
      let squares = reversi-board-squares(board);
      for (square from 0 below size(squares))
        squares[square] := #f
      end;
      for (piece in initial-pieces(board))
        let square = piece[0];
        squares[square] := piece[1]
      end;
    end method initialize-board;

In this method there is a green-colored call to *reversi-board-squares*
on the parameter *board*, an instance of ``<reversi-board>``. Green
coloring denotes an access to a slot whose position in a class is fixed.
This optimization was possible because the *reversi-board-squares*
method is just the implicitly defined accessor for the slot
*reversi-board-squares* :

.. code-block:: dylan

    define class <reversi-board> (<object>)
      slot reversi-board-size :: <integer> = $default-board-size,
        init-keyword: size:;
      slot reversi-board-squares :: <sequence> = #[];
    end class <reversi-board>;

The compiler achieved this optimization because it knew three things.
First, it knew that the generic function implicitly defined by the
accessor method was sealed. (As normal Dylan methods, accessor methods
implicitly define a generic function if one does not already exist; such
a generic function is sealed because implicitly defined generic
functions are sealed by default.) Second, the compiler knew the type of
*board* in the call to the accessor method. Third, the compiler knew
that the class ``<reversi-board>`` was sealed, because classes are sealed
by default.

We can now move on to some other optimization. The call *size(squares)*
in *initialize-board* is colored in magenta. There are several similar
magenta colorings in *game.dylan*, where the compiler could not
optimize a method call on the value returned from
*reversi-board-squares* : calls to *element*, *element-setter*,
*empty?*, and *size*. In all cases this is because the type of
*reversi-board-squares* is ``<sequence>``, which is an open class.

We could seal domains on ``<sequence>`` to get optimizations here. But the
DRM defines ``<sequence>`` as an open class, and it is not good practice
to seal protocols that do not belong to your library or libraries.
However, we can change the type of *reversi-board-squares* to be in a
domain which is already sealed. Changing the slot type to
``<simple-object-vector>`` gives us a sealed type as well as preserving
the protocol in use, so that we do not have to change any of the calls
being made.

#. Go to the definition of ``<reversi-board>``.
#. Change the type of *reversi-board-squares* to be
   ``<simple-object-vector>``.
#. Save *game.dylan* with **File > Save**.
#. Rebuild the application, and refresh the color information for
   *game.dylan* with **View > Refresh**.
#. Go back to the definition of *initialize-board*.

The *size(squares)* call is now colored green. Green coloring means the
compiler determined that the call was equivalent to a slot
access—particularly, an access to slot having a fixed offset from the
memory address at which its class is located. The compiler removed the
need for run-time method dispatch by replacing the call with code to
access the location that would contain the slot value.

This particular optimization was possible because *size* is a slot
accessor for instances of ``<simple-object-vector>``, and, of course,
because ``<simple-object-vector>`` is sealed.

You could examine the effects of this change on other calls that use the
return value of *reversi-board-squares*. Some calls turn blue. Some
calls to *element-setter* remain magenta because the compiler does not
know the type of the index. Constraining the type of the index would
improve such a call, turning it blue or even dark gray (inlined).


