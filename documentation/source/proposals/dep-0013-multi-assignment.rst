*************************
Multiple Value Assignment
*************************

===============  =============================================
DEP #:           13
Type:            Standards Track
Affects-DRM:     Yes
Author:          Carl Gay
Status:          Draft
Created:         10-Dec-2024
Last-Modified:   10-Dec-2024
Post-History:    None
Target-Version:  2025.1
===============  =============================================


Abstract
========

Add new syntax to assign multiple variables from a single multi-valued expression.

Specification
=============

The DRM defines the syntax of assignment expressions to be::

  place := new-value

This DEP adds new syntax on the left hand side to allow for multiple *places* to be set
from the multiple values returned by the ``new-value`` expression, which we rename here
to ``new-values``::

  (place1, place2, ...) := new-values

The first return value is assigned to *place1*, the second value to *place2*, etc.  The
last *place* may be preceded by ``#rest``, in which case it is assigned a sequence
containing all the remaining values.

If there are more *places* than values returned by ``new-values``, the extra *places* are
assigned the value :drm:`#f`.

The name ``_`` (underscore) may be used to indicate that a value is to be ignored. For
example, ``(_, remainder) := round(3.2)`` will ignore the first return value and assign
``remainder`` the value ``0.2``.

The order of assignment of values to *places* is unspecified.


BNF Changes
-----------

No changes are required to the Dylan BNF as described in the DRM.  ``LEAF := EXPRESSION``
is already valid and ``( EXPRESSION )`` syntax is a valid ``LEAF`` production.


Rationale
=========

Multiple value assignment syntax naturally matches the existing multiple value binding
syntax provided by :drm:`let` and can occasionally be a useful tool to make code more
concise.


Examples
========

Some examples from the Open Dylan sources that can be made more concise:

.. code:: dylan

   // This occurs a lot in dfmc/conversion/convert.dylan
   let (f, l) = join-2x1!(first, last, comp); first := f;  last := l; // old
   (first, last) := join-2x1!(first, last, comp);                     // new

.. code:: dylan

   // old:
   define sealed method set-union!
       (set1 :: <bit-set>, set2 :: <bit-set>) => (set1 :: <bit-set>)
     let (vector, pad)
       = bit-vector-or!(set1.member-vector, set2.member-vector,
                        pad1: set1.member-vector-pad,
                        pad2: set2.member-vector-pad);
     set1.member-vector := vector;
     set1.member-vector-pad := pad;
     set1
   end method;

   // new:
   define sealed method set-union!
       (set1 :: <bit-set>, set2 :: <bit-set>) => (set1 :: <bit-set>)
     (set1.member-vector, set1.member-vector-pad)
       := bit-vector-or!(set1.member-vector, set2.member-vector,
                         pad1: set1.member-vector-pad,
                         pad2: set2.member-vector-pad);
     set1
   end method;

.. code:: dylan

   // old:
   for (i from 0 below n)
     let (thread, result) = join-thread(threads[i]);
     results[i] := result;
   end for;

   // new:
   for (i from 0 below n)
     (_, results[i]) := join-thread(threads[i]);
   end for;

.. code:: dylan

   // old:
   define method stop-profiling-type
       (state :: <profiling-state>, keyword :: <cpu-profiling-type>) => ()
     when (element(state, #"cpu-profiling", default: #f))
       let (seconds, microseconds) = timer-stop(state[#"cpu-profiling-timer"]);
       state[#"cpu-time-seconds"]      := seconds;
       state[#"cpu-time-microseconds"] := microseconds;
       state[#"cpu-profiling"]         := #f;
       state[#"cpu-profiling-timer"]   := #f;
     end
   end method stop-profiling-type;

   // new:
   define method stop-profiling-type
       (state :: <profiling-state>, keyword :: <cpu-profiling-type>) => ()
     when (element(state, #"cpu-profiling", default: #f))
       (state[#"cpu-time-seconds"], state[#"cpu-time-microseconds"])
         := timer-stop(state[#"cpu-profiling-timer"]);
       state[#"cpu-profiling"]         := #f;
       state[#"cpu-profiling-timer"]   := #f;
     end
   end method stop-profiling-type;

.. code:: dylan

   // old:
   let (sec, nsec) = %timer-current-time();
   timer.timer-started-seconds := sec;
   timer.timer-started-nanoseconds := nsec;

   // new:
   (timer.timer-started-seconds, timer.timer-started-nanoseconds)
     := %timer-current-time();

.. code:: dylan

   // old:
   let (name, #rest arguments) = tokenize-command-line(command-line);
   *application-name* := name;
   *application-arguments* := apply(vector, arguments);

   // new:
   (*application-name*, #rest *application-arguments*) := tokenize-command-line(command-line);

.. code:: dylan

   // old:
   let (ins, outs) = split-operation-arguments(request-arguments(request));
   request-in-args(request) := ins;
   request-out-args(request) := outs;

   // new:
   (request-in-args(request), request-out-args(request))
     := split-operation-arguments(request-arguments(request));

.. code:: dylan

   // old:
   let (_n, _what) = goto-position-dialog(window, what | #"line");
   n    := _n;
   what := _what

   // new:
   (n, what) := goto-position-dialog(window, what | #"line");

.. code:: dylan

   // old:
   let (width, height) = frame-size(dialog);
   $buffer-box-width  := width;
   $buffer-box-height := height;

   // new:
   ($buffer-box-width, $buffer-box-height) = frame-size(dialog);


Alternatives Considered
=======================

1. One might consider it desirable to allow the left hand side of ``:=`` expressions to
   omit the parentheses in multiple value assignments when it is unambiguous. That is, to
   allow both ``(a, b) := f()`` and ``a, b := f()`` as long as no ambiguity exists.

   In certain contexts parentheses would be required for disambiguation, for example in a
   argument list context:

   .. code:: dylan

      vector(a, b := f())

   Is the above equivalent to ``vector(a, f())`` or ``vector(f())``?

   .. code:: dylan

      vector((a, b) := f())

   The above is unambiguously equivalent to ``vector(f())`` since multiple value
   assignment always returns the first assigned value.

   However, in a body context (i.e., a sequence of semicolon delimited statements)
   omitting the parentheses may be desirable as it is less "noisy":

   .. code:: dylan

      ...; a, b := f(); ...

   **Decision:** To remove any ambiguity, and to match the existence of parentheses for
   multiple value *bindings* with :drm:`let`, parentheses will be *required* for multiple
   value assignments.


Reference Implementation
========================

TODO: permalink


Revision History
================

The revision history of this document is available here:
https://github.com/dylan-lang/opendylan/commits/master/documentation/source/proposals/dep-0013-multi-assignment.rst
