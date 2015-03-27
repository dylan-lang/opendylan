**************************
Notes, Warnings and Errors
**************************

.. current-library:: dfmc-conditions
.. current-module:: dfmc-conditions

.. contents::
   :local:

The compiler provides a framework for handling notes, warnings and
errors as they're encountered and created during the compilation
process.

This code can be found in the `dfmc-conditions`_ library. Initial
skeletal API documentation for this library can be found at
`The DFMC-CONDITIONS API Reference`_.

.. _dfmc-conditions: https://github.com/dylan-lang/opendylan/tree/master/sources/dfmc/conditions

Conceptually, all notes, warnings and errors are subtypes of
:drm:`<condition>`, so we will often refer to them collectively
as *program conditions* or instances of :class:`<program-condition>`.

Status of this Library
**********************

This library is interesting (to some) as it is in a half-completed
state and many things are not yet fully implemented or used. We will
try to note these things below when we discuss them.

Philosophy
**********

The Open Dylan compiler tries hard to not fail at any given
point where an error or otherwise is noted. This leads to many
things that might be *fatal errors* elsewhere being represented
as *serious warnings* here.

In part, this was part of the development model under the
Open Dylan IDE which was very exploratory in nature and many
errors could be corrected while an application was running.

This workflow isn't commonly used with Dylan today, so we
may revise this philosophy and how it applies to the compiler's
handling of program conditions.

Another key element to how program conditions are currently
used today is that when we store values on them, we store it as
the raw object so that it is readily accessible from within
the debugger, rather than only storing the string representations
that would have been otherwise available.

Program Condition Hierarchy
***************************

The root of the condition hierarchy is :class:`<program-condition>`.
This is an abstract class and one of the subclasses such as
:class:`<program-note>`, :class:`<program-error>` or
:class:`<program-restart>` should be subclassed instead.

Reporting a Program Condition
*****************************

The typical way to report that a program condition has arisen
is to use :gf:`note`. There are other mechanisms, such as
:gf:`raise`, :gf:`restart`, :gf:`simple-note` and :gf:`simple-raise`,
but these are not in common usage.

For proper error reporting, you will want to try to report as
accurate a *source location* as you possibly can. This can be
tricky at first, so look at other similar warnings if you need
the assistance.

The actual code for noting a program condition is pretty
straightforward, once you've identified the location to emit
the program condition, and the type of program condition to emit.

.. code-block:: dylan

   note(<wrong-type-in-assignment>,
        variable-name: the-name,
        type: binding-type,
        rhs: rhs-value,
        source-location: fragment-source-location(fragment));

Source Locations
================

There are a couple of useful rules to follow for getting source
locations for noting a program condition during compilation.

* If you're in C-FFI, you're probably working with fragments,
  and so ``fragment-source-location`` is the right function.
* If you're in ``dfmc-definitions``, then you probably also
  want ``fragment-source-location``.
* If you're in conversion, you may be dealing with either
  fragments or model objects. For fragments, you want
  ``fragment-source-location``. For model objects, you want
  ``model-source-location``.
* If you're in ``dfmc-optimization``, then you may likely
  want ``dfm-source-location`` is you're working with a
  object that is part of the control flow or data flow
  graphs (like any computation or temporary). However,
  in some cases, you'll still be working with model objects,
  so keep an eye out for when you need to use
  ``model-source-location``.

Defining a new Program Condition
********************************

Depending on where you are defining your new program condition
within the `Program Condition Hierarchy`_, you will need to use
the appropriate program condition definer:

* :macro:`performance-note-definer`
* :macro:`portability-note-definer`
* :macro:`program-condition-definer`
* :macro:`program-error-definer`
* :macro:`program-note-definer`
* :macro:`program-restart-definer`
* :macro:`program-warning-definer`
* :macro:`run-time-error-warning-definer`
* :macro:`serious-program-warning-definer`
* :macro:`style-warning-definer`

An example definition looks like:

.. code-block:: dylan

   define program-warning <ambiguous-copy-down-method>
     slot condition-method, required-init-keyword: meth:;
     slot condition-other-methods, required-init-keyword: other-methods:;
     format-string "Multiple applicable copy-down methods for %s, picking one at random";
     format-arguments meth;
   end;

An interesting thing to note here is that the *other-methods* are being
recorded by this :class:`<program-note>` even though they are not used
within the formatted output. This is because the additional values can
be useful when viewing the condition within the debugger or by other
programmatic processing such as filtering.

PPML, Pretty Print Markup Language
**********************************

When conditions are stored, their slots are converted to PPML
objects. Many objects within the compiler are already configured
to be able to generate PPML via the many specializations of
``as(class == <ppml>, ...)`` that can be found within the
``dfmc-debug-back-end`` (see `print-condition.dylan`_).

Slots are converted to PPML representations via code that
is autogenerated by the various definer macros which create
a specialization on :gf:`convert-condition-slots-to-ppml`.

Filtering of Program Conditions
*******************************

This is functionality that has not been completed and is
currently not entirely in use.

*To be written.*

How Warnings Are Displayed and Recorded
***************************************

*To be written.*

Responding to a Program Condition
*********************************

In Dylan, the condition system allows for responses to conditions
and can restart a computation with new information. While parts
of ``dfmc-conditions`` are designed to permit this, this functionality,
has never been completed and is not yet working.

Future Work
***********

Look at cleaning up unused API and things that are no longer
necessary.

* :gf:`obsolete-condition?` is probably obsolete.
* :gf:`format-condition` and related code including :type:`<detail-level>`
  are probably no longer necessary with the code in ``dfmc-debug-back-end``
  and the specialization on ``print-object`` present there.
* The specialization on :gf:`print-object` can probably go away.
* :gf:`simple-note` and :gf:`simple-raise` can go away.
* There is a comment in ``dfmc/conversion/convert.dylan`` that the presence
  of ``dfm-context-id`` is a hack until true source locations are available.
  Should we remove ``context-id`` and the supporting code? (On a related note,
  does that implementation of ``dfm-context-id`` even work?

Complete other parts of the implementation:

* Program condition filtering.
* Program restarts.
* Make :class:`<program-error>` distinct from a serious warning. This
  would also need a change to ``dfmc-debug-back-end`` and a specialization
  on :gf:`condition-classification`.
* Use more of the various subclasses of :class:`<program-note>` like the
  style, performance and portability notes. This requires getting the
  filtering to work.
* The implementation doesn't use limited collection types where it can.

The DFMC-CONDITIONS API Reference
*********************************

Definers for new Program Conditions
===================================

.. macro:: program-condition-definer

   :macrocall:

     .. code-block:: dylan

        define [modifier*] program-condition *class-name* (*superclasses*)
          *slot-spec*
          format-string *string*;
          format-arguments *slot*, ...;
          filter *filter*;
        end program-note;

   :parameter modifier: One or more class adjectives. *bnf*
   :parameter class-name: A valid Dylan class name. *bnf*
   :parameter superclasses: One or more Dylan class names to be used as the
     superclasses for the newly created program condition.
   :parameter slot-spec: A slot specification.
   :parameter format-string: A format string valid for use with :gf:`format`.
   :parameter format-arguments: One or more parameters which will be passed
     to :gf:`format` along with the *format-string*. The parameter values
     will be drawn from the corresponding slots.
   :parameter filter: A Dylan expression to be used as the value for
     :gf:`program-note-filter` on the new class. This should either be
     ``#f`` or an instance of :drm:`<function>` which returns a boolean
     value.

   :description:

     This is not typically used outside of the ``dfmc-conditions`` library.
     It is used for creating a new direct subclass of :class:`<program-condition>`.
     Most often, :macro:`program-note-definer` or a similar more specific
     definer macro would be used instead.

     Any additional slot specifications will be modified slightly:

     * The ``constant`` adjective will be removed if present.
     * The type constraint for the slot will be a type union with
       :class:`<ppml>`.

.. macro:: program-note-definer

   :macrocall:

     .. code-block:: dylan

        define [modifier*] program-note *class-name*
          *slot-spec*
          format-string *string*;
          format-arguments *slot*, ...;
          filter *filter*;
        end program-note;

        define [modifier*] program-note *class-name* (*superclasses*)
          *slot-spec*
          format-string *string*;
          format-arguments *slot*, ...;
          filter *filter*;
        end program-note;

   :description:

     Create a new :class:`<program-note>` subclass.

.. macro:: performance-note-definer

   :description:

     Create a new :class:`<performance-note>` subclass.
     See :macro:`program-note-definer` for details.

.. macro:: portability-note-definer

   :description:

     Create a new :class:`<portability-note>` subclass.
     See :macro:`program-note-definer` for details.

.. macro:: program-error-definer

   :description:

     Create a new :class:`<program-error>` subclass.
     See :macro:`program-note-definer` for details.

.. macro:: program-restart-definer

   :description:

     Create a new :class:`<program-restart>` subclass.
     See :macro:`program-note-definer` for details.

.. macro:: program-warning-definer

   :description:

     Create a new :class:`<program-warning>` subclass.
     See :macro:`program-note-definer` for details.

.. macro:: run-time-error-warning-definer

   :description:

     Create a new :class:`<run-time-error-warning>` subclass.
     See :macro:`program-note-definer` for details.

.. macro:: serious-program-warning-definer

   :description:

     Create a new :class:`<serious-program-warning>` subclass.
     See :macro:`program-note-definer` for details.

.. macro:: style-warning-definer

   :description:

     Create a new :class:`<style-warning-note>` subclass.
     See :macro:`program-note-definer` for details.

.. macro:: program-condition-definer-definer

   :description:

     This is not commonly used outside of ``dfmc-conditions``. It is
     creating new program-conditioner definer macros.

Program Conditions
==================

.. class:: <program-condition>
   :open:
   :abstract:

   :superclasses: :const:`<format-string-condition>`

   :keyword compilation-stage: Defaults to the value of :var:`*current-stage*`.
   :keyword program-note-creator: Defaults to the value of :var:`*current-dependent*`.
   :keyword source-location: Defaults to ``#f``. Every effort should be made to supply
     a valid value for this keyword.

   :description:

     The root of the hierarchy is ``<program-condition>``.  All
     errors, warnings, etc, about code in a program being compiled
     should be reported as instances of this class.

     This class should only be used for type declarations and as the
     superclass for mixin properties.  For instantiable classes, it's
     best to subclass one of :class:`<program-error>`,
     :class:`<program-note>`, or :class:`<program-restart>` instead.

.. type:: <program-notes>

   :supertype: :drm:`<sequence>`

.. class:: <program-note>
   :open:
   :abstract:
   :primary:

   :superclasses: :drm:`<warning>`, :class:`<program-condition>`

   :keyword context-id: An instance of :drm:`<string>`.
   :keyword subnotes: A sequence of subnotes, allowing hierarchical
     explanations to be constructed. See `Subnotes`_.

   :description:

     When a *context-id* has been supplied, this is used to give an
     indication of the logical context of the source that the note
     is about, typically to give a concise textual hint, allowing
     for example (where ``"process-foo"`` is the *context-id*::

       foo.dylan:180:Warning in process-foo: Bogus call to bar.

.. class:: <program-error>
   :open:
   :abstract:

   :superclasses: :class:`<program-note>`

   :description:

     A ``<program-error>`` is a language error.  Examples would be (most)
     syntax errors, inconsistent direct superclasses, or a reference to
     an undefined name.

.. class:: <program-restart>
   :open:
   :abstract:
   :primary:

   :superclasses: :class:`<program-condition>`, :drm:`<restart>`

   :keyword default:

   :description:

     A ``<program-restart>`` is a :drm:`<restart>` meant to be used as
     part of the recovery protocol for some :class:`<program-condition>`.

.. class:: <program-warning>
   :open:
   :abstract:

   :superclasses: :class:`<program-note>`

   :description:

     A <program-warning> is a note about something that might be a
     mistake in program, but the compiler is able to compile it without
     intervention.

.. class:: <run-time-error-warning>
   :open:
   :abstract:

   :superclasses: :class:`<program-warning>`

   :description:

     Run-time-error warnings are given when the compiler can prove that
     executing the code will lead definitely lead to a run-time error,
     whether or not that error is handled.  These warnings should be
     hard for the user to suppress.  It should be possible for a user to
     treat these warnings as errors;  that is, stop the compilation
     process because of one.

.. class:: <serious-program-warning>
   :open:
   :abstract:

   :superclasses: :class:`<program-warning>`


.. class:: <style-warning>
   :open:
   :abstract:

   :superclasses: :class:`<program-warning>`

   :description:

     Style warnings are given when the compiler detects code in a style
     that is legal (strictly speaking), but not desirable.  The display
     of style warnings can be inhibited globally, or on a class-by-class
     basis.

.. class:: <performance-note>
   :open:
   :abstract:

   :superclasses: :class:`<program-note>`

   :description:

     Performance notes are given when the compiler is prevented from
     doing an optimization that should be reasonable or expected in the
     current context.  Typical reasons would be that it has insufficient
     type, sealing, or program flow information.


.. class:: <portability-note>
   :open:
   :abstract:

   :superclasses: :class:`<program-note>`

   :description:

     Portability notes are given when the compiler detects something
     that is valid in the Open Dylan compiler, but is not part of
     portable Dylan or could have undefined effects in Dylan.

     It should be possible to turn these warnings into errors, to
     support a standards-conforming version of the compiler.

Program Condition Slots
=======================

.. generic-function:: condition-compilation-stage

   :signature: condition-compilation-stage (object) => (value)

   :parameter object: An instance of :class:`<program-condition>`.
   :value value: An instance of :drm:`<object>`.

.. generic-function:: condition-context-id

   :signature: condition-context-id (object) => (value)

   :parameter object: An instance of :class:`<program-note>`.
   :value value: An instance of :drm:`<object>`.

.. generic-function:: condition-program-note-creator

   :signature: condition-program-note-creator (object) => (value)

   :parameter object: An instance of :class:`<program-condition>`.
   :value value: An instance of :drm:`<object>`.

.. generic-function:: condition-source-location

   :signature: condition-source-location (object) => (value)

   :parameter object: An instance of :class:`<program-condition>`.
   :value value: An instance of :drm:`<object>`.

Signaling Program Conditions
============================

.. generic-function:: note
   :open:

   :signature: note (class #key #all-keys) => ()

   :parameter class: An instance of ``subclass(<program-condition>)``.

   :description:

     The primary program condition signaling interface is ``note``,
     which calls :drm:`make` on the condition class and signals it,
     possibly returning. It can be used for any program condition, but
     is mainly oriented towards :class:`<program-note>`.

   :example:

     .. code-block:: dylan

        note(<inaccessible-open-definition>,
             binding: form-variable-binding(form),
             source-location: form-source-location(form));

.. method:: note
   :specializer: subclass(<program-condition>)

.. macro:: maybe-note

.. generic-function:: raise
   :open:

   :signature: raise (class #key #all-keys) => ()

   :parameter class: An instance of ``subclass(<program-condition>)``.

   :description:

     This function is analogous to the standard Dylan :drm:`error`
     function and is guaranteed to not return.

.. method:: raise
   :specializer: subclass(<program-error>)

.. generic-function:: restart
   :open:

   :signature: restart (class #key #all-keys) => ()

   :parameter class: An instance of ``subclass(<program-restart>)``.

.. method:: restart
   :specializer: subclass(<program-restart>)

Preserving Program Conditions
=============================

Program conditions are tracked in each library. They are stored in
a table that is associated with each ``<library-description>``
via :gf:`library-conditions-table`. There are implementations of
another generic function, :gf:`remove-dependent-program-conditions`
which is commonly invoked during *retraction*. (What *retraction*
is for isn't clear to me at this point.)

.. generic-function:: add-program-condition

   :signature: add-program-condition (condition) => ()

   :parameter condition: An instance of :drm:`<condition>`.

   :description:

     Records a program condition. This does not usually need
     to be invoked directly outside of ``dfmc-conditions``
     where it is usually invoked during the filtering of a
     program condition.

.. method:: add-program-condition
   :specializer: <condition>

   :description:

     Runtime errors that are not :class:`<program-condition>` are
     not currently tracked. This method doesn't record them.

.. method:: add-program-condition
   :specializer: <program-condition>

   :description:

     Preserves a program condition by storing it in the
     :gf:`library-conditions-table` for the current
     library being compiled.

     .. note: If :var:`*subnotes-queue*` is not ``#f``, then
        the ``condition`` is added to :var:`*subnotes-queue*`
        instead of being tracked by the current library
        description.

        This happens in conjunction with the use of
        :macro:`accumulate-subnotes-during` and :macro:`note-during`.

.. generic-function:: library-conditions-table

   :signature: library-conditions-table (library) => (table)

   :parameter library: An instance of :drm:`<object>`.
   :value table: An instance of :class:`<table>`.

.. generic-function:: remove-program-conditions-from!

   :signature: remove-program-conditions-from! (table key stages) => ()

   :parameter table: An instance of :drm:`<object>`.
   :parameter key: An instance of :drm:`<object>`.
   :parameter stages: An instance of :drm:`<object>`.

Recovery and Restarting
=======================

.. macro:: condition-block

.. variable:: *error-recovery-model*

Subnotes
========

This is a very rarely used capability within the program condition
system and isn't currently well supported by the compiler output
to standard out and standard error.

Any :class:`<program-note>` can have additional notes attached to it.
These notes are useful for attaching extra data to a note, like possible
options or the sets of conflicting items.

An example usage of subnotes is:

.. code-block:: dylan

   note(<ambiguous-copy-down-method>,
        meth: m,
        other-methods: others,
        source-location: m.model-source-location,
        subnotes: map(method (m)
                        make(<ambiguous-copy-down-method-option>,
                             meth: m,
                             source-location: m.model-source-location)
                      end,
                      others));

.. note:: Subnotes are not displayed by the default printing of
   program conditions by the command line compiler. They can be
   found in the condition log file that is created during the
   build process. (``_build/build/foo/foo.log``)

.. generic-function:: subnotes

   :signature: subnotes (object) => (value)

   :parameter object: An instance of :class:`<program-note>`.
   :value value: An instance of :const:`<program-notes>`.

.. macro:: note-during

.. macro:: accumulate-subnotes-during

.. variable:: *subnotes-queue*
   :thread:

Printing Program Conditions
===========================

.. variable:: *detail-level*
   :thread:

   :type: :type:`<detail-level>`

   :description:

     .. note:: This is currently ignored.

.. type:: <detail-level>

   :equivalent: ``one-of(#"terse", #"normal", #"verbose")``

   :description:

     A simple, three-tiered approach to the amount of detail a
     condition presents.

     .. note:: This is currently ignored.

   :operations:

     :gf:`format-condition`

.. generic-function:: format-condition

   :signature: format-condition (stream condition detail-level) => ()

   :parameter stream: An instance of :class:`<stream>`.
   :parameter condition: An instance of :class:`<program-condition>`.
   :parameter detail-level: An instance of :const:`<detail-level>`.

   :description:

     This calls :gf:`format` to write to the ``stream``. The format
     string and arguments come from the condition's
     :drm:`condition-format-string` and :drm:`condition-format-arguments`
     respectively.

.. method:: print-object
   :specializer: <program-condition>, <stream>

   :signature: print-object (condition, stream) => ()

   :parameter condition: An instance of :class:`<program-condition>`.
   :parameter stream: An instance of :class:`<stream>`.

   :description:

     This calls :gf:`format-condition` with a *detail-level* of
     ``#"terse"``.

     This is provided for integrating program condition printing
     with the usual mechanisms for formatted output.

     .. note:: This is not actually called often at all as there
        is a more specific specialization on :class:`<program-note>`
        defined in ``dfmc-debug-back-end``.

Unclassified API
================

.. constant:: $record-program-note

.. function:: $signal-program-error

   :signature: $signal-program-error (c) => ()

   :parameter c: An instance of :drm:`<condition>`.

.. function:: $signal-program-note

   :signature: $signal-program-note (c) => ()

   :parameter c: An instance of :drm:`<condition>`.

.. class:: <ignore-serious-note>

   :superclasses: :class:`<program-restart>`

   :keyword format-string:
   :keyword note:

.. constant:: <program-note-filter>

.. generic-function:: convert-condition-slots-to-ppml

   :signature: convert-condition-slots-to-ppml (condition) => ()

   :parameter condition: An instance of :drm:`<condition>`.

   :description:

     Converts all slots on a condition to their PPML representation. This
     is typically autogenerated by the various program condition definer
     macros. It is called from :gf:`add-program-condition`.

.. method:: convert-condition-slots-to-ppml
   :specializer: <condition>

.. method:: convert-condition-slots-to-ppml
   :specializer: type-union(<simple-condition>, <simple-error>, <simple-warning>)

.. method:: convert-condition-slots-to-ppml
   :specializer: <program-note>

.. method:: convert-condition-slots-to-ppml
   :specializer: <program-restart>

.. method:: convert-condition-slots-to-ppml
   :specializer: <program-warning>

.. method:: convert-condition-slots-to-ppml
   :specializer: <serious-program-warning>

.. method:: convert-condition-slots-to-ppml
   :specializer: <program-error>

.. method:: convert-condition-slots-to-ppml
   :specializer: <run-time-error-warning>

.. method:: convert-condition-slots-to-ppml
   :specializer: <style-warning>

.. method:: convert-condition-slots-to-ppml
   :specializer: <performance-note>

.. method:: convert-condition-slots-to-ppml
   :specializer: <portability-note>

.. method:: convert-condition-slots-to-ppml
   :specializer: <ignore-serious-note>

.. macro:: convert-slots-to-ppml

.. variable:: dfmc-continue
   :thread:

.. variable:: dfmc-restart
   :thread:

.. function:: do-with-program-conditions

   :signature: do-with-program-conditions (body) => (#rest results)

   :parameter body: An instance of :drm:`<object>`.
   :value #rest results: An instance of :drm:`<object>`.

.. generic-function:: interesting-note?

   :signature: interesting-note? (note) => (interesting?)

   :parameter note: An instance of :class:`<program-note>`.
   :value interesting?: An instance of :drm:`<boolean>`.

   :description:

     True if the note is interesting to the user, according to the
     yet-to-be-defined compiler policy object.  Uninteresting conditions
     are suppressed, either by not printing messages for them or not
     logging them at all.  Because all errors and restarts are *serious*,
     they are also interesting.

.. method:: interesting-note?
   :specializer: <program-note>

   :parameter note: An instance of :class:`<program-note>`.
   :value interesting?: Always returns ``#t``.

.. method:: interesting-note?
   :specializer: <performance-note>

   :parameter note: An instance of :class:`<performance-note>`.
   :value interesting?: Always returns ``#f``.

.. generic-function:: make-program-note-filter

   :signature: make-program-note-filter (#key file-name from to in class action) => (filter)

   :parameter #key file-name: An instance of :drm:`<string>`.
   :parameter #key from: An instance of :drm:`<integer>`.
   :parameter #key to: An instance of :drm:`<integer>`.
   :parameter #key in: An instance of :drm:`<string>`.
   :parameter #key class: An instance of ``subclass(<condition>)``.
   :parameter #key action: An instance of :drm:`<function>`.
   :value filter: An instance of :const:`<program-note-filter>`.

.. generic-function:: obsolete-condition?
   :open:

   :signature: obsolete-condition? (condition) => (obsolete?)

   :parameter condition: An instance of :class:`<program-condition>`.
   :value obsolete?: An instance of :drm:`<boolean>`.

.. method:: obsolete-condition?
   :specializer: <program-condition>

   :parameter condition: An instance of :class:`<program-condition>`.
   :value obsolete?: Always returns ``#f``.

   :description:

     .. note:: This is never used.

.. generic-function:: present-program-error

   :signature: present-program-error (condition) => ()

   :parameter condition: An instance of :drm:`<condition>`.

.. method:: present-program-error
   :specializer: <condition>

.. method:: present-program-error
   :specializer: <program-note>

.. generic-function:: present-program-note

   :signature: present-program-note (condition) => ()

   :parameter condition: An instance of :drm:`<condition>`.

.. method:: present-program-note
   :specializer: <condition>

.. method:: present-program-note
   :specializer: <program-note>

.. function:: program-note-class-=

   :signature: program-note-class-= (class) => (pred)

   :parameter class: An instance of ``subclass(<condition>)``.
   :value pred: An instance of :drm:`<function>`.

.. function:: program-note-file-name-=

   :signature: program-note-file-name-= (file-name) => (pred)

   :parameter file-name: An instance of :drm:`<string>`.
   :value pred: An instance of :drm:`<function>`.

.. generic-function:: program-note-filter
   :open:

   :signature: program-note-filter (class) => (filter)

   :parameter class: An instance of ``subclass(<condition>)``.
   :value filter: An instance of :const:`<program-note-filter>`.

.. method:: program-note-filter
   :specializer: subclass(<program-note>)

.. method:: program-note-filter
   :specializer: subclass(<condition>)

.. method:: program-note-filter
   :specializer: subclass(<program-warning>)

.. method:: program-note-filter
   :specializer: subclass(<serious-program-warning>)

.. method:: program-note-filter
   :specializer: subclass(<run-time-error-warning>)

.. method:: program-note-filter
   :specializer: subclass(<style-warning>)

.. method:: program-note-filter
   :specializer: subclass(<performance-note>)

.. method:: program-note-filter
   :specializer: subclass(<portability-note>)

.. generic-function:: program-note-filter-setter
   :open:

   :signature: program-note-filter-setter (filter class) => (filter)

   :parameter filter: An instance of :const:`<program-note-filter>`.
   :parameter class: An instance of ``subclass(<program-condition>)``.
   :value filter: An instance of :const:`<program-note-filter>`.

.. method:: program-note-filter-setter
   :specializer: <program-note-filter>, subclass(<program-condition>)

.. function:: program-note-in

   :signature: program-note-in (form) => (pred)

   :parameter form: An instance of :drm:`<string>`.
   :value pred: An instance of :drm:`<function>`.

.. function:: program-note-location-between

   :signature: program-note-location-between (from to) => (pred)

   :parameter from: An instance of :drm:`<integer>`.
   :parameter to: An instance of :drm:`<integer>`.
   :value pred: An instance of :drm:`<function>`.

.. generic-function:: report-condition
   :open:

   :signature: report-condition (condition) => ()

   :parameter condition: An instance of :drm:`<condition>`.

.. generic-function:: serious-note?

   :signature: serious-note? (note) => (serious?)

   :parameter note: An instance of :class:`<program-note>`.
   :value serious?: An instance of :drm:`<boolean>`.

   :description:

     True if this note is serious -- that is, requires terminating the
     current processing and picking a restart.  The default behavior
     is that notes are not serious, but the policy object should allow
     upgrading them, with options like *"all warnings are errors"* for
     making :class:`<program-warning>` serious, or *"strict Dylan"* for
     making :class:`<portability-note>` serious.

     Errors are always serious, by definition, because the compiler
     can't just skip them.  Restarts are always serious, as much as such
     a definition make sense for them.

.. method:: serious-note?
   :specializer: <program-note>

   :parameter note: An instance of :class:`<program-note>`.
   :value serious?: Always returns ``#f``.

.. method:: serious-note?
   :specializer: <program-error>

   :parameter note: An instance of :class:`<program-error>`.
   :value serious?: Always returns ``#t``.

.. method:: serious-note?
   :specializer: <serious-program-warning>

   :parameter note: An instance of :class:`<serious-program-warning>`.
   :value serious?: Always returns ``#t``.

.. generic-function:: simple-note

   :signature: simple-note (class format-string #rest args) => ()

   :parameter class: An instance of ``subclass(<program-note>)``.
   :parameter format-string: An instance of :drm:`<string>`.
   :parameter #rest args: An instance of :drm:`<object>`.

.. generic-function:: simple-raise

   :signature: simple-raise (class format-string #rest args) => ()

   :parameter class: An instance of ``subclass(<program-error>)``.
   :parameter format-string: An instance of :drm:`<string>`.
   :parameter #rest args: An instance of :drm:`<object>`.

.. macro:: with-program-conditions

.. macro:: with-simple-abort-retry-restart

.. _print-condition.dylan: https://github.com/dylan-lang/opendylan/blob/master/sources/dfmc/debug-back-end/print-condition.dylan
