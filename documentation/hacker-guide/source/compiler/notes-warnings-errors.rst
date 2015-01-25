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

* :class:`<program-condition>`

  * :class:`<program-error>`
  * :class:`<program-warning>`

    * :class:`<serious-program-warning>`
    * :class:`<run-time-error-warning>` *(Unused)*
    * :class:`<style-warning>` *(Unused)*

  * :class:`<program-note>`

    * :class:`<performance-note>`
    * :class:`<portability-note>` *(Unused)*

  * :class:`<program-restart>` *(Unused)*

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

PPML, Pretty Print Markup Language
**********************************

*To be written.*

Filtering of Program Conditions
*******************************

This is functionality that has not been completed and is
currently not entirely in use.

*To be written.*

How Warnings Are Displayed and Recorded
***************************************

*To be written.*

Subnotes
********

This is a very rarely used capability within the program condition
system and isn't currently well supported by the compiler output
to standard out and standard error.

*To be written.*

The DFMC-CONDITIONS API Reference
*********************************

Definers for new Program Conditions
===================================

.. macro:: program-condition-aux-definer

.. macro:: performance-note-definer

.. macro:: portability-note-definer

.. macro:: program-condition-definer

.. macro:: program-error-definer

.. macro:: program-condition-definer-definer

.. macro:: program-note-definer

.. macro:: program-restart-definer

.. macro:: program-warning-definer

.. macro:: run-time-error-warning-definer

.. macro:: serious-program-warning-definer

.. macro:: style-warning-definer

Program Conditions
==================

.. class:: <performance-note>
   :open:
   :abstract:

   :superclasses: :class:`<program-note>`


.. class:: <portability-note>
   :open:
   :abstract:

   :superclasses: :class:`<program-note>`


.. class:: <program-condition>
   :open:
   :abstract:

   :superclasses: :const:`<format-string-condition>`

   :keyword compilation-stage:
   :keyword program-note-creator:
   :keyword source-location:

.. class:: <program-error>
   :open:
   :abstract:

   :superclasses: :class:`<serious-program-warning>`

.. class:: <program-note>
   :open:
   :abstract:
   :primary:

   :superclasses: :drm:`<warning>`, :class:`<program-condition>`

   :keyword context-id:
   :keyword subnotes:

.. class:: <program-restart>
   :open:
   :abstract:
   :primary:

   :superclasses: :class:`<program-condition>`, :drm:`<restart>`

   :keyword default:

.. class:: <program-warning>
   :open:
   :abstract:

   :superclasses: :class:`<program-note>`


.. class:: <run-time-error-warning>
   :open:
   :abstract:

   :superclasses: :class:`<program-warning>`


.. class:: <serious-program-warning>
   :open:
   :abstract:

   :superclasses: :class:`<program-warning>`


.. class:: <style-warning>
   :open:
   :abstract:

   :superclasses: :class:`<program-warning>`

.. current-module:: dfmc-conditions


.. constant:: $record-program-note

.. function:: $signal-program-error

   :signature: $signal-program-error (c) => ()

   :parameter c: An instance of :class:`<condition>`.

.. function:: $signal-program-note

   :signature: $signal-program-note (c) => ()

   :parameter c: An instance of :class:`<condition>`.

.. variable:: *detail-level*

.. variable:: *error-recovery-model*

.. variable:: *subnotes-queue*

.. constant:: <detail-level>

.. class:: <ignore-serious-note>

   :superclasses: :class:`<program-restart>`

   :keyword format-string:
   :keyword note:

.. constant:: <program-note-filter>

.. macro:: accumulate-subnotes-during

.. generic-function:: add-program-condition

   :signature: add-program-condition (condition) => ()

   :parameter condition: An instance of :class:`<condition>`.

.. method:: add-program-condition
   :specializer: <condition>

.. method:: add-program-condition
   :specializer: <program-condition>

.. macro:: condition-block

.. macro:: condition-block-aux

.. generic-function:: condition-compilation-stage

   :signature: condition-compilation-stage (object) => (value)

   :parameter object: An instance of :class:`<program-condition>`.
   :value value: An instance of :class:`<object>`.

.. generic-function:: condition-context-id

   :signature: condition-context-id (object) => (value)

   :parameter object: An instance of :class:`<program-note>`.
   :value value: An instance of :class:`<object>`.

.. macro:: condition-make-filter-definer

.. macro:: condition-make-method-maybe-definer

.. generic-function:: condition-program-note-creator

   :signature: condition-program-note-creator (object) => (value)

   :parameter object: An instance of :class:`<program-condition>`.
   :value value: An instance of :class:`<object>`.

.. generic-function:: condition-source-location

   :signature: condition-source-location (object) => (value)

   :parameter object: An instance of :class:`<program-condition>`.
   :value value: An instance of :class:`<object>`.

.. generic-function:: convert-condition-slots-to-ppml

   :signature: convert-condition-slots-to-ppml (condition) => ()

   :parameter condition: An instance of :class:`<condition>`.

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

.. variable:: dfmc-restart

.. function:: do-with-program-conditions

   :signature: do-with-program-conditions (body) => (#rest results)

   :parameter body: An instance of :class:`<object>`.
   :value #rest results: An instance of :class:`<object>`.

.. generic-function:: format-condition

   :signature: format-condition (stream condition detail-level) => ()

   :parameter stream: An instance of :class:`<stream>`.
   :parameter condition: An instance of :class:`<program-condition>`.
   :parameter detail-level: An instance of :const:`<detail-level>`.

.. generic-function:: interesting-note?

   :signature: interesting-note? (note) => (interesting?)

   :parameter note: An instance of :class:`<program-note>`.
   :value interesting?: An instance of :class:`<boolean>`.

.. method:: interesting-note?
   :specializer: <program-note>

.. method:: interesting-note?
   :specializer: <performance-note>

.. generic-function:: library-conditions-table

   :signature: library-conditions-table (library) => (table)

   :parameter library: An instance of :class:`<object>`.
   :value table: An instance of :class:`<table>`.

.. generic-function:: make-program-note-filter

   :signature: make-program-note-filter (#key file-name from to in class action) => (filter)

   :parameter #key file-name: An instance of :class:`<string>`.
   :parameter #key from: An instance of :class:`<integer>`.
   :parameter #key to: An instance of :class:`<integer>`.
   :parameter #key in: An instance of :class:`<string>`.
   :parameter #key class: An instance of ``subclass(<condition>)``.
   :parameter #key action: An instance of :class:`<function>`.
   :value filter: An instance of :const:`<program-note-filter>`.

.. macro:: maybe-note

.. generic-function:: note
   :open:

   :signature: note (class #key #all-keys) => ()

   :parameter class: An instance of ``subclass(<program-condition>)``.

.. method:: note
   :specializer: subclass(<program-condition>)

.. macro:: note-during

.. generic-function:: obsolete-condition?
   :open:

   :signature: obsolete-condition? (condition) => (obsolete?)

   :parameter condition: An instance of :class:`<program-condition>`.
   :value obsolete?: An instance of :class:`<boolean>`.

.. method:: obsolete-condition?
   :specializer: <program-condition>

.. generic-function:: present-program-error

   :signature: present-program-error (condition) => ()

   :parameter condition: An instance of :class:`<condition>`.

.. method:: present-program-error
   :specializer: <condition>

.. method:: present-program-error
   :specializer: <program-note>

.. generic-function:: present-program-note

   :signature: present-program-note (condition) => ()

   :parameter condition: An instance of :class:`<condition>`.

.. method:: present-program-note
   :specializer: <condition>

.. method:: present-program-note
   :specializer: <program-note>

.. function:: program-note-class-=

   :signature: program-note-class-= (class) => (pred)

   :parameter class: An instance of ``subclass(<condition>)``.
   :value pred: An instance of :class:`<function>`.

.. function:: program-note-file-name-=

   :signature: program-note-file-name-= (file-name) => (pred)

   :parameter file-name: An instance of :class:`<string>`.
   :value pred: An instance of :class:`<function>`.

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

   :parameter form: An instance of :class:`<string>`.
   :value pred: An instance of :class:`<function>`.

.. function:: program-note-location-between

   :signature: program-note-location-between (from to) => (pred)

   :parameter from: An instance of :class:`<integer>`.
   :parameter to: An instance of :class:`<integer>`.
   :value pred: An instance of :class:`<function>`.

.. generic-function:: raise
   :open:

   :signature: raise (class #key #all-keys) => ()

   :parameter class: An instance of ``subclass(<program-condition>)``.

.. method:: raise
   :specializer: subclass(<program-error>)

.. generic-function:: remove-program-conditions-from!

   :signature: remove-program-conditions-from! (table key stages) => ()

   :parameter table: An instance of :class:`<object>`.
   :parameter key: An instance of :class:`<object>`.
   :parameter stages: An instance of :class:`<object>`.

.. generic-function:: report-condition
   :open:

   :signature: report-condition (condition) => ()

   :parameter condition: An instance of :class:`<condition>`.

.. generic-function:: restart
   :open:

   :signature: restart (class #key #all-keys) => ()

   :parameter class: An instance of ``subclass(<program-restart>)``.

.. method:: restart
   :specializer: subclass(<program-restart>)

.. generic-function:: serious-note?

   :signature: serious-note? (note) => (serious?)

   :parameter note: An instance of :class:`<program-note>`.
   :value serious?: An instance of :class:`<boolean>`.

.. method:: serious-note?
   :specializer: <program-note>

.. method:: serious-note?
   :specializer: <program-error>

.. method:: serious-note?
   :specializer: <serious-program-warning>

.. generic-function:: simple-note

   :signature: simple-note (class format-string #rest args) => ()

   :parameter class: An instance of ``subclass(<program-note>)``.
   :parameter format-string: An instance of :class:`<string>`.
   :parameter #rest args: An instance of :class:`<object>`.

.. generic-function:: simple-raise

   :signature: simple-raise (class format-string #rest args) => ()

   :parameter class: An instance of ``subclass(<program-error>)``.
   :parameter format-string: An instance of :class:`<string>`.
   :parameter #rest args: An instance of :class:`<object>`.

.. generic-function:: subnotes

   :signature: subnotes (object) => (value)

   :parameter object: An instance of :class:`<program-note>`.
   :value value: An instance of :const:`<program-notes>`.

.. macro:: with-program-conditions

.. macro:: with-simple-abort-retry-restart
