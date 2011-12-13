************************
The Common Dylan Library
************************

Introduction
============

The Common Dylan library contains the Common Extensions library and the
Dylan library. It provides a number of features that were either omitted
from the Dylan language described in the DRM, or that Open Dylan's
developers have found useful in a broad range of situations.

The Common Dylan library exports the following modules:

-  *common-extensions*
   Miscellaneous extensions to the Dylan language.
-  *harlequin-extensions*
   Provided for backward compatibility.
-  *simple-format* Simple formatting facilities. For more flexible
   formatting and printing, consider the separate Format and Print
   libraries.
-  *simple-random* A simple facility for generating pseudo-random
   integers.
-  *finalization* An object finalization interface.
-  *transcendentals*
   A set of open generic functions for ANSI C-like behavior over real
   numbers.
-  *machine-words*
   A set of functions for representing a limited range of integral
   values.

General language extensions
===========================

The *common-extensions* module contains a variety of useful basic
extensions to the Dylan language.

The Common Dylan extensions are:

-  Collection model extensions: `\<stretchy-sequence\>`_, `\<string-table\>`_,
   `difference`_, `fill-table!`_, `find-element`_, `position`_,
   `remove-all-keys!`_, and `define table`_.
-  Condition system extensions: `\<format-string-condition\>`_,
   `\<simple-condition\>`_, and `condition-to-string`_.
-  Program constructs: `iterate`_ and `when`_.
-  Application development conveniences: `iterate`_, `debug-message`_,
   `ignore`_, `ignorable`_, *profiling*, `timing`_, `$unsupplied`_,
   `unsupplied?`_, `unsupplied`_, `when`_, `$unfound`_, `one-of`_,
   `unfound?`_, and `found?`_.
-  Type conversion functions: `integer-to-string`_, `string-to-integer`_,
   and `float-to-string`_.

See `The COMMON-EXTENSIONS module`_ for reference descriptions of these items.

Simple formatting and printing
==============================

Common Dylan provides several libraries relevant to formatting and
printing strings, or otherwise using strings for output. These libraries
include Format, Format-out, Print, and Standard-IO. The facilities
provided by these libraries will be excess to many users’ requirements,
who may prefer to use the *simple-format* module that the
*common-extensions* library exports.

The `format-out`_ function converts its
arguments into a Dylan *format string* and then sends that string to the
standard output. The `format-to-string`_
function converts its arguments into a format string and then returns
that format string.

See `The SIMPLE-FORMAT module`_ for reference
descriptions of *format-out* and *format-string*.

Simple random number generation
===============================

Common Dylan provides a simple facility for generating sequences of
pseudo-random integers via the *simple-random* module exported from the
*common-extensions* library.

Instances of the sealed class `\<random\>`_
generate pseudo-random integers. Given an instance of *<random>*, the
function `random`_ will return a
pseudo-random integer. See `The SIMPLE-RANDOM
module`_ for reference descriptions of *random*
and *<random>*.

Finalization
============

Common Dylan provides a finalization interface in the *finalization*
module of *common-extensions*. This section explains finalization, the
finalization interface provided, and how to use the interface in
applications. See `The FINALIZATION module`_
for reference descriptions of the interface.

What is finalization?
---------------------

The `Memory Management Reference <http://www.memorymanagement.org>`_ defines
finalization as follows:

    In garbage-collected languages, it is often necessary to perform actions
    on some objects after they are no longer in use and before their memory
    can be recycled. These actions are known as finalization or termination.

    A common use of finalization is to release a resource when the
    corresponding “proxy” object dies. For example, an open file might be
    represented by a stream object. When the stream object has no references
    and can be collected, it is certain that the file is no longer in use by
    the [application] and can be closed.

Finalization is also commonly required when interfacing Dylan code with
foreign code that does not have automatic memory management. If an
interface involves a Dylan object that references a foreign object, it
may be necessary to free the memory resources of the foreign object when
the Dylan object is reclaimed.

How the finalization interface works
------------------------------------

The following sections give a broad overview of how finalization works
and how to use the interface.

Registering objects for finalization
------------------------------------

Finalization works through cooperation with the garbage collector.
Objects that are no longer referenced by the application that created
them will eventually be discovered by Dylan’s garbage collector and are
then available to be reclaimed.

By default, the garbage collector reclaims such objects without
notifying your application. If it is necessary to finalize an object
before it is reclaimed, your application must inform the garbage
collector.

The garbage collector maintains a register of objects requiring
finalization before being reclaimed. To add an object to the register,
call the function `finalize-when-unreachable`_ on the object.
Objects on the register are said to be *finalizable*.

If the garbage collector discovers that a finalizable object is no
longer referenced by the application, it does not reclaim it
immediately. Instead, it takes the object off its finalization register,
and adds it to the *finalization queue*.

The finalization queue contains all the objects awaiting finalization.
The garbage collector will not reclaim the objects until they have been
finalized.

Draining the finalization queue
-------------------------------

Objects in the finalization queue wait there until the application
drains it by calling the function `drain-finalization-queue`_. This function
finalizes every object in the queue.

The finalization queue is not normally drained automatically. See
`How can my application drain the finalization queue
automatically?`_ for details of how you can set
up a thread to do so.

.. note:: The order in which objects in the finalization queue are
   finalized is not defined. Applications should not make any assumptions
   about finalization ordering.

Finalizers
----------

The `drain-finalization-queue`_ function
finalizes each object in the finalization queue by calling the generic
function `finalize`_ on it. You should define
methods for `finalize`_ on those classes
whose instances may require finalization. These methods are called
*finalizers*.

The recommended interface to finalization is through
`finalize-when-unreachable`_ and `drain-finalization-queue`_, but
calling `finalize`_ on an object directly is also
permitted. If you are certain you are finished with an object, it may be
desirable to do so. For example, you might want to finalize an object
created in a local binding before it goes out of scope.

.. note:: Finalizable objects are only removed from the register if the
   garbage collector discovers that they are unreachable and moves them
   into the finalization queue. Calling *finalize* on an object directly
   does not affect its registration status.

The `drain-finalization-queue`_ function
makes each call to `finalize`_ inside
whatever dynamic handler environment is present when
*drain-finalization-queue* is called. If the call to
*drain-finalization-queue* is aborted via a non-local exit during a call
to *finalize*, the finalization queue retains all the objects that had
been added to it but which had not been passed to *finalize*.

There is a default method for `finalize`_ on
*<object>*. The method does nothing. It is available so that it is safe
for all finalizers to call *next-method*, a practice that we strongly
encourage. See `Writing finalizers`_.

After finalization
------------------

Once an object in the finalization queue has been finalized, it
typically becomes available for reclamation by the garbage collector.
Because it has been taken off the garbage collector’s finalization
register, it will not be queued up for finalization again.

.. note:: There are exceptions to this rule; see `The effects of
   multiple registrations`_ and `The effects of
   resurrecting objects`_.

Upon application exit
---------------------

There are no guarantees that objects which are registered for
finalization will actually be finalized before the application exits.
This is not a problem on many operating systems, which free any
resources held by a process when it exits.

Where it is necessary to guarantee an action at the time the application
exits, you should use a more explicit mechanism.

The effects of multiple registrations
-------------------------------------

Sometimes objects are registered for finalization more than once. The
effects of multiple registration are defined as follows:

Calling `finalize-when-unreachable`_ on an
object *n* times causes that object to be added to the finalization
queue up to *n* times, where *n* is greater than or equal to zero. There
is no guarantee that the object will be added exactly *n* times.

Note that this definition so general that it does not guarantee that any
object will ever be added to be finalization queue. In practice, Common
Dylan’s implementation guarantees that an object is added to the queue
at least once whenever an object has ben determined to be unreachable by
the garbage collector.

To remain robust under multiple registration, finalizers should be
idempotent: that is, the effect of multiple *finalize* calls on an
object should is the same as the effect of a single call.

The effects of resurrecting objects
-----------------------------------

If a finalizer makes an object reachable again, by storing a reference
to the object in a variable, slot, or collection, we say it has
*resurrected* it. An object may also be resurrected if it becomes
reachable again when some other object is resurrected (because it is
directly or indirectly referenced by that other object).

Resurrecting objects has pitfalls, and must be done with great care.
Since finalizers typically destructively modify objects when freeing
their resources, it is common for finalization to render objects
unusable. We do not recommend resurrection if there is any possibility
of the object being left in an unusable state, or if the object
references any other objects whose transitive closure might include an
object left in such a state by another call to *finalize*.

If you do resurrect objects, note that they will not be finalized again
unless you re-register them.

The effects of finalizing objects directly
------------------------------------------

Any object that has been finalized directly, through the application
itself calling *finalize* on it, may not yet be unreachable. Like any
normal object it only becomes eligible for reclamation when it is
unreachable. If such an object was also registered for finalization
using *finalize-when-unreachable*, it can end up being finalized again
via the queue mechanism.

Finalization and weak tables
----------------------------

If an object is both registered for finalization and is weakly referred
to from a weak table, finalization occurs *first*, with weak references
being removed afterwards. That is, reachability is defined in terms of
strong references only, as far as finalization is concerned. Weak
references die only when an object’s storage is finally reclaimed.

For more on weak tables, see :ref:`Weak tables <weak-tables>`.

Writing finalizers
------------------

Because the default `finalize`_ method, on
*<object>*, does nothing, you must define your own
`finalize`_ methods to get results from the
finalization interface. This section contains useful information about
writing finalizers.

Class-based finalization
------------------------

If your application defines a class for which all instances require
finalization, call `finalize-when-unreachable`_ in its *initialize*
method.

Parallels with INITIALIZE methods
---------------------------------

The default method on *<object>* is provided to make it safe to call
*next-method* in all finalizers. This situation is parallel to that for
class *initialize* methods, which call *next-method* before performing
their own initializations. By doing so, *initialize* methods guarantee
that the most specific initializations occur last.

By contrast, finalizers should call *next-method* last, in case they
depend on the superclass finalizer not being run.

Simplicity and robustness
-------------------------

Write finalizers that are simple and robust. They might be called in any
context, including within other threads; with careful design, your
finalizers will work in most or all possible situations.

A finalizer might be called on the same object more than once. This
could occur if the object was registered for finalization more than
once, or if your application registered the object for finalization and
also called *finalize* on it directly. To account for this, write
finalizers that are idempotent: that is, the effect of multiple calls is
the same as the effect of a single call. See `The effects of
multiple registrations`_ for more on the effects
of multiple registrations.

Remember that the order in which the finalization queue is processed is
not defined. Finalizers cannot make assumptions about ordering.

This is particularly important to note when writing finalizers for
classes that are typically used to form circular or otherwise
interestingly connected graphs of objects. If guarantees about
finalization in graphs of objects are important, we suggest registering
a root object for finalization and making its finalizer traverse the
graph (in some graph-specific well-ordered fashion) and call the
*finalize* method for each object in the graph requiring finalization.

Singleton finalizers
--------------------

Do not write singleton methods on `finalize`_. The singleton method
itself would refer to the object, and hence prevent it from becoming
unreachable.

Using finalization in applications
----------------------------------

This section answers questions about using finalization in an
application.

How can my application drain the finalization queue automatically?
------------------------------------------------------------------

If you would prefer the queue to be drained asynchronously, use the
automatic finalization interface. For more details, see
`automatic-finalization-enabled?`_ and
`automatic-finalization-enabled?-setter`_.

Libraries that do not wish to depend on automatic finalization should
not use those functions. They should call
`drain-finalization-queue`_ synchronously at
useful times, such as whenever they call *finalize-when-unreachable*.

Libraries that are not written to depend on automatic finalization
should always behave correctly if they are used in an application that
does use it.

When should my application drain the finalization queue?
--------------------------------------------------------

If you do not use automatic finalization, drain the queue synchronously
at useful points in your application, such as whenever you call
`finalize-when-unreachable`_ on an object.

The COMMON-EXTENSIONS module
============================

This section contains a reference entry for each item exported from the
Common Extensions library’s *common-extensions* module.

assert
------

Statement macro
'''''''''''''''

Summary
       

Signals an error if the expression passed to it evaluates to false.

Macro call (1)
              

assert *expression* *format-string* [*format-arg* ]\* => *false*
                                                                

Macro call (2)
              

assert *expression* => *false*
                              

Arguments
         

*expression* A Dylan expression*bnf*.
                                      

*format-string* A Dylan expression*bnf*.
                                         

*format-arg* A Dylan expression*bnf*.
                                      

Values
      

*false* *#f*.
              

Description
           

Signals an error if *expression* evaluates to *#f*.

An assertion or “assert” is a simple tool for testing that conditions
hold in program code.

The *format-string* is a format string as defined on page 112 of the
DRM. If *format-string* is supplied, the error is formatted accordingly,
along with any instances of *format-arg*.

If *expression* is not *#f*, *assert* does not evaluate *format-string*
or any instances of *format-arg*.

See also
        

`debug-assert`_

<byte-character>
----------------

Sealed class
''''''''''''

Summary
       

The class of 8-bit characters that instances of *<byte-string>* can
contain.

Superclasses
            

<character>
           

Init-keywords
             

None.
     

Description
           

The class of 8-bit characters that instances of *<byte-string>* can
contain.

concatenate!
------------

Open generic function
'''''''''''''''''''''

Summary
       

A destructive version of the Dylan language’s *concatenate* ; that is,
one that might modify its first argument.

Signature
         

concatenate! *sequence* #rest *more-sequences* => *result-sequence*
                                                                   

Arguments
         

*sequence* An instance of *<sequence>*.
                                        

*more-sequences*
                

Instances of *<sequence>*.
                           

Values
      

*result-sequence* An instance of *<sequence>*.
                                               

Description
           

A destructive version of the Dylan language’s *concatenate* ; that is,
one that might modify its first argument.

It returns the concatenation of one or more sequences, in a sequence
that may or may not be freshly allocated. If *result-sequence* is
freshly allocated, then, as for *concatenate*, it is of the type
returned by *type-for-copy* of *sequence*.

Example
       

> define variable \*x\* = "great-";
                                   

"great-"
        

> define variable \*y\* = "abs";

"abs"
     

> concatenate! (\*x\*, \*y\*);
                              

"great-abs"
           

> \*x\*;
        

"great-abs"

>
 

condition-to-string
-------------------

Open generic function
'''''''''''''''''''''

Summary
       

Returns a string representation of a condition object.

Signature
         

condition-to-string *condition* => *string*
                                           

Arguments
         

*condition* An instance of *<condition>*.
                                          

Values
      

*string* An instance of *<string>*.
                                    

Description
           

Returns a string representation of a general instance of *<condition>*.
There is a method on `<format-string-condition\>`_ and method on
*<type-error>*.

debug-assert
------------

Statement macro
'''''''''''''''

Summary
       

Signals an error if the expression passed to it evaluates to false — but
only when the code is compiled in interactive development mode.

Macro call (1)
              

debug-assert *expression* *format-string* [ *format-arg* ]\* => *false*
                                                                       

Macro call (2)
              

debug-assert *expression* => *false*
                                    

Arguments
         

*expression* A Dylan expression*bnf*.
                                      

*format-string* A Dylan expression*bnf*.
                                         

*format-arg* A Dylan expression*bnf*.
                                      

Values
      

*false* *#f*.
              

Description
           

Signals an error if *expression* evaluates to false — but only when the
code is compiled in debugging mode.

An assertion or “assert” is a simple and popular development tool for
testing conditions in program code.

This macro is identical to *assert*, except that the assert is defined
to take place only while debugging.

The Open Dylan compiler removes debug-assertions when it compiles code in
“production” mode as opposed to “debugging” mode.

The *format-string* is a format string as defined on page 112 of the
DRM.

debug-message
-------------

Function
''''''''

Summary
       

Formats a string and outputs it to the debugger.

Signature
         

debug-message *format-string* #rest *format-args* => ()
                                                       

Arguments
         

*format-string* An instance of *<string>*.
                                           

*format-args* Instances of *<object>*.
                                       

Values
      

None.
     

Description
           

Formats a string and outputs it to the debugger.

The *format-string* is a format string as defined on page 112 of the
DRM.

default-handler
---------------

G.f. method
'''''''''''

Summary
       

Prints the message of a warning instance to the Open Dylan debugger
window’s messages pane.

Syntax
      

default-handler *warning* => *false*
                                    

Arguments
         

*warning* An instance of *<warning>*.
                                      

Values
      

*false* *#f*.
              

Description
           

Prints the message of a warning instance to the Open Dylan debugger
window’s messages pane. It uses `debug-message`_, to do so.

This method is a required, predefined method in the Dylan language,
described on page 361 of the DRM as printing the warning’s message in an
implementation-defined way. We document this method here because our
implementation of it uses the function `debug-message`_, which is defined
in the Harlequin-Extensions library. Thus to use this *default-handler* method
on *<warning>*, your library needs to use the Harlequin-Extensions
library or a library that uses it (such as Harlequin-Dylan), rather than
simply using the Dylan library.

Example
       

In the following code, the signalled messages appear in the Harlequin
Dylan debugger window.

define class <my-warning> (<warning>)
                                     

end class;
          

define method say-hello()
                         

format-out("hello there!\\n");

signal("help!");

signal(make(<my-warning>));

format-out("goodbye\\n");

end method say-hello;
                     

say-hello();
            

The following messages appear in the debugger messages pane:

Application Dylan message: Warning: help!
                                         

Application Dylan message: Warning: {<my-warning>}
                                                  

Where *{<my-warning>}* means an instance of *<my-warning>*.

See also
        

`debug-message`_.

*default-handler*, page 361 of the DRM.

default-last-handler
--------------------

Function
''''''''

Summary
       

Formats and outputs a Dylan condition using *format-out* and passes
control on to the next handler.

Syntax
      

default-last-handler *serious-condition* *next-handler* => ()
                                                             

Arguments
         

*serious-condition*
                   

A object of class *<serious-condition>*.
                                         

*next-handler* A function.
                          

Values
      

None.

Description
           

A handler utility function defined on objects of class
*<serious-condition>* that can be by bound dynamically around a
computation via *let* *handler* or installed globally via
*last-handler-definer*.

This function formats and outputs the Dylan condition
*serious-condition* using *format-out* from the Format-Out library, and
passes control on to the next handler.

This function is automatically installed as the last handler if your
library uses the Harlequin-Extensions library.

Example
       

The following form defines a dynamic handler around some body:

let handler <serious-condition> = default-last-handler;
                                                       

while the following form installs a globally visible last-handler:

define last-handler <serious-condition>
                                       

= default-last-handler;
                       

See also
        

`last-handler-definer`_

*win32-last-handler* in the *C FFI and Win32* library reference, under
library *win32-user* and module *win32-default-handler*.

define table
------------

Definition macro
''''''''''''''''

Summary
       

Defines a constant binding in the current module and initializes it to a
new table object.

Macro call
          

define table *name* [ :: *type* ] = { [ *key* => *element* ]\* }
                                                                

Arguments
         

*name* A Dylan name*bnf*.
                          

*type* A Dylan operand*bnf*. Default value: *<table>*.
                                                        

*key* A Dylan expression*bnf*.
                               

*element* A Dylan expression*bnf*.
                                   

Description
           

Defines a constant binding *name* in the current module, and initializes
it to a new table object, filled in with the keys and elements
specified.

If the argument *type* is supplied, the new table created is an instance
of that type. Therefore *type* must be *<table>* or a subclass thereof.
If *type* is not supplied, the new table created is an instance of a
concrete subclass of *<table>*.

Example
       

define table $colors :: <object-table>
                                      

= { #"red" => $red,

#"green" => $green,

#"blue" => $blue };
                   

difference
----------

Open generic function
'''''''''''''''''''''

Summary
       

Returns a sequence containing the elements of one sequence that are not
members of a second.

Signature
         

difference *sequence* *1* *sequence* *2* #key *test* =>
*result-sequence*
                                                                         

Arguments
         

*sequence* *1* An instance of *<sequence>*.
                                            

*sequence* *2* An instance of *<sequence>*.
                                            

*test* An instance of *<function>*. Default value: *\\==*.
                                                            

Values
      

*result-sequence* An instance of *<sequence>*.
                                               

Description
           

Returns a sequence containing the elements of *sequence* *1* that are
not members of *sequence* *2*. You can supply a membership test
function as *test*.

Example
       

> difference(#(1,2,3), #(2,3,4));
                                 

#(1)

>
 

false-or
--------

Function
''''''''

Summary
       

Returns a union type comprised of *singleton(#f)* and one or more types.

Signature
         

false-or *type* #rest *more-types* => *result-type*
                                                   

Arguments
         

*type* An instance of *<type>*.
                                

*more-types* Instances of *<type>*.
                                    

Values
      

*result-type* An instance of *<type>*.
                                       

Description
           

Returns a union type comprised of *singleton(#f)*, *type*, any other
types passed as *more-types*.

This function is useful for specifying slot types and function return
values.

The expression

false-or(*t* *1*, *t* *2*, ..)
                                

is type-equivalent to

type-union(singleton(#f), *t* *1*, *t* *2*, ..)
                                                 

fill-table!
-----------

Function
''''''''

Summary
       

Fills a table with the keys and elements supplied.

Signature
         

fill-table! *table* *keys-and-elements* => *table*
                                                  

Arguments
         

*table* An instance of *<table>*.
                                  

*keys-and-elements*
                   

An instance of *<sequence>*.
                             

Values
      

*table* An instance of *<table>*.
                                  

Description
           

Modifies table so that it contains the keys and elements supplied in the
sequence *keys-and-elements*.

This function interprets *keys-and-elements* as key-element pairs, that
is, it treats the first element as a table key, the second as the table
element corresponding to that key, and so on. The keys and elements
should be suitable for *table*.

Because *keys-and-elements* is treated as a sequence of paired
key-element values, it should contain an even number of elements; if it
contains an odd number of elements, *fill-table!* ignores the last
element (which would have been treated as a key).

find-element
------------

Open generic function
'''''''''''''''''''''

Summary
       

Returns an element from a collection such that the element satisfies a
predicate.

Signature
         

find-element *collection* *function* #key *skip* *failure* => *element*
                                                                       

Arguments
         

*collection* An instance of *<collection>*.
                                            

*predicate* An instance of *<function>*.
                                         

*skip* An instance of *<integer>*. Default value: 0.
                                                     

*failure* An instance of *<object>*. Default value: *#f*.
                                                           

Values
      

*element* An instance of *<object>*.
                                     

Description
           

Returns a collection element that satisfies *predicate*.

This function is identical to Dylan’s *find-key*, but it returns the
element that satisfies *predicate* rather than the key that corresponds
to the element.

float-to-string
---------------

Function
''''''''

Summary
       

Formats a floating-point number to a string.

Signature
         

float-to-string *float* => *string*
                                   

Arguments
         

*float* An instance of *<float>*.
                                  

Values
      

*string* An instance of *<string>*.
                                    

Description
           

Formats a floating-point number to a string. It uses scientific notation
where necessary.

<format-string-condition>
-------------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of conditions that take a format string.

Superclasses
            

<condition>
           

Init-keywords
             

None.
     

Description
           

The class of conditions that take a format string, as defined by the
DRM.

It is the superclass of Dylan’s *<simple-condition>*.

See also
        

The Format library.

found?
------

Function
''''''''

Summary
       

Returns true if *object* is not equal to `$unfound`_, and false otherwise.

Signature
         

found? *object* => *boolean*
                            

Arguments
         

*object* An instance of *<object>*.
                                    

Values
      

*boolean* An instance of *<boolean>*.
                                      

Description
           

Returns true if *object* is not equal to `$unfound`_, and false otherwise.

It uses *\\=* as the equivalence predicate.

ignore
------

Function
''''''''

Summary
       

A compiler directive that tells the compiler it must not issue a warning
if its argument is bound but not referenced.

Signature
         

ignore *variable* => ()
                       

Arguments
         

*variable* A Dylan variable-name*bnf*.
                                       

Values
      

None.
     

Description
           

When the compiler encounters a variable that is bound but not
referenced, it normally issues a warning. The *ignore* function is a
compiler directive that tells the compiler it *must not* issue this
warning if *variable* is bound but not referenced. The *ignore* function
has no run-time cost.

The *ignore* function is useful for ignoring arguments passed to, or
values returned by, a function, method, or macro. The function has the
same extent as a *let* ; that is, it applies to the smallest enclosing
implicit body.

Use *ignore* if you never intend to reference *variable* within the
extent of the *ignore*. The compiler will issue a warning to tell you
if your program violates the *ignore*. If you are not concerned about
the *ignore* being violated, and do not wish to be warned if violation
occurs, use `ignorable`_ instead.

Example
       

This function ignores some of its arguments:

define method foo (x ::<integer>, #rest args)
                                             

ignore(args);

…

end
   

Here, we use *ignore* to ignore one of the values returned by *fn* :

let (x,y,z) = fn();
                   

ignore(y);
          

See also
        

`ignorable`_

ignorable
---------

Function
''''''''

Summary
       

A compiler directive that tells the compiler it *need not* issue a
warning if its argument is bound but not referenced.

Signature
         

ignorable *variable* => ()
                          

Arguments
         

*variable* A Dylan variable-name*bnf*.
                                       

Values
      

None.

Description
           

When the compiler encounters a variable that is bound but not
referenced, it normally issues a warning. The *ignorable* function is a
compiler directive that tells the compiler it *need not* issue this
warning if *variable* is bound but not referenced. The *ignorable*
function has no run-time cost.

The *ignorable* function is useful for ignoring arguments passed to, or
values returned by, a function, method, or macro. The function has the
same extent as a *let* ; that is, it applies to the smallest enclosing
implicit body.

The *ignorable* function is similar to `ignore`_. However, unlike
`ignore`_, it does not issue a warning if you subsequently reference
*variable* within the extent of the *ignorable* declaration. You might
prefer *ignorable* to `ignore`_ if you are not concerned about such
violations and do not wish to be warned about them.

Example
       

This function ignores some of its arguments:

define method foo (x ::<integer>, #rest args)
                                             

ignorable(args);

…

end
   

Here, we use *ignorable* to ignore one of the values returned by *fn* :

let (x,y,z) = fn();
                   

ignorable(y);
             

See also
        

`ignore`_

integer-to-string
-----------------

Function
''''''''

Summary
       

Returns a string representation of an integer.

Signature
         

integer-to-string *integer* #key *base* *size* *fill* => *string*
                                                                 

Arguments
         

*integer* An instance of *<integer>*.
                                      

*base* An instance of *<integer>*. Default value: 10.
                                                      

*size* An instance of *<integer>* or *#f*. Default value: *#f*.
                                                                 

*fill* An instance of *<character>*. Default value: 0.
                                                       

Values
      

*string* An instance of *<byte-string>*.
                                         

Description
           

Returns a string representation of *integer* in the given *base*, which
must be between 2 and 36. The size of the string is right-aligned to
*size* if *size* is not *#f*, and it is filled with the *fill*
character. If the string is already larger than *size* then it is not
truncated.

iterate
-------

Statement macro
'''''''''''''''

Summary
       

Iterates over a body.

Macro call
          

iterate *name* ({*argument* [ = *init-value* ]}\*)
 [ *body* ]
 end [ iterate ]
                                                  

Arguments
         

*name* A Dylan variable-name*bnf*.
                                   

*argument* A Dylan variable-name*bnf*.
                                       

*init-value* A Dylan expression*bnf*.
                                      

*body* A Dylan body*bnf*.
                          

Values
      

Zero or more instances of *<object>*.
                                      

Description
           

Defines a function that can be used to iterate over a body. It is
similar to *for*, but allows you to control when iteration will occur.

It creates a function called *name* which will perform a single step of
the iteration at a time; *body* can call *name* whenever it wants to
iterate another step. The form evaluates by calling the new function
with the initial values specified.

last-handler-definer
--------------------

Definition macro
''''''''''''''''

Summary
       

Defines a “last-handler” to be used after any dynamic handlers and
before calling *default-handler*.

Definition
          

define last-handler (*condition*, #key *test*, *init-args*)
                                                              

= *handler* ;
             

define last-handler condition = handler;
                                        

define last-handler;
                    

Arguments
         

*condition* A Dylan expression*bnf*. The class of condition for which
the handler should be invoked.
                                                                                                     

*test* A Dylan expression*bnf*. A function of one argument called on
the condition to test applicability of the handler.
                                                                                                                         

*init-args* A Dylan expression*bnf*. A sequence of initialization
arguments used to make an instance of the handler’s condition class.
                                                                                                                                       

*handler* A Dylan expression*bnf*. A function of two arguments,
*condition* and *next-handler*, that is called on a condition which
matches the handler’s condition class and test function.
                                                                                                                                                                                              

Values
      

None.

Description
           

A last-handler is a global form of the dynamic handler introduced via
*let* *handler*, and is defined using an identical syntax. The last
handler is treated as a globally visible dynamic handler. During
signalling if a last-handler has been installed then it is the last
handler tested for applicability before *default-handler* is invoked. If
a last-handler has been installed then it is also the last handler
iterated over in a call to *do-handlers*.

The first two defining forms are equivalent to the two alternate forms
of let handler. If more than one of these first defining forms is
executed then the last one executed determines the installed handler.
The current last-handler can be uninstalled by using the degenerate
third case of the defining form, that has no condition description or
handler function.

The intention is that libraries will install last handlers to provide
basic runtime error handling, taking recovery actions such as quitting
the application, trying to abort the current application operation, or
entering a connected debugger.

Example
       

The following form defines a last-handler function called
*default-last-handler* that is invoked on conditions of class
*<serious-condition>* :

define last-handler <serious-condition>
                                       

= default-last-handler;
                       

See also
        

`one-of`_

*win32-last-handler* in the *C FFI and Win32* library reference, under
library *win32-user* and module *win32-default-handler*.

one-of
------

Function
''''''''

Summary
       

Returns a union type comprised of singletons formed from its arguments.

Signature
         

one-of *object* #rest *more-objects* => *type*
                                              

Arguments
         

*object* An instance of *<object>*.
                                    

*more-objects* Instances of *<object>*.
                                        

Values
      

*type* An instance of *<type>*.
                                

Description
           

Returns a union type comprised of *singleton(* *object* *)* and the
singletons of any other objects passed with *more-object*.

one-of(*x*, *y*, *z*)
                        

Is a type expression that is equivalent to

type-union(singleton(*x*), singleton(*y*), singleton(*z*))
                                                             

position
--------

Open generic function
'''''''''''''''''''''

Summary
       

Returns the key at which a particular value occurs in a sequence.

Signature
         

position *sequence* *value* #key *predicate* *skip* => *key*
                                                            

Arguments
         

*sequence* An instance of *<sequence>*.
                                        

*value* An instance of *<object>*.
                                   

*predicate* An instance of *<function>*. Default value: *\\==*.
                                                                 

*skip* An instance of *<integer>*. Default value: 0.
                                                     

Values
      

*key* An instance of *<object>*.
                                 

Description
           

Returns the key at which *value* occurs in *sequence*.

If *predicate* is supplied, *position* uses it as an equivalence
predicate for comparing *sequence* ’s elements to *value*. It should
take two objects and return a boolean. The default predicate used is
*\\==*.

The *skip* argument is interpreted as it is by Dylan’s *find-key*
function: *position* ignores the first *skip* elements that match
*value*, and if *skip* or fewer elements satisfy *predicate*, it
returns *#f*.

remove-all-keys!
----------------

Open generic function
'''''''''''''''''''''

Summary
       

Removes all keys in a mutable collection, leaving it empty.

Signature
         

remove-all-keys! *mutable-collection* => ()
                                           

Arguments
         

*mutable-collection*
                    

An instance of *<mutable-collection>*.
                                       

Values
      

None.
     

Description
           

Modifies *mutable-collection* by removing all its keys and leaving it
empty. There is a predefined method on *<table>*.

<simple-condition>
------------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of simple conditions.

Superclasses
            

<format-string-condition>
                         

Init-keywords
             

None.
     

Description
           

The class of simple conditions. It is the superclass of *<simple-error>*,
*<simple-warning>*, and *<simple-restart>*.

Operations
          

*condition-format-string*

*condition-format-args*

Example
       

<stretchy-sequence>
-------------------

Open abstract class
'''''''''''''''''''

Summary
       

The class of stretchy sequences.

Superclasses
            

<sequence> <stretchy-collection>
                                

Init-keywords
             

None.
     

Description
           

The class of stretchy sequences.

<string-table>
--------------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of tables that use strings for keys.

Superclasses
            

<table>
       

Init-keywords
             

None.
     

Description
           

The class of tables that use instances of *<string>* for their keys. It
is an error to use a key that is not an instance of *<string>*.

Keys are compared with the equivalence predicate *\\=*.

The elements of the table are instances of *<object>*.

It is an error to modify a key once it has been used to add an element
to a *<string-table>*. The effects of modification are not defined.

.. note:: This class is also exported from the *table-extensions* module
   of the *table-extensions* library.

string-to-integer
-----------------

Function
''''''''

Summary
       

Returns the integer represented by its string argument, or by a
substring of that argument, in a number base between 2 and 36.

Signature
         

string-to-integer *string* #key *base* *start* *end* *default* =>
*integer* *next-key*
                                                                                      

Arguments
         

*string* An instance of *<byte-string>*.
                                         

*base* An instance of *<integer>*. Default value: 10.
                                                      

*start* An instance of *<integer>*. Default value: 0.
                                                      

*end* An instance of *<integer>*. Default value: *sizeof(* *string* *)*
.
                                                                          

*default* An instance of *<integer>*. Default value: *$unsupplied*.
                                                                     

Values
      

*integer* An instance of *<integer>*.
                                      

*next-key* An instance of *<integer>*.
                                       

Description
           

Returns the integer represented by the characters of *string* in the
number base *base*, where *base* is between 2 and 36. You can constrain
the search to a substring of *string* by giving values for *start* and
*end*.

This function returns the next key beyond the last character it
examines.

If there is no integer contained in the specified region of the string,
this function returns *default*, if specified. If you do not give a
value for *default*, this function signals an error.

This function is similar to C’s *strtod* function.

subclass
--------

Function
''''''''

Summary
       

Returns a type representing a class and its subclasses.

Signature
         

subclass *class* => *subclass-type*
                                   

Arguments
         

*class* An instance of *<class>*.
                                  

Values
      

*subclass-type* An instance of *<type>*.
                                         

Description
           

Returns a type that describes all the objects representing subclasses of
the given class. We term such a type a *subclass type*.

The *subclass* function is allowed to return an existing type if that
type is type equivalent to the subclass type requested.

Without *subclass*, methods on generic functions (such as Dylan’s
standard *make* and *as*) that take types as arguments are impossible
to reuse without resorting to ad hoc techniques. In the language defined
by the DRM, the only mechanism available for specializing such methods
is to use singleton types. A singleton type specializer used in this
way, by definition, gives a method applicable to exactly one type. In
particular, such methods are not applicable to subtypes of the type in
question. In order to define reusable methods on generic functions like
this, we need a type which allows us to express applicability to a type
and all its subtypes.

For an object *O* and class *Y*, the following *instance?* relationship
applies:

INSTANCE-1: instance?(*O*, subclass(*Y*))
                                           

True if and only if *O* is a class and *O* is a subclass of *Y*.

For classes *X* and *Y* the following *subtype?* relationships hold
(note that a rule applies only when no preceding rule matches):

SUBTYPE-1: subtype?(subclass(*X*), subclass(*Y*))
                                                   

True if and only if *X* is a subclass of *Y*.

SUBTYPE-2: subtype?(singleton(*X*), subclass(*Y*))
                                                    

True if and only if *X* is a class and *X* is a subclass of *Y*.

SUBTYPE-3: subtype?(subclass(*X*), singleton(*Y*))
                                                    

Always false.

SUBTYPE-4: subtype?(subclass(*X*), *Y*)
                                         

where *Y* is not a subclass type. True if *Y* is *<class>* or any proper
superclass of *<class>* (including *<object>*, any
implementation-defined supertypes, and unions involving any of these).
There may be other implementation-defined combinations of types *X* and
*Y* for which this is also true.

SUBTYPE-5: subtype?(*X*, subclass(*Y*))
                                         

where *X* is not a subclass type. True if *Y* is *<object>* or any
proper supertype of *<object>* and *X* is a subclass of *<class>*.

Note that by subclass relationships *SUBTYPE-4* and *SUBTYPE-5*, we get
this correspondence: *<class>* and *subclass(<object>)* are type
equivalent.

Where the *subtype?* test has not been sufficient to determine an
ordering for a method’s argument position, the following further
method-ordering rules apply to cases involving subclass types (note that
a rule applies only when no preceding rule matches):

*SPECIFICITY+1*. *subclass(* *X* *)* precedes *subclass(* *Y* *)* when
the argument is a class *C* and *X* precedes *Y* in the class precedence
list of *C*.

*SPECIFICITY+2*. *subclass(* *X* *)* always precedes *Y*, *Y* not a
subclass type. That is, applicable subclass types precede any other
applicable class-describing specializer.

The constraints implied by sealing come by direct application of sealing
rules 1–3 (see page 136of the DRM) and the following disjointness
criteria for subclass types (note that a rule applies only when no
preceding rule matches):

*DISJOINTNESS+1*. A subclass type *subclass(* *X* *)* and a type *Y*
are disjoint if *Y* is disjoint from *<class>*, or if *Y* is a subclass
of *<class>* without instance classes that are also subclasses of *X*.

*DISJOINTNESS+2*. Two subclass types *subclass(* *X* *)* and
*subclass(* *Y* *)* are disjoint if the classes *X* and *Y* are
disjoint.

*DISJOINTNESS+3*. A subclass type *subclass(* *X* *)* and a singleton
type *singleton(* *O* *)* are disjoint unless *O* is a class and *O* is
a subclass of *X*.

The guiding principle behind the semantics is that, as far as possible,
methods on classes called with an instance should behave isomorphically
to corresponding methods on corresponding subclass types called with the
class of that instance. So, for example, given the heterarchy:

<object>
        

\|

<A>

/ \\

<B> <C>

\\ /

<D>
   

and methods:

method foo (<A>)
                

method foo (<B>)

method foo (<C>)

method foo (<D>)
                

method foo-using-type (subclass(<A>))
                                     

method foo-using-type (subclass(<B>))

method foo-using-type (subclass(<C>))

method foo-using-type (subclass(<D>))
                                     

that for a direct instance *D* *1* of *<D>* :

foo-using-type(<D>)
                   

should behave analogously to:

foo(D1)
       

with respect to method selection.

Example
       

define class <A> (<object>) end;
                                

define class <B> (<A>) end;

define class <C> (<A>) end;

define class <D> (<B>, <C>) end;
                                

define method make (class :: subclass(<A>), #key)
                                                 

print("Making an <A>");

next-method();

end method;
           

define method make (class :: subclass(<B>), #key)
                                                 

print("Making a <B>");

next-method();

end method;
           

define method make (class :: subclass(<C>), #key)
                                                 

print("Making a <C>");

next-method();

end method;
           

define method make (class :: subclass(<D>), #key)
                                                 

print("Making a <D>");

next-method();

end method;
           

? make(<D>);
            

Making a <D>

Making a <B>

Making a <C>

Making an <A>

{instance of <D>}
                 

supplied?
---------

Function
''''''''

Summary
       

Returns true if its argument is not equal to the unique “unsupplied”
value, `$unsupplied`_, and false if it is.

Signature
         

supplied? *object* => *supplied?*
                                 

Arguments
         

*object* An instance of *<object>*.
                                    

Values
      

*supplied?*
           

An instance of *<boolean>*.
                            

Description
           

Returns true if *object* is not equal to the unique “unsupplied” value,
`$unsupplied`_, and false if it is. It uses
*\\=* as the equivalence predicate.

See also
        

`$unsupplied`_

`unsupplied`_

`$unsupplied`_

timing
------

Statement macro
'''''''''''''''

Summary
       

Returns the time, in seconds and microseconds, spent executing the body
of code it is wrapped around.

Macro call
          

timing () [ *body* ] end [ timing ]
                                   

Arguments
         

*body* A Dylan body*bnf*
                        

Values
      

*seconds* An instance of *<integer>*.
                                      

*microseconds* An instance of *<integer>*.
                                           

Description
           

Returns the time, in seconds and microseconds, spent executing the body
of code it is wrapped around.

The first value returned is the number of whole seconds spent in *body*.
The second value returned is the number of microseconds spent in
*body* in addition to *seconds*.

Example
       

An example:

timing ()
         

for (i from 0 to 200)

format-to-string("%d %d", i, i + 1)

end

end;
    

=> 1 671000
           

$unfound
--------

Constant
''''''''

Summary
       

A unique value that can be used to indicate that a search operation
failed.

Type
    

<list>
      

Value
     

A unique value.

Description
           

A unique value that can be used to indicate that a search operation
failed.

See also
        

`found?`_

`unfound?`_

`unfound`_

unfound
-------

Function
''''''''

Summary
       

Returns the unique “unfound” value, `$unfound`_.

Signature
         

unfound () => *unfound-marker*
                              

Arguments
         

None.
     

Values
      

*unfound-marker* The value `$unfound`_.
                                                                  

Description
           

Returns the unique “unfound” value, `$unfound`_.

See also
        

`found?`_

`unfound?`_

`$unfound`_

unfound?
--------

Function
''''''''

Summary
       

Returns true if its argument is equal to the unique “unfound” value,
`$unfound`_, and false if it is not.

Signature
         

unfound? *object* => *unfound?*
                               

Arguments
         

*object* An instance of *<object>*.
                                    

Values
      

*unfound?* An instance of *<boolean>*.
                                       

Description
           

Returns true if *object* is equal to the unique “unfound” value,
`$unfound`_, and false if it is not. It uses *\\=*
as the equivalence predicate.

See also
        

`found?`_

`$unfound`_

`unfound`_

$unsupplied
-----------

Constant
''''''''

Summary
       

A unique value that can be used to indicate that a keyword was not
supplied.

Type
    

<list>
      

Value
     

A unique value.

Description
           

A unique value that can be used to indicate that a keyword was not
supplied.

See also
        

`supplied?`_

`unsupplied`_

`unsupplied?`_

unsupplied
----------

Function
''''''''

Summary
       

Returns the unique “unsupplied” value, `$unsupplied`_.

Signature
         

unsupplied () => *unsupplied-marker*
                                    

Arguments
         

None.
     

Values
      

*unsupplied-marker*
                   

The value `$unsupplied`_.
                                                    

Description
           

Returns the unique “unsupplied” value, `$unsupplied`_.

See also
        

`supplied?`_

`$unsupplied`_

`unsupplied?`_

unsupplied?
-----------

Function
''''''''

Summary
       

Returns true if its argument is equal to the unique “unsupplied” value,
`$unsupplied`_, and false if it is not.

Signature
         

unsupplied? *value* => *boolean*
                                

Arguments
         

*value* An instance of *<object>*.
                                   

Values
      

*boolean* An instance of *<boolean>*.
                                      

Description
           

Returns true if its argument is equal to the unique “unsupplied” value,
`$unsupplied`_, and false if it is not. It
uses *\\=* as the equivalence predicate.

See also
        

`supplied?`_

`$unsupplied`_

`unsupplied`_

when
----

Statement macro
'''''''''''''''

Summary
       

Executes an implicit body if a test expression is true, and does nothing
if the test is false.

Macro call
          

when (*test*) [ *consequent* ] end [ when ]
                                            

Arguments
         

*test* A Dylan expression*bnf*.
                                

*consequent* A Dylan body*bnf*.
                                

Values
      

Zero or more instances of *<object>*.
                                      

Description
           

Executes *consequent* if *test* is true, and does nothing if *test* is
false.

This macro behaves identically to Dylan’s standard *if* statement macro,
except that there is no alternative flow of execution when the test is
false.

Example
       

when (x < 0)
            

~ x;

end;
    

The SIMPLE-FORMAT module
========================

This section contains a reference entry for each item exported from the
Harlequin-extensions library’s *simple-format* module.

format-out
----------

Function
''''''''

Summary
       

Formats its arguments to the standard output.

Signature
         

format-out *format-string* #rest *format-arguments* => ()
                                                         

Arguments
         

*format-string* An instance of *<byte-string>*.
                                                

*format-arguments*
                  

Instances of *<object>*.
                         

Values
      

None.
     

Description
           

Formats its arguments to the standard output.

This function does not use the *\*standard-output\** stream defined by
the Standard-IO library.

format-to-string
----------------

Function
''''''''

Summary
       

Returns a formatted string constructed from its arguments.

Signature
         

format-to-string *format-string* #rest *format-arguments* => *string*
                                                                     

Arguments
         

*format-string* An instance of *<byte-string>*.
                                                

*format-arguments*
                  

Instances of *<object>*.
                         

Values
      

*result-string* An instance of *<byte-string>*.
                                                

Exceptions
          

This function signals an error if any of the format directives in
*format-string* are invalid.

Description
           

Returns a formatted string constructed from its arguments, which include
a *format-string* of formatting directives and a series of
*format-arguments* to be formatted according to those directives.

The *format-string* must be a Dylan format string as described on pages
112–114 of the DRM.

The SIMPLE-RANDOM module
========================

This section contains a reference entry for each item exported from the
Harlequin-extensions library’s *simple-random* module.

<random>
--------

Sealed instantiable class
'''''''''''''''''''''''''

Summary
       

The class of random number generators.

Superclasses
            

<object>
        

Init-keywords
             

*seed* An instance of *<integer>*. Default value: computed to be
random.
                                                                         

Description
           

The class of random number generators.

The seed value from which to start the sequence of integers. Default
value: computed to be random.

Example
       

random
------

Function
''''''''

Summary
       

Returns a pseudorandomly generated number greater than or equal to zero
and less than a specified value.

Signature
         

random *upperbound* #key *random* => *random-integer*
                                                     

Arguments
         

*range* An instance of *<integer>*.
                                    

*random* An instance of *<random>*.
                                    

Values
      

*random-integer*
                

An instance of *<integer>*.
                            

Description
           

Returns a pseudorandomly generated number greater than or equal to zero
and less than *range*.

The FINALIZATION module
=======================

This section contains a reference description for each item in the
finalization interface. These items are exported from the
*common-extensions* library in a module called *finalization*.

automatic-finalization-enabled?
-------------------------------

Function
''''''''

Summary
       

Returns true if automatic finalization is enabled, and false otherwise.

Signature
         

automatic-finalization-enabled? () => *enabled?*
                                                

Arguments
         

None.

Values
      

*enabled?* An instance of *<boolean>*. Default value: *#f*.
                                                             

Description
           

Returns true if automatic finalization is enabled, and false otherwise.

See also
        

`automatic-finalization-enabled?-setter`_

`drain-finalization-queue`_

`finalize-when-unreachable`_

`finalize`_

automatic-finalization-enabled?-setter
--------------------------------------

Function
''''''''

Summary
       

Sets the automatic finalization system state.

Signature
         

automatic-finalization-enabled?-setter *newval* => ()
                                                     

Arguments
         

*newval* An instance of *<boolean>*.
                                     

Values
      

None.

Description
           

Sets the automatic finalization system state to *newval*.

The initial state is *#f*. If the state changes from *#f* to *#t*, a
new thread is created which regularly calls `drain-finalization-queue`_
inside an empty dynamic environment (that is, no dynamic condition
handlers). If the state changes from *#t* to *#f*, the thread exits.

See also
        

`automatic-finalization-enabled?`_

`drain-finalization-queue`_

`finalize-when-unreachable`_

`finalize`_

drain-finalization-queue
------------------------

Function
''''''''

Summary
       

Calls `finalize`_ on every object in the
finalization queue.

Signature
         

drain-finalization-queue () => ()
                                 

Arguments
         

None.

Values
      

None.

Description
           

Calls `finalize`_ on each object that is
awaiting finalization.

Each call to `finalize`_ is made inside
whatever dynamic handler environment is present when
*drain-finalization-queue* is called. If the call to
*drain-finalization-queue* is aborted via a non-local exit during a call
to *finalize*, the finalization queue retains all the objects that had
been added to it but which had not been passed to *finalize*.

The order in which objects in the finalization queue will be finalized
is not defined. Applications should not make any assumptions about
finalization ordering.

See also
        

`finalize-when-unreachable`_

`finalize`_

`automatic-finalization-enabled?`_

`automatic-finalization-enabled?-setter`_

finalize-when-unreachable
-------------------------

Function
''''''''

Summary
       

Registers an object for finalization.

Signature
         

finalize-when-unreachable *object* => *object*
                                              

Arguments
         

*object* An instance of *<object>*.
                                    

Values
      

*object* An instance of *<object>*.
                                    

Description
           

Registers *object* for finalization. If *object* becomes unreachable, it
is added to the finalization queue rather than being immediately
reclaimed.

*Object* waits in the finalization queue until the application calls
`drain-finalization-queue`_, which processes each object in the queue
by calling the generic function `finalize`_ on it.

The function returns its argument.

See also
        

`finalize`_

`drain-finalization-queue`_

`automatic-finalization-enabled?`_

`automatic-finalization-enabled?-setter`_

finalize
--------

Open generic function
'''''''''''''''''''''

Summary
       

Finalizes an object.

Signature
         

finalize *object* => ()
                       

Arguments
         

*object* An instance of *<object>*.
                                    

Values
      

None.

Description
           

Finalizes *object*.

You can define methods on *finalize* to perform class-specific
finalization procedures. These methods are called *finalizers*.

A default `finalize`_ method on *<object>* is provided.

The main interface to finalization is the function `drain-finalization-queue`_,
which calls *finalize* on each object awaiting finalization. Objects join the
finalization queue if they become unreachable after being registered for
finalization with `finalize-when-unreachable`_. However, you can
call *finalize* directly if you wish.

Once finalized, *object* is available for reclamation by the garbage
collector, unless finalization made it reachable again. (This is called
*resurrection* ; see `The effects of resurrecting
objects`_.) Because the object has been taken off
the garbage collector’s finalization register, it will not be added to
the finalization queue again, unless it is resurrected. However, it
might still appear in the queue if it was registered more than once.

Do not write singleton methods on `finalize`_. A singleton method would itself
reference the object, and hence prevent it from becoming unreachable.

See also
        

`finalize`_.

`finalize-when-unreachable`_

`drain-finalization-queue`_

`automatic-finalization-enabled?`_

`automatic-finalization-enabled?-setter`_

finalize
--------

G.f. method
'''''''''''

Summary
       

Finalizes an object.

Signature
         

finalize *object* => ()
                       

Arguments
         

*object* An instance of *<object>*.
                                    

Values
      

None.
     

Description
           

This method is a default finalizer for all objects. It does nothing, and
is provided only to make *next-method* calls safe for all methods on
`finalize`_.

See also
        

`finalize-when-unreachable`_

`finalize`_

`drain-finalization-queue`_

`automatic-finalization-enabled?`_

`automatic-finalization-enabled?-setter`_


