********
Integers
********

Introduction
============

This chapter describes the Common Dylan implementation of arithmetic
functions, especially integer arithmetic. It describes a number of
extensions to the Dylan language, which are available from the Dylan
library. It also describes a generic arithmetic facility that, through
the use of other libraries, allows you to extend arithmetic to special
number types, such as “big” (64-bit) integers.

Throughout this chapter, arguments are instances of the class specified
by the argument name (ignoring any numeric suffixes), unless otherwise
noted. Thus, the arguments *integer*, *integer1*, and *integer2* would
all be instances of the class *<integer>*.

The goals of the extensions to the Dylan language described in this
chapter are as follows:

Provide arithmetic operations that are closed over small integers.
                                                                  

This allows type inference to propagate small integer declarations more
widely, because there is no possibility of automatic coercion into some
more general format.

Make the arithmetic operations that are closed over small integers
easily accessible to programmers.
                                                                                                    

Allow the Dylan library to be described in such a way that only small
integers are present by default, moving support for infinite precision
integer arithmetic to the Big-Integers library, which must be explicitly
used.
                                                                                                                                                                                                                           

Support infinite precision integer arithmetic through the Big-Integers
library.
                                                                               

.. note:: Using that library in another library does not have a negative
   effect on the correctness or performance of other libraries in the same
   application that do not use it.

Maintain compatibility with the DRM specification.
                                                  

In particular, the extensions support the production of efficient code
for programs written to be portable with respect to the DRM
specification. Use of implementation-specific types or operations in
order to get reasonable efficiency is not required. This precludes
relegating the *<integer>* class and *limited-<integer>* types to
inefficient implementations.

.. note:: When there are several distinct interfaces with the same name
   but in different modules, the notation *interface* *#* *module* is used
   in this chapter to remove ambiguity.

Specify that the class *<integer>* has a finite,
implementation-dependent range, bounded by the constants
*$minimum-integer* and *$maximum-integer*.
                                                                                                                                                     

The representation for integers must be at least 28 bits, including the
sign. That is, the minimum conforming value for *$maximum-integer* is
2*27* -1 and the maximum conforming value for *$minimum-integer* is
-2*27*.

*Rationale:* Restricting *<integer>* in this way allows the programmer
to stay in the efficient range without requiring exact knowledge of what
that range might be. The full generality of extended precision integers
is provided by the Big-Integers library, for programmers who actually
need that functionality.

Define the type *<machine-number>* to be the type union of *<float>* and
*<integer>*.
                                                                                      

The Dylan library provides implementations of the generic functions and
functions described in this chapter. If the result of one of these
operations is specified to be an instance of *<integer>* and the
mathematically correct result cannot be represented as an *<integer>*
then an error is signaled. This removes fully generic arithmetic from
the Dylan library. In particular, it removes extended integers, ratios,
and rectangular complex numbers.

Extensions to the Dylan library
===============================

This section describes the extensions to the Dylan library that provide
the arithmetic operations available as standard to your applications.
You do not have to explicitly use any additional libraries to have
access to any of the functionality described in this section. Note that
this section only describes extensions to the Dylan library; for
complete descriptions, you should also refer to the *Dylan Reference
Manual*.

Note that the Common-Dylan library also has these extensions because it
uses the Dylan library.

Ranges
------

The initialization arguments for *<range>* must all be instances of
*<machine-number>* rather than *<real>*.

Specific constructors
---------------------

The following specific constructors are available for use with the class
*<integer>*.

limited
-------

G.f. method
'''''''''''

Summary
       

Defines a new type that represents a subset of the class *<integer>*.

Arguments
         

-  *singleton(<integer>)*
-  *min:* The lower bound of the range. The default is
   *$minimum-integer*.
-  *max: The upper bound of the range. The default is $maximum-integer*

Signature
         

limited *integer-class* #key *min* *max* => *limited-type*

Description
           

The *integer-class* argument is the class *<integer>*, and all other
arguments are instances of *<integer>*. The range of *<integer>* is
bounded by default.

range
-----

Function
''''''''

Summary
       

This function is used to specify ranges of numbers.

Arguments
         

Signature
         

range (#key from:, to:, above:, below:, by:, size:) => <range>

Description
           

All of the supplied arguments must be instances of *<machine-number>*.

Equality comparisons
--------------------

The *=* function compares two objects and returns *#t* if the values of
the two objects are equal to each other, that is of the same magnitude.

=
~

Generic function, Sealed domain, G.f. method
''''''''''''''''''''''''''''''''''''''''''''

Summary
       

Tests its arguments to see if they are of the same magnitude.

Signature
         

= *object1* *object2* => *boolean* (*Generic function*)
 = *complex1* *complex2* => *boolean* (*Sealed domain*)
 = *machine-number1* *machine-number2* => *boolean* (*G.f. method*)

Value
     

*<boolean>*

Other available methods are described in the *Dylan Reference Manual*.

Magnitude comparisons
---------------------

The Dylan library provides the following interfaces for testing the
magnitude of two numbers:

<
~

Generic function, Sealed domain, G.f. method
''''''''''''''''''''''''''''''''''''''''''''

Summary
       

Returns #t if its first argument is less than its second argument.

Signature
         

< *object1* *object2* => *boolean* (*Generic function*)*
* < *complex1* *complex2* (*Sealed domain*)
 < *machine-number1* *machine-number2* => *boolean* (*G.f. method*)

Other available methods are described in the *Dylan Reference Manual*.

Properties of numbers
---------------------

Various number properties can be tested using the following predicates
in the Dylan library:

odd?
----

Open generic function, Sealed domain, G.f. method
'''''''''''''''''''''''''''''''''''''''''''''''''

Summary
       

Tests whether the argument supplied represents an odd value.

Signature
         

odd? *object* => *boolean* (*Open generic function*)
 odd? *complex* => *boolean* (*Sealed domain*)
 odd? *integer* => *boolean* (*G.f. method*)

even?
-----

Open generic function, Sealed domain, G.f. method
'''''''''''''''''''''''''''''''''''''''''''''''''

Summary
       

Tests whether the argument supplied represents an even value

Signature
         

even? *object* => *boolean* (*Open generic function*)
 even? *complex* *=>* *boolean* (*Sealed domain*)
 even? *integer* => *boolean* (*G.f. method*)

zero?
     

Open generic function
                     

zero? *object* => *boolean*

zero?
     

Sealed domain
             

zero? *complex*

zero?
     

G.f. method
           

zero? *machine-number* => *boolean*

Tests whether the argument supplied represents a zero value.

positive?
         

Open generic function
                     

positive? *object* => *boolean*

positive?
         

Sealed domain
             

positive? *complex*

positive?
         

G.f. method
           

positive? *machine-number* => *boolean*

Tests whether the argument supplied represents a positive value.

negative?
         

Open generic function
                     

negative? *object* => *boolean*

negative?
         

Sealed domain
             

negative? *complex*

negative?
         

G.f. method
           

negative? *machine-number* => *boolean*

Tests whether the argument supplied represents a negative value.

integral?
         

Open generic function
                     

integral? *object* => *boolean*

integral?
         

Sealed domain
             

integral? *complex*

integral?
         

G.f. method
           

integral? *machine-number* => *boolean*

Tests whether the argument supplied represents an integral value.

Arithmetic operations
---------------------

The following arithmetic operations are available in the Dylan library:

+
 

Open generic function
                     

+ *object1* *object2* => #rest *object*

+
 

Sealed domain
             

+ *complex1* *complex* 2

+
 

G.f. method
           

+ *integer1* *integer* 2 => *integer*

+
 

G.f. method
           

+ *machine-number1* *machine-number2* => *machine-number*

Returns the sum of the two supplied arguments. The actual type of the
value is determined by the contagion rules when applied to the
arguments.

-
 

Open generic function
                     

- *object1* *object2* => #rest *object*

-
 

Sealed domain
             

- *complex1* *complex2*

-
 

G.f. method
           

- *integer1 integer2* => *integer*

-
 

G.f. method
           

- *machine-number1* *machine-number2* => *machine-number*

Returns the result of subtracting the second argument from the first.
The actual type of the value is determined by the contagion rules when
applied to the arguments.

\*
  

Open generic function
                     

\* *object1* *object2* => #rest *object*

\*
  

Sealed domain
             

\* *complex1* *complex2*

\*
  

G.f. method
           

\* *integer1* *integer* 2 => *integer*

\*
  

G.f. method
           

\* *machine-number1* *machine-number2* => *machine-number*

Returns the result of multiplying the two arguments. The actual type of
the value is determined by the contagion rules when applied to the
arguments.

/
 

Open generic function
                     

/ *object1* *object2* => #rest *object*

/
 

Sealed domain
             

/ *complex1* *complex2*

/
 

G.f. method
           

/ *float1* *float* 2 => *float*

Returns the result of dividing the first argument by the second. The
actual type of the value is determined by the contagion rules when
applied to the arguments.

negative
        

Open generic function
                     

negative *object* => #rest *negative-object*

negative
        

Sealed domain
             

negative *complex*

negative
        

G.f. method
           

negative *integer* => *negative-integer*

negative
        

G.f. method
           

negative *float* => *negative-float*

Negates the supplied argument. The returned value is of the same float
format as the supplied argument.

floor
     

Function
        

floor *machine-number* => *integer* *machine-number*
 floor *integer* => *integer* *integer*
 floor *float* => *integer* *float*

Truncates a number toward negative infinity. The integer part is
returned as *integer*, the remainder is of the same float format as the
argument.

ceiling
       

Function
        

ceiling *machine-number* => *integer* *machine-number*
 ceiling *integer* => *integer* *integer*
 ceiling *float* => *integer* *float*

Truncates a number toward positive infinity. The integer part is
returned as *integer*, the remainder is of the same float format as the
argument.

round
     

Function
        

round *machine-number* => *integer* *machine-number*
 round *integer* => *integer* *integer*
 round *float* => *integer* *float*

Rounds a number toward the nearest mathematical integer. The integer
part is returned as *integer*, the remainder is of the same float
format as the argument. If the argument is exactly between two integers,
then the result *integer* will be a multiple of two.

truncate
        

Function
        

truncate *machine-number* => *integer* *machine-number*
 truncate *integer* => *integer* *integer*
 truncate *float* => *integer* *float*

Truncates a number toward zero. The integer part is returned as
*integer*, the remainder is of the same float format as the argument.

floor/
      

Function
        

floor/ *machine-number1* *machine-number2* => *integer* *machine-number*
 floor/ *integer1* *integer2* => *integer* *integer*
 floor/ *machine-number1* *machine-number2* => *integer*
*machine-number*

Divides the first argument into the second and truncates the result
toward negative infinity. The integer part is returned as *integer*,
the type of the remainder is determined by the contagion rules when
applied to the arguments.

ceiling/
        

Function
        

ceiling/ *machine-number1* *machine-number2* => *integer*
*machine-number*
 ceiling/ *integer1* *integer2* => *integer* *integer*
 ceiling/ *machine-number1* *machine-number2* => *integer*
*machine-number*

Divides the first argument into the second and truncates the result
toward positive infinity. The integer part is returned as *integer*,
the type of the remainder is determined by the contagion rules when
applied to the arguments.

round/
      

Function
        

round/ *machine-number1* *machine-number2* => *integer* *machine-number*
 round/ *integer1* *integer2* => *integer* *integer*
 round/ *machine-number1* *machine-number2* => *integer*
*machine-number*

Divides the first argument into the second and rounds the result toward
the nearest mathematical integer. The integer part is returned as
*integer*, the type of the remainder is determined by the contagion
rules when applied to the arguments.

truncate/
         

Function
        

truncate/ *machine-number1* *machine-number2* => *integer*
*machine-number*
 truncate/ *integer1* *integer* 2 => *integer* *integer*
 truncate/ *machine-number1* *machine-number2* => *integer*
*machine-number*

Divides the first argument into the second and truncates the result
toward zero. The integer part is returned as *integer*, the type of the
remainder is determined by the contagion rules when applied to the
arguments.

modulo
      

Function
        

modulo *machine-number1* *machine-number2* => *machine-number*
 modulo *integer1* *integer2* => *integer*
 modulo *machine-number1* *machine-number2* => *machine-number*

Returns the second value of *floor/ (* *arg1* *,* *arg2* *)*. The
actual type of the second value is determined by the contagion rules
when applied to the arguments.

remainder
         

Function
        

remainder *machine-number1* *machine-number2* => *machine-number*
 remainder *integer1* *integer2* => *integer*
 remainder *machine-number1* *machine-number2* => *machine-number*

Returns the second value of *truncate/ (* *arg1* *,* *arg2* *)*.The
actual type of the second value is determined by the contagion rules
when applied to the arguments.

^
 

Open generic function
                     

^ *object1* *object2* => #rest *object*

^
 

Sealed domain
             

^ *complex1* *complex* 2

^
 

G.f. method
           

^ *integer1* *integer2* => *integer*

^
 

G.f. method
           

^ *float1* *integer2* => *float*

Returns the first argument raised to the power of the second argument.
The value is of the same float format as the first argument. An error is
signalled if both arguments are 0.

abs
   

Open generic function
                     

abs *object* => #rest *object*

abs
   

Sealed domain
             

abs *complex*

abs
   

G.f. method
           

abs *integer* => *integer*

abs
   

G.f. method
           

abs *float* => *float*

Returns the absolute value of the argument. The value is of the same
float format as the argument.

logior
      

Function
        

logior #rest *integers* => *integer*

Returns the bitwise inclusive *OR* of its integer arguments.

logxor
      

Function
        

logxor #rest *integers* => *integer*

Returns the bitwise exclusive *OR* of its integer arguments.

logand
      

Function
        

logand #rest *integers* => *integer*

Returns the bitwise *AND* of its integer arguments.

lognot
      

Function
        

lognot *integer1* => *integer2*

Returns the bitwise *NOT* of its integer arguments.

logbit?
       

Function
        

logbit? *index* *integer* => *boolean*

Tests the value of a particular bit in its integer argument. The *index*
argument is an instance of *<integer>*.

ash
   

Function
        

ash *integer1* *count* => *integer*

Performs an arithmetic shift on its first argument.

lcm
   

Function
        

lcm *integer1* *integer2* => *integer*

Returns the least common multiple of its two arguments.

gcd
   

Function
        

gcd *integer1* *integer2* => *integer*

Returns the greatest common divisor of its two arguments.

Collections
-----------

The keys for sequences are always instances of *<integer>*. This means
that certain kinds of collections cannot be sequences; very large (or
unbounded) sparse arrays are an example.

The table protocol
------------------

The following functions in the Dylan library are extended. Note that the
hash IDs for tables are always instances of *<integer>*.

merge-hash-codes
                

Function
        

merge-hash-codes *id1* *state1* *id2* *state2* #key *ordered?*
 => *merged-id* *merged-state*

Returns a hash code created from the merging of two argument hash codes.
The *id* arguments are hash IDs, and the *state* arguments are hash
states (instances of *<object>*). The *ordered?* argument is an
instance of *<boolean>*. The returned merged values are instances of
*<integer>* and *<object>*, as determined by the name of each argument.

object-hash
           

Function
        

object-hash *object* => *hash-id* *hash-state*

The hash function for the equivalence predicate *==*. The return values
are of the same types as the return values of *merge-hash-codes*.

Iteration constructs
--------------------

for
   

Statement macro
               

The *start*, *bound*, and *increment* expressions in a numeric clause
must evaluate to instances of *<machine-number>* for this macro.

The Generic-Arithmetic library
==============================

The Generic-Arithmetic library exports the functions described in this
section from an exported module called *generic-arithmetic*.

The Generic-Arithmetic library provides a fully extensible version of
all arithmetic operations. If an application only uses
Generic-Arithmetic, these versions of the operators reduce themselves to
be equivalent to those in the Dylan library. But when you use additional
implementation libraries, the arithmetic operators are extended.

The Big-Integers library is one such implementation library. It provides
a 64-bit implementation of *<integer>*.

The standard integer implementation in the Dylan library is actually
part of the following class hierarchy:

<abstract-integer>

<integer>

<big-integer>

<double-integer>
                

(The classes *<big-integer>* and *<double-integer>* are implementation
classes. You do not need to use them.)

The modules in the Generic-Arithmetic library export
*<abstract-integer>* with the name *<integer>*. They also export a full
set of arithmetic operators that use instances of *<abstract-integer>*
rather than instances of *<integer>* (in the Dylan library naming
scheme). However, those operators just fall back to the Dylan library
operators until you include an implementation library, such as
Big-Integers, in your application.

When you use the Big-Integers library, the arithmetic operators exported
by Generic-Arithmetic are enhanced to extend their results to 64-bit
integers. If a result is small enough to fit in a Dylan library
*<integer>*, it will be fitted into one.

Note that the Generic-Arithmetic library uses the same naming
conventions for arithmetic operators as used by the Dylan library. This
means that some renaming is required in modules that require access to
both the basic Dylan interfaces and the interfaces supplied by the
Generic-Arithmetic library. As described earlier, the notation
*interface* *#* *module* is used to denote different interfaces of the
same name, where *interface* is the name of the interface, and *module*
is the name of the module it is exported from.

See `Using special arithmetic features`_ for an example of how to use
an implementation library with Generic-Arithmetic.

Ranges
------

The Generic-Arithmetic library defines the class *<range>*, which is in
most respects functionally equivalent to *<range>#Dylan*, but uses
generic arithmetic operations in its implementation so that the
initialization arguments can be instances of *<real>*, rather than
being restricted to *<machine-number>*.

Classes
-------

The class *<abstract-integer>* is imported and re-exported under the
name *<integer>#generic-arithmetic*.

Specific constructors
---------------------

range
     

Function
        

range #key *from* *to* *above* *below* *by* *size* => *range*

This function is identical to the function *range#Dylan*, except that
all of the supplied arguments must be instances of *<real>*.

Arithmetic operations
---------------------

The following functions all apply *function* *#Dylan* to the arguments
and return the results, where *function* is the appropriate function
name. See `Arithmetic operations`_ for descriptions of each function
as implemented in the Dylan library.

+ *object1* *object2* => #rest *object*

- *object1* *object2* => #rest *object*

\* *object1* *object2* => #rest *object*

/ *object1* *object2* => #rest *object*

negative *object* => #rest *negative-object*

floor *real1* => *abstract-integer* *real*

ceiling *real1* => *abstract-integer* *real*

round *real1* => *abstract-integer* *real*

truncate *real1* => *abstract-integer* *real*

floor/ *real1* *real2* => *abstract-integer* *real*

ceiling/ *real1* *real2* => *abstract-integer* *real*

round/ *real1* *real2* => *abstract-integer* *real*

truncate/ *real1* *real2* => *abstract-integer* *real*

modulo *real1* *real2* => *real*

remainder *real1* *real2* => *real*

^ *object1* *object2* => #rest *object*

abs *object1* => #rest *object*

logior #rest *abstract-integer1* => *abstract-integer*

logxor #rest *abstract-integer1* => *abstract-integer*

logand #rest *abstract-integer1* => *abstract-integer*

lognot *abstract-integer1* => *abstract-integer*

logbit? *integer* *abstract-integer* => *boolean*

ash *abstract-integer1* *integer* => *abstract-integer*

lcm *abstract-integer1* *abstract-integer2* => *abstract-integer*

gcd *abstract-integer1* *abstract-integer2* => *abstract-integer*
                                                                 

Iteration constructs
--------------------

While a programmer could make use of generic arithmetic in a *for* loop
by using explicit-step clauses, this approach leads to a loss of
clarity. The definition of the *for* macro is complex, so a version that
uses generic arithmetic in numeric clauses is provided, rather than
requiring programmers who want that feature to reconstruct it.

for
   

Statement macro
               

The *start*, *bound*, and *increment* expressions in a numeric clause
must evaluate to instances of *<machine-number>* for this macro.
Otherwise, this macro is similar to *for#Dylan*.

Exported modules from the Generic-Arithmetic library
----------------------------------------------------

The Generic-Arithmetic library exports several modules that are provided
for the convenience of programmers who wish to create additional modules
based on the *dylan* module plus various combinations of the arithmetic
models.

The Dylan-Excluding-Arithmetic module
-------------------------------------

The Dylan-Excluding-Arithmetic module imports and re-exports all of the
interfaces exported by the *dylan* module from the Dylan library, except
for the following excluded interfaces:

<integer>

range

+ - \* /

negative

floor ceiling round truncate

floor/ ceiling/ round/ truncate/

modulo remainder

^

abs

logior logxor logand lognot

logbit?

ash

lcm gcd

for
   

The Dylan-Arithmetic module
---------------------------

The Dylan-Arithmetic module imports and re-exports all of the interfaces
exported by the *dylan* module from the Dylan library which are excluded
by the *dylan-excluding-arithmetic* module.

The Generic-Arithmetic-Dylan module
-----------------------------------

The Generic-Arithmetic-Dylan module imports and reexports all of the
interfaces exported by the *dylan-excluding-arithmetic* module and the
*generic-arithmetic* module.

The *dylan-excluding-arithmetic*, *dylan-arithmetic*, and
*generic-arithmetic* modules provide convenient building blocks for
programmers to build the particular set of global name bindings they
wish to work with. The purpose of the *generic-arithmetic-dylan* module
is to provide a standard environment in which generic arithmetic is the
norm, for those programmers who might want that.

Using special arithmetic features
=================================

As noted in `The Generic-Arithmetic library`_, the Generic-Arithmetic
library provides an extensible protocol for adding specialized arithmetic
functionality to your applications. By using the Generic-Arithmetic
library alongside a special implementation library, you can make the
standard arithmetic operations support number types such as big (64-bit)
integers, or complex numbers.

This section provides an example of extending the basic Dylan arithmetic
features using the Generic-Arithmetic library and the Big-Integers
implementation library.

To use special arithmetic features, an a library’s *define* *library*
declaration must use at least the following libraries:

common-dylan

generic-arithmetic

*special-arithmetic-implementation-library*
                                           

So for Big-Integers you would write:

define library foo

use common-dylan;

use generic-arithmetic;

use big-integers;

…

end library foo;
                

Next you have to declare a module. There are three ways of using
big-integer arithmetic that we can arrange with a suitable module
declaration:

Replace all integer arithmetic with the big-integer arithmetic
                                                              

Use both, with normal arithmetic remaining the default
                                                      

Use both, with the big-integer arithmetic becoming the default
                                                              

To get one of the three different effects described above, you need to
arrange the *define* *module* declaration accordingly. To replace all
integer arithmetic with big-integer arithmetic, include the following in
your *define* *module* declaration:

use generic-arithmetic-common-dylan;
                                    

(Note that the module definition should not use the Big-Integers module.
The Big-Integers library is used as a side-effects library only, that
is, it is referenced in the library definition so that it will be
loaded. Its definitions extend the Generic-Arithmetic library.)

If you replace all integer arithmetic with big-integer arithmetic in
this way, there will be performance hits. For instance, loop indices
will have to be checked at run-time to see whether a normal or big
integer representation is being used, and a choice must be made about
the representation for an incremented value.

You can take a different approach that reduces the cost of big-integer
arithmetic. Under this approach you leave normal integer arithmetic
unchanged, and get access to big-integer arithmetic when you need it. To
do this, use the same libraries but instead of using the
*common-dylan-generic-arithmetic* module, include the following in your
*define* *module* declaration:

use common-dylan;

use generic-arithmetic, prefix: "ga/"; // use any prefix you like
                                                                 

This imports the big-integer arithmetic binding names, but gives them a
prefix *ga/*, using the standard renaming mechanism available in module
declarations. Thus you gain access to big arithmetic using renamed
classes and operations like:

ga/<integer>

ga/+

ga/-

ga/\*

…
 

The operations take either instances of *<integer>* or *ga/<integer>* (a
subclass of *<integer>*) and return instances of *ga/<integer>*.

Note that having imported the big-integer operations under new names,
you have to use prefix rather than infix syntax when calling them. For
example:

ga/+ (5, 4);
            

not:

5 ga/+ 4;
         

The existing functions like *+* and *-* will only accept *<integer>*
instances and *ga/<integer>* instances small enough to be represented as
*<integer>* instances.

Under this renaming scheme, reduced performance will be confined to the
*ga/* operations. Other operations, such as loop index increments and
decrements, will retain their efficiency.

Finally, you can make big-integer arithmetic the default but keep normal
arithmetic around for when you need it. Your *define* *module*
declaration should contain:

use generic-arithmetic-common-dylan;

use dylan-arithmetic, prefix: "dylan/"; //use any prefix you like
                                                                 

The Big-Integers library
========================

The Big-Integers library exports a module called *big-integers*, which
imports and re-exports all of the interfaces exported by the
*generic-arithmetic* module of the Generic-Arithmetic library.

The Big-Integers library modifies the behavior of functions provided by
the Dylan library as described in this section.

Specific constructors
---------------------

The Big-Integers library extends the functionality of specific
constructors in the Dylan library as follows:

limited
       

G.f. method
           

limited *abstract-integer-class* #key *min* *max* => *limited-type*

Returns a limited integer type, which is a subtype of
*<abstract-integer>*, whose instances are integers greater than or
equal to *min* (if specified) and less than or equal to *max* (if
specified). If no keyword arguments are specified, the result type is
equivalent to *<abstract-integer>*. The argument
*abstract-integer-class* is the class *<abstract-integer>*.

If both *min* and *max* are supplied, and both are instances of
*<integer>*, then the result type is equivalent to calling *limited* on
*<integer>* with those same bounds.

The Limited Integer Type Protocol is extended to account for limited
*<abstract-integer>* types.

Instances and subtypes in the Big-Integers library

:: todo Fix header style here---
                                                  

This is true if and only if …

… all these clauses are true

instance?
 (x,
 limited(<abstract-integer>,
 min: y, max: z))

instance?(x, <abstract-integer>)
 (y <= x)
 (x <= z)

instance?
 (x,
 limited(<abstract-integer>,
 min: y))

instance?(x, <abstract-integer>)
 (y <= x)

instance?
 (x,
 limited(<abstract-integer>,
 max: z))

instance?(x, <abstract-integer>)
 (x <= z)

subtype?
 (limited(<abstract-integer>,
 min: w, max: x),
 limited(<abstract-integer>,
 min: y, max: z))

(w >= y)
 (x <= z)

subtype?
 (limited(<abstract-integer>,
 min: w ...),
 limited(<abstract-integer>,
 min: y))

(w >= y)

subtype?
 (limited(<abstract-integer>,
 max: x ...),
 limited(<abstract-integer>,
 max: z))

(x <= z)

Type-equivalence in the Big-Integers library
:: todo Fix header style here---
                                                  
                                            

This is type equivalent to …

… this, if and only if …

… this is true

limited
 (<abstract-integer>,
 min: y, max: z)

limited
 (<integer>,
 min: y, max: z)

*y* and *z* are both instances of *<integer>*.

limited
 (<abstract-integer>,
 min: y,
 max: $maximum-integer)

limited
 (<integer>, min: y)

*y* is an instance of *<integer>*.

limited
 (<abstract-integer>,
 min: $minimum-integer,
 max: z)

limited
 (<integer>, max: z)

*z* is an instance of *<integer>*.

Type disjointness is modified as follows to account for limited
*<abstract-integer>* types.

A limited integer type is disjoint from a class if their base types are
disjoint or the class is *<integer>* and the range of the limited
integer type is disjoint from the range of *<integer>* (that is, from
*$minimum-integer* to *$maximum-integer*).

Equality comparisons
--------------------

The behavior of equality comparisons in the Dylan library is modified by
the Big-Integers library as follows:

= *abstract-integer1* *abstract-integer2* => *boolean*
 = *abstract-integer* *float* => *boolean*
 = *float* *abstract-integer* => *boolean*

Magnitude comparisons
---------------------

The behavior of magnitude comparisons in the Dylan library is modified
by the Big-Integers library as follows:

< *abstract-integer1* *abstract-integer2* => *boolean
* < *abstract-integer* *float* => *boolean*
 < *float* *abstract-integer* => *boolean*

Properties of numbers
---------------------

The behavior of number property tests in the Dylan library is modified
by the Big-Integers library as follows:

odd? *abstract-integer* => *boolean
* even? *abstract-integer* => *boolean*
 zero? *abstract-integer* => *boolean*
 positive? *abstract-integer* => *boolean*
 negative? *abstract-integer* => *boolean*
 integral? *abstract-integer* => *boolean*

Arithmetic operations
---------------------

The Big-Integers library modifies the behavior of the functions provided
by the Generic-Arithmetic library as described below.

The actual type of the return value for all the following interfaces is
determined by the contagion rules when applied to the arguments.

+ *abstract-integer1* *abstract-integer2* => *abstract-integer
* + *abstract-integer* *float1* => *float*
 + *float1* *abstract-integer* => *float*

- *abstract-integer1* *abstract-integer2* => *abstract-integer
* - *abstract-integer* *float1* => *float*
 - *float1* *abstract-integer* => *float*

\* *abstract-integer1* *abstract-integer2* => *abstract-integer
* \* *abstract-integer* *float1* => *float*
 \* *float1* *abstract-integer* => *float*

The return value of the following interface is of the same float format
as the argument.

negative *abstract-integer* => *negative-abstract-integer*

The second return value of all the following interfaces is of the same
float format as the argument.

floor *abstract-integer* => *abstract-integer1* *abstract-integer2
* floor *float1* => *abstract-integer* *float*

ceiling *abstract-integer* => *abstract-integer1* *abstract-integer2
* ceiling *float1* => *abstract-integer* *float*

round *abstract-integer* => *abstract-integer1* *abstract-integer2
* round *float1* => *abstract-integer* *float*

truncate *abstract-integer* => *abstract-integer1* *abstract-integer2
* truncate *float1* => *abstract-integer* *float*

The second return value of all the following interfaces is of the same
float format as the first argument.

floor/ *abstract-integer1* *abstract-integer2* => *abstract-integer3*
*abstract-integer4
* floor/ *float1* *abstract-integer1* => *abstract-integer2* *float2*

ceiling/ *abstract-integer1* *abstract-integer2*
 => *abstract-integer3* *abstract-integer4*
 ceiling/ *float1* *abstract-integer1* => *abstract-integer2* *float2*

round/ *abstract-integer1* *abstract-integer2* => *abstract-integer3*
*abstract-integer4
* round/ *float1* *abstract-integer1* => *abstract-integer2* *float2*

truncate/ *abstract-integer1* *abstract-integer2*
 => *abstract-integer3* *abstract-integer4
* truncate/ *float1* *abstract-integer1* => *abstract-integer2* *float2*

The second return value of the following interfaces is of the same float
format as the second argument.

floor/ *abstract-integer1* *float1* => *abstract-integer2* *float2*

ceiling/ *abstract-integer1* *float1* => *abstract-integer2* *float2*

round/ *abstract-integer1* *float1* => *abstract-integer2* *float2*

truncate/ *abstract-integer1* *float1* => *abstract-integer2* *float2*

The return value of the following interfaces is of the same float format
as the first argument.

modulo *float1* *abstract-integer* => *float*

remainder *float1* *abstract-integer* => *float*

The return value of the following interfaces is of the same float format
as the second argument.

modulo *abstract-integer1* *abstract-integer2* => *abstract-integer
* modulo *abstract-integer* *float1* => *float*

remainder *abstract-integer1* *abstract-integer2* => *abstract-integer
* remainder *abstract-integer* *float1* => *float*

The behavior of the following miscellaneous interfaces is also modified
by the Big-Integers library.

^ *abstract-integer1* *integer* => *abstract-integer
* abs *abstract-integer1* => *abstract-integer*
 logior #rest *abstract-integer1* => *abstract-integer*
 logxor #rest *abstract-integer1* => *abstract-integer*
 logand #rest *abstract-integer1* => *abstract-integer*
 lognot *abstract-integer1* => *abstract-integer*
 logbit? *integer* *abstract-integer* => *boolean*
 ash *abstract-integer1* *integer* => *abstract-integer*
 lcm *abstract-integer1* *abstract-integer2* => *abstract-integer*
 gcd *abstract-integer1* *abstract-integer2* => *abstract-integer*


