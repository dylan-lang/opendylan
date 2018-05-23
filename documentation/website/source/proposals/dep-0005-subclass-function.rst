********
Subclass
********

==============  =============================================
DEP #:          5
Type:           Standards Track
Affects-DRM:    Yes
Author:         Keith Playford
Status:         Accepted
Created:        13-Jul-1995
Last-Modified:  08-Aug-1995
Post-History:   None
==============  =============================================

*Note: This DEP was converted from the old Dylan Change Proposal
Format to DEP format with minimal changes for ReStructuredText.  It
does not fully conform to the new format.*

Abstract
========

   Methods on generic functions such as Dylan's standard "make" and "as"
   which take types as arguments are impossible to reuse in Dylan without
   resorting to ad hoc techniques. As things stand, the only available
   mechanism for specializing such methods is through the use of
   singleton types. A singleton type specializer used in this way, by
   definition, gives a method applicable to exactly one type. In
   particular, such methods are not applicable to subtypes of the type in
   question.

   In order to define reusable methods on generic functions like this, we
   need a type which allows us to express applicability to a type and all
   its subtypes.

Specification
=============

   Define the following:

   .. code-block:: dylan

     subclass (class) => type

   A call to this function returns a type which describes all the
   objects representing subclasses of the given class. We term such a
   type a "subclass type".

   The subclass function is allowed to return an existing type if that
   type is type equivalent to the subclass type requested.

   For an object O and class Y, the following instance? relationship
   applies:

   * INSTANCE-1. instance?(O, subclass(Y))
     This will be true if and only if O is a class and O is a subclass of Y.

   For classes X and Y the following subtype? relationships hold (note
   that a rule applies only when no preceding rule matches):

   * SUBTYPE-1. subtype?(subclass(X), subclass(Y))
     This will be true if and only if X is a subclass of Y.

   * SUBTYPE-2. subtype?(singleton(X), subclass(Y))
     This will be true if and only if X is a class and X is a subclass of
     Y.

   * SUBTYPE-3. subtype?(subclass(X), singleton(Y))
     This is always false.

   * SUBTYPE-4. subtype?(subclass(X), Y), where Y is not a subclass type
     This will be true if Y is <class> or any proper superclass of
     <class> (including <object>, any implementation-defined supertypes,
     and unions involving any of these). There may be other
     implementation-defined combinations of types X and Y for which this
     is also true.

   * SUBTYPE-5. subtype?(X, subclass(Y)), where X is not a subclass type
     This will be true if Y is <object> or any proper supertype of
     <object> and X is a subclass of <class>.

   Note that by subclass relationships SUBYPE-4 and SUBTYPE-5, we get
   this correspondance:

   * <class> and subclass(<object>) are type equivalent.

   Where the subtype? test has not been sufficient to determine an
   ordering for a method's argument position, the following further
   method ordering rules apply to cases involving subclass types (note
   that a rule applies only when no preceding rule matches):

   * SPECIFICITY+1. subclass(X) precedes subclass(Y) when the argument is
     a class C and X precedes Y in the class precedence list of C.

   * SPECIFICITY+2. subclass(X) always precedes Y, Y not a subclass type.
     That is, applicable subclass types precede any other applicable
     class-describing specializer.

   The constraints implied by sealing come by direct application of
   sealing rules 1 - 3 and the following disjointness criteria for
   subclass types (note that a rule applies only when no preceding rule
   matches):

   * DISJOINTNESS+1. A subclass type subclass(X) and a type Y are
     disjoint if Y is disjoint from <class>.

   * DISJOINTNESS+2. Two subclass types subclass(X) and subclass(Y) are
     disjoint if the classes X and Y are disjoint.

   * DISJOINTNESS+3. A subclass type subclass(X) and a singleton type
     singleton(O) are disjoint unless O is a class and O is a subclass of
     X.

   Method sealing's rule 3 must be changed to include the following:

   * A method M (with specializers S1...Sn) in G also potentially blocks
     C at argument position i if there exist j and k such that
     subclass(Dj) is a pseudosubtype of Si, subclass(Dk) is a
     pseudosubtype of Ti, and subclass(Dk) is not a pseudosubtype of Si.

   This relies on the acceptance of Version 6 of "Method Sealing Bug".

Amendment
=========

   Specify that a subclass of <class> is considered disjoint from a
   subclass type until a class is created that is a common instance of
   both.

   Do this by changing the disjointness rule:

   * A subclass type subclass(X) and a type Y are disjoint if Y is
     disjoint from <class>.

   to read:

   * A subclass type subclass(X) and a type Y are disjoint if Y is
     disjoint from <class>, or if Y is a subclass of <class> without
     instance classes that are also subclasses of X.

Rationale
=========

   Subclass types don't address everything in the problem description,
   particularly limited collection types, but are simpler to understand
   than subtype types and still very useful, particularly within user-
   defined frameworks where limited types are not an issue.

   The guiding principle behind the semantics is that, as far as
   possible, methods on classes called with an instance should behave
   isomorphically to corresponding methods on corresponding subclass
   types called with the class of that instance. So, for example, given
   the heterachy::

     <object>
         \
         <A>
         / \
       <B> <C>
         \ /
         <D>

   and methods:

   .. code-block:: dylan

     method foo (<A>)
     method foo (<B>)
     method foo (<C>)
     method foo (<D>)

     method foo-using-type (subclass(<A>))
     method foo-using-type (subclass(<B>))
     method foo-using-type (subclass(<C>))
     method foo-using-type (subclass(<D>))

   that for a direct instance D1 of <D>:

   .. code-block:: dylan

     foo-using-type(<D>)

   should behave analogously to:

   .. code-block:: dylan

     foo(D1)

   with respect to method selection.

   The clause added to sealing's "Rule 3" forces class creation to
   respect sealing constraints on this parallel heterarchy. Thus, sealing
   methods over subclass(C1), ..., subclass(CN) results in the same
   constraints on class creation as sealing methods with the same
   structure over C1, ..., CN if metaclasses are ignored.

   The rule that has subclass specializers precede other specializers
   applicable to types seems arbitrary but looks to be most useful and
   leads to the simplest "rule of thumb", being that for a set of
   applicable types the general order in decreasing order of
   applicability is:

   * singleton specializer
   * subclass specializers
   * class specializers

   No attempt is made to be "clever" with subtype? relationships. In
   particular, the simplifying assumption is made that any class might
   have subclasses we know nothing about, regardless of any sealing
   declarations that might appear in the code. This seems a reasonable
   course to take.

   The rationale for the amendment is to prevent sealing a generic
   function over a subclass type of a class from blocking the creation of
   new metaclasses unnecessarily. The amendment may or may not be
   considered necessary at this point given that Dylan does not currently
   address the issue of introducing new metaclasses. Note that even
   without the amendment an implementation is free to have a number of
   initial, explicitly-defined metaclasses without any problems, in which
   case the situation is analogous to that of limited <integer>.

Examples
========

   .. code-block:: dylan

     // Common usage:

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

     // Less common usage:

     // Metatype methods

     define method classify (type :: <type>)
       print("A type");
     end method;

     define method classify (type :: <class>)
       print("A class");
       next-method();
     end method;

     define method classify (type :: <singleton>)
       print("A singleton");
       next-method();
     end method;

     // "User" level subclass methods

     define method classify (type :: subclass(<object>))
       print("A subclass of <object>");
       next-method();
     end method;

     ? classify(<symbol>);
     A subclass of <object>
     A class
     A type

     ? classify(singleton(<symbol>))
     A subclass of <object>
     A singleton
     A type

     ? classify(subclass(<symbol>))
     A subclass of <object>
     A type

Cost to implementors
====================

   Another type to implement. Given the number of types Dylan has and
   allows you to specialize on already, any implementation is going to
   require a well thought out type framework. Given this, it should be
   reasonable to add in the new rules for <subclass> as specified.

Cost to users
=============

   Another type to understand, although they shouldn't have to face it
   unless they have need of it, and even then only the "obvious" aspects
   of its behaviour are likely to be necessary in user programs.

Performance impact
==================

   Singleton types, and possibly limited integer types, are likely to be
   implemented through some kind of secondary dispatch scheme
   already. Subclass types could be dealt with using the same
   technique. In the common case where a generic function defines a
   restriction on an argument that constrains it to be a type object
   (make, as), there is potential to do somewhat better by hijacking the
   primary dispatch mechanism in the corresponding argument position.

Benefits
========

   Methods that take classes as arguments become reusable!

Aesthetics
==========

   Good on balance because it fills a tangible hole in the language in a
   consistent, intuitive way rather than leaving the hole there to be
   fallen into or filled by ad hoc techniques.

Discussion
==========

   The subclass relationship definitions don't spell everything out
   explicitly when it comes to union types - for the definition of new
   types to be tractable we have to be able to fall back on existing
   definitions of composite types like union, and we do here.

From Keith Playford in 1995
===========================

   Subclass specialisation is interesting in that most groups seem to
   recognise the need for it and, in fact, actually implement and use it
   in some form or another: we now support it, as does Mindy through an
   extension of the "limited" type mechanism, and some of the mail on
   collection coercion suggests that Apple Dylan supports it too.

   Our own implementation follows the shelved proposal "Subclass
   Specialization" by providing the type constructor "subclass" which
   returns a type describing all the subclasses of its argument class.
   The method specificity rules are extended such that the CPL of the
   argument class is taken into account. Loosely, this means that a
   method set specialising on the subclass types subclass(Ci) will be
   selected and ordered by a class argument C in a way isomorphic to a
   corresponding method set specialising on Ci given argument make(C).

   We'd like to bring "Subclass Specialization" up to date, hopefully in
   line with the above, and try to get it agreed between the partners.
   Like all of these issues, if it can't be included in the book, we need
   to at least negotiate a standard way (library/module) of getting at
   "subclass" in implementations that support it. This could imply
   registry as a standard implementation-dependent library, with
   Gwydion's agreement.


From the Gwydion docs
=====================

   Gwydion compilers supports subclass specializers via the *limited* function.
   A subclass specializer causes a method to be invoked whenever the generic
   function was called on a value that is the specified class or any subclass
   of the specified class. The method is never invoked on a value that is an
   instance (direct or indirect) of the specified class, only when the value
   is a subclass of the specified class. The following is an example:

   .. code-block:: dylan

     define method make (result-class :: limited(<class>, subclass-of: <my-class>))
       let x = next-method();
       do-special-logging-or-something(x);
       x;
     end method;

Revision History
================
::

   Version 1, 13th July 95, Keith Playford (Harlequin)
   Version 2, 8th August 95, Keith Playford (Harlequin)

Status
======

   Accepted 08 August 1995
