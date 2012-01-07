********************
The Printing Modules
********************

Introduction
------------

The IO library’s printing modules provide an interface that outputs an
object in Dylan literal syntax if the object can be represented as a
Dylan literal, and otherwise, outputs the object in an
implementation-dependent manner. There are two functions, *print* and
*print-object*. The *print* function accepts keyword arguments that
form a print request, controlling features such as circular printing,
how deep within a data structure to print, how many elements in long
sequences to print before using an ellipsis notation, whether pretty
printing is desired, and so on. Users extend *print* ’s ability to print
various objects by adding methods to the *print-object* function. The
*print* function handles most of the overhead to satisfy special print
requests, outputting any special notations required, and it only calls
*print-object* when it is necessary to print objects. Users should
always call the *print* function to output objects, especially
recursively from within *print-object* methods to output an object’s
components. Users should never call *print-object* directly.

The IO library exports two modules for use with printing, *print* and
*pprint*. Reference entries for the interfaces exported from the
*print* module can be found in `The PRINT
module <print.htm#86222>`_, and reference entries for interfaces
exported from the *pprint* module are in `The PPRINT
module <print.htm#32575>`_.

These modules uses the Streams module. See ` <streams.htm#93942>`_ for
full details of the Streams module.

Print functions
---------------

The Print module offers two functions for users to call to print
objects, *print* and *print-to-string*.

print
'''''

Function
        

print *object* *stream* #key *level length circle? pretty?* => ()
                                                                 

Prints *object* to *stream* according to the print request formed by the
keyword arguments. A first call to *print* creates a printing stream to
represent the print request, and recursive calls to *print* on this
printing stream process the keyword arguments differently (see below).
There are inspection functions for querying the print request. When
*print* actually prints an object, it calls *print-object*. Though the
inspection functions for querying the print request allow you to inspect
any parameter of the print request, *print-object* methods should only
need to call *print-length*. All other aspects of the print request are
handled by *print*. There is one exception which is described in `
Pretty printing <print.htm#86451>`_.

The *level* keyword controls how deep into a nested data structure to
print. The value *#f* indicates that there is no limit. The default,
*\*print-level\**, has no effect on recursive calls to *print*.
Recursive calls to *print* may change the value of *print-level*
explicitly, but *print* always uses a value to ensure the print request
formed by the first call to *print* is never exceeded. For example, if a
first call to *print* set the level to 5, and while at a depth of 3, a
recursive call specified a level of 4, the recursive call would only
descend 2 more levels, not 4.

The *length* keyword controls how many elements of a sequence to print
before printing ellipsis notation (*...*). The value *#f* indicates
that there is no limit. The *print-length* control can be interpreted
loosely by some *print-object* methods to control how many *elements* of
any kind of object to print; for example, the default ``<object>`` method
might regard *print-length* to determine how many slot-name/value pairs
to print. The default, *\*print-length\**, has no effect on recursive
calls to *print*. Recursive calls to *print* may change the value of
*print-length* explicitly, but they may only decrease the value, never
increase it.

The *circle?* keyword indicates whether printing should check all
subcomponent references to make sure the printing process does not
infinitely recurse through a data structure. Circular printing also tags
objects that occur more than once when they are first printed, and later
occurrences are printed as a reference to the previously emitted tag.
The default, *\*print-circle?\**, has no effect on recursive calls to
*print*. If *print-circle?* is already *#t*, then it remains *#t*
throughout all recursive calls. If *print-circle?* is *#f*, then
recursive calls to *print* can change the value to *#t* ; however, when
printing exits the dynamic scope of the call that changed the value to
*#t*, the value reverts back to *#f*. If the original call to *print*
specifies *circle?* as *#f*, and dynamically distinct recursive calls
turn circular printing on and off, all output generated while circular
printing was on shares the same tagging space; that is, if *#1#* is
printed twice, once from each of two distinct recursive calls to print,
then each *#1#* is guaranteed to signify the same *==* object.

The *pretty?* keyword indicates whether printing should attempt to
insert line breaks and indentation to format objects according to how
programmers tend to find it easier to read data. The default,
*\*print-pretty?\**, has no effect on recursive calls to *print*. If
*print-pretty?* is already *#t*, then it remains *#t* throughout all
recursive calls. If *print-pretty?* is *#f*, then recursive calls to
*print* can change the value to *#t* ; however, when printing exits the
dynamic scope of the call that changed the value to *#t*, the value
reverts back to *#f*.

print-to-string
'''''''''''''''

Function
        

print-to-string *object* #key *level length circle? pretty?* => *result*
                                                                        

Calls *print* to produce output according to the print request formed by
the keyword arguments and returns the result as a string. The *level*,
*length*, *circle?*, and *pretty?* keywords are as for *print*.

print-object
''''''''''''

Open generic function
                     

print-object *object stream* => ()
                                  

Prints an *object* to a *stream*. You should extend the ability of
*print* to print various objects by adding methods to the *print-object*
function. When *print* actually prints an object, it calls
*print-object*. You should never call *print-object* directly.

The Print module exports the following variables which provide default
values for calls to the print function. Their values are
implementation-dependent.

\*print-level\*
'''''''''''''''

Variable
        

This is an ``<integer>`` that controls how deeply into a nested expression
to print.

\*print-length\*
''''''''''''''''

Variable
        

This is an ``<integer>`` that controls how many elements at a given level
to print.

\*print-circle?\*
'''''''''''''''''

Variable
        

A boolean that controls whether or not to print recursively. When
*\*print-circle\** is *#f*, printing proceeds recursively and attempts
to print a circular structure results in failure to terminate.

\*print-pretty\*
''''''''''''''''

Variable
        

A boolean that controls whether or not print does pretty-printing.

Pretty printing
---------------

When writing *print-object* methods, you can ignore whether pretty
printing is in effect. If you write your *print-object* method using
pretty printing functions, then when pretty printing is in effect, the
output is pretty printed. When pretty printing is not in effect, your
method produces output as though you had not written it to use pretty
printing. All *print-object* methods that are written to do pretty
printing must call the pretty printing functions within the dynamic
scope of a call to *pprint-logical-block* ; otherwise, the pretty
printing functions are no-ops.

The following interfaces are exported from the *pprint* module:
                                                               

\*default-line-length\*
'''''''''''''''''''''''

Variable
        

An integer that controls the line length used by the pretty printer to
determine how much output will fit on a single line. The value must be
an integer. The default is 80.

\*print-miser-width\*
'''''''''''''''''''''

Variable
        

An integer that controls *miser mode*. Whenever a logical block (see
*pprint-logical-block*) begins in a column of output that is greater
than *\*default-line-length\* - \*print-miser-width\**, then pretty
printing is in miser mode. The value must be an integer or *#f* (the
default). *#f* indicates that the pretty printer should never enter
miser mode.

pprint-logical-block
''''''''''''''''''''

Function
        

pprint-logical-block *stream* #key *prefix per-line-prefix body suffix
column* => ()
                                                                                    

Groups printing into a logical block. The logical block provides
boundaries for new levels of indentation, affects *#"linear"* newlines,
and so on. The *prefix* keyword is a string to print at the beginning of
the logical block. The blocks indentation is automatically set to be one
character position greater than the column in which *prefix* ends.
Alternatively, *per-line-prefix* is a string to print on every line of
the logical block. The *pprint-logical-block* function signals an error
if it is called with both *prefix* and *per-line-prefix* supplied as
non-*#f*.

The *suffix* keyword is a string to print at the end of the logical
block.

The *column* keyword advises the pretty printer as to the current column
of the output stream (the default is zero). This keyword may be ignored
entirely by some methods, and it may be ignored in some cases by methods
that can better determine the stream’s current output column.

The *body* keyword must be a function that can take one argument, and
this argument is a stream. The function specified by *body* should use
the *stream* argument passed to it; the *body* function should not close
over the *stream* argument to *pprint-logical-block*. The function
*pprint-logical-block* wraps *stream* with a pretty printing stream when
*stream* is any other kind of stream. If *stream* is already a pretty
printing stream, then the *body* function is called on *stream*.

All *print-object* methods that are written to do pretty printing must
call the other pretty printing functions within the dynamic scope of a
call to *pprint-logical-block* ; otherwise, the pretty printing
functions are no-ops.

pprint-newline
''''''''''''''

Function
        

pprint-newline *kind stream* => ()
                                  

Announces a conditional newline to the pretty printer. The pretty
printer emits a newline depending on the *kind* and the state of the
pretty printer’s current line buffer. The *kind* argument can be one of
the following:

-  *#"fill"* Emit a newline if the current *section* of output does not
   fit on one line.
-  *#"linear"* Emit a newline if any *#"linear"* newline in the current
   *section* needs to be emitted. That is, if a current *section* of
   output cannot fit on one line, and any one of the *#"linear"*
   newlines in the section needs to be emitted, then emit them all.
-  *#"miser"* Emit a newline as if it were a *#"linear"* newline, but
   only when *miser* *mode* is in effect. Miser style is in effect when
   a logical block starts past a particular column of output.
-  *#"mandatory"* Emit a newline always. Establish that any containing
   *sections* cannot be printed on a single line so that *#"linear"* and
   *#"miser"* newlines will be emitted as appropriate.

pprint-indent
'''''''''''''

Function
        

pprint-indent *relative-to n stream* => ()
                                          

Specifies the indentation to use within the current logical block. When
*relative-to* is *#"block"*, then *pprint-indent* sets the indentation
to the column of the first character of the logical block plus *n*.
When *relative-to* is *#"current"*, then *pprint-indent* sets the
indentation to the current column plus *n*. In both cases, *n* is a
``<fixed-integer>``.

pprint-tab
''''''''''

Function
        

pprint-tab *kind colnum colinc stream* => ()
                                            

-  *kind* One of *#"line"*, *#"line-relative"*, *#"section"*,
   *#"section-relative"*.
-  *colnum* An instance of ``<fixed-integer>``.
-  *colinc* An instance of ``<fixed-integer>``.
-  *stream* An instance of ``<stream>``.

Announces a tab to the pretty printer. *Colnum* and *colinc* have
meaning based on the value of *kind*, which can be one of the
following:

-  *#"line"* Tab to output column *colnum*. If the output is already at
   or beyond *colnum*, then add *colinc* to *colnum* until printing can
   continue at a column beyond the end of the output already on the
   line.

*#"line-relative"*
                  

-  Output *colnum* spaces. Then output enough spaces to tab to a column
   that is a multiple of *colinc* from the beginning of the line.
-  *#"section"* This is similar to *#"line"*, but column counting is
   relative to the beginning of the current *section* rather than the
   beginning of the line.

*#"section-relative"*
                     

-  This is similar to *#"line-relative"*, but column counting is
   relative to the beginning of the current *section* rather than the
   beginning of the line.

In all cases, *colnum* and *colinc* are instances of ``<fixed-integer>``.

The PRINT module
----------------

This section contains a reference entry for each item exported from the
IO library’s *print* module.

print
~~~~~

Function
^^^^^^^^

Summary
       

Prints *object* to the specified stream.

Signature
         

print *object* *stream* #key *level length circle? pretty?* => ()
                                                                 

Arguments
         

-  object An instance of ``<object>``.
-  *stream* An instance of ``<stream>``.
-  *level* *#f* or an instance of ``<fixed-integer>``. Default value:
   *\*print-level\**.
-  *length* *#f* or an instance of ``<fixed-integer>``. Default value:
   *\*print-length\**.
-  *circle?* An instance of ``<boolean>``. Default value:
   *\*print-circle?\**.
-  *pretty?* An instance of ``<boolean>``. Default value:
   *\*print-pretty?\**.

Values
      

-  None.

Description
           

Prints *object* to *stream* according to the print request formed by the
keyword arguments. A first call to *print* creates a printing stream to
represent the print request, and recursive calls to *print* on this
printing stream process the keyword arguments differently (see below).
There are inspection functions for querying the print request. When
*print* actually prints an object, it calls *print-object*. Though the
inspection functions for querying the print request allow you to inspect
any parameter of the print request, *print-object* methods should only
need to call *print-length*. All other aspects of the print request are
handled by *print*. There is one exception, which is described in `
Pretty printing <print.htm#86451>`_.

The *level* keyword controls how deep into a nested data structure to
print. The value *#f* indicates that there is no limit. The default,
*\*print-level\**, has no effect on recursive calls to *print*.
Recursive calls to *print* may change the value of *print-level*
explicitly, but *print* always uses a value to ensure the print request
formed by the first call to *print* is never exceeded. For example, if a
first call to *print* set the level to 5, and while at a depth of 3, a
recursive call specified a level of 4, the recursive call would only
descend 2 more levels, not 4.

The *length* keyword controls how many elements of a sequence to print
before printing ellipsis notation (*...*). The value *#f* indicates
that there is no limit. The *print-length* control can be interpreted
loosely by some *print-object* methods to control how many *elements* of
any kind of object to print; for example, the default ``<object>`` method
might regard *print-length* to determine how many slot-name/value pairs
to print. The default, *\*print-length\**, has no effect on recursive
calls to *print*. Recursive calls to *print* may change the value of
*print-length* explicitly, but they may only decrease the value, never
increase it.

The *circle?* keyword indicates whether printing should check all
subcomponent references to make sure the printing process does not
infinitely recurse through a data structure. Circular printing also tags
objects that occur more than once when they are first printed, and later
occurrences are printed as a reference to the previously emitted tag.
The default, *\*print-circle?\**, has no effect on recursive calls to
*print*. If *print-circle?* is already *#t*, then it remains *#t*
throughout all recursive calls. If *print-circle?* is *#f*, then
recursive calls to *print* can change the value to *#t* ; however, when
printing exits the dynamic scope of the call that changed the value to
*#t*, the value reverts back to *#f*. If the original call to *print*
specifies *circle?* as *#f*, and dynamically distinct recursive calls
turn circular printing on and off, all output generated while circular
printing was on shares the same tagging space; that is, if *#1#* is
printed twice, once from each of two distinct recursive calls to print,
then each *#1#* is guaranteed to signify the same *==* object.

The *pretty?* keyword indicates whether printing should attempt to
insert line breaks and indentation to format objects according to how
programmers tend to find it easier to read data. The default,
*\*print-pretty?\**, has no effect on recursive calls to *print*. If
*print-pretty?* is already *#t*, then it remains *#t* throughout all
recursive calls. If *print-pretty?* is *#f*, then recursive calls to
*print* can change the value to *#t* ; however, when printing exits the
dynamic scope of the call that changed the value to *#t*, the value
reverts back to *#f*.

\*print-circle?\*
~~~~~~~~~~~~~~~~~

Variable
^^^^^^^^

Summary
       

Controls whether or not to print recursively.

Type
    

<boolean>
         

Initial value
             

None.

Description
           

Controls whether or not to print recursively. When *\*print-circle\** is
*#f*, printing proceeds recursively and attempts to print a circular
structure results in failure to terminate.

\*print-length\*
~~~~~~~~~~~~~~~~

Variable
^^^^^^^^

Summary
       

Controls the number of elements of an expression to print.

Type
    

false-or(<integer>)
                   

Initial value
             

None.

Description
           

Controls how many elements to print at a given level of a nested
expression.

\*print-level\*
~~~~~~~~~~~~~~~

Variable
^^^^^^^^

Summary
       

Controls how deeply into a nested expression to print.

Type
    

false-or(<integer>)
                   

Initial value
             

None.

Description
           

Controls how many levels of a nested expression to print.

print-object
~~~~~~~~~~~~

Open generic function
^^^^^^^^^^^^^^^^^^^^^

Summary
       

Prints an object to a stream.

Signature
         

print-object *object stream* => ()
                                  

Arguments
         

-  *object* An instance of ``<object>``.
-  *stream* An instance of ``<stream>``.

Values
      

-  None.

Description
           

Prints an object to a stream. You should extend the ability of *print*
to print various objects by adding methods to the *print-object*
function. When *print* actually prints an object, it calls
*print-object*. You should never call *print-object* directly.

\*print-pretty\*
~~~~~~~~~~~~~~~~

Variable
^^^^^^^^

Summary
       

Controls whether or not pretty printing is used.

Type
    

<boolean>
         

Initial value
             

None.

Description
           

Controls whether or not *print* does pretty printing.

print-to-string
~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Calls *print* on *object.* and returns the result as a string*.*

Signature
         

print-to-string *object* #key *level length circle? pretty?* => *result*
                                                                        

Arguments
         

-  *object* An instance of ``<object>``.
-  *level* *#f* or an instance of ``<fixed-integer>``. Default value:
   *\*print-level\**.
-  *length* *#f* or an instance of ``<fixed-integer>``. Default value:
   *\*print-length\*.*
-  circle? An instance of ``<boolean>``. Default value:
   *\*print-circle?\**.
-  *pretty?* An instance of ``<boolean>``. Default value:
   *\*print-pretty?\*.*

Values
      

-  *result* An instance of ``<byte-string>``.

Description
           

Calls *print* to produce output according to the print request formed by
the keyword arguments and returns the result as a string.

The PPRINT module
-----------------

This section contains a reference entry for each item exported from the
IO library’s *pprint* module.

\*default-line-length\*
~~~~~~~~~~~~~~~~~~~~~~~

Variable
^^^^^^^^

Summary
       

Controls the default line length used by the pretty printer.

Type
    

<integer>
         

Initial value
             

80

Description
           

Controls the line length used by the pretty printer to determine how
much output will fit on a single line. The value must be an integer.

pprint-indent
~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Specifies the indentation to use within the current logical block.

Signature
         

pprint-indent *relative-to n stream* => ()
                                          

Arguments
         

-  *relative-to* One of *#"block"* or *#"current"*.
-  *n* An instance of ``<fixed-integer>``.
-  *stream* An instance of ``<stream>``.

Values
      

-  None.

Description
           

Specifies the indentation to use within the current logical block. When
*relative-to* is *#"block"*, then *pprint-indent* sets the indentation
to the column of the first character of the logical block plus *n*.
When *relative-to* is *#"current"*, then *pprint-indent* sets the
indentation to the current column plus *n*.

pprint-logical-block
~~~~~~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Groups printing into a logical block.

Signature
         

pprint-logical-block *stream* #key *prefix per-line-prefix body
 suffix column* => ()
                                                               

Arguments
         

-  *stream* An instance of ``<stream>``.
-  *prefix* *#f* or an instance of ``<byte-string>``.
-  *per-line-prefix* *#f* or an instance of ``<byte-string>``.
-  *body* An instance of ``<function>``.
-  *suffix* *#f* or an instance of ``<byte-string>``.
-  *column* A *limited* instance of ``<fixed-integer>``, minimum 0.

Values
      

-  None.

Description
           

Groups printing into a logical block. The logical block provides
boundaries for new levels of indentation, affects *#"linear"* newlines,
and so on. *Prefix* is a string to print at the beginning of the logical
block. The blocks indentation is automatically set to be one character
position greater than the column in which *prefix* ends. Alternatively,
*per-line-prefix* is a string to print on every line of the logical
block. This function signals an error if it is called with both *prefix*
and *per-line-prefix* supplied as non-*#f*. *Suffix* is a string to
print at the end of the logical block. *Column* advises the pretty
printer as to the current column of the output stream (the default is
zero). The *column* argument may be ignored entirely by some methods,
and it may be ignored in some cases by methods that can better determine
the stream’s current output column.

The *body* keyword must be a function that can take one argument, and
this argument is a stream. The *body* function should use the stream
argument passed to it; the *body* function should not close over the
stream argument to *pprint-logical-block*. *Pprint-logical-block* wraps
*stream* with a pretty printing stream when *stream* is any other kind
of stream. If *stream* is already a pretty printing stream, then the
*body* function is called on *stream*.

All *print-object* methods that are written to do pretty printing must
call the other pretty printing functions within the dynamic scope of a
call to *pprint-logical-block* ; otherwise, the pretty printing
functions are no-ops.

pprint-newline
~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Announces a conditional newline to the pretty printer.

Signature
         

pprint-newline *kind stream* => ()
                                  

Arguments
         

-  *kind* One of *#"fill"*, *#"linear"*, *#"miser"*, *#"mandatory"*.
-  *stream* An instance of ``<stream>``.

Values
      

-  None.

Description
           

Announces a conditional newline to the pretty printer. The pretty
printer emits a newline depending on the *kind* and the state of the
pretty printer’s current line buffer. The *kind* argument has roughly
the following meanings:

-  *#"fill"* Emit a newline if the current *section* of output does not
   fit on one line.
-  *#"linear"* Emit a newline if any *#"linear"* newline in the current
   *section* needs to be emitted. That is, if a current *section* of
   output cannot fit on one line, and any one of the *#"linear"*
   newlines in the section needs to be emitted, then emit them all.
-  *#"miser"* Emit a newline as if it were a *#"linear"* newline, but
   only when *miser* *mode* is in effect. Miser style is in effect when
   a logical block starts past a particular column of output.
-  *#"mandatory"* Emit a newline always. Establish that any containing
   *sections* cannot be printed on a single line so that *#"linear"* and
   *#"miser"* newlines will be emitted as appropriate.

pprint-tab
~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Announces a tab to the pretty printer.

Signature
         

pprint-tab *kind colnum colinc stream* => ()
                                            

Arguments
         

-  *kind* One of *#"line"*, *#"line-relative"*, *#"section"*,
   *#"section-relative"*.
-  *colnum* An instance of ``<fixed-integer>``.
-  *colinc* An instance of ``<fixed-integer>``.
-  *stream* An instance of ``<stream>``.

Values
      

-  None.

Description
           

Announces a tab to the pretty printer. The *colnum* and *colinc*
arguments have meaning based on the value of *kind*:

-  *#"line"* Tab to output column *colnum*. If the output is already at
   or beyond *colnum*, then add *colinc* to *colnum* until printing can
   continue at a column beyond the end of the output already on the
   line.

*#"line-relative"*
                  

-  Output *colnum* spaces. Then output enough spaces to tab to a column
   that is a multiple of *colinc* from the beginning of the line.
-  *#"section"* Similar to *#"line"*, but column counting is relative
   to the beginning of the current *section* rather than the beginning
   of the line.

*#"section-relative"*
                     

-  Similar to *#"line-relative"*, but column counting is relative to
   the beginning of the current *section* rather than the beginning of
   the line.

\*print-miser-width\*
~~~~~~~~~~~~~~~~~~~~~

Variable
^^^^^^^^

Summary
       

Controls miser mode.

Type
    

false-or(<integer>)
                   

Initial value
             

None.

Description
           

Controls *miser mode*. Pretty printing is in miser mode whenever a
logical block (see *pprint-logical-block*) begins in a column of output
that is greater than

\*default-line-length\* - \*print-miser-width\*
                                               

The value must be an integer or *#f* (the default); *#f* indicates that
the pretty printer should never enter miser mode.


