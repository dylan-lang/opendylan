**********************
The File-System Module
**********************

Introduction
------------

The File-System module is part of the System library and provides a
generic interface to the file system of the local machine. Remotely
mounted file systems are also accessible using this module.

This chapter describes the functions and types that the File-System
module contains.

Types specific to file system operations
----------------------------------------

The File-System module contains a number of types specifically designed
for use by interfaces in the module.

Firstly, the type *<file-type>* represents the types of entity that may
be present on a given file system. Three entities are allowed: a file, a
directory, and a link to a file or directory located elsewhere in the
file system. Together, these represent any entity that can be placed on
a file system mounted on the local machine. The *<file-type>* type is
defined as

one-of(#"file", #"directory", #"link")
                                      

The type *<pathname>* represents the set of classes that may be used to
represent pathnames that indicate entities on the file system. It is
defined as a type alias of *<string>*.

Lastly, the type *<copy/rename-disposition>* represents the possible
values of the *if-exists:* keyword to the functions *rename-file* and
*copy-file* described in `Manipulating
files <file-system.htm#92027>`_. It is defined as

one-of (#"signal", #"replace")
                              

If the value of the keyword for either function is *#"signal"* (the
default for both functions), then you are prompted before an existing
file is overwritten as a result of a copy or rename operation. If
*#"replace"*, then an existing file is overwritten without prompting.

Manipulating files
------------------

The File-System module contains a number of interfaces that let you
perform standard file management operations on files already resident on
the filesystem. You can rename, copy, or delete any file, and you can
set any available properties for the file.

delete-file
'''''''''''

Function
        

delete-file *file* => ()
                        

Use this function to delete a specified file. If *file* is actually a
link, then the link is deleted, rather than the file it points to.

rename-file
'''''''''''

Function
        

copy-file
'''''''''

Function
        

rename-file *old-file* *new-file* #key *if-exists* => ()
                                                        

copy-file *old-file* *new-file* #key *if-exists* => ()
                                                      

Renames or copies *old-file* to *new-file*.

For both functions, if *new-file* already exists, then the behavior
depends on the value of *if-exists*, which is an instance of `
<copy/rename-disposition> <file-system.htm#73023>`_. The default
behavior is to prompt you before overwriting a file.

file-property-setter
''''''''''''''''''''

Sealed generic function
                       

file-property-setter *new-value* *file* *key* => *new-value*
                                                            

Sets a property of *file* to *new-value*. The property that is set is
specified by the value of *key*, which must be one of the following:

#"author" #"size" #"creation-date" #"access-date" #"modification-date"
#"write-date" #"readable?" #"writeable?" #"executable?"
                                                                                                                              

The type of *new-value* (and hence the type of the return value of
*file-property-setter*), is determined by the value of *key*. For
example, if *key* is *#"readable?"*, then *new-value* should be an
instance of *<boolean>*. For full details, see `
file-property-setter <file-system.htm#16395>`_.

Manipulating directories
------------------------

The File-System module contains a number of interfaces that let you
create and delete directories. These can be used in conjunction with the
file manipulation operations described in `Manipulating
files <file-system.htm#92027>`_ to perform file management tasks at any
position in the file system.

create-directory
''''''''''''''''

Function
        

create-directory *parent* *name* => *directory*
                                               

Creates a new directory *name* in the directory *parent*. The full
pathname of the directory is returned, and you can use this return value
in conjunction with *concatenate* to create pathnames of any entities
that you create in the new directory.

delete-directory
''''''''''''''''

Function
        

delete-directory *directory* => ()
                                  

Deletes a directory specified by *directory*. Whether or not the
directory needs to be empty before it can be deleted is determined by
the platform on which you are running.

ensure-directories-exist
''''''''''''''''''''''''

Function
        

ensure-directories-exist *file* => *created?*
                                             

Use this function when you want to guarantee that a particular directory
structure has been created on disk. It ensures that the individual
directories that constitute the pathname specified by *file* exist, and
creates any that do not. If *ensure-directories-exist* actually creates
any directories, then *#t* is returned.

do-directory
''''''''''''

Function
        

do-directory *function* *directory* => ()
                                         

Performs *function* once for every item in the specified *directory*.
The *function* must have the following signature:

*function* *directory* *name* *type* => ()
                                          

where *directory* is the name of the directory specified to
*do-directory*, *name* is an instance of *<byte-string>*, and *type*
is an instance of `<file-type> <file-system.htm#48717>`_.

Within *function*, you can concatenate the values of *directory* and
*name* to generate a `<pathname> <file-system.htm#95733>`_ suitable
for use by the other functions in the File-system module.

working-directory-setter
''''''''''''''''''''''''

Function
        

working-directory-setter *directory* => *directory*
                                                   

Sets the working directory for the current process.

Finding out file system information
-----------------------------------

A number of functions return environment information regarding the
directory structure of the file system. Each function takes no
arguments, and returns a pathname or list of pathnames. The return
values can be use in conjunction with other functions to perform
file-based operations relative to the directories involved.

home-directory
''''''''''''''

Function
        

home-directory () => *home-directory*
                                     

Returns the *<pathname>* of the current value of the home directory. You
can use the return value of *home-directory* in conjunction with
*concatenate* to specify the pathname of any entity in the home
directory.

root-directories
''''''''''''''''

Function
        

root-directories () => *roots*
                              

Returns a sequence containing the pathnames of the root directories of
all the file systems connected to the local machine.

temp-directory
''''''''''''''

Function
        

temp-directory () => *temp-directory*
                                     

Returns the *<pathname>* of the temporary directory in use on the local
machine. If no temporary directory is defined, this function returns
false. You can use the return value of *temp-directory* in conjunction
with *concatenate* to specify pathnames of entities in the temporary
directory.

working-directory
'''''''''''''''''

Function
        

working-directory () => *working-directory*
                                           

Returns the *<pathname>* of the current working directory in the current
process on the local machine. You can use the return value of
*working-directory* in conjunction with *concatenate* to specify
pathnames of entities in the working directory.

Finding out file information
----------------------------

Several interfaces in the File-System module allow you to interrogate
files for information. You can find out whether a file exists, what its
name is, or which directory it resides in, and you can find the current
properties of the file.

file-exists?
''''''''''''

Function
        

file-exists? *file* => *exists?*
                                

Returns true if *file* exists, false otherwise. If *file* is actually a
link, then *file-exists* checks the target of the link, and returns true
if the target exists.

file-properties
'''''''''''''''

Function
        

file-properties *file* => *properties*
                                      

Returns all the properties of the specified file. The properties are
returned as a concrete subclass of *<explicit-key-collection>*.

file-property
'''''''''''''

Sealed generic function
                       

file-property *file* *key* => *property*
                                        

Returns a particular property of the specified file. The property
returned is dependent on the value of *key*, and as such, may be of a
number of types. For more information about the possible values of *key*,
see `file-property-setter <file-system.htm#16395>`_.

file-type
'''''''''

Function
        

file-type *file* => *file-type*
                               

Returns the file type of the entity specified by *file*, as an instance
of *<file-type>*. A given entity can either be a file, a directory, or
a link to a file or directory.

The FILE-SYSTEM module
----------------------

This section contains a reference entry for each item included in the
File-System module.

copy-file
~~~~~~~~~

Function
^^^^^^^^

Summary
       

Creates a copy of a file.

Signature
         

copy-file *old-file* *new-file* #key *if-exists* => ()
                                                      

Arguments
         

-  *old-file* An instance of `<pathname> <file-system.htm#95733>`_.
-  *new-file* An instance of `<pathname> <file-system.htm#95733>`_.
-  *if-exists* An instance of
    `<copy/rename-disposition> <file-system.htm#73023>`_. Default
   value: *#"signal"*.

Values
      

-  None

Description
           

Copies *old-file* to *new-file*. If *new-file* already exists, the
action of this function is controlled by the value of *if-exists*. The
default is to prompt you before overwriting an existing file.

See also
        

`<copy/rename-disposition> <file-system.htm#73023>`_

`rename-file <file-system.htm#56627>`_

<copy/rename-disposition>
~~~~~~~~~~~~~~~~~~~~~~~~~

Type
^^^^

Summary
       

The type that represents possible actions when overwriting existing
files.

Equivalent
          

one-of(#"signal", #"replace")
                             

Supertypes
          

None.

Init-keywords
             

-  None.

Description
           

This type represents the acceptable values for the *if-exists:* argument
to the `copy-file <file-system.htm#62557>`_ and `
rename-file <file-system.htm#56627>`_ functions. Only two values are
acceptable:

-  If *#"signal"* is used, then you are warned before a file is
   overwritten during a copy or move operation.
-  If *#"replace"* is used, then you are not warned before a file is
   overwritten during a copy or move operation.

Operations
          

`copy-file <file-system.htm#62557>`_ `
rename-file <file-system.htm#56627>`_

See also
        

`copy-file <file-system.htm#62557>`_

`rename-file <file-system.htm#56627>`_

create-directory
~~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Creates a new directory in the specified parent directory.

Signature
         

create-directory *parent* *name* => *directory*
                                               

Arguments
         

-  *parent* An instance of `<pathname> <file-system.htm#95733>`_.
-  *name* An instance of *<string>*.

Values
      

-  *directory* An instance of `<pathname> <file-system.htm#95733>`_.

Description
           

Creates *directory* in the specified *parent* directory. The return
value of this function can be used with *concatenate* to create
pathnames of entities in the new directory.

See also
        

`delete-directory <file-system.htm#20176>`_

delete-directory
~~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Deletes the specified directory.

Signature
         

delete-directory *directory* => ()
                                  

Arguments
         

-  *directory* An instance of `<pathname> <file-system.htm#95733>`_.

Values
      

-  None.

Description
           

Deletes the specified directory. Whether or not the directory must be
empty before it can be deleted is platform dependent.

See also
        

`create-directory <file-system.htm#25429>`_

`delete-file <file-system.htm#77765>`_

delete-file
~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Deletes the specified file system entity.

Signature
         

delete-file *file* => ()
                        

Arguments
         

-  *file* An instance of `<pathname> <file-system.htm#95733>`_.

Values
      

-  None.

Description
           

Deletes the file system entity specified by *file*. If *file* refers to
a link, the link is removed, but the actual file that the link points to
is not removed.

do-directory
~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Executes the supplied function once for each entity in the specified
directory.

Signature
         

do-directory *function* *directory* => ()
                                         

Arguments
         

-  *function* An instance of *<function>*.
-  *directory* An instance of `<pathname> <file-system.htm#95733>`_.

Values
      

-  None.

Description
           

Executes *function* once for each entity in *directory*.

The signature of *function* is

*function* *directory* *name* *type* => ()
                                          

where *directory* is an instance of `
<pathname> <file-system.htm#95733>`_, *name* is an instance of
*<byte-string>*, and *type* is an instance of `
<file-type> <file-system.htm#48717>`_.

Within *function*, the values of *directory* and *name* can be
concatenated to generate a `<pathname> <file-system.htm#95733>`_
suitable for use by the other functions in the module.

The following calls are equivalent

do-directory(my-function, "C:\\USERS\\JOHN\\FOO.TEXT")
                                                      

do-directory(my-function, "C:\\USERS\\JOHN\\")
                                              

as they both operate on the contents of *C:\\USERS\\JOHN*. The call

do-directory(my-function, "C:\\USERS\\JOHN")
                                            

is not equivalent as it will operate on the contents of *C:\\USERS*.

ensure-directories-exist
~~~~~~~~~~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Ensures that all the directories in the pathname leading to a file
exist, creating any that do not, as needed.

Signature
         

ensure-directories-exist *file* => *created?*
                                             

Arguments
         

-  *file* An instance of `<pathname> <file-system.htm#95733>`_.

Values
      

-  *created?* An instance of *<boolean>*.

Description
           

Ensures that all the directories in the pathname leading to a file
exist, creating any that do not, as needed. The return value indicates
whether or not any directory was created.

The following calls are equivalent

ensure-directories-exist("C:\\USERS\\JOHN\\FOO.TEXT")
                                                     

ensure-directories-exist("C:\\USERS\\JOHN\\")
                                             

as they will both create the directories *USERS* and *JOHN* if needed.
The call

ensure-directories-exist("C:\\USERS\\JOHN")
                                           

is not equivalent as it will only create *USERS* if needed.

Example
       

ensure-directories-exist("C:\\USERS\\JOHN\\FOO.TEXT")
                                                     

See also
        

`create-directory <file-system.htm#25429>`_

file-exists?
~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Returns *#t* if the specified file exists.

Signature
         

file-exists? *file* => *exists?*
                                

Arguments
         

-  *file* An instance of `<pathname> <file-system.htm#95733>`_.

Values
      

-  *exists?* An instance of *<boolean>*.

Description
           

Returns *#t* if *file* exists. If it refers to a link, the target of the
link is checked.

file-properties
~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Returns all the properties of a file system entity.

Signature
         

file-properties *file* => *properties*
                                      

Arguments
         

-  *file* An instance of `<pathname> <file-system.htm#95733>`_.

Values
      

-  *properties* An instance of a concrete subclass of
   *<explicit-key-collection>*.

Description
           

Returns all the properties of *file*. The keys to the properties
collection are the same as those use by `
file-property <file-system.htm#47498>`_, above.

Example
       

file-properties() [#"size"]
                           

See also
        

`file-property <file-system.htm#47498>`_

`file-property-setter <file-system.htm#16395>`_

file-property
~~~~~~~~~~~~~

Sealed generic function
^^^^^^^^^^^^^^^^^^^^^^^

Summary
       

Returns the specified property of a file system entity.

Signature
         

file-property *file* #key *key* => *property*
                                             

Arguments
         

-  *file* An instance of `<pathname> <file-system.htm#95733>`_.
-  *key* One of *#"author"*, *#"size"*, *#"creation-date"*,
   *#"access-date"*, *#"modification-date"*, *#"write-date"*,
   *#"readable?"*, *#"writeable?"*, *#"executable?"*.

Values
      

-  *property* The value of the property specified by *key*. The type of
   the value returned depends on the value of *key*: see the
   description for details.

Description
           

Returns the property of *file* specified by *key*. The value returned
depends on the value of *key*, as shown in Table `Return value
types of file-property <file-system.htm#32600>`_.

Return value types of *file-property*
                                     
+------------------------+-------------------------------+
| Value of *key*         | Type of return value          |
+========================+===============================+
| *#"author"*            | *false-or(<string>)*          |
+------------------------+-------------------------------+
| *#"size"*              | *<integer>*                   |
+------------------------+-------------------------------+
| *#"creation-date"*     | `<date> <date.htm#54319>`_    |
+------------------------+-------------------------------+
| *#"access-date"*       | `<date> <date.htm#54319>`_    |
+------------------------+-------------------------------+
| *#"modification-date"* | `<date> <date.htm#54319>`_    |
+------------------------+-------------------------------+
| *#"write-date"*        | `<date> <date.htm#54319>`_    |
+------------------------+-------------------------------+
| *#"readable?"*         | *<boolean>*                   |
+------------------------+-------------------------------+
| *#"writeable?"*        | *<boolean>*                   |
+------------------------+-------------------------------+
| *#"executable?"*       | *<boolean>*                   |
+------------------------+-------------------------------+

Not all platforms implement all of the above keys. Some platforms may
support additional keys. The *#"author"* key is supported on all
platforms but may return *#f* if it is not meaningful on a given
platform. The *#"modification-date"* and *#"write-date"* keys are
identical. Use of an unsupported key signals an error.

All keys listed above are implemented by Win32, though note that
*#"author"* always returns *#f*.

See also
        

`file-property-setter <file-system.htm#16395>`_

`file-properties <file-system.htm#71762>`_

file-property-setter
~~~~~~~~~~~~~~~~~~~~

Sealed generic function
^^^^^^^^^^^^^^^^^^^^^^^

Summary
       

Sets the specified property of a file system entity to a given value.

Signature
         

file-property-setter *new-value* *file* *key* => *new-value*
                                                            

Arguments
         

-  *new-value* The type of this depends on the value of *key*. See the
   description for details.
-  *file* An instance of `<pathname> <file-system.htm#95733>`_.
-  *key* One of *#"author"*, *#"size"*, *#"creation-date"*,
   *#"access-date"*, *#"modification-date"*, *#"write-date"*,
   *#"readable?"*, *#"writeable?"*, *#"executable?"*.

Values
      

-  *new-value* The type of this depends on the value of *key*. See the
   description for details.

Description
           

Sets the property of *file* specified by *key* to *new-value*. The type
of *new-value* depends on the property specified by key, as shown in
Table `New value types of
file-property-setter <file-system.htm#73198>`_ below.

New value types of *file-property-setter*
                                            
+------------------------+-------------------------------+
| Value of *key*         | Type of *new-value*           |
+========================+===============================+
| *#"author"*            | *false-or(<string>)*          |
+------------------------+-------------------------------+
| *#"size"*              | *<integer>*                   |
+------------------------+-------------------------------+
| *#"creation-date"*     | `<date> <date.htm#54319>`_    |
+------------------------+-------------------------------+
| *#"access-date"*       | `<date> <date.htm#54319>`_    |
+------------------------+-------------------------------+
| *#"modification-date"* | `<date> <date.htm#54319>`_    |
+------------------------+-------------------------------+
| *#"write-date"*        | `<date> <date.htm#54319>`_    |
+------------------------+-------------------------------+
| *#"readable?"*         | *<boolean>*                   |
+------------------------+-------------------------------+
| *#"writeable?"*        | *<boolean>*                   |
+------------------------+-------------------------------+
| *#"executable?"*       | *<boolean>*                   |
+------------------------+-------------------------------+

Note that *file-property-setter* returns the value that was set, and so
return values have the same types as specified values, depending on the
value of *key*.

Not all platforms implement all of the above keys. Some platforms may
support additional keys. The *#"modification-date"* and *#"write-date"*
keys are identical. Use of an unsupported key signals an error.

The only property that can be set on Win32 is *#"writeable?"*.

See also
        

`file-property <file-system.htm#47498>`_

`file-properties <file-system.htm#71762>`_

<file-system-error>
~~~~~~~~~~~~~~~~~~~

Error
^^^^^

Summary
       

Error type signaled when any other functions in the File-System module
signal an error.

Superclasses
            

*<error>* and *<simple-condition>*

Init-keywords
             

Description
           

Signalled when one of the file system functions triggers an error, such
as a permissions error when trying to delete or rename a file.

Operations
          

None.

file-type
~~~~~~~~~

Function
^^^^^^^^

Summary
       

Returns the type of the specified file system entity.

Signature
         

file-type *file* => *file-type*
                               

Arguments
         

-  *file* An instance of `<pathname> <file-system.htm#95733>`_.

Values
      

-  *file-type* An instance of `
   <file-type> <file-system.htm#48717>`_.

Description
           

Returns the type of *file*, the specified file system entity. A file
system entity can either be a file, a directory, or a link to another
file or directory.

<file-type>
~~~~~~~~~~~

Type
^^^^

Summary
       

The type representing all possible types of a file system entity.

Equivalent
          

one-of(#"file", #"directory", #"link")
                                      

Supertypes
          

None.

Init-keywords
             

-  None.

Description
           

The type representing all possible types of a file system entity. An
entity on the file system can either be a file, a directory or folder,
or a link to another file or directory. The precise terminology used to
refer to these different types of entity depends on the operating system
you are working in.

Operations
          

`do-directory <file-system.htm#12243>`_

home-directory
~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Returns the current value of the home directory.

Signature
         

home-directory () => *home-directory*
                                     

Arguments
         

-  None.

Values
      

-  *home-directory* An instance of `
   <pathname> <file-system.htm#95733>`_.

Description
           

Returns the current value of the home directory. The return value of
this function can be used with concatenate to create pathnames of
entities in the home directory.

<pathname>
~~~~~~~~~~

Type
^^^^

Summary
       

The type representing a file system entity.

Equivalent
          

<string>
        

Supertypes
          

None.

Init-keywords
             

-  None.

Description
           

A type that identifies a file system entity.

Operations
          

`copy-file <file-system.htm#62557>`_ `
create-directory <file-system.htm#25429>`_ `
delete-directory <file-system.htm#20176>`_
 `delete-file <file-system.htm#77765>`_ `
do-directory <file-system.htm#12243>`_ `
ensure-directories-exist <file-system.htm#50234>`_ `
file-exists? <file-system.htm#55287>`_ `
file-properties <file-system.htm#71762>`_ `
file-property <file-system.htm#47498>`_
 `file-property-setter <file-system.htm#16395>`_ `
file-type <file-system.htm#12684>`_ `
home-directory <file-system.htm#73621>`_
 `rename-file <file-system.htm#56627>`_

rename-file
~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Renames a specified file.

Signature
         

rename-file *old-file* *new-file* #key *if-exists* => ()
                                                        

Arguments
         

-  *old-file* An instance of `<pathname> <file-system.htm#95733>`_.
-  *new-file* An instance of `<pathname> <file-system.htm#95733>`_.
-  *if-exists* An instance of
    `<copy/rename-disposition> <file-system.htm#73023>`_. Default
   value: *#"signal"*.

Values
      

-  None

Description
           

Renames *old-file* to *new-file*. If *new-file* already exists, the
action of this function is controlled by the value of *if-exists*. The
default is to prompt you before overwriting an existing file.

This operation may fail if the source and destination are not on the
same file system.

See also
        

`copy-file <file-system.htm#62557>`_

`<copy/rename-disposition> <file-system.htm#73023>`_

root-directories
~~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Returns a sequence containing the pathnames of the root directories of
the file systems on the local machine.

Signature
         

root-directories () => *roots*
                              

Arguments
         

-  None.

Values
      

-  *roots* An instances of *<sequence>*.

Description
           

Returns a sequence containing the pathnames of the root directories of
the file systems on the local machine.

temp-directory
~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Returns the pathname of the temporary directory in use.

Signature
         

temp-directory () => *temp-directory*
                                     

Arguments
         

-  None.

Values
      

-  *temp-directory* An instance of `
   <pathname> <file-system.htm#95733>`_, or false.

Description
           

Returns the pathname of the temporary directory in use. The return value
of this function can be used with *concatenate* to create pathnames of
entities in the temporary directory. If no temporary directory is
defined, *temp-directory* returns *#f*. On Windows the temporary
directory is specified by the *TMP* environment variable.

working-directory
~~~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Returns the working directory for the current process.

Signature
         

working-directory () => *working-directory*
                                           

Arguments
         

None.

Values
      

*working-directory*
                   

An instance of `<pathname> <file-system.htm#95733>`_.
                                                         

Description
           

Returns the *<pathname>* of the current working directory in the current
process on the local machine. You can use the return value of
*working-directory* in conjunction with *concatenate* to specify
pathnames of entities in the working directory.

See also
        

`working-directory-setter <file-system.htm#45018>`_

working-directory-setter
~~~~~~~~~~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Sets the working directory for the current process.

Signature
         

working-directory-setter *directory* => *directory*
                                                   

Arguments
         

-  *directory* An instance of `<pathname> <file-system.htm#95733>`_.

Values
      

-  *directory* An instance of `<pathname> <file-system.htm#95733>`_.

Description
           

Sets the working directory for the current process.

Note that the following calls are equivalent

working-directory() := "C:\\USERS\\JOHN\\FOO.TEXT";
                                                   

working-directory() := "C:\\USERS\\JOHN\\";
                                           

as they will both set the working directory to *C:\\USERS\\JOHN*. The
call

working-directory() := "C:\\USERS\\JOHN";
                                         

is not equivalent as it sets the working directory to *C:\\USERS*.

Example
       

working-directory() := "C:\\USERS\\JOHN\\";
                                           

See also
        

`working-directory <file-system.htm#90472>`_

