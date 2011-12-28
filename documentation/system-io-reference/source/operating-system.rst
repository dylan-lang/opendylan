***************************
The Operating-System Module
***************************

Introduction
------------

The Operating-System module is part of the System library. It provides
an interface to some features of the host machine’s operating system.

This chapter describes the functions and constants that the
Operating-System module contains.

Manipulating environment information
------------------------------------

The Operating-System module contains a number of interfaces for
examining and specifying information about the operating system
environment of the host machine. As well as providing constants that you
can use in your code, you can examine and set the value of any
environment variable in the system.

The following constants contain machine-specific information.
                                                             

$architecture-little-endian
'''''''''''''''''''''''''''

Constant
        

$machine-name
'''''''''''''

Constant
        

$os-name
''''''''

Constant
        

$os-variant
'''''''''''

Constant
        

$os-version
'''''''''''

Constant
        

$platform-name
''''''''''''''

Constant
        

These constants contain information about the hardware and software
resident on the host machine. The constants let you programmatically
check the current system conditions before executing a piece of code.

The constant *$architecture-little-endian* is a boolean value that is
true if the processor architecture is little-endian and false if it is
big-endian. (A processor is little-endian if the rightmost bit in a word
is the least-significant bit.) For processors implementing the Intel x86
architecture this value is *#t*.

The constant *$machine-name* specifies the hardware installed in the
machine. The constant *$os-name* specifies the operating system that is
running.

The constant *$os-variant* is a symbol value distinguishing between
variants of the operating system identified by *$os-name*, where
relevant; otherwise it has the same value as *$os-name*. On Windows,
the possible values are *#"win3.1"*, *#"win95"*, *#"win98"*, and
*#"winnt"*.

The constant *$os-version* is a string value that identifies the version
of the operating system. For Windows NT, a typical value would be
*"4.0.1381 Service Pack 3"*. For Windows 95, a typical value would be
*"4.0.1212 B"*.

The *$platform-name* constant is an amalgam of the information contained
in *$machine-name* and *$os-name*, to enable you to conveniently
conditionalize your code based on both values.

The following two functions let you manipulate the values of any
environment variables in the system.

environment-variable
''''''''''''''''''''

Function
        

environment-variable-setter
'''''''''''''''''''''''''''

Function
        

environment-variable *name* => *value*
                                      

environment-variable-setter *new-value* *name* => *new-value*
                                                             

The function *environment-variable* returns the current value of any
environment variable. The function *environment-variable-setter* lets
you specify the value of any environment variable. All arguments and
returns values in these functions are instances of *<byte-string>*.If
the environment variable passed to *environment-variable* does not
exist, it creates it. For *environment-variable-setter*, if *new-value*
is *#f*, then the environment variable specified is undefined, if
undefining environment variables is supported.

The following functions access information about the user logged on to
the current machine, where available.

login-name
''''''''''

Function
        

login-name () => *name-or-false*
                                

Returns as an instance of *<string>* the name of the user logged on to
the current machine, or *#f* if unavailable.

login-group
'''''''''''

Function
        

login-group () => *group-or-false*
                                  

Returns as an instance of *<string>* the group (for example NT domain,
or Windows Workgroup) of which the user logged on to the current machine
is a member, or *#f* if the group unavailable.

owner-name
''''''''''

Function
        

owner-name () => *name-or-false*
                                

Returns as an instance of *<string>* the name of the user who owns the
current machine (that is, the name entered when the machine was
registered), or *#f* if the name unavailable.

owner-organization
''''''''''''''''''

Function
        

owner-organization () => *organization-or-false*
                                                

Returns as an instance of *<string>* the organization to which the user
who owns the current machine belongs, or *#f* if the name unavailable.

Manipulating application information
------------------------------------

The Operating-System module contains a number of functions for
manipulating information specific to a given application, rather than
the environment as a whole. You can run or quit any application, and
interrogate the running application for application-specific
information.

run-application
'''''''''''''''

Function
        

run-application *command* #key *under-shell?* *inherit-console?*
                                                                

*activate?* *minimize?*
                       

=> *status*
           

Runs the application specified by *command*. Using this function is
equivalent to typing *command* in an MS-DOS console window. The function
returns the exit status of the application.

If *under-shell?* is *#t*, an MS-DOS shell is created to run the
application; otherwise, the application is run directly. It is *#f* by
default.

If *inherit-console?* is *#t*, the new application uses the same
console window as the current application; otherwise, the new
application is created with a separate console window. It is *#t* by
default.

If the *activate?* keyword is *#t*, the shell window becomes the active
window. It is *#t* by default.

If the *minimize?* keyword is *#t*, the command’s shell will appear
minimized. It is *#f* by default.

exit-application
''''''''''''''''

Function
        

exit-application *status* => ()
                               

Terminates the running application. Returns the value of *status*.

application-arguments
'''''''''''''''''''''

Function
        

application-name
''''''''''''''''

Function
        

application-filename
''''''''''''''''''''

Function
        

application-arguments () => *arguments*
                                       

application-name () => *name*
                             

application-filename () => *false-or-filename*
                                              

These functions respectively return the arguments passed to the running
application, the name of the running application, and the full filename
(that is, the absolute pathname) of the running application.

These functions take no arguments. The function *application-arguments*
returns an instance of *<simple-object-vector>* ; *application-name*
returns an instance of *<byte-string>* ; and *application-filename*
returns an instance of *false-or(<byte-string>)*.

tokenize-command-string
'''''''''''''''''''''''

Function
        

tokenize-command-string *line* => *command* #rest *arguments*
                                                             

This argument passed to this function is an MS-DOS command that could be
used to start an application from the MS-DOS command line. It returns
the command itself, together with any command-line arguments. All
arguments and return values are instances of *<byte-string>*. (In the
case of the arguments returned, each individual argument is an instance
of *<byte-string>*.) You can use this function to break up any MS-DOS
command into its constituent parts.

The OPERATING-SYSTEM module
---------------------------

This section contains a reference entry for each item exported from the
Operating-System module’s Operating-System module.

application-arguments
~~~~~~~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Returns the arguments passed to the running application.

Signature
         

application-arguments => *arguments*
                                    

Arguments
         

None.

Values
      

*arguments* An instance of *<simple-object-vector>*.
                                                     

Description
           

Returns the arguments passed to the running application as a vector of
instances of *<byte-string>*.

See also
        

`application-filename <operating-system.htm#34263>`_

`application-name <operating-system.htm#78353>`_

`tokenize-command-string <operating-system.htm#69612>`_

application-filename
~~~~~~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Returns the full filename of the running application.

Signature
         

application-filename => *false-or-filename*
                                           

Arguments
         

None.
     

Values
      

*false-or-filename* An instance of *false-or(<byte-string>)*.
                                                              

Description
           

Returns the full filename (that is, the absolute pathname) of the
running application, or *#f* if the filename cannot be determined.

Example
       

The following is an example of an absolute pathname naming an
application:

"C:\\Program Files\\foo\\bar.exe"
                                 

See also
        

`application-arguments <operating-system.htm#69782>`_

`application-name <operating-system.htm#29524>`_

`tokenize-command-string <operating-system.htm#69612>`_

application-name
~~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Returns the name of the running application.

Signature
         

application-name => *name*
                          

Arguments
         

None.
     

Values
      

*name* An instance of *<byte-string>*.
                                       

Description
           

Returns the name of the running application. This is normally the
command name as typed on the command line and may be a non-absolute
pathname.

Example
       

The following is an example of a non-absolute pathname used to refer to
the application name.

"foo\\bar.exe"
              

See also
        

`application-arguments <operating-system.htm#69782>`_

`application-filename <operating-system.htm#34263>`_

`tokenize-command-string <operating-system.htm#69612>`_

environment-variable
~~~~~~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Returns the value of a specified environment variable.

Signature
         

environment-variable *name* => *value*
                                      

Arguments
         

*name* An instance of *<byte-string>*.
                                       

Values
      

*value* An instance of *<byte-string>*, or *#f*.
                                                  

Description
           

Returns the value of the environment variable specified by *name*, or
*#f* if there is no such environment variable.

See also
        

`environment-variable-setter <operating-system.htm#41256>`_

environment-variable-setter
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Sets the value of an environment variable.

Signature
         

environment-variable-setter *new-value* *name* => *new-value*
                                                             

Arguments
         

*new-value* An instance of *<byte-string>*, or *#f*.
                                                      

*name* An instance of *<byte-string>*.
                                       

Values
      

*new-value* An instance of *<byte-string>*, or *#f*.
                                                      

Description
           

Changes the value of the environment variable specified by *name* to
*new-value*. If *new-value* is *#f*, the environment variable is
undefined. If the environment variable does not already exist,
*environment-variable-setter* creates it.

*Note:* Windows 95 places restrictions on the number of environment
variables allowed, based on the total length of the names and values of
the existing environment variables. The function
*environment-variable-setter* only creates a new environment variable if
it is possible within these restrictions. See the relevant Windows 95
documentation for more details.

See also
        

`environment-variable <operating-system.htm#62898>`_

exit-application
~~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Terminates execution of the running application.

Signature
         

exit-application *status* => ()
                               

Arguments
         

*status* An instance of *<integer>*.
                                     

Values
      

None.
     

Description
           

Terminates execution of the running application, returning the value of
*status* to whatever launched the application, for example an MS-DOS
window or Windows 95/NT shell.

See also
        

`run-application <operating-system.htm#15849>`_

login-name
~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Returns as an instance of *<string>* the name of the user logged on to
the current machine, or *#f* if unavailable.

Signature
         

login-name () => *name-or-false*
                                

Arguments
         

None.

Values
      

*name-or-false* An instance of *false-or(<string>)*.
                                                     

Description
           

Returns as an instance of *<string>* the name of the user logged on to
the current machine, or *#f* if unavailable.

See also
        

`login-group <operating-system.htm#23810>`_

login-group
~~~~~~~~~~~

Function
^^^^^^^^

Signature
         

login-group () => *group-or-false*
                                  

Arguments
         

None.

Values
      

*group-or-false* An instance of *false-or(<string>)*.
                                                      

Description
           

Returns as an instance of *<string>* the group (for example NT domain,
or Windows Workgroup) of which the user logged on to the current machine
is a member, or *#f* if the group is unavailable.

See also
        

`login-name <operating-system.htm#42695>`_

$machine-name
~~~~~~~~~~~~~

Constant
^^^^^^^^

Summary
       

Constant specifying the type of hardware installed in the host machine.

Type
    

Symbol.

Initial value
             

#"x86"
      

Description
           

This constant is a symbol that represents the type of hardware installed
in the host machine.

Example
       

*#"x86"*, *#"alpha"*

See also
        

`$os-name <operating-system.htm#15312>`_

`$os-variant <operating-system.htm#26099>`_

`$os-version <operating-system.htm#26717>`_

`$platform-name <operating-system.htm#20095>`_

$os-name
~~~~~~~~

Constant
^^^^^^^^

Summary
       

Constant specifying the operating system running on the host machine.

Type
    

Symbol.

Initial value
             

#"win32"
        

Description
           

This constant is a symbol that represents the operating system running
on the host machine.

Example
       

*#"win32"*, *#"osf3"*

See also
        

`$machine-name <operating-system.htm#97185>`_

`$os-variant <operating-system.htm#26099>`_

`$os-version <operating-system.htm#26717>`_

`$platform-name <operating-system.htm#20095>`_

$os-variant
~~~~~~~~~~~

Constant
^^^^^^^^

Summary
       

Constant specifying which variant of an operating system the current
machine is running, where relevant.

Type
    

Symbol.

Initial value
             

See Description.

Description
           

This constant is a symbol value distinguishing between variants of the
operating system identified by *$os-name*, where relevant; otherwise it
has the same value as *$os-name*. On Windows, the possible values are
*#"win3.1"*, *#"win95"*, *#"win98"*, and *#"winnt"*.

See also
        

`$machine-name <operating-system.htm#97185>`_

`$os-name <operating-system.htm#15312>`_

`$os-version <operating-system.htm#26717>`_

`$platform-name <operating-system.htm#20095>`_

$os-version
~~~~~~~~~~~

Constant
^^^^^^^^

Summary
       

Constant specifying which version of an operating system the current
machine is running.

Type
    

*<string>*

Initial value
             

See Description.

Description
           

The constant *$os-version* is a string value that identifies the version
of the operating system. For Windows NT, a typical value would be
*"4.0.1381 Service Pack 3"*. For Windows 95, a typical value would be
*"4.0.1212 B"*.

See also
        

`$machine-name <operating-system.htm#97185>`_

`$os-name <operating-system.htm#15312>`_

`$os-variant <operating-system.htm#26099>`_

`$platform-name <operating-system.htm#20095>`_

owner-name
~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Returns the name of the user who owns the current machine, if available.

Signature
         

owner-name () => *name-or-false*
                                

Arguments
         

None.

Values
      

name-or-false An instance of *false-or(<string>)*.
                                                   

Description
           

Returns as an instance of *<string>* the name of the user who owns the
current machine (that is, the name entered when the machine was
registered), or *#f* if the name is unavailable.

owner-organization
~~~~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Returns the organization to which the user who owns the current machine
belongs, if available.

Signature
         

owner-organization () => *organization-or-false*
                                                

Arguments
         

None.

Values
      

*organization-or-false*
                       

An instance of *false-or(<string>)*.
                                     

Description
           

Returns as an instance of *<string>* the organization to which the user
who owns the current machine belongs, or *#f* if the name is
unavailable.

$platform-name
~~~~~~~~~~~~~~

Constant
^^^^^^^^

Summary
       

Constant specifying the operating system running on and the type of
hardware installed in the host machine.

Type
    

Symbol.

Initial value
             

#"x86-win32"
            

Description
           

This constant is a symbol that represents the both the operating system
running on, and the type of hardware installed in, the host machine. It
is a combination of the `$os-name <operating-system.htm#15312>`_ and
`$machine-name <operating-system.htm#97185>`_ constants.

Example
       

*#"x86-win32"*, *#"alpha-osf3"*

See also
        

`$machine-name <operating-system.htm#97185>`_

`$os-name <operating-system.htm#15312>`_

run-application
~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Launches an application using the specified name and arguments.

Signature
         

run-application *command* #key *minimize?* *activate?*
                                                      

*under-shell?* *inherit-console?*
                                 

=> *status*
           

Arguments
         

*command* An instance of *<string>*.
                                     

*minimize?* An instance of *<boolean>*.
                                        

*activate?* An instance of *<boolean>*.
                                        

** Values
         

*status* An instance of *<integer>*.
                                     

Description
           

Launches an application using the name and arguments specified in
command. Using this function is equivalent to typing the command in a
MS-DOS window. The return value is the exit status returned by the
application.

If the *minimize?* keyword is *#t*, the command’s shell will appear
minimized. It is *#f* by default.

If the *activate?* keyword is *#t*, the shell window becomes the active
window. It is *#t* by default.

If the *under-shell?* keyword is *#t*, an MS-DOS shell is created to
run the application; otherwise, the application is run directly. It is
*#f* by default.

If the *inherit-console?* keyword is *#t*, the new application uses the
same console window as the current application; otherwise, the new
application is created with a separate console window. It is *#t* by
default.

See also
        

`exit-application <operating-system.htm#57525>`_

tokenize-command-string
~~~~~~~~~~~~~~~~~~~~~~~

Function
^^^^^^^^

Summary
       

Parses a command line into a command name and arguments.

Signature
         

tokenize-command-string *line* => *command* #rest *arguments*
                                                             

Arguments
         

*line* An instance of *<byte-string>*.
                                       

Values
      

*command* An instance of *<byte-string>*.
                                          

*arguments* Instances of *<byte-string>*.
                                          

Description
           

Parses the command specified in *line* into a command name and
arguments. The rules used to tokenize the string are given in
Microsoft’s C/C++ reference in the section “Parsing C Command-Line
Arguments”.

See also
        

`application-arguments <operating-system.htm#69782>`_

`application-name <operating-system.htm#78353>`_


