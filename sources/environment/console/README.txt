The libraries in this directory are the command-line entry points into the Open
Dylan application.

The modules used by each library determine the commands and features built in to
the application, but the libraries themselves simply process the command-line
options passed to the application, convert them to <command> objects, and
execute them.

The libraries and their features are:

____Library_______________________Compile__Execute__Remote__Plugins___________

DC  dylan-compiler                compile                   plugins
DE  dylan-environment             compile  execute  remote  plugins

Compile         Can open and build projects.
Execute         Can execute and debug applications.
Remote          Includes remote debugging.
Plugins         Includes Motley and Tool-Scepter plug-ins.
______________________________________________________________________________

The Dylan files in this directory used by each library are:

File_______________________________DC DE______________________________________

compiler-library                   x
compiler-command-line              x
compiler-module                    x

environment-library                   x
environment-command-line              x
environment-module                    x

command-line                       x  x
start                              x  x
______________________________________________________________________________

Both libraries export a single module called console-environment. This module
is defined in compiler-module.dylan for the DC library and in
environment-module.dylan for the DE library. It does not export any bindings.

The command-line options are DEFINED in compiler-command-line.dylan or
environment-command-line.dylan:

Libraries_________Options_______________________In file_______________________

DC                <main-command>                compiler-command-line.dylan
DE                <main-command>                environment-command-line.dylan
______________________________________________________________________________

The command-line options are STORED in <basic-main-command> or its subclass
<environment-main-command>:

Libraries_________Storage_______________________In file_______________________

DC                <basic-main-command>          command-line.dylan
DE                <environment-main-command>    environment-command-line.dylan
______________________________________________________________________________

The options are converted into <command> objects and executed in
command-line.dylan and environment-command-line.dylan.
