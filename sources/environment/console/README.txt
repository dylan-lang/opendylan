The four libraries in this directory are the various command-line entry points
into the Open Dylan application.

The modules used by each library determine the commands and features built in to
the application, but the libraries themselves simply process the command-line
options passed to the application, convert them to <command> objects, and
execute them.

The libraries and their features are:

____Library_______________________Compile__Execute__Remote__Plugins

DC  dylan-compiler      compile
DE  dylan-environment   compile  execute
DCC  dylan-compiler-with-tools              compile                   plugins
DCE  dylab-environment-with-tools           compile  execute  remote  plugins

Compile         Can open and build projects.
Execute         Can execute and debug applications.
Remote          Includes remote debugging.
Plugins         Includes Motley and Tool-Scepter plug-ins.
_____________________________________________________________________________

The Dylan files in this directory used by each library are:

File___________________________DC DE___DCC DCE

compiler-library       dc
tools-compiler-library                       dcc
compiler-command-line          dc      dcc
compiler-module                dc      dcc

environment-library                   de
tools-environment-library                                   dce
environment-command-line          de      dce
environment-module                            de      dce

command-line                   all
start                          all
____________________________________________________________

All four libraries export a single module called
console-environment. This module is defined in compiler-module.dylan
for the DC and DCC libraries and in environment-module.dylan for the
DE and DCE libraries. It does not export any bindings.

The command-line options are defined in compiler-command-line.dylan or
environment-command-line.dylan:

Libraries_______Options_________________________In file_______________________

DC, DCC          <main-command>         compiler-command-line.dylan
DE, DCE          <main-command>         environment-command-line.dylan
______________________________________________________________________________

The command-line options are stored in <basic-main-command> or its subclass
<environment-main-command>:

Libraries_________Storage_______________________In file_______________________

DC,  DCC    <main-command>          command-line.dylan
DE,  DCE    <main-command>    environment-command-line.dylan
______________________________________________________________________________

The options are converted into <command> objects and executed in
command-line.dylan and environment-command-line.dylan.
