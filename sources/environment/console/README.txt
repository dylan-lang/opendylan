The four libraries in this directory are the various command-line entry points
into the Open Dylan application.

The modules used by each library determine the commands and features built in to
the application, but the libraries themselves simply process the command-line
options passed to the application, convert them to <command> objects, and
execute them.

The libraries and their features are:

____Library_______________________Compile__Execute__Remote__Plugins___________

DC  dylan-compiler                compile
DE  dylan-environment             compile  execute
DCT dylan-compiler-with-tools     compile                   plugins
DET dylan-environment-with-tools  compile  execute  remote  plugins

Compile         Can open and build projects.
Execute         Can execute and debug applications.
Remote          Includes remote debugging.
Plugins         Includes Motley and Tool-Scepter plug-ins.
______________________________________________________________________________

The Dylan files in this directory used by each library are:

File___________________________DC DE___DCT DET________________________________

compiler-library               dc
tools-compiler-library                 dct
compiler-command-line          dc      dct
compiler-module                dc      dct

environment-library               de
tools-environment-library                  det
environment-command-line          de       det
environment-module                de       det

command-line                   all
start                          all
______________________________________________________________________________

All four libraries export a single module called console-environment. This
module is defined in compiler-module.dylan for the DC and DCT libraries and in
environment-module.dylan for the DE and DET libraries. It does not export any
bindings.

The command-line options are DEFINED in compiler-command-line.dylan or
environment-command-line.dylan:

Libraries_________Options_______________________In file_______________________
                                                
DC, DCT           <main-command>                compiler-command-line.dylan
DE, DET           <main-command>                environment-command-line.dylan
______________________________________________________________________________

The command-line options are STORED in <basic-main-command> or its subclass
<environment-main-command>:

Libraries_________Storage_______________________In file_______________________

DC, DCT           <basic-main-command>          command-line.dylan
DE, DET           <environment-main-command>    environment-command-line.dylan
______________________________________________________________________________

The options are converted into <command> objects and executed in
command-line.dylan and environment-command-line.dylan.
