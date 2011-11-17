The four libraries in this directory are the various command-line entry points
into the Open Dylan application.

The modules used by each library determine the commands and features built in to
the application, but the libraries themselves simply process the command-line
options passed to the application, convert them to <command> objects, and
execute them.

The libraries and their features are:

____Library_______________________Compile__Execute__Remote__Plugins

MC  minimal-console-compiler      compile
ME  minimal-console-environment   compile  execute
CC  console-compiler              compile                   plugins
CE  console-environment           compile  execute  remote  plugins

Compile         Can open and build projects.
Execute         Can execute and debug applications.
Remote          Includes remote debugging.
Plugins         Includes Motley and Tool-Scepter plug-ins.
_____________________________________________________________________________

The Dylan files in this directory used by each library are:

File___________________________MC ME___CC CE

minimal-compiler-library       mc
compiler-library                       cc
compiler-command-line          mc      cc
compiler-module                mc      cc

minimal-library                   me
library                                   ce
environment-command-line          me      ce
module                            me      ce

command-line                   mc me   cc ce
start                          mc me   cc ce
____________________________________________________________

All four libraries export a single module called console-environment. This
module is defined in compiler-module.dylan for the CC and MC libraries
and in module.dylan for the CE and ME libraries. It does not export any
bindings.

The command-line options are defined in compiler-command-line.dylan or
environment-command-line.dylan:

Libraries_______Options_________________________In file_______________________

MC, CC          <internal-main-command>         compiler-command-line.dylan
ME, CE          <internal-main-command>         environment-command-line.dylan
______________________________________________________________________________

The command-line options are stored in <basic-main-command> or its subclass
<environment-main-command>:

Libraries_________Storage_______________________In file_______________________

MC,  CC    <main-command>          command-line.dylan
ME,  CE    <main-command>    environment-command-line.dylan
______________________________________________________________________________

The options are converted into <command> objects and executed in
command-line.dylan and environment-command-line.dylan.
