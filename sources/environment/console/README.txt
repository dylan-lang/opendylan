The eight libraries in this directory are the various command-line entry points
into the Open Dylan application.

The modules used by each library determine the commands and features built in to
the application, but the libraries themselves simply process the command-line
options passed to the application, convert them to <command> objects, and
execute them.

The libraries and their features are:

____Library_______________________Compile__Execute__Remote__Plugins__Internal

BC  basic-console-compiler        compile
BE  basic-console-environment     compile  execute
MC  minimal-console-compiler      compile                            internal
ME  minimal-console-environment   compile  execute                   internal
EC  enhanced-console-compiler     compile                   plugins
EE  enhanced-console-environment  compile  execute  remote  plugins
CC  console-compiler              compile                   plugins  internal
CE  console-environment           compile  execute  remote  plugins  internal

Compile		Can open and build projects.
Execute 	Can execute and debug applications.
Remote		Includes remote debugging.
Plugins 	Includes Motley and Tool-Scepter plug-ins.
Internal	Includes internal commands.
_____________________________________________________________________________

The Dylan files in this directory used by each library are:

File___________________________BC BE___MC ME___EC EE___CC CE
                                                            
console-compiler                                            
console-environment                                         
                                                            
basic-compiler-library         bc                           
minimal-compiler-library               mc                   
enhanced-compiler-library                      ec           
compiler-library                                       cc   
compiler-command-line          bc      mc      ec      cc   
compiler-module                bc      mc      ec      cc   
                                                            
basic-library                     be                        
minimal-library                           me                
enhanced-library                                  ee        
library                                                   ce
environment-command-line          be      me      ee      ce
module                            be      me      ee      ce
                                                            
command-line                   bc be   mc me   ec ee   cc ce
start                          bc be   mc me   ec ee   cc ce
____________________________________________________________

You will note that console-compiler.dylan and console-environment.dylan are not
used by any of the libraries. I am not sure what they are for.

All eight libraries export a single module called console-environment. This
module is defined in compiler-module.dylan for the CC, MC, BC, and EC libraries
and in module.dylan for the CE, ME, BE, and EE libraries. It does not export any
bindings.

The command-line options are defined in compiler-command-line.dylan or
environment-command-line.dylan:

Libraries_______Options_________________________In file_______________________

BC, EC		<main-command>			compiler-command-line.dylan
MC, CC		<internal-main-command>		compiler-command-line.dylan
BE, EE		<main-command>			environment-command-line.dylan
ME, CE		<internal-main-command>		environment-command-line.dylan
______________________________________________________________________________

The command-line options are stored in <basic-main-command> or its subclass
<environment-main-command>:

Libraries_________Storage_______________________In file_______________________
                  
BC, MC, EC, CC	  <basic-main-command>	        command-line.dylan
BC, ME, EE, CE	  <environment-main-command>    environment-command-line.dylan
______________________________________________________________________________

The options are converted into <command> objects and executed in
command-line.dylan and environment-command-line.dylan.
