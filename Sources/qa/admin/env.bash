#!/usr/local/bin/bash
#   Script: env.bash
#   Author: Shri Amit(amit)
#
#    Usage: source env.bash
# Synopsis: The following script sets a Bourne or Bash shell
#           environment used by the qa build scripts
#######################################################################

## Since almost all the stuff in /u/dylan/tools/admin/env.bash ##
## is quite useful and applicable here, this script will serve ##
## as the interface and will modify or add whatever is needed  ##
##
source /u/dylan/tools/admin/env.bash
admindir=$qadir/admin;             export admindir
logsdir=$admindir/logs;            export logsdir
logstokeep=3;                      export logstokeep

#eof