###########################################################
#   Script: .bashrc
# Synopsis: The .bashrc file for the Project Dylan account
#           with login name "dylan"
###########################################################

## Setup the correct lispworks license variables for the site ##
##
source /u/dylan/tools/admin/lw-license-variables

## Setup useful PATH's ##
##
function addpath {
  # Add a directory to the path list, make sure we don't double up.
  _IFS=${IFS:-" "}
  _matched=false
  IFS=:
  for i in $PATH; do
     if [ "$i" = "$1" ]; then
        _matched=true
        break
     fi
  done
  IFS=$_IFS
  if [ $_matched = false ]; then
        PATH=$PATH:$1
  fi
  
}
function delpath {
   PATH=`echo $PATH | /usr/bin/sed "s|:$1:|:|"`
}
export -f addpath delpath
PATH=.:/u/dylan/admin/scripts:/usr/local/bin:/bin:/usr/bin:/u/dylan/admin/builds:/u/dylan/qa/admin
for pathdir in  /usr/ccs/bin /opt/bin /usr/bin/X11 /usr/openwin/bin /usr/sbin /sb /usr/ucb /usr/bsd /usr/local/etc /usr/etc /etc /usr/lib
do
  if [ -d $pathdir ]
  then
    addpath $pathdir
  fi
done

if [ "$UID" = '0' ]; then PS0="#"; else PS0="%"; fi
PS1="\h:\w $PS0 "
PS2=">> "

## Some useful alias ##
##
alias l='ls -l'


## Sort out the terminal

# stty sane
#  *** 'stty sane' makes no sense in the .bashrc, as it aborts tty-less remote
#      logins, e.g., rsh.  If you want it, do it interactively on rlogin.
#
if [ "$TERM" = emacs ]; then		# on rlogin turnoff echo etc
   stty -onlcr -echo				# on target hosts
fi

