
# convert from LaTeX to HTML

opt=""
case "$1" in
    -k) opt="$opt $1"; shift ;;
    -f|-p) opt="$opt $1 $2"; shift; shift ;;
    -*) echo "Unrecognized option: $1" ; shift ;;
esac

infile=$1
outfile=$2
echo converting $infile to $outfile

xop=""
aux=`dirname $outfile`/`basename $outfile .html`.LX
if [ -f "$aux" ]
then
  xop="-f $aux"
fi

gema $opt -f /usr/local/lib/gema/latex.dat $xop "$@"

if [ -s $aux ]
then
  diff $aux.bak $aux > /dev/null
  if [ $? -ne 0 ]
  then
    # if the cross-reference index information has changed, then 
    # need to do it a second time.
    echo second pass...
    rm $outfile
    gema $opt -f /usr/local/lib/gema/latex.dat -f $aux "$@"
  fi
fi
