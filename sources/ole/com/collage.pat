! temporary imitation of an approximation of the Collage interface

! Copyright: 1996, 1998 Functional Objects, Inc.  All rights reserved.

! input file:
ARGV:-input <G>\n=@bind{heading;}@write{${.OUT};@imitate-collage{@read{$1}}}\
	@unbind{heading}

! directory containing the header files:
ARGV:-idir <F>\n=@set{idir;$1}

imitate-collage:\Idefine interface <F> *include\W<include-files>\W\
	<interface-body>\Iend\I\L*\;=\N@do-files{$3}\N
imitate-collage:<ReqSpace>=$1
imitate-collage:<I>=$1
imitate-collage:?=?

include-files:\:=
include-files:\"<F>\"=@collect-files{$0}@end
include-files:\{<collect-files>\}=$1@end
include-files:<ReqSpace>=
collect-files:\"<F>\"=@define{desired-file\:@quote{$1}\=\$0\@end}$1\n
collect-files:\S=;,=
collect-files:?<g>=@failed{error Collage-include-files $0}

! process files that have not yet been done
do-files:<done-file>=
do-files:<desired-file>=\
	@define{done-file\:@quote{$1}\=\$0\@end}\
	@{@read{@makepath{${idir;};$1;}}}
do-files:\S=
do-files:<G>=

interface-body:<ReqSpace>=
interface-body:import\:\W\{<interface-imports>\}=\
	@undefine{relevant-name\:\<I\>}\
	@define{bad-struct\:\_\\J\<relevant-name\>\\I\=\$0\@fail}\
	@define{bad-struct\:tag\\J\<relevant-name\>\\I\=\$0\@fail}\
	@define{nogood\:\\W\<relevant-name\>\=\$1\@fail}

interface-body:exclude\:\W\{<interface-excludes>\}=
interface-body:equate\:\W\{<interface-equates>\}=
interface-body:\,=;\;=
interface-body:<G>=@failed{error interface-body $1}

interface-imports:\A=@set-switch{t;0}
interface-imports:<ReqSpace>=
interface-imports:\"<I>\"=@load-only{$1}
interface-imports::load-only

interface-excludes:<ReqSpace>=
interface-excludes:\"<I>\"=@load-obsolete{$1}
interface-excludes::load-obsolete

interface-equates:<ReqSpace>=
interface-equates:\,=
interface-equates:\"<I>\"\W\=\>\W\<<F>\>=\
	@define{type\:\\I$1\\I\\W\<stars\>\=@quote{\<$2\>}\$1\@end}
interface-equates:<G>=@failed{error equate $1}

! required space
ReqSpace:\/\/*\n= $0
ReqSpace:\/\*<comment>\*\/= $0 ;
ReqSpace:<S>=$1
ReqSpace:=@terminate


\#include \<<desired-file>\>=@export-end{}\
	@define{done-file\:@quote{$1}\=\$0\@end}\
	@undefine{desired-file\:@quote{$1}}\
	\N\n@{@read{@mergepath{@inpath;$1;}}}\N\n\
	@set{export-head;\n\ \ export}@set{export-tail;}\
	\/\/ --- end of data from \"$1\" ---\n\n

desired-file:=@fail
done-file:=@fail

ARGV:-include <F>\n=@define{desired-file\:@quote{$1}\=\$0\@end}
