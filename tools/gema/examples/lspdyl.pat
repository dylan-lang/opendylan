
! Lisp to Dylan		9/3/95, 10/9/95, 11/23/95, 12/3/95, 12/24/95, 3/17/96
! including support for (old) Flavors

! not yet handled:
!  do [general case]
!  let [when parallel assignment is really needed]
!  defmacro [incomplete translation]
!  macrolet
!  catch, throw
!  multiple-value-setq
!  prog
!  #+  #-
!  CLOS
!  [and surely many others not yet enumerated]

@set-switch{i;1}
@set-parm{idchars;\-\_\+\=\*\&\%\$\@\<\>\?\/}
@set-syntax{L;\-\.\(\)\'\.}

ARGV:\N-module <I>\n=@set{module;$1}
\B= @set{O;@outpath}@err{\ @inpath -\> $O ...\n}\
	module\: ${module;dylan-user}\n\n
\B\W\$ type *\n=
\;\;\; -\*- Mode*\n=

! comments
\;*\n=\/\/*\n
\#\|*\|\#=\/\**\*\/
space:\;*\n=\/\/*\n
space:\#\|*\|\#=\/\**\*\/
space:^M=
space:\s=\s;\n=\n;\t=\t;<S>=$1
space:=@end

reqspace:\;*\n=\/\/*\n
reqspace:\#\|*\|\#=\/\**\*\/
reqspace:^M=
reqspace:\s=\s;\n=\n;\t=\t;<S>=$1
reqspace:=@terminate

^M=
\s=\s
\n=\n
<reqspace>=$1
<expr>=$1

! literals
expr:"\L<string>"=$0@end
string:\\?=\\?
expr:\#\\<charlit>=$1@end
charlit:space\I='\s'@end
charlit:newline\I='\\n'@end
charlit:tab\I='\\t'@end
charlit:page\I='\\f'@end
charlit:<I2><i>=\$$1$2-character@end
charlit:\\='\\\\'@end
charlit:<G1>\I='$1'@end
charlit:=@fail
expr:'(\W)=\#()@end
expr:(\W)=\#()@end
expr:'<I>\:<I>=\#"$1\/$2"@end
expr:'<I>=\#"$1"@end
expr:'\:<I>=$1\:@end
expr:'(<litlist>)=\#($1)@end
expr:'\#(<litlist>)=\#[$1]@end
expr:'<N>\I=$1@end
expr:'\#\\<charlit>=$1@end
expr:'\P<g20>=\I%quote%\s@err{\Nline @line\: unrecognized\: '$1\n}
expr:\#(<litlist>)=\#[$1]@end
expr:<D>\/<D>=$0@end
expr:<N><optexp>=$1$2@end
expr:-<N><optexp>=-$1$2@end
expr:\#x\W<X>=$0@end
expr:\#o\W<O>=$0@end
expr:\#'eq\I=\\==@end
expr:\#'neq\I=\\\~==@end
expr:\#'equal\I=\\=@end
expr:\#'eql\I=\\=@end
expr:\#'<-A>\I=\\$1@end
expr:\#'(\Wlambda\I\W<arglist><body>)=method ($1)@maybe-block{\d$2} \
	end method@end
expr:\#'<id>=$1@end

expr:\`\W(\W<matchparen>)=\{ @{($1)} \}@end
expr:\`\W<expr>=\{ $1 \}@end
expr:,<id>=\?$1@end
expr:,(string <id>)=\?"$1"@end
expr:',<id>=\?\#"$1"@end
expr:,<expr>=\?($1)@end
expr:.\W,<id>=\?$1@end

expr:<reqspace>=$1

expr:\I\:<I>=$1\:@end

expr:\It\I=\#t@end
expr:\Inil\I=\#f@end

! names with a special meaning in Dylan but not in Lisp:
id:\Ielement\I=$0\@@end
id:\Imake\I=$0\@@end
id:\Iinitialize\I=$0\@@end
id:\Ipair\I=$0\@@end
id:\Irange\I=$0\@@end
id:\Isingleton\I=$0\@@end
id:\Ilimited\I=$0\@@end
id:\Imethod\I=$0\@@end
id:\Iremainder\I=$0\@@end
id:\Ias\I=$0\@@end
id:\Isize\I=$0\@@end
id:\Irank\I=$0\@@end
id:\Idimension\I=$0\@@end
id:\Idimensions\I=$0\@@end
id:\Ihead\I=$0\@@end
id:\Itail\I=$0\@@end
id:\Isignal\I=$0\@@end
id:\Ilocal\I=$0\@@end
id:\Ibegin\I=$0\@@end
id:\Iend\I=$0\@@end
id:\Idefine\I=$0\@@end
id:\Iselect\I=$0\@@end
id:\Ichoose\I=$0\@@end
id:\Ido\I=$0\@@end
id:,<I>=\?@id{$1}@end
id:<I>=$1@end
id:=@fail

expr:<id><optqual>=$1$2@end
optqual:\:=\/
optqual:<I>=$1
optqual:=@end

optexp:<L1>0=@end
optexp:<L1><D>\I=e$2@end
optexp:<L1>-<D>\I=e-$2@end
optexp:=@end

(defvar <id> <expr>\W<optdoc>)=\Ndefine variable $1 \= $2\;$3
(defvar <id> \:unbound\W<optdoc>)=\Ndefine variable $1\;$2
optdoc:\W"\L<string>"= \/\* $1 \*\/@end
optdoc:\;*\n\W=\/\/*\n
optdoc:=@end

(defparameter \*\J<I>\J\*\W<expr>\W<optdoc>)=\Ndefine variable \*$1\* \= $2\;$3
(defparameter <id>\W<expr>\W<optdoc>)=\Ndefine constant $1 \= $2\;$3
(defconstant <id>\W<expr>\W<optdoc>)=\Ndefine constant $1 \= $2\;$3

(defflavor <id>\W<slots>\W<bases>\W<flavopt>)=\
	\Ndefine class \<$1\> ( $3 ) $2$4\;\nend class\;\n
bases:(\W)=\<object\>@end
bases:nil\I=\<object\>@end
bases:(<list>)=@classname{$1}@end
bases:<reqspace>=$1
bases:=@fail

classname:<id>=\<$1\>

slots:(\W)=@end
slots:nil\I=@end
slots:(<slot>)=$1@end
slots:<reqspace>=$1
slots:=@fail
slot:<reqspace>=$1
slot:<I>=\N\tslot @id{$1}, init-keyword\: $1\:\;\n
slot:<id>=\N\tslot $1, init-keyword\: $1\:\;\n
slot:(\W<id>\W<expr>\W)=\N\tslot $1 \= $2\;\n

flavopt:\:inittable-instance-variables\I=
flavopt:\:gettable-instance-variables\I=
flavopt:\:settable-instance-variables\I=
flavopt:(<list>)=\/\/% $1
flavopt:<reqspace>=$1

list:\:<I>\W<expr><term>=$1\: $2$3
list:<expr><term>=$1$2
list:<reqspace>=$1
term:<S><I0>=,$1@end
term:<S>\P(=,$1@end
term:<reqspace>=$1
term:\P)=@end
term:=,\s@end

litlist:(<litlist>)<term>=\#($1)$2
litlist:\#(<litlist>)<term>=\#[$1]$2
litlist:\:<I>=$1\:
litlist:\Inil\I<term>=\#f$1
litlist:<D>\/<D><term>=$0
litlist:<N><term>=$1$2
litlist:<I><term>=\#"$1"$2
litlist:<reqspace>=$1
litlist::list

!morelist:<reqspace>=$1
!morelist:\P\)=@end
!morelist:<list>=, $1@end

(defstruct <id>\W<slot>\W)=\
	\Ndefine class \<$1\> ( \<object\> )\n\s\s$2\nend class \<$1\>\;\n
(defstruct (<id>\W<matchparen>)\W<slot>\W)=\
	\Ndefine class \<$1\> ( \<object\> )\n\s\s$3\;\nend class \<$1\>\;\n\
	@bind{struct-name;\<$1\>}@struct-opts{$2}@unbind{struct-name}
struct-opts:(\:constructor <id>\W(\W<id>\W)\W)=\
	\Ndefine method $1 ($2) make(${struct-name}, $2\: $2) end\;\n
struct-opts:(\:constructor <id>\W<arglist>\W)=\
	\Ndefine method $1 ($2) make(${struct-name}, $2) end\;\n
struct-opts:(\:predicate <id>\W)=\
	define method $1 (object) instance\?(object, ${struct-name}) end\;\n
struct-opts:(\:copier nil\W)=
struct-opts:(\:copier <id>\W)=\
  define method $1 (object \:\: ${struct-name})\n\tshallow-copy(object) end\;\n
struct-opts:\S=
struct-opts:<reqspace>=$1

(defmethod (\W<id> \:after \:init\W)\W<morearglist><body>)=\
	define method initialize( self \:\: @classname{$1}$2)\;\n  next-method()\;$3\Nend initialize\;

(defmethod (\W<id> \:default \:<I>\W)<morearglist><body>)=\
	define method $2\&( self \:\: @classname{$1}$3) \/\* default \*\/\;\
	@maybe-block{\d$4}\Nend method $2\&\;

(defmethod (\W<id> \:after \:<I>\W)<morearglist><body>)=\
	define method $2\&( self \:\: @classname{$1}$3)\;\n  next-method()\;\n\
	@maybe-block{\d$4}\Nend method $2\&\;

(defmethod (\W<id> \:before \:<I>\W)<morearglist><body>)=\
	define method $2\&( self \:\: @classname{$1}$3)\;\
	@maybe-block{\d$4}\N  next-method()\;\nend method $2\&\;

(defmethod (\W<id> \:<I>\W)<morearglist><body>)=\
	define method $2\&( self \:\: @classname{$1}$3)\;\
	@maybe-block{\d$4}\Nend method $2\&\;

(defmethod (\W<id> \:<I>\W)\W<id>\W)=\
	define method $2\&( self \:\: @classname{$1}, \#rest args )\;\
	\n\ \ apply($3, args)\nend method $2\&\;

(defun <id><arglist><body>)=\Ndefine method $1 ($2)\;\
	@maybe-block{$1\d$3}\Nend method $1\;
(defsubst <id><arglist><body>)=\Ndefine method $1 ($2)\;$3\Nend method $1\;

(deff <id>\W<expr>)=\Ndefine constant $1 = $2\;

morearglist:nil\I=@end
morearglist:(\W)=@end
morearglist:(<args>)=, $1@end
morearglist:<reqspace>=$1
morearglist:=@fail

arglist:nil\I=@end
arglist:(\W)=@end
arglist:(<args>)=$1@end
arglist:<reqspace>=$1
arglist:=@fail

args:\&<L>\I=\#$1
args:<id><term>=$1$2
args:<reqspace>=$1
args:(\W<id> <expr>\W)<term>=$1 \= $2$3

body:\A<s>"<string>"<reqspace>\P(=$1\/\/ "$2"\n$3! doc string
body:\s=\s;\n=\n
body:<reqspace>=$1
body:(declare\W(unspecial *))=
body:(declare\I<matchparen>)=\/\/\% declare $1\N
body:<expr>=$1\;
matchparen:(#)=(#)
matchparen:"\L<string>"=$0
matchparen:\;*\n=$0
matchparen:\#\|*\|\#=$0

maybe-block:<I>\d<s>block(return-from-$1)*=$2block(return-from-$1)*
maybe-block:<I>\d<s>*\Ireturn-from-$1(*=$2\I@set{T;@out-column}\
	block(return-from-$1)\
	\N@tab{$T}  $3return-from-$1($4\N@tab{$T}end block\I
maybe-block:<p>\d<s><matchblock>\Ireturn(*=$2\I@set{T;@out-column}\
	block(return)\
	\N@tab{$T}  $3return($4\N@tab{$T}end block\I
maybe-block:<p>\d*=$2

matchblock:\Iblock\W(<i>)<s><matchblock>end\ block=$0
matchblock::matchout

parenthesize:\A\S=
parenthesize:<I>\Z=$1
parenthesize:<N>\Z=$1
parenthesize:(<matchout>)\Z=$0
parenthesize:"<string>"\Z=$0
parenthesize:'<string>'\Z=$0
parenthesize:\#<G>\Z=\#$1
parenthesize:\?\#"<I>"\Z=$0
parenthesize:\?"<I>"\Z=$0
parenthesize:<I>(<matchout>)\Z=$1($2)
parenthesize:<I><s>\[<matchout>\]\Z=$0
parenthesize:*=(*)

! match parentheses in output code
matchout:(#)=(#)
matchout:\[#\]=\[#\]
matchout:"<string>"=$0
matchout:\L'<string>'=$0
matchout:\/\/*\n=$0
matchout:\/\**\*\/=$0
matchout:\)=@fail
matchout:\]=@fail

! operators with an arbitrary number of operands
expr:(\W<mult-op>\I<operands>)=$2@unbind{op}@end
mult-op:and\I=@bind{op;\&}@end
mult-op:or\I=@bind{op;\|}@end
mult-op:\+\I=@bind{op;\+}@end
mult-op:\-\I=@bind{op;\-}@end
mult-op:\*\I=@bind{op;\*}@end
mult-op:=@fail
operands:<expr><opterm>=@parenthesize{$1}$2
operands:<reqspace>=$1
operands:=@fail
opterm:<reqspace>=$1
opterm:\Z=@end
opterm:= ${op} @end

! operators with one operand
expr:(-\I<expr>\W)=\I\- @parenthesize{$1}@end

! operators with two operands
expr:(\W<op>\I\W<expr>\W<expr>\W)=@parenthesize{$2} $1 @parenthesize{$3}@end
op:\<\I=\<@end;\<\=\I=\<\=@end;\>\I=\>@end;\>\=\I=\>\=@end;
op:\=\I=\=@end;eq\I=\=\=@end;\/\=\I=\~\=@end;neq\I=\~\=\=@end
op:expt\I=\^@end
op:setq\I=\:\=@end
op:=@fail

expr:(setq\I\W<expr>\W<expr>\W)=@parenthesize{$1} \:\= $2@end
expr:(setq\W<set-pairs>)=\Ibegin $1\Iend@end
set-pairs:<expr><space><expr>=@parenthesize{$1} \:\= $2$3\;\s
set-pairs:=@terminate

! general case function call:
expr:(\W<id><optqual>\W<list>)=\I$1$2($3)@end

expr:(aref <expr><expr><space>)=$1[$2$3]@end
expr:(array-dimension <expr>\W<expr>)=\Idimension($1, $2)@end
expr:(array-dimensions <expr>)=\Idimensions($1)@end
expr:(block <I><body>)=@set{T;@out-column}block(return-from-$1)$2\
	\N@tab{$T}  end block@end
expr:(car <expr>)=\Ihead($1)@end
expr:(cadr <expr>)=\Isecond($1)@end
expr:(case\I\W<expr><caseclauses>)=\I@set{T;@out-column}\
	select ( $1 )$2\N@tab{$T}  end select@end
expr:(cdr <expr>)=\Itail($1)@end
expr:(ceiling <expr>\W<expr>\W)=\Iceiling\/($1, $2)@end
expr:(comment\I<>)=\/\* $1 \*\/@end
expr:(cond\I\W<condclauses>)=\Icase $1\N  end case @end
expr:(decf <expr> <expr>\W)=$1 \:\= $1 \- $2@end
expr:(decf <expr>\W)=$1 \:\= $1 \- 1@end
expr:(delete <expr>\W<expr>\W<term><list>)=\Iremove\!($2, $1$3$4)@end
expr:(do\W(\W<iterators>)\G\W(\W<expr>\W<optfinal>\W)<body>)=\
	@set{T;@out-column}@maybe-block{\d\
	for ( $1 until\: $2 )$4$3\N@tab{$T}  end for}@end
expr:(dotimes\W(\W<id>\W<expr>\W<optfinal>\W)<body>)=@set{T;@out-column}\
	@maybe-block{\d\
	for ( $1 \:\: \<integer\> from 0 below $2 )$4$3\N@tab{$T}  end for}@end
expr:(dolist\W(\W<id>\W<expr>\W<optfinal>\W)<body>)=@set{T;@out-column}\
	@maybe-block{\d\
	for ( $1 in $2 )$4$3\N@tab{$T}  end for}@end
expr:(ecase\I\W<expr><caseclauses>)=\I@set{T;@out-column}\
	select ( $1 )$2\N@tab{$T}  end select@end
expr:(elt <expr>\W<expr>)=\I$1\[$2\]@end
expr:(etypecase <expr><typecases>)=\I@set{T;@out-column}\
	select ( $1 by instance\? )$2\N@tab{$T}  end select@end
expr:(flet\W(\W<fun-bindings>)\G<body>)=$1$2@end
expr:(floor <expr>\W<expr>\W)=\Ifloor\/($1, $2)@end
expr:(format nil\I<list>)=format-to-string($1)@end
expr:(format t\I<list>)=format-out($1)@end
expr:(funcall\W<expr>\W<list>)=@parenthesize{\I$1}($2)@end
expr:(if\I<expr><expr>\W<elsepart>)=\I@set{T;@out-column}\
	if ($1 )$2$3\N@tab{$T}  end if@end
expr:(incf <expr> <expr>\W)=$1 \:\= $1 \+ $2@end
expr:(incf <expr>\W)=$1 \:\= $1 \+ 1@end
expr:(integerp <expr>)=\Iinstance\?($1, \<integer\>)@end
expr:(labels\W(\W<fun-bindings>)\G<body>)=$1$2@end
expr:(length\I\W<expr>)=\Isize($1)@end
expr:(let\*\W(<bindings>)<body>)=$1$2@end
expr:(let\W(\W(\W\*<I>\* <expr>)\W)<body>)=fluid-bind ( \*$1\* \= $2 )$3\N\
	\tend fluid-bind@end
expr:(let\W(<bindings>)<body>)=$1$2@end
expr:(locally\I<body>)=begin $1\Iend@end
expr:(loop\I\W<id><body>)=%loop( $1$2 )@end
expr:(loop\I<body>)=@set{T;@out-column}\
	block (return) while (\#t)$1\N@tab{$T}  end while\; end block\;@end
expr:(make-instance <typeexpr><term><list>)=\Imake($1$2$3)@end
expr:(map <typeexpr>\W<expr>\W<list>)=\Imap-as($1, $2, $3)@end
expr:(mapc <expr>\W<list>)=\Ido($1, $2)@end
expr:(member\I\W<list>)=member\?($1)@end
expr:(multiple-value-bind (<list>)<expr><body>)=\Ibegin\n@tab{$T}  \
	let ($1) \= $2\;$3\n@tab{$T}  end\;\n@end
expr:(not\W(eq\I<expr>\W<expr>\W)\W)=\
	@parenthesize{$1} \~\=\= @parenthesize{$2}@end
expr:(not\I<expr>\W)=\I\~ @parenthesize{$1}@end
expr:(null\I<expr>\W)=\Inull\?($1)@end
expr:(progn\I<body>)=begin $1 end@end
expr:(push <expr>\W<expr>\W)=\Ipush\!($1, $2)@end
expr:(remove <expr>\W<expr>\W<term><list>)=\Iremove($2, $1$3$4)@end
expr:(remove-if-not <expr>\W<expr>\W)=\Ichoose($1, $2)@end
expr:(return-from <id><expr>\W)=return-from-$1($2)@end
expr:(round <expr>\W<expr>\W)=\Iround\/($1, $2)@end
expr:(send <expr> \:<I>\W)=$2\&($1)@end
expr:(send <expr> \:<I><term><list>)=$2\&($1$3$4)@end
expr:(setf <expr>\W<expr>\W)=$1 \:\= $2@end
expr:(some <expr>\W<expr>\W)=\Iany\?($1, $2)@end
expr:(symbolp <expr>)=\Iinstance\?($1, \<symbol\>)@end
expr:(truncate <expr>\W<expr>\W)=\Itruncate\/($1, $2)@end
expr:(typecase <expr><typecases>)=\I@set{T;@out-column}\
	select ( $1 by instance\? )$2\N@tab{$T}  end select@end
expr:(typep <expr>\W<typeexpr>)=\Iinstance\?($1, $2)@end
expr:(sys\:typep-structure-or-flavor <expr>\W<typeexpr>)=\Iinstance\?($1, $2)@end
expr:(unless\I<expr><body>)=\I@set{T;@out-column}\
	unless ($1)$2\N@tab{$T}  end unless@end
expr:(when\I<expr><body>)=\I@set{T;@out-column}\
	if ($1)$2\N@tab{$T}  end if@end
expr:(zerop <expr>)=\Izero\?($1)@end
expr:(1+ <expr>\W)=@parenthesize{$1} + 1@end
expr:(1- <expr>\W)=@parenthesize{$1} - 1@end

optfinal:<expr>=\n\tfinally $1\;@end
optfinal:=@end

bindings:<id>=let $1\;
bindings:(\W<id><expr>\W)=let $1 \= $2\;
bindings:<reqspace>=$1

condclauses:(\Wt\I\W<body>)=otherwise \=\> $1
condclauses:(<expr>\W<body>)=$1 \=\> $2
condclauses:<reqspace>=$1

caseclauses:(\Wt\I\W<body>)=otherwise \=\> $1
caseclauses:(\W(\W<const-list>)\W<body>)=$1 \=\> $2
caseclauses:(\W<const>\W<body>)=$1 \=\> $2
caseclauses:<reqspace>=$1

fun-bindings:(<id>\W<arglist><body>)=\Ilocal method $1 ($2)\
	@maybe-block{$1\d$3}\N\tend method $1\;
fun-bindings:<reqspace>=$1

iterators:(\W<id>\W<expr>\W<expr>)=$1 \= $2 then $3, ;
iterators:<reqspace>=$1

typecases:(\Wt\I\W<body>)=otherwise \=\> $1
typecases:(\W<type>\W<body>)=$1 \=\> $2
typecases:<reqspace>=$1

const-list:<const><term>=$1$2
const-list:<reqspace>=$1
const:<I>=\#\"$1\"@end
const:(<litlist>)=\#($1)@end
const::expr

typeexpr:\'\W<type>=$1@end
typeexpr::expr
type:<I>=\<$1\>@end

elsepart:(if\I<expr><expr>\W<elsepart>)= elseif ($1)$2$3@end
elsepart:<expr>= else $1@end
elsepart:<reqspace>=$1
elsepart:=@end

expr:=@fail

(proclaim '(\Wspecial <T>)\G\W)=\/\* $0 \*\/\s
(proclaim '(\Winline <T>)\G\W)=\/\* $0 \*\/\s

(defmacro <id>\W(<id>)\W<optdoc>\W<expr>)=\
    define constant $1 $3 = \/\* compile-time \*\/ method($2) $4 end method\;

(defmacro <id>\W(\W<macro-args>)<body>)=\
	define macro $1\n\t\{ $1($2) \} \=\>$3\Nend macro $1\;

macro-args:\&body <id>=\?$1\:body
macro-args:\&rest <id>=\?$1\:body
macro-args:\&<L>\I=\#$1
macro-args:(\W<id> <expr>\W)<term>=\?$1 \= $2$3
macro-args:(<macro-args>)=($1)
macro-args:<id><term>=\?$1$2
macro-args:<reqspace>=$1

(eval-when (<P>)<>)=$0

<U1><p3><g>=$0@err{\Nline @line\: invalid expression\: @cut{$0}\n}
cut:<U40>=$1...@end
cut:*=*
