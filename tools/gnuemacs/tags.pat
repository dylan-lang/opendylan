
! gema patterns to generate an Emacs TAGS file from Dylan source files

! Use like this:
!	gema -f tags.pat -out TAGS *.dylan
!
! Then you can use the Emacs `Meta-.' command to find the definitions of
! Dylan identifiers.

! Normally this is used by the "dtags" script instead of directly.

! $Header: /scm/cvs/fundev/tools/gnuemacs/tags.pat,v 1.1 2004/03/12 00:42:11 cgay Exp $

! case insensitive:
@set-switch{i;1}
! literal characters:
@set-syntax{L;\-\.\(\)}
! discard unmatched input:
@set-switch{match;1}
! additional identifier characters:
@set-parm{idchars;\!\&\*\<\=\>\|\^\$\%\@\_\-\+\~\?\/}

! Write the name of each source file to `stderr' unless "-quiet" option used.
show-file:=@err{\ \ @inpath\n}
ARGV:-quiet =@define{show-file\:\=}

! initialize count and buffer at beginning of file
\B=@set{C;0}@set{buf;}@show-file{}
! write out buffer at end of file
\E=\f\n@inpath{},@length{${buf}}\n${buf}

! count the characters
upcount:<u>=@set{C;@add{$C;@length{$1}}}

! skip file header
\B<P>\:*\n<s>\n=@upcount{$0}

! skip comments
\/\/*\n=@upcount{$0}
\/\*<comment>\*\/=@upcount{$0}
comment:\/\*<comment>\*\/=$0
comment:\/\/*\n=$0

! skip constants
\"<string>\"=@upcount{$0}
string:\\?=\\?;?=?
\'<char>\'=@upcount{$0}
char:\\?=\\?@end;?=?@end

! skip macro definition templates
\{<template>\}=@upcount{$0}
template:\/\/*\n=$0
template:\/\*<comment>\*\/=$0
template:\{<template>\}=$0

! general definition
\Idefine<S><adj><I><S><I><delim>=@dodef{$0}

! operator definition
\Idefine<S>method<S>\\<-A1><i><delim>=@dodef{$0}
\Idefine<S>constant<S>\\<-A1><i><delim>=@dodef{$0}

! old prefix syntax
\N(define-\J<I><S><I><delim>=@dodef{$0}

adj:open<S>=$0
adj:sealed<S>=$0
adj:primary<S>=$0
adj:free<S>=$0
adj:abstract<S>=$0
adj:concrete<S>=$0
adj:inline<S>=$0
adj:not-inline<S>=$0
adj:default-inline<S>=$0
adj:may-inline<S>=$0
adj:inline-only<S>=$0
adj:\/\/*\n<s>=$0
adj:\/\*<comment>\*\/<s>=$0
! allow other adjectives, including "thread", "atomic", "locked", "sideways"...
adj:<J>\I<more-adj>=$1$2
adj:\Pvariable\I=@end
adj:\Pmethod\I=@end
adj:\Pclass\I=@end
adj:\Pconstant\I=@end
adj:\Pfunction\I=@end
adj:\Pgeneric\I=@end
adj:=@end
more-adj:\I\=\I=@fail
more-adj:\I\=\>\I=@fail
more-adj:<I>=$1
more-adj:<S>=$1
more-adj:\/\/*\n<s>=$0
more-adj:\/\*<comment>\*\/<s>=$0
more-adj:\Pvariable\I=@terminate
more-adj:\Pmethod\I=@terminate
more-adj:\Pclass\I=@terminate
more-adj:\Pconstant\I=@terminate
more-adj:=@fail



dodef:*\n<U0>=@upcount{*}@incr{C}
dodef:*=@append{buf;$1\d@line{},$C\n}@upcount{$1}

delim:\P\n=@end
delim:?=?@end

\Idefine<S>variable<s>\(<vars>=@dodef{$0}
vars:\)=\)@end
vars:\P\n=@end

\Idefine-fluid<s>(<s><I><delim>=@dodef{$0}

\N<s><slot-def><s><I><delim>=@dodef{$0}
slot-def:slot\I=slot@end
slot-def:setter\:=$0@end
slot-def:constant<S>=$0
slot-def:atomic<S>=$0
slot-def:locked<S>=$0
slot-def:=@fail
! following are used in C-FFI:
slot-def:array<S>=$0
slot-def:bitfield<S>=$0
slot-def:sealed<S>=$0
slot-def:inline<S>=$0
slot-def:not-inline<S>=$0
slot-def:default-inline<S>=$0
slot-def:may-inline<S>=$0
slot-def:inline-only<S>=$0
slot-def:member\I=$0@end
slot-def:pointer-type-name\:=$0@end
slot-def:address-getter\:=$0@end

! following are used in the compiler macro `define dood-class':
slot-def:weak<S>=$0
slot-def:lazy<S>=$0
slot-def:disk<S>=$0

\#<I>=@upcount{$0}
<I>=@upcount{$1}
\L<S>=@upcount{$1}
?=@incr{C}
