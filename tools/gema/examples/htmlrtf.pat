! translate HTML to RTF for use as Windows help

! Example:
!    gema -f htmlrtf.pat -makefile foo.hpj -helpfile foo.hlp \
!	-title 'the world of foo' foo1.html foo2.html ...
! That produces the files "foo.hpj", "contents.rtf", and all of the "foo*.rtf"
! files.  Then on Windows, run the command:
!    HC31 foo.hpj
! to produce a help file "foo.hlp" compatible with Windows 3.1, or load the
! ".hpj" file into "HCW" to produce a "foo.hlp" file for Windows 95 or NT.
! (The help compiler comes with Visual C++ or the Windows SDK.)

! Problems:
!   * Emits redundant topic and browse sequence for <H1> heading followed
!     by <H2> heading

! Note: the generated options "LCID" and "HLP" are used only by the Win32
! version of the help compiler, so the "-helpfile" option has no effect for
! Windows 3.1 help files.


!==  General options  ==

! case insensitive
@set-switch{i;1}
! use [ ] instead of < >, / is also literal.
@set-syntax{\<\>LLL;\[\]\<\>\/}

!==  Characters needing special handling  ==

! newline is a word separator in HTML, but is ignored in RTF.
! The variable $S is being used to indicate whether a newline needs to
! be accompanied by an explicit space.
\t=\s@set{S;}
\f=\S@set{S;}
^M^J=$S\n@set{S;}
\n\L[S]=\n\s@set{S;}
\n=$S\N@set{S;}
\s\L\W=\s@set{S;}
[I]=$1@set{S;\s}
?=?@set{S;\s}

\\=\\\\@set{S;\s}
\{=\\\{@set{S;\s}
\}=\\\}@set{S;\s}
\&lt\;=\<@set{S;\s}
\&gt\;=\>@set{S;\s}
\&amp\;=\&@set{S;\s}
\&quot\;=\"@set{S;\s}
\&\#[D]\;=\\\'@fill-right{00;@radix{10;16;$1}}@set{S;\s}
\xA0=\\\'A0
[-T1]=\\\'@fill-right{00;@radix{10;16;@char-int{$1}}}@set{S;\s}

\&nbsp\;=\\\'A0
\&copy\;=\\\'A9@set{S;\s}
\&reg\;=\\\'AE@set{S;\s}
\&trade\;=\\\'99@set{S;\s}
\&sect\;=\\\'A7@set{S;\s}
\&middot\;=\\\'B7@set{S;\s}
\&frac14\;=\\\'BC@set{S;\s}
\&frac12\;=\\\'BD@set{S;\s}
\&frac34\;=\\\'BE@set{S;\s}
\`\`=\\\'93
\'\'=\\\'94@set{S;\s}
\`[I]\'=\\\'91$1\\\'92@set{S;\s}
<SUP>2</SUP>=\\\'B2@set{S;\s}
<SUP>3</SUP>=\\\'B3@set{S;\s}
<SUP>1</SUP>=\\\'B9@set{S;\s}

!==  Setup at beginning and end of file  ==

@set{t;1}! font number for typewriter font

rtf-heading:=\{\\rtf1\\windows\\deff0\n\
	\{\\fonttbl\n\
	\{\\f0\\froman Times New Roman\;\}\n\
	\{\\f$t\\fdecor Courier New\;\}\n\
	\{\\f2\\fswiss Arial\;\}\n\
	\{\\f5\\fmodern Courier\;\}\}\n\
	\\fs24\\sb120\n

\B=@rtf-heading{}\
	\@\{\\footnote Converted from \"@inpath\" dated @just-date{@file-time}\}\n\
	@set{end-sect;}@set{end-topic;}@set{end-main;}@set{S;}\
	@set{indent;0}\
	@bind{O;@outpath}\
	@write{${makefile};@make-heading{${helpfile}}\
		@relative-path{${makefile};$O}\n}\
	@err{\s\s$O\n}@unbind{O}\
	\#\{\\footnote @context-string{@downcase{@file}}\}\n

! note: ${indent} is in "twips" = 1/20 point  (1,440 per inch)

just-date:[L] [L] [D] [D]\:[D]\:[D] [D]=$2 $3\, $7
just-date:\S=

\E=\N\}\n

<HEAD>=
</HEAD>=
<BODY>=
</BODY>=
<HTML>=
</HTML>=\N\}\n@end

! comments
<\!--\ Converted\ from\L *-->=\N\@\{\\footnote Converted from *\}\n
<\!--\ \$\L*\$\ -->=\N\@\{\\footnote \$*\$\}\n
<\!--*-->=

!==  Paragraph formatting  ==

<P>\W\P</DL>=
<P>\W[extra-par]=\N\\par\n@set{S;}
extra-par:<P>\W=;=@end

<PRE>[pre]</PRE>\W=\N\\par\\keep\{\\f$t\n$1\}\
	\N\\par\\pard\\sb120\\li${indent}\n@set{S;}
pre:\A\L\W\n=
pre:\n\Z=
pre:\n=\\line\n
pre:^M^J=\\line\n
pre:\L[S]=$1
pre:<P>\L\W=\\par\n
pre:<BR>\L\W=\\line\n
pre::

<BR>\W=\N\\line\n@set{S;}
inc-indent:=@set{indent;@add{${indent};360}}@set{S;}
dec-indent:=@set{indent;@sub{${indent};360}}@set{S;}
<BLOCKQUOTE>=@inc-indent{}\N\\par\\li${indent}\\ri${indent}\s@set{S;}
</BLOCKQUOTE>=@dec-indent{}\N\\par\\li${indent}\\ri${indent}\s@set{S;}

!==  Links  ==

<A NAME\W\=\W[context-string]>\W#</A>=\
	\#\{\\footnote @context-string{@downcase{@file}\#}$1\}$2\S@set{S;}
<A HREF\W\=\W[U]>\G\W#</A>=\
 	\{\\uldb $2\}\{\\v @context-string{@relative-path{$1;$1}}\}
context-string:\"[context-string]\"=$1
context-string:\A\.=_
context-string:\.[I]\#=__
context-string:\#=__
context-string:\A\#=@context-string{@downcase{@file}\#}
context-string:\S=_;[Y1]=_

! automatically generated man page link not relevant here:
<A\ HREF\=\"[I].[D].html"><[I]>$1</$3>($2)</A>=@{<$3>$1</$3>($2)}

!==  Headings ==

<TITLE>\W#</TITLE>[maybe-page]=@cmps{$2;;;;@topic{#}@browse{}}\
	@write{${contents};\N\\par\n}\
	@contents-entry{$1}\
	\N\\par\{\\fs36\\b $1\}\\par\s@set{S;}
<H1>\W#</H1>[maybe-page]\W=\N${end-main}\
	@cmps{$2;;;;@topic{#}@browse{}}@set{end-main;$2}\
	@contents-entry{\s\s$1}\
	\N\\par\{\\fs36 $1\}\\par\n@set{S;}

@set{contents-tag;c0}
contents-entry:[s][strip-tags]=@incr{contents-tag}\
	\N\#\{\\footnote ${contents-tag}\}\n\
 	@write{${contents};$1$1\{\\uldb $2\}\{\\v ${contents-tag}\}\\line\n}

! don't start a new topic if this section is trivially short.
maybe-page:\P\W[u99]<H[D1]>=@end
maybe-page:=\\page\n@end

<H2>\W#</H2>[maybe-page]=\N${end-topic}\
	@cmps{$2;;;;@topic{#}@browse{}}@set{end-topic;$2}\
	@contents-entry{\s\s\s\s$1}\
	\N\\par\{\\fs32\\b $1\}\\par\s@set{S;}

<H3>\W#</H3>=\N${end-sect}@topic{#}@set{end-sect;\\sect\n}\
	\N\\par\{\\fs28\\b $1\}\\par\s@set{S;}

<H4>\W#</H4>=\N\\par\{\\b $1\}\\par\s@set{S;}
<H[D1]>\W#</H$1>=\N\\par $2\\par\s@set{S;}

topic:\A=\$\{\\footnote\s
topic:\Z=\}
topic:\#\{[matchbrace]\}=
topic:\S=\S
topic:\\line[s1]=\S
browse:=@wrap{\+\{\\footnote browsetopic\}}
matchbrace:\{#\}=$0
matchbrace:\\?=\\?

strip-tags:\#\{[matchbrace]\}=
strip-tags:\{\\uldb\s#\}\{\\v [matchbrace]\}=$1
strip-tags:\\line\I[s1]=
strip-tags:\S=\S
strip-tags:\\?=\\?
strip-tags:\{#\}=$0

!==  Fonts ==

<B>=\\b\s
</B>=\\b0\s
<I>=\\i\s
</I>=\\i0\s
<EM>=\\i\s
</EM>=\\i0\s
<VAR>=\\i\s
</VAR>=\\i0\s
<STRONG>=\\b\s
</STRONG>=\\b0\s
<TT>#</TT>=\{\\f$t #\}
<CODE>#</CODE>=\{\\f$t #\}
<SAMP>#</SAMP>=\{\\f$t #\}
<KBD>=\\scaps\s
</KBD>=\\scaps0\s
<ADDRESS>=\S
</ADDRESS>=\S

! comment:  @{\footnote ...}
! font size in half-points:  \fs24

!==  Lists ==

<MENU>=@inc-indent{}\N\\par\\tx${indent}\\li${indent}\\fi-360\s@bind{label;.}
</MENU>=@dec-indent{}\N\\par\\pard\\sb120\n@unbind{label}
<UL>=@inc-indent{}\N\\par\\tx${indent}\\li${indent}\\fi-360\s@bind{label;.}
<LI>\L\W=\\par@put-label{${label}}\\tab\s@set{S;}
put-label:\A.\Z=\{\\f0\\\'95\}! bullet
put-label:[N]=\I$1@incr{label}
</UL>=@dec-indent{}\N\\par\\pard\\sb120\n@unbind{label}
<OL>=@inc-indent{}\N\\par\\tx${indent}\\li${indent}\\fi-360\s@bind{label;1}
</OL>=@dec-indent{}\N\\par\\pard\\sb120\s@unbind{label}

<DL>\L\W=@bind{dti;${indent}}@bind{ddi;@add{${dti};1000}}
<DT>\L\W=@set{indent;${dti}}\N\\par\\li${indent}\s@set{S;}
<DD>\L\W=@set{indent;${ddi}}\N\\par\\li${indent}\s@set{S;}
</DL>\W[extra-par]=@set{indent;${dti}}\N\\par\\li${indent}\s\
	@unbind{dti}@unbind{ddi}

!==  Additional files and file options  ==

@set{makefile;myhelp.hpj}
@set{helpfile;myhelp.hlp}
@set{contents;contents.rtf}
ARGV:-makefile [G]\n=@set{makefile;$1}
ARGV:-helpfile [G]\n=@set{helpfile;$1}
ARGV:-contents [G]\n=@set{contents;$1}
ARGV:-title *\n=@set{title;TITLE\=*\n}
make-heading:[G]=\
	\[OPTIONS\]\n\
	COMPRESS\=HIGH\n\
	LCID\=0x409 0x0 0x0 \;U.S. English\n\
	REPORT\=Yes\n\
	${title;}\
	HLP\=@relative-path{${makefile};${helpfile}}\n\n\
	\[CONFIG\]\n\
	BrowseButtons()\n\n\
	\[FILES\]\n\
	@relative-path{${makefile};${contents}}\n\
   @err{\s\s${makefile}\n\s\s${contents}\n}\
   @set{helpfile;}\
   @write{${contents};@rtf-heading{}@topic{Contents}@browse{}\n}

@ARGV{-odir\n.\n}
@ARGV{-otyp\n.rtf\n}
ARGV:\Z=@write{${contents};\N\}\n}

!==  Default rules for unsupported tags  ==

<[g]>=@err{@file, line @line, unrecognized\: $0\n}
\&[I]\;=@err{@file, line @line, unrecognized\: $0\n}


