
! a Dylan `export' clause is written to the file specified by
! the `-exports' option. 

! Copyright: 1996 Functional Objects, Inc.  All rights reserved.

! $Id: exports.pat,v 1.1 2004/03/12 00:09:32 cgay Exp $

@set{export-file;-}
@set{export-head;  export}
@set{export-tail;}
ARGV:\N-exports *\n=@set{export-file;$1}

! change underscores to hyphens:
map-name:\J_\J=-;?=?

@set-wrap{70;\t}
export:*=@set{x;@map-name{$1}}$x\
	@write{${export-file};${export-head}@wrap{ $x}}\
	@set{export-head;\,}@set{export-tail;\;\n}

export-end:=@write{${export-file};${export-tail;}}\
	@set{export-head;  export}@set{export-tail;}

\E=\N@export-end{}

