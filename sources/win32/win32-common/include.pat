! patterns for using "gema" to expand "%include" directives

! Copyright 1996 Functional Objects, Inc.  All rights reserved.

! case-insensitive
@set-switch{i;1}

\N\%include <F>\W\n=@read{$1}

\B\Wmodule\:#\N\W\n=$0\n\
	\/\* Automatically generated from \"@file\"\; do not edit. \*\/\n\n

\I\%comment\I*\n=\N

\N\%<I>=$0@err{unrecognized directive\: $0\n}@exit-status{1}

