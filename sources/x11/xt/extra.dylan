Module:   Xt
Synopsis: Hand-generated definitions to supplement the automatic translation.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The type designator <X-Boolean> corresponds to the X type Boolean,
// renamed to avoid conflicting with the Dylan type <boolean>.
// Note that it packs differently than a <C-boolean>.

define inline-only function import-boolean
    (value :: <integer>) => (b :: <boolean>) 
  ~ zero?(value)
end;

define inline-only function export-boolean
    (value :: <boolean>) => (value :: <integer>)
  if (value) 1 else 0 end if
end;

define C-mapped-subtype <X-Boolean> (<C-char>)
  map <boolean>,
    import-function: import-boolean,
    export-function: export-boolean;
  pointer-type <X-Boolean*>;
end;


define inline constant <unsigned-32-bits> =
  type-union(<integer>, <machine-word>);

// pointers to scalars
define C-pointer-type <Widget*> => <Widget>;
define C-pointer-type <XrmDatabase*> => <XrmDatabase>;
define C-pointer-type <XrmHashBucket*> => <XrmHashBucket>;
define C-pointer-type <XrmRepresentation*> => <XrmRepresentation>;
define C-pointer-type <XtActionList*> => <XtActionList>;
define C-pointer-type <XtActionProc*> => <XtActionProc>;
define C-pointer-type <XtAppContext*> => <XtAppContext>;
define C-pointer-type <XtCacheRef*> => <XtCacheRef>;
define C-pointer-type <XtPointer*> => <XtPointer>;
define C-pointer-type <XtResourceList*> => <XtResourceList>;

// pointers to pointers
define C-pointer-type <XIconSize**> => <XIconSize*>;
define C-pointer-type <XStandardColormap**> => <XStandardColormap*>;
define C-pointer-type <Widget**> => <Widget*>; // used in "Xm/DropSMgr.h"
define inline constant <X-String*> = <C-string*>;


// enumeration type in "Xutil.h":
define inline constant <XrmBindingList> = <C-int*>;

// array type in "Xresource.h":
define C-pointer-type <XrmSearchList> => <XrmHashTable>;

// macros in "Intrinsic.h":
define inline-only function XtSetArg (arg :: <Arg*>, n, d) => ()
  arg.name-value := n;
  arg.value-value := c-type-cast(<XtArgVal>,d);
end;

// macros in "Xresource.h":
define inline-only function XrmStringsEqual (a1 :: <string>, a2 :: <string>)
  => ( equal? :: <boolean>)
  a1 = a2
end;

