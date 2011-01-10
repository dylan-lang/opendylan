Module:    Ole-Controls
Synopsis:  Manually written code to supplement the mechanical FFI declarations.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Constants from "winerror.h" that are not exported from the Dylan
// code because not used anywhere other than in "olectl.h".

define inline-only constant $SEVERITY-SUCCESS = 0;
define inline-only constant $SEVERITY-ERROR = 1;

// macros in "olectl.h":

// define constant $CLASSFACTORY-E-FIRST = $CLASS-E-NOAGGREGATION;

define function STD-CTL-SCODE (n :: <integer> ) => (scode :: <SCODE>);
  MAKE-SCODE($SEVERITY-ERROR, $FACILITY-CONTROL, n)
end;

define constant CUSTOM-CTL-SCODE = STD-CTL-SCODE;


// temporary hack until exported by "win32-gdi" library:	???
define constant <TEXTMETRICW> = <C-void>;

// probably these should be exported from "ole-automation":
// define C-pointer-type <LPLPTYPEINFO> => <LPTYPEINFO>;
define constant <LPCY> = <PLARGE-INTEGER>;

// Other pointer-pointer names that do not appear in the original header
// file, but references are created by the translation patterns:
define constant <LPSHORT> = <PSHORT>;
define constant <LPLRESULT> = <C-both-long*>;
define C-pointer-type <LPHDC> => <HDC>;
define C-pointer-type <LPHFONT> => <HFONT>;
define constant <LPLCID> = <PLCID>;
define constant <LPOLE-HANDLE> = <LPUINT>;
define constant <LPOLE-XSIZE-HIMETRIC> = <C-long*>;
define constant <LPOLE-YSIZE-HIMETRIC> = <C-long*>;
define constant <LPLPCONNECTIONPOINT> = <Interface*>;
// define constant <LPLPENUMCONNECTIONPOINTS> = <Interface*>;
// define constant <LPLPENUMCONNECTIONS> = <Interface*>;
