Module:       WinSock2
Synopsis:     Hand-generated definitions to supplement the automatic translation.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define inline-only constant <ULONG> = <C-both-unsigned-long>;
define inline-only constant <DWORD> = <C-both-unsigned-long>;
define inline-only constant <LPDWORD> = <C-both-unsigned-long*>;
define inline-only constant <USHORT> = <C-unsigned-short>;
define inline-only constant <WORD>   = <C-unsigned-short>;
define inline-only constant <UCHAR> = <C-unsigned-char>;
define inline-only constant <CHAR> = <C-character>;
define inline-only constant <INT> = <C-int>;
define inline-only constant <BOOL> = <C-Boolean>;

define inline-only constant <LPSTR> = <C-string>;
define inline-only constant <LPWSTR> = <C-unicode-string>;
define inline-only constant <LPVOID> = <C-void*>;
define inline-only constant <PVOID> = <C-void*>;
define inline-only constant <C-BYTE*> = <C-unsigned-char*>;
define inline-only constant <LPINT> = <C-int*>;


define inline-only function import-wchar 
    (value :: <integer>) => (char :: <character>)
  as(<character>, value)
end;
define inline-only function export-wchar
    (value :: <character>) => (i :: <integer>)
  as(<integer>, value)
end;
define C-mapped-subtype <WCHAR> (<C-unsigned-short>)
  map <character>,
    import-function: import-wchar,
    export-function: export-wchar;
end;


define C-pointer-type <LPHANDLE> => <HANDLE>;

define inline-only constant $FFFFFFFF :: <machine-word> =
  as(<machine-word>, -1); // #xFFFFFFFF

define constant $NULL-HANDLE :: <HANDLE> = null-pointer(<HANDLE>);

define inline-only function MAKEWORD (low :: <integer>, high :: <integer>)
 => (value :: <integer>)
  logior(logand(low, #xFF), ash(logand(high, #xFF),8))
end MAKEWORD;


define method LOWORD ( n :: <integer> ) => value :: <integer>;
    logand(n,#xFFFF)
end LOWORD;

define method HIWORD ( n :: <integer> ) => value :: <integer>;
    logand( ash(n,-16), #xFFFF)  
end HIWORD;

define method LOWORD ( n :: <machine-word> ) => value :: <integer>;
  as(<integer>, %logand(n, as(<machine-word>, #xFFFF)))
end LOWORD;

define method HIWORD ( n :: <machine-word> ) => value :: <integer>;
  as(<integer>, u%shift-right(n,16))
end HIWORD;

define method MAKELONG (wLow :: <integer>,
                        wHigh :: <integer>)
 => value :: <ffi-integer>;
  let low :: <integer> = logand(wLow, #xFFFF);
  if ( wHigh > #x0FFF | wHigh < 0 )
    %logior(low, u%shift-left(as(<machine-word>, logand(wHigh, #xFFFF)), 16))
  else
    logior(low, ash(wHigh,16))
  end if
end MAKELONG;

define inline-only constant $ERROR-INVALID-HANDLE       =    6;
define inline-only constant $ERROR-NOT-ENOUGH-MEMORY    =    8;
define inline-only constant $ERROR-INVALID-PARAMETER    =   87;
define inline-only constant $ERROR-OPERATION-ABORTED    =  995;
define inline-only constant $ERROR-IO-INCOMPLETE        =  996;
define inline-only constant $ERROR-IO-PENDING           =  997;

define inline-only constant $WAIT-TIMEOUT = #x00000102;
define inline-only constant $WAIT-IO-COMPLETION = #x000000C0;

define C-struct <OVERLAPPED>
  sealed inline-only slot Internal-value :: <DWORD>;
  sealed inline-only slot InternalHigh-value :: <DWORD>;
  sealed inline-only slot offset-value   :: <DWORD>;
  sealed inline-only slot OffsetHigh-value :: <DWORD>;
  sealed inline-only slot hEvent-value   :: <HANDLE>;
  pointer-type-name: <LPOVERLAPPED>;
  c-name: "struct _OVERLAPPED";
end C-struct <OVERLAPPED>;
define sealed domain make (singleton(<LPOVERLAPPED>));
define sealed domain initialize (<LPOVERLAPPED>);
