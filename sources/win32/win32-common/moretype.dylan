Module:    Win32-common
Synopsis:  Additional declarations to be loaded after the automatically
	   converted files.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Macros in "winuser.h":

define function MAKEINTRESOURCE( n :: <integer> )
	=> value :: <LPTSTR>;
  make(<LPTSTR>, address: n)
end MAKEINTRESOURCE;


// In Win32, RECT and RECTL, although structurally equivalent, are distinct
// types for the sake of source code compatibility with Win16.  But since we
// have no intention of ever supporting Win16 here, it is simpler to
// unify them. 
define constant <RECTL> = <RECT>;
define constant <PRECTL> = <PRECT>;
define constant <LPRECTL> = <LPRECT>;
define constant <LPCRECTL> = <LPCRECT>;


// Null constants for some commonly used pointer types

define constant $NULL-HANDLE :: <HANDLE> = null-pointer( <HANDLE> );
define constant $NULL-HWND :: <HWND> = null-pointer( <HWND> );
define constant $NULL-RECT :: <LPRECT> = null-pointer( <LPRECT> );
define constant $NULL-POINT :: <LPPOINT> = null-pointer( <LPPOINT> );
define constant $NULL-VOID :: <C-void*> = null-pointer( <C-void*> );
define constant $NULL-string :: <C-string> = null-pointer( <C-string> );
define constant $NULL-HDC :: <HDC> = null-pointer(<HDC>);
define constant $NULL-HMENU :: <HMENU> = null-pointer(<HMENU>);
define constant $NULL-HINSTANCE :: <HINSTANCE> = null-pointer(<HINSTANCE>);


// The type designator <BOOLEAN-BYTE> corresponds to the C type BOOLEAN,
// renamed to avoid conflicting with the Dylan type <boolean>.

define inline-only function import-boolean
    (value :: <integer>) => (b :: <boolean>) 
  ~ zero?(value)
end;

define inline-only function export-boolean
    (value :: <boolean>) => (value :: <integer>)
  if (value) 1 else 0 end if
end;

define C-mapped-subtype <BOOLEAN-BYTE> (<C-BYTE>)
  map <boolean>,
    import-function: import-boolean,
    export-function: export-boolean;
  // pointer-type <PBOOLEAN>; // defined in "winnt.h" but never used.
end;

/* // not allowed because of sealing
define inline method c-type-cast
    (class == <BOOLEAN-BYTE>, value :: <object>) => (true? :: <boolean>)
  c-type-cast(<boolean>, value)
end method c-type-cast;
*/

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

/* // not allowed because of sealing
define inline method c-type-cast
    (class == <WCHAR>, value :: <object>) => (char :: <character>)
  c-type-cast(<character>, value)
end method c-type-cast;
*/

// Temporary for backward compatibility
define inline function %free(pointer :: <C-pointer>) => ();
  destroy(pointer);
  values()
end;

