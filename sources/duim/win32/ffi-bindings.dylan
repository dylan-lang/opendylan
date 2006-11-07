Module:    win32-duim
Synopsis:  Win32 back-end utilities
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Simple definitions for freeing things with a COM IMalloc interface.

define C-subtype <C-interface> ( <C-void*> ) end;
define C-pointer-type <C-interface*> => <C-interface>;

define C-function IMalloc/Free
  input parameter This :: <C-interface>;
  input parameter pv :: <C-void*>;
  c-name: "C_IMalloc_Free";
end;


/// Definitions for access to some Win32 Shell API functionality
///--- Maybe put in some win32-XXX library if we ever use more related stuff.

define constant <HRESULT> = <machine-word>;
define constant $NOERROR :: <HRESULT> = as(<HRESULT>,0);

define constant <C-HRESULT> = <C-raw-signed-long>;

define constant $BIF-RETURNONLYFSDIRS = #x0001;
define constant $BFFM-INITIALIZED     = 1;
define constant $BFFM-SETSELECTIONA   = ($WM-USER + 102);
define constant $BFFM-SETSELECTION    = $BFFM-SETSELECTIONA;

define constant <LPBFFCALLBACK> = <C-function-pointer>;

define macro <LPBFFCALLBACK>-callback-wrapper
  { <LPBFFCALLBACK>-callback-wrapper(?new:name,?old:name) }
    => { <WNDPROC>-callback-wrapper(?new,?old) }
end macro <LPBFFCALLBACK>-callback-wrapper;

define C-struct <SHITEMID>              // mkid  
  slot cb-value :: <USHORT>;            // size of identifier, including cb itself 
  array slot abID-array :: <C-BYTE>, length: 1,
        address-getter: abID-value;
  pointer-type-name: <LPSHITEMID>;
  c-name: "struct _SHITEMID";
end C-struct <SHITEMID>;
define constant <LPCSHITEMID> = <LPSHITEMID>; 

ignore(cb-value, cb-value-setter,
       abID-array, abID-array-setter, abID-value,
       <LPCSHITEMID>);

define C-struct <ITEMIDLIST>            // idl  
  slot mkid-value :: <SHITEMID>;        // list of item identifers 
  pointer-type-name: <LPITEMIDLIST>;
  c-name: "struct _ITEMIDLIST";
end C-struct <ITEMIDLIST>;
define constant <LPCITEMIDLIST> = <LPITEMIDLIST>;
 
ignore(mkid-value, mkid-value-setter,
       <LPCITEMIDLIST>);

define C-struct <BROWSEINFOA>
  slot hwndOwner-value :: <HWND>;
  slot pidlRoot-value :: <LPCITEMIDLIST>;
  slot pszDisplayName-value :: <LPSTR>;
  slot lpszTitle-value :: <LPCSTR>;
  slot ulFlags-value :: <UINT>;
  slot lpfn-value :: <LPBFFCALLBACK>;
  slot lParam-value :: <LPARAM>;
  slot iImage2-value :: <INT>;
  // slot iImage-value :: <INT>;        // this name is sealed in win32-controls
  pointer-type-name: <PBROWSEINFOA>;
  c-name: "struct _browseinfoA";
end C-struct <BROWSEINFOA>;
define constant <LPBROWSEINFOA> = <PBROWSEINFOA>;
define constant <LPBROWSEINFO>  = <LPBROWSEINFOA>;

ignore(hwndOwner-value, hwndOwner-value-setter,
       pidlRoot-value, pidlRoot-value-setter,
       pszDisplayName-value, pszDisplayName-value-setter,
       lpszTitle-value, lpszTitle-value-setter,
       ulFlags-value, ulFlags-value-setter,
       lpfn-value, lpfn-value-setter,
       lParam-value, lParam-value-setter,
       iImage2-value, iImage2-value-setter);

define C-function SHBrowseForFolder
  parameter lpbi :: <LPBROWSEINFOA>;
  result value :: <LPITEMIDLIST>;
  c-name: "SHBrowseForFolderA", c-modifiers: "__stdcall";
end;

define C-function SHGetPathFromIDList
  parameter pidl :: <LPCITEMIDLIST>;
  parameter pszPath :: <LPSTR>;
  result value :: <BOOL>;
  c-name: "SHGetPathFromIDListA", c-modifiers: "__stdcall";
end;

define C-function SHGetMalloc
  output parameter ppmalloc :: <C-Interface*>;
  result value :: <C-HRESULT>;
  c-name: "SHGetMalloc", c-modifiers: "__stdcall";
end;


/// Timer proc

define macro <TIMERPROC>-callback-wrapper
  { <TIMERPROC>-callback-wrapper(?new:name,?old:name) } =>
    { define C-callable-wrapper ?new of ?old
        parameter hWnd :: <HWND>;
        parameter uMsg :: <UINT>;
        parameter idEvent :: <UINT>;
        parameter dwTime :: <DWORD>;
        c-modifiers: "__stdcall";
      end C-callable-wrapper }
end;
