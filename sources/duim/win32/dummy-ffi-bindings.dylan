Module:    win32-duim
Synopsis:  Dummy implementations of FFI bindings for loose mode
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*---*** andrewa: this doesn't work yet, unfortunately...
/// Dummy binding definers

define macro dummy-function-definer
  { define dummy-function ?name:name }
    => { define function ?name (#rest objects)
	   error(?"name" ## "cannot be invoked in loose mode!")
	 end;
         ignore(?name) }
end macro dummy-function-definer;

define macro dummy-slot-definer
  { define dummy-slot ?name:name }
    => { define dummy-function ?name;
         define dummy-function ?name ## "-setter" }
end macro dummy-slot-definer;

define macro dummy-class-definer
  { define dummy-class ?name:name ()
      ?dummy-slots:*
    end }
    => { define constant ?name = <LPCSCROLLINFO>;
         ignore(?name);
         ?dummy-slots }
 dummy-slots:
  { ?dummy-slot:*; ... } => { ?dummy-slot; ... }
  { } => { }
 dummy-slot:
  { dummy-slot ?name:name } => { #f } // define dummy-slot ?name }
end macro dummy-class-definer;


/// Dummy FFI bindings

define constant <C-Interface> = #f;

define dummy-function IMalloc/Free;

define constant <HRESULT> = <machine-word>;
define constant $NOERROR :: <HRESULT> = as(<HRESULT>,0);

define constant $BIF-RETURNONLYFSDIRS = #x0001;
define constant $BFFM-INITIALIZED     = 1;
define constant $BFFM-SETSELECTIONA   = ($WM-USER + 102);
define constant $BFFM-SETSELECTION    = $BFFM-SETSELECTIONA;

define macro <LPBFFCALLBACK>-callback-wrapper
  { <LPBFFCALLBACK>-callback-wrapper(?new:name,?old:name) }
    => { #f }
end macro <LPBFFCALLBACK>-callback-wrapper;

define macro <TIMERPROC>-callback-wrapper
  { <TIMERPROC>-callback-wrapper(?new:name,?old:name) }
    => { #f }
end macro <TIMERPROC>-callback-wrapper;

define dummy-class <XXX> ()
end dummy-class <XXX>;

define dummy-class <YYY> ()
  dummy-slot yyy-test
end dummy-class <YYY>;

define dummy-class <ZZZ> ()
  dummy-slot zzz-test
end dummy-class <ZZZ>;

define dummy-class <SHITEMID> ()
//  dummy-slot cb-value;
  dummy-slot abID-array;
end dummy-class <SHITEMID>;

define dummy-class <ITEMIDLIST> ()
  dummy-slot mkid-value;
end dummy-class <ITEMIDLIST>;

define dummy-class <BROWSEINFOA> ()
//  dummy-slot hwndOwner-value;
  dummy-slot pidlRoot-value;
  dummy-slot pszDisplayName-value;
  dummy-slot lpszTitle-value;
  dummy-slot ulFlags-value;
  dummy-slot lpfn-value;
//  dummy-slot lParam-value;
  dummy-slot iImage2-value;
end dummy-class <BROWSEINFOA>;

define constant <LPBROWSEINFOA> = <BROWSEINFOA>;

define dummy-function SHBrowseForFolder;
define dummy-function SHGetPathFromIDList;
define dummy-function SHGetMalloc;
*/
