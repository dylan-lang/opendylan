Module:    Win32-user
Synopsis:  Declarations needed before "winuser.dylan".
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define C-pointer-type <LPHKL> => <HKL>;
define C-pointer-type <LPHWND> => <HWND>;

// Slot acessors defined both here and in "win32-controls":

define open-accessor hIcon-value;
define open-accessor hInstance-value;
define open-accessor hwnd-value;
define open-accessor hwndParent-value;
define open-accessor lParam-value;
define open-accessor pt-value;
define open-accessor hbmMask-value;
define open-accessor lpszText-value;
define open-accessor dwData-value;


// Slot accessors defined both here and in "win32-rich-edit":

define open-accessor hdc-value;
define open-accessor wParam-value;
define open-accessor cch-value;


// Slot accessors defined in both "win32-controls" and "win32-dialog":

define open-accessor hdr-value;


// Slot accessors defined both here and "win32-dialog" and "ole-controls":

define open-accessor hWndOwner-value;
define open-accessor lpszCaption-value;


// Slot accessors defined in both "win32-controls" and "ole-controls":

define open-accessor pszTitle-value;

// Defined here and in "win32-DDE":

define open-accessor length-value;
define open-accessor wFmt-value;

// Defined here and in "win32-shell"; later in "win32-controls" also:

define open-accessor fMask-value;

// Used in "winuser.h", "mmsystem.h", and "vfw.h":
define open-accessor dwStyle-value;
