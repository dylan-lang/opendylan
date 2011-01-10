Module: Ole-Dialogs
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// These didn't get translated because "patterns.pat" thought they
// were function aliases:

define constant $IDD-SERVERNOTREG	= $IDD-SERVERNOTREGA;
define constant $IDD-LINKTYPECHANGED	= $IDD-LINKTYPECHANGEDA;

// Temporary hack until these can be imported from the 
// Win32-Controls library (file prsht):			???
define constant <PROPSHEETHEADERA> = <C-void>;
define constant <PROPSHEETHEADERW> = <C-void>;

// These are the only definitions needed from the OLE library, so just
// define them here temporarily.			???
define constant <LPOLEOBJECT> = <Interface>;
define constant <LPOLECLIENTSITE> = <Interface>;
define C-pointer-type <LPHWND> => <HWND>;

define C-pointer-type <LPHMENU> => <HMENU>;
