Module:    OLE-std-util
Synopsis:  Manually coded declarations for a few special cases.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// These are the only things needed from the OLE library so just define
// them here instead of dragging in the whole library.
define constant <LPOLEOBJECT> = <Interface>;
define constant <LPOLECLIENTSITE> = <Interface>;

//--

// From "winreg.h":
// (Don't use C-subtype so that it won't be disjoint from the real <HKEY>.)
define constant <HKEY> = <HANDLE>;

//--

/*
define C-address pG_szDbgPrefix :: <C-string>
  c-name: "g_szDbgPrefix";
end;
*/

// This was originaly a macro in "olestd.h":
define method OLEDBGDATA-MAIN(szPrefix :: <string>) => ();
/*
  %memcpy(pG_szDbgPrefix, TEXT(szPrefix), max(size(szPrefix),60) + 1);
*/
  values()
end;

//--

// This is the only thing needed from OLEDLG.H, so just do a dummy
// definition here.
define constant <LPOLEUIPASTEENTRY> = <C-void*>;


// Other pointer types referenced by translated code:
define constant <LPLPMONIKER> = <Interface*>;
