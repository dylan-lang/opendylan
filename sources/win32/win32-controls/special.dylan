Module:    Win32-controls
Synopsis:  Manually coded additions to the automatic translation.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The following macros are one-of-a-kind, so it is easier to just hand code
// the Dylan equivalent instead of trying to define translation patterns.

define inline function FORWARD-WM-NOTIFY (hwnd :: <HWND>, idFrom :: <integer>,
					  pnmhdr :: <LPNMHDR>,
					  fn :: <function>);
      fn(hwnd, $WM-NOTIFY, idFrom, pnmhdr.pointer-address)
    end;

define method ListView-GetItemRect(hwnd :: <HWND>, i, prc :: <LPRECT>,
				   code :: <integer>) => ok :: <boolean>;
  0 ~= SendMessage(hwnd, $LVM-GETITEMRECT, i,
		   if ( ~ null-pointer?(prc) )
		     prc.left-value := code;
		     prc.pointer-address
		   else
		     0
		   end if )
end;

define method ListView-SetItemPosition32(hwndLV :: <HWND>, i, x, y) => ();
  with-stack-structure ( ptNewPos :: <LPPOINT> )
    ptNewPos.x-value := x;
    ptNewPos.y-value := y;
    SendMessage(hwndLV, $LVM-SETITEMPOSITION32, i, ptNewPos.pointer-address)
  end with-stack-structure;
  values()
end;

define method TreeView-GetItemRect(hwnd :: <HWND>, hitem :: <C-pointer>,
				   prc :: <LPRECT>, code)
 => result :: <boolean>;
  pointer-value(pointer-cast(<C-void**>, prc)) := hitem;
  0 ~= SendMessage(hwnd, $TVM-GETITEMRECT, code, prc.pointer-address)
end;

define method PropSheet-SetTitle(hDlg :: <HWND>, wStyle :: <integer>,
				 lpszText :: <LPCTSTR>) => ();
  SendMessage(hDlg, $PSM-SETTITLE, wStyle, lpszText.pointer-address);
  values()
end;


// The real definition is in OLE, which we don't want to depend on here:
define constant <LPSTREAM> = <C-pointer>;

// real definition is in "winreg.h":
define constant <HKEY> = <HANDLE>;

define C-pointer-type <LPHPROPSHEETPAGE> => <HPROPSHEETPAGE>;

// This seems to have been mistakenly omitted from COMMCTRL.H of "Sep 21 1997"
define C-struct <TBNOTIFYA>
  slot hdr-value   :: <NMHDR>;
  slot iItem-value :: <C-int>;
  slot tbButton-value :: <TBBUTTON>;
  slot cchText-value :: <C-int>;
  slot pszText-value :: <LPSTR>;
  pointer-type-name: <LPTBNOTIFYA>;
  c-name: "struct tagTBNOTIFYA";
end C-struct <TBNOTIFYA>;

// Not defined or used in the header file; defined here for user's convenience
// and for compatibility with the beta version of this library.
define inline constant <LPHD-ITEM> = <LPHD-ITEMA>;
define inline constant <LPLV-COLUMN> = <LPLV-COLUMNA>;
define inline constant <LPLV-FINDINFO> = <LPLV-FINDINFOA>;
define inline constant <LPLV-ITEM> = <LPLV-ITEMA>;
define inline constant <LPTC-ITEM> = <LPTC-ITEMA>;
