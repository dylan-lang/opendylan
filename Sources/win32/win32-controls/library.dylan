module:    dylan-user	
Synopsis:  Win32 API for Windows Common Controls corresponding to
	   "COMMCTRL.H" and "PRSHT.H"
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Automatically generated from "library.src"; do not edit. */


define library Win32-controls
  use Dylan;
  use C-FFI;
  use Win32-common;
  use Win32-user;
  export Win32-controls;
end;

define module Win32-controls
  use Dylan;
  use C-FFI;
  use Win32-common,
    // export here structure accessors in both modules.
    export: {cx-value, cx-value-setter, cy-value, cy-value-setter,
	     x-value, x-value-setter, y-value, y-value-setter,
	     flags-value, flags-value-setter,
	     dwFlags-value, dwFlags-value-setter,
	     dwSize-value, dwSize-value-setter, u-value, u-value-setter};
  use Win32-user,
    export: {// shared accessors:
	     cbSize-value, cbSize-value-setter,
	     hIcon-value, hIcon-value-setter, hInstance-value,
	     hInstance-value-setter, hwnd-value, hwnd-value-setter,
	     hwndParent-value, hwndParent-value-setter, lParam-value,
	     lParam-value-setter, pt-value, pt-value-setter,
	     hdr-value, hdr-value-setter,
	     <NMHDR>, <LPNMHDR>, hwndFrom-value, idFrom-value, code-value,
	     hwndFrom-value-setter, idFrom-value-setter, code-value-setter,
	     hbmMask-value, hbmMask-value-setter,
	     pszTitle-value, pszTitle-value-setter,
	     lpszText-value, lpszText-value-setter,
	     dwData-value, dwData-value-setter,
	     // types used in functions here:
	     <WINDOWPOS>, <LPWINDOWPOS>, <PWINDOWPOS>, 
	     hwndInsertAfter-value, hwndInsertAfter-value-setter,
	     <DLGPROC>, <LPCDLGTEMPLATE>, style-value, style-value-setter,
	     dwExtendedStyle-value, dwExtendedStyle-value-setter,
	     cdit-value, cdit-value-setter, SendMessage };
 

  // from "commctrl.h":
  export InitCommonControls, $ODT-HEADER, $ODT-TAB, $ODT-LISTVIEW,
	$LVM-FIRST, $TV-FIRST, $HDM-FIRST, $TCM-FIRST, HANDLE-WM-NOTIFY,
	$NM-OUTOFMEMORY, $NM-CLICK, $NM-DBLCLK, $NM-RETURN, $NM-RCLICK,
	$NM-RDBLCLK, $NM-SETFOCUS, $NM-KILLFOCUS;
  export $NM-FIRST, $NM-LAST, $LVN-FIRST, $LVN-LAST, $HDN-FIRST,
	$HDN-LAST, $TVN-FIRST, $TVN-LAST, $TTN-FIRST, $TTN-LAST, $TCN-FIRST,
	$TCN-LAST, $CDN-FIRST, $CDN-LAST, $TBN-FIRST, $TBN-LAST, $UDN-FIRST,
	$UDN-LAST, $MSGF-COMMCTRL-BEGINDRAG, $MSGF-COMMCTRL-SIZEHEADER,
	$MSGF-COMMCTRL-DRAGSELECT, $MSGF-COMMCTRL-TOOLBARCUST, $CLR-NONE,
	$CLR-DEFAULT, <HIMAGELIST>, $ILC-MASK, $ILC-COLOR, $ILC-COLORDDB,
	$ILC-COLOR4, $ILC-COLOR8, $ILC-COLOR16, $ILC-COLOR24, $ILC-COLOR32,
	$ILC-PALETTE, ImageList-Create, ImageList-Destroy,
	ImageList-GetImageCount, ImageList-Add, ImageList-ReplaceIcon,
	ImageList-SetBkColor, ImageList-GetBkColor,
	ImageList-SetOverlayImage, ImageList-AddIcon, $ILD-NORMAL,
	$ILD-TRANSPARENT, $ILD-MASK, $ILD-IMAGE, $ILD-BLEND25, $ILD-BLEND50,
	$ILD-OVERLAYMASK, INDEXTOOVERLAYMASK, $ILD-SELECTED, $ILD-FOCUS,
	$ILD-BLEND, $CLR-HILIGHT, ImageList-Draw;
  export ImageList-Replace, ImageList-AddMasked, ImageList-DrawEx,
	ImageList-Remove, ImageList-GetIcon, ImageList-LoadImage,
	ImageList-BeginDrag, ImageList-EndDrag, ImageList-DragEnter,
	ImageList-DragLeave, ImageList-DragMove,
	ImageList-SetDragCursorImage, ImageList-DragShowNolock,
	ImageList-GetDragImage, ImageList-RemoveAll, ImageList-ExtractIcon,
	ImageList-LoadBitmap, ImageList-Read, ImageList-Write,
	hbmImage-value, hbmImage-value-setter, Unused1-value,
	Unused1-value-setter, Unused2-value, Unused2-value-setter,
	rcImage-value, rcImage-value-setter, <IMAGEINFO>, <LPIMAGEINFO>,
	ImageList-GetIconSize, ImageList-SetIconSize, ImageList-GetImageInfo,
	ImageList-Merge;
  export $WC-HEADER, $HDS-HORZ, $HDS-BUTTONS, $HDS-HIDDEN, <HDITEMA>,
	<HD-ITEM>, mask-value, mask-value-setter, cxy-value,
	cxy-value-setter, pszText-value, pszText-value-setter, hbm-value,
	hbm-value-setter, cchTextMax-value, cchTextMax-value-setter,
	fmt-value, fmt-value-setter, <HD-ITEMA>, <LPHD-ITEMA>, <LPHDITEMA>;
  export <HDITEM>, <LPHDITEM>;
  export $HDI-WIDTH, $HDI-HEIGHT, $HDI-TEXT, $HDI-FORMAT, $HDI-LPARAM,
	$HDI-BITMAP, $HDF-LEFT, $HDF-RIGHT, $HDF-CENTER, $HDF-JUSTIFYMASK,
	$HDF-RTLREADING, $HDF-OWNERDRAW, $HDF-STRING, $HDF-BITMAP,
	$HDM-GETITEMCOUNT, Header-GetItemCount, $HDM-INSERTITEM,
	Header-InsertItem, $HDM-DELETEITEM, Header-DeleteItem, $HDM-GETITEM,
	Header-GetItem, $HDM-SETITEM, Header-SetItem, <HDLAYOUT>;
  export prc-value, prc-value-setter, pwpos-value, pwpos-value-setter,
	<HD-LAYOUT>, <LPHD-LAYOUT>, <LPHDLAYOUT>;
  export $HDM-LAYOUT, Header-Layout, $HHT-NOWHERE, $HHT-ONHEADER,
	$HHT-ONDIVIDER, $HHT-ONDIVOPEN, $HHT-ABOVE, $HHT-BELOW, $HHT-TORIGHT,
	$HHT-TOLEFT, <HDHITTESTINFO>;
  export iItem-value, iItem-value-setter, <HD-HITTESTINFO>,
	<LPHD-HITTESTINFO>, <LPHDHITTESTINFO>;
  export $HDM-HITTEST, $HDN-ITEMCHANGINGA, $HDN-ITEMCHANGINGW,
	$HDN-ITEMCHANGEDA, $HDN-ITEMCHANGEDW, $HDN-ITEMCLICKA,
	$HDN-ITEMCLICKW, $HDN-ITEMDBLCLICKA, $HDN-ITEMDBLCLICKW,
	$HDN-DIVIDERDBLCLICKA, $HDN-DIVIDERDBLCLICKW, $HDN-BEGINTRACKA,
	$HDN-BEGINTRACKW, $HDN-ENDTRACKA, $HDN-ENDTRACKW, $HDN-TRACKA,
	$HDN-TRACKW, $HDN-ITEMCHANGING, $HDN-ITEMCHANGED, $HDN-ITEMCLICK,
	$HDN-ITEMDBLCLICK, $HDN-DIVIDERDBLCLICK, $HDN-BEGINTRACK,
	$HDN-ENDTRACK, $HDN-TRACK;
  export <NMHEADERA>, <HD-NOTIFY>, iItem-value, iItem-value-setter,
	iButton-value, iButton-value-setter, pitem-value, pitem-value-setter,
	<HD-NOTIFYA>, <LPHD-NOTIFYA>, <LPNMHEADERA>;
  export <NMHEADER>, <LPNMHEADER>;
  export iItem-value, iItem-value-setter, mask-value,
	mask-value-setter, pszText-value, pszText-value-setter,
	cchTextMax-value, cchTextMax-value-setter, iImage-value,
	iImage-value-setter, <NMHDDISPINFOA>, <LPNMHDDISPINFOA>;
  export <NMHDDISPINFO>, <LPNMHDDISPINFO>, $TOOLBARCLASSNAME;
  export iBitmap-value, iBitmap-value-setter, idCommand-value,
	idCommand-value-setter, fsState-value, fsState-value-setter,
	fsStyle-value, fsStyle-value-setter, bReserved-array,
	bReserved-array-setter, bReserved-value, iString-value,
	iString-value-setter, <TBBUTTON>, <LPTBBUTTON>, <PTBBUTTON>,
	<LPCTBBUTTON>;
  export from-value, from-value-setter, to-value, to-value-setter,
	<COLORMAP>, <LPCOLORMAP>, CreateToolbarEx, CreateMappedBitmap,
	$CMB-MASKED, $TBSTATE-CHECKED, $TBSTATE-PRESSED, $TBSTATE-ENABLED,
	$TBSTATE-HIDDEN, $TBSTATE-INDETERMINATE, $TBSTATE-WRAP,
	$TBSTYLE-BUTTON, $TBSTYLE-SEP, $TBSTYLE-CHECK, $TBSTYLE-GROUP,
	$TBSTYLE-CHECKGROUP, $TBSTYLE-TOOLTIPS, $TBSTYLE-WRAPABLE,
	$TBSTYLE-ALTDRAG, $TB-ENABLEBUTTON, $TB-CHECKBUTTON, $TB-PRESSBUTTON,
	$TB-HIDEBUTTON, $TB-INDETERMINATE, $TB-ISBUTTONENABLED,
	$TB-ISBUTTONCHECKED, $TB-ISBUTTONPRESSED, $TB-ISBUTTONHIDDEN,
	$TB-ISBUTTONINDETERMINATE, $TB-SETSTATE, $TB-GETSTATE, $TB-ADDBITMAP,
	hInst-value, hInst-value-setter, nID-value, nID-value-setter,
	<TBADDBITMAP>, <LPTBADDBITMAP>, $HINST-COMMCTRL,
	$IDB-STD-SMALL-COLOR, $IDB-STD-LARGE-COLOR, $IDB-VIEW-SMALL-COLOR,
	$IDB-VIEW-LARGE-COLOR, $STD-CUT, $STD-COPY, $STD-PASTE, $STD-UNDO,
	$STD-REDOW, $STD-DELETE, $STD-FILENEW, $STD-FILEOPEN, $STD-FILESAVE,
	$STD-PRINTPRE, $STD-PROPERTIES, $STD-HELP, $STD-FIND, $STD-REPLACE,
	$STD-PRINT, $VIEW-LARGEICONS, $VIEW-SMALLICONS, $VIEW-LIST,
	$VIEW-DETAILS, $VIEW-SORTNAME, $VIEW-SORTSIZE, $VIEW-SORTDATE,
	$VIEW-SORTTYPE, $VIEW-PARENTFOLDER, $VIEW-NETCONNECT,
	$VIEW-NETDISCONNECT, $VIEW-NEWFOLDER, $TB-ADDBUTTONS,
	$TB-INSERTBUTTON, $TB-DELETEBUTTON, $TB-GETBUTTON, $TB-BUTTONCOUNT,
	$TB-COMMANDTOINDEX, hkr-value, hkr-value-setter, pszSubKey-value,
	pszSubKey-value-setter, pszValueName-value,
	pszValueName-value-setter, <TBSAVEPARAMSA>, <LPTBSAVEPARAMSA>;
  export <TBSAVEPARAMS>, <LPTBSAVEPARAMS>, $TB-SAVERESTOREA,
	$TB-SAVERESTOREW, $TB-CUSTOMIZE, $TB-ADDSTRINGA, $TB-ADDSTRINGW,
	$TB-GETITEMRECT, $TB-BUTTONSTRUCTSIZE, $TB-SETBUTTONSIZE,
	$TB-SETBITMAPSIZE, $TB-AUTOSIZE, $TB-GETTOOLTIPS, $TB-SETTOOLTIPS,
	$TB-SETPARENT, $TB-SETROWS, $TB-GETROWS, $TB-SETCMDID,
	$TB-CHANGEBITMAP, $TB-GETBITMAP, $TB-GETBUTTONTEXTA,
	$TB-GETBUTTONTEXTW, $TB-REPLACEBITMAP, $TB-GETBUTTONTEXT,
	$TB-SAVERESTORE, $TB-ADDSTRING, hInstOld-value,
	hInstOld-value-setter, nIDOld-value, nIDOld-value-setter,
	hInstNew-value, hInstNew-value-setter, nIDNew-value,
	nIDNew-value-setter, nButtons-value, nButtons-value-setter,
	<TBREPLACEBITMAP>, <LPTBREPLACEBITMAP>, $TBBF-LARGE,
	$TB-GETBITMAPFLAGS, $TBN-GETBUTTONINFOA, $TBN-GETBUTTONINFOW,
	$TBN-BEGINDRAG, $TBN-ENDDRAG, $TBN-BEGINADJUST, $TBN-ENDADJUST,
	$TBN-RESET, $TBN-QUERYINSERT, $TBN-QUERYDELETE, $TBN-TOOLBARCHANGE,
	$TBN-CUSTHELP;
  export $TBN-GETBUTTONINFO, <NMTOOLBARA>, <LPNMTOOLBARA>, <TBNOTIFY>,
	<LPTBNOTIFY>;
  export <NMTOOLBAR>, <LPNMTOOLBAR>, $TOOLTIPS-CLASS, <TTTOOLINFOA>,
	<LPTTTOOLINFOA>, <LPTOOLINFO>, <TOOLINFO>, uFlags-value,
	uFlags-value-setter, uId-value, uId-value-setter, rect-value,
	rect-value-setter, hinst-value, hinst-value-setter, <TOOLINFOA>,
	<LPTOOLINFOA>, <PTOOLINFOA>;
  export <TTTOOLINFO>, <LPTTTOOLINFO>, $TTS-ALWAYSTIP, $TTS-NOPREFIX,
	$TTF-IDISHWND, $TTF-CENTERTIP, $TTF-RTLREADING, $TTF-SUBCLASS,
	$TTDT-AUTOMATIC, $TTDT-RESHOW, $TTDT-AUTOPOP, $TTDT-INITIAL,
	$TTM-ACTIVATE, $TTM-SETDELAYTIME, $TTM-ADDTOOLA, $TTM-ADDTOOLW,
	$TTM-DELTOOLA, $TTM-DELTOOLW, $TTM-NEWTOOLRECTA, $TTM-NEWTOOLRECTW,
	$TTM-RELAYEVENT, $TTM-GETTOOLINFOA, $TTM-GETTOOLINFOW,
	$TTM-SETTOOLINFOA, $TTM-SETTOOLINFOW, $TTM-HITTESTA, $TTM-HITTESTW,
	$TTM-GETTEXTA, $TTM-GETTEXTW, $TTM-UPDATETIPTEXTA,
	$TTM-UPDATETIPTEXTW, $TTM-GETTOOLCOUNT, $TTM-ENUMTOOLSA,
	$TTM-ENUMTOOLSW, $TTM-GETCURRENTTOOLA, $TTM-GETCURRENTTOOLW,
	$TTM-WINDOWFROMPOINT;
  export $TTM-ADDTOOL, $TTM-DELTOOL, $TTM-NEWTOOLRECT,
	$TTM-GETTOOLINFO, $TTM-SETTOOLINFO, $TTM-HITTEST, $TTM-GETTEXT,
	$TTM-UPDATETIPTEXT, $TTM-ENUMTOOLS, $TTM-GETCURRENTTOOL;
  export <LPTTHITTESTINFOA>, <LPHITTESTINFO>, ti-value,
	ti-value-setter, <TTHITTESTINFOA>, <LPHITTESTINFOA>;
  export <TTHITTESTINFO>, <LPTTHITTESTINFO>, $TTN-GETDISPINFOA,
	$TTN-GETDISPINFOW, $TTN-SHOW, $TTN-POP, $TTN-GETDISPINFO,
	$TTN-NEEDTEXT, $TTN-NEEDTEXTA, $TTN-NEEDTEXTW, <NMTTDISPINFOA>,
	<LPNMTTDISPINFOA>, <TOOLTIPTEXT>, <LPTOOLTIPTEXT>, szText-array,
	szText-array-setter, szText-value, hinst-value, hinst-value-setter,
	uFlags-value, uFlags-value-setter, <TOOLTIPTEXTA>, <LPTOOLTIPTEXTA>;
  export <NMTTDISPINFO>, <LPNMTTDISPINFO>, $SBARS-SIZEGRIP,
	DrawStatusText, CreateStatusWindow, $STATUSCLASSNAME, $SB-SETTEXTA,
	$SB-SETTEXTW, $SB-GETTEXTA, $SB-GETTEXTW, $SB-GETTEXTLENGTHA,
	$SB-GETTEXTLENGTHW, $SB-GETTEXT, $SB-SETTEXT, $SB-GETTEXTLENGTH;
  export $SB-SETPARTS, $SB-GETPARTS, $SB-GETBORDERS, $SB-SETMINHEIGHT,
	$SB-SIMPLE, $SB-GETRECT, $SBT-OWNERDRAW, $SBT-NOBORDERS, $SBT-POPOUT,
	$SBT-RTLREADING, MenuHelp, ShowHideMenuCtl, GetEffectiveClientRect,
	$MINSYSCOMMAND, $TRACKBAR-CLASS;
  export $TBS-AUTOTICKS, $TBS-VERT, $TBS-HORZ, $TBS-TOP, $TBS-BOTTOM,
	$TBS-LEFT, $TBS-RIGHT, $TBS-BOTH, $TBS-NOTICKS, $TBS-ENABLESELRANGE,
	$TBS-FIXEDLENGTH, $TBS-NOTHUMB, $TBM-GETPOS, $TBM-GETRANGEMIN,
	$TBM-GETRANGEMAX, $TBM-GETTIC, $TBM-SETTIC, $TBM-SETPOS,
	$TBM-SETRANGE, $TBM-SETRANGEMIN, $TBM-SETRANGEMAX, $TBM-CLEARTICS,
	$TBM-SETSEL, $TBM-SETSELSTART, $TBM-SETSELEND, $TBM-GETPTICS,
	$TBM-GETTICPOS, $TBM-GETNUMTICS, $TBM-GETSELSTART, $TBM-GETSELEND,
	$TBM-CLEARSEL, $TBM-SETTICFREQ, $TBM-SETPAGESIZE, $TBM-GETPAGESIZE,
	$TBM-SETLINESIZE, $TBM-GETLINESIZE, $TBM-GETTHUMBRECT,
	$TBM-GETCHANNELRECT, $TBM-SETTHUMBLENGTH, $TBM-GETTHUMBLENGTH;
  export $TB-LINEUP, $TB-LINEDOWN, $TB-PAGEUP, $TB-PAGEDOWN,
	$TB-THUMBPOSITION, $TB-THUMBTRACK, $TB-TOP, $TB-BOTTOM, $TB-ENDTRACK,
	uNotification-value, uNotification-value-setter, ptCursor-value,
	ptCursor-value-setter, <DRAGLISTINFO>, <LPDRAGLISTINFO>,
	$DL-BEGINDRAG, $DL-DRAGGING, $DL-DROPPED, $DL-CANCELDRAG,
	$DL-CURSORSET, $DL-STOPCURSOR, $DL-COPYCURSOR, $DL-MOVECURSOR,
	MakeDragList, DrawInsert, LBItemFromPt, $UPDOWN-CLASS;
  export nSec-value, nSec-value-setter, nInc-value, nInc-value-setter,
	<UDACCEL>, <LPUDACCEL>, $UD-MAXVAL, $UD-MINVAL, $UDS-WRAP,
	$UDS-SETBUDDYINT, $UDS-ALIGNRIGHT, $UDS-ALIGNLEFT, $UDS-AUTOBUDDY,
	$UDS-ARROWKEYS, $UDS-HORZ, $UDS-NOTHOUSANDS, $UDM-SETRANGE,
	$UDM-GETRANGE, $UDM-SETPOS, $UDM-GETPOS, $UDM-SETBUDDY,
	$UDM-GETBUDDY, $UDM-SETACCEL, $UDM-GETACCEL, $UDM-SETBASE,
	$UDM-GETBASE, CreateUpDownControl, <NMUPDOWN>, <LPNMUPDOWN>;
  export iPos-value, iPos-value-setter, iDelta-value,
	iDelta-value-setter, <NM-UPDOWN>, <LPNM-UPDOWN>, $UDN-DELTAPOS,
	$PROGRESS-CLASS;
  export $PBM-SETRANGE, $PBM-SETPOS, $PBM-DELTAPOS, $PBM-SETSTEP,
	$PBM-STEPIT, $HOTKEYF-SHIFT, $HOTKEYF-CONTROL, $HOTKEYF-ALT,
	$HOTKEYF-EXT, $HKCOMB-NONE, $HKCOMB-S, $HKCOMB-C, $HKCOMB-A,
	$HKCOMB-SC, $HKCOMB-SA, $HKCOMB-CA, $HKCOMB-SCA, $HKM-SETHOTKEY,
	$HKM-GETHOTKEY, $HKM-SETRULES, $HOTKEY-CLASS, $CCS-TOP, $CCS-NOMOVEY,
	$CCS-BOTTOM, $CCS-NORESIZE, $CCS-NOPARENTALIGN, $CCS-ADJUSTABLE,
	$CCS-NODIVIDER, $WC-LISTVIEW, $LVS-ICON, $LVS-REPORT, $LVS-SMALLICON,
	$LVS-LIST, $LVS-TYPEMASK, $LVS-SINGLESEL, $LVS-SHOWSELALWAYS,
	$LVS-SORTASCENDING, $LVS-SORTDESCENDING, $LVS-SHAREIMAGELISTS,
	$LVS-NOLABELWRAP, $LVS-AUTOARRANGE, $LVS-EDITLABELS, $LVS-NOSCROLL,
	$LVS-TYPESTYLEMASK, $LVS-ALIGNTOP, $LVS-ALIGNLEFT, $LVS-ALIGNMASK,
	$LVS-OWNERDRAWFIXED, $LVS-NOCOLUMNHEADER, $LVS-NOSORTHEADER,
	$LVM-GETBKCOLOR, ListView-GetBkColor, $LVM-SETBKCOLOR,
	ListView-SetBkColor, $LVM-GETIMAGELIST, ListView-GetImageList,
	$LVSIL-NORMAL, $LVSIL-SMALL, $LVSIL-STATE, $LVM-SETIMAGELIST,
	ListView-SetImageList, $LVM-GETITEMCOUNT, ListView-GetItemCount,
	$LVIF-TEXT, $LVIF-IMAGE, $LVIF-PARAM, $LVIF-STATE, $LVIS-FOCUSED,
	$LVIS-SELECTED, $LVIS-CUT, $LVIS-DROPHILITED, $LVIS-ACTIVATING,
	$LVIS-OVERLAYMASK, $LVIS-STATEIMAGEMASK, INDEXTOSTATEIMAGEMASK,
	<LVITEMA>, <LV-ITEM>, mask-value, mask-value-setter, iItem-value,
	iItem-value-setter, iSubItem-value, iSubItem-value-setter,
	state-value, state-value-setter, stateMask-value,
	stateMask-value-setter, pszText-value, pszText-value-setter,
	cchTextMax-value, cchTextMax-value-setter, iImage-value,
	iImage-value-setter, <LV-ITEMA>, <LPLV-ITEMA>, <LPLVITEMA>;
  export <LVITEM>, <LPLVITEM>;
  export $LPSTR-TEXTCALLBACKW, $LPSTR-TEXTCALLBACKA,
	$LPSTR-TEXTCALLBACK, $I-IMAGECALLBACK, $LVM-GETITEM,
	ListView-GetItem, $LVM-SETITEM, ListView-SetItem, $LVM-INSERTITEM,
	ListView-InsertItem, $LVM-DELETEITEM, ListView-DeleteItem,
	$LVM-DELETEALLITEMS, ListView-DeleteAllItems, $LVM-GETCALLBACKMASK,
	ListView-GetCallbackMask, $LVM-SETCALLBACKMASK,
	ListView-SetCallbackMask, $LVNI-ALL, $LVNI-FOCUSED, $LVNI-SELECTED,
	$LVNI-CUT, $LVNI-DROPHILITED, $LVNI-ABOVE, $LVNI-BELOW, $LVNI-TOLEFT,
	$LVNI-TORIGHT, $LVM-GETNEXTITEM, ListView-GetNextItem, $LVFI-PARAM,
	$LVFI-STRING, $LVFI-PARTIAL, $LVFI-WRAP, $LVFI-NEARESTXY,
	<LVFINDINFOA>, <LV-FINDINFO>, psz-value, psz-value-setter,
	vkDirection-value, vkDirection-value-setter, <LV-FINDINFOA>,
	<LPLV-FINDINFOA>, <LPFINDINFOA>;
  export <LVFINDINFO>, $LVM-FINDITEM, ListView-FindItem, $LVIR-BOUNDS,
	$LVIR-ICON, $LVIR-LABEL, $LVIR-SELECTBOUNDS, $LVM-GETITEMRECT,
	$LVM-SETITEMPOSITION, ListView-SetItemPosition, $LVM-GETITEMPOSITION,
	ListView-GetItemPosition, $LVM-GETSTRINGWIDTH,
	ListView-GetStringWidth, $LVHT-NOWHERE, $LVHT-ONITEMICON,
	$LVHT-ONITEMLABEL, $LVHT-ONITEMSTATEICON, $LVHT-ONITEM, $LVHT-ABOVE,
	$LVHT-BELOW, $LVHT-TORIGHT, $LVHT-TOLEFT, <LVHITTESTINFO>,
	iItem-value, iItem-value-setter, <LV-HITTESTINFO>,
	<LPLV-HITTESTINFO>, <LPLVHITTESTINFO>, $LVM-HITTEST,
	ListView-HitTest, $LVM-ENSUREVISIBLE, ListView-EnsureVisible,
	$LVM-SCROLL, ListView-Scroll, $LVM-REDRAWITEMS, ListView-RedrawItems,
	$LVA-DEFAULT, $LVA-ALIGNLEFT, $LVA-ALIGNTOP, $LVA-SNAPTOGRID,
	$LVM-ARRANGE, ListView-Arrange, $LVM-EDITLABEL, ListView-EditLabel,
	$LVM-GETEDITCONTROL, ListView-GetEditControl, <LVCOLUMNA>,
	<LV-COLUMN>, mask-value, mask-value-setter, fmt-value,
	fmt-value-setter, pszText-value, pszText-value-setter,
	cchTextMax-value, cchTextMax-value-setter, iSubItem-value,
	iSubItem-value-setter, <LV-COLUMNA>, <LPLV-COLUMNA>, <LPLVCOLUMNA>;
  export <LVCOLUMN>, <LPLVCOLUMN>;
  export $LVCF-FMT, $LVCF-WIDTH, $LVCF-TEXT, $LVCF-SUBITEM,
	$LVCFMT-LEFT, $LVCFMT-RIGHT, $LVCFMT-CENTER, $LVCFMT-JUSTIFYMASK,
	$LVM-GETCOLUMN, ListView-GetColumn, $LVM-SETCOLUMN,
	ListView-SetColumn, $LVM-INSERTCOLUMN, ListView-InsertColumn,
	$LVM-DELETECOLUMN, ListView-DeleteColumn, $LVM-GETCOLUMNWIDTH,
	ListView-GetColumnWidth, $LVSCW-AUTOSIZE, $LVSCW-AUTOSIZE-USEHEADER,
	$LVM-SETCOLUMNWIDTH, ListView-SetColumnWidth, $LVM-CREATEDRAGIMAGE,
	ListView-CreateDragImage, $LVM-GETVIEWRECT, ListView-GetViewRect,
	$LVM-GETTEXTCOLOR, ListView-GetTextColor, $LVM-SETTEXTCOLOR,
	ListView-SetTextColor, $LVM-GETTEXTBKCOLOR, ListView-GetTextBkColor,
	$LVM-SETTEXTBKCOLOR, ListView-SetTextBkColor, $LVM-GETTOPINDEX,
	ListView-GetTopIndex, $LVM-GETCOUNTPERPAGE, ListView-GetCountPerPage,
	$LVM-GETORIGIN, ListView-GetOrigin, $LVM-UPDATE, ListView-Update,
	$LVM-SETITEMSTATE, ListView-SetItemState, $LVM-GETITEMSTATE,
	ListView-GetItemState, $LVM-GETITEMTEXT, ListView-GetItemText,
	$LVM-SETITEMTEXT, ListView-SetItemText, $LVM-SETITEMCOUNT,
	ListView-SetItemCount;
  export <PFNLVCOMPARE>, <PFNLVCOMPARE>-callback-wrapper;
  export $LVM-SORTITEMS, ListView-SortItems, $LVM-SETITEMPOSITION32,
	$LVM-GETSELECTEDCOUNT, ListView-GetSelectedCount,
	$LVM-GETITEMSPACING, ListView-GetItemSpacing, $LVM-GETISEARCHSTRING,
	ListView-GetISearchString, <NMLISTVIEW>, <LPNMLISTVIEW>;
  export iItem-value, iItem-value-setter, iSubItem-value,
	iSubItem-value-setter, uNewState-value, uNewState-value-setter,
	uOldState-value, uOldState-value-setter, uChanged-value,
	uChanged-value-setter, ptAction-value, ptAction-value-setter,
	<NM-LISTVIEW>, <LPNM-LISTVIEW>;
  export $LVN-ITEMCHANGING, $LVN-ITEMCHANGED, $LVN-INSERTITEM,
	$LVN-DELETEITEM, $LVN-DELETEALLITEMS, $LVN-BEGINLABELEDITA,
	$LVN-BEGINLABELEDITW, $LVN-ENDLABELEDITA, $LVN-ENDLABELEDITW,
	$LVN-COLUMNCLICK, $LVN-BEGINDRAG, $LVN-BEGINRDRAG, $LVN-GETDISPINFOA,
	$LVN-GETDISPINFOW, $LVN-SETDISPINFOA, $LVN-SETDISPINFOW,
	$LVN-BEGINLABELEDIT, $LVN-ENDLABELEDIT, $LVN-GETDISPINFO,
	$LVN-SETDISPINFO;
  export $LVIF-DI-SETITEM, <NMLVDISPINFOA>, <LV-DISPINFO>, item-value,
	item-value-setter, <LV-DISPINFOA>, <LPLV-DISPINFOA>,
	<LPNMLVDISPINFOA>;
  export <NMLVDISPINFO>, $LVN-KEYDOWN, <NMLVKEYDOWN>;
  export wVKey-value, wVKey-value-setter, <LV-KEYDOWN>,
	<LPLV-KEYDOWN>, <LPNMLVKEYDOWN>, $WC-TREEVIEW, $TVS-HASBUTTONS,
	$TVS-HASLINES, $TVS-LINESATROOT, $TVS-EDITLABELS,
	$TVS-DISABLEDRAGDROP, $TVS-SHOWSELALWAYS, <HTREEITEM>, $TVIF-TEXT,
	$TVIF-IMAGE, $TVIF-PARAM, $TVIF-STATE, $TVIF-HANDLE,
	$TVIF-SELECTEDIMAGE, $TVIF-CHILDREN, $TVIS-SELECTED, $TVIS-CUT,
	$TVIS-DROPHILITED, $TVIS-BOLD, $TVIS-EXPANDED, $TVIS-EXPANDEDONCE,
	$TVIS-OVERLAYMASK, $TVIS-STATEIMAGEMASK, $TVIS-USERMASK,
	$I-CHILDRENCALLBACK, <TVITEMA>, <LPTVITEMA>, <LPTV-ITEM>, <TV-ITEM>,
	mask-value, mask-value-setter, hItem-value, hItem-value-setter,
	state-value, state-value-setter, stateMask-value,
	stateMask-value-setter, pszText-value, pszText-value-setter,
	cchTextMax-value, cchTextMax-value-setter, iImage-value,
	iImage-value-setter, iSelectedImage-value,
	iSelectedImage-value-setter, cChildren-value, cChildren-value-setter,
	<TV-ITEMA>, <LPTV-ITEMA>;
  export <TVITEM>, <LPTVITEM>;
  export $TVI-ROOT, $TVI-FIRST, $TVI-LAST, $TVI-SORT,
	<TVINSERTSTRUCTA>, <LPTVINSERTSTRUCTA>, <TV-INSERTSTRUCT>,
	<LPTV-INSERTSTRUCT>, hParent-value, hParent-value-setter,
	hInsertAfter-value, hInsertAfter-value-setter, item-value,
	item-value-setter, <TV-INSERTSTRUCTA>, <LPTV-INSERTSTRUCTA>;
  export <TVINSERTSTRUCT>, <LPTVINSERTSTRUCT>, $TVM-INSERTITEM,
	TreeView-InsertItem, $TVM-DELETEITEM, TreeView-DeleteItem,
	TreeView-DeleteAllItems, $TVM-EXPAND, TreeView-Expand, $TVE-COLLAPSE,
	$TVE-EXPAND, $TVE-TOGGLE, $TVE-COLLAPSERESET, $TVM-GETITEMRECT,
	$TVM-GETCOUNT, TreeView-GetCount, $TVM-GETINDENT, TreeView-GetIndent,
	$TVM-SETINDENT, TreeView-SetIndent, $TVM-GETIMAGELIST,
	TreeView-GetImageList, $TVSIL-NORMAL, $TVSIL-STATE,
	$TVM-SETIMAGELIST, TreeView-SetImageList, $TVM-GETNEXTITEM,
	TreeView-GetNextItem, $TVGN-ROOT, $TVGN-NEXT, $TVGN-PREVIOUS,
	$TVGN-PARENT, $TVGN-CHILD, $TVGN-FIRSTVISIBLE, $TVGN-NEXTVISIBLE,
	$TVGN-PREVIOUSVISIBLE, $TVGN-DROPHILITE, $TVGN-CARET,
	TreeView-GetChild, TreeView-GetNextSibling, TreeView-GetPrevSibling,
	TreeView-GetParent, TreeView-GetFirstVisible,
	TreeView-GetNextVisible, TreeView-GetPrevVisible,
	TreeView-GetSelection, TreeView-GetDropHilight, TreeView-GetRoot,
	$TVM-SELECTITEM, TreeView-Select, TreeView-SelectItem,
	TreeView-SelectDropTarget, TreeView-SelectSetFirstVisible,
	$TVM-GETITEM, TreeView-GetItem, $TVM-SETITEM, TreeView-SetItem,
	$TVM-EDITLABEL, TreeView-EditLabel, $TVM-GETEDITCONTROL,
	TreeView-GetEditControl, $TVM-GETVISIBLECOUNT,
	TreeView-GetVisibleCount, $TVM-HITTEST, TreeView-HitTest,
	<TVHITTESTINFO>, <LPTVHITTESTINFO>;
  export hItem-value, hItem-value-setter, <TV-HITTESTINFO>,
	<LPTV-HITTESTINFO>, $TVHT-NOWHERE, $TVHT-ONITEMICON,
	$TVHT-ONITEMLABEL, $TVHT-ONITEM, $TVHT-ONITEMINDENT,
	$TVHT-ONITEMBUTTON, $TVHT-ONITEMRIGHT, $TVHT-ONITEMSTATEICON,
	$TVHT-ABOVE, $TVHT-BELOW, $TVHT-TORIGHT, $TVHT-TOLEFT,
	$TVM-CREATEDRAGIMAGE, TreeView-CreateDragImage, $TVM-SORTCHILDREN,
	TreeView-SortChildren, $TVM-ENSUREVISIBLE, TreeView-EnsureVisible,
	$TVM-SORTCHILDRENCB, TreeView-SortChildrenCB, $TVM-ENDEDITLABELNOW,
	TreeView-EndEditLabelNow, $TVM-GETISEARCHSTRING,
	TreeView-GetISearchString;
  export <PFNTVCOMPARE>, <PFNTVCOMPARE>-callback-wrapper, <TVSORTCB>,
	<LPTVSORTCB>;
  export hParent-value, hParent-value-setter, lpfnCompare-value,
	lpfnCompare-value-setter, <TV-SORTCB>, <LPTV-SORTCB>;
  export <NMTREEVIEWA>, <LPNMTREEVIEWA>, <LPNM-TREEVIEW>,
	<NM-TREEVIEW>, action-value, action-value-setter, itemOld-value,
	itemOld-value-setter, itemNew-value, itemNew-value-setter,
	ptDrag-value, ptDrag-value-setter, <NM-TREEVIEWA>, <LPNM-TREEVIEWA>;
  export <NMTREEVIEW>, <LPNMTREEVIEW>;
  export $TVN-SELCHANGINGA, $TVN-SELCHANGINGW, $TVN-SELCHANGEDA,
	$TVN-SELCHANGEDW, $TVC-UNKNOWN, $TVC-BYMOUSE, $TVC-BYKEYBOARD,
	$TVN-GETDISPINFOA, $TVN-GETDISPINFOW, $TVN-SETDISPINFOA,
	$TVN-SETDISPINFOW, $TVIF-DI-SETITEM, <NMTVDISPINFOA>, <TV-DISPINFO>,
	item-value, item-value-setter, <TV-DISPINFOA>, <LPTV-DISPINFOA>,
	<LPNMTVDISPINFOA>;
  export <NMTVDISPINFO>, <LPNMTVDISPINFO>, $TVN-ITEMEXPANDINGA,
	$TVN-ITEMEXPANDINGW, $TVN-ITEMEXPANDEDA, $TVN-ITEMEXPANDEDW,
	$TVN-BEGINDRAGA, $TVN-BEGINDRAGW, $TVN-BEGINRDRAGA, $TVN-BEGINRDRAGW,
	$TVN-DELETEITEMA, $TVN-DELETEITEMW, $TVN-BEGINLABELEDITA,
	$TVN-BEGINLABELEDITW, $TVN-ENDLABELEDITA, $TVN-ENDLABELEDITW,
	$TVN-KEYDOWN, <NMTVKEYDOWN>;
  export wVKey-value, wVKey-value-setter, <TV-KEYDOWN>,
	<LPTV-KEYDOWN>, <LPNMTVKEYDOWN>;
  export $TVN-SELCHANGING, $TVN-SELCHANGED, $TVN-GETDISPINFO,
	$TVN-SETDISPINFO, $TVN-ITEMEXPANDING, $TVN-ITEMEXPANDED,
	$TVN-BEGINDRAG, $TVN-BEGINRDRAG, $TVN-DELETEITEM,
	$TVN-BEGINLABELEDIT, $TVN-ENDLABELEDIT;
  export $WC-TABCONTROL, $TCS-FORCEICONLEFT, $TCS-FORCELABELLEFT,
	$TCS-TABS, $TCS-BUTTONS, $TCS-SINGLELINE, $TCS-MULTILINE,
	$TCS-RIGHTJUSTIFY, $TCS-FIXEDWIDTH, $TCS-RAGGEDRIGHT,
	$TCS-FOCUSONBUTTONDOWN, $TCS-OWNERDRAWFIXED, $TCS-TOOLTIPS,
	$TCS-FOCUSNEVER, $TCM-GETIMAGELIST, TabCtrl-GetImageList,
	$TCM-SETIMAGELIST, TabCtrl-SetImageList, $TCM-GETITEMCOUNT,
	TabCtrl-GetItemCount, $TCIF-TEXT, $TCIF-IMAGE, $TCIF-RTLREADING,
	$TCIF-PARAM, <TCITEMHEADERA>, <TC-ITEMHEADER>, mask-value,
	mask-value-setter, pszText-value, pszText-value-setter,
	cchTextMax-value, cchTextMax-value-setter, iImage-value,
	iImage-value-setter, <TC-ITEMHEADERA>, <LPTC-ITEMHEADERA>,
	<LPTCITEMHEADERA>;
  export <TCITEMHEADER>, <LPTCITEMHEADER>;
  export <TCITEMA>, <TC-ITEM>, mask-value, mask-value-setter,
	pszText-value, pszText-value-setter, cchTextMax-value,
	cchTextMax-value-setter, iImage-value, iImage-value-setter,
	<TC-ITEMA>, <LPTC-ITEMA>, <LPTCITEMA>;
  export <TCITEM>, <LPTCITEM>;
  export $TCM-GETITEM, TabCtrl-GetItem, $TCM-SETITEM, TabCtrl-SetItem,
	$TCM-INSERTITEM, TabCtrl-InsertItem, $TCM-DELETEITEM,
	TabCtrl-DeleteItem, $TCM-DELETEALLITEMS, TabCtrl-DeleteAllItems,
	$TCM-GETITEMRECT, TabCtrl-GetItemRect, $TCM-GETCURSEL,
	TabCtrl-GetCurSel, $TCM-SETCURSEL, TabCtrl-SetCurSel, $TCHT-NOWHERE,
	$TCHT-ONITEMICON, $TCHT-ONITEMLABEL, $TCHT-ONITEM, <TCHITTESTINFO>,
	<LPTCHITTESTINFO>;
  export <TC-HITTESTINFO>, <LPTC-HITTESTINFO>, $TCM-HITTEST,
	TabCtrl-HitTest, $TCM-SETITEMEXTRA, TabCtrl-SetItemExtra,
	$TCM-ADJUSTRECT, TabCtrl-AdjustRect, $TCM-SETITEMSIZE,
	TabCtrl-SetItemSize, $TCM-REMOVEIMAGE, TabCtrl-RemoveImage,
	$TCM-SETPADDING, TabCtrl-SetPadding, $TCM-GETROWCOUNT,
	TabCtrl-GetRowCount, $TCM-GETTOOLTIPS, TabCtrl-GetToolTips,
	$TCM-SETTOOLTIPS, TabCtrl-SetToolTips, $TCM-GETCURFOCUS,
	TabCtrl-GetCurFocus, $TCM-SETCURFOCUS, TabCtrl-SetCurFocus,
	$TCN-KEYDOWN, <NMTCKEYDOWN>;
  export wVKey-value, wVKey-value-setter, <TC-KEYDOWN>,
	<LPTC-KEYDOWN>, $TCN-SELCHANGE, $TCN-SELCHANGING;
  export $ANIMATE-CLASS, $ACS-CENTER, $ACS-TRANSPARENT, $ACS-AUTOPLAY,
	$ACM-OPEN, $ACM-PLAY, $ACM-STOP, $ACN-START, $ACN-STOP,
	Animate-Create, Animate-Open, Animate-OpenEx, Animate-Play,
	Animate-Stop, Animate-Close, Animate-Seek;


  // from "prsht.h":
  export $MAXPROPPAGES, <HPROPSHEETPAGE>;
  export <LPFNPSPCALLBACKA>, <LPFNPSPCALLBACK>-callback-wrapper,
	<LPFNPSPCALLBACK>, $PSP-DEFAULT, $PSP-DLGINDIRECT, $PSP-USEHICON,
	$PSP-USEICONID, $PSP-USETITLE, $PSP-RTLREADING, $PSP-HASHELP,
	$PSP-USEREFPARENT, $PSP-USECALLBACK, $PSP-PREMATURE;
  export $PSPCB-RELEASE, $PSPCB-CREATE, pszTemplate-value,
	pszTemplate-value-setter, pResource-value, pResource-value-setter,
	pszTemplate-value, pszTemplate-value-setter, pResource-value,
	pResource-value-setter, pszIcon-value, pszIcon-value-setter,
	pszIcon-value, pszIcon-value-setter, u2-value, u2-value-setter,
	pfnDlgProc-value, pfnDlgProc-value-setter, pfnCallback-value,
	pfnCallback-value-setter, pcRefParent-value,
	pcRefParent-value-setter, <PROPSHEETPAGEA>, <LPPROPSHEETPAGEA>,
	<LPCPROPSHEETPAGEA>;
  export <LPPROPSHEETPAGE>, <LPCPROPSHEETPAGE>;
  export $PSH-DEFAULT, $PSH-PROPTITLE, $PSH-USEHICON, $PSH-USEICONID,
	$PSH-PROPSHEETPAGE, $PSH-WIZARDHASFINISH, $PSH-WIZARD,
	$PSH-USEPSTARTPAGE, $PSH-NOAPPLYNOW, $PSH-USECALLBACK, $PSH-HASHELP,
	$PSH-MODELESS, $PSH-RTLREADING, $PSH-WIZARDCONTEXTHELP;
  export <PFNPROPSHEETCALLBACK>,
	<PFNPROPSHEETCALLBACK>-callback-wrapper, pszIcon-value,
	pszIcon-value-setter, pszIcon-value, pszIcon-value-setter,
	pszCaption-value, pszCaption-value-setter, nPages-value,
	nPages-value-setter, nStartPage-value, nStartPage-value-setter,
	pStartPage-value, pStartPage-value-setter, nStartPage-value,
	nStartPage-value-setter, pStartPage-value, pStartPage-value-setter,
	u2-value, u2-value-setter, ppsp-value, ppsp-value-setter,
	phpage-value, phpage-value-setter, ppsp-value, ppsp-value-setter,
	phpage-value, phpage-value-setter, u3-value, u3-value-setter,
	pfnCallback-value, pfnCallback-value-setter, <PROPSHEETHEADERA>,
	<LPPROPSHEETHEADERA>;
  export <LPCPROPSHEETHEADERA>;
  export <LPPROPSHEETHEADER>, <LPCPROPSHEETHEADER>;
  export $PSCB-INITIALIZED, $PSCB-PRECREATE, CreatePropertySheetPage,
	DestroyPropertySheetPage, PropertySheet;
  export <LPFNADDPROPSHEETPAGE>,
	<LPFNADDPROPSHEETPAGE>-callback-wrapper, <LPFNADDPROPSHEETPAGES>,
	<LPFNADDPROPSHEETPAGES>-callback-wrapper;
  export <PSHNOTIFY>, <LPPSHNOTIFY>, $PSN-FIRST, $PSN-LAST,
	$PSN-SETACTIVE, $PSN-KILLACTIVE, $PSN-APPLY, $PSN-RESET, $PSN-HELP,
	$PSN-WIZBACK, $PSN-WIZNEXT, $PSN-WIZFINISH, $PSN-QUERYCANCEL,
	$PSNRET-NOERROR, $PSNRET-INVALID, $PSNRET-INVALID-NOCHANGEPAGE,
	$PSM-SETCURSEL, PropSheet-SetCurSel, $PSM-REMOVEPAGE,
	PropSheet-RemovePage, $PSM-ADDPAGE, PropSheet-AddPage, $PSM-CHANGED,
	PropSheet-Changed, $PSM-RESTARTWINDOWS, PropSheet-RestartWindows,
	$PSM-REBOOTSYSTEM, PropSheet-RebootSystem, $PSM-CANCELTOCLOSE,
	PropSheet-CancelToClose, $PSM-QUERYSIBLINGS, PropSheet-QuerySiblings,
	$PSM-UNCHANGED, PropSheet-UnChanged, $PSM-APPLY, PropSheet-Apply,
	$PSM-SETTITLE, $PSM-SETWIZBUTTONS, PropSheet-SetWizButtons,
	$PSWIZB-BACK, $PSWIZB-NEXT, $PSWIZB-FINISH, $PSWIZB-DISABLEDFINISH,
	$PSM-PRESSBUTTON, PropSheet-PressButton, $PSBTN-BACK, $PSBTN-NEXT,
	$PSBTN-FINISH, $PSBTN-OK, $PSBTN-APPLYNOW, $PSBTN-CANCEL,
	$PSBTN-HELP, $PSBTN-MAX;
  export $PSM-SETCURSELID, PropSheet-SetCurSelByID,
	$PSM-SETFINISHTEXT, PropSheet-SetFinishText, $PSM-GETTABCONTROL,
	PropSheet-GetTabControl, $PSM-ISDIALOGMESSAGE,
	PropSheet-IsDialogMessage, $PSM-GETCURRENTPAGEHWND,
	PropSheet-GetCurrentPageHwnd, $ID-PSRESTARTWINDOWS,
	$ID-PSREBOOTSYSTEM, $WIZ-CXDLG, $WIZ-CYDLG, $WIZ-CXBMP, $WIZ-BODYX,
	$WIZ-BODYCX, $PROP-SM-CXDLG, $PROP-SM-CYDLG, $PROP-MED-CXDLG,
	$PROP-MED-CYDLG, $PROP-LG-CXDLG, $PROP-LG-CYDLG;

  // from "special.dylan":
  export FORWARD-WM-NOTIFY;
  export ListView-GetItemRect, ListView-SetItemPosition32,
	 TreeView-GetItemRect, PropSheet-SetTitle;
  export <LPHPROPSHEETPAGE>;
  export <TBNOTIFYA>, <LPTBNOTIFYA>, cchText-value, cchText-value-setter,
    tbButton-value, tbButton-value-setter;
  export <LPHD-ITEM>, <LPLV-COLUMN>, <LPLV-FINDINFO>, <LPLV-ITEM>, <LPTC-ITEM>;

 end module Win32-controls;
