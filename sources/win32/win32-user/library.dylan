module:    Dylan-user	
Synopsis:  Win32 API for window functions implemented in "USER32.DLL".
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Automatically generated from "library.src"; do not edit. */


define library Win32-user
  use functional-dylan;
  use C-FFI;
  use Win32-common;
  use Win32-GDI;
  export Win32-user;

  use Win32-kernel;
  export Win32-default-handler;
end library Win32-user;


define module Win32-user
  use functional-dylan;
  use C-FFI;
  use Win32-common,
    /* have to export here names that are used as structure accessors in
       both modules. */
    export: {cx-value, cy-value, x-value, y-value,
	       cx-value-setter, cy-value-setter, x-value-setter, y-setter,
	     cbSize-value, cbSize-value-setter,
	     cbData-value, cbData-value-setter,
	     lpData-value, lpData-value-setter,
	     offset-value, offset-value-setter,
       /* also export some names that the user may want to use directly: */
	     $MAX-PATH, $NULL-HWND, %logior, <HACCEL>, <HANDLE>,
	     <HBITMAP>, <HBRUSH>, <HCURSOR>, <HDC>, <HICON>, <HINSTANCE>,
	     <HMENU>, <HRGN>, <HWND>, <LPCSTR>, <LPPOINT>, <LPRECT>, <LPSTR>,
	     <LPTSTR>, <LPVOID>, <PPOINT>, MAKEINTRESOURCE, MAKELONG,
	     pointer-address, pointer-cast,
	     left-value, right-value, top-value, bottom-value} ;

  use Win32-GDI,
    import: { <LOGFONTA>, <LPDEVMODEA> },
    export: { <LOGFONTA>, <LPDEVMODEA> };

  // Accessor for "WinMain" parameters:
  //   Make them accessible from both Win32-kernel and Win32-user
  use Win32-kernel,
    import: { application-instance-handle, application-command-line, 
	      application-show-window },
    export: { application-instance-handle, application-command-line,
	      application-show-window };


  // from "winuser.h":
  export <HDWP>, <MENUTEMPLATEA>, <MENUTEMPLATE>, <LPMENUTEMPLATEA>,
	<LPMENUTEMPLATE>, <WNDPROC>, <DLGPROC>, <TIMERPROC>,
	<GRAYSTRINGPROC>, <WNDENUMPROC>, <HOOKPROC>, <SENDASYNCPROC>;
  export <PROPENUMPROCA>;
  export <PROPENUMPROCEXA>;
  export <EDITWORDBREAKPROCA>, <DRAWSTATEPROC>, <PROPENUMPROC>,
	<PROPENUMPROCEX>, <EDITWORDBREAKPROC>, <NAMEENUMPROCA>;
  export <WINSTAENUMPROCA>, <DESKTOPENUMPROCA>;
  export <WINSTAENUMPROC>, <DESKTOPENUMPROC>;
  export $RT-CURSOR, $RT-BITMAP, $RT-ICON, $RT-MENU, $RT-DIALOG,
	$RT-STRING, $RT-FONTDIR, $RT-FONT, $RT-ACCELERATOR, $RT-RCDATA,
	$RT-MESSAGETABLE, $DIFFERENCE, $RT-VERSION, $RT-DLGINCLUDE,
	$RT-PLUGPLAY, $RT-VXD, $RT-ANICURSOR, $RT-ANIICON, $RT-HTML;
  export $SB-HORZ, $SB-VERT, $SB-CTL, $SB-BOTH;
  export $SB-LINEUP, $SB-LINELEFT, $SB-LINEDOWN, $SB-LINERIGHT,
	$SB-PAGEUP, $SB-PAGELEFT, $SB-PAGEDOWN, $SB-PAGERIGHT,
	$SB-THUMBPOSITION, $SB-THUMBTRACK, $SB-TOP, $SB-LEFT, $SB-BOTTOM,
	$SB-RIGHT, $SB-ENDSCROLL;
  export $SW-HIDE, $SW-SHOWNORMAL, $SW-NORMAL, $SW-SHOWMINIMIZED,
	$SW-SHOWMAXIMIZED, $SW-MAXIMIZE, $SW-SHOWNOACTIVATE, $SW-SHOW,
	$SW-MINIMIZE, $SW-SHOWMINNOACTIVE, $SW-SHOWNA, $SW-RESTORE,
	$SW-SHOWDEFAULT, $SW-FORCEMINIMIZE, $SW-MAX;
  export $HIDE-WINDOW, $SHOW-OPENWINDOW, $SHOW-ICONWINDOW,
	$SHOW-FULLSCREEN, $SHOW-OPENNOACTIVATE;
  export $SW-PARENTCLOSING, $SW-OTHERZOOM, $SW-PARENTOPENING,
	$SW-OTHERUNZOOM;
  export $KF-EXTENDED, $KF-DLGMODE, $KF-MENUMODE, $KF-ALTDOWN,
	$KF-REPEAT, $KF-UP;
  export $VK-LBUTTON, $VK-RBUTTON, $VK-CANCEL, $VK-MBUTTON, $VK-BACK,
	$VK-TAB, $VK-CLEAR, $VK-RETURN, $VK-SHIFT, $VK-CONTROL, $VK-MENU,
	$VK-PAUSE, $VK-CAPITAL, $VK-KANA, $VK-HANGEUL, $VK-HANGUL, $VK-JUNJA,
	$VK-FINAL, $VK-HANJA, $VK-KANJI, $VK-ESCAPE, $VK-CONVERT,
	$VK-NONCONVERT, $VK-ACCEPT, $VK-MODECHANGE, $VK-SPACE, $VK-PRIOR,
	$VK-NEXT, $VK-END, $VK-HOME, $VK-LEFT, $VK-UP, $VK-RIGHT, $VK-DOWN,
	$VK-SELECT, $VK-PRINT, $VK-EXECUTE, $VK-SNAPSHOT, $VK-INSERT,
	$VK-DELETE, $VK-HELP, $VK-LWIN, $VK-RWIN, $VK-APPS, $VK-NUMPAD0,
	$VK-NUMPAD1, $VK-NUMPAD2, $VK-NUMPAD3, $VK-NUMPAD4, $VK-NUMPAD5,
	$VK-NUMPAD6, $VK-NUMPAD7, $VK-NUMPAD8, $VK-NUMPAD9, $VK-MULTIPLY,
	$VK-ADD, $VK-SEPARATOR, $VK-SUBTRACT, $VK-DECIMAL, $VK-DIVIDE,
	$VK-F1, $VK-F2, $VK-F3, $VK-F4, $VK-F5, $VK-F6, $VK-F7, $VK-F8,
	$VK-F9, $VK-F10, $VK-F11, $VK-F12, $VK-F13, $VK-F14, $VK-F15,
	$VK-F16, $VK-F17, $VK-F18, $VK-F19, $VK-F20, $VK-F21, $VK-F22,
	$VK-F23, $VK-F24, $VK-NUMLOCK, $VK-SCROLL, $VK-LSHIFT, $VK-RSHIFT,
	$VK-LCONTROL, $VK-RCONTROL, $VK-LMENU, $VK-RMENU, $VK-PROCESSKEY,
	$VK-ATTN, $VK-CRSEL, $VK-EXSEL, $VK-EREOF, $VK-PLAY, $VK-ZOOM,
	$VK-NONAME, $VK-PA1, $VK-OEM-CLEAR;
  export $WH-MIN, $WH-MSGFILTER, $WH-JOURNALRECORD,
	$WH-JOURNALPLAYBACK, $WH-KEYBOARD, $WH-GETMESSAGE, $WH-CALLWNDPROC,
	$WH-CBT, $WH-SYSMSGFILTER, $WH-MOUSE, $WH-HARDWARE, $WH-SHELL,
	$WH-FOREGROUNDIDLE, $WH-CALLWNDPROCRET, $WH-MINHOOK;
  export $HC-ACTION, $HC-GETNEXT, $HC-SKIP, $HC-NOREMOVE, $HC-NOREM,
	$HC-SYSMODALON, $HC-SYSMODALOFF;
  export $HCBT-MOVESIZE, $HCBT-MINMAX, $HCBT-QS, $HCBT-CREATEWND,
	$HCBT-DESTROYWND, $HCBT-ACTIVATE, $HCBT-CLICKSKIPPED,
	$HCBT-KEYSKIPPED, $HCBT-SYSCOMMAND, $HCBT-SETFOCUS;
  export lpcs-value, lpcs-value-setter, hwndInsertAfter-value,
	hwndInsertAfter-value-setter, <CBT-CREATEWNDA>, <LPCBT-CREATEWNDA>;
  export <CBT-CREATEWND>, <LPCBT-CREATEWND>;
  export fMouse-value, fMouse-value-setter, hWndActive-value,
	hWndActive-value-setter, <CBTACTIVATESTRUCT>, <LPCBTACTIVATESTRUCT>;
  export $MSGF-DIALOGBOX, $MSGF-MESSAGEBOX, $MSGF-MENU,
	$MSGF-SCROLLBAR, $MSGF-NEXTWINDOW, $MSGF-MAX, $MSGF-USER;
  export $HSHELL-WINDOWCREATED, $HSHELL-WINDOWDESTROYED,
	$HSHELL-ACTIVATESHELLWINDOW, $HSHELL-WINDOWACTIVATED,
	$HSHELL-GETMINRECT, $HSHELL-REDRAW, $HSHELL-TASKMAN,
	$HSHELL-LANGUAGE;
  export message-value, message-value-setter, paramL-value,
	paramL-value-setter, paramH-value, paramH-value-setter, time-value,
	time-value-setter, hwnd-value, hwnd-value-setter, <EVENTMSG>,
	<LPEVENTMSG>, <PEVENTMSGMSG>, <LPEVENTMSGMSG>;
  export <PEVENTMSG>;
  export lParam-value, lParam-value-setter, wParam-value,
	wParam-value-setter, message-value, message-value-setter, hwnd-value,
	hwnd-value-setter, <CWPSTRUCT>, <LPCWPSTRUCT>, <PCWPSTRUCT>;
  export lResult-value, lResult-value-setter, lParam-value,
	lParam-value-setter, wParam-value, wParam-value-setter,
	message-value, message-value-setter, hwnd-value, hwnd-value-setter,
	<CWPRETSTRUCT>, <LPCWPRETSTRUCT>, <PCWPRETSTRUCT>;
  export pt-value, pt-value-setter, hwnd-value, hwnd-value-setter,
	wHitTestCode-value, wHitTestCode-value-setter, dwExtraInfo-value,
	dwExtraInfo-value-setter, <MOUSEHOOKSTRUCT>, <LPMOUSEHOOKSTRUCT>,
	<PMOUSEHOOKSTRUCT>;
  export hwnd-value, hwnd-value-setter, message-value,
	message-value-setter, wParam-value, wParam-value-setter,
	lParam-value, lParam-value-setter, <HARDWAREHOOKSTRUCT>,
	<LPHARDWAREHOOKSTRUCT>, <PHARDWAREHOOKSTRUCT>;
  export $HKL-PREV, $HKL-NEXT, $KLF-ACTIVATE, $KLF-SUBSTITUTE-OK,
	$KLF-UNLOADPREVIOUS, $KLF-REORDER, $KLF-REPLACELANG,
	$KLF-NOTELLSHELL, $KLF-SETFORPROCESS;
  export $KL-NAMELENGTH, LoadKeyboardLayout, ActivateKeyboardLayout,
	UnloadKeyboardLayout, GetKeyboardLayoutName, GetKeyboardLayoutList,
	GetKeyboardLayout;
  export $DESKTOP-READOBJECTS, $DESKTOP-CREATEWINDOW,
	$DESKTOP-CREATEMENU, $DESKTOP-HOOKCONTROL, $DESKTOP-JOURNALRECORD,
	$DESKTOP-JOURNALPLAYBACK, $DESKTOP-ENUMERATE, $DESKTOP-WRITEOBJECTS,
	$DESKTOP-SWITCHDESKTOP;
  export $DF-ALLOWOTHERACCOUNTHOOK;
  export $WINSTA-ENUMDESKTOPS, $WINSTA-READATTRIBUTES,
	$WINSTA-ACCESSCLIPBOARD, $WINSTA-CREATEDESKTOP,
	$WINSTA-WRITEATTRIBUTES, $WINSTA-ACCESSGLOBALATOMS,
	$WINSTA-EXITWINDOWS, $WINSTA-ENUMERATE, $WINSTA-READSCREEN;
  export $WSF-VISIBLE, $UOI-FLAGS, $UOI-NAME, $UOI-TYPE,
	$UOI-USER-SID, fInherit-value, fInherit-value-setter,
	<USEROBJECTFLAGS>, <LPUSEROBJECTFLAGS>, <PUSEROBJECTFLAGS>,
	style-value, style-value-setter, lpfnWndProc-value,
	lpfnWndProc-value-setter, cbClsExtra-value, cbClsExtra-value-setter,
	cbWndExtra-value, cbWndExtra-value-setter, hInstance-value,
	hInstance-value-setter, hIcon-value, hIcon-value-setter,
	hCursor-value, hCursor-value-setter, hbrBackground-value,
	hbrBackground-value-setter, lpszMenuName-value,
	lpszMenuName-value-setter, lpszClassName-value,
	lpszClassName-value-setter, hIconSm-value, hIconSm-value-setter,
	<WNDCLASSEXA>, <LPWNDCLASSEXA>, <PWNDCLASSEXA>, <WNDCLASSEX>,
	<PWNDCLASSEX>, <LPWNDCLASSEX>;
  export style-value, style-value-setter, lpfnWndProc-value,
	lpfnWndProc-value-setter, cbClsExtra-value, cbClsExtra-value-setter,
	cbWndExtra-value, cbWndExtra-value-setter, hInstance-value,
	hInstance-value-setter, hIcon-value, hIcon-value-setter,
	hCursor-value, hCursor-value-setter, hbrBackground-value,
	hbrBackground-value-setter, lpszMenuName-value,
	lpszMenuName-value-setter, lpszClassName-value,
	lpszClassName-value-setter, <WNDCLASSA>, <LPWNDCLASSA>, <PWNDCLASSA>,
	<WNDCLASS>, <PWNDCLASS>, <LPWNDCLASS>;
  export hwnd-value, hwnd-value-setter, message-value,
	message-value-setter, wParam-value, wParam-value-setter,
	lParam-value, lParam-value-setter, time-value, time-value-setter,
	pt-value, pt-value-setter, <MSG>, <LPMSG>, <PMSG>;
  export $GWL-WNDPROC, $GWL-HINSTANCE, $GWL-HWNDPARENT, $GWL-STYLE,
	$GWL-EXSTYLE, $GWL-USERDATA, $GWL-ID;
  export $GCL-MENUNAME, $GCL-HBRBACKGROUND, $GCL-HCURSOR, $GCL-HICON,
	$GCL-HMODULE, $GCL-CBWNDEXTRA, $GCL-CBCLSEXTRA, $GCL-WNDPROC,
	$GCL-STYLE, $GCW-ATOM, $GCL-HICONSM;
  export $WM-NULL, $WM-CREATE, $WM-DESTROY, $WM-MOVE, $WM-SIZE,
	$WM-ACTIVATE;
  export $WA-INACTIVE, $WA-ACTIVE, $WA-CLICKACTIVE, $WM-SETFOCUS,
	$WM-KILLFOCUS, $WM-ENABLE, $WM-SETREDRAW, $WM-SETTEXT, $WM-GETTEXT,
	$WM-GETTEXTLENGTH, $WM-PAINT, $WM-CLOSE, $WM-QUERYENDSESSION,
	$WM-QUIT, $WM-QUERYOPEN, $WM-ERASEBKGND, $WM-SYSCOLORCHANGE,
	$WM-ENDSESSION, $WM-SHOWWINDOW, $WM-WININICHANGE, $WM-SETTINGCHANGE;
  export $WM-DEVMODECHANGE, $WM-ACTIVATEAPP, $WM-FONTCHANGE,
	$WM-TIMECHANGE, $WM-CANCELMODE, $WM-SETCURSOR, $WM-MOUSEACTIVATE,
	$WM-CHILDACTIVATE, $WM-QUEUESYNC, $WM-GETMINMAXINFO;
  export ptMaxSize-value, ptMaxSize-value-setter, ptMaxPosition-value,
	ptMaxPosition-value-setter, ptMinTrackSize-value,
	ptMinTrackSize-value-setter, ptMaxTrackSize-value,
	ptMaxTrackSize-value-setter, <MINMAXINFO>, <LPMINMAXINFO>,
	<PMINMAXINFO>, $WM-PAINTICON, $WM-ICONERASEBKGND, $WM-NEXTDLGCTL,
	$WM-SPOOLERSTATUS, $WM-DRAWITEM, $WM-MEASUREITEM, $WM-DELETEITEM,
	$WM-VKEYTOITEM, $WM-CHARTOITEM, $WM-SETFONT, $WM-GETFONT,
	$WM-SETHOTKEY, $WM-GETHOTKEY, $WM-QUERYDRAGICON, $WM-COMPAREITEM,
	$WM-COMPACTING, $WM-COMMNOTIFY, $WM-WINDOWPOSCHANGING,
	$WM-WINDOWPOSCHANGED;
  export $PWR-OK, $PWR-FAIL, $PWR-SUSPENDREQUEST, $PWR-SUSPENDRESUME,
	$PWR-CRITICALRESUME, $WM-COPYDATA, $WM-CANCELJOURNAL;
  export dwData-value, dwData-value-setter, <COPYDATASTRUCT>,
	<LPCOPYDATASTRUCT>, <PCOPYDATASTRUCT>, $WM-NOTIFY,
	$WM-INPUTLANGCHANGEREQUEST, $WM-INPUTLANGCHANGE, $WM-TCARD, $WM-HELP,
	$WM-USERCHANGED, $WM-NOTIFYFORMAT, $NFR-ANSI, $NFR-UNICODE,
	$NF-QUERY, $NF-REQUERY, $WM-CONTEXTMENU, $WM-STYLECHANGING,
	$WM-STYLECHANGED, $WM-DISPLAYCHANGE, $WM-GETICON, $WM-SETICON,
	$WM-NCCREATE, $WM-NCDESTROY, $WM-NCCALCSIZE, $WM-NCHITTEST,
	$WM-NCPAINT, $WM-NCACTIVATE, $WM-GETDLGCODE, $WM-SYNCPAINT,
	$WM-NCMOUSEMOVE, $WM-NCLBUTTONDOWN, $WM-NCLBUTTONUP,
	$WM-NCLBUTTONDBLCLK, $WM-NCRBUTTONDOWN, $WM-NCRBUTTONUP,
	$WM-NCRBUTTONDBLCLK, $WM-NCMBUTTONDOWN, $WM-NCMBUTTONUP,
	$WM-NCMBUTTONDBLCLK, $WM-KEYFIRST, $WM-KEYDOWN, $WM-KEYUP, $WM-CHAR,
	$WM-DEADCHAR, $WM-SYSKEYDOWN, $WM-SYSKEYUP, $WM-SYSCHAR,
	$WM-SYSDEADCHAR, $WM-KEYLAST, $WM-IME-STARTCOMPOSITION,
	$WM-IME-ENDCOMPOSITION, $WM-IME-COMPOSITION, $WM-IME-KEYLAST,
	$WM-INITDIALOG, $WM-COMMAND, $WM-SYSCOMMAND, $WM-TIMER, $WM-HSCROLL,
	$WM-VSCROLL, $WM-INITMENU, $WM-INITMENUPOPUP, $WM-MENUSELECT,
	$WM-MENUCHAR, $WM-ENTERIDLE;
  export $WM-CTLCOLORMSGBOX, $WM-CTLCOLOREDIT, $WM-CTLCOLORLISTBOX,
	$WM-CTLCOLORBTN, $WM-CTLCOLORDLG, $WM-CTLCOLORSCROLLBAR,
	$WM-CTLCOLORSTATIC, $WM-MOUSEFIRST, $WM-MOUSEMOVE, $WM-LBUTTONDOWN,
	$WM-LBUTTONUP, $WM-LBUTTONDBLCLK, $WM-RBUTTONDOWN, $WM-RBUTTONUP,
	$WM-RBUTTONDBLCLK, $WM-MBUTTONDOWN, $WM-MBUTTONUP, $WM-MBUTTONDBLCLK,
	$WM-MOUSELAST, $WM-PARENTNOTIFY, $WM-ENTERMENULOOP, $WM-EXITMENULOOP,
	$WM-NEXTMENU, hmenuIn-value, hmenuIn-value-setter, hmenuNext-value,
	hmenuNext-value-setter, hwndNext-value, hwndNext-value-setter,
	<MDINEXTMENU>, <LPMDINEXTMENU>, <PMDINEXTMENU>, $WM-SIZING,
	$WM-CAPTURECHANGED, $WM-MOVING, $WM-POWERBROADCAST,
	$PBT-APMQUERYSUSPEND, $PBT-APMQUERYSTANDBY,
	$PBT-APMQUERYSUSPENDFAILED, $PBT-APMQUERYSTANDBYFAILED,
	$PBT-APMSUSPEND, $PBT-APMSTANDBY, $PBT-APMRESUMECRITICAL,
	$PBT-APMRESUMESUSPEND, $PBT-APMRESUMESTANDBY,
	$PBTF-APMRESUMEFROMFAILURE, $PBT-APMBATTERYLOW,
	$PBT-APMPOWERSTATUSCHANGE, $PBT-APMOEMEVENT, $PBT-APMRESUMEAUTOMATIC,
	$WM-DEVICECHANGE, $WM-MDICREATE, $WM-MDIDESTROY, $WM-MDIACTIVATE,
	$WM-MDIRESTORE, $WM-MDINEXT, $WM-MDIMAXIMIZE, $WM-MDITILE,
	$WM-MDICASCADE, $WM-MDIICONARRANGE, $WM-MDIGETACTIVE, $WM-MDISETMENU,
	$WM-ENTERSIZEMOVE, $WM-EXITSIZEMOVE, $WM-DROPFILES,
	$WM-MDIREFRESHMENU, $WM-IME-SETCONTEXT, $WM-IME-NOTIFY,
	$WM-IME-CONTROL, $WM-IME-COMPOSITIONFULL, $WM-IME-SELECT,
	$WM-IME-CHAR, $WM-IME-KEYDOWN, $WM-IME-KEYUP;
  export $WM-CUT, $WM-COPY, $WM-PASTE, $WM-CLEAR, $WM-UNDO,
	$WM-RENDERFORMAT, $WM-RENDERALLFORMATS, $WM-DESTROYCLIPBOARD,
	$WM-DRAWCLIPBOARD, $WM-PAINTCLIPBOARD, $WM-VSCROLLCLIPBOARD,
	$WM-SIZECLIPBOARD, $WM-ASKCBFORMATNAME, $WM-CHANGECBCHAIN,
	$WM-HSCROLLCLIPBOARD, $WM-QUERYNEWPALETTE, $WM-PALETTEISCHANGING,
	$WM-PALETTECHANGED, $WM-HOTKEY, $WM-PRINT, $WM-PRINTCLIENT,
	$WM-HANDHELDFIRST, $WM-HANDHELDLAST, $WM-AFXFIRST, $WM-AFXLAST,
	$WM-PENWINFIRST, $WM-PENWINLAST, $WM-APP;
  export $WM-USER, $WMSZ-LEFT, $WMSZ-RIGHT, $WMSZ-TOP, $WMSZ-TOPLEFT,
	$WMSZ-TOPRIGHT, $WMSZ-BOTTOM, $WMSZ-BOTTOMLEFT, $WMSZ-BOTTOMRIGHT;
  export $HTERROR, $HTTRANSPARENT, $HTNOWHERE, $HTCLIENT, $HTCAPTION,
	$HTSYSMENU, $HTGROWBOX, $HTMENU, $HTHSCROLL, $HTVSCROLL,
	$HTMINBUTTON, $HTMAXBUTTON, $HTLEFT, $HTRIGHT, $HTTOP, $HTTOPLEFT,
	$HTTOPRIGHT, $HTBOTTOM, $HTBOTTOMLEFT, $HTBOTTOMRIGHT, $HTBORDER,
	$HTOBJECT, $HTCLOSE, $HTHELP;
  export $SMTO-NORMAL, $SMTO-BLOCK, $SMTO-ABORTIFHUNG;
  export $MA-ACTIVATE, $MA-ACTIVATEANDEAT, $MA-NOACTIVATE,
	$MA-NOACTIVATEANDEAT;
  export $ICON-SMALL, $ICON-BIG, RegisterWindowMessage;
  export $SIZE-RESTORED, $SIZE-MINIMIZED, $SIZE-MAXIMIZED,
	$SIZE-MAXSHOW, $SIZE-MAXHIDE;
  export $SIZENORMAL, $SIZEICONIC, $SIZEFULLSCREEN, $SIZEZOOMSHOW,
	$SIZEZOOMHIDE;
  export hwnd-value, hwnd-value-setter, hwndInsertAfter-value,
	hwndInsertAfter-value-setter, <WINDOWPOS>, <LPWINDOWPOS>,
	<PWINDOWPOS>;
  export rgrc-array, rgrc-array-setter, rgrc-value, lppos-value,
	lppos-value-setter, <NCCALCSIZE-PARAMS>, <LPNCCALCSIZE-PARAMS>;
  export $WVR-ALIGNTOP, $WVR-ALIGNLEFT, $WVR-ALIGNBOTTOM,
	$WVR-ALIGNRIGHT, $WVR-HREDRAW, $WVR-VREDRAW, $WVR-REDRAW,
	$WVR-VALIDRECTS;
  export $MK-LBUTTON, $MK-RBUTTON, $MK-SHIFT, $MK-CONTROL,
	$MK-MBUTTON;
  export $WS-OVERLAPPED, $WS-POPUP, $WS-CHILD, $WS-MINIMIZE,
	$WS-VISIBLE, $WS-DISABLED, $WS-CLIPSIBLINGS, $WS-CLIPCHILDREN,
	$WS-MAXIMIZE, $WS-CAPTION, $WS-BORDER, $WS-DLGFRAME, $WS-VSCROLL,
	$WS-HSCROLL, $WS-SYSMENU, $WS-THICKFRAME, $WS-GROUP, $WS-TABSTOP,
	$WS-MINIMIZEBOX, $WS-MAXIMIZEBOX, $WS-TILED, $WS-ICONIC, $WS-SIZEBOX,
	$WS-TILEDWINDOW;
  export $WS-OVERLAPPEDWINDOW, $WS-POPUPWINDOW, $WS-CHILDWINDOW;
  export $WS-EX-DLGMODALFRAME, $WS-EX-NOPARENTNOTIFY, $WS-EX-TOPMOST,
	$WS-EX-ACCEPTFILES, $WS-EX-TRANSPARENT, $WS-EX-MDICHILD,
	$WS-EX-TOOLWINDOW, $WS-EX-WINDOWEDGE, $WS-EX-CLIENTEDGE,
	$WS-EX-CONTEXTHELP, $WS-EX-RIGHT, $WS-EX-LEFT, $WS-EX-RTLREADING,
	$WS-EX-LTRREADING, $WS-EX-LEFTSCROLLBAR, $WS-EX-RIGHTSCROLLBAR,
	$WS-EX-CONTROLPARENT, $WS-EX-STATICEDGE, $WS-EX-APPWINDOW,
	$WS-EX-OVERLAPPEDWINDOW, $WS-EX-PALETTEWINDOW;
  export $CS-VREDRAW, $CS-HREDRAW, $CS-DBLCLKS, $CS-OWNDC,
	$CS-CLASSDC, $CS-PARENTDC, $CS-NOCLOSE, $CS-SAVEBITS,
	$CS-BYTEALIGNCLIENT, $CS-BYTEALIGNWINDOW, $CS-GLOBALCLASS, $CS-IME,
	$PRF-CHECKVISIBLE, $PRF-NONCLIENT, $PRF-CLIENT, $PRF-ERASEBKGND,
	$PRF-CHILDREN, $PRF-OWNED, $BDR-RAISEDOUTER, $BDR-SUNKENOUTER,
	$BDR-RAISEDINNER, $BDR-SUNKENINNER, $BDR-OUTER, $BDR-INNER,
	$EDGE-RAISED, $EDGE-SUNKEN, $EDGE-ETCHED, $EDGE-BUMP, $BF-LEFT,
	$BF-TOP, $BF-RIGHT, $BF-BOTTOM, $BF-TOPLEFT, $BF-TOPRIGHT,
	$BF-BOTTOMLEFT, $BF-BOTTOMRIGHT, $BF-RECT, $BF-DIAGONAL,
	$BF-DIAGONAL-ENDTOPRIGHT, $BF-DIAGONAL-ENDTOPLEFT,
	$BF-DIAGONAL-ENDBOTTOMLEFT, $BF-DIAGONAL-ENDBOTTOMRIGHT, $BF-MIDDLE,
	$BF-SOFT, $BF-ADJUST, $BF-FLAT, $BF-MONO, DrawEdge, $DFC-CAPTION,
	$DFC-MENU, $DFC-SCROLL, $DFC-BUTTON, $DFCS-CAPTIONCLOSE,
	$DFCS-CAPTIONMIN, $DFCS-CAPTIONMAX, $DFCS-CAPTIONRESTORE,
	$DFCS-CAPTIONHELP, $DFCS-MENUARROW, $DFCS-MENUCHECK,
	$DFCS-MENUBULLET, $DFCS-MENUARROWRIGHT, $DFCS-SCROLLUP,
	$DFCS-SCROLLDOWN, $DFCS-SCROLLLEFT, $DFCS-SCROLLRIGHT,
	$DFCS-SCROLLCOMBOBOX, $DFCS-SCROLLSIZEGRIP,
	$DFCS-SCROLLSIZEGRIPRIGHT, $DFCS-BUTTONCHECK, $DFCS-BUTTONRADIOIMAGE,
	$DFCS-BUTTONRADIOMASK, $DFCS-BUTTONRADIO, $DFCS-BUTTON3STATE,
	$DFCS-BUTTONPUSH, $DFCS-INACTIVE, $DFCS-PUSHED, $DFCS-CHECKED,
	$DFCS-ADJUSTRECT, $DFCS-FLAT, $DFCS-MONO, DrawFrameControl;
  export $DC-ACTIVE, $DC-SMALLCAP, $DC-ICON, $DC-TEXT, $DC-INBUTTON,
	DrawCaption;
  export $IDANI-OPEN, $IDANI-CLOSE, $IDANI-CAPTION, DrawAnimatedRects;
  export $CF-TEXT, $CF-BITMAP, $CF-METAFILEPICT, $CF-SYLK, $CF-DIF,
	$CF-TIFF, $CF-OEMTEXT, $CF-DIB, $CF-PALETTE, $CF-PENDATA, $CF-RIFF,
	$CF-WAVE, $CF-UNICODETEXT, $CF-ENHMETAFILE, $CF-HDROP, $CF-LOCALE,
	$CF-MAX, $CF-OWNERDISPLAY, $CF-DSPTEXT, $CF-DSPBITMAP,
	$CF-DSPMETAFILEPICT, $CF-DSPENHMETAFILE;
  export $CF-PRIVATEFIRST, $CF-PRIVATELAST;
  export $CF-GDIOBJFIRST, $CF-GDIOBJLAST;
  export $FVIRTKEY, $FNOINVERT, $FSHIFT, $FCONTROL, $FALT,
	fVirt-value, fVirt-value-setter, key-value, key-value-setter,
	cmd-value, cmd-value-setter, <ACCEL>, <LPACCEL>;
  export hdc-value, hdc-value-setter, fErase-value,
	fErase-value-setter, rcPaint-value, rcPaint-value-setter,
	fRestore-value, fRestore-value-setter, fIncUpdate-value,
	fIncUpdate-value-setter, rgbReserved-array, rgbReserved-array-setter,
	rgbReserved-value, <PAINTSTRUCT>, <LPPAINTSTRUCT>, <PPAINTSTRUCT>;
  export lpCreateParams-value, lpCreateParams-value-setter,
	hInstance-value, hInstance-value-setter, hMenu-value,
	hMenu-value-setter, hwndParent-value, hwndParent-value-setter,
	style-value, style-value-setter, lpszName-value,
	lpszName-value-setter, lpszClass-value, lpszClass-value-setter,
	dwExStyle-value, dwExStyle-value-setter, <CREATESTRUCTA>,
	<LPCREATESTRUCTA>, <CREATESTRUCT>, <LPCREATESTRUCT>, length-value,
	length-value-setter, showCmd-value, showCmd-value-setter,
	ptMinPosition-value, ptMinPosition-value-setter, ptMaxPosition-value,
	ptMaxPosition-value-setter, rcNormalPosition-value,
	rcNormalPosition-value-setter, <WINDOWPLACEMENT>,
	<LPWINDOWPLACEMENT>, <PWINDOWPLACEMENT>, $WPF-SETMINPOSITION,
	$WPF-RESTORETOMAXIMIZED, hwndFrom-value, hwndFrom-value-setter,
	idFrom-value, idFrom-value-setter, code-value, code-value-setter,
	<NMHDR>, <LPNMHDR>;
  export styleOld-value, styleOld-value-setter, styleNew-value,
	styleNew-value-setter, <STYLESTRUCT>, <LPSTYLESTRUCT>;
  export $ODT-MENU, $ODT-LISTBOX, $ODT-COMBOBOX, $ODT-BUTTON,
	$ODT-STATIC;
  export $ODA-DRAWENTIRE, $ODA-SELECT, $ODA-FOCUS;
  export $ODS-SELECTED, $ODS-GRAYED, $ODS-DISABLED, $ODS-CHECKED,
	$ODS-FOCUS, $ODS-DEFAULT, $ODS-COMBOBOXEDIT;
  export CtlType-value, CtlType-value-setter, CtlID-value,
	CtlID-value-setter, itemID-value, itemID-value-setter,
	itemWidth-value, itemWidth-value-setter, itemHeight-value,
	itemHeight-value-setter, itemData-value, itemData-value-setter,
	<MEASUREITEMSTRUCT>, <LPMEASUREITEMSTRUCT>, <PMEASUREITEMSTRUCT>;
  export CtlType-value, CtlType-value-setter, CtlID-value,
	CtlID-value-setter, itemID-value, itemID-value-setter,
	itemAction-value, itemAction-value-setter, itemState-value,
	itemState-value-setter, hwndItem-value, hwndItem-value-setter,
	hDC-value, hDC-value-setter, rcItem-value, rcItem-value-setter,
	itemData-value, itemData-value-setter, <DRAWITEMSTRUCT>,
	<LPDRAWITEMSTRUCT>, <PDRAWITEMSTRUCT>;
  export CtlType-value, CtlType-value-setter, CtlID-value,
	CtlID-value-setter, itemID-value, itemID-value-setter,
	hwndItem-value, hwndItem-value-setter, itemData-value,
	itemData-value-setter, <DELETEITEMSTRUCT>, <LPDELETEITEMSTRUCT>,
	<PDELETEITEMSTRUCT>;
  export CtlType-value, CtlType-value-setter, CtlID-value,
	CtlID-value-setter, hwndItem-value, hwndItem-value-setter,
	itemID1-value, itemID1-value-setter, itemData1-value,
	itemData1-value-setter, itemID2-value, itemID2-value-setter,
	itemData2-value, itemData2-value-setter, dwLocaleId-value,
	dwLocaleId-value-setter, <COMPAREITEMSTRUCT>, <LPCOMPAREITEMSTRUCT>,
	<PCOMPAREITEMSTRUCT>;
  export GetMessage, TranslateMessage, DispatchMessage, PeekMessage;
  export $PM-NOREMOVE, $PM-REMOVE, $PM-NOYIELD, RegisterHotKey,
	UnregisterHotKey, $MOD-ALT, $MOD-CONTROL, $MOD-SHIFT, $MOD-WIN,
	$IDHOT-SNAPWINDOW, $IDHOT-SNAPDESKTOP, $ENDSESSION-LOGOFF,
	$EWX-LOGOFF, $EWX-SHUTDOWN, $EWX-REBOOT, $EWX-FORCE, $EWX-POWEROFF;
  export ExitWindowsEx, SwapMouseButton, GetMessagePos,
	GetMessageTime, GetMessageExtraInfo, SetMessageExtraInfo,
	SendMessage, SendMessageTimeout, SendNotifyMessage,
	SendMessageCallback, BroadcastSystemMessage, $BSM-ALLCOMPONENTS,
	$BSM-VXDS, $BSM-NETDRIVER, $BSM-INSTALLABLEDRIVERS,
	$BSM-APPLICATIONS, $BSM-ALLDESKTOPS, $BSF-QUERY,
	$BSF-IGNORECURRENTTASK, $BSF-FLUSHDISK, $BSF-NOHANG,
	$BSF-POSTMESSAGE, $BSF-FORCEIFHUNG, $BSF-NOTIMEOUTIFNOTHUNG,
	$BROADCAST-QUERY-DENY;
  export PostMessage;
  export $HWND-BROADCAST;
  export ReplyMessage, WaitMessage, WaitForInputIdle, DefWindowProc,
	PostQuitMessage, CallWindowProc, InSendMessage, GetDoubleClickTime,
	SetDoubleClickTime, RegisterClass, UnregisterClass, GetClassInfo,
	RegisterClassEx, GetClassInfoEx, $CW-USEDEFAULT;
  export $HWND-DESKTOP, IsWindow, IsMenu, IsChild, DestroyWindow,
	ShowWindow, ShowWindowAsync, FlashWindow, ShowOwnedPopups, OpenIcon,
	CloseWindow, MoveWindow, SetWindowPos, GetWindowPlacement,
	SetWindowPlacement;
  export BeginDeferWindowPos, DeferWindowPos, EndDeferWindowPos,
	IsWindowVisible, IsIconic, AnyPopup, BringWindowToTop, IsZoomed;
  export $SWP-NOSIZE, $SWP-NOMOVE, $SWP-NOZORDER, $SWP-NOREDRAW,
	$SWP-NOACTIVATE, $SWP-FRAMECHANGED, $SWP-SHOWWINDOW, $SWP-HIDEWINDOW,
	$SWP-NOCOPYBITS, $SWP-NOOWNERZORDER, $SWP-NOSENDCHANGING,
	$SWP-DRAWFRAME, $SWP-NOREPOSITION, $SWP-DEFERERASE,
	$SWP-ASYNCWINDOWPOS;
  export $HWND-TOP, $HWND-BOTTOM, $HWND-TOPMOST, $HWND-NOTOPMOST;
  export style-value, style-value-setter, dwExtendedStyle-value,
	dwExtendedStyle-value-setter, cdit-value, cdit-value-setter,
	<DLGTEMPLATE>, <LPDLGTEMPLATE>, <LPCDLGTEMPLATEA>, <LPCDLGTEMPLATE>;
  export style-value, style-value-setter, dwExtendedStyle-value,
	dwExtendedStyle-value-setter, id-value, id-value-setter,
	<DLGITEMTEMPLATE>, <LPDLGITEMTEMPLATE>, <PDLGITEMTEMPLATEA>,
	<PDLGITEMTEMPLATE>, CreateDialogParam, CreateDialogIndirectParam,
	DialogBoxParam, DialogBoxIndirectParam, EndDialog, GetDlgItem,
	SetDlgItemInt, GetDlgItemInt, SetDlgItemText, GetDlgItemText,
	CheckDlgButton, CheckRadioButton, IsDlgButtonChecked,
	SendDlgItemMessage, GetNextDlgGroupItem, GetNextDlgTabItem,
	GetDlgCtrlID, GetDialogBaseUnits, DefDlgProc;
  export $DLGWINDOWEXTRA, CallMsgFilter;
  export OpenClipboard, CloseClipboard;
  export GetClipboardOwner, SetClipboardViewer, GetClipboardViewer,
	ChangeClipboardChain, SetClipboardData, GetClipboardData,
	RegisterClipboardFormat, CountClipboardFormats, EnumClipboardFormats,
	GetClipboardFormatName, EmptyClipboard, IsClipboardFormatAvailable,
	GetPriorityClipboardFormat, GetOpenClipboardWindow;
  export CharToOem, OemToChar, CharToOemBuff, OemToCharBuff,
	CharUpper, CharUpperBuff, CharLower, CharLowerBuff, CharNext,
	CharPrev, CharNextEx, CharPrevEx;
  export IsCharAlpha, IsCharAlphaNumeric, IsCharUpper, IsCharLower,
	SetFocus, GetActiveWindow, GetFocus, GetKeyState, GetAsyncKeyState,
	GetKeyboardState, SetKeyboardState, GetKeyNameText, GetKeyboardType,
	ToAscii, ToAsciiEx, ToUnicode, OemKeyScan, VkKeyScan, VkKeyScanEx,
	$KEYEVENTF-EXTENDEDKEY, $KEYEVENTF-KEYUP, keybd-event,
	$MOUSEEVENTF-MOVE, $MOUSEEVENTF-LEFTDOWN, $MOUSEEVENTF-LEFTUP,
	$MOUSEEVENTF-RIGHTDOWN, $MOUSEEVENTF-RIGHTUP,
	$MOUSEEVENTF-MIDDLEDOWN, $MOUSEEVENTF-MIDDLEUP, $MOUSEEVENTF-WHEEL,
	$MOUSEEVENTF-ABSOLUTE, mouse-event, MapVirtualKey, MapVirtualKeyEx,
	GetInputState, GetQueueStatus, GetCapture, SetCapture,
	ReleaseCapture, MsgWaitForMultipleObjects, $MWMO-WAITALL,
	$MWMO-ALERTABLE, $MWMO-INPUTAVAILABLE;
  export $QS-KEY, $QS-MOUSEMOVE, $QS-MOUSEBUTTON, $QS-POSTMESSAGE,
	$QS-TIMER, $QS-PAINT, $QS-SENDMESSAGE, $QS-HOTKEY,
	$QS-ALLPOSTMESSAGE, $QS-MOUSE, $QS-INPUT, $QS-ALLEVENTS,
	$QS-ALLINPUT;
  export SetTimer, KillTimer, IsWindowUnicode, EnableWindow,
	IsWindowEnabled, LoadAccelerators, CreateAcceleratorTable,
	DestroyAcceleratorTable, CopyAcceleratorTable, TranslateAccelerator;
  export $SM-CXSCREEN, $SM-CYSCREEN, $SM-CXVSCROLL, $SM-CYHSCROLL,
	$SM-CYCAPTION, $SM-CXBORDER, $SM-CYBORDER, $SM-CXDLGFRAME,
	$SM-CYDLGFRAME, $SM-CYVTHUMB, $SM-CXHTHUMB, $SM-CXICON, $SM-CYICON,
	$SM-CXCURSOR, $SM-CYCURSOR, $SM-CYMENU, $SM-CXFULLSCREEN,
	$SM-CYFULLSCREEN, $SM-CYKANJIWINDOW, $SM-MOUSEPRESENT, $SM-CYVSCROLL,
	$SM-CXHSCROLL, $SM-DEBUG, $SM-SWAPBUTTON, $SM-RESERVED1,
	$SM-RESERVED2, $SM-RESERVED3, $SM-RESERVED4, $SM-CXMIN, $SM-CYMIN,
	$SM-CXSIZE, $SM-CYSIZE, $SM-CXFRAME, $SM-CYFRAME, $SM-CXMINTRACK,
	$SM-CYMINTRACK, $SM-CXDOUBLECLK, $SM-CYDOUBLECLK, $SM-CXICONSPACING,
	$SM-CYICONSPACING, $SM-MENUDROPALIGNMENT, $SM-PENWINDOWS,
	$SM-DBCSENABLED, $SM-CMOUSEBUTTONS, $SM-CXFIXEDFRAME,
	$SM-CYFIXEDFRAME, $SM-CXSIZEFRAME, $SM-CYSIZEFRAME, $SM-SECURE,
	$SM-CXEDGE, $SM-CYEDGE, $SM-CXMINSPACING, $SM-CYMINSPACING,
	$SM-CXSMICON, $SM-CYSMICON, $SM-CYSMCAPTION, $SM-CXSMSIZE,
	$SM-CYSMSIZE, $SM-CXMENUSIZE, $SM-CYMENUSIZE, $SM-ARRANGE,
	$SM-CXMINIMIZED, $SM-CYMINIMIZED, $SM-CXMAXTRACK, $SM-CYMAXTRACK,
	$SM-CXMAXIMIZED, $SM-CYMAXIMIZED, $SM-NETWORK, $SM-CLEANBOOT,
	$SM-CXDRAG, $SM-CYDRAG, $SM-SHOWSOUNDS, $SM-CXMENUCHECK,
	$SM-CYMENUCHECK, $SM-SLOWMACHINE, $SM-MIDEASTENABLED, $SM-CMETRICS,
	GetSystemMetrics, LoadMenu, LoadMenuIndirect, GetMenu, SetMenu,
	ChangeMenu, HiliteMenuItem, GetMenuString, GetMenuState, DrawMenuBar;
  export GetSystemMenu;
  export CreateMenu, CreatePopupMenu, DestroyMenu, CheckMenuItem,
	EnableMenuItem, GetSubMenu, GetMenuItemID, GetMenuItemCount,
	InsertMenu, AppendMenu, ModifyMenu, RemoveMenu, DeleteMenu,
	SetMenuItemBitmaps, GetMenuCheckMarkDimensions, TrackPopupMenu,
	$MNC-IGNORE, $MNC-CLOSE, $MNC-EXECUTE, $MNC-SELECT, rcExclude-value,
	rcExclude-value-setter, <TPMPARAMS>, <LPTPMPARAMS>, TrackPopupMenuEx,
	$MIIM-STATE, $MIIM-ID, $MIIM-SUBMENU, $MIIM-CHECKMARKS, $MIIM-TYPE,
	$MIIM-DATA, fMask-value, fMask-value-setter, fType-value,
	fType-value-setter, fState-value, fState-value-setter, wID-value,
	wID-value-setter, hSubMenu-value, hSubMenu-value-setter,
	hbmpChecked-value, hbmpChecked-value-setter, hbmpUnchecked-value,
	hbmpUnchecked-value-setter, dwItemData-value,
	dwItemData-value-setter, dwTypeData-value, dwTypeData-value-setter,
	cch-value, cch-value-setter, <MENUITEMINFOA>, <LPMENUITEMINFOA>,
	<MENUITEMINFO>, <LPMENUITEMINFO>, <LPCMENUITEMINFOA>,
	<LPCMENUITEMINFO>, InsertMenuItem, GetMenuItemInfo, SetMenuItemInfo;
  export $GMDI-USEDISABLED, $GMDI-GOINTOPOPUPS, GetMenuDefaultItem,
	SetMenuDefaultItem, GetMenuItemRect, MenuItemFromPoint;
  export $TPM-LEFTBUTTON, $TPM-RIGHTBUTTON, $TPM-LEFTALIGN,
	$TPM-CENTERALIGN, $TPM-RIGHTALIGN, $TPM-TOPALIGN, $TPM-VCENTERALIGN,
	$TPM-BOTTOMALIGN, $TPM-HORIZONTAL, $TPM-VERTICAL, $TPM-NONOTIFY,
	$TPM-RETURNCMD;
  export hwndSource-value, hwndSource-value-setter, hwndSink-value,
	hwndSink-value-setter, wFmt-value, wFmt-value-setter, dwData-value,
	dwData-value-setter, ptDrop-value, ptDrop-value-setter,
	dwControlData-value, dwControlData-value-setter, <DROPSTRUCT>,
	<LPDROPSTRUCT>, <PDROPSTRUCT>, $DOF-EXECUTABLE, $DOF-DOCUMENT,
	$DOF-DIRECTORY, $DOF-MULTIPLE, $DOF-PROGMAN, $DOF-SHELLDATA,
	$DO-DROPFILE, $DO-PRINTFILE, DragObject, DragDetect, DrawIcon;
  export $DT-TOP, $DT-LEFT, $DT-CENTER, $DT-RIGHT, $DT-VCENTER,
	$DT-BOTTOM, $DT-WORDBREAK, $DT-SINGLELINE, $DT-EXPANDTABS,
	$DT-TABSTOP, $DT-NOCLIP, $DT-EXTERNALLEADING, $DT-CALCRECT,
	$DT-NOPREFIX, $DT-INTERNAL, $DT-EDITCONTROL, $DT-PATH-ELLIPSIS,
	$DT-END-ELLIPSIS, $DT-MODIFYSTRING, $DT-RTLREADING,
	$DT-WORD-ELLIPSIS;
  export iTabLength-value, iTabLength-value-setter, iLeftMargin-value,
	iLeftMargin-value-setter, iRightMargin-value,
	iRightMargin-value-setter, uiLengthDrawn-value,
	uiLengthDrawn-value-setter, <DRAWTEXTPARAMS>, <LPDRAWTEXTPARAMS>;
  export DrawText, DrawTextEx, GrayString, $DST-COMPLEX, $DST-TEXT,
	$DST-PREFIXTEXT, $DST-ICON, $DST-BITMAP, $DSS-NORMAL, $DSS-UNION,
	$DSS-DISABLED, $DSS-MONO, $DSS-RIGHT, DrawState, TabbedTextOut,
	GetTabbedTextExtent, UpdateWindow, SetActiveWindow,
	GetForegroundWindow, PaintDesktop, SetForegroundWindow, WindowFromDC,
	GetDC, GetDCEx;
  export $DCX-WINDOW, $DCX-CACHE, $DCX-NORESETATTRS,
	$DCX-CLIPCHILDREN, $DCX-CLIPSIBLINGS, $DCX-PARENTCLIP,
	$DCX-EXCLUDERGN, $DCX-INTERSECTRGN, $DCX-EXCLUDEUPDATE,
	$DCX-INTERSECTUPDATE, $DCX-LOCKWINDOWUPDATE, $DCX-VALIDATE;
  export GetWindowDC, ReleaseDC, BeginPaint, EndPaint, GetUpdateRect,
	GetUpdateRgn, SetWindowRgn, GetWindowRgn, ExcludeUpdateRgn,
	InvalidateRect, ValidateRect, InvalidateRgn, ValidateRgn;
  export RedrawWindow;
  export $RDW-INVALIDATE, $RDW-INTERNALPAINT, $RDW-ERASE,
	$RDW-VALIDATE, $RDW-NOINTERNALPAINT, $RDW-NOERASE, $RDW-NOCHILDREN,
	$RDW-ALLCHILDREN, $RDW-UPDATENOW, $RDW-ERASENOW, $RDW-FRAME,
	$RDW-NOFRAME;
  export LockWindowUpdate, ScrollWindow, ScrollDC, ScrollWindowEx,
	$SW-SCROLLCHILDREN, $SW-INVALIDATE, $SW-ERASE, GetScrollPos,
	GetScrollRange, ShowScrollBar, EnableScrollBar;
  export $ESB-ENABLE-BOTH, $ESB-DISABLE-BOTH, $ESB-DISABLE-LEFT,
	$ESB-DISABLE-RIGHT, $ESB-DISABLE-UP, $ESB-DISABLE-DOWN,
	$ESB-DISABLE-LTUP, $ESB-DISABLE-RTDN, SetProp, GetProp, RemoveProp,
	EnumPropsEx, EnumProps, SetWindowText, GetWindowText,
	GetWindowTextLength, GetClientRect, GetWindowRect, AdjustWindowRect,
	AdjustWindowRectEx, $HELPINFO-WINDOW, $HELPINFO-MENUITEM,
	iContextType-value, iContextType-value-setter, iCtrlId-value,
	iCtrlId-value-setter, hItemHandle-value, hItemHandle-value-setter,
	dwContextId-value, dwContextId-value-setter, MousePos-value,
	MousePos-value-setter, <HELPINFO>, <LPHELPINFO>,
	SetWindowContextHelpId, GetWindowContextHelpId, SetMenuContextHelpId,
	GetMenuContextHelpId;
  export $MB-OK, $MB-OKCANCEL, $MB-ABORTRETRYIGNORE, $MB-YESNOCANCEL,
	$MB-YESNO, $MB-RETRYCANCEL, $MB-ICONHAND, $MB-ICONQUESTION,
	$MB-ICONEXCLAMATION, $MB-ICONASTERISK, $MB-USERICON, $MB-ICONWARNING,
	$MB-ICONERROR, $MB-ICONINFORMATION, $MB-ICONSTOP, $MB-DEFBUTTON1,
	$MB-DEFBUTTON2, $MB-DEFBUTTON3, $MB-DEFBUTTON4, $MB-APPLMODAL,
	$MB-SYSTEMMODAL, $MB-TASKMODAL, $MB-HELP, $MB-NOFOCUS,
	$MB-SETFOREGROUND, $MB-DEFAULT-DESKTOP-ONLY, $MB-TOPMOST, $MB-RIGHT,
	$MB-RTLREADING, $MB-TYPEMASK, $MB-ICONMASK, $MB-DEFMASK,
	$MB-MODEMASK, $MB-MISCMASK, MessageBox, MessageBoxEx,
	<MSGBOXCALLBACK>;
  export hwndOwner-value, hwndOwner-value-setter, hInstance-value,
	hInstance-value-setter, lpszText-value, lpszText-value-setter,
	lpszCaption-value, lpszCaption-value-setter, dwStyle-value,
	dwStyle-value-setter, lpszIcon-value, lpszIcon-value-setter,
	dwContextHelpId-value, dwContextHelpId-value-setter,
	lpfnMsgBoxCallback-value, lpfnMsgBoxCallback-value-setter,
	dwLanguageId-value, dwLanguageId-value-setter, <MSGBOXPARAMSA>,
	<LPMSGBOXPARAMSA>, <PMSGBOXPARAMSA>, <MSGBOXPARAMS>, <PMSGBOXPARAMS>,
	<LPMSGBOXPARAMS>;
  export MessageBoxIndirect;
  export MessageBeep, ShowCursor, SetCursorPos, SetCursor,
	GetCursorPos, ClipCursor, GetClipCursor, GetCursor, CreateCaret,
	GetCaretBlinkTime, SetCaretBlinkTime, DestroyCaret, HideCaret,
	ShowCaret, SetCaretPos, GetCaretPos, ClientToScreen, ScreenToClient,
	MapWindowPoints, WindowFromPoint, ChildWindowFromPoint, $CWP-ALL,
	$CWP-SKIPINVISIBLE, $CWP-SKIPDISABLED, $CWP-SKIPTRANSPARENT,
	ChildWindowFromPointEx;
  export $CTLCOLOR-MSGBOX, $CTLCOLOR-EDIT, $CTLCOLOR-LISTBOX,
	$CTLCOLOR-BTN, $CTLCOLOR-DLG, $CTLCOLOR-SCROLLBAR, $CTLCOLOR-STATIC,
	$CTLCOLOR-MAX, $COLOR-SCROLLBAR, $COLOR-BACKGROUND,
	$COLOR-ACTIVECAPTION, $COLOR-INACTIVECAPTION, $COLOR-MENU,
	$COLOR-WINDOW, $COLOR-WINDOWFRAME, $COLOR-MENUTEXT,
	$COLOR-WINDOWTEXT, $COLOR-CAPTIONTEXT, $COLOR-ACTIVEBORDER,
	$COLOR-INACTIVEBORDER, $COLOR-APPWORKSPACE, $COLOR-HIGHLIGHT,
	$COLOR-HIGHLIGHTTEXT, $COLOR-BTNFACE, $COLOR-BTNSHADOW,
	$COLOR-GRAYTEXT, $COLOR-BTNTEXT, $COLOR-INACTIVECAPTIONTEXT,
	$COLOR-BTNHIGHLIGHT, $COLOR-3DDKSHADOW, $COLOR-3DLIGHT,
	$COLOR-INFOTEXT, $COLOR-INFOBK, $COLOR-DESKTOP, $COLOR-3DFACE,
	$COLOR-3DSHADOW, $COLOR-3DHIGHLIGHT, $COLOR-3DHILIGHT,
	$COLOR-BTNHILIGHT;
  export GetSysColor, GetSysColorBrush;
  export SetSysColors, DrawFocusRect, FillRect, FrameRect, InvertRect,
	SetRect, SetRectEmpty, CopyRect, InflateRect, IntersectRect,
	UnionRect, SubtractRect, OffsetRect, IsRectEmpty, EqualRect,
	PtInRect, GetWindowWord, SetWindowWord, GetWindowLong, SetWindowLong,
	GetClassWord, SetClassWord, GetClassLong, SetClassLong,
	GetDesktopWindow;
  export GetParent, SetParent, EnumChildWindows, FindWindow,
	FindWindowEx;
  export EnumWindows, GetClassName, GetTopWindow, GetLastActivePopup;
  export $GW-HWNDFIRST, $GW-HWNDLAST, $GW-HWNDNEXT, $GW-HWNDPREV,
	$GW-OWNER, $GW-CHILD, $GW-MAX, GetWindow;
  export SetWindowsHookEx, UnhookWindowsHookEx, CallNextHookEx;
  export $MF-INSERT, $MF-CHANGE, $MF-APPEND, $MF-DELETE, $MF-REMOVE,
	$MF-BYCOMMAND, $MF-BYPOSITION, $MF-SEPARATOR, $MF-ENABLED,
	$MF-GRAYED, $MF-DISABLED, $MF-UNCHECKED, $MF-CHECKED,
	$MF-USECHECKBITMAPS, $MF-STRING, $MF-BITMAP, $MF-OWNERDRAW,
	$MF-POPUP, $MF-MENUBARBREAK, $MF-MENUBREAK, $MF-UNHILITE, $MF-HILITE,
	$MF-DEFAULT, $MF-SYSMENU, $MF-HELP, $MF-RIGHTJUSTIFY,
	$MF-MOUSESELECT;
  export $MFT-STRING, $MFT-BITMAP, $MFT-MENUBARBREAK, $MFT-MENUBREAK,
	$MFT-OWNERDRAW, $MFT-RADIOCHECK, $MFT-SEPARATOR, $MFT-RIGHTORDER,
	$MFT-RIGHTJUSTIFY, $MFS-GRAYED, $MFS-DISABLED, $MFS-CHECKED,
	$MFS-HILITE, $MFS-ENABLED, $MFS-UNCHECKED, $MFS-UNHILITE,
	$MFS-DEFAULT, CheckMenuRadioItem;
  export versionNumber-value, versionNumber-value-setter,
	<MENUITEMTEMPLATEHEADER>, <LPMENUITEMTEMPLATEHEADER>,
	<PMENUITEMTEMPLATEHEADER>;
  export mtOption-value, mtOption-value-setter, mtID-value,
	mtID-value-setter, mtString-array, mtString-array-setter,
	mtString-value, <MENUITEMTEMPLATE>, <LPMENUITEMTEMPLATE>,
	<PMENUITEMTEMPLATE>, $MF-END;
  export $SC-SIZE, $SC-MOVE, $SC-MINIMIZE, $SC-MAXIMIZE,
	$SC-NEXTWINDOW, $SC-PREVWINDOW, $SC-CLOSE, $SC-VSCROLL, $SC-HSCROLL,
	$SC-MOUSEMENU, $SC-KEYMENU, $SC-ARRANGE, $SC-RESTORE, $SC-TASKLIST,
	$SC-SCREENSAVE, $SC-HOTKEY, $SC-DEFAULT, $SC-MONITORPOWER,
	$SC-CONTEXTHELP, $SC-SEPARATOR;
  export $SC-ICON, $SC-ZOOM;
  export LoadBitmap, LoadCursor, LoadCursorFromFile, CreateCursor,
	DestroyCursor;
  export $IDC-ARROW, $IDC-IBEAM, $IDC-WAIT, $IDC-CROSS, $IDC-UPARROW,
	$IDC-SIZE, $IDC-ICON, $IDC-SIZENWSE, $IDC-SIZENESW, $IDC-SIZEWE,
	$IDC-SIZENS, $IDC-SIZEALL, $IDC-NO, $IDC-APPSTARTING, $IDC-HELP,
	SetSystemCursor;
  export fIcon-value, fIcon-value-setter, xHotspot-value,
	xHotspot-value-setter, yHotspot-value, yHotspot-value-setter,
	hbmMask-value, hbmMask-value-setter, hbmColor-value,
	hbmColor-value-setter, <ICONINFO>, <LPICONINFO>, <PICONINFO>,
	LoadIcon, CreateIcon, DestroyIcon, LookupIconIdFromDirectory,
	LookupIconIdFromDirectoryEx, CreateIconFromResource,
	CreateIconFromResourceEx, xHotSpot-value, xHotSpot-value-setter,
	yHotSpot-value, yHotSpot-value-setter, cbWidth-value,
	cbWidth-value-setter, Planes-value, Planes-value-setter,
	BitsPixel-value, BitsPixel-value-setter, <CURSORSHAPE>,
	<LPCURSORSHAPE>, $IMAGE-BITMAP, $IMAGE-ICON, $IMAGE-CURSOR,
	$IMAGE-ENHMETAFILE, $LR-DEFAULTCOLOR, $LR-MONOCHROME, $LR-COLOR,
	$LR-COPYRETURNORG, $LR-COPYDELETEORG, $LR-LOADFROMFILE,
	$LR-LOADTRANSPARENT, $LR-DEFAULTSIZE, $LR-VGACOLOR,
	$LR-LOADMAP3DCOLORS, $LR-CREATEDIBSECTION, $LR-COPYFROMRESOURCE,
	$LR-SHARED, LoadImage, CopyImage, $DI-MASK, $DI-IMAGE, $DI-NORMAL,
	$DI-COMPAT, $DI-DEFAULTSIZE, DrawIconEx, CreateIconIndirect,
	CopyIcon, GetIconInfo, $RES-ICON, $RES-CURSOR;
  export $OBM-CLOSE, $OBM-UPARROW, $OBM-DNARROW, $OBM-RGARROW,
	$OBM-LFARROW, $OBM-REDUCE, $OBM-ZOOM, $OBM-RESTORE, $OBM-REDUCED,
	$OBM-ZOOMD, $OBM-RESTORED, $OBM-UPARROWD, $OBM-DNARROWD,
	$OBM-RGARROWD, $OBM-LFARROWD, $OBM-MNARROW, $OBM-COMBO,
	$OBM-UPARROWI, $OBM-DNARROWI, $OBM-RGARROWI, $OBM-LFARROWI,
	$OBM-OLD-CLOSE, $OBM-SIZE, $OBM-OLD-UPARROW, $OBM-OLD-DNARROW,
	$OBM-OLD-RGARROW, $OBM-OLD-LFARROW, $OBM-BTSIZE, $OBM-CHECK,
	$OBM-CHECKBOXES, $OBM-BTNCORNERS, $OBM-OLD-REDUCE, $OBM-OLD-ZOOM,
	$OBM-OLD-RESTORE, $OCR-NORMAL, $OCR-IBEAM, $OCR-WAIT, $OCR-CROSS,
	$OCR-UP, $OCR-SIZE, $OCR-ICON, $OCR-SIZENWSE, $OCR-SIZENESW,
	$OCR-SIZEWE, $OCR-SIZENS, $OCR-SIZEALL, $OCR-ICOCUR, $OCR-NO,
	$OCR-APPSTARTING, $OIC-SAMPLE, $OIC-HAND, $OIC-QUES, $OIC-BANG,
	$OIC-NOTE, $OIC-WINLOGO, $OIC-WARNING, $OIC-ERROR, $OIC-INFORMATION;
  export $ORD-LANGDRIVER;
  export $IDI-APPLICATION, $IDI-HAND, $IDI-QUESTION, $IDI-EXCLAMATION,
	$IDI-ASTERISK, $IDI-WINLOGO, $IDI-WARNING, $IDI-ERROR,
	$IDI-INFORMATION, LoadString;
  export $IDOK, $IDCANCEL, $IDABORT, $IDRETRY, $IDIGNORE, $IDYES,
	$IDNO, $IDCLOSE, $IDHELP;
  export $ES-LEFT, $ES-CENTER, $ES-RIGHT, $ES-MULTILINE,
	$ES-UPPERCASE, $ES-LOWERCASE, $ES-PASSWORD, $ES-AUTOVSCROLL,
	$ES-AUTOHSCROLL, $ES-NOHIDESEL, $ES-OEMCONVERT, $ES-READONLY,
	$ES-WANTRETURN, $ES-NUMBER;
  export $EN-SETFOCUS, $EN-KILLFOCUS, $EN-CHANGE, $EN-UPDATE,
	$EN-ERRSPACE, $EN-MAXTEXT, $EN-HSCROLL, $EN-VSCROLL, $EC-LEFTMARGIN,
	$EC-RIGHTMARGIN, $EC-USEFONTINFO;
  export $EM-GETSEL, $EM-SETSEL, $EM-GETRECT, $EM-SETRECT,
	$EM-SETRECTNP, $EM-SCROLL, $EM-LINESCROLL, $EM-SCROLLCARET,
	$EM-GETMODIFY, $EM-SETMODIFY, $EM-GETLINECOUNT, $EM-LINEINDEX,
	$EM-SETHANDLE, $EM-GETHANDLE, $EM-GETTHUMB, $EM-LINELENGTH,
	$EM-REPLACESEL, $EM-GETLINE, $EM-LIMITTEXT, $EM-CANUNDO, $EM-UNDO,
	$EM-FMTLINES, $EM-LINEFROMCHAR, $EM-SETTABSTOPS, $EM-SETPASSWORDCHAR,
	$EM-EMPTYUNDOBUFFER, $EM-GETFIRSTVISIBLELINE, $EM-SETREADONLY,
	$EM-SETWORDBREAKPROC, $EM-GETWORDBREAKPROC, $EM-GETPASSWORDCHAR,
	$EM-SETMARGINS, $EM-GETMARGINS, $EM-SETLIMITTEXT, $EM-GETLIMITTEXT,
	$EM-POSFROMCHAR, $EM-CHARFROMPOS;
  export $WB-LEFT, $WB-RIGHT, $WB-ISDELIMITER;
  export $BS-PUSHBUTTON, $BS-DEFPUSHBUTTON, $BS-CHECKBOX,
	$BS-AUTOCHECKBOX, $BS-RADIOBUTTON, $BS-3STATE, $BS-AUTO3STATE,
	$BS-GROUPBOX, $BS-AUTORADIOBUTTON, $BS-OWNERDRAW, $BS-LEFTTEXT,
	$BS-TEXT, $BS-ICON, $BS-BITMAP, $BS-LEFT, $BS-RIGHT, $BS-CENTER,
	$BS-TOP, $BS-BOTTOM, $BS-VCENTER, $BS-PUSHLIKE, $BS-MULTILINE,
	$BS-NOTIFY, $BS-FLAT, $BS-RIGHTBUTTON;
  export $BN-CLICKED, $BN-DOUBLECLICKED, $BN-DBLCLK, $BN-SETFOCUS,
	$BN-KILLFOCUS;
  export $BM-GETCHECK, $BM-SETCHECK, $BM-GETSTATE, $BM-SETSTATE,
	$BM-SETSTYLE, $BM-CLICK, $BM-GETIMAGE, $BM-SETIMAGE, $BST-UNCHECKED,
	$BST-CHECKED, $BST-INDETERMINATE, $BST-PUSHED, $BST-FOCUS;
  export $SS-LEFT, $SS-CENTER, $SS-RIGHT, $SS-ICON, $SS-BLACKRECT,
	$SS-GRAYRECT, $SS-WHITERECT, $SS-BLACKFRAME, $SS-GRAYFRAME,
	$SS-WHITEFRAME, $SS-USERITEM, $SS-SIMPLE, $SS-LEFTNOWORDWRAP,
	$SS-OWNERDRAW, $SS-BITMAP, $SS-ENHMETAFILE, $SS-ETCHEDHORZ,
	$SS-ETCHEDVERT, $SS-ETCHEDFRAME, $SS-TYPEMASK, $SS-NOPREFIX,
	$SS-NOTIFY, $SS-CENTERIMAGE, $SS-RIGHTJUST, $SS-REALSIZEIMAGE,
	$SS-SUNKEN, $SS-ENDELLIPSIS, $SS-PATHELLIPSIS, $SS-WORDELLIPSIS,
	$SS-ELLIPSISMASK;
  export $STM-SETICON, $STM-GETICON, $STM-SETIMAGE, $STM-GETIMAGE,
	$STN-CLICKED, $STN-DBLCLK, $STN-ENABLE, $STN-DISABLE, $STM-MSGMAX;
  export $DWL-MSGRESULT, $DWL-DLGPROC, $DWL-USER;
  export IsDialogMessage, MapDialogRect, DlgDirList;
  export $DDL-READWRITE, $DDL-READONLY, $DDL-HIDDEN, $DDL-SYSTEM,
	$DDL-DIRECTORY, $DDL-ARCHIVE, $DDL-POSTMSGS, $DDL-DRIVES,
	$DDL-EXCLUSIVE, DlgDirSelectEx, DlgDirListComboBox,
	DlgDirSelectComboBoxEx;
  export $DS-ABSALIGN, $DS-SYSMODAL, $DS-LOCALEDIT, $DS-SETFONT,
	$DS-MODALFRAME, $DS-NOIDLEMSG, $DS-SETFOREGROUND, $DS-3DLOOK,
	$DS-FIXEDSYS, $DS-NOFAILCREATE, $DS-CONTROL, $DS-CENTER,
	$DS-CENTERMOUSE, $DS-CONTEXTHELP, $DM-GETDEFID, $DM-SETDEFID,
	$DM-REPOSITION;
  export $DC-HASDEFID;
  export $DLGC-WANTARROWS, $DLGC-WANTTAB, $DLGC-WANTALLKEYS,
	$DLGC-WANTMESSAGE, $DLGC-HASSETSEL, $DLGC-DEFPUSHBUTTON,
	$DLGC-UNDEFPUSHBUTTON, $DLGC-RADIOBUTTON, $DLGC-WANTCHARS,
	$DLGC-STATIC, $DLGC-BUTTON, $LB-CTLCODE;
  export $LB-OKAY, $LB-ERR, $LB-ERRSPACE;
  export $LBN-ERRSPACE, $LBN-SELCHANGE, $LBN-DBLCLK, $LBN-SELCANCEL,
	$LBN-SETFOCUS, $LBN-KILLFOCUS;
  export $LB-ADDSTRING, $LB-INSERTSTRING, $LB-DELETESTRING,
	$LB-SELITEMRANGEEX, $LB-RESETCONTENT, $LB-SETSEL, $LB-SETCURSEL,
	$LB-GETSEL, $LB-GETCURSEL, $LB-GETTEXT, $LB-GETTEXTLEN, $LB-GETCOUNT,
	$LB-SELECTSTRING, $LB-DIR, $LB-GETTOPINDEX, $LB-FINDSTRING,
	$LB-GETSELCOUNT, $LB-GETSELITEMS, $LB-SETTABSTOPS,
	$LB-GETHORIZONTALEXTENT, $LB-SETHORIZONTALEXTENT, $LB-SETCOLUMNWIDTH,
	$LB-ADDFILE, $LB-SETTOPINDEX, $LB-GETITEMRECT, $LB-GETITEMDATA,
	$LB-SETITEMDATA, $LB-SELITEMRANGE, $LB-SETANCHORINDEX,
	$LB-GETANCHORINDEX, $LB-SETCARETINDEX, $LB-GETCARETINDEX,
	$LB-SETITEMHEIGHT, $LB-GETITEMHEIGHT, $LB-FINDSTRINGEXACT,
	$LB-SETLOCALE, $LB-GETLOCALE, $LB-SETCOUNT, $LB-INITSTORAGE,
	$LB-ITEMFROMPOINT, $LB-MSGMAX;
  export $LBS-NOTIFY, $LBS-SORT, $LBS-NOREDRAW, $LBS-MULTIPLESEL,
	$LBS-OWNERDRAWFIXED, $LBS-OWNERDRAWVARIABLE, $LBS-HASSTRINGS,
	$LBS-USETABSTOPS, $LBS-NOINTEGRALHEIGHT, $LBS-MULTICOLUMN,
	$LBS-WANTKEYBOARDINPUT, $LBS-EXTENDEDSEL, $LBS-DISABLENOSCROLL,
	$LBS-NODATA, $LBS-NOSEL, $LBS-STANDARD;
  export $CB-OKAY, $CB-ERR, $CB-ERRSPACE;
  export $CBN-ERRSPACE, $CBN-SELCHANGE, $CBN-DBLCLK, $CBN-SETFOCUS,
	$CBN-KILLFOCUS, $CBN-EDITCHANGE, $CBN-EDITUPDATE, $CBN-DROPDOWN,
	$CBN-CLOSEUP, $CBN-SELENDOK, $CBN-SELENDCANCEL;
  export $CBS-SIMPLE, $CBS-DROPDOWN, $CBS-DROPDOWNLIST,
	$CBS-OWNERDRAWFIXED, $CBS-OWNERDRAWVARIABLE, $CBS-AUTOHSCROLL,
	$CBS-OEMCONVERT, $CBS-SORT, $CBS-HASSTRINGS, $CBS-NOINTEGRALHEIGHT,
	$CBS-DISABLENOSCROLL, $CBS-UPPERCASE, $CBS-LOWERCASE;
  export $CB-GETEDITSEL, $CB-LIMITTEXT, $CB-SETEDITSEL, $CB-ADDSTRING,
	$CB-DELETESTRING, $CB-DIR, $CB-GETCOUNT, $CB-GETCURSEL,
	$CB-GETLBTEXT, $CB-GETLBTEXTLEN, $CB-INSERTSTRING, $CB-RESETCONTENT,
	$CB-FINDSTRING, $CB-SELECTSTRING, $CB-SETCURSEL, $CB-SHOWDROPDOWN,
	$CB-GETITEMDATA, $CB-SETITEMDATA, $CB-GETDROPPEDCONTROLRECT,
	$CB-SETITEMHEIGHT, $CB-GETITEMHEIGHT, $CB-SETEXTENDEDUI,
	$CB-GETEXTENDEDUI, $CB-GETDROPPEDSTATE, $CB-FINDSTRINGEXACT,
	$CB-SETLOCALE, $CB-GETLOCALE, $CB-GETTOPINDEX, $CB-SETTOPINDEX,
	$CB-GETHORIZONTALEXTENT, $CB-SETHORIZONTALEXTENT,
	$CB-GETDROPPEDWIDTH, $CB-SETDROPPEDWIDTH, $CB-INITSTORAGE,
	$CB-MSGMAX;
  export $SBS-HORZ, $SBS-VERT, $SBS-TOPALIGN, $SBS-LEFTALIGN,
	$SBS-BOTTOMALIGN, $SBS-RIGHTALIGN, $SBS-SIZEBOXTOPLEFTALIGN,
	$SBS-SIZEBOXBOTTOMRIGHTALIGN, $SBS-SIZEBOX, $SBS-SIZEGRIP;
  export $SBM-SETPOS, $SBM-GETPOS, $SBM-SETRANGE, $SBM-SETRANGEREDRAW,
	$SBM-GETRANGE, $SBM-ENABLE-ARROWS, $SBM-SETSCROLLINFO,
	$SBM-GETSCROLLINFO, $SIF-RANGE, $SIF-PAGE, $SIF-POS,
	$SIF-DISABLENOSCROLL, $SIF-TRACKPOS, $SIF-ALL, fMask-value,
	fMask-value-setter, nMin-value, nMin-value-setter, nMax-value,
	nMax-value-setter, nPage-value, nPage-value-setter, nPos-value,
	nPos-value-setter, nTrackPos-value, nTrackPos-value-setter,
	<SCROLLINFO>, <LPSCROLLINFO>, <LPCSCROLLINFO>, SetScrollInfo,
	GetScrollInfo;
  export $MDIS-ALLCHILDSTYLES;
  export $MDITILE-VERTICAL, $MDITILE-HORIZONTAL,
	$MDITILE-SKIPDISABLED, szClass-value, szClass-value-setter,
	szTitle-value, szTitle-value-setter, hOwner-value,
	hOwner-value-setter, style-value, style-value-setter, lParam-value,
	lParam-value-setter, <MDICREATESTRUCTA>, <LPMDICREATESTRUCTA>,
	<MDICREATESTRUCT>, <LPMDICREATESTRUCT>, hWindowMenu-value,
	hWindowMenu-value-setter, idFirstChild-value,
	idFirstChild-value-setter, <CLIENTCREATESTRUCT>,
	<LPCLIENTCREATESTRUCT>, DefFrameProc, DefMDIChildProc,
	TranslateMDISysAccel, ArrangeIconicWindows, CreateMDIWindow,
	TileWindows, CascadeWindows, <HELPPOLY>, mkSize-value,
	mkSize-value-setter, mkKeylist-value, mkKeylist-value-setter,
	szKeyphrase-array, szKeyphrase-array-setter, szKeyphrase-value,
	<MULTIKEYHELPA>, <LPMULTIKEYHELPA>, <PMULTIKEYHELPA>, <MULTIKEYHELP>,
	<PMULTIKEYHELP>, <LPMULTIKEYHELP>, wStructSize-value,
	wStructSize-value-setter, dx-value, dx-value-setter, dy-value,
	dy-value-setter, wMax-value, wMax-value-setter, rgchMember-array,
	rgchMember-array-setter, rgchMember-value, <HELPWININFOA>,
	<LPHELPWININFOA>, <PHELPWININFOA>, <HELPWININFO>, <PHELPWININFO>,
	<LPHELPWININFO>;
  export $HELP-CONTEXT, $HELP-QUIT, $HELP-INDEX, $HELP-CONTENTS,
	$HELP-HELPONHELP, $HELP-SETINDEX, $HELP-SETCONTENTS,
	$HELP-CONTEXTPOPUP, $HELP-FORCEFILE, $HELP-KEY, $HELP-COMMAND,
	$HELP-PARTIALKEY, $HELP-MULTIKEY, $HELP-SETWINPOS, $HELP-CONTEXTMENU,
	$HELP-FINDER, $HELP-WM-HELP, $HELP-SETPOPUP-POS, $HELP-TCARD,
	$HELP-TCARD-DATA, $HELP-TCARD-OTHER-CALLER, $IDH-NO-HELP,
	$IDH-MISSING-CONTEXT, $IDH-GENERIC-HELP-BUTTON, $IDH-OK, $IDH-CANCEL,
	$IDH-HELP, WinHelp;
  export $SPI-GETBEEP, $SPI-SETBEEP, $SPI-GETMOUSE, $SPI-SETMOUSE,
	$SPI-GETBORDER, $SPI-SETBORDER, $SPI-GETKEYBOARDSPEED,
	$SPI-SETKEYBOARDSPEED, $SPI-LANGDRIVER, $SPI-ICONHORIZONTALSPACING,
	$SPI-GETSCREENSAVETIMEOUT, $SPI-SETSCREENSAVETIMEOUT,
	$SPI-GETSCREENSAVEACTIVE, $SPI-SETSCREENSAVEACTIVE,
	$SPI-GETGRIDGRANULARITY, $SPI-SETGRIDGRANULARITY,
	$SPI-SETDESKWALLPAPER, $SPI-SETDESKPATTERN, $SPI-GETKEYBOARDDELAY,
	$SPI-SETKEYBOARDDELAY, $SPI-ICONVERTICALSPACING,
	$SPI-GETICONTITLEWRAP, $SPI-SETICONTITLEWRAP,
	$SPI-GETMENUDROPALIGNMENT, $SPI-SETMENUDROPALIGNMENT,
	$SPI-SETDOUBLECLKWIDTH, $SPI-SETDOUBLECLKHEIGHT,
	$SPI-GETICONTITLELOGFONT, $SPI-SETDOUBLECLICKTIME,
	$SPI-SETMOUSEBUTTONSWAP, $SPI-SETICONTITLELOGFONT,
	$SPI-GETFASTTASKSWITCH, $SPI-SETFASTTASKSWITCH,
	$SPI-SETDRAGFULLWINDOWS, $SPI-GETDRAGFULLWINDOWS,
	$SPI-GETNONCLIENTMETRICS, $SPI-SETNONCLIENTMETRICS,
	$SPI-GETMINIMIZEDMETRICS, $SPI-SETMINIMIZEDMETRICS,
	$SPI-GETICONMETRICS, $SPI-SETICONMETRICS, $SPI-SETWORKAREA,
	$SPI-GETWORKAREA, $SPI-SETPENWINDOWS, $SPI-GETHIGHCONTRAST,
	$SPI-SETHIGHCONTRAST, $SPI-GETKEYBOARDPREF, $SPI-SETKEYBOARDPREF,
	$SPI-GETSCREENREADER, $SPI-SETSCREENREADER, $SPI-GETANIMATION,
	$SPI-SETANIMATION, $SPI-GETFONTSMOOTHING, $SPI-SETFONTSMOOTHING,
	$SPI-SETDRAGWIDTH, $SPI-SETDRAGHEIGHT, $SPI-SETHANDHELD,
	$SPI-GETLOWPOWERTIMEOUT, $SPI-GETPOWEROFFTIMEOUT,
	$SPI-SETLOWPOWERTIMEOUT, $SPI-SETPOWEROFFTIMEOUT,
	$SPI-GETLOWPOWERACTIVE, $SPI-GETPOWEROFFACTIVE,
	$SPI-SETLOWPOWERACTIVE, $SPI-SETPOWEROFFACTIVE, $SPI-SETCURSORS,
	$SPI-SETICONS, $SPI-GETDEFAULTINPUTLANG, $SPI-SETDEFAULTINPUTLANG,
	$SPI-SETLANGTOGGLE, $SPI-GETWINDOWSEXTENSION, $SPI-SETMOUSETRAILS,
	$SPI-GETMOUSETRAILS, $SPI-SETSCREENSAVERRUNNING,
	$SPI-SCREENSAVERRUNNING, $SPI-GETFILTERKEYS, $SPI-SETFILTERKEYS,
	$SPI-GETTOGGLEKEYS, $SPI-SETTOGGLEKEYS, $SPI-GETMOUSEKEYS,
	$SPI-SETMOUSEKEYS, $SPI-GETSHOWSOUNDS, $SPI-SETSHOWSOUNDS,
	$SPI-GETSTICKYKEYS, $SPI-SETSTICKYKEYS, $SPI-GETACCESSTIMEOUT,
	$SPI-SETACCESSTIMEOUT, $SPI-GETSERIALKEYS, $SPI-SETSERIALKEYS,
	$SPI-GETSOUNDSENTRY, $SPI-SETSOUNDSENTRY;
  export $SPIF-UPDATEINIFILE, $SPIF-SENDWININICHANGE,
	$SPIF-SENDCHANGE, $METRICS-USEDEFAULT, iBorderWidth-value,
	iBorderWidth-value-setter, iScrollWidth-value,
	iScrollWidth-value-setter, iScrollHeight-value,
	iScrollHeight-value-setter, iCaptionWidth-value,
	iCaptionWidth-value-setter, iCaptionHeight-value,
	iCaptionHeight-value-setter, lfCaptionFont-value,
	lfCaptionFont-value-setter, iSmCaptionWidth-value,
	iSmCaptionWidth-value-setter, iSmCaptionHeight-value,
	iSmCaptionHeight-value-setter, lfSmCaptionFont-value,
	lfSmCaptionFont-value-setter, iMenuWidth-value,
	iMenuWidth-value-setter, iMenuHeight-value, iMenuHeight-value-setter,
	lfMenuFont-value, lfMenuFont-value-setter, lfStatusFont-value,
	lfStatusFont-value-setter, lfMessageFont-value,
	lfMessageFont-value-setter, <NONCLIENTMETRICSA>,
	<LPNONCLIENTMETRICSA>, <PNONCLIENTMETRICSA>, <NONCLIENTMETRICS>,
	<PNONCLIENTMETRICS>, <LPNONCLIENTMETRICS>, $ARW-BOTTOMLEFT,
	$ARW-BOTTOMRIGHT, $ARW-TOPLEFT, $ARW-TOPRIGHT, $ARW-STARTMASK,
	$ARW-STARTRIGHT, $ARW-STARTTOP, $ARW-LEFT, $ARW-RIGHT, $ARW-UP,
	$ARW-DOWN, $ARW-HIDE, iWidth-value, iWidth-value-setter,
	iHorzGap-value, iHorzGap-value-setter, iVertGap-value,
	iVertGap-value-setter, iArrange-value, iArrange-value-setter,
	<MINIMIZEDMETRICS>, <LPMINIMIZEDMETRICS>, <PMINIMIZEDMETRICS>,
	iHorzSpacing-value, iHorzSpacing-value-setter, iVertSpacing-value,
	iVertSpacing-value-setter, iTitleWrap-value, iTitleWrap-value-setter,
	lfFont-value, lfFont-value-setter, <ICONMETRICSA>, <LPICONMETRICSA>,
	<PICONMETRICSA>, <ICONMETRICS>, <PICONMETRICS>, <LPICONMETRICS>,
	iMinAnimate-value, iMinAnimate-value-setter, <ANIMATIONINFO>,
	<LPANIMATIONINFO>;
  export lpszActivePort-value, lpszActivePort-value-setter,
	lpszPort-value, lpszPort-value-setter, iBaudRate-value,
	iBaudRate-value-setter, iPortState-value, iPortState-value-setter,
	iActive-value, iActive-value-setter, <SERIALKEYSA>, <LPSERIALKEYSA>,
	<SERIALKEYS>, <LPSERIALKEYS>, $SERKF-SERIALKEYSON, $SERKF-AVAILABLE,
	$SERKF-INDICATOR;
  export lpszDefaultScheme-value, lpszDefaultScheme-value-setter,
	<HIGHCONTRASTA>, <LPHIGHCONTRASTA>, <HIGHCONTRAST>, <LPHIGHCONTRAST>,
	$HCF-HIGHCONTRASTON, $HCF-AVAILABLE, $HCF-HOTKEYACTIVE,
	$HCF-CONFIRMHOTKEY, $HCF-HOTKEYSOUND, $HCF-INDICATOR,
	$HCF-HOTKEYAVAILABLE, $CDS-UPDATEREGISTRY, $CDS-TEST,
	$CDS-FULLSCREEN, $CDS-GLOBAL, $CDS-SET-PRIMARY, $CDS-RESET,
	$CDS-SETRECT, $CDS-NORESET, $DISP-CHANGE-SUCCESSFUL,
	$DISP-CHANGE-RESTART, $DISP-CHANGE-FAILED, $DISP-CHANGE-BADMODE,
	$DISP-CHANGE-NOTUPDATED, $DISP-CHANGE-BADFLAGS,
	$DISP-CHANGE-BADPARAM, ChangeDisplaySettings, $ENUM-CURRENT-SETTINGS,
	$ENUM-REGISTRY-SETTINGS, EnumDisplaySettings, SystemParametersInfo;
  export iWaitMSec-value, iWaitMSec-value-setter, iDelayMSec-value,
	iDelayMSec-value-setter, iRepeatMSec-value, iRepeatMSec-value-setter,
	iBounceMSec-value, iBounceMSec-value-setter, <FILTERKEYS>,
	<LPFILTERKEYS>;
  export $FKF-FILTERKEYSON, $FKF-AVAILABLE, $FKF-HOTKEYACTIVE,
	$FKF-CONFIRMHOTKEY, $FKF-HOTKEYSOUND, $FKF-INDICATOR, $FKF-CLICKON,
	<STICKYKEYS>, <LPSTICKYKEYS>;
  export $SKF-STICKYKEYSON, $SKF-AVAILABLE, $SKF-HOTKEYACTIVE,
	$SKF-CONFIRMHOTKEY, $SKF-HOTKEYSOUND, $SKF-INDICATOR,
	$SKF-AUDIBLEFEEDBACK, $SKF-TRISTATE, $SKF-TWOKEYSOFF;
  export iMaxSpeed-value, iMaxSpeed-value-setter,
	iTimeToMaxSpeed-value, iTimeToMaxSpeed-value-setter,
	iCtrlSpeed-value, iCtrlSpeed-value-setter, <MOUSEKEYS>,
	<LPMOUSEKEYS>;
  export $MKF-MOUSEKEYSON, $MKF-AVAILABLE, $MKF-HOTKEYACTIVE,
	$MKF-CONFIRMHOTKEY, $MKF-HOTKEYSOUND, $MKF-INDICATOR, $MKF-MODIFIERS,
	$MKF-REPLACENUMBERS;
  export iTimeOutMSec-value, iTimeOutMSec-value-setter,
	<ACCESSTIMEOUT>, <LPACCESSTIMEOUT>;
  export $ATF-TIMEOUTON, $ATF-ONOFFFEEDBACK, $SSGF-NONE,
	$SSGF-DISPLAY, $SSTF-NONE, $SSTF-CHARS, $SSTF-BORDER, $SSTF-DISPLAY,
	$SSWF-NONE, $SSWF-TITLE, $SSWF-WINDOW, $SSWF-DISPLAY, $SSWF-CUSTOM,
	iFSTextEffect-value, iFSTextEffect-value-setter,
	iFSTextEffectMSec-value, iFSTextEffectMSec-value-setter,
	iFSTextEffectColorBits-value, iFSTextEffectColorBits-value-setter,
	iFSGrafEffect-value, iFSGrafEffect-value-setter,
	iFSGrafEffectMSec-value, iFSGrafEffectMSec-value-setter,
	iFSGrafEffectColor-value, iFSGrafEffectColor-value-setter,
	iWindowsEffect-value, iWindowsEffect-value-setter,
	iWindowsEffectMSec-value, iWindowsEffectMSec-value-setter,
	lpszWindowsEffectDLL-value, lpszWindowsEffectDLL-value-setter,
	iWindowsEffectOrdinal-value, iWindowsEffectOrdinal-value-setter,
	<SOUNDSENTRYA>, <LPSOUNDSENTRYA>, <SOUNDSENTRY>, <LPSOUNDSENTRY>;
  export $SSF-SOUNDSENTRYON, $SSF-AVAILABLE, $SSF-INDICATOR,
	<TOGGLEKEYS>, <LPTOGGLEKEYS>;
  export $TKF-TOGGLEKEYSON, $TKF-AVAILABLE, $TKF-HOTKEYACTIVE,
	$TKF-CONFIRMHOTKEY, $TKF-HOTKEYSOUND, $TKF-INDICATOR;
  export $SLE-ERROR, $SLE-MINORERROR, $SLE-WARNING, SetLastErrorEx;

  // defined in "moreuser.dylan":
  export CreateWindow, CreateDialog, CreateDialogIndirect, DialogBox,
	GetNextWindow, CopyCursor;
  export MAKEWPARAM, MAKELPARAM, MAKELRESULT;
  export CreateWindowEx;

  // defined in "predecl.dylan":
  export hdr-value, hdr-value-setter, pszTitle-value, pszTitle-value-setter;
  export lpszCaption-value, length-value, hWndOwner-value, wFmt-value,
    dwData-value, lpszText-value;
  export lpszCaption-value-setter, length-value-setter,
    hWndOwner-value-setter, wFmt-value-setter,
    dwData-value-setter, lpszText-value-setter;
  export <LPHKL>, <LPHWND>;

end module Win32-user;



define module Win32-default-handler
  use functional-dylan;
  use Win32-common;
  use Win32-user;
  use Win32-kernel;
  use simple-format;
  export
    *error-module-handle*,
    win32-last-handler;
end module Win32-default-handler;
