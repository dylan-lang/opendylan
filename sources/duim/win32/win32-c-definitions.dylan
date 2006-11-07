Module:    Win32-duim
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define C-struct <RECT>
  sealed inline-only slot left-value     :: <LONG>;
  sealed inline-only slot top-value      :: <LONG>;
  sealed inline-only slot right-value    :: <LONG>;
  sealed inline-only slot bottom-value   :: <LONG>;
  pointer-type-name: <LPRECT>;
  c-name: "struct tagRECT";
end C-struct <RECT>;
define sealed domain make (singleton(<LPRECT>));
define sealed domain initialize (<LPRECT>);

define C-struct w/<POINT>
  sealed inline-only slot x-value        :: <LONG>;
  sealed inline-only slot y-value        :: <LONG>;
  pointer-type-name: <LPPOINT>;
  c-name: "struct tagPOINT";
end C-struct w/<POINT>;
define sealed domain make (singleton(<LPPOINT>));
define sealed domain initialize (<LPPOINT>);

define C-struct <POINTL>
  sealed inline-only slot x-value        :: <LONG>;
  sealed inline-only slot y-value        :: <LONG>;
  pointer-type-name: <LPPOINTL>;
  c-name: "struct _POINTL";
end C-struct <POINTL>;
define sealed domain make (singleton(<LPPOINTL>));
define sealed domain initialize (<LPPOINTL>);

define C-struct <SIZE>
  sealed inline-only slot cx-value       :: <LONG>;
  sealed inline-only slot cy-value       :: <LONG>;
  pointer-type-name: <LPSIZE>;
  c-name: "struct tagSIZE";
end C-struct <SIZE>;
define sealed domain make (singleton(<LPSIZE>));
define sealed domain initialize (<LPSIZE>);

define C-struct <POINTS>
  sealed inline-only slot x-value        :: <SHORT>;
  sealed inline-only slot y-value        :: <SHORT>;
  pointer-type-name: <LPPOINTS>;
  c-name: "struct tagPOINTS";
end C-struct <POINTS>;
define sealed domain make (singleton(<LPPOINTS>));
define sealed domain initialize (<LPPOINTS>);

define C-struct <LOGFONTA>
  sealed inline-only slot lfHeight-value :: <LONG>;
  sealed inline-only slot lfWidth-value  :: <LONG>;
  sealed inline-only slot lfEscapement-value :: <LONG>;
  sealed inline-only slot lfOrientation-value :: <LONG>;
  sealed inline-only slot lfWeight-value :: <LONG>;
  sealed inline-only slot lfItalic-value :: <C-BYTE>;
  sealed inline-only slot lfUnderline-value :: <C-BYTE>;
  sealed inline-only slot lfStrikeOut-value :: <C-BYTE>;
  sealed inline-only slot lfCharSet-value :: <C-BYTE>;
  sealed inline-only slot lfOutPrecision-value :: <C-BYTE>;
  sealed inline-only slot lfClipPrecision-value :: <C-BYTE>;
  sealed inline-only slot lfQuality-value :: <C-BYTE>;
  sealed inline-only slot lfPitchAndFamily-value :: <C-BYTE>;
  sealed inline-only array slot lfFaceName-array :: <CHAR>,
        length: $LF-FACESIZE, address-getter: lfFaceName-value;
  pointer-type-name: <LPLOGFONT>;
  c-name: "struct tagLOGFONTA";
end C-struct <LOGFONTA>;
define sealed domain make (singleton(<LPLOGFONT>));
define sealed domain initialize (<LPLOGFONT>);

define C-struct <DEVMODEA>
  sealed inline-only array slot dmDeviceName-array :: <C-BYTE>,
        length: $CCHDEVICENAME, address-getter: dmDeviceName-value;
  sealed inline-only slot dmSpecVersion-value :: <WORD>;
  sealed inline-only slot dmDriverVersion-value :: <WORD>;
  sealed inline-only slot dmSize-value   :: <WORD>;
  sealed inline-only slot dmDriverExtra-value :: <WORD>;
  sealed inline-only slot dmFields-value :: <DWORD>;
  sealed inline-only slot u-value        :: <u%2>;
  sealed inline-only slot dmScale-value  :: <C-short>;
  sealed inline-only slot dmCopies-value :: <C-short>;
  sealed inline-only slot dmDefaultSource-value :: <C-short>;
  sealed inline-only slot dmPrintQuality-value :: <C-short>;
  sealed inline-only slot dmColor-value  :: <C-short>;
  sealed inline-only slot dmDuplex-value :: <C-short>;
  sealed inline-only slot dmYResolution-value :: <C-short>;
  sealed inline-only slot dmTTOption-value :: <C-short>;
  sealed inline-only slot dmCollate-value :: <C-short>;
  sealed inline-only array slot dmFormName-array :: <C-BYTE>,
        length: $CCHFORMNAME, address-getter: dmFormName-value;
  sealed inline-only slot dmLogPixels-value :: <WORD>;
  sealed inline-only slot dmBitsPerPel-value :: <DWORD>;
  sealed inline-only slot dmPelsWidth-value :: <DWORD>;
  sealed inline-only slot dmPelsHeight-value :: <DWORD>;
  sealed inline-only slot dmDisplayFlags-value :: <DWORD>;
  sealed inline-only slot dmDisplayFrequency-value :: <DWORD>;
  sealed inline-only slot dmICMMethod-value :: <DWORD>;
  sealed inline-only slot dmICMIntent-value :: <DWORD>;
  sealed inline-only slot dmMediaType-value :: <DWORD>;
  sealed inline-only slot dmDitherType-value :: <DWORD>;
  sealed inline-only slot dmReserved1    :: <DWORD>;
  sealed inline-only slot dmReserved2    :: <DWORD>;
  pointer-type-name: <LPDEVMODE>;
  c-name: "struct _devicemodeA";
end C-struct <DEVMODEA>;
define sealed domain make (singleton(<LPDEVMODE>));
define sealed domain initialize (<LPDEVMODE>);

define C-struct <BITMAP>
  sealed inline-only slot bmType-value   :: <LONG>;
  sealed inline-only slot bmWidth-value  :: <LONG>;
  sealed inline-only slot bmHeight-value :: <LONG>;
  sealed inline-only slot bmWidthBytes-value :: <LONG>;
  sealed inline-only slot bmPlanes-value :: <WORD>;
  sealed inline-only slot bmBitsPixel-value :: <WORD>;
  sealed inline-only slot bmBits-value   :: <LPVOID>;
  pointer-type-name: <LPBITMAP>;
  c-name: "struct tagBITMAP";
end C-struct <BITMAP>;
define sealed domain make (singleton(<LPBITMAP>));
define sealed domain initialize (<LPBITMAP>);

define C-struct <TEXTMETRICA>
  sealed inline-only slot tmHeight-value :: <LONG>;
  sealed inline-only slot tmAscent-value :: <LONG>;
  sealed inline-only slot tmDescent-value :: <LONG>;
  sealed inline-only slot tmInternalLeading-value :: <LONG>;
  sealed inline-only slot tmExternalLeading-value :: <LONG>;
  sealed inline-only slot tmAveCharWidth-value :: <LONG>;
  sealed inline-only slot tmMaxCharWidth-value :: <LONG>;
  sealed inline-only slot tmWeight-value :: <LONG>;
  sealed inline-only slot tmOverhang-value :: <LONG>;
  sealed inline-only slot tmDigitizedAspectX-value :: <LONG>;
  sealed inline-only slot tmDigitizedAspectY-value :: <LONG>;
  sealed inline-only slot tmFirstChar-value :: <C-BYTE>;
  sealed inline-only slot tmLastChar-value :: <C-BYTE>;
  sealed inline-only slot tmDefaultChar-value :: <C-BYTE>;
  sealed inline-only slot tmBreakChar-value :: <C-BYTE>;
  sealed inline-only slot tmItalic-value :: <C-BYTE>;
  sealed inline-only slot tmUnderlined-value :: <C-BYTE>;
  sealed inline-only slot tmStruckOut-value :: <C-BYTE>;
  sealed inline-only slot tmPitchAndFamily-value :: <C-BYTE>;
  sealed inline-only slot tmCharSet-value :: <C-BYTE>;
  pointer-type-name: <LPTEXTMETRIC>;
  c-name: "struct tagTEXTMETRICA";
end C-struct <TEXTMETRICA>;
define sealed domain make (singleton(<LPTEXTMETRIC>));
define sealed domain initialize (<LPTEXTMETRIC>);

define C-struct <LOGBRUSH>
  sealed inline-only slot lbStyle-value  :: <UINT>;
  sealed inline-only slot lbColor-value  :: <COLORREF>;
  sealed inline-only slot lbHatch-value  :: <LONG>;
  pointer-type-name: <LPLOGBRUSH>;
  c-name: "struct tagLOGBRUSH";
end C-struct <LOGBRUSH>;
define sealed domain make (singleton(<LPLOGBRUSH>));
define sealed domain initialize (<LPLOGBRUSH>);

define C-struct <s%1>
  sealed inline-only slot dmOrientation-value :: <C-short>;
  sealed inline-only slot dmPaperSize-value :: <C-short>;
  sealed inline-only slot dmPaperLength-value :: <C-short>;
  sealed inline-only slot dmPaperWidth-value :: <C-short>;
end;

define C-union <u%2>
  sealed inline-only slot u-value        :: <s%1>;
  sealed inline-only slot dmPosition-value :: <POINTL>;
end;

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


define C-struct <SYSTEMTIME>
  sealed inline-only slot wYear-value    :: <WORD>;
  sealed inline-only slot wMonth-value   :: <WORD>;
  sealed inline-only slot wDayOfWeek-value :: <WORD>;
  sealed inline-only slot wDay-value     :: <WORD>;
  sealed inline-only slot wHour-value    :: <WORD>;
  sealed inline-only slot wMinute-value  :: <WORD>;
  sealed inline-only slot wSecond-value  :: <WORD>;
  sealed inline-only slot wMilliseconds-value :: <WORD>;
  pointer-type-name: <LPSYSTEMTIME>;
  c-name: "struct _SYSTEMTIME";
end C-struct <SYSTEMTIME>;
define sealed domain make (singleton(<LPSYSTEMTIME>));
define sealed domain initialize (<LPSYSTEMTIME>);

define C-struct <STARTUPINFOA>
  sealed inline-only slot cb-value       :: <DWORD>;
  sealed inline-only slot lpReserved     :: <LPSTR>;
  sealed inline-only slot lpDesktop-value :: <LPSTR>;
  sealed inline-only slot lpTitle-value  :: <LPSTR>;
  sealed inline-only slot dwX-value      :: <DWORD>;
  sealed inline-only slot dwY-value      :: <DWORD>;
  sealed inline-only slot dwXSize-value  :: <DWORD>;
  sealed inline-only slot dwYSize-value  :: <DWORD>;
  sealed inline-only slot dwXCountChars-value :: <DWORD>;
  sealed inline-only slot dwYCountChars-value :: <DWORD>;
  sealed inline-only slot dwFillAttribute-value :: <DWORD>;
  sealed inline-only slot dwFlags-value  :: <DWORD>;
  sealed inline-only slot wShowWindow-value :: <WORD>;
  sealed inline-only slot cbReserved2    :: <WORD>;
  sealed inline-only slot lpReserved2    :: <LPBYTE>;
  sealed inline-only slot hStdInput-value :: <HANDLE>;
  sealed inline-only slot hStdOutput-value :: <HANDLE>;
  sealed inline-only slot hStdError-value :: <HANDLE>;
  pointer-type-name: <LPSTARTUPINFO>;
  c-name: "struct _STARTUPINFOA";
end C-struct <STARTUPINFOA>;
define sealed domain make (singleton(<LPSTARTUPINFO>));
define sealed domain initialize (<LPSTARTUPINFO>);

define C-struct <OSVERSIONINFO>
  sealed inline-only slot dwOSVersionInfoSize-value :: <DWORD>;
  sealed inline-only slot dwMajorVersion-value :: <DWORD>;
  sealed inline-only slot dwMinorVersion-value :: <DWORD>;
  sealed inline-only slot dwBuildNumber-value :: <DWORD>;
  sealed inline-only slot dwPlatformId-value :: <DWORD>;
  sealed inline-only array slot szCSDVersion-array :: <CHAR>,
        length: 128, address-getter: szCSDVersion-value;
  pointer-type-name: <LPOSVERSIONINFO>;
  c-name: "struct _OSVERSIONINFOA";
end C-struct <OSVERSIONINFO>;
define sealed domain make (singleton(<LPOSVERSIONINFO>));
define sealed domain initialize (<LPOSVERSIONINFO>);

define C-struct <MINMAXINFO>
  sealed inline-only slot ptReserved     :: w/<POINT>;
  sealed inline-only slot ptMaxSize-value :: w/<POINT>;
  sealed inline-only slot ptMaxPosition-value :: w/<POINT>;
  sealed inline-only slot ptMinTrackSize-value :: w/<POINT>;
  sealed inline-only slot ptMaxTrackSize-value :: w/<POINT>;
  pointer-type-name: <LPMINMAXINFO>;
  c-name: "struct tagMINMAXINFO";
end C-struct <MINMAXINFO>;
define sealed domain make (singleton(<LPMINMAXINFO>));
define sealed domain initialize (<LPMINMAXINFO>);

define C-struct <ACCEL>
  sealed inline-only slot fVirt-value    :: <C-BYTE>;
  sealed inline-only slot key-value      :: <WORD>;
  sealed inline-only slot cmd-value      :: <WORD>;
  pointer-type-name: <LPACCEL>;
  c-name: "struct tagACCEL";
end C-struct <ACCEL>;
define sealed domain make (singleton(<LPACCEL>));
define sealed domain initialize (<LPACCEL>);

define C-struct <PAINTSTRUCT>
  sealed inline-only slot hdc-value      :: <HDC>;
  sealed inline-only slot fErase-value   :: <BOOL>;
  sealed inline-only slot rcPaint-value  :: <RECT>;
  sealed inline-only slot fRestore-value :: <BOOL>;
  sealed inline-only slot fIncUpdate-value :: <BOOL>;
  sealed inline-only array slot rgbReserved-array :: <C-BYTE>,
        length: 32, address-getter: rgbReserved-value;
  pointer-type-name: <LPPAINTSTRUCT>;
  c-name: "struct tagPAINTSTRUCT";
end C-struct <PAINTSTRUCT>;
define sealed domain make (singleton(<LPPAINTSTRUCT>));
define sealed domain initialize (<LPPAINTSTRUCT>);

define C-struct <NMHDR>
  sealed inline-only slot hwndFrom-value :: <HWND>;
  sealed inline-only slot idFrom-value   :: <UINT>;
  sealed inline-only slot code-value     :: <UINT>;
  pointer-type-name: <LPNMHDR>;
  c-name: "struct tagNMHDR";
end C-struct <NMHDR>;
define sealed domain make (singleton(<LPNMHDR>));
define sealed domain initialize (<LPNMHDR>);

define C-struct <DLGTEMPLATE>
  sealed inline-only slot style-value    :: <DWORD>;
  sealed inline-only slot dwExtendedStyle-value :: <DWORD>;
  sealed inline-only slot cdit-value     :: <WORD>;
  sealed inline-only slot x-value        :: <C-short>;
  sealed inline-only slot y-value        :: <C-short>;
  sealed inline-only slot cx-value       :: <C-short>;
  sealed inline-only slot cy-value       :: <C-short>;
  pack: 2;
  pointer-type-name: <LPDLGTEMPLATE>;
end C-struct <DLGTEMPLATE>;
define sealed domain make (singleton(<LPDLGTEMPLATE>));
define sealed domain initialize (<LPDLGTEMPLATE>);

define C-struct <DLGITEMTEMPLATE>
  sealed inline-only slot style-value    :: <DWORD>;
  sealed inline-only slot dwExtendedStyle-value :: <DWORD>;
  sealed inline-only slot x-value        :: <C-short>;
  sealed inline-only slot y-value        :: <C-short>;
  sealed inline-only slot cx-value       :: <C-short>;
  sealed inline-only slot cy-value       :: <C-short>;
  sealed inline-only slot id-value       :: <WORD>;
  pack: 2;
  pointer-type-name: <LPDLGITEMTEMPLATE>;
end C-struct <DLGITEMTEMPLATE>;
define sealed domain make (singleton(<LPDLGITEMTEMPLATE>));
define sealed domain initialize (<LPDLGITEMTEMPLATE>);

define C-struct <MENUITEMINFO>
  sealed inline-only slot cbSize-value   :: <UINT>;
  sealed inline-only slot fMask-value    :: <UINT>;
  sealed inline-only slot fType-value    :: <UINT>;
  sealed inline-only slot fState-value   :: <UINT>;
  sealed inline-only slot wID-value      :: <UINT>;
  sealed inline-only slot hSubMenu-value :: <HMENU>;
  sealed inline-only slot hbmpChecked-value :: <HBITMAP>;
  sealed inline-only slot hbmpUnchecked-value :: <HBITMAP>;
  sealed inline-only slot dwItemData-value :: <DWORD>;
  sealed inline-only slot dwTypeData-value :: <LPSTR>;
  sealed inline-only slot cch-value      :: <UINT>;
  pointer-type-name: <LPMENUITEMINFO>;
  c-name: "struct tagMENUITEMINFOA";
end C-struct <MENUITEMINFO>;
define sealed domain make (singleton(<LPMENUITEMINFO>));
define sealed domain initialize (<LPMENUITEMINFO>);

define C-struct <ICONINFO>
  sealed inline-only slot fIcon-value    :: <BOOL>;
  sealed inline-only slot xHotspot-value :: <DWORD>;
  sealed inline-only slot yHotspot-value :: <DWORD>;
  sealed inline-only slot hbmMask-value  :: <HBITMAP>;
  sealed inline-only slot hbmColor-value :: <HBITMAP>;
  pointer-type-name: <LPICONINFO>;
  c-name: "struct _ICONINFO";
end C-struct <ICONINFO>;
define sealed domain make (singleton(<LPICONINFO>));
define sealed domain initialize (<LPICONINFO>);

define C-struct <SCROLLINFO>
  sealed inline-only slot cbSize-value   :: <UINT>;
  sealed inline-only slot fMask-value    :: <UINT>;
  sealed inline-only slot nMin-value     :: <C-int>;
  sealed inline-only slot nMax-value     :: <C-int>;
  sealed inline-only slot nPage-value    :: <UINT>;
  sealed inline-only slot nPos-value     :: <C-int>;
  sealed inline-only slot nTrackPos-value :: <C-int>;
  pointer-type-name: <LPSCROLLINFO>;
  c-name: "struct tagSCROLLINFO";
end C-struct <SCROLLINFO>;
define sealed domain make (singleton(<LPSCROLLINFO>));
define sealed domain initialize (<LPSCROLLINFO>);
define inline constant <LPCSCROLLINFO> = /* const */ <LPSCROLLINFO>;

define C-struct <WNDCLASSA>
  sealed inline-only slot style-value    :: <UINT>;
  sealed inline-only slot lpfnWndProc-value :: <C-function-pointer>;
  sealed inline-only slot cbClsExtra-value :: <C-int>;
  sealed inline-only slot cbWndExtra-value :: <C-int>;
  sealed inline-only slot hInstance-value :: <HINSTANCE>;
  sealed inline-only slot hIcon-value    :: <HICON>;
  sealed inline-only slot hCursor-value  :: <HCURSOR>;
  sealed inline-only slot hbrBackground-value :: <HBRUSH>;
  sealed inline-only slot lpszMenuName-value :: <LPCSTR>;
  sealed inline-only slot lpszClassName-value :: <LPCSTR>;
  pointer-type-name: <PWNDCLASS>;
  c-name: "struct tagWNDCLASSA";
end C-struct <WNDCLASSA>;
define sealed domain make (singleton(<LPWNDCLASSA>));
define sealed domain initialize (<LPWNDCLASSA>);
define inline constant <LPWNDCLASSA>  = <PWNDCLASS>;

define C-struct <MSG>
  sealed inline-only slot hwnd-value     :: <HWND>;
  sealed inline-only slot message-value  :: <UINT>;
  sealed inline-only slot wParam-value   :: <WPARAM>;
  sealed inline-only slot lParam-value   :: <LPARAM>;
  sealed inline-only slot time-value     :: <DWORD>;
  sealed inline-only slot pt-value       :: w/<POINT>;
  pointer-type-name: <LPMSG>;
  c-name: "struct tagMSG";
end C-struct <MSG>;
define sealed domain make (singleton(<LPMSG>));
define sealed domain initialize (<LPMSG>);

define C-struct <OPENFILENAME>
  sealed inline-only slot lStructSize-value :: <DWORD>;
  sealed inline-only slot hwndOwner-value :: <HWND>;
  sealed inline-only slot hInstance-value :: <HINSTANCE>;
  sealed inline-only slot lpstrFilter-value :: <LPCSTR>;
  sealed inline-only slot lpstrCustomFilter-value :: <LPSTR>;
  sealed inline-only slot nMaxCustFilter-value :: <DWORD>;
  sealed inline-only slot nFilterIndex-value :: <DWORD>;
  sealed inline-only slot lpstrFile-value :: <LPSTR>;
  sealed inline-only slot nMaxFile-value :: <DWORD>;
  sealed inline-only slot lpstrFileTitle-value :: <LPSTR>;
  sealed inline-only slot nMaxFileTitle-value :: <DWORD>;
  sealed inline-only slot lpstrInitialDir-value :: <LPCSTR>;
  sealed inline-only slot lpstrTitle-value :: <LPCSTR>;
  sealed inline-only slot Flags-value    :: <DWORD>;
  sealed inline-only slot nFileOffset-value :: <WORD>;
  sealed inline-only slot nFileExtension-value :: <WORD>;
  sealed inline-only slot lpstrDefExt-value :: <LPCSTR>;
  sealed inline-only slot lCustData-value :: <LPARAM>;
  sealed inline-only slot lpfnHook-value :: <C-function-pointer>;
  sealed inline-only slot lpTemplateName-value :: <LPCSTR>;
  pack: 1;
  pointer-type-name: <LPOPENFILENAME>;
  c-name: "struct tagOFNA";
end C-struct <OPENFILENAME>;
define sealed domain make (singleton(<LPOPENFILENAME>));
define sealed domain initialize (<LPOPENFILENAME>);

define C-struct <CHOOSECOLOR>
  sealed inline-only slot lStructSize-value :: <DWORD>;
  sealed inline-only slot hwndOwner-value :: <HWND>;
  sealed inline-only slot hInstance-value :: <HWND>;
  sealed inline-only slot rgbResult-value :: <COLORREF>;
  sealed inline-only slot lpCustColors-value :: <LPCOLORREF>;
  sealed inline-only slot Flags-value    :: <DWORD>;
  sealed inline-only slot lCustData-value :: <LPARAM>;
  sealed inline-only slot lpfnHook-value :: <C-function-pointer>;
  sealed inline-only slot lpTemplateName-value :: <LPCSTR>;
  pack: 1;
  pointer-type-name: <LPCHOOSECOLOR>;
  c-name: "struct tagCHOOSECOLORA";
end C-struct <CHOOSECOLOR>;
define sealed domain make (singleton(<LPCHOOSECOLOR>));
define sealed domain initialize (<LPCHOOSECOLOR>);

define C-struct <CHOOSEFONT>
  sealed inline-only slot lStructSize-value :: <DWORD>;
  sealed inline-only slot hwndOwner-value :: <HWND>;
  sealed inline-only slot hDC-value      :: <HDC>;
  sealed inline-only slot lpLogFont-value :: <LPLOGFONT>;
  sealed inline-only slot iPointSize-value :: <INT>;
  sealed inline-only slot Flags-value    :: <DWORD>;
  sealed inline-only slot rgbColors-value :: <COLORREF>;
  sealed inline-only slot lCustData-value :: <LPARAM>;
  sealed inline-only slot lpfnHook-value :: <C-function-pointer>;
  sealed inline-only slot lpTemplateName-value :: <LPCSTR>;
  sealed inline-only slot hInstance-value :: <HINSTANCE>;
  sealed inline-only slot lpszStyle-value :: <LPSTR>;
  sealed inline-only slot nFontType-value :: <WORD>;
  sealed inline-only slot ___MISSING_ALIGNMENT__ :: <WORD>;
  sealed inline-only slot nSizeMin-value :: <INT>;
  sealed inline-only slot nSizeMax-value :: <INT>;
  pack: 1;
  pointer-type-name: <LPCHOOSEFONT>;
  c-name: "struct tagCHOOSEFONTA";
end C-struct <CHOOSEFONT>;
define sealed domain make (singleton(<LPCHOOSEFONT>));
define sealed domain initialize (<LPCHOOSEFONT>);

define C-struct <PRINTDLG>
  sealed inline-only slot lStructSize-value :: <DWORD>;
  sealed inline-only slot hwndOwner-value :: <HWND>;
  sealed inline-only slot hDevMode-value :: <HGLOBAL>;
  sealed inline-only slot hDevNames-value :: <HGLOBAL>;
  sealed inline-only slot hDC-value      :: <HDC>;
  sealed inline-only slot Flags-value    :: <DWORD>;
  sealed inline-only slot nFromPage-value :: <WORD>;
  sealed inline-only slot nToPage-value  :: <WORD>;
  sealed inline-only slot nMinPage-value :: <WORD>;
  sealed inline-only slot nMaxPage-value :: <WORD>;
  sealed inline-only slot nCopies-value  :: <WORD>;
  sealed inline-only slot hInstance-value :: <HINSTANCE>;
  sealed inline-only slot lCustData-value :: <LPARAM>;
  sealed inline-only slot lpfnPrintHook-value :: <C-function-pointer>;
  sealed inline-only slot lpfnSetupHook-value :: <C-function-pointer>;
  sealed inline-only slot lpPrintTemplateName-value :: <LPCSTR>;
  sealed inline-only slot lpSetupTemplateName-value :: <LPCSTR>;
  sealed inline-only slot hPrintTemplate-value :: <HGLOBAL>;
  sealed inline-only slot hSetupTemplate-value :: <HGLOBAL>;
  pack: 1;
  pointer-type-name: <LPPRINTDLG>;
  c-name: "struct tagPDA";
end C-struct <PRINTDLG>;
define sealed domain make (singleton(<LPPRINTDLG>));
define sealed domain initialize (<LPPRINTDLG>);

define C-struct <TOOLINFOA>
  sealed inline-only slot cbSize-value   :: <UINT>;
  sealed inline-only slot uFlags-value   :: <UINT>;
  sealed inline-only slot hwnd-value     :: <HWND>;
  sealed inline-only slot uId-value      :: <UINT>;
  sealed inline-only slot rect-value     :: <RECT>;
  sealed inline-only slot hinst-value    :: <HINSTANCE>;
  sealed inline-only slot lpszText-value :: <LPSTR>;
  pack: 1;
  pointer-type-name: <LPTOOLINFOA>;
  c-name: "struct tagTOOLINFOA";
end C-struct <TOOLINFOA>;
define sealed domain make (singleton(<LPTOOLINFOA>));
define sealed domain initialize (<LPTOOLINFOA>);

define C-struct <TOOLTIPTEXTA>
  sealed inline-only slot hdr-value      :: <NMHDR>;
  sealed inline-only slot lpszText-value :: <LPSTR>;
  sealed inline-only array slot szText-array :: <C-char>, length: 80,
        address-getter: szText-value;
  sealed inline-only slot hinst-value    :: <HINSTANCE>;
  sealed inline-only slot uFlags-value   :: <UINT>;
  pack: 1;
  pointer-type-name: <LPTOOLTIPTEXT>;
  c-name: "struct tagNMTTDISPIFNOA";
end C-struct <TOOLTIPTEXTA>;
define sealed domain make (singleton(<LPTOOLTIPTEXT>));
define sealed domain initialize (<LPTOOLTIPTEXT>);

define C-struct <LV-ITEMA>
  sealed inline-only slot mask-value     :: <UINT>;
  sealed inline-only slot iItem-value    :: <C-int>;
  sealed inline-only slot iSubItem-value :: <C-int>;
  sealed inline-only slot state-value    :: <UINT>;
  sealed inline-only slot stateMask-value :: <UINT>;
  sealed inline-only slot pszText-value  :: <LPSTR>;
  sealed inline-only slot cchTextMax-value :: <C-int>;
  sealed inline-only slot iImage-value   :: <C-int>;
  sealed inline-only slot lParam-value   :: <LPARAM>;
  pack: 1;
  pointer-type-name: <LPLV-ITEM>;
  c-name: "struct tagLVITEMA";
end C-struct <LV-ITEMA>;
define sealed domain make (singleton(<LPLV-ITEM>));
define sealed domain initialize (<LPLV-ITEM>);

define C-struct <LV-HITTESTINFO>
  sealed inline-only slot pt-value       :: w/<POINT>;
  sealed inline-only slot flags-value    :: <UINT>;
  sealed inline-only slot iItem-value    :: <C-int>;
  pack: 1;
  pointer-type-name: <LPLV-HITTESTINFO>;
  c-name: "struct tagLVHITTESTINFO";
end C-struct <LV-HITTESTINFO>;
define sealed domain make (singleton(<LPLV-HITTESTINFO>));
define sealed domain initialize (<LPLV-HITTESTINFO>);

define C-struct <LV-COLUMNA>
  sealed inline-only slot mask-value     :: <UINT>;
  sealed inline-only slot fmt-value      :: <C-int>;
  sealed inline-only slot cx-value       :: <C-int>;
  sealed inline-only slot pszText-value  :: <LPSTR>;
  sealed inline-only slot cchTextMax-value :: <C-int>;
  sealed inline-only slot iSubItem-value :: <C-int>;
  pack: 1;
  pointer-type-name: <LPLV-COLUMN>;
  c-name: "struct tagLVCOLUMNA";
end C-struct <LV-COLUMNA>;
define sealed domain make (singleton(<LPLV-COLUMN>));
define sealed domain initialize (<LPLV-COLUMN>);

define C-struct <NM-LISTVIEW>
  sealed inline-only slot hdr-value      :: <NMHDR>;
  sealed inline-only slot iItem-value    :: <C-int>;
  sealed inline-only slot iSubItem-value :: <C-int>;
  sealed inline-only slot uNewState-value :: <UINT>;
  sealed inline-only slot uOldState-value :: <UINT>;
  sealed inline-only slot uChanged-value :: <UINT>;
  sealed inline-only slot ptAction-value :: w/<POINT>;
  sealed inline-only slot lParam-value   :: <LPARAM>;
  pack: 1;
  pointer-type-name: <LPNM-LISTVIEW>;
  c-name: "struct tagNMLISTVIEW";
end C-struct <NM-LISTVIEW>;
define sealed domain make (singleton(<LPNM-LISTVIEW>));
define sealed domain initialize (<LPNM-LISTVIEW>);

define C-struct <LV-KEYDOWN>
  sealed inline-only slot hdr-value      :: <NMHDR>;
  sealed inline-only slot wVKey-value    :: <WORD>;
  sealed inline-only slot flags-value    :: <UINT>;
  pack: 1;
  pointer-type-name: <LPLV-KEYDOWN>;
  c-name: "struct tagLVKEYDOWN";
end C-struct <LV-KEYDOWN>;
define sealed domain make (singleton(<LPLV-KEYDOWN>));
define sealed domain initialize (<LPLV-KEYDOWN>);

define C-struct <TV-ITEMA>
  sealed inline-only slot mask-value     :: <UINT>;
  sealed inline-only slot hItem-value    :: <HTREEITEM>;
  sealed inline-only slot state-value    :: <UINT>;
  sealed inline-only slot stateMask-value :: <UINT>;
  sealed inline-only slot pszText-value  :: <LPSTR>;
  sealed inline-only slot cchTextMax-value :: <C-int>;
  sealed inline-only slot iImage-value   :: <C-int>;
  sealed inline-only slot iSelectedImage-value :: <C-int>;
  sealed inline-only slot cChildren-value :: <C-int>;
  sealed inline-only slot lParam-value   :: <LPARAM>;
  pack: 1;
  pointer-type-name: <LPTVITEM>;
  c-name: "struct tagTVITEMA";
end C-struct <TV-ITEMA>;
define sealed domain make (singleton(<LPTVITEM>));
define sealed domain initialize (<LPTVITEM>);
define inline constant <TVITEMA> = <TV-ITEMA>;
define inline constant <LPTV-ITEM> = <LPTVITEM>;


define C-struct <TV-INSERTSTRUCTA>
  sealed inline-only slot hParent-value  :: <HTREEITEM>;
  sealed inline-only slot hInsertAfter-value :: <HTREEITEM>;
  sealed inline-only slot item-value     :: <TV-ITEMA>;
  pack: 1;
  pointer-type-name: <LPTV-INSERTSTRUCT>;
  c-name: "struct tagTVINSERTSTRUCTA";
end C-struct <TV-INSERTSTRUCTA>;
define sealed domain make (singleton(<LPTV-INSERTSTRUCT>));
define sealed domain initialize (<LPTV-INSERTSTRUCT>);

define C-struct <TV-HITTESTINFO>
  sealed inline-only slot pt-value       :: w/<POINT>;
  sealed inline-only slot flags-value    :: <UINT>;
  sealed inline-only slot hItem-value    :: <HTREEITEM>;
  pack: 1;
  pointer-type-name: <LPTV-HITTESTINFO>;
  c-name: "struct tagTVHITTESTINFO";
end C-struct <TV-HITTESTINFO>;
define sealed domain make (singleton(<LPTV-HITTESTINFO>));
define sealed domain initialize (<LPTV-HITTESTINFO>);

define C-struct <NM-TREEVIEWA>
  sealed inline-only slot hdr-value      :: <NMHDR>;
  sealed inline-only slot action-value   :: <UINT>;
  sealed inline-only slot itemOld-value  :: <TVITEMA>;
  sealed inline-only slot itemNew-value  :: <TVITEMA>;
  sealed inline-only slot ptDrag-value   :: w/<POINT>;
  pack: 1;
  pointer-type-name: <LPNM-TREEVIEW>;
  c-name: "struct tagNMTREEVIEWA";
end C-struct <NM-TREEVIEWA>;
define sealed domain make (singleton(<LPNM-TREEVIEWA>));
define sealed domain initialize (<LPNM-TREEVIEWA>);
define inline constant <LPNM-TREEVIEWA> = <LPNM-TREEVIEW>;

define C-struct <TV-KEYDOWN>
  sealed inline-only slot hdr-value      :: <NMHDR>;
  sealed inline-only slot wVKey-value    :: <WORD>;
  sealed inline-only slot flags-value    :: <UINT>;
  pack: 1;
  pointer-type-name: <LPTV-KEYDOWN>;
  c-name: "struct tagTVKEYDOWN";
end C-struct <TV-KEYDOWN>;
define sealed domain make (singleton(<LPTV-KEYDOWN>));
define sealed domain initialize (<LPTV-KEYDOWN>);

define C-struct <TC-ITEMA>
  sealed inline-only slot mask-value     :: <UINT>;
  sealed inline-only slot lpReserved1    :: <UINT>;
  sealed inline-only slot lpReserved2    :: <UINT>;
  sealed inline-only slot pszText-value  :: <LPSTR>;
  sealed inline-only slot cchTextMax-value :: <C-int>;
  sealed inline-only slot iImage-value   :: <C-int>;
  sealed inline-only slot lParam-value   :: <LPARAM>;
  pack: 1;
  pointer-type-name: <LPTC-ITEMA>;
  c-name: "struct tagTCITEMA";
end C-struct <TC-ITEMA>;
define sealed domain make (singleton(<LPTC-ITEMA>));
define sealed domain initialize (<LPTC-ITEMA>);

define C-struct <TC-KEYDOWN>
  sealed inline-only slot hdr-value      :: <NMHDR>;
  sealed inline-only slot wVKey-value    :: <WORD>;
  sealed inline-only slot flags-value    :: <UINT>;
  pack: 1;
  pointer-type-name: <LPTC-KEYDOWN>;
  c-name: "struct tagTCKEYDOWN";
end C-struct <TC-KEYDOWN>;
define sealed domain make (singleton(<LPTC-KEYDOWN>));
define sealed domain initialize (<LPTC-KEYDOWN>);

define C-struct <HH-AKLINK>
  sealed inline-only slot cbStruct-value :: <C-int>;
  sealed inline-only slot fReserved   :: <BOOL>;
  sealed inline-only slot pszKeywords-value :: <LPCTSTR>;
  sealed inline-only slot pszUrl-value :: <LPCTSTR>;
  sealed inline-only slot pszMsgText-value :: <LPCTSTR>;
  sealed inline-only slot pszMsgTitle-value :: <LPCTSTR>;
  sealed inline-only slot pszWindow-value :: <LPCTSTR>;
  sealed inline-only slot fIndexOnFail-value :: <BOOL>;
  pointer-type-name: <LPHH-AKLINK>;
  c-name: "struct tagHH_AKLINK";
end C-struct <HH-AKLINK>;
define sealed domain make (singleton(<LPHH-AKLINK>));
define sealed domain initialize (<LPHH-AKLINK>);

define C-struct <ACTCTX>
  sealed inline-only slot cbSize-value :: <DWORD>;
  sealed inline-only slot dwFlags-value :: <DWORD>;
  sealed inline-only slot lpSource-value :: <LPCSTR>;
  sealed inline-only slot wProcessorArchitecture-value :: <USHORT>;
  sealed inline-only slot wLangId-value :: <WORD>;
  sealed inline-only slot lpAssemblyDirectory-value :: <LPCSTR>;
  sealed inline-only slot lpResourceName-value :: <LPCSTR>;
  sealed inline-only slot lpApplicationName-value :: <LPCSTR>;
  sealed inline-only slot hModule-value :: <HMODULE>;
  pointer-type-name: <PACTCTX>;
  c-name: "struct tagACTCTXA";
end C-struct <ACTCTX>;
define sealed domain make (singleton(<PACTCTX>));
define sealed domain initialize (<PACTCTX>);
define inline constant <PCACTCTX> = /* const */ <PACTCTX>;

define macro callback-definer
  { define callback ?new:name :: ?ftype:name = ?old:name } =>
        { ?ftype ## "-callback-wrapper" (?new, ?old) }
end;

define macro <WNDPROC>-callback-wrapper
  { <WNDPROC>-callback-wrapper(?new:name,?old:name) } =>
    { define C-callable-wrapper ?new of ?old
        parameter hWnd :: <HWND>;
        parameter Msg  :: <UINT>;
        parameter wParam :: <WPARAM>;
        parameter lParam :: <LPARAM>;
        result    value :: <LRESULT>;
        c-modifiers: "__stdcall";
      end C-callable-wrapper }
end;

define macro <DLGPROC>-callback-wrapper
  { <DLGPROC>-callback-wrapper(?new:name,?old:name) } =>
    { define C-callable-wrapper ?new of ?old
        parameter hWnd :: <HWND>;
        parameter Msg  :: <UINT>;
        parameter wParam :: <WPARAM>;
        parameter lParam :: <LPARAM>;
        result    value :: <BOOL>;
        c-modifiers: "__stdcall";
      end C-callable-wrapper }
end;

define macro <ENUMRESTYPEPROC>-callback-wrapper
 { <ENUMRESTYPEPROC>-callback-wrapper(?new:name,?old:name) } =>
 { define C-callable-wrapper ?new of ?old
  parameter hModule    :: <HMODULE>;
  parameter lpType     :: <LPTSTR>;
  parameter lParam     :: <LONG>;
  result value :: <BOOL>;
  c-modifiers: "__stdcall";
 end C-callable-wrapper }
end;

define macro <ENUMRESNAMEPROC>-callback-wrapper
 { <ENUMRESNAMEPROC>-callback-wrapper(?new:name,?old:name) } =>
 { define C-callable-wrapper ?new of ?old
  parameter hModule    :: <HMODULE>;
  parameter lpType     :: <LPCTSTR>;
  parameter lpName     :: <LPTSTR>;
  parameter lParam     :: <LONG>;
  result value :: <BOOL>;
  c-modifiers: "__stdcall";
 end C-callable-wrapper }
end;

define macro <ENUMRESLANGPROC>-callback-wrapper
 { <ENUMRESLANGPROC>-callback-wrapper(?new:name,?old:name) } =>
 { define C-callable-wrapper ?new of ?old
  parameter hModule    :: <HMODULE>;
  parameter lpType     :: <LPCTSTR>;
  parameter lpName     :: <LPCTSTR>;
  parameter wLanguage  :: <WORD>;
  parameter lParam     :: <LONG>;
  result value :: <BOOL>;
  c-modifiers: "__stdcall";
 end C-callable-wrapper }
end;


define inline-only C-function Arc
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  parameter int3       :: <C-int>;
  parameter int4       :: <C-int>;
  parameter int5       :: <C-int>;
  parameter int6       :: <C-int>;
  parameter int7       :: <C-int>;
  parameter int8       :: <C-int>;
  parameter int9       :: <C-int>;
  result value :: <BOOL>;
  c-name: "Arc", c-modifiers: "__stdcall";
end;

define inline-only C-function BitBlt
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  parameter int3       :: <C-int>;
  parameter int4       :: <C-int>;
  parameter int5       :: <C-int>;
  parameter hdc6       :: <HDC>;
  parameter int7       :: <C-int>;
  parameter int8       :: <C-int>;
  parameter dword9     :: <DWORD>;
  result value :: <BOOL>;
  c-name: "BitBlt", c-modifiers: "__stdcall";
end;

define inline-only C-function CreateCompatibleBitmap
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  parameter int3       :: <C-int>;
  result value :: <HBITMAP>;
  c-name: "CreateCompatibleBitmap", c-modifiers: "__stdcall";
end;

define inline-only C-function CreateCompatibleDC
  parameter hdc1       :: <HDC>;
  result value :: <HDC>;
  c-name: "CreateCompatibleDC", c-modifiers: "__stdcall";
end;

define inline-only C-function CreateFont
  parameter int1       :: <C-int>;
  parameter int2       :: <C-int>;
  parameter int3       :: <C-int>;
  parameter int4       :: <C-int>;
  parameter int5       :: <C-int>;
  parameter dword6     :: <DWORD>;
  parameter dword7     :: <DWORD>;
  parameter dword8     :: <DWORD>;
  parameter dword9     :: <DWORD>;
  parameter dword10    :: <DWORD>;
  parameter dword11    :: <DWORD>;
  parameter dword12    :: <DWORD>;
  parameter dword13    :: <DWORD>;
  parameter lpcstr14   :: <LPCSTR>;
  result value :: <HFONT>;
  c-name: "CreateFontA", c-modifiers: "__stdcall";
end;

define inline-only C-function CreatePen
  parameter int1       :: <C-int>;
  parameter int2       :: <C-int>;
  parameter colorref3  :: <COLORREF>;
  result value :: <HPEN>;
  c-name: "CreatePen", c-modifiers: "__stdcall";
end;

define inline-only C-function CreatePatternBrush
  parameter hbitmap1   :: <HBITMAP>;
  result value :: <HBRUSH>;
  c-name: "CreatePatternBrush", c-modifiers: "__stdcall";
end;

define inline-only C-function CreateSolidBrush
  parameter colorref1  :: <COLORREF>;
  result value :: <HBRUSH>;
  c-name: "CreateSolidBrush", c-modifiers: "__stdcall";
end;

define inline-only C-function DeleteDC
  parameter hdc1       :: <HDC>;
  result value :: <BOOL>;
  c-name: "DeleteDC", c-modifiers: "__stdcall";
end;

define inline-only C-function DeleteObject
  parameter hgdiobj1   :: <HGDIOBJ>;
  result value :: <BOOL>;
  c-name: "DeleteObject", c-modifiers: "__stdcall";
end;

define inline-only C-function Ellipse
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  parameter int3       :: <C-int>;
  parameter int4       :: <C-int>;
  parameter int5       :: <C-int>;
  result value :: <BOOL>;
  c-name: "Ellipse", c-modifiers: "__stdcall";
end;

define inline-only C-function GetDeviceCaps
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  result value :: <C-int>;
  c-name: "GetDeviceCaps", c-modifiers: "__stdcall";
end;

define inline-only C-function GetStockObject
  parameter int1       :: <C-int>;
  result value :: <HGDIOBJ>;
  c-name: "GetStockObject", c-modifiers: "__stdcall";
end;

define inline-only C-function GetTextExtentPoint32
  parameter hdc1       :: <HDC>;
  parameter lpcstr2    :: <LPCSTR>;
  parameter int3       :: <C-int>;
  parameter lpsize4    :: <LPSIZE>;
  result value :: <BOOL>;
  c-name: "GetTextExtentPoint32A", c-modifiers: "__stdcall";
end;

define inline-only C-function LineTo
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  parameter int3       :: <C-int>;
  result value :: <BOOL>;
  c-name: "LineTo", c-modifiers: "__stdcall";
end;

define inline-only C-function Pie
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  parameter int3       :: <C-int>;
  parameter int4       :: <C-int>;
  parameter int5       :: <C-int>;
  parameter int6       :: <C-int>;
  parameter int7       :: <C-int>;
  parameter int8       :: <C-int>;
  parameter int9       :: <C-int>;
  result value :: <BOOL>;
  c-name: "Pie", c-modifiers: "__stdcall";
end;

define inline-only C-function Rectangle
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  parameter int3       :: <C-int>;
  parameter int4       :: <C-int>;
  parameter int5       :: <C-int>;
  result value :: <BOOL>;
  c-name: "Rectangle", c-modifiers: "__stdcall";
end;

define inline-only C-function RoundRect
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  parameter int3       :: <C-int>;
  parameter int4       :: <C-int>;
  parameter int5       :: <C-int>;
  parameter int6       :: <C-int>;
  parameter int7       :: <C-int>;
  result value :: <BOOL>;
  c-name: "RoundRect", c-modifiers: "__stdcall";
end;

define inline-only C-function SelectObject
  parameter hdc1       :: <HDC>;
  parameter hgdiobj2   :: <HGDIOBJ>;
  result value :: <HGDIOBJ>;
  c-name: "SelectObject", c-modifiers: "__stdcall";
end;

define inline-only C-function SetBkColor
  parameter hdc1       :: <HDC>;
  parameter colorref2  :: <COLORREF>;
  result value :: <COLORREF>;
  c-name: "SetBkColor", c-modifiers: "__stdcall";
end;

define inline-only C-function SetBkMode
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  result value :: <C-int>;
  c-name: "SetBkMode", c-modifiers: "__stdcall";
end;

define inline-only C-function SetPixel
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  parameter int3       :: <C-int>;
  parameter colorref4  :: <COLORREF>;
  result value :: <COLORREF>;
  c-name: "SetPixel", c-modifiers: "__stdcall";
end;

define inline-only C-function SetROP2
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  result value :: <C-int>;
  c-name: "SetROP2", c-modifiers: "__stdcall";
end;

define inline-only C-function SetTextColor
  parameter hdc1       :: <HDC>;
  parameter colorref2  :: <COLORREF>;
  result value :: <COLORREF>;
  c-name: "SetTextColor", c-modifiers: "__stdcall";
end;

define inline-only C-function SetTextAlign
  parameter hdc1       :: <HDC>;
  parameter uint2      :: <UINT>;
  result value :: <UINT>;
  c-name: "SetTextAlign", c-modifiers: "__stdcall";
end;

define inline-only C-function GetTextMetrics
  parameter hdc1       :: <HDC>;
  parameter lptextmetrica2 :: <LPTEXTMETRIC>;
  result value :: <BOOL>;
  c-name: "GetTextMetricsA", c-modifiers: "__stdcall";
end;

define inline-only C-function AbortPath
  parameter hdc1       :: <HDC>;
  result value :: <BOOL>;
  c-name: "AbortPath", c-modifiers: "__stdcall";
end;

define inline-only C-function ArcTo
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  parameter int3       :: <C-int>;
  parameter int4       :: <C-int>;
  parameter int5       :: <C-int>;
  parameter int6       :: <C-int>;
  parameter int7       :: <C-int>;
  parameter int8       :: <C-int>;
  parameter int9       :: <C-int>;
  result value :: <BOOL>;
  c-name: "ArcTo", c-modifiers: "__stdcall";
end;

define inline-only C-function BeginPath
  parameter hdc1       :: <HDC>;
  result value :: <BOOL>;
  c-name: "BeginPath", c-modifiers: "__stdcall";
end;

define inline-only C-function CloseFigure
  parameter hdc1       :: <HDC>;
  result value :: <BOOL>;
  c-name: "CloseFigure", c-modifiers: "__stdcall";
end;

define inline-only C-function EndPath
  parameter hdc1       :: <HDC>;
  result value :: <BOOL>;
  c-name: "EndPath", c-modifiers: "__stdcall";
end;

define inline-only C-function FillPath
  parameter hdc1       :: <HDC>;
  result value :: <BOOL>;
  c-name: "FillPath", c-modifiers: "__stdcall";
end;

define inline-only C-function SelectClipPath
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  result value :: <BOOL>;
  c-name: "SelectClipPath", c-modifiers: "__stdcall";
end;

define inline-only C-function StrokeAndFillPath
  parameter hdc1       :: <HDC>;
  result value :: <BOOL>;
  c-name: "StrokeAndFillPath", c-modifiers: "__stdcall";
end;

define inline-only C-function StrokePath
  parameter hdc1       :: <HDC>;
  result value :: <BOOL>;
  c-name: "StrokePath", c-modifiers: "__stdcall";
end;

define inline-only C-function GetObject
  parameter hgdiobj1   :: <HGDIOBJ>;
  parameter int2       :: <C-int>;
  parameter lpvoid3    :: <LPVOID>;
  result value :: <C-int>;
  c-name: "GetObjectA", c-modifiers: "__stdcall";
end;

define inline-only C-function MoveToEx
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  parameter int3       :: <C-int>;
  parameter lppoint4   :: <LPPOINT>;
  result value :: <BOOL>;
  c-name: "MoveToEx", c-modifiers: "__stdcall";
end;

define inline-only C-function TextOut
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  parameter int3       :: <C-int>;
  parameter lpcstr4    :: <LPCSTR>;
  parameter int5       :: <C-int>;
  result value :: <BOOL>;
  c-name: "TextOutA", c-modifiers: "__stdcall";
end;

define inline-only C-function Polygon
  parameter hdc1       :: <HDC>;
  parameter lppoint2   ::  /* const */ <LPPOINT>;
  parameter int3       :: <C-int>;
  result value :: <BOOL>;
  c-name: "Polygon", c-modifiers: "__stdcall";
end;

define inline-only C-function Polyline
  parameter hdc1       :: <HDC>;
  parameter lppoint2   ::  /* const */ <LPPOINT>;
  parameter int3       :: <C-int>;
  result value :: <BOOL>;
  c-name: "Polyline", c-modifiers: "__stdcall";
end;

define inline-only C-function PolyBezierTo
  parameter hdc1       :: <HDC>;
  parameter lppoint2   ::  /* const */ <LPPOINT>;
  parameter dword3     :: <DWORD>;
  result value :: <BOOL>;
  c-name: "PolyBezierTo", c-modifiers: "__stdcall";
end;

define inline-only C-function GetTextFace
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  parameter lpstr3     :: <LPSTR>;
  result value :: <C-int>;
  c-name: "GetTextFaceA", c-modifiers: "__stdcall";
end;

define inline-only C-function GlobalAlloc
  parameter uFlags     :: <UINT>;
  parameter dwBytes    :: <DWORD>;
  result value :: <HGLOBAL>;
  c-name: "GlobalAlloc", c-modifiers: "__stdcall";
end;

define inline-only C-function GlobalLock
  parameter hMem       :: <HGLOBAL>;
  result value :: <LPVOID>;
  c-name: "GlobalLock", c-modifiers: "__stdcall";
end;

define inline-only C-function GlobalUnlock
  parameter hMem       :: <HGLOBAL>;
  result value :: <BOOL>;
  c-name: "GlobalUnlock", c-modifiers: "__stdcall";
end;

define inline-only C-function ExitProcess
  parameter uExitCode  :: <UINT>;
  c-name: "ExitProcess", c-modifiers: "__stdcall";
end;

define inline-only C-function GetLastError
  result value :: <DWORD>;
  c-name: "GetLastError", c-modifiers: "__stdcall";
end;

define inline-only C-function SetLastError
  parameter dwErrCode  :: <DWORD>;
  c-name: "SetLastError", c-modifiers: "__stdcall";
end;

define inline-only C-function GetModuleFileName
  parameter hModule    :: <HMODULE>;
  parameter lpFilename :: <LPSTR>;
  parameter nSize      :: <DWORD>;
  result value :: <DWORD>;
  c-name: "GetModuleFileNameA", c-modifiers: "__stdcall";
end;

define inline-only C-function GetModuleHandle
  parameter lpModuleName :: <LPCSTR>;
  result value :: <HMODULE>;
  c-name: "GetModuleHandleA", c-modifiers: "__stdcall";
end;

define inline-only C-function GetProcAddress
  parameter hModule    :: <HMODULE>;
  parameter lpProcName :: <LPCSTR>;
  result value :: <C-function-pointer>;
  c-name: "GetProcAddress", c-modifiers: "__stdcall";
end;

define inline-only C-function GetStartupInfo
  parameter lpStartupInfo :: <LPSTARTUPINFO>;
  c-name: "GetStartupInfoA", c-modifiers: "__stdcall";
end;

define inline-only C-function OutputDebugString
  parameter lpOutputString :: <LPCSTR>;
  c-name: "OutputDebugStringA", c-modifiers: "__stdcall";
end;

define inline-only C-function GetVersionEx
  parameter lpVersionInformation :: <LPOSVERSIONINFO>;
  result value :: <BOOL>;
  c-name: "GetVersionExA", c-modifiers: "__stdcall";
end;

define inline-only C-function IsDBCSLeadByte
  parameter TestChar   :: <C-BYTE>;
  result value :: <BOOL>;
  c-name: "IsDBCSLeadByte", c-modifiers: "__stdcall";
end;

define inline-only C-function DrawEdge
  parameter hdc        :: <HDC>;
  parameter qrc        :: <LPRECT>;
  parameter edge       :: <UINT>;
  parameter grfFlags   :: <UINT>;
  result value :: <BOOL>;
  c-name: "DrawEdge", c-modifiers: "__stdcall";
end;

define inline-only C-function GetMessage
  parameter lpMsg      :: <LPMSG>;
  parameter hWnd       :: <HWND>;
  parameter wMsgFilterMin :: <UINT>;
  parameter wMsgFilterMax :: <UINT>;
  result value :: <BOOL>;
  c-name: "GetMessageA", c-modifiers: "__stdcall";
end;

define inline-only C-function TranslateMessage
  parameter lpMsg      ::  /* const */ <LPMSG>;
  result value :: <BOOL>;
  c-name: "TranslateMessage", c-modifiers: "__stdcall";
end;

define inline-only C-function DispatchMessage
  parameter lpMsg      ::  /* const */ <LPMSG>;
  result value :: <LONG>;
  c-name: "DispatchMessageA", c-modifiers: "__stdcall";
end;

define inline-only C-function SendMessage
  parameter hWnd       :: <HWND>;
  parameter Msg        :: <UINT>;
  parameter wParam     :: <WPARAM>;
  parameter lParam     :: <LPARAM>;
  result value :: <LRESULT>;
  c-name: "SendMessageA", c-modifiers: "__stdcall";
end;

define inline-only C-function PostMessage
  parameter hWnd       :: <HWND>;
  parameter Msg        :: <UINT>;
  parameter wParam     :: <WPARAM>;
  parameter lParam     :: <LPARAM>;
  result value :: <BOOL>;
  c-name: "PostMessageA", c-modifiers: "__stdcall";
end;

define inline-only C-function DefWindowProc
  parameter hWnd       :: <HWND>;
  parameter Msg        :: <UINT>;
  parameter wParam     :: <WPARAM>;
  parameter lParam     :: <LPARAM>;
  result value :: <LRESULT>;
  c-name: "DefWindowProcA", c-modifiers: "__stdcall";
end;

define inline-only C-function PostQuitMessage
  parameter nExitCode  :: <C-int>;
  c-name: "PostQuitMessage", c-modifiers: "__stdcall";
end;

define inline-only C-function CallWindowProc
  parameter lpPrevWndFunc :: <C-function-pointer>;
  parameter hWnd       :: <HWND>;
  parameter Msg        :: <UINT>;
  parameter wParam     :: <WPARAM>;
  parameter lParam     :: <LPARAM>;
  result value :: <LRESULT>;
  c-name: "CallWindowProcA", c-modifiers: "__stdcall";
end;

define inline-only C-function RegisterClass
  parameter lpWndClass ::  /* const */ <LPWNDCLASSA>;
  result value :: <ATOM>;
  c-name: "RegisterClassA", c-modifiers: "__stdcall";
end;

define inline-only C-function UnregisterClass
  parameter lpClassName :: <LPCSTR>;
  parameter hInstance  :: <HINSTANCE>;
  result value :: <BOOL>;
  c-name: "UnregisterClassA", c-modifiers: "__stdcall";
end;

define inline-only C-function DestroyWindow
  parameter hWnd       :: <HWND>;
  result value :: <BOOL>;
  c-name: "DestroyWindow", c-modifiers: "__stdcall";
end;

define inline-only C-function ShowWindow
  parameter hWnd       :: <HWND>;
  parameter nCmdShow   :: <C-int>;
  result value :: <BOOL>;
  c-name: "ShowWindow", c-modifiers: "__stdcall";
end;

define inline-only C-function SetWindowPos
  parameter hWnd       :: <HWND>;
  parameter hWndInsertAfter :: <HWND>;
  parameter X          :: <C-int>;
  parameter Y          :: <C-int>;
  parameter cx         :: <C-int>;
  parameter cy         :: <C-int>;
  parameter uFlags     :: <UINT>;
  result value :: <BOOL>;
  c-name: "SetWindowPos", c-modifiers: "__stdcall";
end;

define inline-only C-function IsWindowVisible
  parameter hWnd       :: <HWND>;
  result value :: <BOOL>;
  c-name: "IsWindowVisible", c-modifiers: "__stdcall";
end;

define inline-only C-function GetDlgItem
  parameter hDlg       :: <HWND>;
  parameter nIDDlgItem :: <C-int>;
  result value :: <HWND>;
  c-name: "GetDlgItem", c-modifiers: "__stdcall";
end;

define inline-only C-function GetDialogBaseUnits
  result value :: <C-both-long>;
  c-name: "GetDialogBaseUnits", c-modifiers: "__stdcall";
end;


define inline-only C-function DefDlgProc
  parameter hDlg       :: <HWND>;
  parameter Msg        :: <UINT>;
  parameter wParam     :: <WPARAM>;
  parameter lParam     :: <LPARAM>;
  result value :: <LRESULT>;
  c-name: "DefDlgProcA", c-modifiers: "__stdcall";
end;

define inline-only C-function OpenClipboard
  parameter hWndNewOwner :: <HWND>;
  result value :: <BOOL>;
  c-name: "OpenClipboard", c-modifiers: "__stdcall";
end;

define inline-only C-function CloseClipboard
  result value :: <BOOL>;
  c-name: "CloseClipboard", c-modifiers: "__stdcall";
end;

define inline-only C-function GetClipboardOwner
  result value :: <HWND>;
  c-name: "GetClipboardOwner", c-modifiers: "__stdcall";
end;

define inline-only C-function SetClipboardData
  parameter uFormat    :: <UINT>;
  parameter hMem       :: <HANDLE>;
  result value :: <HANDLE>;
  c-name: "SetClipboardData", c-modifiers: "__stdcall";
end;

define inline-only C-function GetClipboardData
  parameter uFormat    :: <UINT>;
  result value :: <HANDLE>;
  c-name: "GetClipboardData", c-modifiers: "__stdcall";
end;

define inline-only C-function EnumClipboardFormats
  parameter format     :: <UINT>;
  result value :: <UINT>;
  c-name: "EnumClipboardFormats", c-modifiers: "__stdcall";
end;

define inline-only C-function EmptyClipboard
  result value :: <BOOL>;
  c-name: "EmptyClipboard", c-modifiers: "__stdcall";
end;

define inline-only C-function SetFocus
  parameter hWnd       :: <HWND>;
  result value :: <HWND>;
  c-name: "SetFocus", c-modifiers: "__stdcall";
end;

define inline-only C-function GetFocus
  result value :: <HWND>;
  c-name: "GetFocus", c-modifiers: "__stdcall";
end;

define inline-only C-function GetKeyState
  parameter nVirtKey   :: <C-int>;
  result value :: <SHORT>;
  c-name: "GetKeyState", c-modifiers: "__stdcall";
end;

define inline-only C-function ToAscii
  parameter uVirtKey   :: <UINT>;
  parameter uScanCode  :: <UINT>;
  parameter lpKeyState :: <PBYTE>;
  parameter lpChar     :: <LPWORD>;
  parameter uFlags     :: <UINT>;
  result value :: <C-int>;
  c-name: "ToAscii", c-modifiers: "__stdcall";
end;

define inline-only C-function ToUnicode
  parameter wVirtKey   :: <UINT>;
  parameter wScanCode  :: <UINT>;
  parameter lpKeyState :: <PBYTE>;
  parameter pwszBuff   :: <LPWSTR>;
  parameter cchBuff    :: <C-int>;
  parameter wFlags     :: <UINT>;
  result value :: <C-int>;
  c-name: "ToUnicode", c-modifiers: "__stdcall";
end;

define inline-only C-function SetCapture
  parameter hWnd       :: <HWND>;
  result value :: <HWND>;
  c-name: "SetCapture", c-modifiers: "__stdcall";
end;

define inline-only C-function ReleaseCapture
  result value :: <BOOL>;
  c-name: "ReleaseCapture", c-modifiers: "__stdcall";
end;

define inline-only C-function SetTimer
  parameter hWnd       :: <HWND>;
  parameter nIDEvent   :: <UINT>;
  parameter uElapse    :: <UINT>;
  parameter lpTimerFunc :: <C-function-pointer>;
  result value :: <UINT>;
  c-name: "SetTimer", c-modifiers: "__stdcall";
end;

define inline-only C-function KillTimer
  parameter hWnd       :: <HWND>;
  parameter uIDEvent   :: <UINT>;
  result value :: <BOOL>;
  c-name: "KillTimer", c-modifiers: "__stdcall";
end;

define inline-only C-function EnableWindow
  parameter hWnd       :: <HWND>;
  parameter bEnable    :: <BOOL>;
  result value :: <BOOL>;
  c-name: "EnableWindow", c-modifiers: "__stdcall";
end;

define inline-only C-function IsWindowEnabled
  parameter hWnd       :: <HWND>;
  result value :: <BOOL>;
  c-name: "IsWindowEnabled", c-modifiers: "__stdcall";
end;

define inline-only C-function CreateAcceleratorTable
  parameter lpaccel1   :: <LPACCEL>;
  parameter int2       :: <C-int>;
  result value :: <HACCEL>;
  c-name: "CreateAcceleratorTableA", c-modifiers: "__stdcall";
end;

define inline-only C-function DestroyAcceleratorTable
  parameter hAccel     :: <HACCEL>;
  result value :: <BOOL>;
  c-name: "DestroyAcceleratorTable", c-modifiers: "__stdcall";
end;

define inline-only C-function TranslateAccelerator
  parameter hWnd       :: <HWND>;
  parameter hAccTable  :: <HACCEL>;
  parameter lpMsg      :: <LPMSG>;
  result value :: <C-int>;
  c-name: "TranslateAcceleratorA", c-modifiers: "__stdcall";
end;

define inline-only C-function GetSystemMetrics
  parameter nIndex     :: <C-int>;
  result value :: <C-int>;
  c-name: "GetSystemMetrics", c-modifiers: "__stdcall";
end;

define inline-only C-function GetMenu
  parameter hWnd       :: <HWND>;
  result value :: <HMENU>;
  c-name: "GetMenu", c-modifiers: "__stdcall";
end;

define inline-only C-function SetMenu
  parameter hWnd       :: <HWND>;
  parameter hMenu      :: <HMENU>;
  result value :: <BOOL>;
  c-name: "SetMenu", c-modifiers: "__stdcall";
end;

define inline-only C-function DrawMenuBar
  parameter hWnd       :: <HWND>;
  result value :: <BOOL>;
  c-name: "DrawMenuBar", c-modifiers: "__stdcall";
end;

define inline-only C-function CreateMenu
  result value :: <HMENU>;
  c-name: "CreateMenu", c-modifiers: "__stdcall";
end;

define inline-only C-function CreatePopupMenu
  result value :: <HMENU>;
  c-name: "CreatePopupMenu", c-modifiers: "__stdcall";
end;

define inline-only C-function DestroyMenu
  parameter hMenu      :: <HMENU>;
  result value :: <BOOL>;
  c-name: "DestroyMenu", c-modifiers: "__stdcall";
end;

define inline-only C-function CheckMenuItem
  parameter hMenu      :: <HMENU>;
  parameter uIDCheckItem :: <UINT>;
  parameter uCheck     :: <UINT>;
  result value :: <DWORD>;
  c-name: "CheckMenuItem", c-modifiers: "__stdcall";
end;

define inline-only C-function EnableMenuItem
  parameter hMenu      :: <HMENU>;
  parameter uIDEnableItem :: <UINT>;
  parameter uEnable    :: <UINT>;
  result value :: <BOOL>;
  c-name: "EnableMenuItem", c-modifiers: "__stdcall";
end;

define inline-only C-function GetSubMenu
  parameter hMenu      :: <HMENU>;
  parameter nPos       :: <C-int>;
  result value :: <HMENU>;
  c-name: "GetSubMenu", c-modifiers: "__stdcall";
end;

define inline-only C-function GetMenuItemCount
  parameter hMenu      :: <HMENU>;
  result value :: <C-int>;
  c-name: "GetMenuItemCount", c-modifiers: "__stdcall";
end;

define inline-only C-function AppendMenu
  parameter hMenu      :: <HMENU>;
  parameter uFlags     :: <UINT>;
  parameter uIDNewItem :: <UINT>;
  parameter lpNewItem  :: <LPCSTR>;
  result value :: <BOOL>;
  c-name: "AppendMenuA", c-modifiers: "__stdcall";
end;

define inline-only C-function DeleteMenu
  parameter hMenu      :: <HMENU>;
  parameter uPosition  :: <UINT>;
  parameter uFlags     :: <UINT>;
  result value :: <BOOL>;
  c-name: "DeleteMenu", c-modifiers: "__stdcall";
end;

define inline-only C-function InsertMenuItem
  parameter hmenu1     :: <HMENU>;
  parameter uint2      :: <UINT>;
  parameter bool3      :: <BOOL>;
  parameter lpcmenuiteminfoa4 :: <LPMENUITEMINFO>;
  result value :: <BOOL>;
  c-name: "InsertMenuItemA", c-modifiers: "__stdcall";
end;

define inline-only C-function SetMenuItemInfo
  parameter hmenu1     :: <HMENU>;
  parameter uint2      :: <UINT>;
  parameter bool3      :: <BOOL>;
  parameter lpcmenuiteminfoa4 :: <LPMENUITEMINFO>;
  result value :: <BOOL>;
  c-name: "SetMenuItemInfoA", c-modifiers: "__stdcall";
end;

define inline-only C-function TabbedTextOut
  parameter hDC        :: <HDC>;
  parameter X          :: <C-int>;
  parameter Y          :: <C-int>;
  parameter lpString   :: <LPCSTR>;
  parameter nCount     :: <C-int>;
  parameter nTabPositions :: <C-int>;
  parameter lpnTabStopPositions :: <LPINT>;
  parameter nTabOrigin :: <C-int>;
  result value :: <LONG>;
  c-name: "TabbedTextOutA", c-modifiers: "__stdcall";
end;

define inline-only C-function GetTabbedTextExtent
  parameter hDC        :: <HDC>;
  parameter lpString   :: <LPCSTR>;
  parameter nCount     :: <C-int>;
  parameter nTabPositions :: <C-int>;
  parameter lpnTabStopPositions :: <LPINT>;
  result value :: <DWORD>;
  c-name: "GetTabbedTextExtentA", c-modifiers: "__stdcall";
end;

define inline-only C-function UpdateWindow
  parameter hWnd       :: <HWND>;
  result value :: <BOOL>;
  c-name: "UpdateWindow", c-modifiers: "__stdcall";
end;

define inline-only C-function GetForegroundWindow
  result value :: <HWND>;
  c-name: "GetForegroundWindow", c-modifiers: "__stdcall";
end;

define inline-only C-function SetForegroundWindow
  parameter hWnd       :: <HWND>;
  result value :: <BOOL>;
  c-name: "SetForegroundWindow", c-modifiers: "__stdcall";
end;

define inline-only C-function GetDC
  parameter hWnd       :: <HWND>;
  result value :: <HDC>;
  c-name: "GetDC", c-modifiers: "__stdcall";
end;

define inline-only C-function ReleaseDC
  parameter hWnd       :: <HWND>;
  parameter hDC        :: <HDC>;
  result value :: <C-int>;
  c-name: "ReleaseDC", c-modifiers: "__stdcall";
end;

define inline-only C-function BeginPaint
  parameter hWnd       :: <HWND>;
  parameter lpPaint    :: <LPPAINTSTRUCT>;
  result value :: <HDC>;
  c-name: "BeginPaint", c-modifiers: "__stdcall";
end;

define inline-only C-function EndPaint
  parameter hWnd       :: <HWND>;
  parameter lpPaint    ::  /* const */ <LPPAINTSTRUCT>;
  result value :: <BOOL>;
  c-name: "EndPaint", c-modifiers: "__stdcall";
end;

define inline-only C-function InvalidateRect
  parameter hWnd       :: <HWND>;
  parameter lpRect     ::  /* const */ <LPRECT>;
  parameter bErase     :: <BOOL>;
  result value :: <BOOL>;
  c-name: "InvalidateRect", c-modifiers: "__stdcall";
end;

define inline-only C-function SetWindowText
  parameter hWnd       :: <HWND>;
  parameter lpString   :: <LPCSTR>;
  result value :: <BOOL>;
  c-name: "SetWindowTextA", c-modifiers: "__stdcall";
end;

define inline-only C-function GetWindowText
  parameter hWnd       :: <HWND>;
  parameter lpString   :: <LPSTR>;
  parameter nMaxCount  :: <C-int>;
  result value :: <C-int>;
  c-name: "GetWindowTextA", c-modifiers: "__stdcall";
end;

define inline-only C-function GetClientRect
  parameter hWnd       :: <HWND>;
  parameter lpRect     :: <LPRECT>;
  result value :: <BOOL>;
  c-name: "GetClientRect", c-modifiers: "__stdcall";
end;

define inline-only C-function GetWindowRect
  parameter hWnd       :: <HWND>;
  parameter lpRect     :: <LPRECT>;
  result value :: <BOOL>;
  c-name: "GetWindowRect", c-modifiers: "__stdcall";
end;

define inline-only C-function AdjustWindowRectEx
  parameter lpRect     :: <LPRECT>;
  parameter dwStyle    :: <DWORD>;
  parameter bMenu      :: <BOOL>;
  parameter dwExStyle  :: <DWORD>;
  result value :: <BOOL>;
  c-name: "AdjustWindowRectEx", c-modifiers: "__stdcall";
end;

define inline-only C-function MessageBox
  parameter hWnd       :: <HWND>;
  parameter lpText     :: <LPCSTR>;
  parameter lpCaption  :: <LPCSTR>;
  parameter uType      :: <UINT>;
  result value :: <C-int>;
  c-name: "MessageBoxA", c-modifiers: "__stdcall";
end;

define inline-only C-function MessageBeep
  parameter uType      :: <UINT>;
  result value :: <BOOL>;
  c-name: "MessageBeep", c-modifiers: "__stdcall";
end;

define inline-only C-function ShowCursor
  parameter bShow      :: <BOOL>;
  result value :: <C-int>;
  c-name: "ShowCursor", c-modifiers: "__stdcall";
end;

define inline-only C-function SetCursorPos
  parameter X          :: <C-int>;
  parameter Y          :: <C-int>;
  result value :: <BOOL>;
  c-name: "SetCursorPos", c-modifiers: "__stdcall";
end;

define inline-only C-function SetCursor
  parameter hCursor    :: <HCURSOR>;
  result value :: <HCURSOR>;
  c-name: "SetCursor", c-modifiers: "__stdcall";
end;

define inline-only C-function GetCursorPos
  parameter lpPoint    :: <LPPOINT>;
  result value :: <BOOL>;
  c-name: "GetCursorPos", c-modifiers: "__stdcall";
end;

define inline-only C-function CreateCaret
  parameter hWnd       :: <HWND>;
  parameter hBitmap    :: <HBITMAP>;
  parameter nWidth     :: <C-int>;
  parameter nHeight    :: <C-int>;
  result value :: <BOOL>;
  c-name: "CreateCaret", c-modifiers: "__stdcall";
end;

define inline-only C-function DestroyCaret
  result value :: <BOOL>;
  c-name: "DestroyCaret", c-modifiers: "__stdcall";
end;

define inline-only C-function HideCaret
  parameter hWnd       :: <HWND>;
  result value :: <BOOL>;
  c-name: "HideCaret", c-modifiers: "__stdcall";
end;

define inline-only C-function ShowCaret
  parameter hWnd       :: <HWND>;
  result value :: <BOOL>;
  c-name: "ShowCaret", c-modifiers: "__stdcall";
end;

define inline-only C-function SetCaretPos
  parameter X          :: <C-int>;
  parameter Y          :: <C-int>;
  result value :: <BOOL>;
  c-name: "SetCaretPos", c-modifiers: "__stdcall";
end;

define inline-only C-function ClientToScreen
  parameter hWnd       :: <HWND>;
  parameter lpPoint    :: <LPPOINT>;
  result value :: <BOOL>;
  c-name: "ClientToScreen", c-modifiers: "__stdcall";
end;

define inline-only C-function ScreenToClient
  parameter hWnd       :: <HWND>;
  parameter lpPoint    :: <LPPOINT>;
  result value :: <BOOL>;
  c-name: "ScreenToClient", c-modifiers: "__stdcall";
end;

define inline-only C-function GetSysColor
  parameter nIndex     :: <C-int>;
  result value :: <DWORD>;
  c-name: "GetSysColor", c-modifiers: "__stdcall";
end;

define inline-only C-function GetWindowLong
  parameter hWnd       :: <HWND>;
  parameter nIndex     :: <C-int>;
  result value :: <LONG>;
  c-name: "GetWindowLongA", c-modifiers: "__stdcall";
end;

define inline-only C-function SetWindowLong
  parameter hWnd       :: <HWND>;
  parameter nIndex     :: <C-int>;
  parameter dwNewLong :: <C-both-long>;
  result value :: <LONG>;
  c-name: "SetWindowLongA", c-modifiers: "__stdcall";
end;

define inline-only C-function GetClassLong
  parameter hWnd       :: <HWND>;
  parameter nIndex     :: <C-int>;
  result value :: <DWORD>;
  c-name: "GetClassLongA", c-modifiers: "__stdcall";
end;

define inline-only C-function GetTopWindow
  parameter hWnd       :: <HWND>;
  result value :: <HWND>;
  c-name: "GetTopWindow", c-modifiers: "__stdcall";
end;

define inline-only C-function GetWindow
  parameter hWnd       :: <HWND>;
  parameter uCmd       :: <UINT>;
  result value :: <HWND>;
  c-name: "GetWindow", c-modifiers: "__stdcall";
end;

define inline-only C-function LoadBitmap
  parameter hInstance  :: <HINSTANCE>;
  parameter lpBitmapName :: <LPCSTR>;
  result value :: <HBITMAP>;
  c-name: "LoadBitmapA", c-modifiers: "__stdcall";
end;

define inline-only C-function LoadCursor
  parameter hInstance  :: <HINSTANCE>;
  parameter lpCursorName :: <LPCSTR>;
  result value :: <HCURSOR>;
  c-name: "LoadCursorA", c-modifiers: "__stdcall";
end;

define inline-only C-function LoadIcon
  parameter hInstance  :: <HINSTANCE>;
  parameter lpIconName :: <LPCSTR>;
  result value :: <HICON>;
  c-name: "LoadIconA", c-modifiers: "__stdcall";
end;

define inline-only C-function LoadImage
  parameter hinstance1 :: <HINSTANCE>;
  parameter lpcstr2    :: <LPCSTR>;
  parameter uint3      :: <UINT>;
  parameter int4       :: <C-int>;
  parameter int5       :: <C-int>;
  parameter uint6      :: <UINT>;
  result value :: <HANDLE>;
  c-name: "LoadImageA", c-modifiers: "__stdcall";
end;

define inline-only C-function DrawIconEx
  parameter hdc        :: <HDC>;
  parameter xLeft      :: <C-int>;
  parameter yTop       :: <C-int>;
  parameter hIcon      :: <HICON>;
  parameter cxWidth    :: <C-int>;
  parameter cyWidth    :: <C-int>;
  parameter istepIfAniCur :: <UINT>;
  parameter hbrFlickerFreeDraw :: <HBRUSH>;
  parameter diFlags    :: <UINT>;
  result value :: <BOOL>;
  c-name: "DrawIconEx", c-modifiers: "__stdcall";
end;

define inline-only C-function GetIconInfo
  parameter hIcon      :: <HICON>;
  parameter piconinfo  :: <LPICONINFO>;
  result value :: <BOOL>;
  c-name: "GetIconInfo", c-modifiers: "__stdcall";
end;

define inline-only C-function EnumResourceTypes
  parameter hModule    :: <HMODULE>;
  parameter lpEnumFunc :: <C-function-pointer>;
  parameter lParam     :: <LONG>;
  result value :: <BOOL>;
  c-name: "EnumResourceTypesA", c-modifiers: "__stdcall";
end;

define inline-only C-function EnumResourceNames
  parameter hModule    :: <HMODULE>;
  parameter lpType     :: <LPCSTR>;
  parameter lpEnumFunc :: <C-function-pointer>;
  parameter lParam     :: <LONG>;
  result value :: <BOOL>;
  c-name: "EnumResourceNamesA", c-modifiers: "__stdcall";
end;

define inline-only C-function EnumResourceLanguages
  parameter hModule    :: <HMODULE>;
  parameter lpType     :: <LPCSTR>;
  parameter lpName     :: <LPCSTR>;
  parameter lpEnumFunc :: <C-function-pointer>;
  parameter lParam     :: <LONG>;
  result value :: <BOOL>;
  c-name: "EnumResourceLanguagesA", c-modifiers: "__stdcall";
end;

define inline-only C-function FindResourceEx
  parameter hModule    :: <HMODULE>;
  parameter lpType     :: <LPCSTR>;
  parameter lpName     :: <LPCSTR>;
  parameter wLanguage  :: <WORD>;
  result value :: <HRSRC>;
  c-name: "FindResourceExA", c-modifiers: "__stdcall";
end;

define inline-only C-function LoadResource
  parameter hModule    :: <HMODULE>;
  parameter hResInfo   :: <HRSRC>;
  result value :: <HGLOBAL>;
  c-name: "LoadResource", c-modifiers: "__stdcall";
end;

define inline-only C-function SizeofResource
  parameter hModule    :: <HMODULE>;
  parameter hResInfo   :: <HRSRC>;
  result value :: <DWORD>;
  c-name: "SizeofResource", c-modifiers: "__stdcall";
end;

define inline-only C-function IsDialogMessage
  parameter hDlg       :: <HWND>;
  parameter lpMsg      :: <LPMSG>;
  result value :: <BOOL>;
  c-name: "IsDialogMessageA", c-modifiers: "__stdcall";
end;

define inline-only C-function SetScrollInfo
  parameter hwnd1      :: <HWND>;
  parameter int2       :: <C-int>;
  parameter lpcscrollinfo3 :: <LPSCROLLINFO>;
  parameter bool4      :: <BOOL>;
  result value :: <C-int>;
  c-name: "SetScrollInfo", c-modifiers: "__stdcall";
end;

define inline-only C-function WinHelp
  parameter hWndMain   :: <HWND>;
  parameter lpszHelp   :: <LPCSTR>;
  parameter uCommand   :: <UINT>;
  parameter dwData :: <C-both-unsigned-long>;
  result value :: <BOOL>;
  c-name: "WinHelpA", c-modifiers: "__stdcall";
end;

define inline function CreateDialog(hInstance, lpName, hWndParent,
                                    lpDialogFunc);
  CreateDialogParam(hInstance, lpName, hWndParent, lpDialogFunc, 0)
end CreateDialog;

define inline-only C-function CreateDialogParam
  parameter hInstance  :: <HINSTANCE>;
  parameter lpTemplateName :: <LPCSTR>;
  parameter hWndParent :: <HWND>;
  parameter lpDialogFunc :: <C-function-pointer>;
  parameter dwInitParam :: <LPARAM>;
  result value :: <HWND>;
  c-name: "CreateDialogParamA", c-modifiers: "__stdcall";
end;

define C-function GetNextWindow
  parameter hWnd       :: <HWND>;
  parameter uCmd       :: <UINT>;
  result value :: <HWND>;
  c-name: "GetWindow", c-modifiers: "__stdcall";
end;

define C-function CreateWindowEx
  parameter dwExStyle  :: <DWORD>;
  parameter lpClassName :: <LPCSTR>;
  parameter lpWindowName :: <LPCSTR>;
  parameter dwStyle    :: <C-both-int>; // was <DWORD>
  parameter X          :: <C-both-int>; // was <C-int>
  parameter Y          :: <C-both-int>;
  parameter nWidth     :: <C-both-int>;
  parameter nHeight    :: <C-both-int>;
  parameter hWndParent :: <HWND>;
  parameter hMenu      :: <HMENU>;
  parameter hInstance  :: <HINSTANCE>;
  parameter lpParam    :: <LPVOID>;
  result value :: <HWND>;
  c-name: "CreateWindowExA", c-modifiers: "__stdcall";
end;

define inline-only C-function GetOpenFileName
  parameter lpopenfilenamea1 :: <LPOPENFILENAME>;
  result value :: <BOOL>;
  c-name: "GetOpenFileNameA", c-modifiers: "__stdcall";
end;

define inline-only C-function GetSaveFileName
  parameter lpopenfilenamea1 :: <LPOPENFILENAME>;
  result value :: <BOOL>;
  c-name: "GetSaveFileNameA", c-modifiers: "__stdcall";
end;

define inline-only C-function ChooseColor
  parameter lpchoosecolora1 :: <LPCHOOSECOLOR>;
  result value :: <BOOL>;
  c-name: "ChooseColorA", c-modifiers: "__stdcall";
end;

define inline-only C-function ChooseFont
  parameter lpchoosefonta1 :: <LPCHOOSEFONT>;
  result value :: <BOOL>;
  c-name: "ChooseFontA", c-modifiers: "__stdcall";
end;

define inline-only C-function PrintDlg
  parameter lpprintdlga1 :: <LPPRINTDLG>;
  result value :: <BOOL>;
  c-name: "PrintDlgA", c-modifiers: "__stdcall";
end;

define inline-only C-function CommDlgExtendedError
  result value :: <DWORD>;
  c-name: "CommDlgExtendedError", c-modifiers: "__stdcall";
end;

define inline-only C-function InitCommonControls
  c-name: "InitCommonControls", c-modifiers: "__stdcall";
end;

define inline-only C-function ImageList-Create
  parameter cx         :: <C-int>;
  parameter cy         :: <C-int>;
  parameter flags      :: <UINT>;
  parameter cInitial   :: <C-int>;
  parameter cGrow      :: <C-int>;
  result value :: <HIMAGELIST>;
  c-name: "ImageList_Create", c-modifiers: "__stdcall";
end;

define inline-only C-function ImageList-Destroy
  parameter himl       :: <HIMAGELIST>;
  result value :: <BOOL>;
  c-name: "ImageList_Destroy", c-modifiers: "__stdcall";
end;

define inline-only C-function ImageList-Add
  parameter himl       :: <HIMAGELIST>;
  parameter hbmImage   :: <HBITMAP>;
  parameter hbmMask    :: <HBITMAP>;
  result value :: <C-int>;
  c-name: "ImageList_Add", c-modifiers: "__stdcall";
end;

define inline-only C-function ImageList-ReplaceIcon
  parameter himl       :: <HIMAGELIST>;
  parameter i          :: <C-int>;
  parameter hicon      :: <HICON>;
  result value :: <C-int>;
  c-name: "ImageList_ReplaceIcon", c-modifiers: "__stdcall";
end;

define inline-only function ImageList-AddIcon (himl, hicon);
  ImageList-ReplaceIcon(himl, -1, hicon)
end;

define C-function HtmlHelp
  parameter hwndCaller :: <HWND>;
  parameter hh/pszFile    :: <LPCSTR>;
  parameter uCommand   :: <UINT>;
  parameter dwData     :: <DWORD>;
  result value :: <HWND>;
  c-name: "HtmlHelpA", c-modifiers: "__stdcall";
end;

define inline-only C-function RegCloseKey
  parameter hKey       :: <HKEY>;
  result value :: <LONG>;
  c-name: "RegCloseKey", c-modifiers: "__stdcall";
end;

define inline-only C-function RegOpenKeyEx
  parameter hKey       :: <HKEY>;
  parameter lpSubKey   :: <LPCSTR>;
  parameter ulOptions  :: <DWORD>;
  parameter samDesired :: <DWORD>;
  output parameter phkResult :: <PHKEY>;
  result value :: <LONG>;
  c-name: "RegOpenKeyExA", c-modifiers: "__stdcall";
end;

define inline-only C-function CreateActCtx
  indirect: #t;
  parameter pActCtx :: <PCACTCTX>;
  result hActCtx :: <HANDLE>;
  c-modifiers: "__stdcall";
end;

define inline-only C-function ActivateActCtx
  indirect: #t;
  parameter hActCtx         :: <HANDLE>;
  output parameter lpCookie :: <DWORD**>;
  result value :: <BOOL>;
  c-modifiers: "__stdcall";
end;

define C-function FormatMessage
  parameter dwFlags      :: <DWORD>;
  parameter lpSource     :: <LPCVOID>;
  parameter dwMessageId  :: <DWORD>;
  parameter dwLanguageId :: <DWORD>;
  parameter lpBuffer     :: <LPSTR>;
  parameter nSize        :: <DWORD>;
  parameter _dummy       :: <C-pointer>; // va_list *Arguments
  result val :: <DWORD>;
  c-name: "FormatMessageA", c-modifiers: "__stdcall";
end;

define function win32-error-message
    (error-code :: type-union(<integer>, <machine-word>))
 => (message :: false-or(<byte-string>));
  let buf-size :: <integer> = 600;
  with-stack-structure (buffer :: <LPTSTR>, size: buf-size)
    let length :: <integer> =
      FormatMessage(logior($FORMAT-MESSAGE-FROM-SYSTEM,
                           $FORMAT-MESSAGE-IGNORE-INSERTS),
                    $NULL-VOID,
                    error-code,
                    $LANG-USER-DEFAULT,
                    buffer,
                    buf-size,
                    $NULL-VOID);
    if ( length <= 0 )
      #f
    else
      let last :: <integer> = length - 1;
      while ( last >= 0 & buffer[last] <= '\r' ) // remove trailing CRLF
        buffer[last] := '\0';
        last := last - 1;
      end while;
      as(<byte-string>, buffer)
    end if
  end with-stack-structure;
end;


define function application-instance-handle () => (hInstance :: <HINSTANCE>)
  pointer-cast(<HINSTANCE>, GetModuleHandle($NULL-string))
end function application-instance-handle;

define function application-show-window () => (nCmdShow :: <signed-int>)
  with-stack-structure (startup-info :: <LPSTARTUPINFO>)
    startup-info.dwFlags-value := 0;
    GetStartupInfo(startup-info);
    if ( ~ zero?(%logand(startup-info.dwFlags-value, $STARTF-USESHOWWINDOW)) )
      startup-info.wShowWindow-value
    else
      10                        //---*** $SW-SHOWDEFAULT
    end
  end
end function application-show-window;

