Module:    Win32-duim
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $FFFFFFFF :: <machine-word> =
  as(<machine-word>, -1); // #xFFFFFFFF

define constant <U8> = limited(<integer>, min: 0, max: #xFF);
define constant <U16> = limited(<integer>, min: 0, max: #xFFFF);
define constant <S16> = limited(<integer>, min: -#x8000, max: #x7FFF);
define constant <ambiguous-short> =	// 16 bits, signed or unsigned
  limited(<integer>, min: -#x8000, max: #xFFFF);

define constant <signed-long> =
  type-union(<integer>, <machine-word>);
define constant <unsigned-long> = 
  type-union(limited(<integer>, min: 0), <machine-word>);

define constant <signed-int> = <signed-long>;
define constant <unsigned-int> = <unsigned-long>;

define constant <C-BYTE*> = <C-unsigned-char*>;
define constant <LPARAM>  = <C-both-long>;
define constant <LRESULT> = <C-both-long>;
define constant <WPARAM>  = <C-both-unsigned-int>;
define constant <UINT>    = <C-both-unsigned-int>;

define inline constant <USHORT> = <C-unsigned-short>;
define inline constant <DWORD> = <C-both-unsigned-long>;
define C-pointer-type <DWORD*> => <DWORD>;
define C-pointer-type <DWORD**> => <DWORD*>;
define constant <BOOL> = <C-Boolean>;
define inline constant <C-BYTE> = <C-unsigned-char>;
define inline constant <WORD> = <C-unsigned-short>;
define inline constant <PBYTE> = <C-BYTE*>;
define inline constant <LPBYTE> = <C-BYTE*>;
define inline constant <LPINT> = <C-int*>;
define C-pointer-type <LPWORD> => <WORD>;
define inline constant <LPVOID> = <C-void*>;
define inline constant <LPCVOID> =  /* const */ <C-void*>;
define inline constant <INT> = <C-int>;
define inline constant <CHAR> = <C-character>;
define inline constant <SHORT> = <C-short>;
define inline constant <LONG> = <C-both-long>;
define inline constant <PWCHAR> = <C-unicode-string>;
define inline constant <LPWSTR> = <C-unicode-string>;
define inline constant <LPSTR> = <C-string>;
define inline constant <LPCSTR> =  /* const */ <C-string>;
define inline constant <PTCHAR> = <C-string>;
define inline constant <LPTSTR> = <LPSTR>;
define inline constant <LPCTSTR> = <LPCSTR>;

define inline constant <ATOM> = <WORD>;
define inline constant <HGLOBAL> = <HANDLE>;
define inline constant <COLORREF> = <DWORD>;
define C-pointer-type <LPCOLORREF> => <DWORD>;
define C-subtype <WNDPROC> ( <C-function-pointer> ) end;
define C-subtype <HIMAGELIST> ( <C-void*> ) end;
define C-subtype <HTREEITEM> ( <C-void*> ) end;
define constant <HKEY> = <HANDLE>;
define C-pointer-type <PHKEY> => <HKEY>;

// Null constants for some commonly used pointer types

define constant $NULL-HWND :: <HWND> = null-pointer( <HWND> );
define constant $NULL-RECT :: <LPRECT> = null-pointer( <LPRECT> );
define constant $NULL-POINT :: <LPPOINT> = null-pointer( <LPPOINT> );
define constant $NULL-VOID :: <C-void*> = null-pointer( <C-void*> );
define constant $NULL-string :: <C-string> = null-pointer( <C-string> );
define constant $NULL-HDC :: <HDC> = null-pointer(<HDC>);
define constant $NULL-HMENU :: <HMENU> = null-pointer(<HMENU>);
define constant $NULL-HINSTANCE :: <HINSTANCE> = null-pointer(<HINSTANCE>);

define inline-only constant $MAX-PATH                   =  260;

define generic LOWORD ( n :: <object> ) => value :: <U16>;
define generic HIWORD ( n :: <object> ) => value :: <U16>;

define inline method LOWORD ( n :: <integer> ) => value :: <U16>;
    logand(n,#xFFFF)
end LOWORD;

define inline method HIWORD ( n :: <integer> ) => value :: <U16>;
    logand( ash(n,-16), #xFFFF)  
end HIWORD;

define inline method LOWORD ( n :: <machine-word> ) => value :: <U16>;
  as(<integer>, %logand(n, as(<machine-word>, #xFFFF)))
end LOWORD;

// Not inline because of Bug 2262.
define method HIWORD ( n :: <machine-word> ) => value :: <U16>;
  as(<integer>, u%shift-right(n,16))
end HIWORD;

define method LOWORD ( n :: <double-integer> ) => value :: <U16>;
  LOWORD(%double-integer-low(n))
end LOWORD;

define method HIWORD ( n :: <double-integer> ) => value :: <U16>;
  HIWORD(%double-integer-low(n))
end HIWORD;

define sealed method MAKELONG ( wLow :: <ambiguous-short>,
			        wHigh :: <ambiguous-short> )
			=> value :: <unsigned-long>;
  let low :: <integer> = logand(wLow, #xFFFF);
  if ( wHigh > #x0FFF | wHigh < 0 )
    %logior(low, u%shift-left(as(<machine-word>, logand(wHigh, #xFFFF)), 16))
  else
    logior(low, ash(wHigh,16))
  end if
end MAKELONG;

define sealed method MAKELONG ( wLow :: <boolean>, wHigh :: <object> )
	=> value :: <unsigned-long>;
  MAKELONG( (if(wLow) 1 else 0 end if), wHigh)
end MAKELONG;

// The following is a substitute for MAKEPOINT, MAKEPOINTS, and LONG2POINT.
// Unpacks a 32-bit value into two signed 16-bit numbers
define function LPARAM-TO-XY( lparam ) => ( x :: <S16> , y :: <S16> );

  local method extend-short( u :: <U16> ) => s :: <S16>;
	  // sign-extend a 16-bit number
	  if ( logand( u, #x8000 ) = 0 )
	    u
	  else
	    logior( u, lognot(#x7FFF) );
	  end if;
	end extend-short;

  values( extend-short(LOWORD(lparam)), extend-short(HIWORD(lparam)) )
end LPARAM-TO-XY;

define inline method LOBYTE ( n :: <integer> ) => value :: <U8>;
  logand(n,#xFF)
end LOBYTE;

define inline method HIBYTE ( n :: <integer> ) => value :: <U8>;
  logand(ash(n,-8), #xFF)  
end HIBYTE;

define method LOBYTE ( n :: <machine-word> ) => value :: <U8>;
  LOBYTE(LOWORD(n))
end LOBYTE;

define method HIBYTE ( n :: <machine-word> ) => value :: <U8>;
  HIBYTE(LOWORD(n))
end HIBYTE;

define sealed method MAKEWORD ( low-byte :: <integer>, high-byte :: <integer> )
 => value :: <U16>;
  logior(logand(low-byte, #xFF), ash(logand(high-byte, #xFF),8))
end MAKEWORD;


define inline function MAKELANGID(p :: <U16>, s :: <U16>)
  => (value :: <integer>);
   logior( ash(s, 10), p)
end MAKELANGID;

define function MAKEINTRESOURCE( n :: <integer> )
	=> value :: <LPTSTR>;
  make(<LPTSTR>, address: n)
end MAKEINTRESOURCE;

define inline function RGB(red :: <U8>, green :: <U8>, blue :: <U8> )
			 => value :: <integer>;
  logior ( logior(red, ash(green,8)), ash(blue,16) )
end RGB;

define inline function GetRValue(rgb :: <integer>) => red :: <U8>;
  logand(#xFF,rgb)
end;

define inline function GetGValue(rgb :: <integer>) => green :: <U8>;
  logand(#xFF, ash(rgb,-8))
end;

define inline function GetBValue(rgb :: <integer>) => blue :: <U8>;
  logand(#xFF, ash(rgb,-16))
end;

define inline function MAKELPARAM(l, h); MAKELONG(l, h) end;



define inline-only constant $ACTCTX-FLAG-RESOURCE-NAME-VALID = #x00000008;
define inline-only constant $ACTCTX-FLAG-HMODULE-VALID       = #x00000080;
define inline-only constant $ANSI-CHARSET               =    0;
define inline-only constant $ANSI-FIXED-FONT            =   11;
define inline-only constant $ANSI-VAR-FONT              =   12;
define inline-only constant $BDR-RAISEDINNER            = #x0004;
define inline-only constant $BDR-RAISEDOUTER            = #x0001;
define inline-only constant $BDR-SUNKENINNER            = #x0008;
define inline-only constant $BDR-SUNKENOUTER            = #x0002;
define inline-only constant $BF-BOTTOM                  = #x0008;
define inline-only constant $BF-FLAT                    = #x4000;
define inline-only constant $BF-LEFT                    = #x0001;
define inline-only constant $BF-RECT                    = logior($BF-LEFT, $BF-TOP, $BF-RIGHT, $BF-BOTTOM);
define inline-only constant $BF-RIGHT                   = #x0004;
define inline-only constant $BF-TOP                     = #x0002;
define inline-only constant $BLACK-BRUSH                =    4;
define inline-only constant $BLACK-PEN                  =    7;
define inline-only constant $BLACKNESS                  = #x00000042;
define inline-only constant $BM-GETCHECK                = #x00F0;
define inline-only constant $BM-SETCHECK                = #x00F1;
define inline-only constant $BM-SETIMAGE                = #x00F7;
define inline-only constant $BM-SETSTYLE                = #x00F4;
define inline-only constant $BN-CLICKED                 =    0;
define inline-only constant $BN-DBLCLK   = $BN-DOUBLECLICKED;
define inline-only constant $BN-DOUBLECLICKED           =    5;
define inline-only constant $BS-BITMAP                  = #x00000080;
define inline-only constant $BS-CHECKBOX                = #x00000002;
define inline-only constant $BS-DEFPUSHBUTTON           = #x00000001;
define inline-only constant $BS-GROUPBOX                = #x00000007;
define inline-only constant $BS-ICON                    = #x00000040;
define inline-only constant $BS-PUSHBUTTON              = #x00000000;
define inline-only constant $BS-PUSHLIKE                = #x00001000;
define inline-only constant $BS-RADIOBUTTON             = #x00000004;
define inline-only constant $BST-CHECKED                = #x0001;
define inline-only constant $CB-ADDSTRING               = #x0143;
define inline-only constant $CB-ERR                     =   -1;
define inline-only constant $CB-GETCURSEL               = #x0147;
define inline-only constant $CB-GETDROPPEDSTATE         = #x0157;
define inline-only constant $CB-RESETCONTENT            = #x014B;
define inline-only constant $CB-SETCURSEL               = #x014E;
define inline-only constant $CB-SHOWDROPDOWN            = #x014F;
define inline-only constant $CBN-EDITCHANGE             =    5;
define inline-only constant $CBN-SELENDOK               =    9;
define inline-only constant $CBS-AUTOHSCROLL            = #x0040;
define inline-only constant $CBS-DROPDOWN               = #x0002;
define inline-only constant $CBS-DROPDOWNLIST           = #x0003;
define inline-only constant $CC-ANYCOLOR                = #x00000100;
define inline-only constant $CC-RGBINIT                 = #x00000001;
define inline-only constant $CC-SHOWHELP                = #x00000008;
define inline-only constant $CCHDEVICENAME              =   32;
define inline-only constant $CCHFORMNAME                =   32;
define inline-only constant $CF-APPLY                   = #x00000200;
define inline-only constant $CF-EFFECTS                 = #x00000100;
define inline-only constant $CF-FIXEDPITCHONLY          = #x00004000;
define inline-only constant $CF-FORCEFONTEXIST          = #x00010000;
define inline-only constant $CF-INITTOLOGFONTSTRUCT     = #x00000040;
define inline-only constant $CF-NOSCRIPTSEL             = #x00800000;
define inline-only constant $CF-SCREENFONTS             = #x00000001;
define inline-only constant $CF-SHOWHELP                = #x00000004;
define inline-only constant $CF-TEXT                    =    1;
define inline-only constant $CLIP-DEFAULT-PRECIS        =    0;
define inline-only constant $CLR-INVALID                = $FFFFFFFF;
define inline-only constant $COLOR-3DFACE = $COLOR-BTNFACE;
define inline-only constant $COLOR-3DHIGHLIGHT = $COLOR-BTNHIGHLIGHT;
define inline-only constant $COLOR-3DSHADOW = $COLOR-BTNSHADOW;
define inline-only constant $COLOR-BTNFACE              =   15;
define inline-only constant $COLOR-BTNHIGHLIGHT         =   20;
define inline-only constant $COLOR-BTNSHADOW            =   16;
define inline-only constant $COLOR-WINDOW               =    5;
define inline-only constant $COLOR-WINDOWTEXT           =    8;
define inline-only constant $CS-DBLCLKS                 = #x0008;
define inline-only constant $CS-HREDRAW                 = #x0002;
define inline-only constant $CS-OWNDC                   = #x0020;
define inline-only constant $CS-VREDRAW                 = #x0001;
define inline-only constant $CW-USEDEFAULT              = as(<machine-word>, #x80000000);
define inline-only constant $DEFAULT-CHARSET            =    1;
define inline-only constant $DEFAULT-GUI-FONT           =   17;
define inline-only constant $DEFAULT-PITCH              =    0;
define inline-only constant $DEFAULT-QUALITY            =    0;
define inline-only constant $DI-COMPAT                  = #x0004;
define inline-only constant $DLGWINDOWEXTRA             =   30;
define inline-only constant $DSTINVERT                  = #x00550009;
define inline-only constant $DS-SETFONT                 = #x40;
define inline-only constant $EDGE-BUMP                  = logior($BDR-RAISEDOUTER, $BDR-SUNKENINNER);
define inline-only constant $EDGE-ETCHED                = logior($BDR-SUNKENOUTER, $BDR-RAISEDINNER);
define inline-only constant $EDGE-RAISED                = logior($BDR-RAISEDOUTER, $BDR-RAISEDINNER);
define inline-only constant $EDGE-SUNKEN                = logior($BDR-SUNKENOUTER, $BDR-SUNKENINNER);
define inline-only constant $EM-GETSEL                  = #x00B0;
define inline-only constant $EM-SCROLLCARET             = #x00B7;
define inline-only constant $EM-SETSEL                  = #x00B1;
define inline-only constant $EN-CHANGE                  = #x0300;
define inline-only constant $EN-KILLFOCUS               = #x0200;
define inline-only constant $ERROR-SUCCESS              =    0;
define inline-only constant $ES-AUTOHSCROLL             = #x0080;
define inline-only constant $ES-MULTILINE               = #x0004;
define inline-only constant $ES-PASSWORD                = #x0020;
define inline-only constant $ES-READONLY                = #x0800;
define inline-only constant $ES-WANTRETURN              = #x1000;
define inline-only constant $FF-DECORATIVE              = ash(5,4);
define inline-only constant $FF-DONTCARE                = ash(0,4);
define inline-only constant $FF-MODERN                  = ash(3,4);
define inline-only constant $FF-ROMAN                   = ash(1,4);
define inline-only constant $FF-SCRIPT                  = ash(4,4);
define inline-only constant $FF-SWISS                   = ash(2,4);
define inline-only constant $FIXED-PITCH                =    1;
define inline-only constant $FORMAT-MESSAGE-FROM-SYSTEM = #x00001000;
define inline-only constant $FORMAT-MESSAGE-IGNORE-INSERTS = #x00000200;
define inline-only constant $GCW-ATOM                   =  -32;
define inline-only constant $GMEM-DDESHARE              = #x2000;
define inline-only constant $GMEM-MOVEABLE              = #x0002;
define inline-only constant $GW-CHILD                   =    5;
define inline-only constant $GW-HWNDNEXT                =    2;
define inline-only constant $GWL-STYLE                  =  -16;
define inline-only constant $GWL-WNDPROC                =   -4;
define inline-only constant $HELP-COMMAND               = #x0102;
define inline-only constant $HELP-CONTENTS              = #x0003;
define inline-only constant $HELP-CONTEXT               = #x0001;
define inline-only constant $HELP-CONTEXTPOPUP          = #x0008;
define inline-only constant $HELP-FINDER                = #x000b;
define inline-only constant $HELP-HELPONHELP            = #x0004;
define inline-only constant $HELP-INDEX                 = #x0003;
define inline-only constant $HELP-KEY                   = #x0101;
define inline-only constant $HELP-QUIT                  = #x0002;
define inline-only constant $HELP-SETWINPOS             = #x0203;
define inline-only constant $HH-ALINK-LOOKUP                    = #x0013;
define inline-only constant $HH-DISPLAY-TOPIC                   = #x0000;
define inline-only constant $HH-HELP-CONTEXT                    = #x000F;
define inline-only constant $HKEY-CLASSES-ROOT          = as(<HKEY>,as(<machine-word>, #x80000000));
define inline-only constant $HORZSIZE                   =    4;
define inline-only constant $HWND-BOTTOM                = make(<HWND>, address: 1);
define inline-only constant $HWND-TOP                   = make(<HWND>, address: 0);
define inline-only constant $ICON-BIG                   =    1;
define inline-only constant $ICON-SMALL                 =    0;
define inline-only constant $IDC-APPSTARTING            = MAKEINTRESOURCE(32650);
define inline-only constant $IDC-ARROW                  = MAKEINTRESOURCE(32512);
define inline-only constant $IDC-CROSS                  = MAKEINTRESOURCE(32515);
define inline-only constant $IDC-IBEAM                  = MAKEINTRESOURCE(32513);
define inline-only constant $IDC-SIZENESW               = MAKEINTRESOURCE(32643);
define inline-only constant $IDC-SIZENS                 = MAKEINTRESOURCE(32645);
define inline-only constant $IDC-SIZENWSE               = MAKEINTRESOURCE(32642);
define inline-only constant $IDC-SIZEWE                 = MAKEINTRESOURCE(32644);
define inline-only constant $IDC-WAIT                   = MAKEINTRESOURCE(32514);
define inline-only constant $IDCANCEL                   =    2;
define inline-only constant $IDHELP                     =    9;
define inline-only constant $IDI-APPLICATION            = MAKEINTRESOURCE(32512);
define inline-only constant $IDNO                       =    7;
define inline-only constant $IDOK                       =    1;
define inline-only constant $IDYES                      =    6;
define inline-only constant $ILC-COLOR8                 = #x0008;
define inline-only constant $ILC-MASK                   = #x0001;
define inline-only constant $IMAGE-BITMAP               =    0;
define inline-only constant $IMAGE-ICON                 =    1;
define inline-only constant $KEY-ENUMERATE-SUB-KEYS     = #x0008;
define inline-only constant $KEY-QUERY-VALUE            = #x0001;
define inline-only constant $LANG-NEUTRAL               = #x00;
define inline-only constant $LANG-USER-DEFAULT          = MAKELANGID($LANG-NEUTRAL,$SUBLANG-DEFAULT);
define inline-only constant $LB-ADDSTRING               = #x0180;
define inline-only constant $LB-ERR                     =   -1;
define inline-only constant $LB-GETCURSEL               = #x0188;
define inline-only constant $LB-GETSELITEMS             = #x0191;
define inline-only constant $LB-RESETCONTENT            = #x0184;
define inline-only constant $LB-SETCURSEL               = #x0186;
define inline-only constant $LB-SETSEL                  = #x0185;
define inline-only constant $LBN-DBLCLK                 =    2;
define inline-only constant $LBN-SELCANCEL              =    3;
define inline-only constant $LBN-SELCHANGE              =    1;
define inline-only constant $LBS-DISABLENOSCROLL        = #x1000;
define inline-only constant $LBS-EXTENDEDSEL            = #x0800;
define inline-only constant $LBS-NOTIFY                 = #x0001;
define inline-only constant $LF-FACESIZE                =   32;
define inline-only constant $LOGPIXELSX                 =   88;
define inline-only constant $LOGPIXELSY                 =   90;
define inline-only constant $LR-DEFAULTCOLOR            = #x0000;
define inline-only constant $LVCF-FMT                   = #x0001;
define inline-only constant $LVCF-SUBITEM               = #x0008;
define inline-only constant $LVCF-TEXT                  = #x0004;
define inline-only constant $LVCF-WIDTH                 = #x0002;
define inline-only constant $LVCFMT-CENTER              = #x0002;
define inline-only constant $LVCFMT-LEFT                = #x0000;
define inline-only constant $LVCFMT-RIGHT               = #x0001;
define inline-only constant $LVHT-ONITEM                = logior($LVHT-ONITEMICON, $LVHT-ONITEMLABEL, $LVHT-ONITEMSTATEICON);
define inline-only constant $LVHT-ONITEMICON            = #x0002;
define inline-only constant $LVHT-ONITEMLABEL           = #x0004;
define inline-only constant $LVHT-ONITEMSTATEICON       = #x0008;
define inline-only constant $LVIF-IMAGE                 = #x0002;
define inline-only constant $LVIF-STATE                 = #x0008;
define inline-only constant $LVIF-TEXT                  = #x0001;
define inline-only constant $LVIS-SELECTED              = #x0002;
define inline-only constant $LVM-DELETEALLITEMS         = #x1009;
define inline-only constant $LVM-DELETECOLUMN           = #x101C;
define inline-only constant $LVM-DELETEITEM             = #x1008;
define inline-only constant $LVM-ENSUREVISIBLE          = #x1013;
define inline-only constant $LVM-HITTEST                = #x1012;
define inline-only constant $LVM-INSERTCOLUMN           = #x101B;
define inline-only constant $LVM-INSERTITEM             = #x1007;
define inline-only constant $LVM-SETCOLUMN              = #x101A;
define inline-only constant $LVM-SETCOLUMNWIDTH         = #x101E;
define inline-only constant $LVM-SETIMAGELIST           = #x1003;
define inline-only constant $LVM-SETITEM                = #x1006;
define inline-only constant $LVM-SETITEMCOUNT           = #x102F;
define inline-only constant $LVN-COLUMNCLICK            = -108;
define inline-only constant $LVN-ITEMCHANGED            = -101;
define inline-only constant $LVN-KEYDOWN                = -155;
define inline-only constant $LVS-ICON                   = #x0000;
define inline-only constant $LVS-LIST                   = #x0003;
define inline-only constant $LVS-NOCOLUMNHEADER         = #x4000;
define inline-only constant $LVS-REPORT                 = #x0001;
define inline-only constant $LVS-SHOWSELALWAYS          = #x0008;
define inline-only constant $LVS-SINGLESEL              = #x0004;
define inline-only constant $LVS-SMALLICON              = #x0002;
define inline-only constant $LVS-TYPEMASK               = #x0003;
define inline-only constant $LVSCW-AUTOSIZE             =   -1;
define inline-only constant $LVSIL-NORMAL               =    0;
define inline-only constant $LVSIL-SMALL                =    1;
define inline-only constant $MB-APPLMODAL               = #x00000000;
define inline-only constant $MB-ICONASTERISK            = #x00000040;
define inline-only constant $MB-ICONERROR = $MB-ICONHAND;
define inline-only constant $MB-ICONEXCLAMATION         = #x00000030;
define inline-only constant $MB-ICONHAND                = #x00000010;
define inline-only constant $MB-ICONINFORMATION = $MB-ICONASTERISK;
define inline-only constant $MB-ICONQUESTION            = #x00000020;
define inline-only constant $MB-ICONSTOP = $MB-ICONHAND;
define inline-only constant $MB-ICONWARNING = $MB-ICONEXCLAMATION;
define inline-only constant $MB-OK                      = #x00000000;
define inline-only constant $MB-OKCANCEL                = #x00000001;
define inline-only constant $MB-SETFOREGROUND           = #x00010000;
define inline-only constant $MB-SYSTEMMODAL             = #x00001000;
define inline-only constant $MB-TASKMODAL               = #x00002000;
define inline-only constant $MB-YESNO                   = #x00000004;
define inline-only constant $MB-YESNOCANCEL             = #x00000003;
define inline-only constant $MERGEPAINT                 = #x00BB0226;
define inline-only constant $MF-BYCOMMAND               = #x00000000;
define inline-only constant $MF-BYPOSITION              = #x00000400;
define inline-only constant $MF-CHECKED                 = #x00000008;
define inline-only constant $MF-DEFAULT                 = #x00001000;
define inline-only constant $MF-ENABLED                 = #x00000000;
define inline-only constant $MF-GRAYED                  = #x00000001;
define inline-only constant $MF-POPUP                   = #x00000010;
define inline-only constant $MF-SEPARATOR               = #x00000800;
define inline-only constant $MF-STRING                  = #x00000000;
define inline-only constant $MF-UNCHECKED               = #x00000000;
define inline-only constant $MFS-CHECKED = $MF-CHECKED;
define inline-only constant $MFS-DEFAULT = $MF-DEFAULT;
define inline-only constant $MFS-DISABLED = $MFS-GRAYED;
define inline-only constant $MFS-ENABLED = $MF-ENABLED;
define inline-only constant $MFS-GRAYED                 = #x00000003;
define inline-only constant $MFT-RADIOCHECK             = #x00000200;
define inline-only constant $MFT-STRING  = $MF-STRING;
define inline-only constant $MIIM-DATA                  = #x00000020;
define inline-only constant $MIIM-ID                    = #x00000002;
define inline-only constant $MIIM-STATE                 = #x00000001;
define inline-only constant $MIIM-TYPE                  = #x00000010;
define inline-only constant $MK-CONTROL                 = #x0008;
define inline-only constant $MK-LBUTTON                 = #x0001;
define inline-only constant $MK-MBUTTON                 = #x0010;
define inline-only constant $MK-RBUTTON                 = #x0002;
define inline-only constant $MK-SHIFT                   = #x0004;
define inline-only constant $NM-DBLCLK                  =   -3;
define inline-only constant $NM-RCLICK                  =   -5;
define inline-only constant $NOTSRCCOPY                 = #x00330008;
define inline-only constant $NOTSRCERASE                = #x001100A6;
define inline-only constant $NULL-BRUSH                 =    5;
define inline-only constant $NULL-PEN                   =    8;
define inline-only constant $OEM-CHARSET                =  255;
define inline-only constant $OFN-ALLOWMULTISELECT       = #x00000200;
define inline-only constant $OFN-EXPLORER               = #x00080000;
define inline-only constant $OFN-FILEMUSTEXIST          = #x00001000;
define inline-only constant $OFN-HIDEREADONLY           = #x00000004;
define inline-only constant $OFN-OVERWRITEPROMPT        = #x00000002;
define inline-only constant $OUT-TT-PRECIS              =    4;
define inline-only constant $PBM-SETPOS                 = #x402;
define inline-only constant $PBM-SETRANGE               = #x401;
define inline-only constant $PD-ALLPAGES                = #x00000000;
define inline-only constant $PD-COLLATE                 = #x00000010;
define inline-only constant $PD-PRINTSETUP              = #x00000040;
define inline-only constant $PD-PRINTTOFILE             = #x00000020;
define inline-only constant $PD-SHOWHELP                = #x00000800;
define inline-only constant $PD-USEDEVMODECOPIES        = #x00040000;
define inline-only constant $PS-DASH                    =    1;
define inline-only constant $PS-DASHDOT                 =    3;
define inline-only constant $PS-DASHDOTDOT              =    4;
define inline-only constant $PS-DOT                     =    2;
define inline-only constant $PS-SOLID                   =    0;
define inline-only constant $R2-BLACK                   =    1;
define inline-only constant $R2-COPYPEN                 =   13;
define inline-only constant $R2-MASKNOTPEN              =    3;
define inline-only constant $R2-MASKPEN                 =    9;
define inline-only constant $R2-MASKPENNOT              =    5;
define inline-only constant $R2-MERGENOTPEN             =   12;
define inline-only constant $R2-MERGEPEN                =   15;
define inline-only constant $R2-MERGEPENNOT             =   14;
define inline-only constant $R2-NOP                     =   11;
define inline-only constant $R2-NOT                     =    6;
define inline-only constant $R2-NOTCOPYPEN              =    4;
define inline-only constant $R2-NOTMASKPEN              =    8;
define inline-only constant $R2-NOTMERGEPEN             =    2;
define inline-only constant $R2-NOTXORPEN               =   10;
define inline-only constant $R2-WHITE                   =   16;
define inline-only constant $R2-XORPEN                  =    7;
define inline-only constant $RGN-AND                    =    1;
define inline-only constant $RGN-COPY                   =    5;
define inline-only constant $RGN-DIFF                   =    4;
define inline-only constant $RGN-OR                     =    2;
define inline-only constant $RGN-XOR                    =    3;
define inline-only constant $RT-BITMAP                  = MAKEINTRESOURCE(2);
define inline-only constant $RT-CURSOR                  = MAKEINTRESOURCE(1);
define inline-only constant $RT-DIALOG                  = MAKEINTRESOURCE(5);
define inline-only constant $RT-ICON                    = MAKEINTRESOURCE(3);
define inline-only constant $SB-BOTTOM                  =    7;
define inline-only constant $SB-CTL                     =    2;
define inline-only constant $SB-ENDSCROLL               =    8;
define inline-only constant $SB-LINEDOWN                =    1;
define inline-only constant $SB-LINEUP                  =    0;
define inline-only constant $SB-PAGEDOWN                =    3;
define inline-only constant $SB-PAGEUP                  =    2;
define inline-only constant $SB-SETMINHEIGHT            = #x408;
define inline-only constant $SB-SETPARTS                = #x404;
define inline-only constant $SB-SETTEXT                = #x401;
define inline-only constant $SB-THUMBPOSITION           =    4;
define inline-only constant $SB-THUMBTRACK              =    5;
define inline-only constant $SB-TOP                     =    6;
define inline-only constant $SBARS-SIZEGRIP             = #x0100;
define inline-only constant $SBS-HORZ                   = #x0000;
define inline-only constant $SBS-VERT                   = #x0001;
define inline-only constant $SBT-NOBORDERS              = #x0100;
define inline-only constant $SIF-ALL                    = logior($SIF-RANGE, $SIF-PAGE, $SIF-POS, $SIF-TRACKPOS);
define inline-only constant $SIF-PAGE                   = #x0002;
define inline-only constant $SIF-POS                    = #x0004;
define inline-only constant $SIF-RANGE                  = #x0001;
define inline-only constant $SIF-TRACKPOS               = #x0010;
define inline-only constant $SIZE-MAXIMIZED             =    2;
define inline-only constant $SIZE-MINIMIZED             =    1;
define inline-only constant $SIZE-RESTORED              =    0;
define inline-only constant $SM-CMOUSEBUTTONS           =   43;
define inline-only constant $SM-CXFULLSCREEN            =   16;
define inline-only constant $SM-CXHSCROLL               =   21;
define inline-only constant $SM-CXICON                  =   11;
define inline-only constant $SM-CXSCREEN                =    0;
define inline-only constant $SM-CXSMICON                =   49;
define inline-only constant $SM-CXVSCROLL               =    2;
define inline-only constant $SM-CYFULLSCREEN            =   17;
define inline-only constant $SM-CYHSCROLL               =    3;
define inline-only constant $SM-CYICON                  =   12;
define inline-only constant $SM-CYMENU                  =   15;
define inline-only constant $SM-CYSCREEN                =    1;
define inline-only constant $SM-CYSMICON                =   50;
define inline-only constant $SM-CYVSCROLL               =   20;
define inline-only constant $SRCAND                     = #x008800C6;
define inline-only constant $SRCCOPY                    = #x00CC0020;
define inline-only constant $SRCERASE                   = #x00440328;
define inline-only constant $SRCINVERT                  = #x00660046;
define inline-only constant $SRCPAINT                   = #x00EE0086;
define inline-only constant $SS-BITMAP                  = #x0000000E;
define inline-only constant $SS-ICON                    = #x00000003;
define inline-only constant $SS-LEFTNOWORDWRAP          = #x0000000C;
define inline-only constant $STARTF-USESHOWWINDOW       = #x00000001;
define inline-only constant $STM-SETIMAGE               = #x0172;
define inline-only constant $SUBLANG-DEFAULT            = #x01;
define inline-only constant $SW-HIDE                    =    0;
define inline-only constant $SW-MAXIMIZE                =    3;
define inline-only constant $SW-MINIMIZE                =    6;
define inline-only constant $SW-RESTORE                 =    9;
define inline-only constant $SW-SHOW                    =    5;
define inline-only constant $SW-SHOWNORMAL              =    1;
define inline-only constant $SWP-NOACTIVATE             = #x0010;
define inline-only constant $SWP-NOMOVE                 = #x0002;
define inline-only constant $SWP-NOSIZE                 = #x0001;
define inline-only constant $SWP-NOZORDER               = #x0004;
define inline-only constant $SYMBOL-CHARSET             =    2;
define inline-only constant $SYSTEM-FONT                =   13;
define inline-only constant $TA-BASELINE                =   24;
define inline-only constant $TA-BOTTOM                  =    8;
define inline-only constant $TA-CENTER                  =    6;
define inline-only constant $TA-LEFT                    =    0;
define inline-only constant $TA-RIGHT                   =    2;
define inline-only constant $TA-TOP                     =    0;
define inline-only constant $TB-BOTTOM                  =    7;
define inline-only constant $TB-LINEDOWN                =    1;
define inline-only constant $TB-LINEUP                  =    0;
define inline-only constant $TB-PAGEDOWN                =    3;
define inline-only constant $TB-PAGEUP                  =    2;
define inline-only constant $TB-THUMBPOSITION           =    4;
define inline-only constant $TB-THUMBTRACK              =    5;
define inline-only constant $TB-TOP                     =    6;
define inline-only constant $TBM-SETPOS                 = #x405;
define inline-only constant $TBM-SETRANGE               = #x406;
define inline-only constant $TBM-SETTICFREQ             = #x414;
define inline-only constant $TBS-AUTOTICKS              = #x0001;
define inline-only constant $TBS-HORZ                   = #x0000;
define inline-only constant $TBS-VERT                   = #x0002;
define inline-only constant $TCIF-TEXT                  = #x0001;
define inline-only constant $TCM-ADJUSTRECT             = #x1328;
define inline-only constant $TCM-DELETEALLITEMS         = #x1309;
define inline-only constant $TCM-GETCURSEL              = #x130B;
define inline-only constant $TCM-GETITEMRECT            = #x130A;
define inline-only constant $TCM-INSERTITEM             = #x1307;
define inline-only constant $TCM-SETCURSEL              = #x130C;
define inline-only constant $TCN-KEYDOWN                = -550;
define inline-only constant $TCN-SELCHANGE              = -551;
define inline-only constant $TPM-BOTTOMALIGN            = #x0020;
define inline-only constant $TPM-CENTERALIGN            = #x0004;
define inline-only constant $TPM-LEFTALIGN              = #x0000;
define inline-only constant $TPM-NONOTIFY               = #x0080;
define inline-only constant $TPM-RETURNCMD              = #x0100;
define inline-only constant $TPM-RIGHTALIGN             = #x0008;
define inline-only constant $TPM-RIGHTBUTTON            = #x0002;
define inline-only constant $TPM-TOPALIGN               = #x0000;
define inline-only constant $TPM-VCENTERALIGN           = #x0010;
define inline-only constant $TRANSPARENT                =    1;
define inline-only constant $TTF-IDISHWND               = #x0001;
define inline-only constant $TTF-SUBCLASS               = #x0010;
define inline-only constant $TTM-ACTIVATE               = #x401;
define inline-only constant $TTM-ADDTOOL                = #x404;
define inline-only constant $TTM-DELTOOL                = #x405;
define inline-only constant $TTN-GETDISPINFO           = -520;
define inline-only constant $TTN-NEEDTEXT = $TTN-GETDISPINFO;
define inline-only constant $TTS-ALWAYSTIP              = #x01;
define inline-only constant $TTS-NOPREFIX               = #x02;
define inline-only constant $TVE-COLLAPSE               = #x0001;
define inline-only constant $TVE-COLLAPSERESET          = #x8000;
define inline-only constant $TVE-EXPAND                 = #x0002;
define inline-only constant $TVE-TOGGLE                 = #x0003;
define inline-only constant $TVGN-CARET                 = #x0009;
define inline-only constant $TVHT-ONITEM                = logior($TVHT-ONITEMICON, $TVHT-ONITEMLABEL, $TVHT-ONITEMSTATEICON);
define inline-only constant $TVHT-ONITEMBUTTON          = #x0010;
define inline-only constant $TVHT-ONITEMICON            = #x0002;
define inline-only constant $TVHT-ONITEMLABEL           = #x0004;
define inline-only constant $TVHT-ONITEMSTATEICON       = #x0040;
define inline-only constant $TVI-LAST                   = make(<HTREEITEM>, address: as(<machine-word>, #xFFFF0002));
define inline-only constant $TVI-ROOT                   = make(<HTREEITEM>, address: as(<machine-word>, #xFFFF0000));
define inline-only constant $TVIF-CHILDREN              = #x0040;
define inline-only constant $TVIF-IMAGE                 = #x0002;
define inline-only constant $TVIF-SELECTEDIMAGE         = #x0020;
define inline-only constant $TVIF-STATE                 = #x0008;
define inline-only constant $TVIF-TEXT                  = #x0001;
define inline-only constant $TVIS-SELECTED              = #x0002;
define inline-only constant $TVM-DELETEITEM             = #x1101;
define inline-only constant $TVM-ENSUREVISIBLE          = #x1114;
define inline-only constant $TVM-EXPAND                 = #x1102;
define inline-only constant $TVM-HITTEST                = #x1111;
define inline-only constant $TVM-INSERTITEM             = #x1100;
define inline-only constant $TVM-SELECTITEM             = #x110B;
define inline-only constant $TVM-SETIMAGELIST           = #x1109;
define inline-only constant $TVM-SETITEM                = #x110D;
define inline-only constant $TVN-GETDISPINFO            = -403;
define inline-only constant $TVN-ITEMEXPANDINGA         = -405;
define inline-only constant $TVN-ITEMEXPANDINGW         = -454;
define inline-only constant $TVN-KEYDOWN                = -412;
define inline-only constant $TVN-SELCHANGEDA            = -402;
define inline-only constant $TVN-SELCHANGEDW            = -451;
define inline-only constant $TVS-HASBUTTONS             = #x0001;
define inline-only constant $TVS-HASLINES               = #x0002;
define inline-only constant $TVS-LINESATROOT            = #x0004;
define inline-only constant $TVS-SHOWSELALWAYS          = #x0020;
define inline-only constant $TVSIL-NORMAL               =    0;
define inline-only constant $UDM-GETPOS                 = #x468;
define inline-only constant $UDM-SETPOS                 = #x467;
define inline-only constant $UDM-SETRANGE               = #x465;
define inline-only constant $UDS-ARROWKEYS              = #x0020;
define inline-only constant $UDS-AUTOBUDDY              = #x0010;
define inline-only constant $UDS-HORZ                   = #x0040;
define inline-only constant $UDS-WRAP                   = #x0001;
define inline-only constant $VARIABLE-PITCH             =    2;
define inline-only constant $VER-PLATFORM-WIN32-NT      =    2;
define inline-only constant $VER-PLATFORM-WIN32-WINDOWS =    1;
define inline-only constant $VER-PLATFORM-WIN32s        =    0;
define inline-only constant $VERTSIZE                   =    6;
define inline-only constant $VK-ADD                     = #x6B;
define inline-only constant $VK-APPS                    = #x5D;
define inline-only constant $VK-BACK                    = #x08;
define inline-only constant $VK-CANCEL                  = #x03;
define inline-only constant $VK-CAPITAL                 = #x14;
define inline-only constant $VK-CLEAR                   = #x0C;
define inline-only constant $VK-CONTROL                 = #x11;
define inline-only constant $VK-DECIMAL                 = #x6E;
define inline-only constant $VK-DELETE                  = #x2E;
define inline-only constant $VK-DIVIDE                  = #x6F;
define inline-only constant $VK-DOWN                    = #x28;
define inline-only constant $VK-END                     = #x23;
define inline-only constant $VK-ESCAPE                  = #x1B;
define inline-only constant $VK-EXECUTE                 = #x2B;
define inline-only constant $VK-F1                      = #x70;
define inline-only constant $VK-F10                     = #x79;
define inline-only constant $VK-F11                     = #x7A;
define inline-only constant $VK-F12                     = #x7B;
define inline-only constant $VK-F13                     = #x7C;
define inline-only constant $VK-F14                     = #x7D;
define inline-only constant $VK-F15                     = #x7E;
define inline-only constant $VK-F16                     = #x7F;
define inline-only constant $VK-F17                     = #x80;
define inline-only constant $VK-F18                     = #x81;
define inline-only constant $VK-F19                     = #x82;
define inline-only constant $VK-F2                      = #x71;
define inline-only constant $VK-F20                     = #x83;
define inline-only constant $VK-F21                     = #x84;
define inline-only constant $VK-F22                     = #x85;
define inline-only constant $VK-F23                     = #x86;
define inline-only constant $VK-F24                     = #x87;
define inline-only constant $VK-F3                      = #x72;
define inline-only constant $VK-F4                      = #x73;
define inline-only constant $VK-F5                      = #x74;
define inline-only constant $VK-F6                      = #x75;
define inline-only constant $VK-F7                      = #x76;
define inline-only constant $VK-F8                      = #x77;
define inline-only constant $VK-F9                      = #x78;
define inline-only constant $VK-HELP                    = #x2F;
define inline-only constant $VK-HOME                    = #x24;
define inline-only constant $VK-INSERT                  = #x2D;
define inline-only constant $VK-LCONTROL                = #xA2;
define inline-only constant $VK-LEFT                    = #x25;
define inline-only constant $VK-LWIN                    = #x5B;
define inline-only constant $VK-MENU                    = #x12;
define inline-only constant $VK-MULTIPLY                = #x6A;
define inline-only constant $VK-NEXT                    = #x22;
define inline-only constant $VK-NUMLOCK                 = #x90;
define inline-only constant $VK-NUMPAD0                 = #x60;
define inline-only constant $VK-NUMPAD1                 = #x61;
define inline-only constant $VK-NUMPAD2                 = #x62;
define inline-only constant $VK-NUMPAD3                 = #x63;
define inline-only constant $VK-NUMPAD4                 = #x64;
define inline-only constant $VK-NUMPAD5                 = #x65;
define inline-only constant $VK-NUMPAD6                 = #x66;
define inline-only constant $VK-NUMPAD7                 = #x67;
define inline-only constant $VK-NUMPAD8                 = #x68;
define inline-only constant $VK-NUMPAD9                 = #x69;
define inline-only constant $VK-PAUSE                   = #x13;
define inline-only constant $VK-PRINT                   = #x2A;
define inline-only constant $VK-PRIOR                   = #x21;
define inline-only constant $VK-RETURN                  = #x0D;
define inline-only constant $VK-RIGHT                   = #x27;
define inline-only constant $VK-RMENU                   = #xA5;
define inline-only constant $VK-RWIN                    = #x5C;
define inline-only constant $VK-SCROLL                  = #x91;
define inline-only constant $VK-SELECT                  = #x29;
define inline-only constant $VK-SEPARATOR               = #x6C;
define inline-only constant $VK-SHIFT                   = #x10;
define inline-only constant $VK-SNAPSHOT                = #x2C;
define inline-only constant $VK-SPACE                   = #x20;
define inline-only constant $VK-SUBTRACT                = #x6D;
define inline-only constant $VK-TAB                     = #x09;
define inline-only constant $VK-UP                      = #x26;
define inline-only constant $WA-INACTIVE                =    0;
define inline-only constant $WHITE-BRUSH                =    0;
define inline-only constant $WHITE-PEN                  =    6;
define inline-only constant $WHITENESS                  = #x00FF0062;
define inline-only constant $WM-ACTIVATE                = #x0006;
define inline-only constant $WM-CHAR                    = #x0102;
define inline-only constant $WM-CLOSE                   = #x0010;
define inline-only constant $WM-COMMAND                 = #x0111;
define inline-only constant $WM-DESTROY                 = #x0002;
define inline-only constant $WM-ENABLE                  = #x000A;
define inline-only constant $WM-ERASEBKGND              = #x0014;
define inline-only constant $WM-GETMINMAXINFO           = #x0024;
define inline-only constant $WM-GETTEXTLENGTH           = #x000E;
define inline-only constant $WM-HOTKEY                  = #x0312;
define inline-only constant $WM-HSCROLL                 = #x0114;
define inline-only constant $WM-INITMENUPOPUP           = #x0117;
define inline-only constant $WM-KEYDOWN                 = #x0100;
define inline-only constant $WM-KEYUP                   = #x0101;
define inline-only constant $WM-KILLFOCUS               = #x0008;
define inline-only constant $WM-LBUTTONDBLCLK           = #x0203;
define inline-only constant $WM-LBUTTONDOWN             = #x0201;
define inline-only constant $WM-LBUTTONUP               = #x0202;
define inline-only constant $WM-MBUTTONDBLCLK           = #x0209;
define inline-only constant $WM-MBUTTONDOWN             = #x0207;
define inline-only constant $WM-MBUTTONUP               = #x0208;
define inline-only constant $WM-MENUSELECT              = #x011F;
define inline-only constant $WM-MOUSEMOVE               = #x0200;
define inline-only constant $WM-MOVE                    = #x0003;
define inline-only constant $WM-NOTIFY                  = #x004E;
define inline-only constant $WM-NULL                    = #x0000;
define inline-only constant $WM-PAINT                   = #x000F;
define inline-only constant $WM-RBUTTONDBLCLK           = #x0206;
define inline-only constant $WM-RBUTTONDOWN             = #x0204;
define inline-only constant $WM-RBUTTONUP               = #x0205;
define inline-only constant $WM-SETCURSOR               = #x0020;
define inline-only constant $WM-SETFOCUS                = #x0007;
define inline-only constant $WM-SETFONT                 = #x0030;
define inline-only constant $WM-SETICON                 = #x0080;
define inline-only constant $WM-SETREDRAW               = #x000B;
define inline-only constant $WM-SIZE                    = #x0005;
define inline-only constant $WM-SYSCHAR                 = #x0106;
define inline-only constant $WM-SYSKEYDOWN              = #x0104;
define inline-only constant $WM-SYSKEYUP                = #x0105;
define inline-only constant $WM-USER                    = #x0400;
define inline-only constant $WM-VSCROLL                 = #x0115;
define inline-only constant $WS-CAPTION                 = #x00C00000;
define inline-only constant $WS-CHILD                   = as(<machine-word>, #x40000000);
define inline-only constant $WS-CLIPSIBLINGS            = #x04000000;
define inline-only constant $WS-DISABLED                = #x08000000;
define inline-only constant $WS-EX-CLIENTEDGE           = #x00000200;
define inline-only constant $WS-EX-CONTROLPARENT        = #x00010000;
define inline-only constant $WS-EX-DLGMODALFRAME        = #x00000001;
define inline-only constant $WS-EX-NOPARENTNOTIFY       = #x00000004;
define inline-only constant $WS-EX-TOPMOST              = #x00000008;
define inline-only constant $WS-GROUP                   = #x00020000;
define inline-only constant $WS-HSCROLL                 = #x00100000;
define inline-only constant $WS-MAXIMIZE                = #x01000000;
define inline-only constant $WS-MAXIMIZEBOX             = #x00010000;
define inline-only constant $WS-MINIMIZE                = as(<machine-word>, #x20000000);
define inline-only constant $WS-ICONIC   = $WS-MINIMIZE;
define inline-only constant $WS-MINIMIZEBOX             = #x00020000;
define inline-only constant $WS-OVERLAPPED              = #x00000000;
define inline-only constant $WS-OVERLAPPEDWINDOW        = logior($WS-OVERLAPPED, $WS-CAPTION, $WS-SYSMENU, $WS-THICKFRAME, $WS-MINIMIZEBOX, $WS-MAXIMIZEBOX);
define inline-only constant $WS-SIZEBOX  = $WS-THICKFRAME;
define inline-only constant $WS-SYSMENU                 = #x00080000;
define inline-only constant $WS-TABSTOP                 = #x00010000;
define inline-only constant $WS-THICKFRAME              = #x00040000;
define inline-only constant $WS-VSCROLL                 = #x00200000;


define constant $STATUSCLASSNAME = "msctls_statusbar32";
define constant $TRACKBAR-CLASS = "msctls_trackbar32";
define constant $PROGRESS-CLASS = "msctls_progress32";
define constant $UPDOWN-CLASS  = "msctls_updown32";
define constant $WC-LISTVIEW   = "SysListView32";
define constant $WC-TABCONTROL = "SysTabControl32";
define constant $WC-TREEVIEW   = "SysTreeView32";
define constant $TOOLTIPS-CLASS = "tooltips_class32";

