Module:       windows-viewer
Author:       Andy Armstrong
Synopsis:     Windows viewer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $desktop-window = GetDesktopWindow();

/// Basic information

define method window-id
    (handle :: <HWND>)
 => (id)
  GetWindowLong(handle, $GWL-ID)
end method window-id;

define method window-valid?
    (handle :: <HWND>) => (valid? :: <boolean>)
  ~null-handle?(handle) & IsWindow(handle)
end method window-valid?;

define method ensure-window-valid
    (handle :: <HWND>) => (handle :: false-or(<HWND>))
  window-valid?(handle) & handle
end method ensure-window-valid;


/// Class name

define method window-class-name
    (handle :: <HWND>) => (name :: <string>)
  let length = 256;
  let buffer-size = length + 1;
  with-stack-structure (buffer :: <C-string>, size: buffer-size)
    let actual-length = GetClassName(handle, buffer, buffer-size);
    when (actual-length = 0) ensure-no-win32-error("GetWindowText") end;
    as(<byte-string>, copy-sequence(buffer, end: actual-length))
  end
end method window-class-name;

define method window-class-description
    (handle :: <HWND>) => (description :: <string>)
  let class = window-class-name(handle);
  let (style, ext-style) = window-raw-styles(handle);
  get-window-class-description
    (handle, class, as(<symbol>, class), style, ext-style)
end method window-class-description;

define method get-window-class-description
    (handle :: <HWND>, name :: <string>, class :: <symbol>,
     style, ext-style)
 => (description :: <string>)
  name
end method get-window-class-description;


/// Window label

define method window-label
    (handle :: <HWND>) => (title :: false-or(<string>))
  if (handle == $desktop-window)
    "Desktop"
  else
    if (window-valid?(handle))
      let length = SendMessage(handle, $WM-GETTEXTLENGTH, 0, 0);
      if (length > 0)
	let buffer-size = length + 1;
	with-stack-structure (buffer :: <C-string>, size: buffer-size)
	  let actual-length = GetWindowText(handle, buffer, buffer-size);
	  when (actual-length = 0) ensure-no-win32-error("GetWindowText") end;
	  as(<byte-string>, buffer)
	end
      end
    end
  end
end method window-label;

define method window-name
    (handle :: <HWND>) => (name :: <string>)
  if (window-valid?)
    let label = window-label(handle);
    if (label & ~empty?(label))
      label
    else
      format-to-string
	("%s: %s",
	 window-class-description(handle),
	 number-to-hex-string(handle.pointer-address))
    end
  else
    "[destroyed]"
  end
end method window-name;


/// Window children

define variable *parent* :: false-or(<HWND>) = #f;
define constant $children :: <stretchy-vector> = make(<stretchy-vector>);

define macro <WNDENUMPROC>-callback-wrapper
  { <WNDENUMPROC>-callback-wrapper(?new:name,?old:name) } =>
    { define C-callable-wrapper ?new of ?old
	parameter hWnd :: <HWND>;
	parameter lParam :: <LPARAM>;
	result    value :: <BOOL>;
	c-modifiers: "__stdcall";
      end C-callable-wrapper }
end;

define method enum-children
    (handle :: <HWND>, lParam)
 => (value :: <boolean>)
  ignore(lParam);
//  if (window-parent(handle) = *parent*)
    add!($children, handle);
//  else
//    debug-message("Non-child found during EnumChildWindows: %= [%=]",
//		  window-name(handle), handle.pointer-address)
//  end;
  #t
end method enum-children;

define callback WndEnum-Proc :: <WNDENUMPROC> = enum-children;

define method window-children
    (handle :: <HWND>) => (children :: <sequence>)
  $children.size := 0;
  *parent* := handle;
  if (window-valid?(handle))
    if (handle == $desktop-window)
      EnumWindows(WndEnum-Proc, 0)
    else
      EnumChildWindows(handle, WndEnum-Proc, 0)
    end
  end;
  as(<simple-object-vector>, $children)
end method window-children;

define method window-has-children?
    (handle :: <HWND>) => (has-children? :: <boolean>)
  ~null-handle?(GetWindow(handle, $GW-CHILD))
end method window-has-children?;

define method window-owner
    (handle :: <HWND>) => (parent :: false-or(<HWND>))
  ensure-window-valid(GetWindow(handle, $GW-OWNER))
end method window-owner;

define method window-parent
    (handle :: <HWND>) => (parent :: false-or(<HWND>))
  ensure-window-valid(GetParent(handle))
end method window-parent;

define method window-next-window
    (handle :: <HWND>) => (child :: false-or(<HWND>))
  ensure-window-valid(GetNextWindow(handle, $GW-HWNDNEXT))
end method window-next-window;

define method window-previous-window
    (handle :: <HWND>) => (child :: false-or(<HWND>))
  ensure-window-valid(GetNextWindow(handle, $GW-HWNDPREV))
end method window-previous-window;

define method window-top-child
    (handle :: <HWND>) => (child :: false-or(<HWND>))
  ensure-window-valid(GetTopWindow(handle))
end method window-top-child;


/// Window geometry

define sealed method get-window-edges
    (handle :: <HWND>)
 => (left :: <integer>, top :: <integer>,
     right :: <integer>, bottom :: <integer>)
  with-stack-structure (rect :: <LPRECT>)
    GetWindowRect(handle, rect);
    let left   = rect.left-value;
    let top    = rect.top-value;
    let right  = rect.right-value;
    let bottom = rect.bottom-value;
    values(left, top, right, bottom)
  end
end method get-window-edges;

define sealed method get-client-edges
    (handle :: <HWND>)
 => (left :: <integer>, top :: <integer>,
     right :: <integer>, bottom :: <integer>)
  with-stack-structure (rect :: <LPRECT>)
    GetClientRect(handle, rect);
    let left   = rect.left-value;
    let top    = rect.top-value;
    let right  = rect.right-value;
    let bottom = rect.bottom-value;
    values(left, top, right, bottom)
  end
end method get-client-edges;



/// Menu handling

/*---*** Not ready yet...
define class <menu-item> (<object>)
  constant slot menu-item-label :: <string>,
    required-init-keyword: label:;
  constant slot menu-item-enabled? :: <boolean>,
    required-init-keyword: enabled?:;
  constant slot menu-item-checked? :: <boolean>,
    required-init-keyword: checked?:;
end class <menu-item>;

define method window-menu-bar
    (handle :: <HWND>) => (menu-bar :: false-or(<HMENU>))
  let menu = GetMenu(handle);
  ~null-handle?(menu) & menu
end method window-menu-bar;

define method menu-items
    (handle :: <HMENU>) => (items :: <sequence>)
  let count :: <integer> = GetMenuItemCount(handle);
  let items :: <vector> = make(<vector>, size: count);
  with-stack-structure (item-info :: <LPMENUITEMINFO>)
    for (index :: <integer> from 0 below count)
      GetMenuItemInfo(handle, index, 1, item-info);
      let flags = item-info.fState-value;
      items[index] 
	:= make(<menu-item>,
		label:    "Unknown",
		enabled?: #f,
		chcked?:  #f)
    end
  end;
  items
end method menu-items;
*/


/// Styles

define constant $basic-styles
  = vector(pair($WS-BORDER,           "WS_BORDER"),
	   pair($WS-CAPTION,          "WS_CAPTION"),
	   pair($WS-CHILD,            "WS_CHILD"),
	   pair($WS-CHILDWINDOW,      "WS_CHILDWINDOW"),
	   pair($WS-CLIPCHILDREN,     "WS_CLIPCHILDREN"),
	   pair($WS-CLIPSIBLINGS,     "WS_CLIPSIBLINGS"),
	   pair($WS-DISABLED,         "WS_DISABLED"),
	   pair($WS-DLGFRAME,         "WS_DLGFRAME"),
	   pair($WS-GROUP,            "WS_GROUP"),
	   pair($WS-HSCROLL,          "WS_HSCROLL"),
	   pair($WS-ICONIC,           "WS_ICONIC"),
	   pair($WS-MAXIMIZE,         "WS_MAXIMIZE"),
	   pair($WS-MAXIMIZEBOX,      "WS_MAXIMIZEBOX"),
	   pair($WS-MINIMIZE,         "WS_MINIMIZE"),
	   pair($WS-MINIMIZEBOX,      "WS_MINIMIZEBOX"),
	   pair($WS-MAXIMIZE,         "WS_MAXIMIZE"),
	   pair($WS-OVERLAPPED,       "WS_OVERLAPPED"),
	   pair($WS-OVERLAPPEDWINDOW, "WS_OVERLAPPEDWINDOW"),
	   pair($WS-POPUP,            "WS_POPUP"),
	   pair($WS-POPUPWINDOW,      "WS_POPUPWINDOW"),
	   pair($WS-SIZEBOX,          "WS_SIZEBOX"),
	   pair($WS-SYSMENU,          "WS_SYSMENU"),
	   pair($WS-TABSTOP,          "WS_TABSTOP"),
	   pair($WS-THICKFRAME,       "WS_THICKFRAME"),
	   pair($WS-TILED,            "WS_TILED"),
	   pair($WS-TILEDWINDOW,      "WS_TILEDWINDOW"),
	   pair($WS-VISIBLE,          "WS_VISIBLE"),
	   pair($WS-VSCROLL,          "WS_VSCROLL"));

define constant $extended-styles
  = vector(pair($WS-EX-ACCEPTFILES,      "WS_EX_ACCEPTFILES"),
	   pair($WS-EX-APPWINDOW,        "WS_EX_APPWINDOW"),
	   pair($WS-EX-CLIENTEDGE,       "WS_EX_CLIENTEDGE"),
	   pair($WS-EX-CONTEXTHELP,      "WS_EX_CONTEXTHELP"),
	   pair($WS-EX-CONTROLPARENT,    "WS_EX_CONTROLPARENT"),
	   pair($WS-EX-DLGMODALFRAME,    "WS_EX_DLGMODALFRAME"),
	   // NT 5.0 options
	   // pair($WS-EX-LAYERED,          "WS_EX_LAYERED"),
	   // pair($WS-EX-LAYOUTRTL,        "WS_EX_LAYOUTRTL"),
	   pair($WS-EX-LEFT,             "WS_EX_LEFT"),
	   pair($WS-EX-LEFTSCROLLBAR,    "WS_EX_LEFTSCROLLBAR"),
	   pair($WS-EX-LTRREADING,       "WS_EX_LTRREADING"),
	   pair($WS-EX-MDICHILD,         "WS_EX_MDICHILD"),
	   // NT 5.0 options
	   // pair($WS-EX-NOACTIVATE,        "WS_EX_NOACTIVATE"),
	   // pair($WS-EX-NOINHERITLAYOUT,  "WS_EX_NOINHERITLAYOUT"),
	   pair($WS-EX-NOPARENTNOTIFY,   "WS_EX_NOPARENTNOTIFY"),
	   pair($WS-EX-OVERLAPPEDWINDOW, "WS_EX_OVERLAPPEDWINDOW"),
	   pair($WS-EX-PALETTEWINDOW,    "WS_EX_PALETTEWINDOW"),
	   pair($WS-EX-RIGHT,            "WS_EX_RIGHT"),
	   pair($WS-EX-RIGHTSCROLLBAR,   "WS_EX_RIGHTSCROLLBAR"),
	   pair($WS-EX-RTLREADING,       "WS_EX_RTLREADING"),
	   pair($WS-EX-STATICEDGE,       "WS_EX_STATICEDGE"),
	   pair($WS-EX-TOOLWINDOW,       "WS_EX_TOOLWINDOW"),
	   pair($WS-EX-TOPMOST,          "WS_EX_TOPMOST"),
	   pair($WS-EX-TRANSPARENT,      "WS_EX_TRANSPARENT"),
	   pair($WS-EX-WINDOWEDGE,       "WS_EX_WINDOWEDGE"));

define method window-raw-styles
    (handle :: <HWND>)
 => (style, extended-style)
  values(GetWindowLong(handle, $GWL-STYLE),
	 GetWindowLong(handle, $GWL-EXSTYLE))
end method window-raw-styles;

define method window-styles
    (handle :: <HWND>)
 => (styles :: <sequence>)
  let (style, ext-style) = window-raw-styles(handle);
  decode-window-styles(handle, style, $basic-styles)
end method window-styles;

define method window-extended-styles
    (handle :: <HWND>)
 => (styles :: <sequence>)
  let (style, ext-style) = window-raw-styles(handle);
  decode-window-styles(handle, style, $extended-styles)
end method window-extended-styles;

define method window-class-styles
    (handle :: <HWND>)
 => (styles :: <sequence>)
  let (style, ext-style) = window-raw-styles(handle);
  let class = as(<symbol>, window-class-name(handle));
  let style-info = window-class-style-info(handle, class);
  decode-window-styles(handle, style, style-info)
end method window-class-styles;

define method window-class-style-info
    (handle :: <HWND>, class :: <symbol>)
 => (styles :: <sequence>)
  #[]
end method window-class-style-info;

define method decode-window-styles
    (handle :: <HWND>, style, style-info :: <sequence>)
 => (styles :: <sequence>)
  let styles = make(<stretchy-vector>);
  let mw/zero = as(<machine-word>, 0);
  for (info :: <pair> in style-info)
    let flag = info.head;
    let name = info.tail;
    add!(styles, name);
    add!(styles, flag-set?(flag, style))
  end;
  styles
end method decode-window-styles;


/// Button class information

define constant $button-styles
  = vector(pair($BS-3STATE,          "BS_3STATE"),
	   pair($BS-AUTO3STATE,      "BS_AUTO3STATE"),
	   pair($BS-AUTOCHECKBOX,    "BS_AUTOCHECKBOX"),
	   pair($BS-AUTORADIOBUTTON, "BS_AUTORADIOBUTTON"),
	   pair($BS-BITMAP,          "BS_BITMAP"),
	   pair($BS-BOTTOM,          "BS_BOTTOM"),
	   pair($BS-CENTER,          "BS_CENTER"),
	   pair($BS-CHECKBOX,        "BS_CHECKBOX"),
	   pair($BS-DEFPUSHBUTTON,   "BS_DEFPUSHBUTTON"),
	   pair($BS-FLAT,            "BS_FLAT"),
	   pair($BS-GROUPBOX,        "BS_GROUPBOX"),
	   pair($BS-ICON,            "BS_ICON"),
	   pair($BS-LEFT,            "BS_LEFT"),
	   pair($BS-LEFTTEXT,        "BS_LEFTTEXT"),
	   pair($BS-MULTILINE,       "BS_MULTILINE"),
	   pair($BS-NOTIFY,          "BS_NOTIFY"),
	   pair($BS-OWNERDRAW,       "BS_OWNERDRAW"),
	   pair($BS-PUSHBUTTON,      "BS_PUSHBUTTON"),
	   pair($BS-PUSHLIKE,        "BS_PUSHLIKE"),
	   pair($BS-RADIOBUTTON,     "BS_RADIOBUTTON"),
	   pair($BS-RIGHT,           "BS_RIGHT"),
	   pair($BS-RIGHTBUTTON,     "BS_RIGHTBUTTON"),
	   pair($BS-TEXT,            "BS_TEXT"),
	   pair($BS-TOP,             "BS_TOP"),
	   // Now obsolete
	   // pair($BS-USERBUTTON,     "BS_USERBUTTON"),
	   pair($BS-VCENTER,         "BS_VCENTER"));

define method window-class-style-info
    (handle :: <HWND>, class == #"button")
 => (styles :: <sequence>)
  $button-styles
end method window-class-style-info;

define method get-window-class-description
    (handle :: <HWND>, name :: <string>, class == #"button",
     style, ext-style)
 => (description :: <string>)
  case
    flag-set?($BS-GROUPBOX, style)      => "Group Box";
    flag-set?($BS-CHECKBOX, style)      => "Check Box";
    flag-set?($BS-RADIOBUTTON, style)   => "Radio Button";
    flag-set?($BS-PUSHBUTTON, style)    => "Push Button";
    flag-set?($BS-DEFPUSHBUTTON, style) => "Push Button";
    otherwise                           => "Button";
  end
end method get-window-class-description;


/// Combo box class information

define constant $combo-box-styles
  = vector(pair($CBS-AUTOHSCROLL,       "CBS_AUTOHSCROLL"),
	   pair($CBS-DISABLENOSCROLL,   "CBS_DISABLENOSCROLL"),
	   pair($CBS-DROPDOWN,          "CBS_DROPDOWN"),
	   pair($CBS-DROPDOWNLIST,      "CBS_DROPDOWNLIST"),
	   pair($CBS-HASSTRINGS,        "CBS_HASSTRINGS"),
	   pair($CBS-LOWERCASE,         "CBS_LOWERCASE"),
	   pair($CBS-NOINTEGRALHEIGHT,  "CBS_NOINTEGRALHEIGHT"),
	   pair($CBS-OEMCONVERT,        "CBS_OEMCONVERT"),
	   pair($CBS-OWNERDRAWFIXED,    "CBS_OWNERDRAWFIXED"),
	   pair($CBS-OWNERDRAWVARIABLE, "CBS_OWNERDRAWVARIABLE"),
	   pair($CBS-SIMPLE,            "CBS_SIMPLE"),
	   pair($CBS-SORT,              "CBS_SORT"),
	   pair($CBS-UPPERCASE,         "CBS_UPPERCASE"));

define method window-class-style-info
    (handle :: <HWND>, class == #"combobox")
 => (styles :: <sequence>)
  $combo-box-styles
end method window-class-style-info;

define method get-window-class-description
    (handle :: <HWND>, name :: <string>, class == #"combobox",
     style, ext-style)
 => (description :: <string>)
  case
    flag-set?($CBS-DROPDOWNLIST, style) => "Drop Down List";
    flag-set?($CBS-DROPDOWN, style)     => "Drop Down Combo Box";
    otherwise                           => "Combo Box";
  end
end method get-window-class-description;


/// Edit control class information

define constant $edit-control-styles
  = vector(pair($ES-AUTOHSCROLL, "ES_AUTOHSCROLL"),
	   pair($ES-AUTOVSCROLL, "ES_AUTOVSCROLL"),
	   pair($ES-CENTER,      "ES_CENTER"),
	   pair($ES-LEFT,        "ES_LEFT"),
	   pair($ES-LOWERCASE,   "ES_LOWERCASE"),
	   pair($ES-MULTILINE,   "ES_MULTILINE"),
	   pair($ES-NOHIDESEL,   "ES_NOHIDESEL"),
	   pair($ES-NUMBER,      "ES_NUMBER"),
	   pair($ES-OEMCONVERT,  "ES_OEMCONVERT"),
	   pair($ES-PASSWORD,    "ES_PASSWORD"),
	   pair($ES-READONLY,    "ES_READONLY"),
	   pair($ES-RIGHT,       "ES_RIGHT"),
	   pair($ES-UPPERCASE,   "ES_UPPERCASE"),
	   pair($ES-WANTRETURN,  "ES_WANTRETURN"));

define method window-class-style-info
    (handle :: <HWND>, class == #"edit")
 => (styles :: <sequence>)
  $edit-control-styles
end method window-class-style-info;

define method get-window-class-description
    (handle :: <HWND>, name :: <string>, class == #"edit",
     style, ext-style)
 => (description :: <string>)
  case
    flag-set?($ES-PASSWORD, style)  => "Password Edit Control";
    flag-set?($ES-MULTILINE, style) => "Multiline Edit Control";
    otherwise                       => "Edit Control";
  end
end method get-window-class-description;


/// List box class information

define constant $list-box-styles
  = vector(pair($LBS-DISABLENOSCROLL,   "LBS_DISABLENOSCROLL"),
	   pair($LBS-EXTENDEDSEL,       "LBS_EXTENDEDSEL"),
	   pair($LBS-HASSTRINGS,        "LBS_HASSTRINGS"),
	   pair($LBS-MULTICOLUMN,       "LBS_MULTICOLUMN"),
	   pair($LBS-MULTIPLESEL,       "LBS_MULTIPLESEL"),
	   pair($LBS-NODATA,            "LBS_NODATA"),
	   pair($LBS-NOINTEGRALHEIGHT,  "LBS_NOINTEGRALHEIGHT"),
	   pair($LBS-NOREDRAW,          "LBS_NOREDRAW"),
	   pair($LBS-NOSEL,             "LBS_NOSEL"),
	   pair($LBS-NOTIFY,            "LBS_NOTIFY"),
	   pair($LBS-OWNERDRAWFIXED,    "LBS_OWNERDRAWFIXED"),
	   pair($LBS-OWNERDRAWVARIABLE, "LBS_OWNERDRAWVARIABLE"),
	   pair($LBS-SORT,              "LBS_SORT"),
	   pair($LBS-STANDARD,          "LBS_STANDARD"),
	   pair($LBS-USETABSTOPS,       "LBS_USETABSTOPS"),
	   pair($LBS-WANTKEYBOARDINPUT, "LBS_WANTKEYBOARDINPUT"));

define method window-class-style-info
    (handle :: <HWND>, class == #"listbox")
 => (styles :: <sequence>)
  $list-box-styles
end method window-class-style-info;

define method get-window-class-description
    (handle :: <HWND>, name :: <string>, class == #"listbox",
     style, ext-style)
 => (description :: <string>)
  "List Box"
end method get-window-class-description;


/// Scroll bar class information

define constant $scroll-bar-styles
  = vector(pair($SBS-BOTTOMALIGN,             "SBS_BOTTOMALIGN"),
	   pair($SBS-HORZ,                    "SBS_HORZ"),
	   pair($SBS-LEFTALIGN,               "SBS_LEFTALIGN"),
	   pair($SBS-RIGHTALIGN,              "SBS_RIGHTALIGN"),
	   pair($SBS-SIZEBOX,                 "SBS_SIZEBOX"),
	   pair($SBS-SIZEBOXBOTTOMRIGHTALIGN, "SBS_SIZEBOXBOTTOMRIGHTALIGN"),
	   pair($SBS-SIZEBOXTOPLEFTALIGN,     "SBS_SIZEBOXTOPLEFTALIGN"),
	   pair($SBS-SIZEGRIP,                "SBS_SIZEGRIP"),
	   pair($SBS-TOPALIGN,                "SBS_TOPALIGN"),
	   pair($SBS-VERT,                    "SBS_VERT"));

define method window-class-style-info
    (handle :: <HWND>, class == #"scrollbar")
 => (styles :: <sequence>)
  $scroll-bar-styles
end method window-class-style-info;

define method get-window-class-description
    (handle :: <HWND>, name :: <string>, class == #"scrollbar",
     style, ext-style)
 => (description :: <string>)
  case
    flag-set?($SBS-HORZ, style) => "Horizontal Scroll Bar";
    flag-set?($SBS-VERT, style) => "Vertical Scroll Bar";
    otherwise                   => "Scroll Bar";
  end
end method get-window-class-description;


/// Static class information

define constant $static-styles
  = vector(pair($SS-BITMAP,         "SS_BITMAP"),
	   pair($SS-BLACKFRAME,     "SS_BLACKFRAME"),
	   pair($SS-BLACKRECT,      "SS_BLACKRECT"),
	   pair($SS-CENTER,         "SS_CENTER"),
	   pair($SS-CENTERIMAGE,    "SS_CENTERIMAGE"),
	   pair($SS-ENDELLIPSIS,    "SS_ENDELLIPSIS"),
	   pair($SS-ENHMETAFILE,    "SS_ENHMETAFILE"),
	   pair($SS-ETCHEDFRAME,    "SS_ETCHEDFRAME"),
	   pair($SS-ETCHEDHORZ,     "SS_ETCHEDHORZ"),
	   pair($SS-ETCHEDVERT,     "SS_ETCHEDVERT"),
	   pair($SS-GRAYFRAME,      "SS_GRAYFRAME"),
	   pair($SS-GRAYRECT,       "SS_GRAYRECT"),
	   pair($SS-ICON,           "SS_ICON"),
	   pair($SS-LEFT,           "SS_LEFT"),
	   pair($SS-LEFTNOWORDWRAP, "SS_LEFTNOWORDWRAP"),
	   pair($SS-NOPREFIX,       "SS_NOPREFIX"),
	   pair($SS-NOTIFY,         "SS_NOTIFY"),
	   pair($SS-OWNERDRAW,      "SS_OWNERDRAW"),
	   pair($SS-PATHELLIPSIS,   "SS_PATHELLIPSIS"),
	   pair($SS-REALSIZEIMAGE,  "SS_REALSIZEIMAGE"),
	   pair($SS-RIGHT,          "SS_RIGHT"),
	   pair($SS-RIGHTJUST,      "SS_RIGHTJUST"),
	   pair($SS-SIMPLE,         "SS_SIMPL"),
	   pair($SS-SUNKEN,         "SS_SUNKEN"),
	   pair($SS-WHITEFRAME,     "SS_WHITEFRAME"),
	   pair($SS-WHITERECT,      "SS_WHITERECT"),
	   pair($SS-WORDELLIPSIS,   "SS_WORDELLIPSIS"));

define method window-class-style-info
    (handle :: <HWND>, class == #"static")
 => (styles :: <sequence>)
  $static-styles
end method window-class-style-info;

define method get-window-class-description
    (handle :: <HWND>, name :: <string>, class == #"static",
     style, ext-style)
 => (description :: <string>)
  case
    flag-set?($SS-BITMAP, style)    => "Static Bitmap";
    flag-set?($SS-ICON, style)      => "Static Icon";
    flag-set?($SS-OWNERDRAW, style) => "Static Owner-drawn";
    otherwise                       => "Static Text";
  end
end method get-window-class-description;
