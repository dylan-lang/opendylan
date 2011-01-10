Module:       windows-viewer
Author:       Andy Armstrong
Synopsis:     Windows viewer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Messages

define class <window-message> (<object>)
  constant slot message-handle :: <HWND>,
    required-init-keyword: handle:;
  constant slot message-id,
    required-init-keyword: id:;
  constant slot message-wParam,
    required-init-keyword: wParam:;
  constant slot message-lParam,
    required-init-keyword: lParam:;
  slot message-description :: <string> = "",
    init-keyword: description:;
end class <window-message>;

define table $message-name-table :: <object-table>
  = { $WM-NULL                  => "WM_NULL",
      $WM-CREATE                => "WM_CREATE",
      $WM-DESTROY               => "WM_DESTROY",
      $WM-MOVE                  => "WM_MOVE",
      $WM-SIZE                  => "WM_SIZE",
      $WM-ACTIVATE              => "WM_ACTIVATE",
      $WM-SETFOCUS              => "WM_SETFOCUS",
      $WM-KILLFOCUS             => "WM_KILLFOCUS",
      $WM-ENABLE                => "WM_ENABLE",
      $WM-SETREDRAW             => "WM_SETREDRAW",
      $WM-SETTEXT               => "WM_SETTEXT",
      $WM-GETTEXT               => "WM_GETTEXT",
      $WM-GETTEXTLENGTH         => "WM_GETTEXTLENGTH",
      $WM-PAINT                 => "WM_PAINT",
      $WM-CLOSE                 => "WM_CLOSE",
      $WM-QUERYENDSESSION       => "WM_QUERYENDSESSION",
      $WM-QUIT                  => "WM_QUIT",
      $WM-QUERYOPEN             => "WM_QUERYOPEN",
      $WM-ERASEBKGND            => "WM_ERASEBKGND",
      $WM-SYSCOLORCHANGE        => "WM_SYSCOLORCHANGE",
      $WM-ENDSESSION            => "WM_ENDSESSION",
      $WM-SHOWWINDOW            => "WM_SHOWWINDOW",
      $WM-WININICHANGE          => "WM_WININICHANGE",
      $WM-DEVMODECHANGE         => "WM_DEVMODECHANGE",
      $WM-ACTIVATEAPP           => "WM_ACTIVATEAPP",
      $WM-FONTCHANGE            => "WM_FONTCHANGE",
      $WM-TIMECHANGE            => "WM_TIMECHANGE",
      $WM-CANCELMODE            => "WM_CANCELMODE",
      $WM-SETCURSOR             => "WM_SETCURSOR",
      $WM-MOUSEACTIVATE         => "WM_MOUSEACTIVATE",
      $WM-CHILDACTIVATE         => "WM_CHILDACTIVATE",
      $WM-QUEUESYNC             => "WM_QUEUESYNC",
      $WM-GETMINMAXINFO         => "WM_GETMINMAXINFO",
      $WM-PAINTICON             => "WM_PAINTICON",
      $WM-ICONERASEBKGND        => "WM_ICONERASEBKGND",
      $WM-NEXTDLGCTL            => "WM_NEXTDLGCTL",
      $WM-SPOOLERSTATUS         => "WM_SPOOLERSTATUS",
      $WM-DRAWITEM              => "WM_DRAWITEM",
      $WM-MEASUREITEM           => "WM_MEASUREITEM",
      $WM-DELETEITEM            => "WM_DELETEITEM",
      $WM-VKEYTOITEM            => "WM_VKEYTOITEM",
      $WM-CHARTOITEM            => "WM_CHARTOITEM",
      $WM-SETFONT               => "WM_SETFONT",
      $WM-GETFONT               => "WM_GETFONT",
      $WM-SETHOTKEY             => "WM_SETHOTKEY",
      $WM-GETHOTKEY             => "WM_GETHOTKEY",
      $WM-QUERYDRAGICON         => "WM_QUERYDRAGICON",
      $WM-COMPAREITEM           => "WM_COMPAREITEM",
      $WM-COMPACTING            => "WM_COMPACTING",
      $WM-WINDOWPOSCHANGING     => "WM_WINDOWPOSCHANGING",
      $WM-WINDOWPOSCHANGED      => "WM_WINDOWPOSCHANGED",
      // $WM-POWER              => "WM_POWER",
      $WM-COPYDATA              => "WM_COPYDATA",
      $WM-CANCELJOURNAL         => "WM_CANCELJOURNAL",
      $WM-NCCREATE              => "WM_NCCREATE",
      $WM-NCDESTROY             => "WM_NCDESTROY",
      $WM-NCCALCSIZE            => "WM_NCCALCSIZE",
      $WM-NCHITTEST             => "WM_NCHITTEST",
      $WM-NCPAINT               => "WM_NCPAINT",
      $WM-NCACTIVATE            => "WM_NCACTIVATE",
      $WM-GETDLGCODE            => "WM_GETDLGCODE",
      $WM-NCMOUSEMOVE           => "WM_NCMOUSEMOVE",
      $WM-NCLBUTTONDOWN         => "WM_NCLBUTTONDOWN",
      $WM-NCLBUTTONUP           => "WM_NCLBUTTONUP",
      $WM-NCLBUTTONDBLCLK       => "WM_NCLBUTTONDBLCLK",
      $WM-NCRBUTTONDOWN         => "WM_NCRBUTTONDOWN",
      $WM-NCRBUTTONUP           => "WM_NCRBUTTONUP",
      $WM-NCRBUTTONDBLCLK       => "WM_NCRBUTTONDBLCLK",
      $WM-NCMBUTTONDOWN         => "WM_NCMBUTTONDOWN",
      $WM-NCMBUTTONUP           => "WM_NCMBUTTONUP",
      $WM-NCMBUTTONDBLCLK       => "WM_NCMBUTTONDBLCLK",
      $EM-GETSEL                => "EM_GETSEL",
      $EM-SETSEL                => "EM_SETSEL",
      $EM-GETRECT               => "EM_GETRECT",
      $EM-SETRECT               => "EM_SETRECT",
      $EM-SETRECTNP             => "EM_SETRECTNP",
      $EM-SCROLL                => "EM_SCROLL",
      $EM-LINESCROLL            => "EM_LINESCROLL",
      $EM-SCROLLCARET           => "EM_SCROLLCARET",
      $EM-GETMODIFY             => "EM_GETMODIFY",
      $EM-SETMODIFY             => "EM_SETMODIFY",
      $EM-GETLINECOUNT          => "EM_GETLINECOUNT",
      $EM-LINEINDEX             => "EM_LINEINDEX",
      $EM-SETHANDLE             => "EM_SETHANDLE",
      $EM-GETHANDLE             => "EM_GETHANDLE",
      $EM-GETTHUMB              => "EM_GETTHUMB",
      $EM-LINELENGTH            => "EM_LINELENGTH",
      $EM-REPLACESEL            => "EM_REPLACESEL",
      $EM-GETLINE               => "EM_GETLINE",
      $EM-LIMITTEXT             => "EM_LIMITTEXT",
      $EM-CANUNDO               => "EM_CANUNDO",
      $EM-UNDO                  => "EM_UNDO",
      $EM-FMTLINES              => "EM_FMTLINES",
      $EM-LINEFROMCHAR          => "EM_LINEFROMCHAR",
      $EM-SETTABSTOPS           => "EM_SETTABSTOPS",
      $EM-SETPASSWORDCHAR       => "EM_SETPASSWORDCHAR",
      $EM-EMPTYUNDOBUFFER       => "EM_EMPTYUNDOBUFFER",
      $EM-GETFIRSTVISIBLELINE   => "EM_GETFIRSTVISIBLELINE",
      $EM-SETREADONLY           => "EM_SETREADONLY",
      $EM-SETWORDBREAKPROC      => "EM_SETWORDBREAKPROC",
      $EM-GETWORDBREAKPROC      => "EM_GETWORDBREAKPROC",
      $EM-GETPASSWORDCHAR       => "EM_GETPASSWORDCHAR",
      $SBM-SETPOS               => "SBM_SETPOS",
      $SBM-GETPOS               => "SBM_GETPOS",
      $SBM-SETRANGE             => "SBM_SETRANGE",
      $SBM-GETRANGE             => "SBM_GETRANGE",
      // $SBM-ENABLE_ARROWS     => "SBM_ENABLE_ARROWS",
      $SBM-SETRANGEREDRAW       => "SBM_SETRANGEREDRAW",
      $BM-GETCHECK              => "BM_GETCHECK",
      $BM-SETCHECK              => "BM_SETCHECK",
      $BM-GETSTATE              => "BM_GETSTATE",
      $BM-SETSTATE              => "BM_SETSTATE",
      $BM-SETSTYLE              => "BM_SETSTYLE",
      $WM-KEYDOWN               => "WM_KEYDOWN",
      $WM-KEYUP                 => "WM_KEYUP",
      $WM-CHAR                  => "WM_CHAR",
      $WM-DEADCHAR              => "WM_DEADCHAR",
      $WM-SYSKEYDOWN            => "WM_SYSKEYDOWN",
      $WM-SYSKEYUP              => "WM_SYSKEYUP",
      $WM-SYSCHAR               => "WM_SYSCHAR",
      $WM-SYSDEADCHAR           => "WM_SYSDEADCHAR",
      // $WM-CONVERTREQUEST     => "WM_CONVERTREQUEST",
      // $WM-CONVERTRESULT      => "WM_CONVERTRESULT",
      // $WM-INTERIM            => "WM_INTERIM",
      $WM-IME-STARTCOMPOSITION  => "WM_IME_STARTCOMPOSITION",
      $WM-IME-ENDCOMPOSITION    => "WM_IME_ENDCOMPOSITION",
      $WM-IME-COMPOSITION       => "WM_IME_COMPOSITION",
      $WM-INITDIALOG            => "WM_INITDIALOG",
      $WM-COMMAND               => "WM_COMMAND",
      $WM-SYSCOMMAND            => "WM_SYSCOMMAND",
      $WM-TIMER                 => "WM_TIMER",
      $WM-HSCROLL               => "WM_HSCROLL",
      $WM-VSCROLL               => "WM_VSCROLL",
      $WM-INITMENU              => "WM_INITMENU",
      $WM-INITMENUPOPUP         => "WM_INITMENUPOPUP",
      $WM-MENUSELECT            => "WM_MENUSELECT",
      $WM-MENUCHAR              => "WM_MENUCHAR",
      $WM-ENTERIDLE             => "WM_ENTERIDLE",
      $WM-CTLCOLORMSGBOX        => "WM_CTLCOLORMSGBOX",
      $WM-CTLCOLOREDIT          => "WM_CTLCOLOREDIT",
      $WM-CTLCOLORLISTBOX       => "WM_CTLCOLORLISTBOX",
      $WM-CTLCOLORBTN           => "WM_CTLCOLORBTN",
      $WM-CTLCOLORDLG           => "WM_CTLCOLORDLG",
      $WM-CTLCOLORSCROLLBAR     => "WM_CTLCOLORSCROLLBAR",
      $WM-CTLCOLORSTATIC        => "WM_CTLCOLORSTATIC",
      $WM-MOUSEFIRST            => "WM_MOUSEFIRST",
      $WM-MOUSEMOVE             => "WM_MOUSEMOVE",
      $WM-LBUTTONDOWN           => "WM_LBUTTONDOWN",
      $WM-LBUTTONUP             => "WM_LBUTTONUP",
      $WM-LBUTTONDBLCLK         => "WM_LBUTTONDBLCLK",
      $WM-RBUTTONDOWN           => "WM_RBUTTONDOWN",
      $WM-RBUTTONUP             => "WM_RBUTTONUP",
      $WM-RBUTTONDBLCLK         => "WM_RBUTTONDBLCLK",
      $WM-MBUTTONDOWN           => "WM_MBUTTONDOWN",
      $WM-MBUTTONUP             => "WM_MBUTTONUP",
      $WM-MBUTTONDBLCLK         => "WM_MBUTTONDBLCLK",
      $WM-MOUSELAST             => "WM_MOUSELAST",
      $WM-PARENTNOTIFY          => "WM_PARENTNOTIFY",
      $WM-ENTERMENULOOP         => "WM_ENTERMENULOOP",
      $WM-EXITMENULOOP          => "WM_EXITMENULOOP",
      $WM-NEXTMENU              => "WM_NEXTMENU",
      $WM-SIZING                => "WM_SIZING",
      $WM-CAPTURECHANGED        => "WM_CAPTURECHANGED",
      $WM-MOVING                => "WM_MOVING",
      $WM-POWERBROADCAST        => "WM_POWERBROADCAST",
      $CB-GETEDITSEL            => "CB_GETEDITSEL",
      $CB-LIMITTEXT             => "CB_LIMITTEXT",
      $CB-SETEDITSEL            => "CB_SETEDITSEL",
      $CB-ADDSTRING             => "CB_ADDSTRING",
      $CB-DELETESTRING          => "CB_DELETESTRING",
      $CB-DIR                   => "CB_DIR",
      $CB-GETCOUNT              => "CB_GETCOUNT",
      $CB-GETCURSEL             => "CB_GETCURSEL",
      $CB-GETLBTEXT             => "CB_GETLBTEXT",
      $CB-GETLBTEXTLEN          => "CB_GETLBTEXTLEN",
      $CB-INSERTSTRING          => "CB_INSERTSTRING",
      $CB-RESETCONTENT          => "CB_RESETCONTENT",
      $CB-FINDSTRING            => "CB_FINDSTRING",
      $CB-SELECTSTRING          => "CB_SELECTSTRING",
      $CB-SETCURSEL             => "CB_SETCURSEL",
      $CB-SHOWDROPDOWN          => "CB_SHOWDROPDOWN",
      $CB-GETITEMDATA           => "CB_GETITEMDATA",
      $CB-SETITEMDATA           => "CB_SETITEMDATA",
      $CB-GETDROPPEDCONTROLRECT => "CB_GETDROPPEDCONTROLRECT",
      $CB-SETITEMHEIGHT         => "CB_SETITEMHEIGHT",
      $CB-GETITEMHEIGHT         => "CB_GETITEMHEIGHT",
      $CB-SETEXTENDEDUI         => "CB_SETEXTENDEDUI",
      $CB-GETEXTENDEDUI         => "CB_GETEXTENDEDUI",
      $CB-GETDROPPEDSTATE       => "CB_GETDROPPEDSTATE",
      $CB-FINDSTRINGEXACT       => "CB_FINDSTRINGEXACT",
      $CB-SETLOCALE             => "CB_SETLOCALE",
      $CB-GETLOCALE             => "CB_GETLOCALE",
      $STM-SETICON              => "STM_SETICON",
      $STM-GETICON              => "STM_GETICON",
      $LB-ADDSTRING             => "LB_ADDSTRING",
      $LB-INSERTSTRING          => "LB_INSERTSTRING",
      $LB-DELETESTRING          => "LB_DELETESTRING",
      $LB-SELITEMRANGEEX        => "LB_SELITEMRANGEEX",
      $LB-RESETCONTENT          => "LB_RESETCONTENT",
      $LB-SETSEL                => "LB_SETSEL",
      $LB-SETCURSEL             => "LB_SETCURSEL",
      $LB-GETSEL                => "LB_GETSEL",
      $LB-GETCURSEL             => "LB_GETCURSEL",
      $LB-GETTEXT               => "LB_GETTEXT",
      $LB-GETTEXTLEN            => "LB_GETTEXTLEN",
      $LB-GETCOUNT              => "LB_GETCOUNT",
      $LB-SELECTSTRING          => "LB_SELECTSTRING",
      $LB-DIR                   => "LB_DIR",
      $LB-GETTOPINDEX           => "LB_GETTOPINDEX",
      $LB-FINDSTRING            => "LB_FINDSTRING",
      $LB-GETSELCOUNT           => "LB_GETSELCOUNT",
      $LB-GETSELITEMS           => "LB_GETSELITEMS",
      $LB-SETTABSTOPS           => "LB_SETTABSTOPS",
      $LB-GETHORIZONTALEXTENT   => "LB_GETHORIZONTALEXTENT",
      $LB-SETHORIZONTALEXTENT   => "LB_SETHORIZONTALEXTENT",
      $LB-SETCOLUMNWIDTH        => "LB_SETCOLUMNWIDTH",
      $LB-ADDFILE               => "LB_ADDFILE",
      $LB-SETTOPINDEX           => "LB_SETTOPINDEX",
      $LB-GETITEMRECT           => "LB_GETITEMRECT",
      $LB-GETITEMDATA           => "LB_GETITEMDATA",
      $LB-SETITEMDATA           => "LB_SETITEMDATA",
      $LB-SELITEMRANGE          => "LB_SELITEMRANGE",
      $LB-SETANCHORINDEX        => "LB_SETANCHORINDEX",
      $LB-GETANCHORINDEX        => "LB_GETANCHORINDEX",
      $LB-SETCARETINDEX         => "LB_SETCARETINDEX",
      $LB-GETCARETINDEX         => "LB_GETCARETINDEX",
      $LB-SETITEMHEIGHT         => "LB_SETITEMHEIGHT",
      $LB-GETITEMHEIGHT         => "LB_GETITEMHEIGHT",
      $LB-FINDSTRINGEXACT       => "LB_FINDSTRINGEXACT",
      $LB-SETLOCALE             => "LB_SETLOCALE",
      $LB-GETLOCALE             => "LB_GETLOCALE",
      $LB-SETCOUNT              => "LB_SETCOUNT",
      $WM-MOUSEMOVE             => "WM_MOUSEMOVE",
      $WM-LBUTTONDOWN           => "WM_LBUTTONDOWN",
      $WM-LBUTTONUP             => "WM_LBUTTONUP",
      $WM-LBUTTONDBLCLK         => "WM_LBUTTONDBLCLK",
      $WM-RBUTTONDOWN           => "WM_RBUTTONDOWN",
      $WM-RBUTTONUP             => "WM_RBUTTONUP",
      $WM-RBUTTONDBLCLK         => "WM_RBUTTONDBLCLK",
      $WM-MBUTTONDOWN           => "WM_MBUTTONDOWN",
      $WM-MBUTTONUP             => "WM_MBUTTONUP",
      $WM-MBUTTONDBLCLK         => "WM_MBUTTONDBLCLK",
      $WM-PARENTNOTIFY          => "WM_PARENTNOTIFY",
      $WM-ENTERMENULOOP         => "WM_ENTERMENULOOP",
      $WM-EXITMENULOOP          => "WM_EXITMENULOOP",
      $WM-MDICREATE             => "WM_MDICREATE",
      $WM-MDIDESTROY            => "WM_MDIDESTROY",
      $WM-MDIACTIVATE           => "WM_MDIACTIVATE",
      $WM-MDIRESTORE            => "WM_MDIRESTORE",
      $WM-MDINEXT               => "WM_MDINEXT",
      $WM-MDIMAXIMIZE           => "WM_MDIMAXIMIZE",
      $WM-MDITILE               => "WM_MDITILE",
      $WM-MDICASCADE            => "WM_MDICASCADE",
      $WM-MDIICONARRANGE        => "WM_MDIICONARRANGE",
      $WM-MDIGETACTIVE          => "WM_MDIGETACTIVE",
      $WM-MDISETMENU            => "WM_MDISETMENU",
      $WM-ENTERSIZEMOVE         => "WM_ENTERSIZEMOVE",
      $WM-EXITSIZEMOVE          => "WM_EXITSIZEMOVE",
      $WM-DROPFILES             => "WM_DROPFILES",
      $WM-MDIREFRESHMENU        => "WM_MDIREFRESHMENU",
      $WM-COPY                  => "WM_COPY",
      $WM-PASTE                 => "WM_PASTE",
      $WM-CLEAR                 => "WM_CLEAR",
      $WM-UNDO                  => "WM_UNDO",
      $WM-RENDERFORMAT          => "WM_RENDERFORMAT",
      $WM-RENDERALLFORMATS      => "WM_RENDERALLFORMATS",
      $WM-DESTROYCLIPBOARD      => "WM_DESTROYCLIPBOARD",
      $WM-DRAWCLIPBOARD         => "WM_DRAWCLIPBOARD",
      $WM-PAINTCLIPBOARD        => "WM_PAINTCLIPBOARD",
      $WM-VSCROLLCLIPBOARD      => "WM_VSCROLLCLIPBOARD",
      $WM-SIZECLIPBOARD         => "WM_SIZECLIPBOARD",
      $WM-ASKCBFORMATNAME       => "WM_ASKCBFORMATNAME",
      $WM-CHANGECBCHAIN         => "WM_CHANGECBCHAIN",
      $WM-HSCROLLCLIPBOARD      => "WM_HSCROLLCLIPBOARD",
      $WM-QUERYNEWPALETTE       => "WM_QUERYNEWPALETTE",
      $WM-PALETTEISCHANGING     => "WM_PALETTEISCHANGING",
      $WM-PALETTECHANGED        => "WM_PALETTECHANGED",
      $WM-HOTKEY                => "WM_HOTKEY",
      $WM-DDE-INITIATE          => "WM_DDE_INITIATE",
      $WM-DDE-TERMINATE         => "WM_DDE_TERMINATE",
      $WM-DDE-ADVISE            => "WM_DDE_ADVISE",
      $WM-DDE-UNADVISE          => "WM_DDE_UNADVISE",
      $WM-DDE-ACK               => "WM_DDE_ACK",
      $WM-DDE-DATA              => "WM_DDE_DATA",
      $WM-DDE-REQUEST           => "WM_DDE_REQUEST",
      $WM-DDE-POKE              => "WM_DDE_POKE",
      $WM-DDE-EXECUTE           => "WM_DDE_EXECUTE" };

define method message-name
    (message :: <window-message>)
 => (name :: <string>)
  let id = message.message-id;
  element($message-name-table, id, default: #f)
    | number-to-hex-string(id)
end method message-name;

define method message-window-name
    (message :: <window-message>)
 => (name :: <string>)
  window-name(message.message-handle)
end method message-window-name;

define method message-category
    (message :: <window-message>)
 => (category :: <symbol>)
  select (message.message-id)
    $WM-NCCREATE, $WM-NCDESTROY, $WM-NCCALCSIZE, $WM-NCHITTEST,
    $WM-NCPAINT, $WM-NCACTIVATE, "WM_NCACTIVATE", $WM-NCMBUTTONUP =>
      #"non-client";
    otherwise =>
      #"general";
  end
end method message-category;


/// Messages pane

define pane <messages-pane> ()
  slot pane-handle :: false-or(<HWND>) = #f,
    setter: %handle-setter,
    init-keyword: handle:;
  constant slot pane-hook-dll :: <HMODULE>,
    required-init-keyword: hook-dll:;
  constant slot pane-all-threads? :: <boolean> = #f;
  constant slot pane-messages :: <stretchy-object-vector>
    = make(<stretchy-object-vector>);
  constant slot pane-categories :: <sequence> = #[#"general"],
    init-keyword: categories:;
  pane start-button (pane)
    make(<push-button>,
	 label: "Start",
	 activate-callback: method (gadget)
			      ignore(gadget);
			      start-message-logging(pane)
			    end);
  pane stop-button (pane)
    make(<push-button>,
	 label: "Stop",
	 activate-callback: method (gadget)
			      ignore(gadget);
			      stop-message-logging(pane)
			    end);
  pane messages-table (pane)
    make(<table-control>,
         headings: #["Window", "Message", "Options", "wParam", "lParam"],
         widths:   #[80, 180, 160, 80, 80],
	 items:    pane.pane-messages,
         generators: vector(message-window-name,
			    message-name,
			    message-description,
			    message-wParam,
			    message-lParam),
	 label-key: method (object)
		      select (object by instance?)
			<integer>, <machine-word> =>
			  number-to-hex-string(object);
			otherwise =>
			  format-to-string("%s", object);
		      end
		    end,
         popup-menu-callback: curry(display-message-popup-menu, pane));
  layout (pane)
    vertically (spacing: 4)
      horizontally (spacing: 4)
        pane.start-button;
        pane.stop-button;
      end;
      pane.messages-table
    end;
end pane <messages-pane>;

define method pane-handle-setter
    (handle :: false-or(<HWND>), pane :: <messages-pane>)
 => (handle :: false-or(<HWND>))
  stop-message-logging(pane);
  pane.%handle := handle;
  pane.pane-messages.size := 0;
  update-messages-table(pane);
  handle
end method pane-handle-setter;

define method update-messages-table
    (pane :: <messages-pane>) => ()
  gadget-items(pane.messages-table)
    := pane.pane-filtered-messages
end method update-messages-table;

define method pane-filtered-messages
    (pane :: <messages-pane>) => (messages :: <sequence>)
  let messages :: <stretchy-object-vector> 
    = make(<stretchy-object-vector>);
  let categories = pane.pane-categories;
  for (message :: <window-message> in pane.pane-messages)
    if (member?(message.message-category, categories))
      add!(messages, message)
    end
  end;
  messages
end method pane-filtered-messages;

define function record-windows-message
    (pane :: <messages-pane>, message :: <window-message>) => ()
  add!(pane.pane-messages, message);
  // update-messages-table(pane)
end function record-windows-message;

define function display-message-popup-menu
    (pane :: <messages-pane>, #rest args)
  notify-user(format-to-string("Popup!: %=", args), owner: pane)
end function display-message-popup-menu;

//---*** For some reason this wasn't imported into win32-user
define inline-only C-function GetWindowThreadProcessId
  parameter hWnd          :: <HWND>;
  parameter lpdwProcessId :: <LPDWORD>;
  result value :: <DWORD>;
  c-name: "GetWindowThreadProcessId", c-modifiers: "__stdcall";
end;

define constant $null-lpdword = make(<LPDWORD>, address: 0);

define function start-message-logging
    (pane :: <messages-pane>) => ()
  let hook-dll = pane.pane-hook-dll;
  let handle = pane.pane-handle;
  let thread 
    = if (pane.pane-all-threads?)
	0
      else
	GetWindowThreadProcessId(handle, $null-lpdword)
      end;
  install-windows-hooks
    (hook-dll, thread, 
     method (message :: <window-message>)
       let frame = sheet-frame(pane);
       frame & call-in-frame(frame, record-windows-message, pane, message)
     end);
  frame-status-message(pane.sheet-frame)
    := format-to-string("Collecting events for %=...",
			window-name(handle))
end function start-message-logging;

define function stop-message-logging
    (pane :: <messages-pane>) => ()
  uninstall-windows-hooks();
  update-messages-table(pane);
  frame-status-message(pane.sheet-frame) := ""
end function stop-message-logging;
