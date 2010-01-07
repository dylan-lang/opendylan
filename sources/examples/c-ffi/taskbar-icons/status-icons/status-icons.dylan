Module:   status-icons
Synopsis: A library abstracting persistent status icon display
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Icon loading.

define method load-taskbar-icon 
    (name :: <byte-string>) => (handle :: <HANDLE>)
  LoadImage
    (application-instance-handle(), name, $IMAGE-ICON, 16, 16, $LR-DEFAULTCOLOR);
end method;

// The top level exe must provide these icon resources.
define constant $ok-icon      = load-taskbar-icon("TASKBAROK");
define constant $warning-icon = load-taskbar-icon("TASKBARWARNING");
define constant $error-icon   = load-taskbar-icon("TASKBARERROR");
define constant $our-icon-id  = 1; // Arbitrary.

//// Implementation interface.

/* TODO: Define an add-icon method corresponding to the following C:

  void AddIcon (void)
  {
    NOTIFYICONDATA data;

    data.cbSize = sizeof(NOTIFYICONDATA);
    data.hWnd   = (HWND)NULL;
    data.uId    = OUR_ICON_ID;
    data.uFlags = NIF_ICON;
    data.hIcon  = OK_ICON;
    Shell_NotifyIcon(NIM_ADD, data);
  }
*/

define method add-icon () => ()
  with-stack-structure (data :: <PNOTIFYICONDATA>)
    data.cbSize-value := size-of(<NOTIFYICONDATA>);
    data.hWnd-value   := $null-hwnd;
    data.uId-value    := $our-icon-id;
    data.uFlags-value := $NIF-ICON;
    data.hIcon-value  := $ok-icon;
    Shell-NotifyIcon($NIM-ADD, data);
  end;
end method;

/* TODO: Define a change-icon method corresponding to the following C:

  void ChangeIcon (HANDLE icon)
  {
    NOTIFYICONDATA data;

    data.cbSize = sizeof(NOTIFYICONDATA);
    data.hWnd   = (HWND)NULL;
    data.uId    = OUR_ICON_ID;
    data.uFlags = NIF_ICON;
    data.hIcon  = icon;
    Shell_NotifyIcon(NIM_MODIFY, data);
  }
*/

define method change-icon (icon) => ()
  with-stack-structure (data :: <PNOTIFYICONDATA>)
    data.cbSize-value := size-of(<NOTIFYICONDATA>);
    data.hWnd-value   := $null-hwnd;
    data.uId-value    := $our-icon-id;
    data.uFlags-value := $NIF-ICON;
    data.hIcon-value  := icon;
    Shell-NotifyIcon($NIM-MODIFY, data);
  end;
end method;

/* TODO: Define a change-tip method corresponding to the following C:

  void ChangeTip (LPSTR tip)
  {
    NOTIFYICONDATA data;

    data.cbSize = sizeof(NOTIFYICONDATA);
    data.hWnd   = (HWND)NULL;
    data.uId    = OUR_ICON_ID;
    data.uFlags = NIF_TIP;
    strcpy(data.szTip, tip);
    Shell_NotifyIcon(NIM_MODIFY, data);
  }
*/

define method change-tip (tip) => ()
  with-stack-structure (data :: <PNOTIFYICONDATA>)
    data.cbSize-value := size-of(<NOTIFYICONDATA>);
    data.hWnd-value   := $null-hwnd;
    data.uId-value    := $our-icon-id;
    data.uFlags-value := $NIF-TIP;
    let c-tip = data.szTip-value;
    for (i from 0, char in tip)
      c-tip[i] := char;
    finally
      c-tip[i] := '\0';
    end;
    Shell-NotifyIcon($NIM-MODIFY, data);
  end;
end method;

/* TODO: Define a remove-icon method corresponding to the following C:

  void RemoveIcon ()
  {
    NOTIFYICONDATA data;

    data.cbSize = sizeof(NOTIFYICONDATA);
    data.hWnd   = (HWND)NULL;
    data.uId    = OUR_ICON_ID;
    Shell_NotifyIcon(NIM_DELETE, data);
  }
*/

define method remove-icon () => ()
  with-stack-structure (data :: <PNOTIFYICONDATA>)
    data.cbSize-value := size-of(<NOTIFYICONDATA>);
    data.hWnd-value   := null-pointer(<HWND>);
    data.uId-value    := $our-icon-id;
    // data.uFlags-value := 0; 
    Shell-NotifyIcon($NIM-DELETE, data);
  end;
end method;

//// Client interface.

define variable *icon-displayed?* = #f;

define method ensure-status-display () => ()
  if (~*icon-displayed?*)
    add-icon();
    *icon-displayed?* := #t;
  end;
end method;

define method stop-status-display () => ()
  if (*icon-displayed?*)
    remove-icon();
    *icon-displayed?* := #f;
  end;
end method;

define method display-status-icon (icon, tip) => ()
  ensure-status-display(); 
  change-icon(icon);
  if (tip) change-tip(tip) end;
end method;

define method display-status-ok (#key tip :: false-or(<string>)) => ()
  display-status-icon($ok-icon, tip);
end method;

define method display-status-warning (#key tip :: false-or(<string>)) => ()
  display-status-icon($warning-icon, tip);
end method;

define method display-status-error (#key tip :: false-or(<string>)) => ()
  display-status-icon($error-icon, tip);
end method;
