Module:   dylan-user
Synopsis: Raw interface to win32 taskbar management
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module win32-taskbar
  use win32-common;
  use win32-user;
  use c-ffi;
  use threads;
  use table-extensions;
  use machine-words;
  use finalization;
  use functional-dylan;
  use simple-format;
  use simple-random;

  export
    <NOTIFYICONDATA>, <PNOTIFYICONDATA>,
    // IMPORTED: cbSize-value, cbSize-value-setter, 
    // IMPORTED: hWnd-value, hWnd-value-setter,
    uID-value, uID-value-setter,
    uFlags-value, uFlags-value-setter,
    uCallbackMessage-value, uCallbackMessage-value-setter,
    // IMPORTED: hIcon-value, hIcon-value-setter,
    szTip-value, szTip-array, szTip-array-setter,
    $NIM-ADD, $NIM-MODIFY, $NIM-DELETE,
    $NIF-MESSAGE, $NIF-ICON, $NIF-TIP,
    Shell-NotifyIcon;

end module win32-taskbar;
