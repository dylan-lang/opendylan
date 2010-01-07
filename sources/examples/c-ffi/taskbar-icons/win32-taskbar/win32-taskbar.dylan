Module:   win32-taskbar
Synopsis: Raw interface to win32 taskbar management
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// TODO: Define <NOTIFYICONDATAA> struct and pointer class.

define C-struct <NOTIFYICONDATAA>
  slot cbSize-value   :: <DWORD>;
  slot hWnd-value     :: <HWND>;
  slot uID-value      :: <UINT>;
  slot uFlags-value   :: <UINT>;
  slot uCallbackMessage-value :: <UINT>;
  slot hIcon-value    :: <HICON>;
  array slot szTip-array :: <CHAR>, length: 64,
	address-getter: szTip-value;
  pack: 1;
  pointer-type-name: <LPNOTIFYICONDATAA>;
  c-name: "struct _NOTIFYICONDATAA";
end C-struct <NOTIFYICONDATAA>;

// TODO: Define typedef aliases for the above.

define constant <PNOTIFYICONDATAA> = <LPNOTIFYICONDATAA>;
define constant <NOTIFYICONDATA> = <NOTIFYICONDATAA>;
define constant <PNOTIFYICONDATA> = <PNOTIFYICONDATAA>;

// TODO: Define message/flag integer constants.

define constant $NIM-ADD                    = #x00000000;
define constant $NIM-MODIFY                 = #x00000001;
define constant $NIM-DELETE                 = #x00000002;
define constant $NIF-MESSAGE                = #x00000001;
define constant $NIF-ICON                   = #x00000002;
define constant $NIF-TIP                    = #x00000004;

// TODO: Define Shell-NotifyIcon C function.

define C-function Shell-NotifyIcon
  parameter dwMessage  :: <DWORD>;
  parameter lpData     :: <PNOTIFYICONDATAA>;
  result value :: <BOOL>;
  c-name: "Shell_NotifyIconA", c-modifiers: "__stdcall";
end;

// TODO: Add Shell32.lib to the project's Sources
