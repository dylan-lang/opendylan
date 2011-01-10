Module:    Win32-user
Synopsis:  Additional features from "winuser.h" needing manual translation.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//  defined in "winuser.h" as a macros:

define inline function CreateWindow ( lpClassName, lpWindowName, dwStyle, X, Y,
			    nWidth, nHeight, hWndParent, hMenu, hInstance,
			    lpParam ) => value :: <HWND>;
  CreateWindowEx (0, lpClassName, lpWindowName, dwStyle, X, Y,
		  nWidth, nHeight, hWndParent, hMenu, hInstance, lpParam);
end CreateWindow;

define inline function CreateDialog(hInstance, lpName, hWndParent,
				    lpDialogFunc);
  CreateDialogParam(hInstance, lpName, hWndParent, lpDialogFunc, 0)
end CreateDialog;
 
define inline function CreateDialogIndirect(hInstance, lpTemplate, hWndParent,
					  lpDialogFunc);
  CreateDialogIndirectParam(hInstance, lpTemplate, hWndParent,
			    lpDialogFunc, 0)
end CreateDialogIndirect;

define inline function DialogBox(hInstance, lpTemplate, hWndParent,
				 lpDialogFunc);
  DialogBoxParam(hInstance, lpTemplate, hWndParent, lpDialogFunc, 0)
end DialogBox;

/* -- this way doesn't work with SCC compiler (Bug 147) --  ???
define constant GetNextWindow = GetWindow;

define constant CopyCursor = CopyIcon;
*/

define C-function GetNextWindow
  parameter hWnd       :: <HWND>;
  parameter uCmd       :: <UINT>;
  result value :: <HWND>;
  c-name: "GetWindow", c-modifiers: "__stdcall";
end;

define C-function CopyCursor
  parameter hIcon      :: <HCURSOR>;
  result value :: <HCURSOR>;
  c-name: "CopyIcon", c-modifiers: "__stdcall";
end;


define inline function MAKEWPARAM(l, h); MAKELONG(l, h) end;
define inline function MAKELPARAM(l, h); MAKELONG(l, h) end;
define inline function MAKELRESULT(l, h); MAKELONG(l, h) end;

// This one handled specially because some of the parameters need to 
// accept values outside the range of an <integer>:
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

