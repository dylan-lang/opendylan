Module:    Win32-common
Synopsis:  `define callback' macro
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Win32 callbacks via `define callback'
//
// The user does:
//
//    define callback WndProc :: <WNDPROC> = main-window-function;
//
// which defines WndProc as a function pointer callable from C, which will
// invoke the Dylan function main-window-function.
//


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

// Note: the `c-name:' clause is intentionally omitted so that a unique
//   name will be generated for each expansion of the macro.

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

// The following probably should be moved to the `win32-dialog' library.
// But it seems like there ought to be a cleaner way to extend this.

define macro <LPOFNHOOKPROC>-callback-wrapper
  { <LPOFNHOOKPROC>-callback-wrapper(?new:name,?old:name) } =>
    { <DLGPROC>-callback-wrapper(?new,?old) }
end;

define macro <LPCCHOOKPROC>-callback-wrapper
  { <LPCCHOOKPROC>-callback-wrapper(?new:name,?old:name) } =>
    { <DLGPROC>-callback-wrapper(?new,?old) }
end;

define macro <LPFRHOOKPROC>-callback-wrapper
  { <LPFRHOOKPROC>-callback-wrapper(?new:name,?old:name) } =>
    { <DLGPROC>-callback-wrapper(?new,?old) }
end;

define macro <LPCFHOOKPROC>-callback-wrapper
  { <LPCFHOOKPROC>-callback-wrapper(?new:name,?old:name) } =>
    { <DLGPROC>-callback-wrapper(?new,?old) }
end;

define macro <LPPRINTHOOKPROC>-callback-wrapper
  { <LPPRINTHOOKPROC>-callback-wrapper(?new:name,?old:name) } =>
    { <DLGPROC>-callback-wrapper(?new,?old) }
end;

define macro <LPSETUPHOOKPROC>-callback-wrapper
  { <LPSETUPHOOKPROC>-callback-wrapper(?new:name,?old:name) } =>
    { <DLGPROC>-callback-wrapper(?new,?old) }
end;
