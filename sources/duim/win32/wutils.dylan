Module:    win32-duim
Synopsis:  Win32 back-end utilities
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Some useful constants

//---*** Can we get these from somewhere?
define constant $false    :: <integer> = 0;
define constant $true     :: <integer> = 1;
define constant $NO_ERROR :: <integer> = 0;

//--- We can't use <WPARAM> because it is a C type, and we use <object>
//--- rather than something more specific to speed up dispatch.
define constant <wparam-type>  = <object>;
define constant <lparam-type>  = <object>;
define constant <lresult-type> = <object>;
define constant <message-type> = <signed-long>;
define constant <options-type> = <object>;

define constant $null-HACCEL = null-pointer(<HACCEL>);

define constant $empty-c-string = make(<C-string>, size: 0);

//--- Kludge to prevent recursion during various operations
define thread variable *port-did-it?* :: <boolean> = #f;


///---*** Hacked version of size-of to avoid inlining

define function safe-size-of (value) => (size :: <integer>)
  size-of(value)
end function safe-size-of;


/// Error handling

// Many Windows functions return #f, 0 or a NULL pointer in case of failure;
// the following function can be used to check the returned value and
// signal an error in case of failure.
define method check-result
    (name :: <string>, handle :: <C-pointer>) => (value :: <C-pointer>)
  when (null-pointer?(handle))
    report-error(name)
  end;
  handle
end method check-result;

define method check-result
    (name :: <string>, integer :: type-union(<integer>, <machine-word>))
 => (value :: type-union(<integer>, <machine-word>))
  when (zero?(integer))
    report-error(name)
  end;
  integer
end method check-result;

define method check-result
    (name :: <string>, ok? :: <boolean>) => (value :: <boolean>)
  unless (ok?)
    report-error(name)
  end;
  ok?
end method check-result;

define function report-error
    (name :: <string>, #key error) => ()
  let error = error | GetLastError();
  unless (error = $NO_ERROR)
    cerror("Try to continue anyway",
           "%s error %d: %s",
           name, error, win32-error-message(error))
  end
end function report-error;

define function ensure-no-error (name :: <string>) => ()
  let error = GetLastError();
  unless (error = $NO_ERROR)
    report-error(name, error: error)
  end
end function ensure-no-error;

define function ensure-no-dialog-error (name :: <string>) => ()
  let error = CommDlgExtendedError();
  unless (error = $NO_ERROR)
    report-error(name, error: error)
  end
end function ensure-no-dialog-error;

define function not-yet-implemented
    (format-message :: <string>, #rest format-args)
  apply(error, 
        concatenate(format-message, " not yet implemented!"),
        format-args)
end function not-yet-implemented;

// A simpler debug message since the console debugger is too slow!
define function windows-debug-message
    (format-string :: <string>, #rest format-arguments) => ()
  let message
    = block ()
        apply(format-to-string, format-string, format-arguments)
      exception (error :: <error>)
        block ()
          format-to-string("*** debug-message crashed: %s", error)
        exception (error :: <error>)
          "*** debug-message crashed"
        end
      end;
  OutputDebugString(concatenate(message, "\n"))
end function windows-debug-message;


/// Win32 metrics

define class <win32-metrics> (<object>)
  sealed slot win32-pixels-per-inch :: <integer> = 100,
    init-keyword: pixels-per-inch:;
  sealed constant slot win32-dialog-x-units :: <integer>,
    required-init-keyword: x-units:;
  sealed constant slot win32-dialog-y-units :: <integer>,
    required-init-keyword: y-units:;
  sealed constant slot win32-mouse-buttons :: <integer>,
    required-init-keyword: mouse-buttons:;
end class <win32-metrics>;

define method make-win32-metrics () => (metrics :: <win32-metrics>)
  let base-units = GetDialogBaseUnits();
  make(<win32-metrics>,
       x-units: LOWORD(base-units),
       y-units: HIWORD(base-units),
       mouse-buttons: GetSystemMetrics($SM-CMOUSEBUTTONS))
end method make-win32-metrics;


/// Window size functions (should be in win32-user?)

// Note that the return values are in _screen_ coordinates
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

define method get-window-size
    (handle :: <HWND>)
 => (width :: <integer>, height :: <integer>)
  let (left, top, right, bottom) = get-window-edges(handle);
  values(right - left, bottom - top)
end method get-window-size;

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

define sealed method get-client-size
    (handle :: <HWND>)
 => (width :: <integer>, height :: <integer>)
  let (left, top, right, bottom) = get-client-edges(handle);
  values(right - left, bottom - top)
end method get-client-size;


/*
define sealed method map-dialog-rectangle
    (handle :: <HWND>,
     x :: <integer>, y :: <integer>, width :: <integer>, height :: <integer>)
 => (x :: <integer>, y :: <integer>, width :: <integer>, height :: <integer>)
  with-stack-structure (rect :: <LPRECT>)
    rect.left-value   := x;
    rect.top-value    := y;
    rect.right-value  := x + width;
    rect.bottom-value := y + height;
    MapDialogRect(handle, rect);
    let x      = rect.left-value;
    let y      = rect.top-value;
    let width  = rect.right-value  - rect.left-value;
    let height = rect.bottom-value - rect.top-value;
    values(x, y, width, height)
  end
end method map-dialog-rectangle;
*/


/// "Atomic" redisplay

//---*** Maybe move this to win32-user?
define macro with-delayed-drawing
  { with-delayed-drawing (?handle:name)
      ?body:body
    end }
 => { block ()
        SendMessage(?handle, $WM-SETREDRAW, $false, 0);
        ?body
      cleanup
        SendMessage(?handle, $WM-SETREDRAW, $true, 0);
        InvalidateRect(?handle, $NULL-RECT, #f)
      end }
end macro with-delayed-drawing;


/// String conversion utilities

// The rules for handling newlines:
//      \r\n => \r\n
//      \n   => \r\n
//      \r   => \r
define sealed method convert-to-windows-newlines
    (string :: <byte-string>)
 => (new-string :: <byte-string>)
  let n-newlines = count(curry(\==, '\n'), string);
  if (n-newlines = 0)
    string
  else
    let length :: <integer>     = size(string);
    let result :: <byte-string> = make(<byte-string>, size: length + n-newlines);
    without-bounds-checks
      let j :: <integer> = 0;           // index into 'result'
      for (i :: <integer> from 0 below length)
        let ch = string[i];
        case
          ch == '\n' =>
            result[j + 0] := '\r';
            result[j + 1] := '\n';
            inc!(j, 2);
          ch == '\r' =>
            let next = (i < length - 1) & string[i + 1];
            if (next == '\n')
              result[j + 0] := '\r';
              result[j + 1] := '\n';
              inc!(i, 1);               // skip both '\r' and '\n'
              inc!(j, 2)
            else
              result[j + 0] := '\r';
              inc!(j, 1)
            end;
          otherwise =>
            result[j] := ch;
            inc!(j, 1);
        end
      end
    end;
    result
  end
end method convert-to-windows-newlines;

// The rules for handling newlines:
//      \r\n => \n
//      \n   => \n
//      \r   => [gone]
// This is equivalent to just dropping \r when we see it.
define method convert-from-windows-newlines
    (string :: <byte-string>)
 => (new-string :: <byte-string>)
  let n-returns = count(curry(\==, '\r'), string);
  if (n-returns = 0)
    string
  else
    let length :: <integer>     = size(string);
    let result :: <byte-string> = make(<byte-string>, size: length - n-returns);
    without-bounds-checks
      let j :: <integer> = 0;           // index into 'result'
      for (i :: <integer> from 0 below length)
        let ch = string[i];
        unless (ch == '\r')
          result[j] := ch;
          inc!(j, 1)
        end
      end
    end;
    result
  end
end method convert-from-windows-newlines;
