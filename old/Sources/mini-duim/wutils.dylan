Module:    win32-duim
Synopsis:  Win32 back-end utilities
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Some useful macros

define macro table-definer
  { define table ?table-name:name :: ?table-type:expression
      ?entries
    end }
    => { define constant ?table-name :: ?table-type = make(?table-type);
         begin let the-table = ?table-name; ?entries end; }
 entries:
  { } => { }
  { ?key:expression => ?value:expression; ... }
    => { the-table[ ?key ] := ?value; ... }
end macro table-definer;

//---*** This should be in the FFI
define macro with-c-string
  { with-c-string (?c-string:name = ?string:expression, #rest ?options:*)
      ?body
    end }
    => { begin
	   let with-c-string-body = method (?c-string :: <C-string>) ?body end;
	   do-with-c-string(?string, with-c-string-body, ?options)
         end
         }
end macro with-c-string;

define method do-with-c-string
    (string :: <string>, function :: <function>, 
     #rest options, #key start: _start, end: _end)
 => (#rest values)
  ignore(_start, _end);
  let c-string :: <C-string> = TEXT(apply(copy-sequence, string, options));
  block ()
    function(c-string);
  cleanup
    //---*** Do we need to destroy this?
    // destroy(c-string)
  end
end method do-with-c-string;


/// Some useful functions

// Many Windows functions return #f or a NULL pointer in case of failure;
// the following function can be used to check the returned value and
// signal an error in case of failure.
define method check-result
    (name :: <string>, handle :: <C-pointer>) => (value :: <C-pointer>)
  if (null-pointer?(handle))
    report-error(name)
  end;
  handle
end method check-result;

define method check-result
    (name :: <string>, ok? :: <boolean>) => (value :: <boolean>)
  unless (ok?)
    report-error(name)
  end;
  ok?
end method check-result;

define function report-error (name :: <string>) => ()
  cerror("Try to continue anyway", "%s error %d", name, GetLastError())
end function report-error;

// For reporting unrecognized parameter values:
define function warn-invalid
    (kind :: <string>, value :: <object>) => ()
  cerror("Use default value", "Invalid %s: #=", kind, value)
end function warn-invalid;

define function not-yet-implemented
    (format-message :: <string>, #rest format-args) => ()
  apply(error, 
	concatenate(format-message, " not yet implemented!"),
	format-args)
end function not-yet-implemented;


/// Win32 metrics

define class <win32-metrics> (<object>)
  slot win32-mouse-buttons :: <integer>,
    required-init-keyword: mouse-buttons:;
  slot win32-pixels-per-inch :: <integer> = 100,
    init-keyword: pixels-per-inch:;
  slot win32-dialog-base-units :: <integer>,
    required-init-keyword: dialog-base-units:;
end class <win32-metrics>;

define method make-win32-metrics 
    () => (metrics :: <win32-metrics>)
  make(<win32-metrics>,
       dialog-base-units: 2,  //---*** Really: GetDialogBaseUnits(),
       mouse-buttons:     GetSystemMetrics($SM-CMOUSEBUTTONS))
end method make-win32-metrics;
