Module:    win32-resources-internal
Synopsis:  Windows resource decoding
Author:    Roman Budzianowski, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//--- We can't use <LPARAM> or <WPARAM> because they are C types, so we just
//--- use <object> rather than something more specific to speed up dispatch
// define constant <wparam-type>  = <object>;
define constant <lparam-type>  = <object>;

define C-pointer-type <LPCDLGITEMTEMPLATEA> => <DLGITEMTEMPLATE>;

define C-pointer-type <WORD*> => <WORD>;

define inline function pointer+
    (p :: <C-statically-typed-pointer>, offset :: <integer>)
 => (p :: <C-statically-typed-pointer>)
  pointer-value-address(p, index: offset)
end function pointer+;

define inline function pointer-address-16
    (p) => (p-16)
  %logand(\%+(p, 1), as(<machine-word>, #xFFFFFFFE))
end function pointer-address-16;

define inline function pointer-address-32
    (p) => (p-32)
  %logand(\%+(p, 3), as(<machine-word>, #xFFFFFFFC))
end function pointer-address-32;


/// Error handling

// define constant $false    :: <integer> = 0;
// define constant $true     :: <integer> = 1;

define constant $NO_ERROR :: <integer> = 0;

define variable *debug-resources?* :: <boolean> = #f;

// A simpler debug message since the console debugger is too slow!
define function debug-message
    (format-string :: <string>, #rest format-arguments) => ()
  when (*debug-resources?*)
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
  end
end function debug-message;

// Many Windows functions return #f or a NULL pointer in case of failure;
// the following function can be used to check the returned value and
// signal an error in case of failure.
define sealed method check-result
    (name :: <string>, handle :: <C-pointer>) => (value :: <C-pointer>)
  when (null-pointer?(handle))
    report-error(name)
  end;
  handle
end method check-result;

define sealed method check-result
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
