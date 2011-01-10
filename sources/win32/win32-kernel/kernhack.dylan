Module:    Win32-Kernel
Synopsis:  Special cases not handled in automatically generated FFI code.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Handle this function as a special case to return two values
// instead of returning a structure. 

define C-function %GetLargestConsoleWindowSize
  parameter hConsoleOutput :: <HANDLE>;
  result value :: <C-unsigned-long>;
  c-name: "GetLargestConsoleWindowSize", c-modifiers: "__stdcall";
end;

define method GetLargestConsoleWindowSize( hConsoleOutput :: <HANDLE> )
		=> ( x :: <integer>, y :: <integer> );
  let s :: <integer> = %GetLargestConsoleWindowSize(hConsoleOutput);
  values( LOWORD(s), HIWORD(s) );
end;


// This one is handled specially because va_list is not yet supported, 
// but for our current purposes we can just pass a NULL pointer.
define C-function FormatMessage
  parameter dwFlags	 :: <DWORD>;
  parameter lpSource	 :: <LPCVOID>;
  parameter dwMessageId	 :: <DWORD>;
  parameter dwLanguageId :: <DWORD>;
  parameter lpBuffer	 :: <LPSTR>;
  parameter nSize	 :: <DWORD>;
  parameter _dummy	 :: <C-pointer>; // va_list *Arguments
  result val :: <DWORD>;
  c-name: "FormatMessageA", c-modifiers: "__stdcall";
end;


// Defined in "winnls.h" as ``typedef DWORD LCTYPE;'' but need to make sure
// that this allows full 32-bit values even if <DWORD> doesn't.
define constant <LCTYPE> = <C-both-unsigned-long>;

// some pointer-to-pointer type names created by the translation process:
define C-pointer-type <LPLPSTR> => <LPSTR>;
define inline constant <LPLPCSTR> = <LPLPSTR>;
define C-pointer-type <LPLPOVERLAPPED> => <LPOVERLAPPED>;

define inline constant <LPLARGE-INTEGER> = <PLARGE-INTEGER>;
define constant <DWORDLONG> = <LARGE-INTEGER>; // 64-bit integer
define constant <PVOID64> = <C-void*>;


/// Error handling utilities

define function win32-error-message
    (error-code :: type-union(<integer>, <machine-word>))
 => (message :: false-or(<byte-string>));
  let buf-size :: <integer> = 600;
  with-stack-structure (buffer :: <LPTSTR>, size: buf-size)
    let length :: <integer> =
      FormatMessage(logior($FORMAT-MESSAGE-FROM-SYSTEM,
			   $FORMAT-MESSAGE-IGNORE-INSERTS),
		    $NULL-VOID,
		    error-code,
		    $LANG-USER-DEFAULT,
		    buffer,
		    buf-size,
		    $NULL-VOID);
    if ( length <= 0 )
      #f
    else
      let last :: <integer> = length - 1;
      while ( last >= 0 & buffer[last] <= '\r' ) // remove trailing CRLF
	buffer[last] := '\0';
	last := last - 1;
      end while;
      as(<byte-string>, buffer)
    end if
  end with-stack-structure;
end;

// Many Windows functions return #f or a NULL pointer in case of failure;
// the following function can be used to check the returned value and
// signal an error in case of failure.
define method check-win32-result
    (name :: <string>, handle :: <C-pointer>) => (value :: <C-pointer>)
  if (null-pointer?(handle))
    report-win32-error(name)
  end;
  handle
end method check-win32-result;

define method check-win32-result
    (name :: <string>, ok? :: <boolean>) => (value :: <boolean>)
  unless (ok?)
    report-win32-error(name)
  end;
  ok?
end method check-win32-result;

define function report-win32-error
    (name :: <string>, #key error) => ()
  let error = error | GetLastError();
  unless (error == $NO-ERROR)
    cerror("Try to continue anyway",
	   "%s error %d: %s",
	   name, error, win32-error-message(error))
  end
end function report-win32-error;

define function ensure-no-win32-error (name :: <string>) => ()
  let error = GetLastError();
  unless (error == $NO-ERROR)
    report-win32-error(name, error: error)
  end
end function ensure-no-win32-error;


/// Accessors for "WinMain" parameters:
///   [NOTE: The concept of WinMain is strictly a Microsoft C run-time invention
///    which has been cast in concrete by its inclusion in the Win32 API]

define function application-instance-handle () => (hInstance :: <HINSTANCE>)
  pointer-cast(<HINSTANCE>, GetModuleHandle($NULL-string))
end function application-instance-handle;

define variable *application-command-line* = #f;

define inline-only function ensure-application-command-line () => ()
  unless (*application-command-line*)
    let command-line = GetCommandLine();
    // Strip the command name --
    //   This code is cribbed from the Operating-System library to get the hairy
    //   Win32 quoting convention correct...
    local method whitespace? (c :: <character>) => (whitespace? :: <boolean>)
	    c = ' ' | c = '\t' | c = '\n'
	  end method whitespace?;
    local method skip-whitespace 
	      (string :: <C-string>, _start :: <integer>, _end :: <integer>)
	   => (_new-start :: <integer>)
	    while (_start < _end & whitespace?(string[_start]))
	      _start := _start + 1
	    end;
	    _start
	  end method skip-whitespace;
    let _start :: <integer> = 0;
    let _end :: <integer> = size(command-line);
    _start := skip-whitespace(command-line, _start, _end);
    if (_start < _end)
      let escaped? :: <boolean> = #f;
      let escaped-count :: <integer> = 0;
      let quoted? :: <boolean> = #f;
      let done? :: <boolean> = #f;
      while (_start < _end & ~done?)
	let c :: <character> = command-line[_start];
	case
	  escaped? & c = '\\' =>
	    escaped-count := escaped-count + 1;
	  escaped? & c = '"' =>
	    if (even?(escaped-count))
	      escaped? := #f;
	      _start := _start - 1;
	    else
	      escaped? := #f;
	    end;
	  escaped? =>
	    escaped? := #f;
	  quoted? & whitespace?(c) =>
	    #f;
	  c = '\\' =>
	    escaped? := #t;
	    escaped-count := 1;
	  c = '"' =>
	    quoted? := ~quoted?;
	  whitespace?(c) =>
	    done? := #t;
	  otherwise =>
	    #f;
	end;
	_start := _start + 1
      end;
      _start := skip-whitespace(command-line, _start, _end);
    end;
    if (_start < _end)
      *application-command-line* := copy-sequence(command-line, start: _start)
    else
      *application-command-line* := C-string-constant("")
    end
  end
end function ensure-application-command-line;

/// Defined to return the command line without the command name ...
define function application-command-line () => (lpCmdLine :: <LPSTR>)
  ensure-application-command-line();
  *application-command-line*
end function application-command-line;

///---*** NOTE: Really belongs in win32-user but there are naming conflicts
///---*** that we don't have time to solve at this time...
define function application-show-window () => (nCmdShow :: <signed-int>)
  with-stack-structure (startup-info :: <LPSTARTUPINFO>)
    startup-info.dwFlags-value := 0;
    GetStartupInfo(startup-info);
    if ( ~ zero?(%logand(startup-info.dwFlags-value, $STARTF-USESHOWWINDOW)) )
      startup-info.wShowWindow-value
    else
      10			//---*** $SW-SHOWDEFAULT
    end
  end
end function application-show-window;
