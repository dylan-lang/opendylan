Module:    Win32-default-handler
Synopsis:  Display unhandled error conditions in a pop-up message window.
Author:    David N. Gray, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// For an OLE Control application, this variable will be set to the handle
// of the top-level DLL so that we don't show the pathname of the
// container application. 
define variable *error-module-handle* :: <HMODULE> = $NULL-HINSTANCE;

/// WIN32-LAST-HANDLER

define method win32-last-handler (condition :: <serious-condition>, next-handler :: <function>)
 => ()
  let condition-string :: <string> = safe-condition-to-string(condition);
  OutputDebugString(condition-string);
  OutputDebugString("\r\n");
  let message :: <string> =
    format-to-string("Unhandled Dylan error:\n\n"
		     "%s\n\n"
		     "Do you want to quit?\n\n"
		     "Press YES to quit, NO to ignore or CANCEL to debug.",
		     condition-string);

  // Use the executable file name as the window title
  let buf-size = $MAX-PATH;
  let szFullPath :: <LPTSTR> = make(<LPTSTR>, size: buf-size);
  let module-handle :: <HMODULE> = 
    if ( null-handle?(*error-module-handle*) )
      application-instance-handle()
    else
      *error-module-handle*
    end if;
  let path-length :: <integer> =
    GetModuleFileName(module-handle, szFullPath, buf-size);

  let button :: <integer> =
    MessageBox($NULL-HWND, 
	       message, /* message text */
	       if(path-length > 0) szFullPath else "Dylan" end if, /* title */
	       logior($MB-YESNOCANCEL,	 // buttons
		      $MB-ICONSTOP,	 // icon
		      $MB-SETFOREGROUND, // appear at front
		      $MB-TASKMODAL)	 // freeze all windows of application
		 );
  destroy(szFullPath);
  select (button)
    $IDYES    => ExitProcess(0);
    $IDNO     => abort();
    $IDCANCEL => default-handler(condition); // Cut out the other handlers now and invoke the debugger immediately.
  end select;
end method;

define method safe-condition-to-string
    (condition :: <condition>) => (string :: <string>)
  block ()
    block ()
      format-to-string("%s", condition);
    exception (print-error :: <error>)
      format-to-string("%=\nsignalled while trying to print an instance of %=",
		       print-error, object-class(condition));
    end block;
  exception (error :: <error>)
    "*** Crashed trying to print condition ***"
  end
end method safe-condition-to-string;


/// LAST-HANDLER

define last-handler <serious-condition> = win32-last-handler;
