Module:    Win32-duim
Synopsis:  Display unhandled error conditions in a pop-up message window.
Author:    David N. Gray, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// WIN32-LAST-HANDLER.  Really should have some more duimish thing...

define method win32-duim-last-handler (condition :: <serious-condition>, next-handler :: <function>)
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

  // Use the executable file name as the window title.  Unfortunately
  // for OLE controls, this is a bit misleading, but have instituted
  // no way to get at the OLE control's module-handle.
  let buf-size = $MAX-PATH;
  let szFullPath :: <LPTSTR> = make(<LPTSTR>, size: buf-size);
  let module-handle :: <HMODULE> = application-instance-handle();
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

define last-handler <serious-condition> = win32-duim-last-handler;
