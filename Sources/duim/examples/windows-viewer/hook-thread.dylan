Module:       windows-viewer
Author:       Andy Armstrong
Synopsis:     Windows viewer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *hook* :: false-or(<HHOOK>) = #f;
define variable *hook-callback* :: false-or(<function>) = #f;
define variable *last-message* :: false-or(<window-message>) = #f;

define method create-hook-thread
    () => ()
  make(<thread>,
       name: "Hook thread",
       function: run-hook-loop)
end method create-hook-thread;

define method run-hook-loop
    () => ()
  register-hook-class();
  // Make a hidden window to receive hook messages
  let hook-window
    = CreateWindowEx
        (0,
	 $hook-class,
	 $hook-window,
	 $WS-OVERLAPPEDWINDOW,
	 0, 0, 0, 0,
	 $NULL-HWND,
	 $null-hMenu,
	 application-instance-handle(),
	 $NULL-VOID);
  check-win32-result("CreateWindow", hook-window);
  with-stack-structure (message :: <LPMSG>)
    while (IsWindow(hook-window)
	     & GetMessage(message, hook-window, 0, 0))
      TranslateMessage(message);
      DispatchMessage(message)
    end
  end
end method run-hook-loop;

define function register-hook-class
    () => ()
  with-stack-structure (wc :: <PWNDCLASS>)
    // Fill in window class structure with parameters that describe the
    // main window.
    wc.lpszClassName-value := $hook-class;
    wc.style-value         := 0;
    wc.lpfnWndProc-value   := WndProc;
    wc.cbClsExtra-value    := 0;
    wc.cbWndExtra-value    := 0;
    wc.hInstance-value     := application-instance-handle(); // owner
    wc.hIcon-value         := null-pointer(<HICON>);
    wc.hCursor-value       := null-pointer(<HCURSOR>);
    wc.hbrBackground-value := as(<HBRUSH>, 1 + $COLOR-3DFACE);
    wc.lpszMenuName-value  := $NULL-string;	// no menu yet
    let class-id = RegisterClass(wc);
    when (zero?(class-id))			// register the window class
      report-win32-error("RegisterClass")
    end;
    class-id
  end
end function register-hook-class;

define sealed method hook-window-callback
    (handle :: <HWND>, message, wParam, lParam)
 => (result)
  let return-code :: false-or(<integer>)
    = select (message)
	$WM-COPYDATA =>
	  let data :: <LPCOPYDATASTRUCT> 
	    = make(<LPCOPYDATASTRUCT>, address: lParam);
	  select (data.dwData-value)
	    $message-id =>
	      let cwp :: <LPCWPSTRUCT>
		= pointer-cast(<LPCWPSTRUCT>, data.lpData-value);
	      let message
		= make(<window-message>,
		       handle: cwp.hWnd-value,
		       id:     cwp.message-value,
		       wParam: cwp.wParam-value,
		       lParam: cwp.lParam-value);
	      // format-out("Message received: %=:\n  id:%= w:%= l:%=\n", 
	      // 	     message.message-handle.pointer-address,
	      //	     message.message-id,
	      //	     message.message-wParam,
	      //	     message.message-lParam);
	      *hook-callback*(message);
	      *last-message* := message;
	      $true;
	    $description-id =>
	      let message = *last-message*;
	      if (message)
		let text = pointer-cast(<C-string>, data.lpData-value);
		message.message-description := as(<byte-string>, text)
	      end;
	    otherwise =>
	      format-out("Unexpected data: %=\n", data.dwData-value);
	  end;
	$WM-DESTROY =>
	  PostQuitMessage(0);
	  0;
	otherwise =>
	  #f;
      end;
  return-code
    | DefWindowProc(handle, message, wParam, lParam)
end method hook-window-callback;

define callback WndProc :: <WNDPROC> = hook-window-callback;


define function install-windows-hooks
    (module :: <HMODULE>, thread, callback :: <function>) => ()
  uninstall-windows-hooks();
  *hook* := SetWindowsHookEx($WH-CALLWNDPROC, Hook-Proc, module, thread);
  if (null-handle?(*hook*))
    let error = GetLastError();
    unless (error == $NO-ERROR)
      cerror("Try to continue anyway",
	     "SetWindowsHookEx error %d: %s",
	     error, win32-error-message(error))
    end
  end;
  *hook-callback* := callback
end function install-windows-hooks;

define function uninstall-windows-hooks
    () => (okay? :: <boolean>)
  if (*hook*)
    UnhookWindowsHookEx(*hook*);
    *hook* := #f
  else
    #t
  end
end function uninstall-windows-hooks;
