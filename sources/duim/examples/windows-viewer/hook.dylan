Module:    windows-hook
Author:    Andy Armstrong
Synopsis:  Windows viewer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $message-data = make(<LPCWPSTRUCT>);
define variable *hook-window* :: <HWND> = $NULL-HWND;

define method find-hook-window
    () => (handle :: false-or(<HWND>))
  let last-handle = *hook-window*;
  case
    ~null-handle?(last-handle) & IsWindow(last-handle) =>
      last-handle;
    otherwise =>
      let window = FindWindow($hook-class, $hook-window);
      unless (null-handle?(window))
	*hook-window* := window;
	window
      end
  end
end method find-hook-window;

define sealed method CallWndProc-hook
    (hc, wParam, lParam)
 => (value :: <signed-long>)
  let cwp :: <LPCWPSTRUCT> = make(<LPCWPSTRUCT>, address: lParam);
  if (hc >= 0 & ~null-pointer?(cwp))
    let window = find-hook-window();
    if (window)
      let handle = cwp.hWnd-value; 
      let message = cwp.message-value;
      let description = message-description($message-data, message);
      $message-data.hWnd-value    := handle;
      $message-data.message-value := message;
      $message-data.wParam-value  := cwp.wParam-value;
      $message-data.lParam-value  := cwp.lParam-value;
      // debug-message("Hook: %= %= %=",
      //	    hc, wParam, lParam);
      // debug-message("  hwnd:%= id:%= w:%= l:%=",
      //	    $message-data.hWnd-value.pointer-address,
      //	    $message-data.message-value,
      //	    $message-data.wParam-value,
      //	    $message-data.lParam-value);
      with-stack-structure (data :: <LPCOPYDATASTRUCT>)
	data.dwData-value := $message-id;
	data.cbData-value := size-of(<CWPSTRUCT>);
	data.lpData-value := $message-data;
	// debug-message("Sending message for %=: id:%= w:%= l:%=",
	//	      $message-data.hWnd-value.pointer-address,
	//	      $message-data.message-value,
	//	      $message-data.wParam-value,
	//	      $message-data.lParam-value);
	SendMessage(window, $WM-COPYDATA,
		    handle.pointer-address, data.pointer-address)
      end;
      with-stack-structure (data :: <LPCOPYDATASTRUCT>)
        with-c-string (c-string = description)
	  data.dwData-value := $description-id;
	  data.cbData-value := description.size + 1;
	  data.lpData-value := c-string;
	  SendMessage(window, $WM-COPYDATA,
		      handle.pointer-address, c-string.pointer-address)
	end
      end
    end
  end;
  CallNextHookEx(null-pointer(<HHOOK>), hc, wParam, lParam)
end method CallWndProc-hook;

define macro <HOOKPROC>-callback-wrapper
  { <HOOKPROC>-callback-wrapper(?new:name,?old:name) } =>
    { define C-callable-wrapper ?new of ?old
	parameter hc  :: <INT>;
	parameter wParam :: <WPARAM>;
	parameter lParam :: <LPARAM>;
	result    value :: <LRESULT>;
	c-modifiers: "__stdcall";
      end C-callable-wrapper }
end macro <HOOKPROC>-callback-wrapper;

define callback Hook-Proc :: <HOOKPROC> = CallWndProc-hook;


/// Give a brief textual description of a message

define method message-description
    (cwp :: <LPCWPSTRUCT>, message)
 => (description :: <string>)
  ""
end method message-description;

define method message-description
    (cwp :: <LPCWPSTRUCT>, message == $WM-WINDOWPOSCHANGED)
 => (description :: <string>)
  let pos = c-type-cast(<LPWINDOWPOS>, cwp.lParam-value);
  ""
/*  
  let flags = pos.flags-value;
  concatenate
    (if (%logbit?($SWP-NOMOVE, flags))
       ""
     else
       format-to-string("new pos:%=,%=", pos.x-value, pos.y-value)
     end,
     if (%logbit?($SWP-NOSIZE, flags))
       ""
     else
       format-to-string("new size:%=,%=", pos.cx-value, pos.cy-value)
     end)
*/
end method message-description;
