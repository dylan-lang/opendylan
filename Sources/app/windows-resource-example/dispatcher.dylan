Module:    example-support
Synopsis:  Windows resource example
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <window-dispatcher> (<table>)
end;

define method table-protocol(t :: <window-dispatcher>)
  => (test-function :: <function>, hash-function :: <function>);
  values(window-equal, window-hash)
end;

define method window-equal (w1 :: <HWND>, w2 :: <HWND>)
  let result = (w1.pointer-address = w2.pointer-address);
/*  debug-out("w1= %= w2= %= =: %=\n", 
	    as(<integer>, w1),
	    as(<integer>, w2),
	    result);
*/
  result
end;

define method window-hash(hWnd :: <HWND>, hash-state :: <hash-state>)
  let (hid, hs) = object-hash(hWnd.pointer-address, hash-state);
  //debug-out("window hash: %=, %=\n", hid, hs);
  values(hid, hs)
end;

define variable *command-dispatcher* = make(<window-dispatcher>);
define variable *message-dispatcher* = make(<window-dispatcher>);

//define variable *command-dispatcher* = make(<equal-table>);
//define variable *message-dispatcher* = make(<equal-table>);

define variable *command-dispatching-tables* = make(<table>);
define variable *message-dispatching-tables* = make(<table>);

define class <command-table> (<object-table>) end;

define method add-command(ct :: <command-table>, 
			  commandId :: <unsigned-int>,
			  callback :: <function>) => ();
  debug-out("\tAdding handler for: %d \n", commandId);
  ct[commandId] := callback;
end;

define method register-command-handler(hWnd :: <HWND>,
				       commandId :: <unsigned-int>,
				       callback :: <function>) => ();
  register-handler(*command-dispatcher*, hWnd, commandId, callback);
end;

define method register-message-handler(hWnd :: <HWND>,
				       message :: <unsigned-int>,
				       callback :: <function>) => ();
  register-handler(*message-dispatcher*, hWnd, message, callback);
end;

define method register-handler(wd :: <window-dispatcher>,
			       hWnd :: <HWND>,
			       commandId :: <unsigned-int>,
			       callback :: <function>) => ();
  let window-table = element(wd, hWnd, default: #f);
  unless(window-table)
    window-table := wd[hWnd] := make(<object-table>);
  end;

  window-table[commandId] := callback;
end;

define method register-command-table-internal(hWnd :: <HWND>,
					      ct :: <command-table>) => ();
  debug-out("Registering command table for window: %=\n", 
	    hWnd.pointer-address);
  *command-dispatcher*[hWnd] := ct;
end;

define method register-message-table-internal(hWnd :: <HWND>,
					      ct :: <command-table>) => ();
  debug-out("Registering message table for window: %=\n", 
	    hWnd.pointer-address);
  *message-dispatcher*[hWnd] := ct;
end;

define method register-command-table(id :: <unsigned-int>,
				     ct :: <command-table>) => ();
  debug-out("(pre)Registering command table for : %d\n", id);
  *command-dispatching-tables*[id] := ct;
end;

define method register-message-table(id :: <unsigned-int>,
				     ct :: <command-table>) => ();
  debug-out("(pre)Registering message table for : %d\n", id); 
  *message-dispatching-tables*[id] := ct;
end;

define function dispatch-command(hWnd :: <HWND>,
				 commandId :: <unsigned-int>,
				 uParam :: <unsigned-int>,  
				 lParam :: <integer>) 
 => (handled? :: <boolean>);
  debug-out("Dispatching command: %d for window: %=\n", 
	    commandId, hWnd.pointer-address);
  dispatch-internal(*command-dispatcher*, hWnd, commandId, uParam, lParam);
end;

define function dispatch-message(hWnd :: <HWND>,
				 commandId :: <unsigned-int>,
				 uParam :: <unsigned-int>,  
				 lParam :: <integer>) 
 => (handled? :: <boolean>);
  debug-out("Dispatching message: %d for window: %=\n", 
	    commandId, hWnd.pointer-address);
  dispatch-internal(*message-dispatcher*, hWnd, commandId, uParam, lParam);
end;

define function dispatch-internal(dw :: <window-dispatcher>,
				  hWnd :: <HWND>,
				  commandId :: <unsigned-int>,
				  uParam :: <unsigned-int>,  
				  lParam :: <integer>) 
 => (handled? :: <boolean>);
  let window-table = element(dw, hWnd, default: #f);
  if(window-table)
    let callback = element(window-table, commandId, default: #f);
    if(callback)
      debug-out("Found callback: %= \n", callback);
      callback(hWnd, commandId, uParam, lParam);
      #t
    else
      debug-out("\tNo handler for: %d\n", commandId);
      #f
    end;
  else
    debug-out("No command table registered for window: %=\n", 
	      hWnd.pointer-address);
    #f
  end
end;

  
