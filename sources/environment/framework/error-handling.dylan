Module:    environment-framework
Synopsis:  Environment Framework
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Useful constants

define constant $environment-user-exit-code   = 1;


/// ENVIRONMENT-HANDLER

define method environment-handler 
    (condition :: <serious-condition>, next-handler :: <function>)
  block ()
    do-environment-handler(condition, next-handler);
  exception (condition :: <serious-condition>)
    debug-message("Internal error in environment error handler: %s",
		  safe-condition-to-string(condition));
    default-handler(condition)
  end block;
end method environment-handler;


/// DO-ENVIRONMENT-HANDLER

define method do-environment-handler
    (condition :: <serious-condition>, next-handler :: <function>)
 => ()
  let (dialog, ok?) = choose-condition-action(condition);
  if (ok?)
    select (dialog.handler-dialog-action-pane.gadget-value)
      #"abort" => abort();
      #"exit"  => exit-application($environment-user-exit-code);
      #"debug" => default-handler(condition); // Cut out the other handlers now and invoke the debugger immediately.
    end select;
  else
    // If user manages to cancel dialog somehow, just abort I presume?
    abort();
  end if;
end method do-environment-handler;


/// DO-ENVIRONMENT-HANDLER

define method do-environment-handler
    (condition :: <out-of-memory-condition>, next-handler :: <function>)
 => ()
  let product-name = safe-release-product-name();
  let message
    = format-to-string
        ("%s is running very low on memory, so unsaved data may be lost.\n"
         "Please close any unused projects or applications.",
	 product-name);
  let owner = current-frame();
  notify-user(message,
              title: product-name,
              style: #"error",
              owner: if (owner & frame-mapped?(owner)) owner end)
end method do-environment-handler;


/// CHOOSE-CONDITION-ACTION

define method choose-condition-action 
    (condition :: <condition>)
 => (dialog :: <environment-handler-dialog>, ok? :: <boolean>)
  let framem = find-frame-manager();
  with-frame-manager (framem)
    let dialog
      = make(<environment-handler-dialog>,
	     owner: current-frame(),
	     condition: condition);
    values(dialog, start-dialog(dialog) & #t);
  end;
end method choose-condition-action;


/// $INTERNAL-ERROR-BITMAP

define variable $internal-error-bitmap = #f;


/// SAFE-RELEASE-PRODUCT-NAME
//
// Unfortunately, release-product-name isn't available until the edition
// specific DLL has been initialized, and this error handler can kick in
// before then. So just use "Open Dylan" if we can't do better,
// because we really shouldn't be crashing here.

define constant $product-name = "Open Dylan";

define function safe-release-product-name
    () => (name :: <string>)
  block ()
    release-product-name()
  exception (error :: <error>)
    $product-name
  end
end function safe-release-product-name;


/// SAFE-CONDITION-TO-STRING
//
// Be sure that we never crash trying to display a condition message.

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


/// <ENVIRONMENT-HANDLER-DIALOG>

define frame <environment-handler-dialog> (<dialog-frame>)
  keyword title: = safe-release-product-name();
  keyword cancel-callback: = #f;
  constant slot handler-dialog-condition :: <condition>,
    required-init-keyword: condition:;
  pane handler-dialog-message-pane (dialog)
    make(<text-editor>,
	 read-only?: #t, tab-stop?: #t,
	 border: #"sunken",
	 scroll-bars: #"vertical",
	 lines: 6, columns: 80);
  pane handler-dialog-action-pane (dialog)
    make(<radio-box>,
	 items: compute-handler-action-items(),
	 label-key: tail,
	 value-key: head,
	 orientation: #"vertical",
	 selection: vector(0));
  layout (dialog)
    horizontally (spacing: 8, y-alignment: #"top")
      make(<label>,
	   label: $internal-error-bitmap,
	   width:  32, min-width:  32, max-width:  32,
	   height: 32, min-height: 32, max-height: 32);
      vertically (spacing: 8, x-alignment: #"left")
        make(<label>,
	     label: "INTERNAL ERROR",
	     text-style: make(<text-style>, weight: #"bold", size: #"large"));
        dialog.handler-dialog-message-pane;
        dialog.handler-dialog-action-pane;
      end;
    end;
  input-focus (dialog)
    dialog.dialog-exit-button;
end frame <environment-handler-dialog>;

define method initialize
    (dialog :: <environment-handler-dialog>, #key)
  next-method();
  let condition-message 
    = safe-condition-to-string(dialog.handler-dialog-condition);
  dialog.handler-dialog-message-pane.gadget-value := condition-message;
end method initialize;


/// COMPUTE-HANDLER-ACTION-ITEMS

define method compute-handler-action-items 
    () => (items)
  let product-name = safe-release-product-name();
  local method add-product-name (item :: <pair>)
	  pair(head(item),
	       format-to-string(tail(item), product-name))
	end method;
  let items = #[#(#"abort" . "Abort the current operation and continue %s"),
		#(#"exit" . "Exit %s"),
		#(#"debug" . "Debug %s")];
  map(add-product-name, items)
end method compute-handler-action-items;

