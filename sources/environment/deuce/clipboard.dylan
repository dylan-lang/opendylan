Module:    environment-deuce
Synopsis:  Environment Deuce
Author:    Scott McKay, Hugh Greene, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Clipboard implementation

define method cut-object?
    (gadget :: <environment-deuce-pane>, target :: <deuce-command-target>)
 => (cut? :: <boolean>)
  window-mark(gadget) ~= #f & ~target-read-only?(target)
end method cut-object?;

define method cut-object?
    (gadget :: <environment-deuce-pane>, target :: <buffer-command-target>)
 => (cut? :: <boolean>)
  #f
end method cut-object?;

define method cut-object
    (gadget :: <environment-deuce-pane>, target :: <deuce-command-target>) => ()
  if (cut-object?(gadget, target))
    deuce/execute-command-in-frame(window-frame(gadget), cut-region)
  else
    clipboard-function-disabled-message(gadget, target, #"cut")
  end
end method cut-object;


define method copy-object?
    (gadget :: <environment-deuce-pane>, target :: <deuce-command-target>)
 => (copy? :: <boolean>)
  window-mark(gadget) ~= #f
  | begin
      let policy = editor-policy(frame-editor(window-frame(gadget)));
      unselected-copy-policy(policy) == #"copy-line"
    end
end method copy-object?;

define method copy-object?
    (gadget :: <environment-deuce-pane>, target :: <buffer-command-target>)
 => (copy? :: <boolean>)
  #f
end method copy-object?;

define method copy-object
    (gadget :: <environment-deuce-pane>, target :: <deuce-command-target>) => ()
  if (copy-object?(gadget, target))
    deuce/execute-command-in-frame(window-frame(gadget), copy-region)
  else
    clipboard-function-disabled-message(gadget, target, #"copy")
  end
end method copy-object;


define method paste-object?
    (gadget :: <environment-deuce-pane>, target :: <deuce-command-target>)
 => (paste? :: <boolean>)
  ~target-read-only?(target)
  & with-clipboard (clipboard = gadget)
      clipboard & clipboard-data-available?(<string>, clipboard)
  end
end method paste-object?;

define method paste-object
    (gadget :: <environment-deuce-pane>, target :: <deuce-command-target>) => ()
  if (paste-object?(gadget, target))
    deuce/execute-command-in-frame(window-frame(gadget), paste)
  else
    clipboard-function-disabled-message(gadget, target, #"paste")
  end
end method paste-object;


define method delete-object?
    (gadget :: <environment-deuce-pane>, target :: <deuce-command-target>)
 => (delete? :: <boolean>)
  //--- We don't test for a region, because typing Delete should delete
  //--- a single character if there is no region.  Yech.
  ~target-read-only?(target)
end method delete-object?;

define method delete-object?
    (gadget :: <environment-deuce-pane>, target :: <buffer-command-target>)
 => (delete? :: <boolean>)
  #f
end method delete-object?;

define method delete-object
    (gadget :: <environment-deuce-pane>, target :: <deuce-command-target>) => ()
  if (delete-object?(gadget, target))
    deuce/execute-command-in-frame(window-frame(gadget), delete-region)
  else
    clipboard-function-disabled-message(gadget, target, #"delete")
  end
end method delete-object;


define function clipboard-function-disabled-message
    (gadget :: <environment-deuce-pane>, target :: <deuce-command-target>,
     type :: <symbol>) => ()
  let read-only-modification? = (type ~== #"copy" & target-read-only?(target));
  let message
    = case
	read-only-modification? =>
	  format-to-string("Cannot %s as this file is read-only.", type);
	type == #"paste" =>
	  "Cannot paste into this file.";
	otherwise =>
	  format-to-string("Cannot %s from this file.", type);
      end;
  environment-error-message(message, owner: sheet-frame(gadget))
end function clipboard-function-disabled-message;
