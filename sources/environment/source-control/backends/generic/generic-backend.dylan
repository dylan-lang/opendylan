Module:    source-control-generic-backend
Synopsis:  Environment-Generic Source Control System Interface
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Generic EXE back-end

define sealed class <generic-source-control-system> (<source-control-system>)
  // The name of the image to be used as the server
  sealed constant slot %image-name :: <string>  = "",
    init-keyword: image-name:;
  // When #t, this starts a server session and uses it persistently
  // When #f, this just uses 'os/run-application' (yech!)
  sealed constant slot %use-session? :: <boolean> = #f,
    init-keyword: use-session?:;
end class <generic-source-control-system>;

define sealed domain make (singleton(<generic-source-control-system>));
define sealed domain initialize (<generic-source-control-system>);

define method note-source-control-system-selected
    (sccs :: <generic-source-control-system>) => ()
  *claim-command-string*     := *default-claim-command-string*;
  *check-out-command-string* := *default-check-out-command-string*;
  *check-in-command-string*  := *default-check-in-command-string*;
  *abandon-command-string*   := *default-abandon-command-string*;
  *merge-command-string*     := *default-merge-command-string*;
  *diff-command-string*      := *default-diff-command-string*;
  *report-command-string*    := *default-report-command-string*;
  *add-command-string*       := *default-add-command-string*;
  *remove-command-string*    := *default-remove-command-string*;
end method note-source-control-system-selected;


define open abstract class <generic-source-control-command>
    (<source-code-control-command>, <basic-string-command>)
end class <generic-source-control-command>;

define method initialize
    (command :: <generic-source-control-command>, #key compound, pathname) => ()
  next-method();
  when (pathname & ~compound)
    let (compound, unit, branch) = get-compound-and-unit(pathname);
    sccs-command-compound(command) := compound;
    sccs-command-unit(command)     := unit;
    sccs-command-branch(command)   := branch;
  end
end method initialize;

define method execute-command
    (command :: <generic-source-control-command>) => (#rest values)
  let sccs = command-server(command);
  ensure-server-started(sccs);
  let pathname  = sccs-command-pathname(command);
  let locator   = pathname & as(<file-locator>, pathname);
  let directory = locator  & locator-directory(locator);
  if (directory)
    let old-directory = working-directory();
    block ()
      working-directory() := directory;
      do-source-control-commands(sccs, string-for-command(command));
    cleanup
      working-directory() := old-directory
    end
  else
    do-source-control-commands(sccs, string-for-command(command))
  end
end method execute-command;


define method ensure-server-started
    (sccs :: <generic-source-control-system>)
  when (sccs.%use-session?)
    #f		//--- implement the "session" code here
  end
end method ensure-server-started;

define method do-source-control-commands
    (sccs :: <generic-source-control-system>, #rest strings)
  if (sccs.%use-session?)
    // Execute the commands one at a time
    for (string :: <string> in strings)
      do-source-control-command(sccs, string)
    end
  else
    // Concatenate the commands, then execute them as one
    let strings = as(<list>, strings);
    let string  = head(strings);
    let strings = tail(strings);
    while (~empty?(strings))
      string  := concatenate-as(<string>, string, "; ", head(strings));
      strings := tail(strings)
    end;
    do-source-control-command(sccs, string)
  end
end method do-source-control-commands;

define method do-source-control-command
    (sccs :: <generic-source-control-system>, string :: <string>)
  if (sccs.%use-session?)
    #f		//--- implement the "session" code here
  else
    //--- This already waits, but don't we want to return a status code?
    run-application(concatenate-as(<string>, sccs.%image-name, " ", string),
		    under-shell?: #t)
  end
end method do-source-control-command;


define method string-for-argument
    (server :: <generic-source-control-system>, command :: <generic-source-control-command>,
     name == #"pathname", value :: false-or(<string>))
 => (string :: <string>)
  maybe-format-to-string("-pathname %s ", value)
end method string-for-argument;

define method string-for-argument
    (server :: <generic-source-control-system>, command :: <generic-source-control-command>,
     name == #"compound", value :: false-or(<string>))
 => (string :: <string>)
  maybe-format-to-string("-compound %s ", value)
end method string-for-argument;

define method string-for-argument
    (server :: <generic-source-control-system>, command :: <generic-source-control-command>,
     name == #"unit", value :: false-or(<string>))
 => (string :: <string>)
  maybe-format-to-string("-unit %s ", value)
end method string-for-argument;

define method string-for-argument
    (server :: <generic-source-control-system>, command :: <generic-source-control-command>,
     name == #"branch", value :: false-or(<string>))
 => (string :: <string>)
  maybe-format-to-string("-branch %s ", value)
end method string-for-argument;

define method string-for-argument
    (server :: <generic-source-control-system>, command :: <generic-source-control-command>,
     name == #"reason", value :: false-or(<string>))
 => (string :: <string>)
  maybe-format-to-string("-reason \"%s\" ", value)
end method string-for-argument;

define inline function maybe-format-to-string
    (format-string :: <string>, value :: false-or(<string>))
 => (string :: <string>)
  if (value & ~empty?(value))
    format-to-string(format-string, value)
  else
    ""
  end
end function maybe-format-to-string;


define macro generic-command-definer
  { define generic-command ?:name ?slots:* end }
    => { define sealed string-command "<generic-" ## ?name ## "-command>"
	     ("<sccs-" ## ?name ## "-command>", <generic-source-control-command>)
	   inherited named-argument pathname is sccs-command-pathname;
	   inherited named-argument compound is sccs-command-compound;
	   inherited named-argument unit     is sccs-command-unit;
	   inherited named-argument branch   is sccs-command-branch;
	   inherited named-argument reason   is sccs-command-reason;
	   ?slots
	 end;
	 define sealed domain make (singleton("<generic-" ## ?name ## "-command>"));
	 define sealed domain initialize ("<generic-" ## ?name ## "-command>");
	 define sealed method class-for-sccs-command
	     (sccs :: <generic-source-control-system>, class == "<sccs-" ## ?name ## "-command>")
	  => (class == "<generic-" ## ?name ## "-command>")
	   "<generic-" ## ?name ## "-command>"
	 end method class-for-sccs-command; }
end macro generic-command-definer;


/// The default generic commands
/// NB: no spaces after arguments because 'string-for-argument' does this

// 'note-source-control-system-selected' copies the default command strings
// into the current command strings, but the command string chooser
// updates and saves the back-end commands strings.

define variable *default-claim-command-string*
  = "checkout -claim soft $(compound)$(unit)$(branch)$(reason)";

define generic-command claim
  keyword pattern-string: = *claim-command-string*;
end;


define variable *default-check-out-command-string*
  = "checkout $(compound)$(unit)$(branch)";

define generic-command check-out
  keyword pattern-string: = *check-out-command-string*;
end;


define variable *default-check-in-command-string*
  = "checkin $(compound)$(unit)$(branch)$(reason)";

define generic-command check-in
  keyword pattern-string: = *check-in-command-string*;
end;


define variable *default-abandon-command-string*
  = "abandon $(compound)$(unit)$(branch)";

define generic-command abandon
  keyword pattern-string: = *abandon-command-string*;
end;


define variable *default-merge-command-string*
  = "merge $(compound)$(unit)$(branch)$(reason)";

define generic-command merge
  keyword pattern-string: = *merge-command-string*;
end;


define variable *default-diff-command-string*
  = "diff $(compound)$(unit)$(branch)";

define generic-command diff
  keyword pattern-string: = *diff-command-string*
end;


define variable *default-report-command-string*
  = "report $(compound)$(unit)$(branch)";

define generic-command report
  keyword pattern-string: = *report-command-string*;
end;


define variable *default-add-command-string*
  = "add $(compound)$(unit)$(branch)$(reason)";

define generic-command add
  keyword pattern-string: = *add-command-string*;
end;


define variable *default-remove-command-string*
  = "remove $(compound)$(unit)$(branch)$(reason)";

define generic-command remove
  keyword pattern-string: = *remove-command-string*;
end;


/// Source control argument gathering

define method get-source-control-arguments
    (sccs :: <generic-source-control-system>,
     #key pathname :: false-or(<pathname>) = #f, reason? = #f, owner)
 => (pathname :: false-or(<pathname>),
     compound :: false-or(<string>), unit :: false-or(<string>), branch :: false-or(<string>),
     reason   :: false-or(<string>))
  let frame  = sheet-frame(top-level-sheet(owner));
  let framem = frame-manager(frame);
  let (compound, unit, branch)
    = if (pathname) get-compound-and-unit(pathname)
      else values(#f, #f, #f) end;
  with-frame-manager (framem)
    let min-width = 250;
    let compound-field
      = make(<text-field>,
	     min-width: min-width,
	     value: compound | "",
	     value-changing-callback:
	       method (gadget)
		 let dialog = sheet-frame(gadget);
		 dialog-exit-enabled?(dialog) := ~empty?(gadget-value(gadget))
	       end method);
    let unit-field
      = make(<text-field>,
	     min-width: min-width,
	     value: unit | "");
    let branch-field
      = make(<text-field>,
	     min-width: min-width,
	     value: branch | "");
    let reason-field
      = reason? & make(<text-field>,
		       min-width: min-width);
    let layout
      = if (reason?)
	  make(<table-layout>,
	       x-spacing: 8, y-spacing: 2,
	       contents: vector(vector(make(<label>, label: "&Compound:"), compound-field),
				vector(make(<label>, label: "&Unit:"),     unit-field),
				vector(make(<label>, label: "&Branch:"),   branch-field),
				vector(make(<label>, label: "&Reason:"),   reason-field)))
	else
	  make(<table-layout>,
	       x-spacing: 8, y-spacing: 2,
	       contents: vector(vector(make(<label>, label: "&Compound:"), compound-field),
				vector(make(<label>, label: "&Unit:"),     unit-field),
				vector(make(<label>, label: "&Branch:"),   branch-field)))
	end;
    let dialog = make(<dialog-frame>,
		      title: "Select Compound and Unit",
		      layout: layout,
		      input-focus: compound-field,
		      mode: #"modal",
		      owner: frame);
    let exit-status = start-dialog(dialog);
    if (exit-status & ~empty?(gadget-value(compound-field)))
      values(pathname,
	     gadget-value(compound-field),
	     ~empty?(gadget-value(unit-field))   & gadget-value(unit-field),
	     ~empty?(gadget-value(branch-field)) & gadget-value(branch-field),
	     reason? & ~empty?(gadget-value(reason-field)) & gadget-value(reason-field))
    else
      values(#f, #f, #f, #f, #f)
    end
  end
end method get-source-control-arguments;

define method get-compound-and-unit
    (pathname :: <pathname>)
 => (compound :: false-or(<string>), unit :: false-or(<string>), branch :: false-or(<string>))
  values(#f, #f, #f)
end method get-compound-and-unit;
