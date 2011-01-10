Module:    source-control-generic-backend
Synopsis:  Environment-Generic Source Control System Interface
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Perforce back-end

define sealed class <perforce-source-control-system> (<generic-source-control-system>)
  keyword image-name:   = "p4";
  keyword use-session?: = #f;
  keyword name:         = #"perforce";
  keyword title:        = "Perforce source control system";
end class <perforce-source-control-system>;

define sealed domain make (singleton(<perforce-source-control-system>));
define sealed domain initialize (<perforce-source-control-system>);

define method note-source-control-system-selected
    (sccs :: <perforce-source-control-system>) => ()
  *claim-command-string*     := *perforce-claim-command-string*;
  *check-out-command-string* := *perforce-check-out-command-string*;
  *check-in-command-string*  := *perforce-check-in-command-string*;
  *abandon-command-string*   := *perforce-abandon-command-string*;
  *merge-command-string*     := *perforce-merge-command-string*;
  *diff-command-string*      := *perforce-diff-command-string*;
  *report-command-string*    := *perforce-report-command-string*;
  *add-command-string*       := *perforce-add-command-string*;
  *remove-command-string*    := *perforce-remove-command-string*;
end method note-source-control-system-selected;

define method string-for-argument
    (server :: <perforce-source-control-system>, command :: <generic-source-control-command>,
     name == #"pathname", value :: false-or(<string>))
 => (string :: <string>)
  maybe-format-to-string("%s ", value)
end method string-for-argument;


/// The Perforce commands
/// NB: no spaces after arguments because 'string-for-argument' does this

// 'note-source-control-system-selected' copies the Perforce command strings
// into the current command strings, but the command string chooser
// updates and saves the back-end commands strings.

//---*** These all need to do a "submit" to submit the changes

define variable *perforce-claim-command-string*
  = "edit $(pathname)";

define variable *perforce-check-out-command-string*
  = "sync $(pathname)";

define variable *perforce-check-in-command-string*
  = "add $(pathname)";

define variable *perforce-abandon-command-string*
  = "revert $(pathname)";

define variable *perforce-merge-command-string*
  = "resolve $(pathname)";

define variable *perforce-diff-command-string*
  = "diff $(pathname)";

define variable *perforce-report-command-string*
  = "filelog $(pathname)";

define variable *perforce-add-command-string*
  = "add $(pathname)";

define variable *perforce-remove-command-string*
  = "delete $(pathname)";


define method get-source-control-arguments
    (sccs :: <perforce-source-control-system>,
     #key pathname :: false-or(<pathname>) = #f, reason? = #f, owner)
 => (pathname :: false-or(<pathname>),
     compound :: false-or(<string>), unit :: false-or(<string>), branch :: false-or(<string>),
     reason   :: false-or(<string>))
  let frame    = sheet-frame(top-level-sheet(owner));
  let pathname = choose-file(owner: frame,
			     direction: #"input",
			     default: pathname,
			     if-does-not-exist: #"ask");
  if (pathname)
    let locator  = as(<file-locator>, pathname);
    let compound = as(<string>, locator-directory(locator));
    let unit     = as(<string>, locator-name(locator));
    let branch   = #f;
    let reason   = #f;
    values(pathname, compound, unit, branch, reason)
  else
    values(#f, #f, #f, #f, #f)
  end
end method get-source-control-arguments;
