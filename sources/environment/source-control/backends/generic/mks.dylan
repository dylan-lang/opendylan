Module:    source-control-generic-backend
Synopsis:  Environment-Generic Source Control System Interface
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// MKS back-end

define sealed class <mks-source-control-system> (<generic-source-control-system>)
  keyword image-name:   = "mkssi32";
  keyword use-session?: = #f;
  keyword name:         = #"mks";
  keyword title:        = "MKS source control system";
end class <mks-source-control-system>;

define sealed domain make (singleton(<mks-source-control-system>));
define sealed domain initialize (<mks-source-control-system>);

define method note-source-control-system-selected
    (sccs :: <mks-source-control-system>) => ()
  *claim-command-string*     := *mks-claim-command-string*;
  *check-out-command-string* := *mks-check-out-command-string*;
  *check-in-command-string*  := *mks-check-in-command-string*;
  *abandon-command-string*   := *mks-abandon-command-string*;
  *merge-command-string*     := *mks-merge-command-string*;
  *diff-command-string*      := *mks-diff-command-string*;
  *report-command-string*    := *mks-report-command-string*;
  *add-command-string*       := *mks-add-command-string*;
  *remove-command-string*    := *mks-remove-command-string*;
end method note-source-control-system-selected;

define method string-for-argument
    (server :: <mks-source-control-system>, command :: <generic-source-control-command>,
     name == #"pathname", value :: false-or(<string>))
 => (string :: <string>)
  maybe-format-to-string("%s ", value)
end method string-for-argument;


/// The MKS commands
/// NB: no spaces after arguments because 'string-for-argument' does this

// 'note-source-control-system-selected' copies the MKS command strings
// into the current command strings, but the command string chooser
// updates and saves the back-end commands strings.

define variable *mks-claim-command-string*
  = "checkout $(pathname)";

define variable *mks-check-out-command-string*
  = "synchronize $(pathname)";

define variable *mks-check-in-command-string*
  = "checkin $(pathname)";

define variable *mks-abandon-command-string*
  = "undo $(pathname)";

define variable *mks-merge-command-string*
  = "resolve $(pathname)";

define variable *mks-diff-command-string*
  = "compare $(pathname)";

define variable *mks-report-command-string*
  = "report $(pathname)";

define variable *mks-add-command-string*
  = "add $(pathname)";

define variable *mks-remove-command-string*
  = "remove $(pathname)";


define method get-source-control-arguments
    (sccs :: <mks-source-control-system>,
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
