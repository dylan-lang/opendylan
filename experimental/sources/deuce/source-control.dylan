Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Source version control protocol

define constant <source-control-operation>
    = one-of(#"claim", #"check-in", #"check-out", #"abandon",
	     #"merge", #"diff", #"report",
	     #"add", #"remove");

// This takes a 'window' argument in case the back-end needs to query
define open generic do-source-control-operation
    (window :: <window>, operation :: <source-control-operation>,
     #key pathname :: false-or(<pathname>),
	  reason?  :: <boolean>)
 => (success? :: <boolean>, pathname :: false-or(<pathname>), message :: false-or(<string>));

define method do-source-control-operation
    (window :: <window>, operation :: <source-control-operation>,
     #key pathname :: false-or(<pathname>),
	  reason?  :: <boolean>)
 => (success? :: <boolean>, pathname :: false-or(<pathname>), message :: false-or(<string>))
  values(#f, #f, "Don't know how to do any source control operations!")
end method do-source-control-operation;


/// Source version control commands

define command vc-claim (frame)
    "Claim a compound or unit from the source control system."
  do-vc-command(frame, #"claim",
		reason?: #t, revert?: #t)
end command vc-claim;

define command vc-check-out (frame)
    "Check out a compound or unit from the source control system, without claiming it."
  do-vc-command(frame, #"check-out",
		reason?: #f, revert?: #t)
end command vc-check-out;

define command vc-check-in (frame)
    "Check in a compound or unit into the source control system."
  do-vc-command(frame, #"check-in",
		reason?: #t, revert?: #t)
end command vc-check-in;

define command vc-abandon (frame)
    "Abandon a previously claimed compound or unit from the source control system."
  do-vc-command(frame, #"abandon",
		reason?: #f, revert?: #t)
end command vc-abandon;

define command vc-merge (frame)
    "Merge a claimed compound or unit against the source control system."
  do-vc-command(frame, #"merge",
		reason?: #f, revert?: #t)
end command vc-merge;

define command vc-diff (frame)
    "Compare a checked-out compound or unit against the source control system."
  do-vc-command(frame, #"diff",
		reason?: #f, revert?: #f)
end command vc-diff;

define command vc-report (frame)
    "Report the changes in a compound or a unit."
  do-vc-command(frame, #"report",
		reason?: #f, revert?: #f)
end command vc-report;

define command vc-add (frame)
    "Add a new unit to the source control system."
  do-vc-command(frame, #"add",
		reason?: #t, revert?: #t)
end command vc-add;

define command vc-remove (frame)
    "Remove a unit from the source control system."
  do-vc-command(frame, #"remove", 
		reason?: #t, revert?: #f)
end command vc-remove;

define function do-vc-command
    (frame :: <editor-state-mixin>, operation :: <source-control-operation>,
     #key reason? = #f, revert? = #f) => ()
  let window :: <basic-window> = frame-window(frame);
  let buffer :: <basic-buffer> = frame-buffer(frame);
  let (success?, pathname, message)
    = do-source-control-operation(window, operation,
				  pathname: buffer-default-pathname(buffer), reason?: reason?);
  when (message)
    display-message(window, "%s", message)
  end;
  when (success? & revert?)
    let editor = frame-editor(frame);
    let buffer = pathname & find-buffer-from-pathname(editor, pathname);
    when (buffer)
      let reverted? = revert-buffer-if-necessary(buffer, window: window);
      when (reverted? & buffer = frame-buffer(frame))
	queue-redisplay(window, $display-all)
      end
    end
  end;
  frame-last-command-type(frame) := #"version-control"
end function do-vc-command;
