Module:    environment-tools
Synopsis:  Environment tools
Author:    Andy Armstrong, Chris Page, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Environment tool

define open abstract class <environment-tool>
    (<environment-simple-frame>)
end class <environment-tool>;

define method initialize
    (frame :: <environment-tool>, #key) => ()
  next-method();
  command-enabled?(frame-find-profiler, frame) := profiling-available?();
end method initialize;

define method start-environment-frame
    (frame :: <environment-tool>) => ()
  with-environment-handlers (frame)
    next-method()
  end
end method start-environment-frame;

define method handle-invalid-object-error
    (frame :: <environment-tool>, error :: <invalid-object-error>)
  let message
    = block ()
	format-to-string
	  ("Object '%s' no longer exists, so cannot be browsed",
	   frame-object-unique-name(frame, error.condition-object))
      exception (object :: <invalid-object-error>)
	"This object no longer exists, so cannot be browsed"
      end;
  environment-warning-message(message, owner: frame);
  abort()
end method handle-invalid-object-error;

define method handle-expired-timeout
    (frame :: <environment-tool>, error :: <timeout-expired>)
  let message
    = "This operation can not be completed as a compilation is in progress.";
  environment-error-message(message, owner: frame);
  abort()
end method handle-expired-timeout;


/// Tool bar handling

define open generic make-environment-tool-bar-buttons
    (frame :: <frame>) => (buttons :: <sequence>);

define method make-build-tool-bar-buttons
    (frame :: <environment-tool>)
 => (buttons :: <sequence>)
  vector(//---*** Do we want"Clean Build" on the tool bar as well?
         make(<button>, 
              label: $build-bitmap,
	      documentation: $build-title,
              command: frame-build-project,
              activate-callback: method (sheet)
                                   let frame = sheet-frame(sheet);
                                   frame-build-project(frame)
                                 end))
end method make-build-tool-bar-buttons;

define method make-application-tool-bar-buttons
    (frame :: <environment-tool>)
 => (buttons :: <sequence>)
  vector(make(<button>,
              label: $run-bitmap,
	      documentation: $run-title,
              command: frame-start-or-resume-application),
         make(<button>, 
              label: $pause-bitmap,
	      documentation: $pause-title,
              command: frame-pause-application),
         make(<button>, 
              label: $stop-bitmap,
	      documentation: $stop-title,
              command: frame-stop-application))
end method make-application-tool-bar-buttons;

define method make-debug-tool-bar-buttons
    (frame :: <environment-tool>)
 => (buttons :: <sequence>)
  vector(make(<button>,
              label: $debug-bitmap,
	      documentation: $debug-title,
              command: frame-debug-application),
	 make(<button>,
              label: $interact-bitmap,
	      documentation: $interact-title,
              command: frame-interact))
end method make-debug-tool-bar-buttons;

define constant $start-profiling-title = "Start Profiling";
define constant $stop-profiling-title  = "Stop Profiling";

define method make-profile-tool-bar-buttons
    (frame :: <environment-tool>)
 => (buttons :: <sequence>)
  local method profile-documentation
	    (profiling? :: <boolean>)
	  if (profiling?)
	    $stop-profiling-title
	  else
	    $start-profiling-title
	  end
	end method profile-documentation;
  let project = frame.frame-current-project;
  let profiling?
    = if (project)
	let application = project.project-application;
	//---*** Need to have a protocol to tell if profiling...
	application & #f
      end;
  let button
    = make(<check-button>,
	   label: $profile-bitmap,
	   documentation: profile-documentation(profiling?),
	   button-style: #"push-button",
	   value: profiling?,
	   value-changed-callback:
	     method (gadget :: <check-button>)
	       let profiling? = gadget-value(gadget);
	       frame-profiling?(frame) := profiling?;
	       gadget-documentation(gadget)
		 := profile-documentation(profiling?)
	     end);
  tune-in($project-channel,
	  method (message :: <profiling-state-change-message>)
	    gadget-value(button) := message.message-enabled?
	  end,
	  message-type: <profiling-state-change-message>);
  vector(button)
end method make-profile-tool-bar-buttons;

define method make-environment-tool-bar-buttons
    (frame :: <environment-tool>)
 => (buttons :: <sequence>)
  with-frame-manager (frame-manager(frame))
    let buttons :: <stretchy-object-vector> = make(<stretchy-object-vector>);
    local method add-buttons
	      (make-buttons-function :: <function>, 
	       #key type :: false-or(<type>))
	   => ()
	    if (~type | instance?(frame, type))
	      add!(buttons,
		   make(<row-layout>,
			children: make-buttons-function(frame),
			spacing: 0))
	    end
	  end method add-buttons;
    add-buttons(make-history-tool-bar-buttons,   type: <frame-history-mixin>);
    add-buttons(make-clipboard-tool-bar-buttons, type: <frame-clipboard-mixin>);
    add-buttons(make-undo-tool-bar-buttons,      type: <frame-undo-mixin>);
    add-buttons(make-search-tool-bar-buttons);
    add-buttons(make-build-tool-bar-buttons);
    add-buttons(make-application-tool-bar-buttons);
    add-buttons(make-debug-tool-bar-buttons);
    if (profiling-available?())
      add-buttons(make-profile-tool-bar-buttons)
    end;
    buttons
  end
end method make-environment-tool-bar-buttons;

//---*** This should be done by DUIM somehow
define constant $tool-bar-group-spacing :: <integer> = 8;

define method make-environment-tool-bar
    (frame :: <environment-tool>, #key extra-buttons, max-width)
 => (tool-bar :: false-or(<tool-bar>))
  with-frame-manager (frame-manager(frame))
    let common-buttons = make-environment-tool-bar-buttons(frame);
    let buttons
      = if (extra-buttons)
          concatenate(common-buttons, extra-buttons)
        else
          common-buttons
        end;
    make(<tool-bar>,
         child: make(<row-layout>,
		     children:  buttons,
		     spacing:   $tool-bar-group-spacing,
		     max-width: max-width))
  end
end method make-environment-tool-bar;


/// Basic environment tool

define open abstract class <basic-environment-tool>
    (<frame-module-mixin>,
     <frame-clipboard-mixin>,
     <frame-selection-mixin>,
     //--- cpage: 1997.10.22 Order of these next two is significant.
     <search-target-frame-mixin>,
     <frame-search-mixin>,
     <frame-linking-mixin>,
     <frame-reuse-mixin>,
     <frame-help-mixin>,
     <environment-tool>)
end class <basic-environment-tool>;


/// Clipboard handling

define method command-available-for-focus?
    (frame :: <basic-environment-tool>, command == <frame-cut-command>)
 => (available? :: <boolean>)
  let target = frame-selection-target(frame);
  target & cut-object?(target.target-pane, target)
end method command-available-for-focus?;

define method command-available-for-focus?
    (frame :: <basic-environment-tool>, command == <frame-copy-command>)
 => (available? :: <boolean>)
  let target = frame-selection-target(frame);
  target & copy-object?(target.target-pane, target)
end method command-available-for-focus?;

define method command-available-for-focus?
    (frame :: <basic-environment-tool>, command == <frame-paste-command>)
 => (available? :: <boolean>)
  let target = frame-selection-target(frame);
  target & paste-object?(target.target-pane, target)
end method command-available-for-focus?;

define method command-available-for-focus?
    (frame :: <basic-environment-tool>, command == <frame-delete-command>)
 => (available? :: <boolean>)
  let target = frame-selection-target(frame);
  target & delete-object?(target.target-pane, target)
end method command-available-for-focus?;

define method execute-command-for-focus
    (frame :: <basic-environment-tool>, command :: <frame-cut-command>) => ()
  let target = frame-sheet-target(frame, command.command-sheet);
  if (target)
    let pane = target.target-pane;
    if (cut-object?(pane, target))
      cut-object(pane, target)
    else
      environment-action-unavailable(frame, "No cut action available")
    end
  else
    environment-action-unavailable(frame, "No selection available to cut")
  end
end method execute-command-for-focus;

define method execute-command-for-focus
    (frame :: <basic-environment-tool>, command :: <frame-copy-command>) => ()
  let target = frame-sheet-target(frame, command.command-sheet);
  if (target)
    let pane = target & target.target-pane;
    if (copy-object?(pane, target))
      copy-object(pane, target)
    else
      environment-action-unavailable(frame, "No copy action available")
    end
  else
    environment-action-unavailable(frame, "No selection available to copy")
  end
end method execute-command-for-focus;

define method execute-command-for-focus
    (frame :: <basic-environment-tool>, command :: <frame-paste-command>) => ()
  let target = frame-sheet-target(frame, command.command-sheet);
  if (target)
    paste-object(target.target-pane, target)
  else
    let pane = frame-target-pane(frame);
    paste-object(pane, make-command-target(pane, #f))
  end
end method execute-command-for-focus;

define method execute-command-for-focus
    (frame :: <basic-environment-tool>, command :: <frame-delete-command>) => ()
  let target = frame-sheet-target(frame, command.command-sheet);
  if (target)
    let pane = target.target-pane;
    if (delete-object?(pane, target))
      delete-object(pane, target)
    else
      environment-action-unavailable(frame, "No delete action available")
    end
  else
    environment-action-unavailable(frame, "No selection available to delete")
  end
end method execute-command-for-focus;


define method copy-object?
    (frame :: <basic-environment-tool>, target :: <command-target>)
 => (copy? :: <boolean>)
  let object = frame-target-browse-object(frame, target);
  true?(object)
end method copy-object?;

define method copy-object
    (frame :: <basic-environment-tool>, target :: <command-target>)
 => ()
  let object = frame-target-browse-object(frame, target);
  with-clipboard (clipboard = top-level-sheet(frame))
    let string = frame-target-as-string(frame, target);
    add-clipboard-data(clipboard, object);
    unless (string == object)
      add-clipboard-data(clipboard, string)
    end
  end
end method copy-object;


/// Environment project frame

define open abstract class <environment-project-tool>
    (<environment-fixed-project-frame>,
     <basic-environment-tool>)
  constant slot frame-printed-representations :: <object-table> = make-name-cache();
  constant slot frame-unique-names :: <object-table> = make-name-cache();
  constant slot frame-default-names :: <object-table> = make-name-cache();
end class <environment-project-tool>;

define function make-name-cache
    () => (table :: <object-table>)
  make(<object-table>, weak: #"key")
end function make-name-cache;

define method refresh-frame
    (frame :: <environment-project-tool>) => ()
  remove-all-keys!(frame.frame-printed-representations);
  remove-all-keys!(frame.frame-unique-names);
  remove-all-keys!(frame.frame-default-names);
  next-method();
end method refresh-frame;

define method frame-print-environment-object
    (frame :: <environment-project-tool>, object :: <environment-object>,
     #key default :: <string> = $unknown-name)
 => (name :: <string>)
  let cache = frame.frame-printed-representations;
  element(cache, object, default: #f)
    | begin
	let name = next-method();
	element(cache, object) := name
      end
end method frame-print-environment-object;

define method frame-default-object-name
    (frame :: <environment-project-tool>, object :: <environment-object>,
     #key default :: <string> = $unknown-name)
 => (name :: <string>)
  let cache = frame.frame-default-names;
  element(cache, object, default: #f)
    | begin
	let name = next-method();
	element(cache, object) := name
      end
end method frame-default-object-name;

define method frame-object-unique-name
    (frame :: <environment-project-tool>, object :: <environment-object>,
     #key default :: <string> = $unknown-name)
 => (name :: <string>)
  let cache = frame.frame-unique-names;
  element(cache, object, default: #f)
    | begin
	let name = next-method();
	element(cache, object) := name
      end
end method frame-object-unique-name;
