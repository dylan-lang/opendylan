Module:    environment-debugger
Author:    Bill Chiles, Jason Trenouth, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Stop reason options page

define sealed pane <options-stop-reason-page> ()
  pane stop-reason-dialog-type-table (dialog)
    begin
      let stop-reasons :: <vector> = as(<vector>, stop-reason-types());
      make(<table-control>,
  	   headings: #["Exception", "Action"],
	   width: 300,
	   widths: #[200, 100],
	   value-changed-callback: curry(stop-reason-dialog-action-list-update, dialog),
	   generators: vector(stop-reason-label, 
			      compose(stop-reason-action-label, 
				      stop-reason-action)),
	   items: sort!(map-into(stop-reasons, shallow-copy, stop-reasons),
		        test: method (x, y) 
				x.stop-reason-label < y.stop-reason-label
			      end method));
    end;
  pane stop-reason-dialog-action-list (dialog)
    make(<option-box>, 
	 value-changed-callback: curry(stop-reason-dialog-type-table-update, dialog),
	 label-key: stop-reason-action-label,
	 items: vector(stop-reason-ignore-action,
		       stop-reason-report-action,
		       stop-reason-debug-action));
  layout (dialog)
    vertically (spacing: $vertical-spacing)
      grouping ("Exceptions")
        dialog.stop-reason-dialog-type-table
      end;
      grouping ("Action")
        dialog.stop-reason-dialog-action-list
      end;
    end;
end pane;

define sealed domain make (singleton(<options-stop-reason-page>));
define sealed domain initialize (<options-stop-reason-page>);

define sealed method options-page-name 
    (page :: <options-stop-reason-page>)
 => (name :: <string>)
  "Exceptions"
end method;

define sealed method update-from-page 
    (debugger :: <debugger>, page :: <options-stop-reason-page>)
 => ()
  let types = stop-reason-types();
  for (item in page.stop-reason-dialog-type-table.gadget-items)
    element(types, item.stop-reason-class) := item
  end for;
end method;

define function stop-reason-dialog-action-list-update 
    (dialog :: <options-stop-reason-page>, pane :: <table-control>)
 => ()
  let type = dialog.stop-reason-dialog-type-table.gadget-value;
  if (type)
    dialog.stop-reason-dialog-action-list.gadget-value := type.stop-reason-action;
  end if;
end function;

define function stop-reason-dialog-type-table-update 
    (dialog :: <options-stop-reason-page>, pane :: <option-box>)
 => ()
  let action = dialog.stop-reason-dialog-action-list.gadget-value;
  if (action)
    let type = dialog.stop-reason-dialog-type-table.gadget-value;
    if (type)
      type.stop-reason-action := action;
      update-gadget(dialog.stop-reason-dialog-type-table);
    end if;
  end if;
end function;



/// REGISTER-APPLICATION-CALLBACKS (internal)
/// We register a set of callbacks with the application server so that
/// the debugger can be kept informed of application state changes.

define function register-project-application-callbacks 
    (project :: <project-object>) => ()
  let application = project.project-application;
  register-application-callbacks
    (application,
     initialized-callback:         note-application-initialized,
     process-created-callback:     note-application-process-started,
     debugging-callback:           start-debugging,
     thread-message-callback:      note-application-thread-message,
     thread-finished-callback:     note-application-thread-finished,
     started-interaction-callback: note-application-started-interaction,
     just-interacted-callback:     note-application-just-interacted,
     interactive-results-callback: note-application-interactive-results,
//   process-finished-callback:    note-application-process-finished,
     finished-execution-callback:  note-application-finished-execution)
end function;

tune-in($project-channel,
	register-project-application-callbacks,
	message-type: singleton(#"make-application"),
	message?: #f);

define method note-application-process-started
    (project :: <project-object>) => ()
  note-project-process-started(project);
  reset-project-thread-message-logs(project);
end method note-application-process-started;

define variable *application-messages?* :: false-or(<symbol>) = #"environment";

define method note-application-thread-message
    (project :: <project-object>, thread :: <thread-object>,
     message :: <string>)
 => ()
  print-application-message(*application-messages?*, project, thread, message)
end method note-application-thread-message;

define function note-application-finished-execution
    (project :: <project-object>) => ()
  if (project.application-pause-before-termination?)
    let project-browser
      = choose-environment-frame(default-port(),
				 <project-browser>,
				 project: project);
    // Should always be a project-browser but just in case:
    when (project-browser)
      let message = project.termination-message;
      call-in-frame(project-browser,
		    method ()
		      raise-frame(project-browser);
		      deiconify-frame(project-browser);
		      environment-message(message, owner: project-browser)
		    end)
    end
  end
end function note-application-finished-execution;

define function termination-message
    (project :: <project-object>)
 => (message :: <string>)
  let profiling? = true?(project.project-last-profile);
  let application = project.project-application;
  let filename
    = if (application)
	application.application-filename
      else
	project.project-debug-filename
      end;
  format-to-string
    ("The application '%s' has been paused before closing.%s",
     filename,
     if (profiling?)
       "\n\nTo view the profiling results, choose Go>Profiler."
     else
       ""
     end)
end function termination-message;
