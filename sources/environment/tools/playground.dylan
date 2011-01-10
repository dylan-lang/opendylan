Module:    environment-tools
Synopsis:  Environment tools
Author:    Andy Armstrong, Chris Page, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Dylan playground

// <CHECK-LABEL> (internal)

//--- hughg, 1998/10/29: IWBN to offer alternate images, not just the
// Win32 OBW_CHECK bitmap.
define pane <check-label> ()
  slot check-value = #f,
    setter: %value-setter,
    init-keyword: value:;
  slot check-label = #f,
    //--- hughg, 1998/10/30: Not used.
    // setter: %label-setter,
    setter: #f,
    required-init-keyword: label:;
  pane check-value-pane (pane)
    //--- hughg, 1998/11/05: Maybe there's a simpler way to do this, by having
    // just a <label-gadget> and changing its gadget-label; but then how would
    // we force its size (to be the same as the $check-bitmap) when the
    // label's #f?
    begin
      let null-pane  = make(<null-pane>);
      let check-pane = make(<label>, label: $check-bitmap);
      make(<stack-layout>,
	   children: vector(null-pane, check-pane),
	   mapped-page: if (pane.check-value) check-pane else null-pane end)
    end;
  pane check-label-pane (pane)
    make(<label>, label: pane.check-label);
  layout (pane)
    horizontally (x-spacing: 4, y-alignment: #"center")
      pane.check-value-pane;
      pane.check-label-pane;
    end;
end pane <check-label>;

define method check-value-setter (value, pane :: <check-label>) => (value)
  pane.%value := value;
  let layout = pane.check-value-pane;
  stack-layout-mapped-page(layout)
    := sheet-children(layout)[if (value) 1 else 0 end];
  value
end method check-value-setter;

/* --- hughg, 1998/10/30: Not used.
define method check-label-setter (label, pane :: <check-label>) => (label)
  pane.%label := label;
  gadget-label(pane.check-label-pane) := label;
  label
end method check-label-setter;
*/

/// <STARTING-PLAYGROUND-DIALOG> (internal)

// "Starting playground ..." dialog class.
define frame <starting-playground-dialog>
    (<frame-window-settings-mixin>,
     <environment-frame>,
     <dialog-frame>)
  pane pd-opening-project (frame)
    make(<check-label>, label: "Opening playground project");
  pane pd-starting-application (frame)
    make(<check-label>, label: "Starting playground application");
  pane pd-opening-interactor (frame)
    make(<check-label>, label: "Opening interactor");
  pane pd-ready (frame)
    make(<check-label>, label: "Playground ready");
  //--- hughg, 1998/11/09: This button is a hack solution to the problem
  // that the dialog doesn't always exit properly if an error occurs elsewhere,
  // e.g., during project initialisation, so some callbacks are never reached.
  pane pd-close-button (frame)
    make(<push-button>, label: "Close");
  layout (frame)
    with-spacing (thickness: 8)
      vertically (y-spacing: 8, x-alignment: #"right")
	tabling (columns: 2, spacing: 16, y-alignment: #"center")
	  make(<label>,
	       label: $playground-large-bitmap,
	       width:  32, min-width:  32, max-width:  32,
	       height: 32, min-height: 32, max-height: 32);
	  make(<label>, label: "Please wait while the playground starts...");
	  make(<null-pane>);
	  vertically (y-spacing: 8)
	    frame.pd-opening-project;
	    frame.pd-starting-application;
	    frame.pd-opening-interactor;
	    frame.pd-ready;
	  end;
	end;
        frame.pd-close-button;
      end;
    end;
  keyword mode: = #"modeless";
  keyword exit-callback: = #f;
  keyword cancel-callback: = #f;
  keyword resizable?: = #f;
end frame <starting-playground-dialog>;

define method generate-frame-title 
    (frame :: <starting-playground-dialog>) => (title :: <string>)
  "Starting Playground"
end method generate-frame-title;

define window-settings 
    starting-playground-dialog :: <starting-playground-dialog>
  = "Starting Playground";

// Global pointer to dialog and global lock for starting.
define variable *starting-playground-dialog*
    :: false-or(<starting-playground-dialog>)
  = #f;

define constant $starting-playground-dialog-lock :: <simple-lock>
  = make(<simple-lock>);

define function start-playground-dialog (owner :: <frame>) => ()
  *starting-playground-dialog*
    := make(<starting-playground-dialog>, owner: owner);
  gadget-activate-callback(*starting-playground-dialog*.pd-close-button)
    := exit-playground-dialog;
  start-dialog(*starting-playground-dialog*);
end function;

define function exit-playground-dialog (#rest args) => ()
  ignore(args);
  with-lock ($starting-playground-dialog-lock)
    let playground-dialog = *starting-playground-dialog*;
    debug-message("playground-dialog = %=", playground-dialog);
    //---*** hughg, 1998/11/05: This "when" protection shouldn't be needed, but
    // sometimes callbacks aren't tuned-out properly so we end up with some of
    // them registered twice (on the second use of the playground), so this
    // function gets called twice.
    // --- hughg, 1998/11/09: Maybe this function should just forcibly tune-out
    // all channel receivers?  That *should* be safe.
    when (playground-dialog)
      *starting-playground-dialog* := #f;
      debug-message("*starting-playground-dialog* = %=",
		    *starting-playground-dialog*);
      call-in-frame(playground-dialog,
		    method ()
		      debug-message("Exiting dialog");
		      exit-dialog(playground-dialog);
		    end);
    end;
  end;
end function;

//--- hughg, 1998/11/05: These 2 error messages have never been exercised.
define function error-playground-not-found () => ()
  environment-error-message
    (concatenate
       ("The Playground project could not be opened.\n",
	"Make sure the example projects are installed."),
     owner: *starting-playground-dialog*);
  exit-playground-dialog();
end function;

define function error-playground-not-started () => ()
  environment-error-message
    ("The Playground application could not be started.",
     //---*** Do we have any remedial suggestions?
     owner: *starting-playground-dialog*);
  exit-playground-dialog();
end function;

define function check-playground-label (label-getter :: <function>) => ()
  let playground-dialog = *starting-playground-dialog*;
  when (playground-dialog)
    call-in-frame
      (playground-dialog,
       method ()
	 check-value(playground-dialog.label-getter) := #t;
	 raise-frame(playground-dialog);
       end);
  end;
end function;

define function playground-opening-project-callback
    (message :: <frame-found-message>) => ()
  let frame = message.message-frame;
  debug-message("'Opening project' callback got 'frame found' message.");
  when (instance?(frame, <environment-frame>)
	& frame.environment-frame-class-name = #"project-browser"
	& playground-project?(frame.frame-current-project, just-name?: #t))
    debug-message("'Opening project' callback tuned-out.");
    // Tune out this callback.
    tune-out($project-channel, playground-opening-project-callback);
    // (Callback for application startup finishing will be tuned-in
    // in the PLAYGROUND-INTERACT local method.)
    // Tell dialog we're opening the project.
    check-playground-label(pd-opening-project);
  end;
end function;

define function playground-interact (frame :: <project-browser>) => ()
  let project = frame.frame-current-project;
  // Check the label to say we're starting (or already have) the app.
  check-playground-label(pd-starting-application);
  if (frame-application-tethered?(frame, project))
    // If app is already around, check the "opening interactor" label
    // and tune in the callback.
    check-playground-label(pd-opening-interactor);
    tune-in($environment-channel, playground-interactor-opened-callback,
	    message-type: <frame-found-message>);
  else
    // Tell the dialog the project's open & we're starting the app.
    tune-in($project-channel, playground-application-started-callback,
	    message-type: type-union(<run-application-failed-message>,
				     <application-initialized-message>));
  end;
  frame-interact(frame);
end function;

define function playground-application-started-callback
    (message :: type-union(<run-application-failed-message>,
			   <application-initialized-message>))
 => ()
  let project = message.message-project;
  when (playground-project?(project))
    // Tune out this callback.
    tune-out($project-channel, playground-application-started-callback);
    // Tune-in a callback for when the interactor has been found.
    tune-in($environment-channel, playground-interactor-opened-callback,
	    message-type: <frame-found-message>);
    debug-message("'Interactor opened' callback tuned-in.");
    // Note success or failure.
    if (instance?(message, <run-application-failed-message>))
      error-playground-not-started();
    else
      // Tell dialog the app's started and we're awaiting an interactor.
      check-playground-label(pd-opening-interactor);
    end;
  end;
end function;

define function playground-interactor-opened-callback
    (message :: <frame-found-message>) => ()
  let frame = message.message-frame;
  when (instance?(frame, <environment-frame>)
	& frame.environment-frame-class-name = #"debugger"
	& playground-project?(frame.frame-current-project))
    // Tune out this callback.
    tune-out($project-channel, playground-interactor-opened-callback);
    // Tell dialog the app's started and we're awaiting an interactor.
    check-playground-label(pd-ready);
    // Finally, close the dialog after a short pause.
    //--- hughg, 1998/11/04: We could really use <timer-event>s here.
    //--- (We can't execute SLEEP via a CALL-IN-FRAME, or the window
    //--- won't repaint for 2 seconds!  So we spawn another thread.)
    make(<thread>, name: "Closing playground startup dialog...",
	 function: method ()
		     sleep(2);
		     debug-message("Calling exit-playground-dialog");
		     exit-playground-dialog();
		   end);
  end;
end function;

define method frame-open-playground
    (frame :: <frame>) => (status-code :: false-or(<integer>))
  // If the dialog exists, or someone else is in the process of creating it,
  // we're opening the playground already, so needn't (and shouldn't) do any
  // more work.  In that case, we assume it'll work and just return 0.
  with-lock ($starting-playground-dialog-lock)
    if (*starting-playground-dialog*)
      0 // zero
    else
      // Create and display dialog.
      start-playground-dialog(frame);
      // Find and open the project.
      let project = find-playground-project();
      if (project)
	// Open (or reuse) a project browser window for the playground.
        let project-browser = find-project-browser-showing-project(project);
	if (project-browser)
	  check-playground-label(pd-opening-project);
	  call-in-frame(project-browser, playground-interact, project-browser);
	  0
	else
	  tune-in($environment-channel, playground-opening-project-callback,
		  message-type: <frame-found-message>);
	  find-project-browser(project,
			       initialized-callback: playground-interact)

	end;
      else
	error-playground-not-found();
	#f
      end
    end;
  failure
    0 // zero
  end;
end method frame-open-playground;
