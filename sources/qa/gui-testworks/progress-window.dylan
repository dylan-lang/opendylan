Module:       gui-testworks
Summary:      GUI progress window for Tesworks
Author:       Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// ---*** ISSUES:
//
// Provide a way to skip over checks/tests and to step out of suites/tests.
//   - The commented-out code is a failed attempt to do that.  I think I
//     need more/other hooks in TestWorks.


/// Progress window frame class

define function make-text-editor
    (#rest args, #key lines = 4, #all-keys)
 => (text-editor :: <text-editor>)
  apply(make, <text-editor>,
        columns: 80, editable?: #f, scroll-bars: #"vertical",
        lines: lines,
        args)
end function make-text-editor;

define constant $progress-window-name :: <string>
  = "GUI-TestWorks Progress Window";

define frame <progress-window> (<simple-frame>)
  pane main-layout (frame)
    vertically (spacing: 2)
      // Vertically-scrollable text field for check descriptions.
      make(<table-layout>,
        columns: 2, spacing: 2,
        x-alignment: #[#"left", #"left"], y-alignment: #"top",
        contents:
	  vector(
	    vector(make(<label>, label: "Suite:"), frame.progress-suite-name),
	    vector(make(<label>, label: "Test:"),  frame.progress-test-name),
	    vector(make(<label>, label: "Check:"), frame.progress-check-name)
                 ));
      labelling ("Information:") frame.progress-information end;
      horizontally (spacing: 2)
        // Buttons controlling progress.
        frame.progress-continue-button;
//        frame.progress-step-out-button;
        frame.progress-close-button;
      end;
    end;
  layout (frame) frame.main-layout;
  status-bar (frame)
    with-frame-manager (frame-manager(frame))
      make(<status-bar>)
    end;
  constant slot progress-suite-name :: <text-editor> = make-text-editor();
  constant slot progress-test-name :: <text-editor> = make-text-editor();
  constant slot progress-check-name :: <text-editor> = make-text-editor();
  constant slot progress-information :: <text-editor> = make-text-editor();
  constant slot progress-continue-button :: <push-button>
    = make(<push-button>, label: "&Continue", enabled?: #f, default?: #t,
	   documentation: "Continue with test run after a pause",
	   activate-callback: gui-progress-continue-callback);
/*
  constant slot progress-step-out-button :: <push-button>
    = make(<push-button>, label: "&Step Out", enabled?: #f,
	   documentation:
             "Step out of the current test or suite by signalling an error",
	   activate-callback: gui-progress-step-out-callback);
*/
  constant slot progress-close-button :: <push-button>
    = make(<push-button>, label: "C&lose",
	   documentation: "Close progress window (test run will continue)",
	   activate-callback: gui-progress-close-callback);
  constant slot progress-paused-lock :: <semaphore> = make(<semaphore>);
//  slot progress-exit-component? :: <boolean> = #f;
  keyword title: = $progress-window-name;
end frame <progress-window>;

define function gui-progress-continue-callback (sheet :: <sheet>)
  let frame = sheet-frame(sheet);
  gadget-label(frame.frame-status-bar) := "Test run in progress";
  gadget-enabled?(frame.progress-continue-button) := #f;
/*
  gadget-enabled?(frame.progress-step-out-button) := #f;
  frame.progress-exit-component? := #f;
*/
  release(frame.progress-paused-lock);
end function gui-progress-continue-callback;

/*---*** Not used
define function gui-progress-step-out-callback (sheet :: <sheet>)
  let frame = sheet-frame(sheet);
  gadget-label(frame.frame-status-bar) := "Stepping out of test component...";
  gadget-enabled?(frame.progress-continue-button) := #f;
/*
  gadget-enabled?(frame.progress-step-out-button) := #f;
  frame.progress-exit-component? := #t;
*/
  release(frame.progress-paused-lock);
end function gui-progress-step-out-callback;
*/

define function gui-progress-close-callback (sheet :: <sheet>)
  exit-frame(sheet-frame(sheet));
end function gui-progress-close-callback;



/// Access to message areas (useful as hooks from TestWorks)

define function gui-progress-display-message (kind, message :: <string>)
  let frame = *progress-window*;
  let name-gadget
    = select (kind)
	<suite>        => progress-suite-name;
	<test>         => progress-test-name;
	#"check"       => progress-check-name;
        #"information" => progress-information;
	otherwise      => #f;
      end;
  when (frame & name-gadget)
    gadget-text(frame.name-gadget) := message;
  end;
end function gui-progress-display-message;

define function gui-progress-clear-all-messages () => ()
  let frame = *progress-window*;
  when (frame)
    gadget-text(frame.progress-suite-name) := "";
    gadget-text(frame.progress-test-name) := "";
    gadget-text(frame.progress-check-name) := "";
    gadget-text(frame.progress-information) := "";
  end;
end function gui-progress-clear-all-messages;


define function gui-progress-pause ()
  let frame = *progress-window*;
  when (frame)
    gadget-label(frame.frame-status-bar) := "Test run paused...";
    gadget-enabled?(frame.progress-continue-button) := #t;
//    gadget-enabled?(frame.progress-step-out-button) := #t;
    wait-for(frame.progress-paused-lock);
/*
    when (frame.progress-exit-component?)
      // Normally, TestWorks will catch this and report it later.
      error("Tester forcibly exited test component");
    end;
*/
  end;
end function gui-progress-pause;

define function gui-progress-pause-with-check-name (message :: <string>)
  gui-progress-display-message(#"check", message);
  gui-progress-pause();
end function gui-progress-pause-with-check-name;

define function gui-announce-function (component :: <component>)
  gui-progress-display-message
    (component.object-class,
     concatenate
       (component.component-name, "\n",
        component.component-description));
end function gui-announce-function;



/// Startup/shutdown functions

// Note: There can currently be only one instance of a GUI-TestWorks
// progress window open at a time, because the variable is the same
// in all threads, for synchronisation purposes.  (The progress window
// itself always runs in a separate thread.)

define atomic variable *progress-window* :: false-or(<progress-window>) = #f;

define function do-start-progress-window ()
  make(<thread>,
       function: method () start-frame(*progress-window*); end,
       name: $progress-window-name);
end function do-start-progress-window;

define function start-progress-window (#key force?)
  unless (*progress-window* | force?)
    *progress-window* := make(<progress-window>);
    do-start-progress-window();
  end;
end function start-progress-window;


define function exit-progress-window ()
  when (*progress-window*)
    exit-frame(*progress-window*);
  end;
end function exit-progress-window;

define sideways method handle-event
    (frame :: <progress-window>, event :: <frame-exit-event>) => ()
  *progress-window* := #f;
  next-method();
end method handle-event;



/// Simple wrapper function

// Note: These should be functions, but the emu messes up #all-keys then.

define method gui-perform-suite
    (suite :: <suite>,
     #rest args,
     #key announce-function = gui-announce-function,
	  announce-checks?  = #t,
     #all-keys)
 => (result :: <component-result>)
  block ()
    start-progress-window();
    dynamic-bind
        (*announce-check-function* = gui-progress-pause-with-check-name)
      apply
	(perform-suite,
	 suite,
	 announce-function: announce-function,
	 announce-checks?:  announce-checks?,
	 args)
    end
  cleanup
    exit-progress-window();
  end;
end method gui-perform-suite;

define method gui-perform-test
    (test :: <test>,
     #rest args,
     #key announce-function = gui-announce-function,
	  announce-checks?  = #t,
     #all-keys)
 => (result :: <component-result>)
  block ()
    start-progress-window();
    dynamic-bind
        (*announce-check-function* = gui-progress-pause-with-check-name)
      apply
	(perform-test,
	 test,
	 announce-function: announce-function,
	 announce-checks?:  announce-checks?,
	 args)
    end
  cleanup
    exit-progress-window();
  end;
end method gui-perform-test;
