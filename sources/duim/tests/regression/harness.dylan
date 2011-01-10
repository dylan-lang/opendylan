Module:    win32-duim-regression-test-suite
Author:    Andy Armstrong, Scott McKay
Synopsis:  An interactive test-suite for Win32 DUIM
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// A simple harness for all of the tests

define constant $regression-tests :: <stretchy-object-vector>
  = make(<stretchy-vector>);

define class <test-info> (<object>)
  sealed constant slot test-class :: subclass(<simple-frame>),
    required-init-keyword: class:;
  sealed constant slot test-bug-number :: <integer>,
    required-init-keyword: bug-number:;
  sealed constant slot test-summary :: <string>,
    required-init-keyword: summary:;
  sealed constant slot test-documentation :: <string>,
    required-init-keyword: documentation:;
end class <test-info>;

define method find-test-class (class :: <class>)
  block (return)
    for (test :: <test-info> in $regression-tests)
      if (test.test-class == class)
        return(test)
      end
    end
  end
end method find-test-class;

define method install-test 
    (frame-class :: <class>, bug-number :: <integer>, summary :: <string>,
     documentation :: <string>)
 => ()
  let test = find-test-class(frame-class);
  test & remove!($regression-tests, test);
  add!($regression-tests, 
       make(<test-info>,
	    class:         frame-class,
	    bug-number:    bug-number,
	    summary:       summary,
	    documentation: documentation))
end method install-test;

define method test-label
    (test :: <test-info>, #key short?) => (label :: <string>)
  if (short?)
    format-to-string("Bug %d", test.test-bug-number)
  else
    format-to-string("Bug %d: %s",
		     test.test-bug-number,
		     test.test-summary)
  end
end method test-label;

define method start-test-frame 
    (frame :: <frame>, class :: <class>, 
     #rest args,
     #key frame-manager: framem)
 => (thread :: <thread>)
  with-busy-cursor (frame)
    let test = find-test-class(class);
    let frame-class = test.test-class;
    let title = test-label(test, short?: #t);
    local method create-test-frame () => ()
	    with-abort-restart ()
	      let test-frame
	        = if (test)
		    apply(make, frame-class, title: title, args)
		  else
		    apply(make, class, title: "Test", args)
		  end;
	      start-frame(test-frame)
	    end
	  end method create-test-frame;
    make(<thread>,
	 name: title,
	 function: create-test-frame)
  end
end method start-test-frame;

define method sorted-test-frames ()
  sort($regression-tests,
       test: method (test1 :: <test-info>, test2 :: <test-info>)
               test1.test-bug-number > test2.test-bug-number
             end)
end method sorted-test-frames;


/// A frame to show them in

define frame <tests-harness> (<simple-frame>)
  pane tests-pane (frame)
    make(<list-box>,
	 documentation: "Double-click on a test name to run it",
         items: sorted-test-frames(),
	 lines: 15,
         label-key: test-label,
	 value-changed-callback: method (sheet :: <sheet>)
				   let frame = sheet-frame(sheet);
				   let test = gadget-value(sheet);
				   note-test-selected(frame, test)
				 end,
         activate-callback: method (sheet :: <sheet>)
                              let frame = sheet-frame(sheet);
                              let test = gadget-value(sheet);
                              start-test-frame(frame, test.test-class)
                            end);
  pane test-description-pane (frame)
    make(<text-editor>,
	 //---*** Should use a functional space requirement
	 read-only?: #t,
	 min-width: 400, max-width: $fill,
	 lines: 5, fixed-height?: #t,
	 scroll-bars: #"none");
  pane main-layout (frame)
    vertically (spacing: 5)
      grouping ("Start a DUIM regression test:")
	vertically (spacing: 8)
          frame.tests-pane;
          frame.test-description-pane;
	end
      end
    end;
  layout (frame) 
    frame.main-layout;
end frame <tests-harness>;

define method initialize
    (frame :: <tests-harness>, #key) => ()
  next-method();
  note-test-selected(frame, frame.tests-pane.gadget-value)
end method initialize;

define method note-test-selected
    (frame :: <tests-harness>, test :: <test-info>) => ()
  frame.test-description-pane.gadget-text := test.test-documentation
end method note-test-selected;

define method start-tests
    () => (status-code :: <integer>)
  let frame = make(<tests-harness>, title: "Tests");
  start-frame(frame)
end method start-tests;

/*---*** Not used yet...
define macro test-frame-definer
  { define test-frame ?class:name (?number:expression, ?summary:expression)
      ?details:*
    end }
    => { define test-frame ?class (<simple-frame>)
         end;
         install-test(?class, ?number, ?summary) }
details:
  { } => { }
  { ?stuff:*; ... } => { ?stuff; ... }
end macro test-frame-definer;
*/
